(* ************************************************* *)
(* prop1 and simply typed lambda calculus *)
(* ************************************************* *)

Section prop1.

Parameters A B C : Prop.

(* exercise 1 *)
(* prove the following three lemma's *)
(* also use Print to see the proof-term *)
Lemma one1 : ((A -> B -> A) -> A) -> A.
Proof.
intros. apply H. intros. exact H0.
Qed.

Lemma one2 : (A -> B -> C) -> (A -> B) -> A -> C.
Proof.
intros. apply H. exact H1. apply H0. exact H1.
Qed.

Lemma one3 : (B -> (A -> B) -> C) -> B -> C.
Proof.
intros. apply H. exact H0. intro. exact H0.
Qed.



(* exercise 2 *)
(* give of each of the following three types an inhabitant *)
Definition two1 : (A -> A -> B) -> (C -> A) -> C -> B :=
fun (f : A -> A -> B) (g : C -> A) (c : C) =>
  f (g c) (g c)
  .

Definition two2 : (A -> A -> B) -> A -> B :=
fun (f : A -> A -> B) (a:A) =>
  f a a
  .

Definition two3 : (A -> B -> C) -> B -> A -> C :=
fun (f : A -> B -> C) (b:B) (a:A) =>
  f a b
  .



(* exercise 3 *)
(* complete the following four simply typed lambda terms *)
Definition three1 :=
 fun (x : A -> B -> C) (y : A -> B) (z : A) => x z (y z)
  .

Definition three2 :=
 fun (x : B -> A -> C) (y : A -> B) (z : A) => x (y z) z
  .

Definition three3 :=
 fun (x : (B -> A) -> A -> C) (y : A) => x (fun z : B => y) y
  .

Definition three4 :=
 fun (x : A -> B -> C)
     (y : (A -> B -> C) -> A)
     (z : (A -> B -> C) -> B)
  => x (y x) (z x)
  .



(* exercise 4 *)
(* prove the following two lemma's *)
Lemma four1 : (~A \/ B) -> (A -> B).
Proof.
unfold not. intros. elim H. intros. elimtype False. apply H1. exact H0.
intro H1. exact H1.
Qed.

Lemma four2 : (A \/ ~ A) -> ~~A -> A.
Proof.
unfold not. intros. elim H. intro H1. exact H1.
intro H1. elimtype False. apply H0. exact H1.
Qed.



End prop1.

(* ************************************************* *)
(* pred1 and lambda P *)
(* ************************************************* *)

Section pred1.

Parameter Terms : Set.
Parameters M N : Terms.
Parameters P Q : Terms -> Prop.
Parameters R : Terms -> Terms -> Prop.



(* exercise 5 *)
(* prove the following three lemma's *)
(* use Print to see the proof-term *)
(* see practical work 7 *)
Lemma five1 : (forall x:Terms, ~ (P x)) -> ~ (exists x:Terms, P x).
Proof.
unfold not. intros Hall Hexists. elim Hexists. exact Hall.
Qed.

Lemma five2 : forall x:Terms, (P x -> ~ (forall y:Terms, ~(P y))).
Proof.
unfold not. intros. apply H0 with x. exact H.
Qed.

Lemma five3 :
  (forall x y :Terms, R x y -> ~ (R y x)) ->
  (forall x:Terms, ~ (R x x)).
Proof.
unfold not. intros.
apply H with x x. exact H0. exact H0.
Qed.


   
(* exercise 6 *)
(* give inhabitants of the following two types *)

Definition six1 : 
  (forall x y : Terms, R x y -> R y x) ->
  (forall x   : Terms, R x M -> R M x) :=
fun (f : forall x y : Terms, R x y -> R y x) =>
  fun (x:Terms) (r : R x M) => f x M r
  .

Definition six2 :
  (forall x y z : Terms, R x y -> R y z -> R x z) ->
  R M N ->
  R N M ->
  R M M :=
fun (f:forall x y z : Terms, R x y -> R y z -> R x z)
    (rMN : R M N)
    (rNM : R N M) =>
  f M N M rMN rNM
  .


  
(* exercise 7 *)
(* complete the following two lambda-terms *)

Definition seven1 :=
  fun (H : forall x:Terms, P x -> Q x) =>
  fun (I : P M) =>
  H M I
  .

Definition seven2 :=
  fun (H : forall x y : Terms, R x y -> R y x) =>
  fun (I : R M N) =>
  H M N I
  .

Definition seven3 :=
  fun (H : Terms -> P M) =>
  fun (I : forall x, P x -> Q x) =>
  I M (H M)
  .

End pred1.



(* ************************************************* *)
(* prop2 and polymorphic lambda calculus *)
(* ************************************************* *)

Section prop2.

(* exercise 8 *)
(* prove the following two lemma's *)
Lemma eight1 : forall a:Prop, a -> forall b:Prop, b -> a. 
Proof.
intros. exact H.
Qed.

Lemma eight2 : (forall a:Prop, a) -> 
  forall b:Prop, forall c:Prop, ((b->c)->b)->b.
Proof.
intros. apply H0. intro. apply H. 
Qed.



(* exercise 9 *)
(* find inhabitants of the following two types *)
Definition nine1 : forall a:Prop, (forall b:Prop, b) -> a :=
fun (a:Prop) (H:forall b:Prop, b) => H a
  .

Definition nine2 : forall a:Prop, a -> (forall b:Prop, ((a -> b) -> b)) :=
fun (a:Prop) (H:a) (b:Prop) (H0:a->b) => H0 H
  .



(* exercise 10 *)
(* complete the following lambda terms *)

Definition ten1 :=
  fun (a:Prop) =>
  fun (x: forall b:Prop, b) =>
  x a
  .

Definition ten2 :=
  fun (a:Prop) =>
  fun (x:a) =>
  fun (b:Prop) =>
  fun (y:b) =>
  x
  .
 


End prop2.




(* ************************************************* *)
(* inductive datatypes and predicates *)
(* ************************************************* *)

Section inductivetypes.

(* given *)
Fixpoint plus (n m : nat) {struct n} : nat :=
  match n with
  | O => m
  | S p => S (plus p m)
  end.



(* exercise 11 *)
(* prove the following three lemma's *)
Lemma plus_n_O : forall n : nat, n = plus n 0.
Proof.
intro n. induction n.
simpl. reflexivity.
simpl. rewrite <- IHn. reflexivity.
Qed.

Lemma plus_n_S : forall n m : nat, S (plus n m) = plus n (S m).
Proof.
intros n m. induction n.
simpl. reflexivity.
simpl. rewrite <- IHn. reflexivity.
Qed.

Lemma com : forall n m : nat, plus n m = plus m n.
Proof.
induction n. induction m. simpl. reflexivity.
simpl. rewrite <- IHm. simpl. reflexivity.
induction m. simpl. rewrite -> IHn. simpl. reflexivity.
simpl. rewrite IHn. rewrite <- IHm. simpl. rewrite -> IHn. reflexivity.
Qed.



(* given *)
Inductive polybintree (X : Set) : Set :=
    polyleaf : X -> polybintree X
  | polynode : polybintree X -> polybintree X -> polybintree X.



(* exercise 12 *)
(* give a definition counttree that counts the number of leafs *)

Fixpoint counttree (X : Set) (b : polybintree X) {struct b} : nat :=
  match b with
  | polyleaf _ => O
  | polynode l r => plus (counttree X l) (counttree X r)
  end
  .



(* exercise 13 *)
(* give a definition sum that adds the values on the leafs
   for a tree with natural numbers on the leafs *)

Fixpoint sum (b:polybintree nat) {struct b} : nat :=
  match b with
  | polyleaf x => x
  | polynode l r => plus (sum l) (sum r)
  end
  .


(* given *)
Definition ifb (b1 b2 b3:bool) : bool :=
  match b1 with
  | true => b2
  | false => b3
  end.

(* given *)
Definition andb (b1 b2:bool) : bool := ifb b1 b2 false.



(* exercise 14 *)
(* give a definition and that computes the conjunction
   of the values of all leafs where all leafs have a 
   boolean label 
   use andb for conjunction on booleans *)

Fixpoint conjunction (t : polybintree bool) {struct t} : bool :=
  match t with
  | polyleaf b => b
  | polynode l r => andb (conjunction l) (conjunction r)
  end
  .



End inductivetypes.

(*
vim: filetype=coq
*)
