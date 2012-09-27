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
(*! proof *)

Qed.

Lemma one2 : (A -> B -> C) -> (A -> B) -> A -> C.
Proof.
(*! proof *)

Qed.

Lemma one3 : (B -> (A -> B) -> C) -> B -> C.
Proof.
(*! proof *)

Qed.



(* exercise 2 *)
(* give of each of the following three types an inhabitant *)
Definition two1 : (A -> A -> B) -> (C -> A) -> C -> B := (*! term *)
  .

Definition two2 : (A -> A -> B) -> A -> B := (*! term *)
  .

Definition two3 : (A -> B -> C) -> B -> A -> C := (*! term *)
  .



(* exercise 3 *)
(* complete the following four simply typed lambda terms *)
Definition three1 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x z (y z) *)
  .

Definition three2 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x (y z) z *)
  .

Definition three3 := (*! term *)
(* fun (x : ?) (y : ?) => x (fun z : ? => y) y *)
  .

Definition three4 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x (y x) (z x) *)
  .



(* exercise 4 *)
(* prove the following two lemma's *)
Lemma four1 : (~A \/ B) -> (A -> B).
Proof.
(*! proof *)

Qed.

Lemma four2 : (A \/ ~ A) -> ~~A -> A.
Proof.
(*! proof *)

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
(*! proof *)

Qed.

Lemma five2 : forall x:Terms, (P x -> ~ (forall y:Terms, ~(P y))).
Proof.
(*! proof *)

Qed.

Lemma five3 :
  (forall x y :Terms, R x y -> ~ (R y x)) ->
  (forall x:Terms, ~ (R x x)).
Proof.
(*! proof *)

Qed.


   
(* exercise 6 *)
(* give inhabitants of the following two types *)

Definition six1 : 
  (forall x y:Terms, R x y -> R y x) ->
  (forall x : Terms, R x M -> R M x) := (*! term *)
  .

Definition six2 :
  (forall x y z : Terms, R x y -> R y z -> R x z) ->
  R M N ->
  R N M ->
  R M M := (*! term *)
  .


  
(* exercise 7 *)
(* complete the following two lambda-terms *)

Definition seven1 := (*! term *)
(*
  fun (H : forall x:Terms, P x -> Q x) =>
  fun (I : ?) =>
  H M I
*)
  .

Definition seven2 := (*! term *)
(*
  fun (H : forall x y : Terms, R x y -> R y x) =>
  fun (I : ?) =>
  H M N I
*)
  .

Definition seven3 := (*! term *)
(*
  fun (H : ?) =>
  fun (I : forall x, P x -> Q x) =>
  I M (H M)
*)
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
(*! proof *)

Qed.

Lemma eight2 : (forall a:Prop, a) -> 
  forall b:Prop, forall c:Prop, ((b->c)->b)->b.
Proof.
(*! proof *)

Qed.



(* exercise 9 *)
(* find inhabitants of the following two types *)
Definition nine1 : forall a:Prop, (forall b:Prop, b) -> a := (*! term *)
  .

Definition nine2 : forall a:Prop, a -> (forall b:Prop, ((a -> b) -> b)) :=
    (*! term *)
  .



(* exercise 10 *)
(* complete the following lambda terms *)

Definition ten1 := (*! term *)
(*
  fun (a:?) =>
  fun (x: forall b:Prop, b) =>
  x a
*)
  .

Definition ten2 := (*! term *)
(*
  fun (a:?) =>
  fun (x:a) =>
  fun (b:?) =>
  fun (y:b) =>
  x
*)
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
(*! proof *)

Qed.

Lemma plus_n_S : forall n m : nat, S (plus n m) = plus n (S m).
Proof.
(*! proof *)

Qed.

Lemma com : forall n m : nat, plus n m = plus m n.
Proof.
(*! proof *)

Qed.



(* given *)
Inductive polybintree (X : Set) : Set :=
    polyleaf : X -> polybintree X
  | polynode : polybintree X -> polybintree X -> polybintree X.



(* exercise 12 *)
(* give a definition counttree that counts the number of leafs *)

Fixpoint counttree (X : Set) (b : polybintree X) {struct b} : nat :=
    (*! term *)
  .



(* exercise 13 *)
(* give a definition sum that adds the values on the leafs
   for a tree with natural numbers on the leafs *)

Fixpoint sum (b:polybintree nat) {struct b} : nat := (*! term *)
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

Fixpoint conjunction (t : polybintree bool) {struct t} : bool := (*! term *)
  .



End inductivetypes.

(*
vim: filetype=coq
*)
