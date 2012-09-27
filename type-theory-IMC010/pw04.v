(* ***** first some examples ***** *)


(* ***** examples: days ***** *)
Inductive day : Set :=
  | monday : day
  | tuesday : day
  | wednesday : day
  | thursday : day
  | friday : day
  | saturday : day
  | sunday : day.

Definition next_day (d:day) : day :=
  match d with
  | monday => tuesday
  | tuesday => wednesday
  | wednesday => thursday
  | thursday => friday
  | friday => saturday
  | saturday => sunday
  | sunday => monday
  end.

Eval simpl in (next_day friday).
Eval compute in (next_day friday).

Lemma tomorrow : next_day friday = saturday.

Proof.
simpl.
reflexivity.
Qed.

(* ***** examples: booleans ***** *)
Print bool.
Check bool.

(* example : function on booleans,
   two equivalent ways of writing

   Note that it is a definition by cases
   hence the use of "match ... with ..."
   but not a recursive definition
   hence the use of "Definition" instead of "Fixpoint" *)

Definition donothinga (b : bool) : bool :=
  match b with
  | true => true
  | false => false
  end.

Definition donothingb : bool -> bool :=
  fun b : bool =>
  match b with
  | true => true
  | false => false
  end.

Eval compute in (donothinga true).
Eval cbv iota in (donothinga true).
Eval cbv delta iota in (donothinga true).
Eval cbv delta iota beta in (donothinga true).

Check bool_ind.

(* ***** examples: natural numbers ***** *)

(* example: definition of addition of two natural numbers *)
Fixpoint plus (n m : nat) {struct n} : nat :=
  match n with
    O => m
  | (S p) => S (plus p m)
  end.

(* example: computation *)
Eval compute in (plus 0 0).
Eval compute in (plus (S O) (S O)).
Eval simpl in (plus (S O) (S O)).
Eval cbv beta in (plus (S O) (S O)).
Eval cbv beta iota in (plus (S O) (S O)).
Eval cbv delta in (plus (S O) (S O)).
Eval cbv delta beta in (plus (S O) (S O)).
Eval cbv delta iota in (plus (S O) (S O)).
Eval cbv beta iota delta in (plus (S O) (S O)).

Check nat_ind.

Lemma plus_n_O : forall n : nat, plus n O = n.
Proof.
intro n.
induction n.
(* alternative: elim n *)

(* case n = O *)
simpl.
reflexivity.

(* case n > 0 *)
simpl.
rewrite IHn.
reflexivity.
Qed.

(* ***** examples: intuitionistic logic ***** *)
Print True.

Print False.
Check False_ind.

Parameters A B : Prop.
Lemma about_false: False -> A.

Proof.
intro x.
elim x.
(* alternative:
apply False_ind.
assumption.      *)
Qed.


Print and.
Lemma about_intro_and :  A -> B -> A /\ B.
(* apply conj or split *)
Proof.
intros x y.
split ; assumption.
Qed.

Check and_ind.
Lemma about_elim_and : A /\ B -> A.
(* elim or apply and_ind *)

Proof.
intro x.
apply and_ind with A B.
intros.
assumption.
assumption.
Qed.



(* **** now the exercises begin ***** *)



(* ***** exercises: natural numbers ***** *)
Check nat.
Print nat.
(* NB you can use abbreviations of natural numbers: *)
Check 2.
Eval compute in (plus 0 0).
Check nat_ind.

(* example : addition with recursion in the second argument *)
Fixpoint plus2 (n m : nat) {struct m} : nat :=
  match m with
  | O => n
  | S p => S (plus2 n p)
  end.

(* exercise 1 :
   define multiplication with recursion in the first argument
   use "Eval compute" to test your definition *)

Fixpoint mul (n m : nat) {struct n} : nat := (*! term *)
(* complete the definition ! *)
  .


Eval compute in (mul 2 2).
Eval compute in (mul 2 0).
Eval compute in (mul 2 3).

(* exercise 2 :
   prove the following lemma *)
Lemma mul_n_O : forall n : nat, mul n 0 = 0.
Proof.
(*! proof *)

Qed.


(* ***** exercises: booleans ***** *)
Check bool.
Print bool.

(* exercise 3:
   give a definition of a function "negation"
   for negation on booleans *)

Definition negation (b : bool) : bool := (*! term *)
  .


(* exercise 4:
   prove the following two lemmas *)

Lemma forbool : forall b: bool, negation (negation b) = b.
Proof.
(*! proof *)

Qed.

Lemma aboutneg : forall b : bool, b = true \/ negation b = true.
Proof.
(*! proof *)

Qed.


(* ***** exercises: natlists ***** *)

(* definition of a new inductive type
   consisting of finite lists of natural numbers *)
Inductive natlist : Set :=
  | nil : natlist
  | cons : nat -> natlist -> natlist.

(* examples *)
Check nil.
Check (cons 0 nil).
Check (cons 0 (cons 1 nil)).

(* exercise 5:
   define a recursive function "length"
   that takes as input a natlist and gives back as output its length
   Test the function on different inputs. *)

Fixpoint length (l : natlist) : nat := (*! term *)
  .


Eval compute in (length nil).
Eval compute in (length (cons 0 nil)).
Eval compute in (length (cons 0 (cons 1 nil))).

(* exercise 6:
   Give a definition of the recursive function "append"
   that takes as input two lists and gives back as output
   the concatenation of the two lists,
   that is, first all elements of the first input
   and then all elements of the second input.
   Use inducion in the first argument.
   Test the function on different inputs. *)

Fixpoint append (l k : natlist) {struct l} : natlist := (*! term *)
  .

Eval compute in (append (cons 0 nil) nil).
Eval compute in (append (cons 0 nil) (cons 0 nil)).
Eval compute in (append (cons 0 (cons 1 nil)) (cons 2 (cons 3 nil))).

(* example, also on les04.v from the webpage *)
Lemma append_nil : forall l : natlist , append l nil = l.
Proof.
intro l.
elim l.
simpl.
reflexivity.

intro h.
intro t.
intro IH.
simpl.
rewrite IH.
reflexivity.
Qed.


(* exercise 7
   Prove the following lemma *)
Lemma length_append :
 forall k l : natlist, length (append k l) = plus (length k) (length l).
Proof.
(*! proof *)

Qed.


(* exercise 8
   Prove the following lemma *)

Lemma append_assoc :
  forall k l m : natlist, append (append k l) m = append k (append l m).
Proof.
(*! proof *)

Qed.


(* the following function reverses a list *)
Fixpoint reverse (l:natlist) {struct l}: natlist  :=
match l with
|  nil => nil
|  cons h t => append (reverse t) (cons h nil)
end.

(* exercise 9
   Prove the following lemma about the interaction between append and reverse
   Hint: you may want to use the lemma's append_nil and append_assoc;
   use the tactic "rewrite". *)

Lemma reverse_append :
  forall l k : natlist,
  reverse (append l k) = append (reverse k) (reverse l).
Proof.
(*! proof *)

Qed.
