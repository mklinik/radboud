(* ************************************************************** *)
Section pred1.

Variable Terms : Set.
Variable M : Terms.

(* predicates *)
Variable A : Prop.
Variable P : Terms -> Prop.
Variable Q : Terms -> Prop.
Variable R : Terms -> Terms -> Prop.

(* example *)
Theorem example0 : (forall x:Terms, (P x)) -> (P M).
Proof.
intro u.
apply u.
Qed.

Print example0.
(* \u: Pi x:Terms. Px. (u M) *)


(* example *)
Theorem example1 : forall x:Terms, (P x) -> (forall y:Terms, (P y) -> A) -> A.
Proof.
intro x.
intro h.
intro i.
apply i with x.
assumption.
Qed.

Print example1.
(* \x:Terms. \h:(P x). \i:(Pi y:Terms. Py -> A). (i x h) *)

(* example, see slide 35 of week 6 *)
Theorem example2 :
  (forall x : Terms , P x -> Q x)
  ->
  (forall x : Terms , P x)
  ->
  forall y : Terms , Q y.

Proof.
intro h.
intro i.
intro y.
apply h.
apply i.
Qed.
Print example2.
(* \h: (Pi x:Terms. Px -> Qx). \i: (Pi x:Terms Px). \y: Terms. h y (i y) *)

(* exercise 1: prove the lemma and inspect the proof term *)
Lemma one : (forall x : Terms, P x) -> P M.
Proof.
intro u.
apply u.
Qed.
Print one.

(* exercise 2: prove the lemma and inspect the proof term *)
Lemma two : (A -> forall x : Terms, P x) -> (forall y : Terms, A -> P y).
Proof.
intro H.
intro y.
intro x.
apply H.
exact x.
Qed.
Print two.

(* exercise 3: prove the lemma and inspect the proof term *)
Lemma three : A -> forall x : Terms, A.
Proof.
intro x.
intro y.
exact x.
Qed.
Print three.

(* example, see slides 13-14-15 of week 7 *)
Definition AS :=
  forall x y : Terms, (R x y) -> ~(R y x).
Definition IR :=
  forall x:Terms, ~(R x x).

Theorem AS_implies_IR : AS -> IR.
Proof.
unfold AS.
unfold IR.
unfold not.
intro h.
intro x.
intro i.
apply h with x x.
  (* alternative: apply (h x x ) *)
exact i.
exact i.
Qed.
Print AS_implies_IR.

(* given *)
Definition reflif := forall x : Terms, (exists y : Terms, R x y) -> R x x.

(* exercise 4:
   define sym as the proposition stating that
   R is symmetric, that is,
   if x and y are related via R, then y and x are related via R *)
Definition sym := forall x y : Terms, R x y -> R y x.

(* exercise 5:
   define trans as the proposition stating that
   R is transitive, that is,
   if x and y are related via R, and y and z are related via R,
   then x and z are related via R  *)
Definition trans := forall x y z : Terms, (R x y /\ R y z) -> R x z.

(* exercise 6: prove the following Lemma *)
Lemma str : sym -> trans -> reflif.
Proof.
unfold sym, trans, reflif.
intro sym.
intro trans.
intro.
intro.
elim H.
intro.
intro.
apply trans with x0.
split.
exact H0.
apply sym.
apply H0.
Qed.

End pred1.

(* ************************************************************** *)

Section logical_framework.

(* we encode propositional logic
   source: webpage Herman Geuvers
   handbook article Henk Barendregt *)

(* prop representing the propositions is declared as a Set *)
Parameter prop : Set.

(* implication on prop is a binary operator *)
Parameter imp : prop -> prop -> prop.

(* we can use infix notation => for imp *)
Infix "=>" := imp (right associativity, at level 85).

(* T expresses if a proposion in prop is valid
   if (T p) is inhabited then p is valid
   if (T p) is not inhabited then p is not valid *)
Parameter T : prop -> Prop.

(* the variable imp_introduction models the introduction rule for imp *)
Parameter imp_introduction : forall p q : prop, (T p -> T q) -> T (p => q).

(* the variable imp_elimination models the elimination rule for imp *)
Parameter imp_elimination : forall p q : prop, T (p => q) -> T p -> T q.

(* exercise 7 : prove the following lemma *)
Lemma I : forall p : prop, T (p => p).
Proof.
(*! proof *)

Qed.

(* exercise 8 : prove the following lemma *)
Lemma transitivity :
 forall p q r : prop, T (p => q) -> T (q => r) -> T (p => r).
Proof.
(*! proof *)

Qed.

Parameter conjunction : prop -> prop -> prop.
Infix "X" := conjunction (no associativity, at level 90).

(* exercise 9 : define variables that model the introduction
   rule for conjuction on prop, and both elimination rules *)

Parameter conjunction_introduction : (*! term *)
  .

Parameter conjunction_elimination_l : (*! term *)
  .

Parameter conjunction_elimination_r : (*! term *)
  .

(* exercise 10: prove the following lemma *)
Lemma weak : forall a b c : prop, T (a => c) -> T ((a X b) => c).
Proof.
(*! proof *)

Qed.


(* the remainder is not obligatory *)

(* bot represents falsum in prop *)
Parameter bot : prop.

(* not represents negation in prop *)
Definition not (p : prop) := p => bot.

(* not obligatory *)
(* exercise 11 : prove the following lemma *)
Lemma contrapositive : forall p q : prop, T (p => q) -> T (not q => not p).
Proof.
(*! proof *)

Qed.

End logical_framework.

(*
vim: filetype=coq
*)
