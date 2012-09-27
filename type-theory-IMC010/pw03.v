(* Intuitionistic logic is extended to classical logic
   by assuming a classical axiom. There are different
   possibilities for the choice of a classical axiom.
   In this practical work we show the logical equivalence
   of three different classical axioms. *)

(* The following are three classical axioms *)

Definition excluded_middle := forall A:Prop, A \/ ~A.
Definition peirce := forall A B:Prop, ((A -> B)-> A) -> A.
Definition double_negation := forall A:Prop, ~~A -> A.

(* To show that these are equivalent,
   we need to prove (at least) three implications.
   As an example, the implication
   excluded_middle implies peirce is given. *)

Lemma one : excluded_middle -> peirce.
Proof.
unfold excluded_middle.
unfold peirce.
unfold not.
intro EM.
intro A.
intro B.
elim (EM A).

intro x.
intro y.
assumption.

intro x.
intro y.
apply y.
intro z.
elimtype False.
apply x.
assumption.
Qed.

(* There is a new element in the syntax:
   a universal quantification over propositions.
   So in fact these formulas are second-order;
   we come back to that later in the course. *)

(* How to work with these universal quantifications ?
   With "intro" and "apply". Explanation by example:

   If the current goal is "forall A:Prop, A -> A",
   then by doing "intro A" the new goal is A -> A
   and a new hypothesis "A:Prop" appears.

   If the current goal is "C" and there is a hypothesis
   "x: forall A:Prop, B -> A"
   then by "apply x" the current goal is transformed into "B".
   The universally quantified A is instantiated by C.

   Now suppose that the current goal is "C" and
   there is a hypothesis "x: forall A B:Prop, B -> A".
   Then "apply x" does not work because from the
   current goal we can see how to instantiate A
   (namely with C) but not how to instantiate B.
   Therefore we should say "apply x with something."
   choosing something appropriately. *)

(* exercise; you need the "apply with". *)
(*Lemma two : peirce -> double_negation.
Proof.
(*! proof *)

Qed.*)

(* exercise *)
Lemma three : double_negation -> excluded_middle.
Proof.
unfold double_negation.
unfold excluded_middle.
unfold not.
intro DN.
intro.
apply DN.
intro.
apply H.
right.
intro.
apply H.
left.
exact H0.

Qed.

(* exercise *)
Lemma four : excluded_middle -> double_negation.
Proof.
unfold excluded_middle.
unfold double_negation.
unfold not.
intro EM.
intro A.
intro x.
elim (EM A).
intro.
exact H.

intro.
elimtype False.
apply x.
exact H.

Qed.

(* exercise *)
Lemma everything_related :
  excluded_middle -> forall A B : Prop , (A -> B) \/ (B -> A).
Proof.


Qed.

Lemma de_morgan :
  excluded_middle -> forall A B : Prop , ~(~A/\~B) -> A\/B.
Proof.
(*! proof *)

Qed.

(* exercise
   note that this lemma is true intuitionistically *)
Lemma about_implication : forall A B : Prop , (~A \/ B) -> (A -> B).
Proof.
(*! proof *)

Qed.

(* exercise
   for the converse of the previous lemma we need a classical axiom *)
Lemma classical_implication :
  excluded_middle -> forall A B : Prop , (A -> B) -> (~A \/ B).
Proof.
(*! proof *)

Qed.

(* exercise *)
Lemma about_classical_implication :
  excluded_middle -> forall A B : Prop , ~B \/ (A ->B).
Proof.
(*! proof *)

Qed.
