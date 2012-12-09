Parameter P : Set -> Prop.
Parameter v : Set.

Lemma dn : forall P:Prop, ~~P -> P.
Proof.
Admitted.

Goal (exists x:Set, (P x -> forall y:Set, P y)).
Proof.
apply dn.
intro. elim H.
exists v.
apply dn.
intro. elim H0. intro.

(* ~ (forall x:Set, ~ (P x -> forall y:Set, P y)) *)

(* A piece of the puzzle *)
Goal ~ (forall x:Set, (~ P x /\ forall y:Set, P y)).
Proof.
intro.
elim H with v.
unfold not. intro.
intro.
apply H0.
apply H1.
Qed.

Goal ~(forall x:Set, ~ forall y:Set, P y -> ~ P x).
Proof.
intro.
elim H with v.
intro. intro. intro.
Admitted.

Require Import Setoid.

Goal exists x:Set, (P x -> forall y:Set, P y).
Proof.
apply dn.
intro.
pattern (forall y : Set, P y) in H.
elim H.

assert ((exists x, P x \/ ~ forall y:Set, P y) <-> (exists x, P x -> forall y:Set, P y)).
admit.
rewrite <- H.
assert (~ (exists x : Set, P x \/ ~ (forall y : Set, P y)) <-> 
       (forall x:Set, ~ (P x \/ ~ (forall y : Set, P y)))).
admit.
rewrite H0.
intro.
elim H1 with v.
assert ((P v \/ ~ (forall y : Set, P y)) <-> (~ P v /\ ~ ~ (forall y : Set, P y))).
admit.
rewrite H2.
assert (~ ~ (forall y : Set, P y) <-> (forall y:Set, P y)).
admit.
rewrite H3.
unfold not.
intro.