Parameter A B C : Set.


Definition LamOneA' :=
  fun (x:B -> (A -> B) -> C) 
      (y:B)
  => x y ((fun (z:B) (w:A) => z) y).
Check LamOneA'.

Lemma LemOneA : (B -> ((A -> B) -> C)) -> B -> C.
Proof.
intro x.
intro y.
apply x.
exact y.
intro w.
exact y.
Qed.


Definition LamOneA :=
  fun (x:B -> (A -> B) -> C)
      (y:B)
  => x y (fun (w:A) => y).
Check LamOneA.

Lemma LemOneA' : ((B -> (A -> B)) -> C) -> B -> C.
Proof.
intro x.
intro y.
apply x.
intro z.
intro w.
exact y.
Qed.


(* associativity holds in one direction *)
Lemma almostAssociative1 :
  forall A B C : Prop, ((A -> B) -> C) -> (A -> (B -> C)).
Proof.
intros.
apply H.
intro.
exact H1.
Qed.

(* but in the other direction, *)
(* there exist examples for which it doesn't hold... *)
Lemma almostAssociative2 :
  not (forall A B C : Prop, not ((A -> (B -> C)) -> ((A -> B) -> C))).
Proof.
unfold not.
intro.
apply H with True True False.
intros.
apply H0.
tauto.
tauto.
Qed.

(* ... and some for which it holds. *)
Lemma almostAssociative3 :
  not (forall A B C : Prop, (A -> (B -> C)) -> ((A -> B) -> C)).
Proof.
unfold not.
intro.
apply H with False False.
intros.
exact H0.
intros.
exact H0.
Qed.
