(* cf antwoorden voor laatste opgave *)

Section proplogic.
Parameters A B C : Prop.

(* exercise 1 *)
(* complete the following simply typed lambda terms *)

Definition prop1 :=
  fun (x : A -> B -> C) (y : A -> B) (z : A) => x z (y z).

Definition prop2 :=
  fun (x : B -> A -> C) (y : A -> B) (z : A) => x (y z) z.

Definition prop3 :=
  fun (x : (A -> A) -> A -> A) (y : A) => x (fun z : A => y) y.

Definition prop4 :=
  fun (x : A -> B -> C)
      (y : (A -> B -> C) -> A)
      (z : (A -> B -> C) -> B) =>
        x (y x) (z x).

End proplogic.

Section deptypes.

(* exercise 2 *)
(* complete the following dependently typed lambda terms *)

Definition pred1 :=
  fun (l : Set -> Set)
      (A : Set)
      (B : Set)
      (f : l A -> l B)
      (x : l A) => f x.

Definition pred2 :=
  fun (e : nat -> nat -> Set) (n : nat) => forall m : nat, e n m.

End deptypes.



Section drinker.

(* we will prove (twice) the drinker's paradox:
   in a non-empty domain there is someone such that
   if he drinks then everyone drinks *)

(* use the following *)
Parameter D : Set.
Parameter d : D.
Parameter drinks : D -> Prop.

(* law of excluded middle *)
Parameter em: forall p:Prop, p \/ ~p.

(* double negation law *)
Parameter dn: forall p:Prop, ~~p -> p.

(* first proof *)

(* exercise 3 *)
(* prove the following auxiliary lemma
   use dn twice, first time quite soon *)
Lemma aux : (~ forall x:D, drinks x) -> (exists x:D, ~drinks x).
Proof.
unfold not. intro.
apply dn. unfold not. intro.
apply H. intro x. apply dn. unfold not. intro.
apply H0. exists x. exact H1.
Qed.


(* exercise 4 *)
(* prove the drinker's paradox *)
(* first step: use em to distinguish two cases: everyone drinks or not *)
(* for the second case, use aux *)

Theorem drinker : exists x:D, (drinks x) -> (forall y:D, drinks y).
Proof.
elim em with (forall y : D, drinks y).

intro H. exists d. intro Hd. apply H.

intro H. apply aux in H.
inversion H. exists x. intro H1. elimtype False. destruct H0.
exact H1.
Qed.


(* second proof *)

(* exercise 5 *)
(* give another proof of the drinker
   using dn in the first step and then yet another time *)
Theorem drinker2 : exists x:D, (drinks x) -> (forall y:D, drinks y).
Proof.
apply dn. unfold not. exists d.
exists d. intro. apply dn. unfold not. intro.



End drinker.

Section barber.

(* the barber's paradox:
   it is not the case that
   there is someone who shaves exactly everyone who does not shave himself *)

(* setting *)
Parameter V : Set.
Parameter v : V.
Parameter shaves : V -> V -> Prop.

(* the barber's paradox *)
(* exercise 6: prove the barber's paradox
   use inversion twice,
   and distinguish cases using em: x shaves himself or not *)
Theorem barber : ~ exists x : V ,
  (forall y:V, (shaves x y -> ~ shaves y y))
   /\
  (forall y:V, (~ shaves y y -> shaves x y)).
Proof.
(*! proof *)

Qed.


(* possible additional exercise:
   prove the more elegant formulation of the barber's paradox
   with the forall inside the /\ *)

Theorem barber2 : (*! term *)
  .
Proof.
(*! proof *)

Qed.

End barber.

(*
vim: filetype=coq
*)
