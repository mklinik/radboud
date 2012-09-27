(* cf antwoorden voor laatste opgave *)

Section proplogic.
Parameters A B C : Prop.

(* exercise 1 *)
(* complete the following simply typed lambda terms *)

Definition prop1 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x z (y z). *)
  .

Definition prop2 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x (y z) z. *)
  .

Definition prop3 := (*! term *)
(* fun (x : ?) (y : ?) => x (fun z : A => y) y. *)
  .

Definition prop4 := (*! term *)
(* fun (x : ?) (y : ?) (z : ?) => x (y x) (z x). *)
  .

End proplogic.

Section deptypes.

(* exercise 2 *)
(* complete the following dependently typed lambda terms *)

Definition pred1 := (*! term *)
(* fun (l : Set -> Set) (A : ?) (B : ?) (f : l A -> l B) (x : ?) => f x. *)
  .

Definition pred2 := (*! term *)
(* fun (e : nat -> nat -> ?) (n : ?) => forall m : ?, e n m. *)
  .

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
(*! proof *)

Qed.


(* exercise 4 *)
(* prove the drinker's paradox *)
(* first step: use em to distinguish two cases: everyone drinks or not *)
(* for the second case, use aux *)

Theorem drinker : exists x:D, (drinks x) -> (forall y:D, drinks y).
Proof.
(*! proof *)

Qed.


(* second proof *)

(* exercise 5 *)
(* give another proof of the drinker
   using dn in the first step and then yet another time *)
Theorem drinker2 : exists x:D, (drinks x) -> (forall y:D, drinks y).
Proof.
(*! proof *)

Qed.

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

