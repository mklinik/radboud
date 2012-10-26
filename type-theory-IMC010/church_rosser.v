Definition relation (A : Type) := A -> A -> Prop.

(** The syntax {param} declares the parameter [param] as implicit. In the case
of [rtc], it allows one to write [rtc R x y] instead of [rtc A R x y]. *)

(** The reflexive transitive closure of a relation A. **)
(** This signature reads: if R is a binary relation on type A, then it's
reflexive transitive closure is also a binary relation on A **)
Inductive rtc {A} (R : relation A) : relation A :=
  (** The first constructor reads: every A is in reflexive-transitive closure
  to itself, no matter what the relation actually is **)
  | rtc_refl x : rtc R x x
  (** The second constructor reads: given that x is in relation R to y and  y
  is in reflexive transitive closure to z then x is in rtc to z (one step plus
  many steps is again many steps) **)
  | rtc_l x y z : R x y -> rtc R y z -> rtc R x z.

Print rtc_ind.

(** The diamond property, in general **)
(** This definition reads: Two relations R and T together have the diamond property if:

        x
     R / \ T
      /   \
    y1     y2
      \   /
     T \ / R
        z

If x R y1 and x T y2 there exists a z such that y1 T z and y2 R z.
**)
Definition gen_dp {A} (R T : relation A) := forall x y1 y2,
  R x y1 -> T x y2 -> exists z, T y1 z /\ R y2 z.

(** To prove: the diamond property is symmetrical.  **)
Lemma gen_dp_sym {A} (R T : relation A) :
  gen_dp R T -> gen_dp T R.
Proof.
unfold gen_dp.
intro.
intro.
intro.
intro.
intro.
intro.
elim H with x y2 y1.
intro.
intro.
exists x0.
split.
elim H2.
intros.
assumption.
elim H2.
intros.
assumption.
assumption.
assumption.
Qed.

(** Relation A has the diamond property if it has the generic diamond property
with itself **)
Definition dp {A} (R : relation A) := gen_dp R R.

(** Relation A is church-rosser if it's reflexive-transitive closure has the
diamond property. **)
Definition cr {A} (R : relation A) := dp (rtc R).

Lemma rtc_reverse {A} (R : relation A) (x y : A) : R x y -> rtc R x y.
Proof.
intros.
apply (rtc_l R x y).
assumption.
apply (rtc_refl).
Qed.



(** If two relation R, T have the generic diamond property together, then the
reflexive-transitive closure of R and T have the generic diamond property.
**)
Lemma rtc_gen_dp {A} (R T : relation A) :
  gen_dp R T -> gen_dp (rtc R) T.
Proof.



unfold gen_dp.
intro.
intros x y1 y2.

intros.

elim H with x y1 y2.
intro.
intro.
Focus 2.
apply (rtc_reverse R x y1).

induction H0.
exists y2.
split.
assumption.
apply rtc_refl.

apply IHrtc.

inversion H0.
exists y2.
split.
subst y1.
assumption.
apply rtc_refl.

exists y.
split.
Focus 2.
apply (rtc_l R y2 z).



Qed.

Lemma dp_cr {A} (R : relation A) :
  dp R -> cr R.
Proof.
  (* exercise *)
Qed.

(* vim: ft=coq
*)
