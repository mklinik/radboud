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

(** The diamond property holds for any binary relation WTF? **)
Definition gen_dp {A} (R T : relation A) := forall x y1 y2,
  R x y1 -> T x y2 -> exists z, T y1 z /\ R y2 z.

Lemma gen_dp_sym {A} (R T : relation A) :
  gen_dp R T -> gen_dp T R.
Proof.
  (* exercise *)
Qed.

Definition dp {A} (R : relation A) := gen_dp R R.
Definition cr {A} (R : relation A) := dp (rtc R).

Lemma rtc_gen_dp {A} (R T : relation A) :
  gen_dp R T -> gen_dp (rtc R) T.
Proof.
  (* exercise *)
Qed.

Lemma dp_cr {A} (R : relation A) :
  dp R -> cr R.
Proof.
  (* exercise *)
Qed.

