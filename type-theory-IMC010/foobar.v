
Inductive foobar : nat -> Prop :=
 | foob : forall n:nat, foobar n.

Goal foob (3 + 4) = foob (4 + 3).
Proof.
simpl. reflexivity.
Qed.

Goal foobar (3 + 4) = foobar (4 + 3).
Proof.
simpl. reflexivity.
Qed.

Theorem plus_comm : forall n m, n + m = m + n.
Proof.
Admitted.

Goal foobar (3 + 4) = foobar (4 + 3).
Proof.
rewrite plus_comm. reflexivity.
Qed.


Goal foob (3 + 4) = foob (4 + 3).
Admitted.
(*
Proof.
rewrite plus_comm.
reflexivity.
*)

Goal forall i k, foobar (i + k) = foobar (k + i).
Proof.
intros i k.
rewrite plus_comm.
reflexivity.
Qed.

(*
Goal forall i k, foob (i + k) = foob (k + i).
*)