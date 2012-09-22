Inductive mynat : Set := Zero : mynat | Succ : mynat -> mynat.

Fixpoint myplus n m : mynat :=
match n with
  | Zero => m
  | Succ p => Succ (myplus p m)
end.

Lemma plus_n_0 : forall n : mynat, myplus n Zero = n.
Proof.
induction n.
(*simpl.*)
reflexivity.

simpl.
rewrite -> IHn.
reflexivity.
Qed.