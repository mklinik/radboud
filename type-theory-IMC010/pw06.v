(*  *********************************************** *)
(*                  example pred1                   *)
(*  *********************************************** *)

Inductive even : nat -> Prop :=
| evenO : even O
| evenSS : forall n:nat ,
           even n -> even (S (S n)) .

Print le.

Theorem example:
  forall n:nat,
  (even n) \/ (even (S n)).

Proof.
intro n.
induction n.

left.
apply evenO.

inversion IHn.
right.
apply evenSS.
assumption.
left.
assumption.
Qed.


(* We consider successor (short), predecessor (short),
   mirror (still quite short), sort (longer).        *)



(*  *********************************************** *)
(*                      successor                   *)
(*  *********************************************** *)


(* NB:
   here we use the notation for existential quantification in Set *)

(* example *)
Theorem successor:
  forall n:nat, {m:nat | m = S n} .

Proof.
intro n.
(* NB: we do not need induction *)
(* The following step corresponds to an
   application of the introduction rule for
   existential quantification, read upwards.
   Note how the goal is transformed.          *)
exists (S n).
reflexivity.
Qed.

Goal forall n:nat, exists m:nat, m = S n.
intro.
exists (S n).
reflexivity.
Qed.

(* we see the main part of the extracted program. *)
Extraction successor.

(* The extracted program is written to a file. *)
Extraction "suc" successor.
(* The files are saved in your directory on prover
   and can be seen using load. *)

(* How can we use the extracted program ?
   (On your vu-account, not on prover.)
   type "ocaml" to activate the ocaml toplevel compiler
   this gives a prompt #
   #use "suc.ml" ;;
   makes that the file suc.ml is loaded
   NB: #is not the prompt but #use is the command !
   NB: the "" are necessary
   you can use the program by saying for instance
   successor O ;;
   NB: the round thing is capital-o
   NB: every command should end with ;;
   exit with  #quit ;;
   NB: again # is not the prompt, and ;; are necessary  *)



(*  *********************************************** *)
(*                     predecessor                  *)
(*  *********************************************** *)

(* exercise 1 *)
(* A possibility is to proceed by induction on n.
   Use False elimination to establish the case for n=0. *)

Theorem predecessor :
  forall n:nat, ~(n=O) -> {m:nat | S m = n}.
(*  forall n:nat, ~(n=O) -> exists m:nat, S m = n.*)
Proof.
unfold not.
intro n.
intros.
induction n.
elimtype False.
apply H.
reflexivity.

exists n.
reflexivity.
Qed.

(* extraction of the program *)
Extraction predecessor.
Extraction "pred" predecessor.



(*  *********************************************** *)
(*                        mirror                    *)
(*  *********************************************** *)

(* datatype for binary trees with labels on the leafs *)
Inductive bintree : Set :=
  | leaf : nat -> bintree
  | node : bintree -> bintree -> bintree.

(* examples of trees *)
Definition tree1 := leaf 1.
Definition tree2 := node (leaf 1) (leaf 2).
Definition tree3 := node (node (leaf 1) (leaf 2)) (leaf 3).

(* specification *)
Inductive Mirrored : bintree -> bintree -> Prop :=
  | Mirrored_leaf : forall n : nat, Mirrored (leaf n) (leaf n)
  | Mirrored_node :
      forall b b' c c' : bintree,
      Mirrored b b' -> Mirrored c c' -> Mirrored (node b c) (node c' b').

(* exercise 2 *)
(* The theorem from which we will extract a program. *)
(* A possibility is to proceed by induction and to use
   inversion on the induction hypotheses.              *)
Theorem Mirror :
  forall t : bintree,
  {t' : bintree | Mirrored t t'}.
Proof.
intro t.
induction t.
exists (leaf n).
apply Mirrored_leaf.

(* magic happens here *)
inversion IHt1.
inversion IHt2.

exists (node x0 x).
apply Mirrored_node.
exact H.
exact H0.
Qed.

Extraction Mirror.
Extraction "mirror" Mirror.

(* Alternative approach: we give an implementation ...*)
Fixpoint mirror (t : bintree) : bintree :=
match t with
  | leaf n => leaf n
  | node l r => node (mirror r) (mirror l)
end.

(* exercise 3 *)
(* ... and prove its correctness *)
Theorem Mirrored_mirror : forall t : bintree, Mirrored t (mirror t).
Proof.
intro t.
induction t.
simpl.
apply Mirrored_leaf.

simpl.
apply Mirrored_node.
exact IHt1.
exact IHt2.
Qed.

(*  *********************************************** *)
(*                  insertion sort                  *)
(*  *********************************************** *)

(* We will use properties concerning
   less-than-or-equal-to from the coq libary. *)
Require Import Arith.

(* the datatype *)
Inductive natlist : Set :=
  | nil : natlist
  | cons : nat -> natlist -> natlist.

(* specification *)
(* The following predicate was also in practical work 5. *)
Inductive sorted : natlist -> Prop :=
  (* the empty list is sorted *)
| sorted0 : sorted nil
  (* the singleton list is sorted *)
| sorted1 : forall n:nat , sorted (cons n nil)
  (* if n <= h and [h:t] is sorted, so is [n:h:t] *)
| sorted2 : forall n h:nat , forall t:natlist ,
            le n h -> sorted (cons h t) -> sorted (cons n (cons h t)).

(* week 5 exercise 11 *)
Theorem sortednil : sorted nil.
Proof.
apply sorted0.
Qed.

(* week 5 exercise 12 *)
Theorem sortedone : sorted (cons 0 nil).
Proof.
apply (sorted1 0).
Qed.

(* week 5 exercise 13 *)
Theorem sorted_one_two_three :
  sorted (cons 1 (cons 2 (cons 3 nil))).
Proof.
apply (sorted2 1 2).
apply le_S.
apply le_n.

apply (sorted2 2 3).
apply le_S.
apply le_n.

apply (sorted1 3).
Qed.

(* week 5 exercise 14 *)
Theorem sorted_tail :
  forall (n : nat) (l : natlist),
  sorted (cons n l) ->
  sorted l.
Proof.
intros.
induction l.
apply sorted0.
inversion H.
exact H4.
Qed.


(* Inserted n l i expresses
   that the list i equals the list l with n inserted somewhere. *)
Inductive Inserted (n : nat) : natlist -> natlist -> Prop :=
  | Inserted_front :
      forall l : natlist, Inserted n l (cons n l)
  | Inserted_cons :
      forall (m : nat) (l l' : natlist),
      Inserted n l l' -> Inserted n (cons m l) (cons m l').

(* exercise 4 *)
Theorem exercise1_Inserted : Inserted 1 nil (cons 1 nil).
Proof.
apply Inserted_front.
Qed.

(* exercise 5 *)
Theorem exercise2_Inserted : Inserted 1 (cons 1 nil) (cons 1 (cons 1 nil)) .
Proof.
apply Inserted_cons.
apply Inserted_front.
Qed.

(* exercise 6: same theorem but give a different proof *)
Theorem exercise3_Inserted : Inserted 1 (cons 1 nil) (cons 1 (cons 1 nil)) .
Proof.
apply Inserted_front.
Qed.

(* exercise 7 *)
Theorem exercise4_Inserted : ~ Inserted 1 nil (cons 2 nil).
Proof.
unfold not.
intros.
inversion H.
Qed.

Inductive Permutation : natlist -> natlist -> Prop :=
  | Permutation_nil : Permutation nil nil
  | Permutation_cons :
      forall (n : nat) (k l m : natlist),
      Permutation k l -> Inserted n l m -> Permutation (cons n k) m.

(* exercise 8 *)
(* Hint: use inversion_clear several times.
   Same as inversion but with less "pollution" of hypotheses. *)
Lemma Permutation_neg:
  ~(Permutation (cons 1 (cons 2 nil)) (cons 2 (cons 3 nil))).
Proof.
unfold not.
intro.
inversion_clear H.
inversion_clear H1.
inversion_clear H.
inversion_clear H1.
Qed.

(* exercise 9 *)
(* Hint1: use "apply ... with ..." if you have to provide Coq
   with the missing argument. See as an example the first step
   of the following proof, and try to see the error-message in
   case you leave out the "with ..." part.
   Hint2: if you want to apply the constructor of an inductive
   type, you can use the tactic "constructor".
   For instance, instead of "apply Permutation_nil"
   you can use "constructor".                          *)
Lemma Permutation_123:
  Permutation (cons 1 (cons 2 (cons 3 nil))) (cons 3 (cons 2 (cons 1 nil))).
Proof.
apply Permutation_cons with (cons 3 (cons 2 nil)).
apply Permutation_cons with (cons 3 nil).
apply Permutation_cons with nil.
apply Permutation_nil.
apply Inserted_front.
apply Inserted_cons.
apply Inserted_front.
apply Inserted_cons.
apply Inserted_cons.
apply Inserted_front.
Qed.

(* exercise 10 *)
(* The following lemma will be used
   in the proof of the lemma Insert.
   Use induction on l and the "apply ... with ...". *)
Lemma Permutation_refl :
  forall (l : natlist), Permutation l l.
Proof.
intro.
induction l.
apply Permutation_nil.
apply Permutation_cons with l.
apply IHl.
apply Inserted_front.
Qed.

(* We use an auxiliary notion.
   Lowerbound n l expresses that
   n <= m for all elements m of l. *)
Inductive Lowerbound (n : nat) : natlist -> Prop :=
  | Lowerbound_nil : Lowerbound n nil
  | Lowerbound_cons :
      forall (m : nat) (l : natlist),
      n <= m -> Lowerbound n l -> Lowerbound n (cons m l).

(* exercise 11 *)
(* The following lemma is used in the proof of Insert. *)
(* You can use the tactic "rewrite". *)
Lemma Lowerbound_sorted :
  forall (l : natlist) (n : nat),
  Lowerbound n l ->
  sorted l ->
  sorted (cons n l).
Proof.
intros.
induction l.
apply sorted1.
apply sorted2.
Focus 2.
exact H0.
inversion_clear H.
assumption.
Qed.

(* In the proof of the lemma sorted_Lowerbound we will use
   le_trans from the Coq library.
   It is a lemma expressing transitivity of the relation le. *)

Check le_trans.

(* exercise 12 *)
(* complete the following proof; use "apply le_trans with ..." *)
Lemma sorted_Lowerbound :
  forall (l : natlist) (n : nat),
  sorted (cons n l) -> Lowerbound n l.
Proof.
intros.
inversion_clear H.
apply Lowerbound_nil.
apply Lowerbound_cons.
exact H0.
induction t.
apply Lowerbound_nil.
inversion_clear H1.
apply Lowerbound_cons.
apply le_trans with h.
exact H0.
exact H.

apply IHt.

Lemma foo: forall m n:nat, n <= m -> forall l : natlist, sorted (cons m l) -> sorted (cons n l).
Proof.
intros.
inversion_clear H0.
apply sorted1.
apply sorted2.
apply le_trans with m.
exact H.
exact H1.
exact H2.
Qed.

apply foo with n0.
exact H.
exact H2.
Qed.

(* given *)
Lemma Inserted_Lowerbound :
  forall (l l' : natlist) (n m : nat),
  n <= m ->
  Inserted m l l' ->
  Lowerbound n l ->
  Lowerbound n l'.
Proof.
induction l.
intros.
inversion_clear H0.
apply Lowerbound_cons.
exact H.
exact H1.
intros k p m H H0 H1.
inversion_clear H0.
apply Lowerbound_cons.
exact H.
exact H1.
inversion_clear H1.
apply Lowerbound_cons.
exact H0.
apply IHl with m.
exact H.
exact H2.
exact H3.
Qed.

(* given *)
Lemma Permutation_Lowerbound :
  forall (l l' : natlist) (n : nat),
    Permutation l l' -> Lowerbound n l -> Lowerbound n l'.
Proof.
induction l.
intros k n H H0.
inversion_clear H.
exact H0.
intros k m H H0.
inversion_clear H.
inversion_clear H0.
apply Inserted_Lowerbound with l' n.
exact H.
exact H2.
apply IHl.
exact H1.
exact H3.
Qed.

(* given *)
Lemma Insert :
  forall (l : natlist) (n : nat),
  sorted l -> {i : natlist | Inserted n l i /\ sorted i}.
Proof.
induction l.

(* case nil *)
intros n H.
exists (cons n nil).
split.
apply Inserted_front.
apply sorted1.

(* case (cons n l) *)
intros m H.
elim (le_lt_dec m n).

intro I.
exists (cons m (cons n l)).
split.
apply Inserted_front.
apply sorted2.
exact I.
exact H.

intro I.
elim IHl with m.
intros i' H1.
elim H1.
intros H2 H3.
exists (cons n i').
split.
apply Inserted_cons.
exact H2.
apply Lowerbound_sorted.
apply Permutation_Lowerbound with (cons m l).
apply Permutation_cons with l.
apply Permutation_refl.
exact H2.
apply Lowerbound_cons.
apply lt_le_weak.
exact I.
apply sorted_Lowerbound.
exact H.
exact H3.
inversion_clear H.
apply sorted0.
exact H1.
Qed.

Extraction Insert.

(* exercise 13 *)
(* Use induction on l.
   In the induction step, after two inversions,
   use "elim Insert with x n." or parameters
   appropriate to your development for x and n. *)
Theorem Sort :
  forall l : natlist, {l' : natlist | Permutation l l' /\ sorted l'}.
Proof.
(*! proof *)

Qed.

Extraction Sort.
Extraction "insertsort" Sort.



(* Now the implementation and its correcness.
   The remainder is just for reading/curiosity. *)
Fixpoint insert (n : nat) (l : natlist) {struct l} : natlist :=
  match l with
  | nil => cons n nil
  | cons m k =>
      match le_lt_dec n m with
      | left _ => cons n (cons m k)
      | right _ => cons m (insert n k)
      end
  end.

Fixpoint sort (l : natlist) : natlist :=
  match l with
  | nil => nil
  | cons m k => insert m (sort k)
  end.

(* correctness of the implementation *)
Lemma Inserted_insert :
  forall (n : nat) (l : natlist), Inserted n l (insert n l).

Proof.
induction l.

simpl.
apply Inserted_front.

simpl.
elim (le_lt_dec n n0).
intro.
apply Inserted_front.
intro.
apply Inserted_cons.
exact IHl.
Qed.

Lemma Lowerbound_insert :
  forall (l : natlist) (n m : nat),
  m <= n -> Lowerbound m l -> Lowerbound m (insert n l).

Proof.
induction l.

intros n m H H0.
simpl.
apply Lowerbound_cons.
exact H.
exact H0.

intros n' m H H0.
simpl.
elim (le_lt_dec n' n).
intro H1.
apply Lowerbound_cons.
exact H.
exact H0.
intro H1.
inversion_clear H0.
apply Lowerbound_cons.
exact H2.
apply IHl.
exact H.
exact H3.
Qed.

Lemma sorted_insert :
  forall (l : natlist) (n : nat), sorted l -> sorted (insert n l).

Proof.
induction l.

intros n H.
simpl.
apply sorted1.

intros m H.
simpl.
elim (le_lt_dec m n).
intro H0.
apply sorted2.
exact H0.
exact H.
intro H0.
apply Lowerbound_sorted.
apply Lowerbound_insert.
apply lt_le_weak.
exact H0.
apply sorted_Lowerbound.
exact H.
apply IHl.
inversion_clear H.
apply sorted0.
exact H2.
Qed.

Theorem Permutation_sort :
  forall l : natlist, Permutation l (sort l).

Proof.
induction l.
simpl.
apply Permutation_nil.
simpl.
apply Permutation_cons with (sort l).
exact IHl.
apply Inserted_insert.
Qed.

Theorem sorted_sort :
  forall l : natlist, sorted (sort l).

Proof.
induction l.
simpl.
apply sorted0.
simpl.
apply sorted_insert.
exact IHl.
Qed.
(*
vim: filetype=coq
*)
