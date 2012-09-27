(* ***** examples: intuitionistic logic ***** *)

(* The logical connectives for true, false, conjunction,
disjunction are defined by means of inductive predicates.
Roughly the constructors correspond to the introduction
rules, and the induction principle corresponds to the
elimination rules. *)

Print True.
(* True_ind states that P holds if we can prove True from it *)
Check True_ind.

Print False.
(* False_ind gives the elimination rule for False:
any P follows from False *)
Check False_ind.

Parameters A B C : Prop.
Lemma about_false: False -> A.

Proof.
intro x.
elim x.
(* alternative:
elimtype False.
assumption.      *)
(* alternative:
apply False_ind.
assumption.      *)
Qed.

Print and.
Check and_ind.

Lemma about_intro_and : A -> B -> A /\ B.

Proof.
intro x.
intro y.
split.
assumption.
assumption.
(* alternative:
apply conj.
assumption.
assumption.      *)
Qed.

Lemma about_elim_and : A /\ B -> C -> A.
(* elim or apply and_ind *)

Proof.
intros x y.
apply and_ind with A B.
intros.
assumption.
assumption.
Qed.


(* ***** examples: even ***** *)

(* an inductive definition of even *)
Inductive even : nat -> Prop :=
| evenO : even O
| evenSS : forall n:nat , even n -> even (S (S n)).

Check evenO.
Check (even O).
Check (even 1).
Check (evenSS O evenO).
Check (even 2).
Check (evenSS 2 (evenSS O evenO)).
Check (even 4).

(* example *)
Theorem evenzero : (even O).

Proof.
apply evenO.
Qed.

(* example *)
Theorem evenss : forall n:nat , (even n) -> (even (S (S n))).

Proof.
exact evenSS.
Qed.

(*
alternative proof:
intro n.
intro H.
apply evenSS.
exact H.
Qed.
*)


(* ***** examples: le ***** *)

Inductive le (n:nat) : nat -> Prop :=
| le_n : le n n
| le_S : forall m:nat , le n m -> le n (S m).

Check (le O O).
Check (le_n O).
Check (le_n O).
Check (le_S O O (le_n O)).
Definition zero_smaller_than_one := (le_S O O (le_n O)).
Check zero_smaller_than_one.
Check (le_n 7).

(* examples: sorted *)

(* an inductive type for finite lists of natural numbers *)
Inductive natlist : Set :=
| nil : natlist
| cons : nat -> natlist -> natlist.

(* an inductive predicate sorted *)
Inductive sorted : natlist -> Prop :=
| sorted0 : sorted nil
| sorted1 : forall n:nat , sorted (cons n nil)
| sorted2 : forall n h:nat , forall t:natlist ,
            le n h -> sorted (cons h t) -> sorted (cons n (cons h t)).

Check (sorted1 1).
Definition list_one_sorted := (sorted1 1).
Check list_one_sorted.
Check (sorted2 O 1 nil zero_smaller_than_one list_one_sorted).
Definition list_zero_one_sorted := (sorted2 O 1 nil zero_smaller_than_one list_one_sorted).
Check list_zero_one_sorted.




(* ***** examples: inversion ***** *)
Parameter P : nat -> Prop.
Parameter Q : nat -> nat -> Prop.
Parameter R : natlist -> Prop.

Lemma one : forall n : nat, even n -> P n.

Proof.
intros n H.
inversion H.
Abort.

Lemma two : forall n m : nat, le n m -> Q n m.

intros n m H.
inversion H.
Abort.

Lemma three : forall l : natlist, sorted l -> R l.

Proof.
intros l H.
inversion H.
Abort.



(* *************** now the exercises start ********** *)
(* *************** we use definition given above **** *)

(* exercise 1 *)
Theorem even2 : (even 2 ).
Proof.
(*! proof *)

Qed.

(* a few checks *)
Check evenO.
Check evenzero.
Print evenzero.
Check evenSS.
Check even2.
Print even2.

(* exercise 2
   use inversion *)
Theorem noteven1 : ~(even 1).
Proof.
(*! proof *)

Qed.

(* exercise 3
   you may want to use an earlier proved result *)
Theorem even4 : even 4.
Proof.
(*! proof *)

Qed.

(* exercise 4 *)
Theorem noteven3 : ~(even 3).
Proof.
(*! proof *)

Qed.

(* an inductive definition of even and odd *)
Inductive ev : nat -> Prop :=
| evO : ev O
| evS : forall n:nat , odd n -> ev (S n)
with odd : nat -> Prop :=
| oddS : forall n:nat , ev n -> odd (S n).

(* example *)
Theorem evzero : ev O.
Proof.
exact evO.
Qed.

(* example *)
Theorem odd1 : odd 1.
Proof.
exact (oddS O evzero).
Qed.

(* exercise 5 *)
Theorem ev2 : ev 2.
Proof.
(*! proof *)

Qed.

(* exercise 6 *)
Theorem notodd2 : ~ odd 2.
Proof.
(*! proof *)

Qed.

(* exercise 7
   use induction *)
Theorem evorodd : forall n:nat, ev n \/ odd n.
Proof.
(*! proof *)

Qed.

(* exercise 8 *)
Theorem zero_and_zero : le O O.
Proof.
(*! proof *)

Qed.

(* exercise 9 *)
Theorem zero_and_one  : le 0 1.
Proof.
(*! proof *)

Qed.

(* some checks *)
Print zero_and_one.
Check zero_and_one.

(* exercise 10 *)
Theorem one_and_zero : ~ (le 1 0).
Proof.
(*! proof *)

Qed.


(* exercise 11 *)
Theorem sortednil : sorted nil.
Proof.
(*! proof *)

Qed.


(* exercise 12 *)
Theorem sortedone : sorted (cons 0 nil).
Proof.
(*! proof *)

Qed.

(* exercise 13 *)
Theorem sorted_one_two_three :
  sorted (cons 1 (cons 2 (cons 3 nil))).
Proof.
(*! proof *)

Qed.

(* exercise 14 *)
Theorem sorted_tail :
  forall (n : nat) (l : natlist),
  sorted (cons n l) ->
  sorted l.
Proof.
(*! proof *)

Qed.

(* given for exercise 15
   without_last n k l holds if
   n is the last element of k
   and
   l is k without the last element *)
Inductive without_last (n:nat) : natlist -> natlist -> Prop :=
| without_last_one :
  without_last n (cons n nil) nil
| without_last_more :
    forall m:nat, forall l k : natlist,
    without_last n k l -> without_last n (cons m k) (cons m l).

(* exercise 15 *)
(* define a predicate palindrome : natlist -> Prop
   that holds exactly if the input list is equal to its reverse.
   use three clauses: for the empty list, for a list of one
   element, for a list of two or more elements *)

Inductive palindrome : natlist -> Prop := (*! term *)
  .


