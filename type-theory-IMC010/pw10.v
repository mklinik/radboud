(* ************************************************* *)
(* introduction and examples                         *)
(* ************************************************* *)


(* example of a polymorphic function: identity *)

(* first the identity on natural numbers *)
Definition natid : nat -> nat :=
  fun n : nat => n.
Check (natid O).
Eval compute in (natid O).

(* then the identity on booleans *)
Definition boolid: bool -> bool :=
  fun b : bool => b.
Check (boolid true).
Eval compute in (boolid true).

(* now the polymorphic identity *)
Definition polyid : forall A : Set, A -> A :=
  fun (A : Set) => fun (a : A) => a.
Check polyid.

Check (polyid nat).
Check (polyid nat O).
Eval compute in (polyid nat O).
Check (polyid _ O).

Check (polyid bool).
Check (polyid bool true).
Eval compute in (polyid bool true).
Check (polyid _ true).

(* the following does not work:
Check (polyid _).
because Coq cannot infer what should be on the place of _ *)

(* notation *)
Notation id := (polyid _).
Check (id O).
Eval compute in (id O).
Check (id true).
Eval compute in (id true).



(* example of a polymorphic inductive definition: polylists *)

(* first the lists of natural numbers *)
Inductive natlist : Set :=
  natnil : natlist
| natcons : nat -> natlist -> natlist.

(* the list 2;1;0 *)
Check (natcons 2 (natcons 1 (natcons 0 natnil))).

(* then the lists of booleans *)
Inductive boollist : Set :=
  boolnil : boollist
| boolcons : bool -> boollist -> boollist.

(* now the polymorphic lists *)
Inductive polylist (X : Set) : Set :=
    polynil  : polylist X
  | polycons : X -> polylist X -> polylist X.

Check polylist.
Check (polylist nat).
Check (polylist bool).

Check polynil.
Check (polynil nat).
Check (polynil bool).
Check polycons.
Check (polycons nat).
Check (polycons bool).

(* again the list 2;1;0 *)
Check (polycons nat 2 (polycons nat 1 (polycons nat 0 (polynil nat)))).

(* we introduce some handy notation *)
Notation ni := (polynil _).
Notation co := (polycons _).
(* and write the list 2;1;0 again *)
Check (co 2 (co 1 (co 0 ni))).
Check (co true (co false ni)).

(* induction principle for polymorphic lists;
   more about this later *)
Check polylist_ind.

(* length of lists of natural numbers *)
Fixpoint natlength (l:natlist) {struct l} : nat :=
  match l with
    natnil => O
  | natcons h t => S (natlength t)
  end.

Eval compute in (natlength (natcons 2 (natcons 1 (natcons 0 natnil)))).

(* length of polymorphic lists *)
Fixpoint polylength (A:Set)(l:(polylist A)){struct l} : nat :=
  match l with
    polynil => O
  | polycons h t => S (polylength A t)
  end.

(* NB: in the recursive definition,
   polynil and polycons do _not_ get an argument A left of =>;
   they do get such arguments right of => *)

Eval compute in (polylength nat (polycons nat 2 (polycons nat 1 (polycons nat 0 (polynil nat))))).
Eval compute in (polylength nat (co 2 (co 1 (co 0 ni)))).

Eval compute in (polylength bool (polycons bool true (polycons bool false (polynil bool)))).
Eval compute in (polylength bool (co true (co false ni))).

(* remark1 about Coq notation:
   In the definition of polylist
   we use a parameter declaration (X:Set).
   Alternatively, we could define a type Set -> Type
   as follows: *)
Inductive polylistb : Set -> Type :=
  polynilb : forall X:Set, polylistb X
| polyconsb: forall X:Set, X -> polylistb X -> polylistb X.

(* but then we get a quite strange induction predicate *)
Check polylistb_ind.

(* so we usually take the first definition *)




(* ************************************************* *)
(*        start of the  exercises                    *)
(* ************************************************* *)

(*        exercises about polymorphic lists          *)
(*        see pw04 for similar exercises on natlists *)

(* exercise 1 *)
(* give the definition of polyappend for polymorphic lists *)
(* test it using Eval compute on an example *)

Fixpoint polyappend (X:Set) (k l : polylist X) {struct k} : (polylist X) :=
    (*! term *)
  .

Eval compute in (*! term *)
  .

(* prove the following lemma, to be used later*)
Lemma append_nil :
  forall X:Set, forall l: polylist X,
  polyappend X l (polynil X) = l.
Proof.
(*! proof *)

Qed.


(* exercise 3 *)
(* prove the following lemma, to be used later *)
Lemma append_assoc : forall X, forall k l m,
  (polyappend X (polyappend X k l) m) = (polyappend X k (polyappend X l m)).
Proof.
(*! proof *)

Qed.


(* exercise 4 *)
(* prove the following lemma *)
Lemma length_append :
 forall X:Set, forall k l : (polylist X),
  polylength X (polyappend X k l) = plus (polylength X k) (polylength X l).
Proof.
(*! proof *)

Qed.


(* exercise 5 *)
(* give the definition of polyreverse for polymorphic lists *)
(* test it using Eval compute on an example *)

Fixpoint polyreverse (X:Set) (l : polylist X) {struct l} :(polylist X) :=
    (*! term *)
  .


(* exercise 6 *)
(* prove the following lemma *)
Lemma reverse_append :
  forall X:Set, forall k l: (polylist X),
  polyreverse X (polyappend X k l) =
  polyappend X (polyreverse X l) (polyreverse X k).
Proof.
(*! proof *)

Qed.


(* exercise 7 *)
(* prove the following lemma *)
(* this does not correspond to a lemma in pw04 *)
Lemma rev_cons_app :
  forall X:Set, forall k l : (polylist X), forall x:X,
  polyappend X (polyreverse X (polycons X x k)) l =
  polyappend X (polyreverse X k) (polycons X x l).
Proof.
(*! proof *)

Qed.



(*        exercises about polymorphic pairs         *)



(* definition of pairs of natural numbers *)
Inductive natprod : Set :=
| natpair : nat -> nat -> natprod.

Check natpair.
Check natpair 1.
Check (natpair 1 2).

Check natprod_ind.

(* we define the first and second projection *)

Definition natprodfirst (p : natprod) : nat :=
match p with
| natpair x y => x
end.
Eval compute in (natprodfirst (natpair 1 2)).

Definition natprodsecond (p : natprod) : nat :=
match p with
| natpair x y => y
end.
Eval compute in (natprodsecond (natpair 1 2)).

(* exercise 8 *)
(* give a definition prod of polymorphic pairs
   where the first element comes from a set X
   and the second element comes from a set Y    *)

Inductive prod (X Y :Set) : Set := (*! term *)
  .

(* exercise 9 *)
(* give definitions of the first and second projection *)

Definition fst (X Y : Set) (p: prod X Y) : X := (*! term *)
  .

Definition snd (X Y : Set) (p : prod X Y) : Y := (*! term *)
  .


(*        exercises about polymorphic trees       *)



(* definition of natbintrees
   with labels on the nodes  *)
Inductive natbintree : Set :=
    natleaf : natbintree
  | natnode : nat -> natbintree -> natbintree -> natbintree.

Check natleaf.
Check (natnode 1 natleaf natleaf).
Check (natnode 2 (natnode 1 natleaf natleaf) (natnode 3 natleaf natleaf)).

(* the last is the tree
               2
              / \
             1   3
*)

(* exercise 10 *)
(* give the definition polybintree with constructors polyleaf and polynode
   of polymorphic binary trees
   with labels on the nodes
   you may introduce handy notation *)

Inductive polybintree (X : Set) : Set := (*! term *)
  .



(* exercise 11 *)
(* complete the definition of polyflatten
   polyflatten puts the labels of a polybintree from left to right in a polylist *)

Fixpoint polyflatten (X:Set) (t:polybintree X) {struct t} : (polylist X) :=
  (*! term *)
(*
match t with
| polyleaf =>
| polynode x l r =>
end
*)
  .

(* perform some tests using the trees above *)

