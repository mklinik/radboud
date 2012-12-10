(* Huiswerk for Type Theory and Coq 2012

   Markus Klinik

   Satisfiability of propositional formulas
*)

Require Import Coq.Lists.List.
Require Import Coq.Arith.EqNat.


(* Formulas of propositional logic are represented in a straight-forward
   abstract syntax.  The only interesting case is the variable case, which is
   represented simply by a natural number.

*)
Inductive form : Set :=
 | f_var : nat -> form
 | f_and : form -> form -> form
 | f_or  : form -> form -> form
 | f_imp : form -> form -> form
 | f_neg : form -> form
.


(* Models are represented by lists of natural numbers.  We interpret them as
   lists of variables.

   If a variable is an element of a model, it's truth value is 'true'.
   If a variable is not an element of a model, it's truth value is 'false'.

   This way we can query the truth value of a propositional variable in a given
   model by checking for elementship in the list.

*)
Definition model := list nat.


(* Helper function for find_variables

   Straight forward depth-first recursion on the abstract syntax tree.  We just
   collect all variables that we encounter in a list.  If a variable occurs
   multiple times in a formula, then it occurs multiple times in the resulting
   list.  This doesn't matter, because we're only interested in the fact
   whether a variable is in a list at all.

*)
Fixpoint find_variables' (f:form) (vars:list nat) : list nat :=
 match f with
 | f_var x   => cons x vars
 | f_and l r => find_variables' r (find_variables' l vars)
 | f_or  l r => find_variables' r (find_variables' l vars)
 | f_imp l r => find_variables' r (find_variables' l vars)
 | f_neg g   => find_variables' g vars
 end
.

(* Returns a list of all variables that occur in a formula. *)
Definition find_variables : form -> list nat :=
 fun (f:form) => find_variables' f nil.


(* some examples *)

Eval compute in find_variables (f_and (f_var 1) (f_var 2)).

Example findvars_var :
 find_variables (f_var 42) = 42 :: nil.
Proof. reflexivity. Qed.

Example findvars_and :
 find_variables (f_and (f_var 1) (f_var 2)) = 2 :: 1 :: nil.
Proof. reflexivity. Qed.

Example findvars_or :
 find_variables (f_or (f_var 1) (f_var 2)) = 2 :: 1 :: nil.
Proof. reflexivity. Qed.

Example findvars_imp :
 find_variables (f_imp (f_var 1) (f_var 2)) = 2 :: 1 :: nil.
Proof. reflexivity. Qed.

Example findvars_not :
 find_variables (f_neg (f_var 42)) = 42 :: nil.
Proof. reflexivity. Qed.

Example findvars_all :
 find_variables (f_and (f_or (f_neg (f_var 1)) (f_var 2)) (f_imp (f_var 3) (f_var 4)))
  = 4 :: 3 :: 2 :: 1 :: nil.
Proof. reflexivity. Qed.


(* Checks whether the given number x is an element of the list l *)
Definition elem_nat (x:nat) (l:list nat) : bool :=
 existsb (fun y => beq_nat x y) l.


(* Given a model and a formula f, checks whether the model satisfies the
   formula.

   This is your standard way of defining the denotational semantics of an
   object language in the host language.  It corresponds nicely to the
   definition of truth in your favourite book about logic, where the meaning of
   the connectives are explained in terms of their meta-language equivalents.

   The Coq standard library has all the sentential connectives we need.  The
   only interesting case is the variable case.  We define the truth value of a
   variable in a given model to be the elementship of the variable in a list.

*)
Fixpoint models (f:form) (m:model) : bool :=
 match f with
 | f_var x   => elem_nat x m
 | f_and l r => andb  (models l m) (models r m)
 | f_or  l r => orb   (models l m) (models r m)
 | f_imp l r => implb (models l m) (models r m)
 | f_neg g   => negb  (models g m)
 end
.


(* some examples *)

Example models_var :
 models (f_var 1) (1::nil) = true.
Proof. reflexivity. Qed.

Example models_and :
 models (f_and (f_var 2) (f_var 1)) (1::2::nil) = true.
Proof. reflexivity. Qed.

Example not_models_and :
 models (f_and (f_var 2) (f_var 1)) (1::nil) = false.
Proof. reflexivity. Qed.

Example model_or :
 models (f_or (f_var 42) (f_var 1)) (1::nil) = true.
Proof. reflexivity. Qed.

Example not_models_or :
 models (f_or (f_var 42) (f_var 43)) (1::2::nil) = false.
Proof. reflexivity. Qed.

Example models_imp :
 models (f_imp (f_var 1) (f_var 1)) nil = true.
Proof. reflexivity. Qed.

Example not_models_imp :
 models (f_imp (f_var 1) (f_var 42)) (1::nil) = false.
Proof. reflexivity. Qed.

Example models_neg :
 models (f_neg (f_var 42)) nil = true.
Proof. reflexivity. Qed.

Example not_models_neg :
 models (f_neg (f_var 1)) (1::nil) = false.
Proof. reflexivity. Qed.


(* Returns all possible sublists of a given list

   Similar to the powerset of a set.

   All sublists of the empty list [] is the singleton list containing the empty
   list [[]].

   All sublists of a non-empty list [h:t] are all sublists of the tail, one
   time with the head, and one time without.

*)
Fixpoint sublists {A:Type} (l:list A) : (list (list A)) :=
 match l with
 | nil    => nil::nil
 | h :: t => let subtails := (sublists t) in app (map (cons h) subtails) subtails
 end
.

Example sublists_1 :
 sublists (1::nil) = (1::nil)::(nil)::nil.
Proof. reflexivity. Qed.

Example sublists_123 :
 sublists (1::2::3::nil) =
  (1::2::3::nil) ::
  (1::2   ::nil) ::
  (1   ::3::nil) ::
  (1      ::nil) ::
  (   2::3::nil) ::
  (   2   ::nil) ::
  (      3::nil) ::
  (         nil) :: nil.
Proof. reflexivity. Qed.

Definition nilnat : list nat := nil.
Example sublists_nil :
 sublists nilnat = nil::nil.
Proof. reflexivity. Qed.


(* Returns `Some m` if there is a model m satisfying the given formula, `None`
   otherwise.

   With all the definitions above, the definition of find_model is rather
   concise now.  We first compute all possible models of a given formula, that
   is all possible combinations of truth values of the variables occuring in
   the formula.  With our definition of truth, this corresponds to the list of
   all sublists of the variables of the formula.  Then, we let `find` pick the
   first model that satisfies the formula.  If none of the models satisfies the
   formula, `find` returns `None`.

   Note that find_variables might produce a list where the same variable occurs
   multiple times.  find_model thus might try out models that are logically
   equivalent.  This doesn't affect correctness of the method, and as we are
   interested in concise formulations rather than run-time performance here, it
   will do.

*)
Definition find_model (f:form) : option model :=
 find (models f) (sublists (find_variables f)).


Example findmodel_var :
 find_model (f_var 42) = Some (42::nil).
Proof. reflexivity. Qed.

Example findmodel_fail :
 find_model (f_and (f_var 1) (f_neg (f_var 1))) = None.
Proof. reflexivity. Qed.


(* Proof that if `find P l` returns `Some x` then `P x` holds.

   By induction on the list l.

   In an empty list, `find` never returns `Some x`, this case is vacuously
   true.

   If `find P l` returns `Some x` in a non-empty list [h:t] then there are two
   cases:

   Either the result is `Some h`, then `P h` holds, by definition of find.

   Or `P h` doesn't hold, then `find` returns `Some x` for an element of the
   tail, and by the induction hypothesis, P holds for that x.

*)
Lemma found_x_satisfies_predicate :
 forall (A:Type) (P:A -> bool) (l:list A) (x:A), find P l = Some x -> P x = true.
Proof.
intros A P. induction l as [| h t].
(* l = nil *)
intros x H. inversion H.
(* l = h::t *)
intros x H. inversion H.
(* 'Some x' comes either from the 'then' part, or from the 'else' part *)
remember (P h) as Ph. destruct (Ph).
(* 'then' part: P holds for h in the first place. We only have to convince Coq. *)
assert (x = h). inversion H1. reflexivity. rewrite H0. symmetry.  exact HeqPh.
(* 'else' part: P holds for some element in the tail. But that's just the
   induction hypothesis. *)
apply IHt. exact H1.
Qed.


(* Proof that if find_model actually returns some model, this model indeed
   satisfies the formula.

   The proof is rather simple, because find_model is defined in terms of `find`
   and `models`, and we already proved that if `find` returns some element,
   then the property holds for that element.
*)
Theorem find_model_works :
 forall (f:form) (m:model), find_model f = Some m -> models f m = true.
Proof.
intros f m. unfold find_model.
apply found_x_satisfies_predicate.
Qed.

(*
vim: ft=coq
*)
