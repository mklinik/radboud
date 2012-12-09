(* Huiswerk for Type Theory and Coq 2012

  Markus Klinik

  Satisfiability of propositional formulas
*)

Require Import Coq.Lists.List.
Require Import Coq.Arith.EqNat.

Inductive form : Set :=
 | f_var : nat -> form
 | f_and : form -> form -> form
 | f_or  : form -> form -> form
 | f_imp : form -> form -> form
 | f_neg : form -> form
 .

(* If a nat is contained in the model, it's truth value is 'true'
   If a nat is not contained in a model, it's truth value is 'false'
*)
Definition model := list nat.

(* Helper function for find_variables *)
Fixpoint find_variables' (f:form) (vars:list nat) {struct f} : list nat :=
 match f with
 | f_var x   => cons x vars
 | f_and l r => find_variables' r (find_variables' l vars)
 | f_or  l r => find_variables' r (find_variables' l vars)
 | f_imp l r => find_variables' r (find_variables' l vars)
 | f_neg g   => find_variables' g vars
 end
 .

(* Put all free variables found in the given term into a list *)
Definition find_variables : form -> list nat :=
 fun (f:form) => find_variables' f nil.

(* some tests *)

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


(* Checks whether the given number x is in the list l *)
Definition bin_nat (x:nat) (l:list nat) : bool :=
 existsb (fun y => beq_nat x y) l.


(* Given a model and a formula f, checks whether the model satisfies the formula.

   read: m |= f
*)
Fixpoint models (f:form) (m:model) {struct f} : bool :=
 match f with
 | f_var x   => bin_nat x m
 | f_and l r => andb  (models l m) (models r m)
 | f_or  l r => orb   (models l m) (models r m)
 | f_imp l r => implb (models l m) (models r m)
 | f_neg g   => negb  (models g m)
 end
 .

Example models_var :
 models (f_var 1) (1::nil) = true.
Proof. simpl. reflexivity. Qed.

Example models_and :
 models (f_and (f_var 2) (f_var 1)) (1::2::nil) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_and :
 models (f_and (f_var 2) (f_var 1)) (1::nil) = false.
Proof. simpl. reflexivity. Qed.

Example model_or :
 models (f_or (f_var 42) (f_var 1)) (1::nil) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_or :
 models (f_or (f_var 42) (f_var 43)) (1::2::nil) = false.
Proof. simpl. reflexivity. Qed.

Example models_imp :
 models (f_imp (f_var 1) (f_var 1)) nil = true.
Proof. simpl. reflexivity. Qed.

Example not_models_imp :
 models (f_imp (f_var 1) (f_var 42)) (1::nil) = false.
Proof. simpl. reflexivity. Qed.

Example models_neg :
 models (f_neg (f_var 42)) nil = true.
Proof. simpl. reflexivity. Qed.

Example not_models_neg :
 models (f_neg (f_var 1)) (1::nil) = false.
Proof. simpl. reflexivity. Qed.

(* Returns all possible sublists of a given list *)
Fixpoint sublists {A:Type} (l:list A) : (list (list A)) :=
 match l with
 | nil    => nil::nil
 | h :: t => let subtails := (sublists t) in app (map (cons h) subtails) subtails
 end
 .

Example sublists_1 :
 sublists (1::nil) = (1::nil)::(nil)::nil.
Proof. simpl. reflexivity.

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
Proof. simpl. reflexivity. Qed.

Definition nilnat : list nat := nil.
Example sublists_nil :
 sublists nilnat = nil::nil.
Proof. simpl. reflexivity. Qed.


Definition find_model (f:form) : option model :=
 find (models f) (sublists (find_variables f)).

Example findmodel_var :
 find_model (f_var 42) = Some (42::nil).
Proof. reflexivity. Qed.

Example findmodel_fail :
 find_model (f_and (f_var 1) (f_neg (f_var 1))) = None.
Proof. reflexivity. Qed.

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

Theorem find_model_works :
 forall (f:form) (m:model), find_model f = Some m -> models f m = true.
Proof.
intros f m. unfold find_model.
apply found_x_satisfies_predicate.
Qed.

(*
vim: ft=coq
*)
