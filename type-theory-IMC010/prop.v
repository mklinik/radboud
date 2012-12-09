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
Fixpoint models (m:model) (f:form) {struct f} : bool :=
 match f with
 | f_var x   => bin_nat x m
 | f_and l r => andb  (models m l) (models m r)
 | f_or  l r => orb   (models m l) (models m r)
 | f_imp l r => implb (models m l) (models m r)
 | f_neg g   => negb  (models m g)
 end
 .

Example models_var :
 models (1::nil) (f_var 1) = true.
Proof. simpl. reflexivity. Qed.

Example models_and :
 models (1::2::nil) (f_and (f_var 2) (f_var 1)) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_and :
 models (1::nil) (f_and (f_var 2) (f_var 1)) = false.
Proof. simpl. reflexivity. Qed.

Example model_or :
 models (1::nil) (f_or (f_var 42) (f_var 1)) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_or :
 models (1::2::nil) (f_or (f_var 42) (f_var 43)) = false.
Proof. simpl. reflexivity. Qed.

Example models_imp :
 models nil (f_imp (f_var 1) (f_var 1)) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_imp :
 models (1::nil) (f_imp (f_var 1) (f_var 42)) = false.
Proof. simpl. reflexivity. Qed.

Example models_neg :
 models nil (f_neg (f_var 42)) = true.
Proof. simpl. reflexivity. Qed.

Example not_models_neg :
 models (1::nil) (f_neg (f_var 1)) = false.
Proof. simpl. reflexivity. Qed.