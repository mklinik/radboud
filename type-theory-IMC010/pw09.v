(* ************************************************* *)
(* prop2                                             *)
(* ************************************************* *)



(* the paradigmatic example corresponding to           *)
(* the polymorphic identity                            *)
(* we already print the inhabitant although we did not *)
(* yet discuss lambda2                                 *)

Lemma example1a : forall a:Prop, a->a.

Proof.
intro a.
intro x.
exact x.
Qed.
Print example1a.
(* \a:Prop. \x:a. x *)



Lemma example1b : forall a:Set, a -> a.

Proof.
intro a.
intro x.
exact x.
Qed.
Print example1b.
(* \a:Set. \x:a. x *)



(* another example from the course *)
Lemma example2: forall a:Prop, a -> (forall b:Prop, ((a -> b) -> b)).

Proof.
intro a.
intro x.
intro b.
intro y.
apply y.
exact x.
Qed.
Print example2.
(* \a:Prop. \x:a. \b:Prop. \y:a->b. (y x) *)


(* the following "lemma" is not valid *)
(*
Lemma example_wrong : forall a:Prop, a -> (forall b:Prop, b).

Proof.
intro a.
intro x.
intro b.
Abort.
*)


(* exercise 1 *)
(* give inhabitants of the following types: *)
(* forall a : Prop, a -> a *)
(* forall a b : Prop, (a -> b) -> a -> b *)
(* forall a : Prop, a -> forall b : Prop, (a -> b) ->  b*)



(* exercises with negation *)

(* exercise 2 *)
Lemma exercise2 : forall a:Prop, a -> ~~a.
Proof.
(*! proof *)

Qed.

Lemma exercise3: forall a:Prop, ~~~a -> ~a.
Proof.
(*! proof *)

Qed.

Lemma exercise4: forall a:Prop, ~~(~~a -> a).
Proof.
(*! proof *)

Qed.


(* exercises with full intuitionistic prop2 *)

Lemma exercise5 : forall a:Prop, (exists b:Prop, a) -> a.
Proof.
(*! proof *)

Qed.

Lemma exercise6 : forall a:Prop, exists b:Prop, ((a -> b) \/ (b -> a)).
Proof.
(*! proof *)

Qed.



(* exercise with classical prop2 *)

Definition em:= forall a:Prop, a \/ ~a.

Lemma exercise7: em -> forall a b:Prop, ((a -> b) \/ (b -> a)).
Proof.
(*! proof *)

Qed.

(* expressibility of prop2 *)
(* we will represent logical connectives in prop2 *)

(* definition of false *)
Definition new_false := forall a:Prop, a.

(* False implies new_false *)
Lemma exercise8 : False -> new_false.
Proof.
(*! proof *)

Qed.

(* new_false implies False *)
Lemma exercise9 : new_false -> False.
Proof.
(*! proof *)

Qed.

(* from new_false we can prove anything *)
Lemma exercise10 : forall a:Prop, new_false -> a.
Proof.
(*! proof *)

Qed.

(*
vim: filetype=coq
*)
