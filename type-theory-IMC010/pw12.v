(* first part: prop2 and lambda2 and their connection *)
(* via the Curry-Howard-De Bruijn isomorphism         *)

(* the paradigmatic example: the polymorphic identity *)
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

(* the polymorphic identity *)
Definition polyid : forall a:Set, forall x:a, a := 
                    fun a:Set =>  fun x:a =>  x.
(* instantiation by application *)
(* Check gives the types *)
Check polyid.
Check polyid nat.
Check polyid nat O.
(* Eval compute computes normal forms *)
Eval compute in (polyid nat).
Eval compute in (polyid nat O).

(* exercise 1 *)
(* give inhabitants of the following types *)

(* forall a : Prop, a -> a *)
(* forall a b : Prop, (a -> b) -> a -> b *)
(* forall a : Prop, a -> forall b : Prop, (a -> b) ->  b*)

(* exercises with negation *)

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

Lemma exercise7 : em -> forall a b:Prop, ((a -> b) \/ (b -> a)).
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

(*  definition of and *)
(* "a and b" is valid is everything that can be
   derived from {a,b} is valid *)
Definition new_and (a b : Prop) := forall c : Prop, (a -> b -> c) -> c.

(* and implies new_and *)
Lemma exercise11 : forall a b : Prop,a /\ b -> new_and a b.
Proof.
(*! proof *)

Qed.


(* new_and implies and *)
Lemma exercise12 : forall a b : Prop ,
                   new_and a b -> a /\ b.
Proof.
(*! proof *)

Qed.

(* exercise 13 *)
(* assume an inhabitant P of new_and A B with A and B in Prop *)
(* give an inhabitant of A and an inhabitant of B *)
Parameters A B : Prop.
Parameter P : new_and A B.

(* "a or b" is valid if everything that follows form
   {a} and follows from {b} is valid *)
Definition new_or (a b : Prop) := forall c : Prop, (a -> c) -> (b -> c) -> c.

Lemma exercise14 : forall a b , a \/ b -> new_or a b.
Proof.
(*! proof *)

Qed.

Lemma exercise15 : forall a b , new_or a b -> a \/ b.
Proof.
(*! proof *)

Qed.

(* exercise 16 *)
(* given an inhabitant Q:A *)
(* give an inhabitant of new_or A B with A and B in Prop *)
Parameter Q:A.

Check (*! term *)
  .


