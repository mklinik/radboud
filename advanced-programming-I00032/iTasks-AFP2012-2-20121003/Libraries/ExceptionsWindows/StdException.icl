implementation module StdException

/* 
	StdException -- version 1.2

	Written by Arjen van Weelden (with the help of John van Groningen), University of Nijmegen
	Send comments and bug reports to arjenw@cs.kun.nl.

*/

import StdDynamic

:: Catch a = Catch /*h ::*/ .(Dynamic -> a) /*Psp ::*/ !Int /*BCsp ::*/ !Int

raise :: e -> .a | TC e
raise exception = raiseDynamic (dynamic exception :: e^)

raiseDynamic :: Dynamic -> .a
raiseDynamic dyn = raise_ (Catch omega 0 0) (raiseDynamic dyn) dyn
where
	omega :: .a
	omega => omega

(catchIO) infixl 0 :: !u:(*env -> (.a, *env)) v:(e -> u:(*env -> (.a, *env))) -> u:(*env -> (.a, *env)) | TC e & ExceptionEnv env, [u <= v]
(catchIO) f h = f catchAllIO match h

(catchAll) infixl 0 :: u:a v:(Dynamic -> u:a) -> w:(*env -> (u:a, *env)) | ExceptionEnv env, [w <= u, w <= v]
(catchAll) x h = f x h 
where
	f :: .a .(Dynamic -> .a) !*env -> (!.a, !*env)
	f x h env = (x unsafeCatchAll h, env)

(catch) infixl 0 :: u:a v:(e -> u:a) -> w:(*env -> (u:a, *env)) | TC e & ExceptionEnv env, [w <= u, w <= v]
(catch) x h = x catchAll match h /*f x h 
where
	f :: .a .(e -> .a) !*env -> (!.a, !*env) | TC e
	f x h env = (x unsafeCatch h, env)*/

getExceptionIO :: !.(*env -> (.a, *env)) !*env -> (MaybeException .a, !*env) | ExceptionEnv env
getExceptionIO f env = (g f catchAllIO h) env
where
	g :: !.(*env -> (.b, *env)) *env -> (MaybeException .b, !*env)
	g f env
		# (x, env) = f env
		= (NoException x, env)

	h :: !Dynamic !*env -> (!MaybeException .b, !*env)
	h x env = (Exception x, env)

getException :: .a !*env -> (!MaybeException .a, !*env) | ExceptionEnv env
getException e env = (NoException e unsafeCatchAll Exception, env)

(unsafeCatchAll) infixl 0 :: .a .(Dynamic -> .a) -> .a
(unsafeCatchAll) e h = catch_ (Catch h) e

(unsafeCatch) infixl 0 :: .a .(e -> .a) -> .a | TC e
(unsafeCatch) e h = e unsafeCatchAll match h

unsafeGetException :: .a -> MaybeException .a
unsafeGetException e = (NoException e) unsafeCatchAll Exception

evalToNF :: !.a -> .a
evalToNF x = code {
		push_a	0
	.d 1 0
		jsr		_eval_to_nf
	.o 0 0
	}

instance ExceptionEnv World
where
	(catchAllIO) f h = g f h
	where
		g :: !.(*World -> (.a, *World)) .(Dynamic -> .(*World -> (.a, *World))) !*World -> (.a, !*World)
		g f h world
			# (w1, w2) = copy world
			= f` f w1 unsafeCatchAll h` h w2
		where
			f` :: !.(*World -> (.a, *World)) !*World -> (.a, !*World)
			f` f world = f world

			h` :: !.(Dynamic -> .(*World -> (.a, *World))) !*World Dynamic -> (.a, !*World)
			h` h world x = h x world
		
		copy :: !.a -> (!.a, !.a)
		copy x = code inline {
				push_a	0
			}

match :: .(e -> .a) !Dynamic -> .a | TC e
match f (exception :: e^) = f exception
match _ dyn = raiseDynamic dyn

catch_ :: !.(Int -> .(Int -> .Catch .a)) .a -> .a
catch_ f e = code {					|A| !f | e
		pushL		profile_stack_pointer	|B| profile_stack_pointer
		push_b_a	0				|A| profile_stack_pointer | !f | e
		pop_b		1				|B|
		pushD_a		0				|B| Psp
		pop_a		1				|A| !f | e
		pushI		0				|B| 4 | Psp
		addI						|B| Psp + 4
		buildI_b	0				|A| !p=(Int (Psp + 4)) | !f | e
		pop_b		1				|B|
		push_a		1				|A| !f | !p | !f | e
		update_a	1 2				|A| !f | !p | !p | e
		updatepop_a	0 1				|A| !f | !p | e
	.d 2 0
		jsr			e_system_sAP	|A| !(f p) | e
	.o 1 0
		pushI		-4				|B| -4
	:arjen_1
		instruction 1				|addl %esp, %eax
		instruction 224				|B| BCsp
	:arjen_2
		buildI_b	0				|A| !bc=(Int BCsp) | !(f p) | e
		pop_b		1				|B|
		push_a		1				|A| !(f p) | !bc | !(f p) | e
		update_a	1 2				|A| !(f p) | !bc | !bc | e
		updatepop_a	0 1				|A| !(f p) | !bc | e
	.d 2 0
		jsr			e_system_sAP	|A| !c=(f p bc)=(Catch h !p !bc) | e
	.o 1 0
		push_a		1				|A| e | !c | e
		update_a	1 2				|A| e | !c | !c
		updatepop_a	0 1				|A| e | !c
		jsr_eval	0				|A| !e | ?
	.keep 1 0
		updatepop_a 0 1				|A| !e
	}

raise_ :: !(Catch .a) b Dynamic -> .c
raise_ c t x = code {				|A| !c=(Catch ? ? ?) | t | x | ?
		pushL 		stack_p			|B| stack_p
		push_b_a	0				|A| stack_p | !c | t | x | ?
		pop_b		1				|B|
		pushD_a		0				|B| Astart
		pop_a		1				|A| !c | t | x | ?
		pushD_a		0				|B| c-desc | Astart
		pop_a		1				|A| t | x | ?
		create						|A| _ | t | x | ?
		push_node _cycle_in_spine 0 |A| BH | t | x | ?
		pushD_a		0				|B| BH-desc | c-desc | Astart
		pop_a		1				|A| t | x | ?
	:arjen_3
		push_b		1				|B| c-desc | BH-desc | c-desc | Astart
		pushD_a		2				|B| ?-desc | c-desc | BH-desc | c-desc | Astart
		eqI							|B| (?-desc == c-desc) | BH-desc | c-desc | Astart
		jmp_true	arjen_7			|B| BH-desc | c-desc | Astart
		push_b		0				|B| BH-desc | BH-desc | c-desc | Astart
		pushD_a		2				|B| ?-desc | BH-desc | BH-desc | c-desc | Astart
		eqI							|B| (?-desc == BH-desc) | BH-desc | c-desc | Astart
		jmp_false	arjen_4			|B| BH-desc | c-desc | Astart
		fill_a		0 2				|A| t | x | t'
	:arjen_4
		update_a	1 2				|A| t | x | x
		updatepop_a	0 1				|A| t | x | ?
		pushI		-4				|B| -4 | BH-desc | c-desc | Astart
	:arjen_5
		instruction 1				|addl %esi, %ebx
		instruction 243				|B| Asp | BH-desc | c-desc | Astart
	:arjen_6
		push_b		3				|B| Astart | Asp | BH-desc | c-desc | Astart
		ltI							|B| (Astart < Asp) | BH-desc | c-desc | Astart
		jmp_true	arjen_3			|B| BH-desc | c-desc | Astart
		pop_b		3				|B|
		pop_a		1				|A| x
		repl_r_args 2 0				|A| value | type
		updatepop_a	0 1				|A| value
		buildAC		"Run time error, exception raised: " |A| text | x
	.d 1 0
		jsr			print_string_	|A| x
	.o 0 0
	.d 1 0
		jsr			_print_graph	|A|
	.o 0 0
		halt
	:arjen_7
		pop_b		3				|B|
		push_a		2				|A| !c=(Catch h !p !bc) | t | x | !c
		update_a	1 3				|A| !c | t | x | t
		updatepop_a	0 1				|A| !c | x | t
		repl_r_args 1 2				|A| h | x | t |B| p | bc
	:arjen_8
		instruction	137				|movl %eax, %esp
		instruction 196				|BCsp:=bc
	:arjen_9
		updatepop_b	0 1				|B| p
		pushL		profile_stack_pointer	|B| profile_stack_pointer | p
	:arjen_10
		instruction 137				|movl %eax, (%ebx)
		instruction 3				|Psp:=p
	:arjen_11
		pop_b		2				|B|
		jsr_eval	0				|A| !h | x | t
	.d 2 0
		jsr			e_system_sAP	|A| !(h x) | t
	.o 1 0
	}
