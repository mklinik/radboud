definition module StdException

/* 
	StdException -- version 1.2

	Written by Arjen van Weelden (with the help of John van Groningen), University of Nijmegen
	Send comments and bug reports to arjenw@cs.kun.nl.

	Imprecise user-defined synchronous exception handling for Clean 2.1
	Warning: all unsafe* functions are `non-deterministic between compilations'.

	Version history:
		1.2:	Bug fix: Invalid return address fixed, due to profile stack fix.
		1.1:	Bug fix: Profile stack overflow fixed.
		1.0:	Bug fix: Profile stack reset was off by 8.
		0.9:	Polished for first public release
		0.8:	Added evalToNF to force evaluation to normal form;
				Removed DynamicTemp constructor from error message.
		0.7:	Bug fix: Added profile stack reset.
		0.6:	Bug fix: catchAllIO made a mess of the stack.
		0.5:	Added *IO functions: easier exception handling when using functions that operate on the world.
		0.4:	Bug fix: exception did not show when using no console.
		0.3:	Only Dynamic exceptions: exception class prevented catching all exception with a single handler.
		0.2:	Less overhead: everything builds on unsafeCatch instead of unsafeGetException;
				Exception class fixed: user defined instances can be type unsafe.
		0.1:	first release.
*/

import StdDynamic

:: MaybeException a = NoException !a | Exception !Dynamic

raise :: e -> .a | TC e
raiseDynamic :: Dynamic -> .a

class ExceptionEnv env
where
	(catchAllIO) infixl 0 :: !u:(*env -> (.a, *env)) v:(Dynamic -> u:(*env -> (.a, *env))) -> u:(*env -> (.a, *env)), [u <= v]

(catchIO) infixl 0 :: !u:(*env -> (.a, *env)) v:(e -> u:(*env -> (.a, *env))) -> u:(*env -> (.a, *env)) | TC e & ExceptionEnv env, [u <= v]
(catchAll) infixl 0 :: u:a v:(Dynamic -> u:a) -> w:(*env -> (u:a, *env)) | ExceptionEnv env, [w <= u, w <= v]
(catch) infixl 0 :: u:a v:(e -> u:a) -> w:(*env -> (u:a, *env)) | TC e & ExceptionEnv env, [w <= u, w <= v]
getExceptionIO :: !.(*env -> (.a, *env)) !*env -> (MaybeException .a, !*env) | ExceptionEnv env
getException :: .a !*env -> (!MaybeException .a, !*env) | ExceptionEnv env

(unsafeCatchAll) infixl 0 :: .a .(Dynamic -> .a) -> .a
(unsafeCatch) infixl 0 :: .a .(e -> .a) -> .a | TC e
unsafeGetException :: .a -> MaybeException .a

evalToNF :: !.a -> .a

instance ExceptionEnv World
