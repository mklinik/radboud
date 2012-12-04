definition module StdFunc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
//
//	Changes made for Sparkle 0.0.3a (8 January 2002):
//		o The macro definitions for 'flip', 'o', '`bind`' and 'return' have been
//		  replaced by functions.
// ****************************************************************************************

id    :: !.a -> .a								// identity function
const :: !.a .b -> .a							// constant function

flip 			:: !.(.a -> .(.b -> .c)) .b .a -> .c				// Sparkle
(o) infixr  9	::  u:(.a -> .b) u:(.c -> .a) -> u:(.c -> .b)		// Sparkle

twice			:: !(.a -> .a)   .a             -> .a		// f (f x)
while			:: !(a -> .Bool) (a -> a) 	 a 	->  a		// while (p x) f (f x) 
until			:: !(a -> .Bool) (a -> a) 	 a 	->  a		// f (f x) until (p x)
iter			:: !Int 		 (.a -> .a) .a	-> .a		// f (f..(f x)..) 

//	Some handy functions for transforming unique states:

seq				:: ![.(.s -> .s)] .s -> .s					// fn-1 (..(f1 (f0 x))..)
seqList			:: ![St .s .a] .s -> ([.a],.s)				// fn-1 (..(f1 (f0 x))..)

:: St s a :== s -> *(a,s)

// monadic style:

(`bind`) infix 0	:: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]		// Sparkle
return				:: u:a -> u:(St .s u:a)														// Sparkle
