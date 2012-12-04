implementation module StdClass

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
//
//	Changes made for Sparkle 0.0.3a (8 January 2002):
//		o The macro definitions for 'inc', 'dec', '<>', '>', '>=', '<=', 'min' and
//		  'max' have been removed from their corresponding classes and have been
//		  replaced by overloaded functions.
//		o Changed the dependency of the class 'Enum' to 'Ord' instead of '<'.
// ****************************************************************************************

import StdOverloaded
from StdBool import not

//	Remark: derived class members are not implemented yet!
//	For the time-being, macro definitions are used for this purpose
//	This may cause misleading error messages in case of type errors 

class PlusMin a | + , - , zero a

class MultDiv a | * , / , one a

class Arith a 	| PlusMin , MultDiv , abs , sign , ~ a 

// Sparkle
class IncDec a	| + , - , one , zero a

// Sparkle
class Enum a | Ord, IncDec a

// Sparkle
class Eq a | == a

// Sparkle
class Ord a | < a

// Sparkle
inc :: !a -> a | IncDec a
inc x
	= x + one

// Sparkle
dec :: !a -> a | IncDec a
dec x
	= x - one

// Sparkle
(<>) infix  4 :: !a !a -> Bool | Eq a
(<>) x y
	= not (x == y)

// Sparkle
(>) infix  4 :: !a !a -> Bool | Ord a
(>) x y
	= y < x 

// Sparkle
(<=) infix 4 :: !a !a -> Bool | Ord a
(<=) x y
	= not (y<x)

// Sparkle
(>=) infix 4 :: !a !a -> Bool | Ord a
(>=) x y
	= not (x<y) 

// Sparkle
min ::!a !a -> a | Ord a
min x y
	= case (x<y) of True = x; _ = y

// Sparkle
max ::!a !a -> a | Ord a
max x y
	= case (x<y) of True = y; _ = x