module skeleton1

/*
	Course I00032 Advanced Programming 2012
	Skeleton for assignment 1
	Pieter Koopman
	Peter  Achten  (P.Achten@cs.ru.nl)
*/

import StdEnv

/**************** Prelude: *******************************/
//	Example types
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)         
:: Rose a	= Rose a [Rose a]

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b

//	Generic type representations
:: RoseG a	:== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a)	-> RoseG a
fromRose (Rose a l)		= PAIR a l

// Oerdering

::	Ordering = Smaller | Equal | Bigger

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where		// Standard ordering for Int
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Char where		// Standard ordering for Char
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< String where	// Standard lexicographical ordering
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Bool where		// False is smaller than True
	(><) False True  = Smaller
	(><) True  False = Bigger
	(><) _     _     = Equal

/**************** End Prelude *************************/
