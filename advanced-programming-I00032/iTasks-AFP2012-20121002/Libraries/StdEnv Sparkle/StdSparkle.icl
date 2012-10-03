implementation module
	StdSparkle

//	****************************************************************************************
//	Additional StdEnv module for Sparkle 0.0.3a (8 January 2002):
//		o Class 'eval': returns True when its argument can be evaluated to root normal
//		  form and _|_ otherwise (accomplished by making use of strictness)
//		o Function 'evalFiler': returns True when the argument function can be applied to
//		  all elements of the argument list resulting in either True or False. A _|_ is
//		  produced when the argument function returns _|_ on of the argument list elements.
//		o Function 'finite': returns True when the spine of the list can be fully
//		  evaluated (and is thus finite) and _|_ otherwise.
//		o Function 'forceEval', returns second argument while evaluating the first.
//		o Function 'ones': constructs the infinite list containing only the Int 1.
//	****************************************************************************************

import
	StdBool,
	StdChar,
	StdInt

// Sparkle
// ----------------------------------------------------------------------------------------
class eval a :: !a -> Bool
// ----------------------------------------------------------------------------------------

// Sparkle
// ----------------------------------------------------------------------------------------
instance eval Bool
// ----------------------------------------------------------------------------------------
where
	eval :: !Bool -> Bool
	eval x
		= True

// Sparkle
// ----------------------------------------------------------------------------------------
instance eval Char
// ----------------------------------------------------------------------------------------
where
	eval :: !Char -> Bool
	eval x
		= True

// Sparkle
// ----------------------------------------------------------------------------------------
instance eval Int
// ----------------------------------------------------------------------------------------
where
	eval :: !Int -> Bool
	eval x
		= True

// Sparkle
// ----------------------------------------------------------------------------------------
instance eval Real
// ----------------------------------------------------------------------------------------
where
	eval :: !Real -> Bool
	eval x
		= True

// Sparkle
// ----------------------------------------------------------------------------------------
instance eval [a] | eval a
// ----------------------------------------------------------------------------------------
where
	eval :: ![a] -> Bool | eval a
	eval [x:xs]
		= eval x && eval xs
	eval []
		= True

// Sparkle
// ----------------------------------------------------------------------------------------
evalFilter :: !(a -> Bool) ![a] -> Bool
// ----------------------------------------------------------------------------------------
evalFilter p [x:xs]
	= case p x of
		True	-> evalFilter p xs
		False	-> evalFilter p xs
evalFilter p []
	= True

// Sparkle
// ----------------------------------------------------------------------------------------
finite :: ![a] -> Bool
// ----------------------------------------------------------------------------------------
finite [x:xs]
	= finite xs
finite []
	= True

// Sparkle
// ----------------------------------------------------------------------------------------
forceEval :: !a !b -> b
// ----------------------------------------------------------------------------------------
forceEval x y
	= y

// Sparkle
// ----------------------------------------------------------------------------------------
ones :: [Int]
// ----------------------------------------------------------------------------------------
ones
	= [1:ones]