definition module
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

class eval a		:: !a -> Bool							// Sparkle
instance eval Bool											// Sparkle
instance eval Char											// Sparkle
instance eval Int											// Sparkle
instance eval Real											// Sparkle
instance eval [a] | eval a									// Sparkle

evalFilter			:: !(a -> Bool) ![a] -> Bool			// Sparkle
finite				:: ![a] -> Bool							// Sparkle
forceEval			:: !a !b -> b							// Sparkle
ones				:: [Int]								// Sparkle
