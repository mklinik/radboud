definition module ParsersAccessories

import ParsersKernel, ParsersDerived
from StdEnv import class toString, class toChar
from StdMaybe import :: Maybe (..)

number :: Parser  Char Int a	//wants at least one digit and takes all it can get
number` :: Parser  Char Int a	//takes any number of digits non-deterministically

digit :: Parser Char Char a

// drop initial spaces
ds :: (Parser s r t) -> Parser s r t | space s

class space s :: !s -> Bool

instance space Char

// a version of symbol that automatically creates a hypothesis level
symbolH :: (s -> Parser s s t) | ==,toString s

// a version of token that automatically creates a hypothesis level
tokenH :: ([s] -> Parser s [s] t) | ==,toChar s

/*	Computes line and column number, taking into account tabs and line breaks. Mind that tabs and
	line breaks are themselves characters in the input string and have a position.*/
		
lineAndColumn :: [Char] Int			// position returned by error msg
						Int ->		// standard tab width
						(Int,Int)	// line,column

errorToString :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> String

simpleErrorToString :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> String
// For testing only. Quick&Dirty really. Use import path ParserLanguage/For Testing/Language.dcl

flattenSep :: String ![x] -> String | toString x	// concatenate all with String in between

errorToStrings :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [String]

errorToFormat :: SymbolTypes (Rose (String,[SugPosition])) [SugPosition] -> [(Int,String)]

instance toString SymbolType
