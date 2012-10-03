definition module ParsersKernel
from StdEnv import class Eq, class toString, class ==
from StdMaybe import :: Maybe(..)

// abstract type for use in getParsable and setParsable only
:: Parsable s
:: Parser s r t

// ERROR-PROCESSING:

:: SymbolType	= Whole String			// e.g. "letter", "word", "sentence","paragraph"
				| Slice String Int		// when atMost has cut off some
:: SymbolTypes		:== [SymbolType]	// in increasingly coarser units

:: SugPosition	= At Int | EndAt Int

instance == SugPosition

// PARSER CONSTRUCTORS:

// this parser always fails
fail			:: Parser s r t

// the resuling parser always succeeds, consumes no input and produces the given result
yield			:: r -> Parser s r t

// accepts any symbol and produces that as a result
anySymbol		:: Parser s s t

// resulting parser accepts a given symbol
symbol			:: s -> Parser s s t | == s

// resulting parser accepts a symbol if it satisfies the condition
satisfy			:: (s -> Bool) -> Parser s s t

// parser produces the current input-pointer as a result. consumes no input
getPosition		:: Parser s [Int] t

// resulting parser moves the input pointer a given number positions forward
advancePosition :: !Int -> Parser s r t

// parser produces the whole remaining input structure as a result, which is to be
// used in setParsable only. this is forced by (Parsable s) being an abstract type
getParsable 	:: Parser s (Parsable s) t

// repositions the input to any position
setParsable 	:: (Parsable s) -> Parser s r t

// resulting parser accepts the given list of symbols
token			:: [s] -> Parser s [s] t | == s

// parser succeeds if there is no remaining input (on the given hierarchic level)
empty			:: Parser s r t
			
// PARSER COMBINATORS:

// or-combinator tries both alternatives non-deterministically
(<|>)	infixr 4 :: (Parser s r t) (Parser s r t)		-> Parser s r t

// or-combinator tries the second alternative only if the first one fails
(<!>)	infixr 4 :: (Parser s r t) (Parser s r t)		-> Parser s r t

// monadic sequential-combinator
(<&>)	infixr 6 :: (Parser s u t) (u -> Parser s v t)	-> Parser s v t

// arrow-style sequential-combinator
(<++>)	infixl 6 :: (Parser s (r->u) t) (Parser s r t)	-> Parser s u t

/*	p1 <&>  p2 <!> p3 and
	p1 <++> p2 <!> p3 share the following behavior:
	
	a) if p1 succeeds and p2 succeeds, p3 is not tried
	b) if p1 succeeds and p2 fails, p3 is tried
	c) if p1 fails, p2 is not tried and p3 is tried
	
	p1 <&->  p2 <-!> p3 and
	p1 <++-> p2 <-!> p3 modify line b) of the above behavior:
	
	b) if p1 succeeds and p2 fails, p3 is NOT tried
*/

class Orr c
where	(<-!>) infixr 4 :: !(c s r u t) (Parser s r t) -> Parser s r t

:: MonadicSeq s r u t = (<&->) infixr 6 (Parser s u t) (u -> Parser s r t)
instance Orr MonadicSeq

:: ArrowSeq s u r t = (<++->) infixl 6 (Parser s (r->u) t) (Parser s r t)
instance Orr ArrowSeq

// PARSER TRANSFORMERS:

// makes a parser non-deterministic: returns only the first result
first				:: (Parser s r t) -> Parser s r t

// resulting parser applies the check to the recognized item and falis if the check fails
(checkIf) infix 7	:: (Parser s r t) (r -> Bool) -> Parser s r t

// resulting parser applies the check to the recognized item and fails if the check fails
// the string is an error message carried over to the resulting parser. NOT TESTED YET
(checkExplain) infix 7 :: (Parser s r t) (r -> Maybe String) -> Parser s r t

// resulting parser rewinds the input to where it started
rewind				:: (Parser s r t) -> Parser s r t

// resulting parser drops all symbols from the input that satisfy the check
dropCheck			:: (s -> Bool) (Parser s r t) -> Parser s r t

// resulting parser will consume at most the given number of symbols and fials if more would be needed
atMost				:: !Int (Parser s r t) -> Parser s r t

// PARSER TRANSFORMERS THAT LEAVE AND RE-ENTER THE REALM OF CONTINUATIONS:

/*	drill turns a parser that would consume say Char's as its symbols into a parser that consumes lists
	of Char's as is symbols. so if the input is [['my'],['number:'],['54365']] the following parser will
	recognize this, independent of the actual number and deliver that number as a string of digits:
	
	\_ _ n -> n @>		// forget about this: it serves to drop the two words and retain the number
	symbol ['my'] <++> symbol ['number:'] <++> drill (<!+> digit)

	The String below represents the name of a subsymbol, to be used in error messages.
*/ 
drill			:: (Parser s r r) String -> Parser [s] r t

// sortResultBy and minResultBy take a 'less' function
// if (less r1 r2) then r1 appears first in the output (else r2)
sortResultBy	:: (r r -> Bool)  (Parser s r r) -> Parser s r t

//delivers all minimum results non-deterministically
minResultBy 	:: (r r -> Bool) (Parser s r r) -> Parser s r t

//delivers all longest results non-deterministically
longest			:: (Parser s r r) -> Parser s r t

// FOR ERROR REPORTING:

/*	gives a name (a hypothesis level) to the parser, for instance a parser that recognizes if-statements could be
	given the name "if-statement" which will then be used in error messages. it is advisable to avoid string literals
	and use named constants whose values be defined in a separate language module, so one could easily change to a
	different language to state the error messages in
*/
(:>)	infixl 8 :: String (Parser s r t) -> Parser s r t

// gives a name (a hypothesis level) to the second parser in the <&> combinator
(:=>)	infixl 8 :: (r -> String) (r -> Parser s r t) -> (r -> Parser s r t)

// FOR APPLYING A PARSER TO AN INPUT

:: Result r	= Err SymbolTypes (Rose (String,[SugPosition])) [SugPosition] | Succ [r]
	//	The hypothesis paths and the SugPositions are from course to detailed.

// this data structure is used to collect error information
:: RoseNode a = RoseLeaf | RoseTwig a (Rose a)
	// RoseLeaf indicates where a twig ends, so the following are indeed different
	// [RoseTwig 1 [RoseTwig 3 [RoseLeaf,RoseTwig 4 [RoseLeaf]]]]
	// this contains a path [1,3] and a path [1,3,4] (both are closed by a RoseLeaf)
	
:: Rose a :== [RoseNode a]

// actually apply a parser to a list of symbols. the two strings are names for
// the whole input (e.g. "command line") and one symbol (e.g. "character")
parse :: !(Parser s r r) [s] String String -> Result r