definition module ParsersDerived
from ParsersKernel import :: Parser, yield, <&>, <++>
from StdEnv import class Eq, class ==, const

// PARSER COMBINATORS:

// and-combinator that only retains the left hand side result
(<&)	infixr 6	:: (Parser s r t) (Parser s r` t) -> Parser s r t

// and-combinator that only retains the right hand side result
(&>)	infixr 6 // :: (Parser s r t) (Parser s r` t) -> Parser s r` t
(&>) p1 p2 :== p1 <&> const p2

// and combinator that combines both results in a tuple
(<&&>)  infixr 6	:: (Parser s r t) (Parser s u t) -> Parser s (r,u) t

// and combinator that combines a parser for an element and one for a list of such elements
(<:&>)	infixr 6	:: (Parser s r t) (Parser s [r] t) -> Parser s [r] t

// and combinator that combines a parser for an element and one for a list of such elements
// to append something to the result. useful with the <++> combinator
(<:&:>) infixr 6	:: (Parser s r t) (Parser s ([r]->[r]) t) -> Parser s ([r]->[r]) t

// PARSER TRANSFORMERS:

// takes any number of elements non-deterministically 
<*>			:: (Parser s r t) -> Parser s [r] t			

// takes any number of elements non-deterministically,
// to append something to the result.  useful with the <++> combinator 
<*:>		:: (Parser s r t) -> Parser s ([r]->[r]) t

// takes one element or more non-deterministically,
<+>			:: (Parser s r t) -> Parser s [r] t

// takes one element or more non-deterministically,
// to append something to the result.  useful with the <++> combinator 
<+:> 		:: (Parser s r t) -> Parser s ([r]->[r]) t

// takes all elements it can get. zero is okay 
<!*>		:: (Parser s r t) -> Parser s [r] t

// takes all elements it can get. zero is okay 
// to append something to the result.  useful with the <++> combinator 
<!*:> 		:: (Parser s r t) -> Parser s ([r]->[r]) t	// to append something to the result

// takes all elements it can get. at least one required
<!+>		:: (Parser s r t) -> Parser s [r] t

// takes all elements it can get. at least one required
// to append something to the result.  useful with the <++> combinator 
<!+:> 		:: (Parser s r t) -> Parser s ([r]->[r]) t	// to append something to the result

// takes an element or not, non-deterministically
<?>			:: (Parser s r t) (r -> u) u -> Parser s u t

// takes an element if it is there
<!?>		:: (Parser s r t) (r -> u) u -> Parser s u t

// prepends a function to the <++> combinator
(@>)		infix 7 //	:: (r -> r`) (Parser s r t) -> Parser s r` t
(@>)		f p :== yield f <++> p

// applies a function to a parse-result
(<@)		infixl 5 :: (Parser s r t) (r ->r`) -> Parser s r` t

// moves to a point from where the input parser succeeds and produces as a result all the input skipped
grazeTo		:: (Parser s r t) -> Parser s [s] t

// moves to a point from where the input parser succeeds and produces as a result all the input skipped,
// and then moves past the recoginized item
grazeOver	:: (Parser s r t) -> Parser s [s] t

// grazeTo deterministically: a later backtrack will not move further forward
grazeOnce	:: (Parser s r t) -> Parser s [s] t

// grazeTo with undefined result
skipTo		:: (Parser s r t) -> Parser s u t

// grazeOver with undefined result
skipOver	:: (Parser s r t) -> Parser s u t

// grazeOnce with undefined result
skipOnce	:: (Parser s r t) -> Parser s u t

// apply first parser. on success rewind to the start, move forward a number of symbols that
// may depend on the first parser's result. then apply the second parser
scrape		:: (Parser s r t) (r ->Int) (r -> Parser s v t) -> Parser s v t