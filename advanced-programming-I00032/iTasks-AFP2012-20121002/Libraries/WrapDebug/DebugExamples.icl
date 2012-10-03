/*
	Examples of Debug behaviour

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl

*/
module DebugExamples

import StdEnv
import Debug

// these examples use double arrow syntax (->>, <<- and <<->>)
(<<-) infix 0 :: .a !.b -> .a
(<<-) value debugValue
	=	debugBefore debugValue show value

(->>) infix 0 :: !.a .b -> .a
(->>) value debugValue
	=	debugAfter debugValue show value

<<->> :: !.a -> .a
<<->> value
	=	debugValue show value

// show function with debug options
show
	=	debugShowWithOptions
			[DebugMaxChars 79, DebugMaxDepth 5, DebugMaxBreadth 20]

// choose your example here
Start
	=	example1

/*
	a <<- b (debugBefore)
		print b, then evaluate a
*/
example1
	=	abort "example1 value\n" <<- "example1 debug"

/*
	a ->> b (debugAfter)
		evaluate a, then print b
*/
example2
	=	abort "example2 value\n" ->> "example2 debug"

/*
	<<->> a (debugValue)
		print and evaluate a, value can be unique
*/
example3
	=	<<->> "example3"

/*
	debugging also works with infinite values (provided you
	limit the debug output with the DebugMax... options)
*/
example4
	=	"example4" <<- [1..]

/*
	debugging with algebraic values
*/
:: List a
	=	Nil
	|	Cons a (List a)

example5
	=	"example5" <<- Cons 1 (Cons 2 Nil)

/*
	debugging with a record value, note that the field names
	don't appear in the debug output (this information isn't
	available at run-time)
*/
:: R	= {f1 :: Int, f2 :: Int}

example6
	=	"example6" <<- {f1 = 1, f2 = 2}

/*
	debugging with arrays 
*/
example7
	=	"example7" <<- array
	where
		array :: {Int}
		array
			=	{1, 2, 3, 4, 5}

/*
	debugging with closures 
*/
example8
	=	"example8" <<- (take, take 5, take 5 ['Brubeck'])

/*
	debugging may evaluate values that wouldn't otherwise
	be evaluated
*/
example9
	=	hd (<<->> ["example9" : undef])

/*
	debugging may effect strictness, in this example f is not
	strict in its first argument because of the debug function
*/
example10
	=	f "example" "10"
	where
		f a b
			=	(a <<- "f") +++ b

/*
	debugging depends on the evalution order, you'll have to
	understand the evalution order to understand in which order
	the debug values will be printed
*/
example11
	=	fst (concatFirstTwo ["exam","ple11"])
	where
		concatFirstTwo
			=	(get ->> "get first") `bind` \first
			->	(get ->> "get second") `bind` \second
			->	return (first+++second) ->> "return"
	get [h:t]
		=	(h, t)

/* the following examples only work with Clean 2.0.2 or better */


import StdStrictLists

/*
	debugging with strict lists, the strictness (head strict, tail
	strict) doesn't appear in the output (strict lists use the same
	constructors at run-time as ordinary list)
*/
example12
	=	"example12" <<- [! 1 .. 10 !]

/*
	debugging with unboxed lists, the output is ugly: this should
	be improved
*/
example13
	=	"example13" <<- [# 1 .. 10 ]

/*
	debugging with unboxed lists of records, the output is ugly:
	this should be improved
*/
:: R2 = {f :: Int}
example14
	=	"example14" <<- [# {f = 1}, {f = 2} !]

