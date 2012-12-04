/*
	Debug functions.

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module Debug

import StdEnv
import Wrap, ShowWrapped
import StdDebug

// abort and undef from StdMisc are recognised be the strictness analyser
// because we rely on the evaluation order in this module, we use our own abort

non_strict_abort :: !{#Char} -> .a;
non_strict_abort a = code  {
	.d 1 0
		jsr print_string_
	.o 0 0
		halt
	}

print :: ![{#Char}] .b -> .b
print debugStrings value
	| fst (ferror (stderr <<< debugStrings))
		=	non_strict_abort "Debug, print: couldn't write to stderr"
	// otherwise
		=	value

debugBefore :: !.a !(DebugShowFunction .a) .b -> .b
debugBefore debugValue show value
	=	print (show debugValue) value

debugAfter :: .a !(DebugShowFunction .a) !.b -> .b
debugAfter debugValue show value
	=	print (show debugValue) value

debugValue :: !(DebugShowFunction .a) !.a -> .a
debugValue show value
	// copying a unique reference is OK here, because after the show
	// reference1 is no longer in use and show shouldn't change anything
	=	print (show reference1) reference2
	where
		(reference1, reference2)
			=	copyUniqueReference value

		copyUniqueReference :: !.a -> (!.a, !.a)
		copyUniqueReference value
			=	code {
				.o 1 0
					push_a	0				
				.d 2 0
				}

:: DebugShowFunction a :== a -> [{#Char}]

:: DebugOptionRecord
	=	{maxDepth :: !Int, maxBreadth :: !Int, maxChars :: !Int, terminator :: !{#Char}}
DebugDefaultOptions
	:== {maxDepth = MaxInt, maxBreadth = MaxInt, maxChars = MaxInt, terminator = "\n"}

MaxInt
	:== (1<<31)-1

:: DebugShowOption 
	=	DebugMaxDepth !Int			// default MaxInt
	|	DebugMaxBreadth !Int		// default MaxInt
	|	DebugMaxChars !Int			// default MaxInt
	|	DebugTerminator !{#Char}	// default "\n"

(:-) infixl
(:-) a f
	:== f a

debugShowWithOptions :: [DebugShowOption] .a -> [{#Char}]
debugShowWithOptions debugOptions debugValue 
	=	debugValue
	:-	deepWrap
	:-	prune 0 maxDepth maxBreadth
	:-	showWrappedNode
	:-	chop maxChars
	:-	flip (++) [terminator]
	where
		{maxDepth, maxBreadth, maxChars, terminator}
			=	foldl set DebugDefaultOptions debugOptions
			where
				set options (DebugMaxDepth maxDepth)
					=	{options & maxDepth=maxDepth}
				set options (DebugMaxBreadth maxBreadth)
					=	{options & maxBreadth=maxBreadth}
				set options (DebugMaxChars maxChars)
					=	{options & maxChars=maxChars}
				set options (DebugTerminator terminator)
					=	{options & terminator=terminator}

instance <<< [a] | <<< a where
	(<<<) :: !*File ![a] -> *File | <<< a
	(<<<) file []
		=	file
	(<<<) file [h:t]
		=	file <<< h <<< t
