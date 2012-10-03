/*
	Debug functions.

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module ClosureDebug

import StdEnv
import Wrap, ShowWrapped
import StdDebug
import Debug
import ClosureWrap, ClosureShowWrapped

(:-) infixl
(:-) a f
	:== f a

instance prune (ClosureWrappedNode arg) | prune arg where
	prune depth maxDepth maxBreadth (Closure descriptor args unboxed)
		=	Closure descriptor (pruneArray depth maxDepth maxBreadth args) unboxed
	prune depth maxDepth maxBreadth  (NotAClosure node)
		=	NotAClosure (prune depth maxDepth maxBreadth node)

instance showWrapped ClosureWrappedArg where
	showWrapped parentheses {closurearg}
		=	showWrapped parentheses closurearg

:: Indicator
	=	.*.

ClosureIndicator
	:==	wrap .*.

instance prune ClosureWrappedArg where
	prune depth maxDepth maxBreadth a=:{closurearg}
		=	{a & closurearg = prune depth maxDepth maxBreadth closurearg}

pruneClosure :: ClosureOption ClosureDeeplyWrappedNode -> ClosureDeeplyWrappedNode
pruneClosure ClosureShow node
	=	node
pruneClosure _ node
	=	prune node
	where
		prune (Closure descriptor args unboxed)
			=	NotAClosure ClosureIndicator
		prune (NotAClosure node)
			=	NotAClosure (mapWrapNode pruneArg node)

		pruneArg arg=:{closurearg}
			=	{arg & closurearg = prune closurearg}

:: DebugOptionRecord
	=	{maxDepth :: !Int, maxBreadth :: !Int, maxChars :: !Int, terminator :: !{#Char}}
DebugDefaultOptions
	:== {maxDepth = MaxInt, maxBreadth = MaxInt, maxChars = MaxInt, terminator = "\n"}

MaxInt
	:== (1<<31)-1

closureDebugShowWithOptions :: ClosureOption [DebugShowOption] .a -> [{#Char}]
closureDebugShowWithOptions ClosureEvaluate debugOptions debugValue 
	=	debugShowWithOptions debugOptions debugValue
closureDebugShowWithOptions closureOption debugOptions debugValue
	=	debugValue
	:-	closureDeepWrapNode
	:-	pruneClosure closureOption
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
