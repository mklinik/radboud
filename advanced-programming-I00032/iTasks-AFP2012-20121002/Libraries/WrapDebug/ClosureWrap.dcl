/*
	Wrap Clean closures.

	Version 1.0.5
	Arjen van Weelden
	arjenw@cs.ru.nl
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
definition module ClosureWrap

from Wrap import ::WrappedNode, ::WrappedDescriptor,
					::UnwrappedArg, ::WrappedArg, class wrap

::  ClosureWrappedNode arg
	=	Closure !WrappedDescriptor !{!arg} !Int
	|	NotAClosure !(WrappedNode arg)
:: ClosureWrappedArg
	=	{closurearg :: ClosureDeeplyWrappedNode}

:: ClosureShallowlyWrappedNode
	:==	ClosureWrappedNode UnwrappedArg
:: ClosureDeeplyWrappedNode
	:==	ClosureWrappedNode ClosureWrappedArg

closureShallowWrapNode :: .a -> ClosureShallowlyWrappedNode
closureDeepWrapNode :: .a -> ClosureDeeplyWrappedNode

instance wrap (ClosureWrappedNode a) | wrap a
instance wrap ClosureWrappedArg

// check whether all functions have descriptors
closureHasDescriptor :: Bool