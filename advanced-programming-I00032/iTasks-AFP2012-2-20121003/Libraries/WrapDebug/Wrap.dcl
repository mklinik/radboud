/*
	Wrap Clean nodes (for debugging purposes).

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
definition module Wrap

from StdOverloaded import class toString (..)

::	WrappedDescriptorId

instance toString WrappedDescriptorId

::	WrappedDescriptor
    =   WrappedDescriptorCons
    |   WrappedDescriptorNil
    |   WrappedDescriptorTuple
    |   WrappedDescriptorOther !WrappedDescriptorId
    |   WrappedDescriptorUnknown

instance toString WrappedDescriptor

:: UnwrappedArg
	=	E.a: {node :: a}
:: ShallowlyWrappedNode
	:==	WrappedNode UnwrappedArg
:: DeeplyWrappedNode
	:==	WrappedNode WrappedArg
:: WrappedArg
	=	{arg :: DeeplyWrappedNode}

::  WrappedNode arg
	//	basic types
    =   WrappedInt !Int
    |   WrappedChar !Char
    |   WrappedBool !Bool
    |   WrappedReal !Real
    |   WrappedFile !File

	// unboxed arrays of basic types
    |   WrappedString !{#Char}
    |   WrappedIntArray !{#Int}
    |   WrappedBoolArray !{#Bool}
    |   WrappedRealArray !{#Real}
    |   WrappedFileArray !{#File}

	// other arrays
    |   WrappedArray !{!arg}

	// records
    |   WrappedRecord !WrappedDescriptor !{!arg}

	// unboxed lists
    |   WrappedUnboxedList !WrappedDescriptor !{!arg}

	// unboxed lists of records
    |   WrappedUnboxedRecordList !WrappedDescriptor !{!arg}

	// other nodes
    |   WrappedOther !WrappedDescriptor !{!arg}


deepWrap :: !.a -> DeeplyWrappedNode
shallowWrap :: !.a -> ShallowlyWrappedNode

class wrap a :: .b -> a
instance wrap WrappedArg
instance wrap (WrappedNode a) | wrap a
mapWrapNode :: (a -> b) (WrappedNode a) -> WrappedNode b
