/*
	Show Wrapped Node

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module ShowWrapped

import StdEnv
import Wrap

instance == ShowWrappedOptions where
	(==) Don`tShowParentheses Don`tShowParentheses
		=	True
	(==) ShowParentheses ShowParentheses
		=	True
	(==) ShowInList ShowInList
		=	True
	(==) ShowInUnboxedList ShowInUnboxedList
		=	True
	(==) ShowInUnboxedRecordList ShowInUnboxedRecordList
		=	True
	(==) _ _
		=	False

showNil :: ShowWrappedOptions -> [{#Char}]
showNil options
	|  options == Don`tShowParentheses || options == ShowParentheses
		=	["[]"]
	// otherwise
		=	[]

showList :: ShowWrappedOptions ShowWrappedOptions a a -> [{#Char}]
															| showWrapped a
showList options listkind head tail
	=	showList2 options listkind (showWrapped Don`tShowParentheses head) tail

showList2 :: ShowWrappedOptions ShowWrappedOptions [{#Char}] a -> [{#Char}]
															| showWrapped a
showList2 options listkind head tail
	|  options == Don`tShowParentheses || options == ShowParentheses
		=	["[", indicator listkind] ++ headTail listkind head tail ++ ["]"]
	| options == listkind
		=	[", " : headTail listkind head tail]
	// otherwise
		=	[" : " : showList2 Don`tShowParentheses listkind head tail]
	where
		indicator :: ShowWrappedOptions -> {#Char}
		indicator ShowInList
			=	""
		indicator ShowInUnboxedList
			=	"#"
		indicator ShowInUnboxedRecordList
			=	"#"

		headTail :: ShowWrappedOptions  [{#Char}] a -> [{#Char}] | showWrapped a
		headTail options head tail
			=	head
			++	showWrapped options tail

showNonList :: ShowWrappedOptions (WrappedNode a) -> [{#Char}]
															| showWrapped a
showNonList options node
	|  options == Don`tShowParentheses || options == ShowParentheses
		=	showNode options node
	// otherwise
		=	[" : " : showNode options node]

showApplication :: ShowWrappedOptions {#Char} {!a} -> [{#Char}] | showWrapped a
showApplication options symbol args
	| options == ShowParentheses && size args > 0
		=	["(" : application] ++ [")"]
	// otherwise
		=	application
	where
		application
			=	flatten (intersperse [" "]
					[[symbol] : [showWrapped ShowParentheses arg \\ arg <-: args]])

instance showWrapped WrappedArg where
	showWrapped parentheses {arg}
		=	showWrapped parentheses arg

instance showWrapped (WrappedNode a) | showWrapped a where
	showWrapped options (WrappedOther WrappedDescriptorNil args)
		| size args == 0
			=	showNil options
		// otherwise
			=	showApplication options "[]" args
	showWrapped options (WrappedOther WrappedDescriptorCons args)
		| size args == 2
			=	showList options ShowInList args.[0] args.[1]
		// otherwise
			=	showApplication options "[:]" args
	showWrapped options (WrappedUnboxedList descriptor args)
		| size args == 2
			=	showList options ShowInUnboxedList args.[0] args.[1]
		// otherwise
			=	showApplication options "[#:]" args
	showWrapped options (WrappedUnboxedRecordList descriptor args)
		| n >= 2
			=	showList2 options ShowInUnboxedList head tail
		// otherwise
			=	showApplication options "[#:]" args
			where
				n
					=	size args
				head
					=	showWrapped options (WrappedRecord descriptor
										{arg \\ arg <-: args & _ <- [0..n-2]})
				tail
					=	args.[n-1]
	showWrapped options node
		=	showNonList options node

showNode :: !ShowWrappedOptions !(WrappedNode a) -> [{#Char}] | showWrapped a
showNode _ (WrappedInt i)
	=	[toString i]
showNode _ (WrappedChar c)
	=	["\'" +++ toString c +++ "\'"]
showNode _ (WrappedBool b)
	=	[toString b]
showNode _ (WrappedReal r)
	=	[toString r]
showNode _ (WrappedFile _)
	=	["File"]
showNode _ (WrappedString s)
	=	["\"" +++ s +++ "\""]
showNode _ (WrappedIntArray a)
	=	showBasicArray a
showNode _ (WrappedBoolArray a)
	=	showBasicArray a
showNode _ (WrappedRealArray a)
	=	showBasicArray a
showNode _ (WrappedFileArray a)
	=	showBasicArray a
showNode options (WrappedArray a)
	=	["{" : flatten (intersperse [", "] [showWrapped options el \\ el <-: a])] ++ ["}"]
showNode options (WrappedRecord descriptor args)
	=	["{" : flatten (intersperse [" "] [[showDescriptor descriptor]
			: [showWrapped ShowParentheses arg \\ arg <-: args]])] ++ ["}"]
showNode _ (WrappedOther WrappedDescriptorTuple args)
	=	["(" : flatten (intersperse [", "] [showWrapped Don`tShowParentheses arg \\ arg <-: args])] ++ [")"]
showNode options (WrappedOther descriptor args)
	= showApplication options (showDescriptor descriptor) args

showDescriptor :: WrappedDescriptor -> {#Char}
showDescriptor (WrappedDescriptorOther id)
	=	toString id
showDescriptor WrappedDescriptorNil
	=	"[]"
showDescriptor WrappedDescriptorCons
	=	"[:]"
showDescriptor WrappedDescriptorTuple
	=	"(..)"

showBasicArray :: {#a} -> [{#Char}] | toString a & Array {#} a
showBasicArray a
	=	["{" : intersperse ", " [toString el \\ el <-: a]] ++ ["}"]

showWrappedArray :: {WrappedNode a} -> [{#Char}] | showWrapped a
showWrappedArray a
	=	["{" : flatten (intersperse [", "]
			[showWrapped Don`tShowParentheses el \\ el <-: a])] ++ ["}"]

intersperse :: a [a] -> [a]
intersperse separator [a : t=:[b : _]]
	=	[a, separator : intersperse separator t]
intersperse _ l
	=	l

instance toString File
where
	toString :: !File -> {#Char}
	toString _
		=	"File"

showWrappedNode :: a -> [{#Char}] | showWrapped a
showWrappedNode a
	=	showWrapped Don`tShowParentheses a

:: Indicators
	=	@...
	|	.+.

MaxCharsString
	:==	".."
MaxBreadthString
	:==	"..."
MaxBreadthIndicator
	:==	wrap @...
MaxDepthIndicator
	:==	wrap .+.

class prune a | wrap a where
	prune :: !Int !Int !Int a -> a

instance prune (WrappedNode a) | prune a where
	prune depth maxDepth maxBreadth value
		| depth == maxDepth
			=	MaxDepthIndicator
	prune depth maxDepth maxBreadth (WrappedIntArray a)
		=	pruneBasicArray depth maxDepth maxBreadth a
	prune depth maxDepth maxBreadth (WrappedBoolArray a)
		=	pruneBasicArray depth maxDepth maxBreadth a
	prune depth maxDepth maxBreadth (WrappedRealArray a)
		=	pruneBasicArray depth maxDepth maxBreadth a
	prune depth maxDepth maxBreadth (WrappedFileArray a)
		=	pruneBasicArray depth maxDepth maxBreadth a
	prune depth maxDepth maxBreadth (WrappedString a)
		| size a > maxBreadth
			=	WrappedString ((a % (0, maxBreadth-1)) +++ MaxBreadthString)
	prune depth maxDepth maxBreadth (WrappedArray a)
		=	WrappedArray (pruneArray depth maxDepth maxBreadth a)
	prune depth maxDepth maxBreadth (WrappedRecord descriptor args)
		=	WrappedRecord descriptor (pruneArray depth maxDepth maxBreadth args)
	prune depth maxDepth maxBreadth (WrappedOther WrappedDescriptorCons args)
		| size args == 2
			=	WrappedOther WrappedDescriptorCons
					{	prune (depth+1) maxDepth maxBreadth args.[0]
					,	prune depth maxDepth maxBreadth args.[1]
					}
	prune depth maxDepth maxBreadth (WrappedOther WrappedDescriptorTuple args)
		=	WrappedOther WrappedDescriptorTuple (pruneArray depth maxDepth maxBreadth args)
	prune depth maxDepth maxBreadth (WrappedOther descriptor args)
		=	WrappedOther descriptor (pruneArray depth maxDepth maxBreadth args)
	prune _ _ _ a
		=	a

pruneArray :: !Int !Int !Int !{!a} -> {!a} | prune a
pruneArray depth maxDepth maxBreadth a
	| size a > maxBreadth
		=	{{prune (depth+1) maxDepth maxBreadth e \\ e <-: a & i <- [0 .. maxBreadth]}
				& [maxBreadth] = MaxBreadthIndicator}
	// otherwise
		=	{prune (depth+1) maxDepth maxBreadth e \\ e <-: a}

pruneBasicArray :: !Int !Int !Int !(a b) -> WrappedNode c | Array a b & prune c
pruneBasicArray depth maxDepth maxBreadth a
	| size a > maxBreadth
		=	WrappedArray (pruneArray depth maxDepth maxBreadth {wrap e \\ e <-: a & i <- [0 .. maxBreadth]})
	// otherwise
		=	WrappedArray {wrap e \\ e <-: a}

instance prune WrappedArg where
	prune depth maxDepth maxBreadth a=:{arg}
		=	{a & arg = prune depth maxDepth maxBreadth arg}

/* FIXME handle newlines in strings correctly */
chop :: !Int [{#Char}] -> [{#Char}]
chop _ []
	=	[]
chop maxChars list=:[string:strings]
	| maxChars < stringSize + sizeMaxCharsString
		| fits maxChars list
			=	list
		| stringSize > sizeMaxCharsString
			=	[string % (0, maxChars-sizeMaxCharsString-1), MaxCharsString]
		// otherwise
			=	[MaxCharsString]
	// otherwise
		=	[string : chop (maxChars - stringSize) strings]
	where
		stringSize
			=	size string
		sizeMaxCharsString
			=	size MaxCharsString

		fits :: !Int [{#Char}] -> Bool
		fits _ []
			=	True
		fits maxChars [h : t]
			=	maxChars >= size h && fits (maxChars - size h) t
