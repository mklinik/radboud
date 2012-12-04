definition module StdList

//	****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
//
//	Changes made for Sparkle 0.0.3a (8 January 2002):
//		o The class instance 'length' has been replaced by a definition which does not
//		  use an accumulator. A rewritten version of the old definition has been added
//		  under the name 'length2'.
//		  [the standard library of Sparkle contains a proof of "length = length2"]
//		o The 'take' function has been changed to handle negative arguments correctly.
//		  This has also resulted in a change of its strictness.
//		o The 'drop' function has been changed to handle negative arguments correctly.
//		  This has also resulted in a change of its strictness.
//		o The function 'reverse' has been replaced by a definition which does not
//		  use an accumulator. A rewritten version of the old definition has been added
//		  under the name 'reverse2'.
//		  [the standard library of Sparkle contains a proof of "reverse = reverse2"]
//		o The macro's 'foldl' and 'foldr' have been replaced by ordinary functions.
//		o The local function 'f' of 'indexList' has been renamed to 'index_list'.
//		o The cyclic let in the definition of 'repeat' has been replaced by a recursive
//		  call.
//		o The function 'sum' has been replaced by a definition which does not
//		  use an accumulator. A rewritten version of the old definition has been added
//		  under the name 'sum2'.
//		  [the standard library of Sparkle contains a proof of "sum = sum2"]
//		o The function 'prod' has been replaced by a definition which does not
//		  use an accumulator. A rewritten version of the old definition has been added
//		  under the name 'prod2'.
//		  [the standard library of Sparkle contains a proof of "prod = prod2"]
//	****************************************************************************************

import StdClass
import StdInt,StdChar,StdReal

//	Instances of overloaded functions:

instance ==	[a] | == a

instance <	[a] | Ord a

instance length	[]
instance %		[a]

instance toString 	[x] | toChar x	 // Convert [x]    via [Char] into String
instance fromString [x] | fromChar x // Convert String via [Char] into [x]

//	List Operators:

(!!) 	infixl 9	:: ![.a] Int -> .a				//	Get nth element of the list
(++)	infixr 5	:: ![.a] u:[.a] -> u:[.a]		//	Append args
flatten				:: ![[.a]] -> [.a]				//	e0 ++ e1 ++ ... ++ e##
isEmpty				:: ![.a] -> Bool				//	[] ?

//	List breaking or permuting functions:

hd			:: ![.a] -> .a							//	Head of the list
tl			:: !u:[.a] -> u:[.a]					//	Tail of the list
last		:: ![.a] -> .a							//	Last element of the list
init	 	:: ![.a] -> [.a]						//	Remove last element of the list
take		:: Int ![.a] -> [.a]					//	Take first arg1 elements of the list			; Sparkle
takeWhile	:: (a -> .Bool) !.[a] -> .[a]			//	Take elements while pred holds
drop		:: Int !u:[.a] -> u:[.a]				//	Drop first arg1 elements from the list
dropWhile	:: (a -> .Bool) !u:[a] -> u:[a]			//	Drop elements while pred holds
span		:: (a -> .Bool) !u:[a] -> (.[a],u:[a])	//	(takeWhile list,dropWhile list)
filter		:: (a -> .Bool) !.[a] -> .[a]			//	Drop all elements not satisfying pred
reverse		:: ![.a] -> [.a]							//	Reverse the list
insert 		:: (a -> a -> .Bool) a !u:[a] -> u:[a]		//	Insert arg2 when pred arg2 elem holds   
insertAt	:: !Int .a u:[.a] -> u:[.a]				//	Insert arg2 on position arg1 in list
removeAt	:: !Int !u:[.a] -> u:[.a]				//	Remove arg2!!arg1 from list
updateAt 	:: !Int .a u:[.a] -> u:[.a]				//	Replace list!!arg1 by arg2
splitAt		:: !Int u:[.a] -> ([.a],u:[.a])			//	(take n list,drop n list)

//	Creating lists:

map			:: (.a -> .b) ![.a] -> [.b]				//	[f e0,f e1,f e2,...
iterate		:: (a -> a) a -> .[a]					//	[a,f a,f (f a),...
indexList	:: !.[a] -> [Int]						//	[0..maxIndex list]
repeatn		:: !.Int a -> .[a]						//	[e0,e0,...,e0] of length n
repeat		:: a -> [a]								//	[e0,e0,...
unzip		::	![(.a,.b)] 		-> ([.a],[.b])		//	([a0,a1,...],[b0,b1,...])
zip2		:: ![.a] [.b] 		-> [(.a,.b)]		//	[(a0,b0),(a1,b1),...
zip			:: !(![.a],[.b]) 	-> [(.a,.b)]		//	[(a0,b0),(a1,b1),...
diag2		:: !.[a] .[b]		-> [.(a,b)]			//	[(a0,b0),(a1,b0),(a0,b1),...
diag3		:: !.[a] .[b] .[c]	-> [.(a,b,c)]		//	[(a0,b0,c0),(a1,b0,c0),...

//	Folding and scanning:

scan		::  (a -> .(.b -> a)) a ![.b] -> .[a]	//	[r,op r e0,op (op r e0) e1,...

//	On Booleans

and			:: ![.Bool] -> Bool						//	e0 && e1 ... && e##
or			:: ![.Bool] -> Bool						//	e0 || e1 ... || e##
any			:: (.a -> .Bool) ![.a] -> Bool			//	True, if ei is True for some i
all			:: (.a -> .Bool) ![.a] -> Bool			//	True, if ei is True for all i

//	When equality is defined on list elements

isMember		::    a	 !.[a]	-> Bool 	| Eq a	//	Is element in list
isAnyMember		:: !.[a]  !.[a] -> Bool 	| Eq a	//	Is one of arg1 an element arg2
removeMember	:: a !u:[a] -> u:[a] 		| Eq a	//	Remove first occurrence of arg1 from list arg2
removeMembers	:: !u:[a] !.[a]	-> u:[a] 	| Eq a	//	Remove first occurrences in arg2 from list arg1
removeDup		:: !.[a] 		-> .[a] 	| Eq a	//	Remove all duplicates from list
removeIndex 	:: a  !u:[a] -> (Int,u:[a])	| Eq a	//	"removeMember" returning index of removed element
limit			:: !.[a] 		-> a 		| Eq a	//	find two succeeding elements that are equal
													//	e.g. limit [1,3,2,2,1] == 2

//	When addition is defined on list elements

sum :: !.[a] -> a |  + , zero  a					//	sum of list elements, sum [] = zero

//	When multiplication and addition is defined on list elements

prod :: !.[a] -> a | * , one  a 					//	product of list elements, prod [] = one
avg :: !.[a] -> a | / , IncDec a					//	average of list elements, avg [] gives error!

foldl			:: (.a -> .(.b -> .a)) .a ![.b] -> .a		//	Sparkle
foldr			:: (.a -> .(.b -> .b)) .b ![.a] -> .b		//	Sparkle
length2			:: ![a] -> Int								//	Sparkle
reverse2		:: ![.a] -> [.a]							//	Sparkle
sum2			:: !.[Int] -> Int							//	Sparkle
prod2			:: !.[Int] -> Int							//	Sparkle

/*
// exports to generate specialized versions

export Eq Int
export Eq Real
export Eq Char

export + Int
export + Real
export * Int
export * Real
export / Int
export / Real
export zero Int
export zero Real
export one Int
export one Real
export IncDec Int
export IncDec Real

export toChar Char
export fromChar Char
*/
