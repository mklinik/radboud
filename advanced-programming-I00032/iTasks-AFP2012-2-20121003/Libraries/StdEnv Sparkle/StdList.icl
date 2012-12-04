implementation module StdList

// ****************************************************************************************
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
// ****************************************************************************************

import	StdClass, StdMisc, StdEnum, StdInt, StdChar, StdBool, StdArray, StdString, StdReal

// ****************************************************************************************
//	Instances of overloaded functions:
// ****************************************************************************************

instance == [a] | == a
	where
	(==) :: ![a] ![a] -> Bool | == a
	(==) [] []
		= True
	(==) [] _
		= False
	(==) [_:_] []
		= False
	(==) [a:as] [b:bs] 
		| a == b
			= as == bs
		// otherwise
			= False

instance < [a] | Ord a
	where
	(<) :: ![a]	![a] ->	Bool | Ord a
	(<) [] 	 []
		= False
	(<) [] _
		= True
	(<) [_:_] []
		= False
	(<) [a:as] [b:bs]
		| a < b
			= True
		| a > b
			= False
		// otherwise
			= as < bs

// Sparkle
instance length []
	where
		length :: ![a] -> Int
		length [x:xs]
			= length xs + 1
		length []
			= 0

// Sparkle
length2 :: ![a] -> Int
length2 list
	= acc_length 0 list
	where
		acc_length :: !Int ![a] -> Int
		acc_length n [x:xs]
			= acc_length (inc n) xs
		acc_length n []
			= n
	
instance % [a]
	where
	(%) :: ![a] !(!Int,!Int) -> [a]
	(%) list (frm,to) = take (to - frm + 1) (drop frm list)
		
instance toString [x] | toChar x
	where
	toString::![x] -> {#Char} | toChar x
	toString xs = ltosacc 0 xs (createArray l ' ')
	where
		l					= length xs
		ltosacc i [h:t] arr	= ltosacc (inc i) t {arr & [i]=toChar h}
		ltosacc _ []	arr	= arr

instance fromString [x] | fromChar x
	where
	fromString::!{#Char} -> [x] | fromChar x
	fromString s = stolacc s (size s - 1) []
	where
		stolacc :: !String !Int u:[a] -> u:[a] | fromChar a
		stolacc s i acc 
			| i >= 0
				= stolacc s (dec i) [fromChar (s.[i]) : acc] 
			// otherwise
				= acc


// ****************************************************************************************
// standard operators
// ****************************************************************************************
(!!) infixl 9::![.a] Int -> .a
(!!) [] _
	= subscript_error
(!!) list i
	=	index list i
	where
		index ::![.a] !Int -> .a
		index [hd:tl] 0
			= hd
		index [hd:tl] n
			= index tl (n - 1)
		index [] _
			= subscript_error

subscript_error = abort "Subscript error in !!,index too large"

(++) infixr 5::![.a] u:[.a] -> u:[.a]
(++) [hd:tl]	list	= [hd:tl ++ list]
(++) nil 		list	= list

flatten :: ![[.a]] -> [.a]
flatten [h:t]	= h ++ flatten t
flatten []		= []

isEmpty::![.a] -> Bool
isEmpty	[] = True
isEmpty	_  = False

// ****************************************************************************************
// standard functions
// ****************************************************************************************

hd::![.a] -> .a
hd [a:x]	= a
hd []		= abort "hd of []"

tl::!u:[.a] -> u:[.a]
tl [a:x]	= x
tl []		= abort "tl of []"

last::![.a] -> .a
last [a]	= a
last [a:tl]	= last tl
last []		= abort "last of []"

init :: ![.a] -> [.a];
init []       = []
init [x]     = []
init [x:xs] = [x: init xs]

// Sparkle
take :: Int ![.a] -> [.a]
take n [x:xs]
	| n > 0				= [x: take (n-1) xs]
	= []
take n []
	= []

takeWhile::(a -> .Bool) !.[a] -> .[a]
takeWhile f [a:x] | f a	= [a:takeWhile f x]
						= []
takeWhile f []			= []

// Sparkle
drop :: Int !u:[.a] -> u:[.a]
drop n [x:xs]
	| n > 0				= drop (n-1) xs
	= [x:xs]
drop n []
	= []

dropWhile :: (a -> .Bool) !u:[a] -> u:[a]
dropWhile f cons=:[a:x]	| f a	= dropWhile f x
								= cons
dropWhile f []					= []

span :: (a -> .Bool) !u:[a] -> (.[a],u:[a])
span p list=:[x:xs]
	| p x
		= ([x:ys],zs)
		 with	(ys,zs) = span p xs
		= ([],list)
span p []
	= ([], [])

filter::(a -> .Bool) !.[a] -> .[a]
filter f [a:x]	| f a	= [a:filter f x]
						= filter f x
filter f []				= []

// Sparkle
reverse :: ![.a] -> [.a]
reverse [x:xs]
	= reverse xs ++ [x]
reverse []
	= []

// Sparkle
reverse2 :: ![.a] -> [.a]
reverse2 list
	= acc_reverse list []
	where 
		acc_reverse :: ![.a] u:[.a] -> v:[.a], [u <= v]
		acc_reverse [x:xs] list
			= acc_reverse xs [x:list]
		acc_reverse [] list
			= list

insert :: (a a -> .Bool) a !u:[a] -> u:[a];
insert r x ls=:[y : ys]
	| r x y		= 	[x : ls]
				=	[y : insert r x ys]
insert _ x [] 	= 	[x]

insertAt :: !Int .a u:[.a] -> u:[.a]
insertAt 0 x ys	= [x:ys]
insertAt _ x []	= [x]
insertAt n x [y:ys]	= [y : insertAt (n-1) x ys]

removeAt :: !Int !u:[.a] -> u:[.a];
removeAt 0 [y : ys]	= ys
removeAt n [y : ys]	= [y : removeAt (n-1) ys]
removeAt n []		= []

updateAt :: !Int .a u:[.a] -> u:[.a]
updateAt 0 x []	= []
updateAt 0 x [y:ys]	= [x:ys]
updateAt _ x []	= []
updateAt n x [y:ys]	= [y : updateAt (n-1) x ys]

splitAt :: !Int u:[.a] -> ([.a],u:[.a])
splitAt 0     xs	=	([],xs)
splitAt _     []	=	([],[])
splitAt n [x:xs]	=	([x:xs`],xs``) 
	where
		(xs`,xs``) = splitAt (n-1) xs

// Sparkle
foldl :: (.a -> .(.b -> .a)) .a ![.b] -> .a
foldl op r []
	= r
foldl op r [a:x]
	= foldl op (op r a) x

// Sparkle
foldr :: (.a -> .(.b -> .b)) .b ![.a] -> .b
foldr op r []
	= r
foldr op r [a:x]
	= op a (foldr op r x)

// Sparkle
indexList ::!.[a] -> [Int]
indexList x = index_list 0 x
where
	index_list :: !Int ![a] -> [Int]
	index_list n [a:x]
		= [n: index_list (n+1) x]
	index_list n []
		= []

iterate::(a -> a) a -> .[a]
iterate f x	= [x:iterate f (f x)]

map::(.a -> .b) ![.a] -> [.b]
map f [a:x]	= [f a:map f x]
map f []	= []

repeatn::!.Int a -> .[a]
repeatn n x	= take n (repeat x)

// Sparkle
repeat :: a -> [a]
repeat x
	= [x: repeat x]

scan:: (a -> .(.b -> a)) a ![.b] -> .[a]
scan op r [a:x]	= [r:scan op (op r a) x]
scan op r []	= [r]

unzip::![(.a,.b)] -> ([.a],[.b])
unzip []	= 	([], [])
unzip [(x,y) : xys] = ([x : xs],[y : ys])
where
	(xs,ys) = unzip xys

zip2::![.a] [.b] -> [(.a,.b)]
zip2 [a:as] [b:bs]	= [(a,b):zip2 as bs]
zip2 as bs			= []

zip::!(![.a],[.b]) -> [(.a,.b)]
zip (x,y) = zip2 x y

diag3:: !.[a] .[b] .[c]-> [.(a,b,c)]
diag3 xs ys zs = [ (x,y,z) \\ ((x,y),z) <- diag2 (diag2 xs  ys) zs ]

//	diagonalisation: basic idea (for infinite lists):
//
//	diag2 xs ys = flatten [ dig2n n xs ys \\ n <- [1..] ]
//	where dig2n n xs ys = [ (a,b) \\ a <- reverse (take n xs) & b <- take n ys ]
//
//	in the definition below this idea is adapted in order to deal with finite lists too

diag2:: !.[a] .[b] -> [.(a,b)]
diag2 [] ys = []
diag2 xs [] = []
diag2 xs ys = [ (ae,be) \\ (a,b) <- takeall xs [] ys [], ae <- a & be <- b ]
where
	takeall xin xout yin yout
	| morex&&morey	= [(nxout,   nyout) : takeall nxin nxout nyin     nyout ]
	| morey			= [( xout,tl nyout) : takeall  xin  xout nyin (tl nyout)]
	| morex			= [(nxout,    yout) : takeall nxin nxout  yin      yout ]
	// otherwise
					= shift xout yout
	where
		(morex,nxin,nxout) = takexnext xin xout
		(morey,nyin,nyout) = takeynext yin yout

		takexnext [x:xs] accu	= (True, xs,[x:accu])
		takexnext []     accu 	= (False,[],accu)

		takeynext [y:ys] accu	= (True, ys,accu++[y])
		takeynext []     accu	= (False,[],accu)
	
		shift xout [_:ys]	= [(xout,ys): shift xout ys]
		shift _    [] 		= []

// ****************************************************************************************
// Boolean list
// ****************************************************************************************

and::![.Bool] -> Bool
and []
	= True
and [b : tl]
	= b && and tl

or::![.Bool] -> Bool
or []
	= False
or [b : tl]
	= b || or tl

any::(.a -> .Bool) ![.a] -> Bool
any p []
	= False
any p [b : tl]
	= p b || any p tl

all::(.a -> .Bool) ![.a] -> Bool
all p []
	=	True
all p [b : tl]
	= p b && all p tl

isMember::a !.[a] -> Bool | Eq a
isMember x [hd:tl] = hd==x || isMember x tl
isMember x []	= False

isAnyMember	:: !.[a] !.[a] -> Bool | Eq a		// Is one of arg1 an element arg2
isAnyMember [x:xs] list = isMember x list || isAnyMember xs list
isAnyMember [] list = False

removeDup :: !.[a] -> .[a] | Eq a
removeDup [x:xs] = [x:removeDup (filter ((<>) x) xs)]
removeDup _      = []

removeMember:: a !u:[a] -> u:[a] | Eq a
removeMember e [a:as]
	| a==e		= as
				= [a:removeMember e as]
removeMember e [] = []	

removeMembers::!u:[a] !.[a] -> u:[a] | Eq a
removeMembers x []		= x
removeMembers x [b:y]	= removeMembers (removeMember b x) y

removeIndex :: a !u:[a] -> (Int,u:[a]) | Eq a
removeIndex e xs = removei e xs 0
where
	removei :: a u:[a] !Int -> (Int,u:[a]) | == a;
	removei e [x:xs] i
		| x==e
			= (i,xs)
			= (j,[x:res])
			with
				(j,res) = removei e xs (inc i)
	removei e [] i = abort "Error in removeIndex: element not found"

limit::!.[a] -> a | Eq a
limit [a:cons=:[b:x]]
	| a==b		= a
	// otherwise
				= limit cons
limit other		= abort "incorrect use of limit"

// ****************************************************************************************
// On PlusMin
// ****************************************************************************************

// Sparkle
sum :: !.[a] -> a | +, zero a
sum [x:xs]
	= x + sum xs
sum []
	= zero

// Sparkle
sum2 :: !.[Int] -> Int
sum2 xs
	= acc_sum xs 0
	where
		acc_sum :: !.[Int] !Int -> Int
		acc_sum [x:xs] n
			= acc_sum xs (n + x)
		acc_sum [] n
			= n

// ****************************************************************************************
// On Arith
// ****************************************************************************************

// Sparkle
prod :: !.[a] -> a | *, one a
prod [x:xs]
	= x * prod xs
prod []
	= one

// Sparkle
prod2 :: !.[Int] -> Int
prod2 xs
	= acc_prod 1 xs
	where
		acc_prod :: !Int ![Int] -> Int
		acc_prod n [x:xs]
			= acc_prod (n * x) xs
		acc_prod n []
			= n

avg:: !.[a] -> a | / , IncDec a
avg [] = abort "avg called with empty list"
avg x  = accavg zero zero x
where
	accavg n nelem [x:xs] = accavg (n + x) (inc nelem) xs
	accavg n nelem []     = n / nelem
