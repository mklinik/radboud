definition module commondef

//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Common types for the Clean Object I/O system and their access rules.
//	********************************************************************************


import	StdClass, StdFunc
import	StdIOCommon
import	osrgn, ostoolbox, ostypes


k`						:: .x !.y -> .y

/*	Calculation rules on Integers:
*/
dist					::		!Int !Int -> Int					// dist x y = abs (x-y)
setBetween				:: !Int !Int !Int -> Int					// setBetween x low up sets low<=x<=up assuming low<=up 
setBetweenCheckBounds	:: !Int !Int !Int -> Int					// setBetweenCheckBounds x low up sets low<=x<=up if low<=up, low otherwise
isBetween				:: !Int !Int !Int -> Bool					// isBetween x low up returns low<=x<=up
minmax					::      !Int !Int -> (!Int,!Int)			// minmax a b = (min a b,max a b)


/*	Calculation rules on Points, Sizes, and Vectors:
*/
addPointSize			:: !Size !Point2 -> Point2					// {w, h }+{x,y}={x=x+w, y=y+h }

instance zero OSRect
instance ==   OSRect
class addVector a :: !Vector2 !a -> a	// add the vector argument to the second argument
instance addVector Point2
instance addVector OSRect
instance addVector Rectangle
class subVector a :: !Vector2 !a -> a	// subtract the vector argument from the second argument
instance subVector Point2
instance subVector OSRect
instance subVector Rectangle

rectangleToRect			:: !Rectangle			-> OSRect			// (l,t, r,b) such that l<=r && t<=b
rectToRectangle			:: !OSRect				-> Rectangle		// (l,t, r,b) -> {{x=l,y=t},{x=r,y=b}}
isEmptyRect				:: !OSRect				-> Bool				// (l,t, r,b) -> l==r || t==b
isEmptyRectangle		:: !Rectangle			-> Bool				// {corner1,corner2} -> corner1.x==corner2.x || corner1.y==corner2.y
pointInRect				:: !Point2 !OSRect		-> Bool				// {x,y} (l,t, r,b) -> l<=x<=r && t<=y<=b
pointInRectangle		:: !Point2 !Rectangle	-> Bool				// PointInRect point (RectangleToRect rectangle)
posSizeToRect			:: !Point2 !Size		-> OSRect			// {x,y} {w,h} -> ( x,y,  x+w,y+h )	// no check on negative size
posSizeToRectangle		:: !Point2 !Size		-> Rectangle		// {x,y} {w,h} -> {{x,y},{x+w,y+h}}	// no check on negative size
sizeToRect				::		   !Size		-> OSRect			//       {w,h} -> ( 0,0,    w,  h )	// no check on negative size
sizeToRectangle			::		   !Size		-> Rectangle		//       {w,h} -> {zero, {  w,  h}}	// no check on negative size
disjointRects			:: !OSRect !OSRect		-> Bool
intersectRects			:: !OSRect !OSRect		-> OSRect			// if disjoint: EmptyRect; otherwise the intersection
subtractRects			:: !OSRect !OSRect		-> [OSRect]			// subtract @2 from @1
rectSize				:: !OSRect				-> Size				// (l,t, r,b)          -> {abs (r-l), abs (b-t)}

/*	Rules on RgnHandles and OSRects:
*/
intersectRgnRect		:: !OSRgnHandle !OSRect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)	// the intersection of the two arguments

/*	PA: Conversion of Size, Point2, and Vector2 to tuples (toTuple) and from tuples (fromTuple):
*/
class toTuple   a :: !a -> (!Int,!Int)
class fromTuple a :: !(!Int,!Int) -> a

instance toTuple Size;		instance fromTuple Size
instance toTuple Point2;	instance fromTuple Point2
instance toTuple Vector2;	instance fromTuple Vector2

/*	PA: Conversion of OSRect, and Rectangle to 4-tuples (toTuple4) and from 4-tuples (fromTuple4):
*/
class toTuple4   a :: !a -> (!Int,!Int,!Int,!Int)
class fromTuple4 a :: !(!Int,!Int,!Int,!Int) -> a

instance toTuple4 OSRect;	instance fromTuple4 OSRect
instance toTuple4 Rectangle;instance fromTuple4 Rectangle

/*	Tuple functions:
*/
swap	:: !(.a,.b) -> (.b,.a)

/*	Common error generation rule:
*/
error					:: !String !String !String -> .x
fatalError				:: !String !String !String -> .x


/*	Universal dummy value (!!evaluation causes termination with the message: "Fatal error: dummy evaluated!"!!)
*/
dummy					:: !String -> .x


/*	Max Integer constants:
*/
MaxSigned2ByteInt		:== 32767		// 2^15-1
MaxSigned4ByteInt		:== 2147483647	// 2^31-1


/*	Bound data type:
*/
::	Bound
	=	Finite Int												// Fix a finite positive bound of N
	|	Infinite												// No bound

instance == Bound												// Finite i == Finite j && max 0 i == max 0 j; Infinite == Infinite
zeroBound:: !Bound -> Bool										// Finite i && i<=0
decBound :: !Bound -> Bound										// Finite i -> Finite (max 0 (i-1)); Infinite -> Infinite
incBound :: !Bound -> Bound										// Finite i -> Finite (max 1 (i+1)); Infinite -> Infinite


/*	List operations:
*/
::	Cond  x :== x -> Bool
::	UCond x :== x -> *(Bool,x)

(orc)  infixr 2 :: !(Cond x) (Cond x) x -> Bool
(andc) infixr 3 :: !(Cond x) (Cond x) x -> Bool

uisEmpty				:: !v:[u:x] -> (!Bool,!v:[u:x]), [v<=u]
isSingleton				:: ![.x] -> Bool
hdtl					:: !u:[.x] -> (!.x, !u:[.x])
initLast				:: ![.x] -> (![.x],!.x)
split					:: !Int !u:[.x] -> (![.x],!u:[.x])

condMap					:: (Cond x) !(IdFun x)		![x]		-> (!Bool, ![x])
uspan					:: !(UCond .x)				!u:[.x]		-> (![.x],!u:[.x])	// Same as span (StdList), but preserving uniqueness
filterMap				:: !(.x -> *(Bool,.y))		![.x]		-> [.y]
stateMap				:: !(u:x -> v:(.s -> (.y,.s))) ![u:x] !.s -> (![.y],!.s), [v<=u]
stateMap2				:: !(u:x -> .(.s -> .s)) !v:[u:x] !.s -> .s, [v <= u]
strictSeq				:: ![.(.s -> .s)]				   !.s -> .s				// Same as seq (StdFunc), but with strict state argument
strictSeqList			:: !.[.St .s .x]				   !.s -> (![.x],!.s)		// Same as seqList (StdFunc), but with strict state argument
allList					:: !(.x .s -> .(Bool,.s))   ![.x]  !.s -> (!Bool,!.s)

contains				:: !(Cond    x)				![ x] -> Bool
ucontains				:: !(UCond  .x)				!u:[.x] -> (!Bool,	!u:[.x])
cselect					:: !(Cond    x)		 x		![ x] -> (!Bool, x)
ucselect				:: !(Cond    x)		 x		!u:[ x] -> (!Bool, x,!u:[x])
selectedAtIndex			:: !(Cond    x)      x      ![ x] -> (!Index, x)			// if index==0 then not found; item was found at index
access					:: !(St .x *(Bool,.y)) .y	!u:[.x] -> (!Bool,.y,!u:[.x])
accessList				:: !(St .x .y)				![.x] -> (![.y],	![.x])
remove					:: !(Cond    x)		 x		!u:[x] -> (!Bool, x,	!u:[x])
uremove					:: !(UCond  .x)		.x		!u:[.x] -> (!Bool,.x,	!u:[.x])
creplace				:: !(Cond    x)		 x		![ x] -> (!Bool,	![ x])
ucreplace				:: !(UCond  .x)		.x		!u:[.x] -> (!Bool,	!u:[.x])
replaceOrAppend			:: !(Cond    x)      x		![ x] -> [ x]
ureplaceOrAppend		:: !(UCond  .x)     .x		!u:[.x] -> u:[.x]
removeCheck				::					 x	  !u:[x] -> (!Bool,  !u:[ x])	| Eq x
removeSpecialChars		:: ![Char] !{#Char}	-> {#Char}
ulength					:: ![.x]			-> (!Int, ![.x])
disjointLists			:: ![x] ![x]		-> Bool		| Eq x
noDuplicates			:: ![x]				-> Bool		| Eq x
unzip3					:: ![(.a,.b,.c)]	-> (![.a],![.b],![.c])
unzip4					:: ![(.a,.b,.c,.d)]	-> (![.a],![.b],![.c],![.d])
