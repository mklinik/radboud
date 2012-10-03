implementation module commondef


import	StdArray, StdBool, StdChar, StdClass, StdEnum, StdFunc, StdInt, StdList, StdMisc, StdReal, StdString
import	osrgn
import	StdIOCommon



/*	Extensions of StdFunc:
*/
k` :: .x !.y -> .y
k` _ y = y


/*	Calculation rules on Integers:
*/
dist :: !Int !Int -> Int
dist x y = abs (x-y)

setBetween :: !Int !Int !Int -> Int
setBetween x low up
	| x<=low	= low
	| x>=up		= up
	| otherwise	= x

setBetweenCheckBounds :: !Int !Int !Int -> Int
setBetweenCheckBounds x low up
	| up <= low	= low
	| x<=low	= low
	| x>=up		= up
	| otherwise	= x

isBetween :: !Int !Int !Int -> Bool
isBetween x low up
	| x<low		= False
	| otherwise	= x<=up

minmax :: !Int !Int -> (!Int,!Int)
minmax a b
	| a<=b		= (a,b)
	| otherwise	= (b,a)


/*	Calculation rules on Points, Sizes, and Vectors:
*/
addPointSize :: !Size !Point2 -> Point2
addPointSize {w,h} {x,y} = {x=x+w,y=y+h}


instance zero OSRect where
	zero = {rleft=0,rtop=0,rright=0,rbottom=0}
instance == OSRect where
	(==) r1 r2 = r1.rleft==r2.rleft && r1.rtop==r2.rtop && r1.rright==r2.rright && r1.rbottom==r2.rbottom
class addVector a :: !Vector2 !a -> a	// add the vector argument to the second argument
instance addVector Point2 where
	addVector {vx,vy} {x,y} = {x=x+vx,y=y+vy}
instance addVector OSRect where
	addVector {vx,vy} {rleft,rtop,rright,rbottom} = {rleft=rleft+vx,rtop=rtop+vy,rright=rright+vx,rbottom=rbottom+vy}
instance addVector Rectangle where
	addVector v {corner1,corner2} = {corner1=addVector v corner1,corner2=addVector v corner2}
class subVector a :: !Vector2 !a -> a	// subtract the vector argument from the second argument
instance subVector Point2 where
	subVector {vx,vy} {x,y} = {x=x-vx,y=y-vy}
instance subVector OSRect where
	subVector {vx,vy} {rleft,rtop,rright,rbottom} = {rleft=rleft-vx,rtop=rtop-vy,rright=rright-vx,rbottom=rbottom-vy}
instance subVector Rectangle where
	subVector v {corner1,corner2} = {corner1=subVector v corner1,corner2=subVector v corner2}

rectangleToRect :: !Rectangle -> OSRect
rectangleToRect {corner1={x=a,y=b},corner2={x=a`,y=b`}}
	| x_less_x` && y_less_y`= {rleft=a, rtop=b, rright=a`,rbottom=b`}
	| x_less_x`				= {rleft=a, rtop=b`,rright=a`,rbottom=b }
	| y_less_y`				= {rleft=a`,rtop=b, rright=a, rbottom=b`}
	| otherwise				= {rleft=a`,rtop=b`,rright=a, rbottom=b }

where
	x_less_x` = a<=a`
	y_less_y` = b<=b`

rectToRectangle :: !OSRect -> Rectangle
rectToRectangle {rleft,rtop,rright,rbottom}
	= {corner1={x=rleft,y=rtop},corner2={x=rright,y=rbottom}}

isEmptyRect :: !OSRect -> Bool
isEmptyRect {rleft,rtop,rright,rbottom}
	= rleft==rright || rtop==rbottom

isEmptyRectangle :: !Rectangle -> Bool
isEmptyRectangle {corner1,corner2}
	= corner1.x==corner2.x || corner1.y==corner2.y

pointInRect :: !Point2 !OSRect -> Bool
pointInRect {x,y} {rleft,rtop,rright,rbottom}
	= isBetween x rleft rright && isBetween y rtop rbottom

pointInRectangle :: !Point2 !Rectangle -> Bool
pointInRectangle point rectangle
	= pointInRect point (rectangleToRect rectangle)

posSizeToRect :: !Point2 !Size -> OSRect
posSizeToRect {x,y} {w,h}
	= {rleft=left,rtop=top, rright=right,rbottom=bottom}
where
	(left,right)	= minmax x (x+w)
	(top,bottom)	= minmax y (y+h)

posSizeToRectangle :: !Point2 !Size -> Rectangle
posSizeToRectangle pos=:{x,y} {w,h}
	= {corner1=pos,corner2={x=x+w,y=y+h}}

sizeToRect :: !Size -> OSRect
sizeToRect size
	= posSizeToRect zero size

sizeToRectangle :: !Size -> Rectangle
sizeToRectangle {w,h}
	= {zero & corner2={x=w,y=h}}

disjointRects :: !OSRect !OSRect -> Bool
disjointRects rect1 rect2
	= isEmptyRect rect1 || isEmptyRect rect2 || rect1.rleft>=rect2.rright || rect1.rbottom<=rect2.rtop || rect1.rright<=rect2.rleft || rect1.rtop>=rect2.rbottom

intersectRects :: !OSRect !OSRect -> OSRect
intersectRects rect1 rect2
	| disjointRects rect1 rect2	= zero
	| otherwise					= {	rleft	= max rect1.rleft   rect2.rleft
								  ,	rtop	= max rect1.rtop    rect2.rtop
								  ,	rright	= min rect1.rright  rect2.rright
								  ,	rbottom	= min rect1.rbottom rect2.rbottom
								  }

subtractRects :: !OSRect !OSRect -> [OSRect]
subtractRects rect1 rect2
	= subtractFittingRect rect1 (intersectRects rect1 rect2)
where
//	subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
	subtractFittingRect :: !OSRect !OSRect -> [OSRect]
	subtractFittingRect {rleft=l1,rtop=t1,rright=r1,rbottom=b1} {rleft=l2,rtop=t2,rright=r2,rbottom=b2}
		= filter (not o isEmptyRect) (map fromTuple4 [(l1,t1,r1,t2),(l1,t2,l2,b2),(r2,t2,r1,b2),(l1,b2,r1,b1)])


rectSize :: !OSRect -> Size
rectSize {rleft,rtop,rright,rbottom}
	= {w=abs (rright-rleft),h=abs (rbottom-rtop)}


/*	Rules on RgnHandles and Rects:
*/
intersectRgnRect :: !OSRgnHandle !OSRect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
intersectRgnRect rgnH rect tb
	# (aidRgn,tb)	= osnewrectrgn rect tb
	# (secRgn,tb)	= ossectrgn rgnH aidRgn tb
	# tb			= osdisposergn   aidRgn tb
	= (secRgn,tb)

/*	PA: Conversion of Size, Point2, and Vector2 to tuples (toTuple) and from tuples (fromTuple):
*/
class toTuple   a :: !a -> (!Int,!Int)
class fromTuple a :: !(!Int,!Int) -> a

instance toTuple Size where
	toTuple {w,h} = (w,h)
instance toTuple Point2 where
	toTuple {x,y} = (x,y)
instance toTuple Vector2 where
	toTuple {vx,vy} = (vx,vy)

instance fromTuple Size where
	fromTuple (w,h) = {w=w,h=h}
instance fromTuple Point2 where
	fromTuple (x,y) = {x=x,y=y}
instance fromTuple Vector2 where
	fromTuple (vx,vy) = {vx=vx,vy=vy}

/*	PA: Conversion of OSRect, and Rectangle to 4-tuples (toTuple4) and from 4-tuples (fromTuple4):
*/
class toTuple4   a :: !a -> (!Int,!Int,!Int,!Int)
class fromTuple4 a :: !(!Int,!Int,!Int,!Int) -> a

instance toTuple4 OSRect where
	toTuple4 {rleft,rtop,rright,rbottom} = (rleft,rtop,rright,rbottom)
instance toTuple4 Rectangle where
	toTuple4 r = toTuple4 (rectangleToRect r)
instance fromTuple4 OSRect where
	fromTuple4 r = rectangleToRect (fromTuple4 r)
instance fromTuple4 Rectangle where
	fromTuple4 (l,t,r,b) = {corner1={x=l,y=t},corner2={x=r,y=b}}


/*	Tuple functions:
*/
swap :: !(.a,.b) -> (.b,.a)
swap (a,b) = (b,a)


/*	Error generation rule:
*/
error :: !String !String !String -> .x
error rule moduleName error
	= abort ("Error in rule "+++rule+++" ["+++moduleName+++"]: "+++error+++".\n")

//	PA: new version of error to dump fatal errors.
fatalError :: !String !String !String -> .x
fatalError rule moduleName error
	= abort ("Fatal error in rule "+++rule+++" ["+++moduleName+++"]: "+++error+++".\n")


/*	Universal dummy value (!!evaluation causes termination with the message: "Fatal error: dummy evaluated!"!!)
*/
dummy :: !String -> .x
dummy error = abort ("Fatal error: dummy evaluated! "+++error+++".\n")


/*	Max Integer constants:
*/
MaxSigned2ByteInt	:== 32767		// 2^15-1
MaxSigned4ByteInt	:== 2147483647	// 2^31-1


/*	Bound data type:
*/
::	Bound
	=	Finite Int												// Fix a finite positive bound of N
	|	Infinite												// No bound

instance == Bound where
	(==) (Finite i)	(Finite j)	= i==j || i<=0 && j<=0
	(==) Infinite	Infinite	= True
	(==) _			_			= False

zeroBound:: !Bound -> Bool
zeroBound (Finite i)	= i<=0
zeroBound _				= False

decBound :: !Bound -> Bound
decBound (Finite i)
	| i<=0		= Finite 0
	| otherwise	= Finite (i-1)
decBound bound	= bound

incBound :: !Bound -> Bound
incBound (Finite i)
	| i<=0		= Finite 1
	| otherwise	= Finite (i+1)
incBound bound	= bound


/*	List operations:
*/
::	Cond  x :== x -> Bool
::	UCond x :== x -> *(Bool,x)

(orc) infixr 2 :: !(Cond x) (Cond x) x -> Bool
(orc) ca cb x = ca x || cb x

(andc) infixr 3 :: !(Cond x) (Cond x) x -> Bool
(andc) ca cb x = ca x && cb x

uisEmpty :: !v:[u:x] -> (!Bool,!v:[u:x]), [v<=u]
uisEmpty []   = (True,[])
uisEmpty full = (False,full)

isSingleton :: ![.x] -> Bool
isSingleton [x]	= True
isSingleton _	= False

hdtl :: !u:[.x] -> (!.x, !u:[.x])
hdtl [x:xs]		= (x,xs)

initLast :: ![.x] -> (![.x],!.x)
initLast [x]
	= ([],x)
initLast [x:xs]
	# (init,last)	= initLast xs
	= ([x:init],last)

split :: !Int !u:[.x] -> (![.x],!u:[.x])
split _ []
	= ([],[])
split n xs
	| n<=0
		= ([],xs)
	| otherwise
		# (x, xs)	= hdtl xs
		# (ys,zs)	= split (n-1) xs
		= ([x:ys],zs)

condMap :: (Cond x) !(IdFun x) ![x] -> (!Bool,![x])
condMap c f [x:xs]
	# (b,xs)	= condMap c f xs
	| c x		= (True,[f x:xs])
	| otherwise	= (b,   [x:xs])
condMap _ _ _
	= (False,[])

uspan :: !(UCond .a) !u:[.a] -> (![.a],!u:[.a])		// Same as span (StdList), but preserving uniqueness
uspan c [x:xs]
	# (keep,x) = c x
	| keep
		= ([x:ys],zs)
	with
		(ys,zs) = uspan c xs
	| otherwise
		= ([],[x:xs])
uspan _ _
	= ([],[])

filterMap :: !(.x -> *(Bool,.y)) ![.x] -> [.y]
filterMap f [x:xs]
	#! (keep,y)	= f x
	#! ys		= filterMap f xs
	| keep		= [y:ys]
	| otherwise	= ys
filterMap _ _
	= []

stateMap :: !(u:x -> v:(.s -> (.y,.s))) ![u:x] !.s -> (![.y],!.s), [v<=u]
stateMap f [x:xs] s
	#! (y, s)	= f x s
	#! (ys,s)	= stateMap f xs s
	= ([y:ys],s)
stateMap _ _ s
	= ([],s)

stateMap2 :: !(u:x -> .(.s -> .s)) !v:[u:x] !.s -> .s, [v <= u]
stateMap2 f [x:xs] s
	= stateMap2 f xs (f x s)
stateMap2 _ _ s
	= s

strictSeq :: ![.(.s -> .s)] !.s	-> .s		// Same as seq (StdFunc), but with strict state argument
strictSeq [f:fs] s
	= strictSeq fs (f s)
strictSeq _ s
	= s

strictSeqList :: !.[.St .s .x] !.s -> (![.x],!.s)	// Same as seqList (StdFunc), but with strict state argument
strictSeqList [f:fs] s
	# (x, s) = f s
	# (xs,s) = strictSeqList fs s
	= ([x:xs],s)
strictSeqList _ s
	= ([],s)

allList :: !(.x .s -> .(Bool,.s)) ![.x] !.s -> (!Bool,!.s)
allList cond [x:xs] s
	# (ok,s)	= cond x s
	| ok		= allList cond xs s
	| otherwise	= (False,s)
allList _ _ s
	= (True,s)

contains :: !(Cond x) ![x] -> Bool
contains c [x:xs]	= c x || contains c xs
contains _ _		= False

ucontains :: !(UCond .x) !u:[.x] -> (!Bool,!u:[.x])
ucontains c [x:xs]
	# (cond,x) = c x
	| cond
		= (True,[x:xs])
	| otherwise
		# (b,xs) = ucontains c xs
		= (b,[x:xs])
ucontains _ []
	= (False,[])

cselect :: !(Cond x) x ![x] -> (!Bool, x)
cselect c n [x:xs]
	| c x		= (True,x)
	| otherwise	= cselect c n xs
cselect _ n _	= (False,n)

ucselect :: !(Cond x) x !u:[ x] -> (!Bool, x,!u:[x])
ucselect c n [x:xs]
	| c x
		= (True,x,[x:xs])
	| otherwise
		# (found,y,xs)	= ucselect c n xs
		= (found,y,[x:xs])
ucselect _ n []
	= (False,n,[])

selectedAtIndex :: !(Cond x) x ![x] -> (!Index, x)
selectedAtIndex cond dummy xs
	= (if found i 0,x)
where
	(found,i,x) = selected cond dummy xs 1
	
	selected :: (Cond x) x ![x] !Int -> (!Bool,!Int,x)
	selected cond dummy [x:xs] i
		| cond x	= (True,i,x)
		| otherwise	= selected cond dummy xs (i+1)
	selected _ dummy _ i
		= (False,i,dummy)

access :: !(St .x *(Bool,.y)) .y !u:[.x] -> (!Bool,.y,!u:[.x])
access acc n [x:xs]
	# ((cond,y),x) = acc x
	| cond
		= (True,y,[x:xs])
	| otherwise
		# (b,y,xs) = access acc n xs
		= (b,y,[x:xs])
access _ n []
	= (False,n,[])

accessList :: !(St .x .y) ![.x] -> (![.y],![.x])
accessList acc [x:xs]
	#! (y, x)	= acc x
	#! (ys,xs)	= accessList acc xs
	=  ([y:ys],[x:xs])
accessList _ []
	= ([],[])

remove :: !(Cond x) x !u:[x] -> (!Bool,x,!u:[x])
remove c n [x:xs]
	| c x
		= (True,x,xs)
	| otherwise
		# (b,y,xs) = remove c n xs
		= (b,y,[x:xs])
remove _ n _
	= (False,n,[])

uremove :: !(UCond .x) .x !u:[.x] -> (!Bool,.x,!u:[.x])
uremove c n [x:xs]
	# (cond,x)	= c x
	| cond
		= (True,x,xs)
	| otherwise
		# (b,y,xs)	= uremove c n xs
		= (b,y,[x:xs])
uremove _ n []
	= (False,n,[])

creplace :: !(Cond x) x ![x] -> (!Bool,![x])
creplace c y [x:xs]
	| c x
		= (True,[y:xs])
	| otherwise
		# (b,xs)	= creplace c y xs
		= (b,[x:xs])
creplace _ _ _
	= (False,[])

ucreplace :: !(UCond .x) .x !u:[.x] -> (!Bool,!u:[.x])
ucreplace c y [x:xs]
	# (cond,x)= c x
	| cond
		= (True,[y:xs])
	| otherwise
		# (b,xs)	= ucreplace c y xs
		= (b,[x:xs])
ucreplace _ _ []
	= (False,[])

replaceOrAppend :: !(Cond x) x ![x] -> [x]
replaceOrAppend c y [x:xs]
	| c x
		= [y:xs]
	| otherwise
		= [x:replaceOrAppend c y xs]
replaceOrAppend _ y _
	= [y]

ureplaceOrAppend :: !(UCond .x) .x !u:[.x] -> u:[.x]
ureplaceOrAppend c y [x:xs]
	# (cond,x)= c x
	| cond
		= [y:xs]
	| otherwise
		= [x:ureplaceOrAppend c y xs]
ureplaceOrAppend _ y _
	= [y]

ulength :: ![.x] -> (!Int,![.x])
ulength [x:xs]
	# (length,xs)= ulength xs
	= (length+1,[x:xs])
ulength _
	= (0,[])

removeCheck :: x !u:[x] -> (!Bool, !u:[x])	| Eq x
removeCheck y [x:xs]
	| y==x
		= (True,xs)
	| otherwise
		# (b,xs)	= removeCheck y xs
		= (b,[x:xs])
removeCheck _ _
	= (False,[])

removeSpecialChars :: ![Char] !{#Char} -> {#Char}
removeSpecialChars sc string
	= {c\\c<-removeSpecialChars` sc [c\\c<-:string]}
where
	removeSpecialChars` :: ![Char] ![Char] -> [Char]
	removeSpecialChars` sc [c1:cs1=:[c2:cs2]]
		| isMember c1 sc	= [c2:removeSpecialChars` sc cs2]
		| otherwise			= [c1:removeSpecialChars` sc cs1]
	removeSpecialChars` sc [c]
		| isMember c sc		= []
		| otherwise			= [c]
	removeSpecialChars` _ _
		= []

disjointLists :: ![x] ![x] -> Bool	| Eq x
disjointLists xs ys
	| isEmpty xs || isEmpty ys	= True
	| shorter xs ys				= disjointLists` xs ys
	| otherwise					= disjointLists` ys xs
where
	shorter :: ![x] ![x] -> Bool
	shorter [] _				= True
	shorter [x:xs] [y:ys]		= shorter xs ys
	shorter _ _					= False
	
	disjointLists` :: ![x] ![x] -> Bool	| Eq x
	disjointLists` [x:xs] ys	= not (isMember x ys) && disjointLists` xs ys
	disjointLists` _ _			= True

noDuplicates :: ![x] -> Bool	| Eq x
noDuplicates [x:xs]	= not (isMember x xs) && noDuplicates xs
noDuplicates _		= True

unzip3 :: ![(.a,.b,.c)] -> (![.a],![.b],![.c])
unzip3 [(a,b,c):abcs]
	#! (as,bs,cs) = unzip3 abcs
	= ([a:as],[b:bs],[c:cs])
unzip3 []
	= ([],[],[])

unzip4 :: ![(.a,.b,.c,.d)]	-> (![.a],![.b],![.c],![.d])
unzip4 [(a,b,c,d):abcds]
	#! (as,bs,cs,ds) = unzip4 abcds
	= ([a:as],[b:bs],[c:cs],[d:ds])
unzip4 []
	= ([],[],[],[])
