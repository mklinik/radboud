implementation module List

import StdTuple, StdBool, StdList, StdOrdList, Functor, GenEq

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 []				= ([], [], [])
unzip3 [(x,y,z) : xyzs]	= ([x : xs],[y : ys],[z : zs])
where
	(xs,ys,zs) = unzip3 xyzs

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 []					= ([], [], [], [])
unzip4 [(w,x,y,z) : wxyzs]	= ([w : ws],[x : xs],[y : ys],[z : zs])
where
	(ws,xs,ys,zs) = unzip4 wxyzs
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 []						= ([], [], [], [], [])
unzip5 [(v,w,x,y,z) : vwxyzs]	= ([v : vs],[w : ws],[x : xs],[y : ys],[z : zs])
where
	(vs,ws,xs,ys,zs) = unzip5 vwxyzs

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList cond new []         = [new]
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

splitWith :: !(a -> Bool) ![a] -> (![a],![a])
splitWith f [] = ([],[])
splitWith f [x:xs]
	| f x	= let (y,n) = splitWith f xs in ([x:y],n)
			= let (y,n)	= splitWith f xs in (y,[x:n])

sortByIndex :: ![(!Int,!a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

getItems :: ![a] ![Int] -> [a]
getItems list indexes = [x \\ x <- list & idx <- [0..] | isMember idx indexes]

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a
isMemberGen x [hd:tl]	= hd === x || isMemberGen x tl
isMemberGen x []		= False

instance Functor []
where
	fmap f l = [f e \\ e <- l]