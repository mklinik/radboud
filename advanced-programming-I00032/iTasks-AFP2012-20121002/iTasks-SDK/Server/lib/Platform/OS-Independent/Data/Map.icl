implementation module Map

import StdEnv
import Maybe

//Create function
newMap :: w:(Map k u:v), [ w <= u]
newMap = MLeaf

//Insert function
put :: !k u:v !w:(Map k u:v) -> x:(Map k u:v) | Eq k & Ord k, [w x <= u, w <= x]
put k v MLeaf	= MNode MLeaf k 1 v MLeaf
put k v (MNode left nk h nv right) 
	| k == nk	= (MNode left k h v right)
	| k < nk	
		# left				= put k v left
		= update left nk nv right
	| otherwise
		# right				= put k v right
		= update left nk nv right
where
	update left nk nv right
		# (hleft,left)		= height left
		# (hright,right)	= height right
		# h					= (max hleft hright) + 1
		= balance (MNode left nk h nv right)

//Lookup function, non-unique version
get :: !k !(Map k v) -> Maybe v | Eq k & Ord k
get k MLeaf = Nothing
get k (MNode left nk _ nv right)
	| k == nk	= Just nv
	| k < nk	= get k left
				= get k right

//Lookup function, possibly spine unique version
getU :: !k !w:(Map k v) -> x:(Maybe v,!y:(Map k v)) | Eq k & Ord k, [ x <= y, w <= y]
getU k MLeaf = (Nothing, MLeaf)
getU k (MNode left nk h nv right)
	| k == nk	= (Just nv, MNode left nk h nv right)
	| k < nk
		# (mbv, left) = getU k left
		= (mbv, MNode left nk h nv right)
	| otherwise
		# (mbv, right) = getU k right
		= (mbv, MNode left nk h nv right)

//Delete function, only spine unique version
del :: !k !w:(Map k v) -> x:(Map k v) | Eq k & Ord k, [ w <= x]
del k mapping = snd (delU k mapping)

//Delete function
delU :: !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | Eq k & Ord k, [ w y <= u, x <= y, w <= y]
delU k MLeaf = (Nothing, MLeaf)							//Do nothing
delU k (MNode MLeaf nk h nv MLeaf)						//A node with just leaves as children can be safely removed
	| k == nk	= (Just nv, MLeaf)
				= (Nothing, MNode MLeaf nk h nv MLeaf)
delU k (MNode MLeaf nk h nv right)						//A node without smaller items
	| k == nk	= (Just nv, right)						//When found, just remove
	| k < nk	= (Nothing, MNode MLeaf nk h nv right)	//Do nothing, k is not in the mapping
	| otherwise
		# (mbv,right)		= delU k right
		# (hright,right)	= height right
		# h					= hright + 1
		= (mbv, balance (MNode MLeaf nk h nv right))

delU k (MNode left nk h nv MLeaf)						//A node without larger items
	| k == nk	= (Just nv, left)						//When found just remove
	| k < nk	
		# (mbv,left)		= delU k left
		# (hleft,left)		= height left
		# h					= hleft + 1
		= (mbv, balance (MNode left nk h nv MLeaf))
	| otherwise
				= (Nothing, MNode left nk h nv MLeaf)	//Do nothing, k is not in hte mapping

delU k (MNode left nk h nv right)						//A node with both larger and smaller items
	| k == nk	
		# (left,k,v)		= takeMax left
		# (h,left,right)	= parentHeight left right
		= (Just nv, balance (MNode left k h v right))	//Replace with the largest of the smaller items and rebalance
	| k < nk	
		# (mbv, left)		= delU k left
		# (h,left,right)	= parentHeight left right
		= (mbv, balance (MNode left nk h nv right))
	| otherwise
		# (mbv, right)		= delU k right
		# (h,left,right)	= parentHeight left right
		= (mbv, balance (MNode left nk h nv right))
where
	//Takes the k and v values from the maximum node in the tree and removes that node
	takeMax MLeaf = abort "takeMax of leaf evaluated" 
	takeMax (MNode left nk _ nv MLeaf)	= (left, nk, nv)
	takeMax (MNode left nk _ nv right)
					# (right,k,v)		= takeMax right
					# (hleft,left)		= height left
					# (hright,right)	= height right
					# h					= (max hleft hright) + 1
					= (balance (MNode left nk h nv right), k, v)

	//Determines the height of the parent node of two sub trees
	parentHeight left right
		# (hleft,left)		= height left
		# (hright,right)	= height right
		# h					= (max hleft hright) + 1
		= (h, left, right)

//Conversion functions
toList :: !w:(Map k u:v)	-> x:[y:(!k,u:v)] , [w y <= u, x <= y, w <= x]
toList m = toList` m []
where
	toList` MLeaf c = c
	toList` (MNode left k h v right) c = toList` left [(k,v): toList` right c]


fromList :: !w:[x:(!k,u:v)] -> y:(Map k u:v) | Eq k & Ord k, [x y <= u, w <= x, w <= y]
//fromList :: [(k,v)] -> (Map k v) | Eq k & Ord k
fromList [] = newMap
fromList [(k,v):xs] = put k v (fromList xs)

putList :: !w:[x:(!k,u:v)] !w:(Map k u:v) -> y:(Map k u:v) | Eq k & Ord k, [x y <= u, w <= x, w <= y]
putList [] map = map
putList [(k,v):xs] map = putList xs (put k v map)

delList :: ![k] !w:(Map k u:v) -> y:(Map k u:v) | Eq k & Ord k, [w y <= u, w <= y]
delList list map = seq [\map -> snd (delU key map) \\ key <- list] map
//Helper functions

//Determine the height of a tree
//This information is stored inside the tree to prevent complete traversals of the tree
height :: !u:(Map k w:v) -> x:(!Int, y:(Map k w:v)), [u y <= w, x <= y, u <= y]
height MLeaf				= (0,MLeaf)
height (MNode left k h v right)	= (h, MNode left k h v right)

//Balance a tree locally (E.g. not recursive. only inspect and rearrange the top of the tree)
balance :: !u:(Map k w:v) -> x:(Map k w:v), [u x <= w, u <= x] 
balance MLeaf = MLeaf
balance (MNode left k h v right)
	# (hleft,left)		= height left
	# (hright,right)	= height right
	# balanceFactor		= hright - hleft
	| balanceFactor < -1	
		# (ld, left)	= leftDeepest left
		| ld				= leftleftRotate (MNode left k h v right)	//Left-left rotate
							= leftrightRotate (MNode left k h v right)	//Left-right rotate
	| balanceFactor > 1		
		# (ld, right)	= leftDeepest right
		| ld				= rightleftRotate (MNode left k h v right)	//Right-left rotate
							= rightrightRotate (MNode left k h v right) //Right-right rotate
	| otherwise
							= MNode left k h v right 					//Already balanced
where
	leftDeepest MLeaf = (False,MLeaf)
	leftDeepest (MNode left k h v right)
		# (hleft,left)		= height left
		# (hright,right)	= height right
		= (hleft > hright,(MNode left k h v right))

	leftleftRotate (MNode (MNode (MNode d xk xh xv c) pk _ pv b ) rk _ rv a)
		# (ah,a) 	= height a
		# (bh,b) 	= height b
		# rh		= (max bh ah) + 1
		# ph		= (max xh rh) + 1
		= MNode (MNode d xk xh xv c) pk ph pv (MNode b rk rh rv a)
	leftleftRotate node = node

	leftrightRotate (MNode (MNode b rk _ rv (MNode c pk _ pv d)) xk _ xv a)
		# (bh,b)	= height b
		# (ch,c)	= height c
		# rh		= (max bh ch) + 1
		# (dh,d)	= height d
		# (ah,a)	= height a
		# xh		= (max dh ah) + 1
		# ph		= (max rh xh) + 1
		= MNode (MNode b rk rh rv c) pk ph pv (MNode d xk xh xv a)
	leftrightRotate node = node

	rightleftRotate (MNode a xk _ xv (MNode (MNode d pk _ pv c) rk _ rv b))
		# (ah,a)	= height a
		# (dh,d)	= height d	
		# xh		= (max ah dh) + 1
		# (ch,c)	= height c
		# (bh,b)	= height b
		# rh		= (max ch bh) + 1
		# ph		= (max xh rh) + 1
		= MNode (MNode a xk xh xv d) pk ph pv (MNode c rk rh rv b)
	rightleftRotate node = node

	rightrightRotate (MNode a rk _ rv (MNode b pk _ pv (MNode c xk xh xv d)))
		# (ah,a)	= height a
		# (bh,b)	= height b
		# rh		= (max ah bh) + 1
		# ph		= (max rh xh) + 1
		= MNode (MNode a rk rh rv b) pk ph pv (MNode c xk xh xv d)
	rightrightRotate node = node
