implementation module layout


import	StdBool, StdInt, StdList, StdMisc, StdTuple
import	commondef, id
from	windowhandle	import :: LayoutInfo(LayoutFix,LayoutFun,LayoutFrame), :: Origin


layoutError :: String String -> .x
layoutError rule message
	= error rule "layout" message

layoutFatalError :: String String -> .x
layoutFatalError rule message
	= fatalError rule "layout" message


//	The data types used for calculating the layout:
::	LayoutItem
	=	{	liId		:: !Id					// The Id      of the item
		,	liItemPos	:: !ItemPos				// The ItemPos of the item
		,	liItemSize	:: !Size				// The Size    of the item
		}
::	Root
	=	{	rootItem	:: !LayoutItem			// The original item that has been laid out
		,	rootPos		:: !Vector2				// The exact location of the item relative to current origin
		,	rootTree	:: ![Relative]			// The dependent items
		}
::	Relative
	=	{	relativeItem:: !LayoutItem			// The original item that has been laid out
		,	relativePos	:: !Vector2				// The exact location of the item relative to current origin
		}

//	Basic operations on LayoutItems, Roots, and Relatives:
identifyLayoutItem :: !Id !LayoutItem -> Bool
identifyLayoutItem id {liId}
	= id==liId

identifyRoot :: !Id !Root -> Bool
identifyRoot id {rootItem}
	= identifyLayoutItem id rootItem

identifyRelative :: !Id !Relative -> Bool
identifyRelative id {relativeItem}
	= identifyLayoutItem id relativeItem

/*	removeRoot id removes that Root from the [Root] that either:
		* can be identified by id, or
		* contains a Relative that can be identified by id
*/
removeRoot :: !Id ![Root] -> (!Bool,Root,![Root])
removeRoot id [item:items]
	| identifyRoot id item
		= (True,item,items)
	| contains (identifyRelative id) item.rootTree
		= (True,item,items)
	| otherwise
		# (found,root,items)	= removeRoot id items
		= (found,root,[item:items])
removeRoot _ items
	= (False,undef,items)


/*	getLayoutItemPosSize id retrieves the position and size of:
		* the Root argument in case id identifies the root, or
		* the Relative that can be identified by id
*/
getLayoutItemPosSize :: !Id !Root -> (!Bool,!Vector2,!Size)
getLayoutItemPosSize id item
	| identifyRoot id item
		= (True,item.rootPos,item.rootItem.liItemSize)
	# (found,relative)	= cselect (identifyRelative id) undef item.rootTree
	| not found
		= (False,zero,zero)
	| otherwise
		= (True,relative.relativePos,relative.relativeItem.liItemSize)

/*	shiftRelative v shifts the position of the Relative argument by v.
*/
shiftRelative :: !Vector2 !Relative -> Relative
shiftRelative v item=:{relativePos}
	= {item & relativePos=relativePos+v}

/*	shiftRoot v shifts the position of the Root argument and its Relatives by v.
*/
shiftRoot :: !Vector2 !Root -> Root
shiftRoot offset item=:{rootPos,rootTree}
	= {item & rootPos=rootPos+offset,rootTree=map (shiftRelative offset) rootTree}

/*	getRootBoundingBox calculates the smallest enclosing rectangle of the Root
	argument and its Relatives.
*/
getRootBoundingBox :: !Root -> OSRect
getRootBoundingBox item=:{rootPos={vx,vy},rootItem={liItemSize},rootTree}
	= getRelativeBoundingBox rootTree (posSizeToRect {x=vx,y=vy} liItemSize)
where
	getRelativeBoundingBox :: ![Relative] !OSRect -> OSRect
	getRelativeBoundingBox [item:items] boundBox
		= getRelativeBoundingBox items (mergeBoundingBox boundBox (posSizeToRect {x=vx,y=vy} item.relativeItem.liItemSize))
	where
		{vx,vy}	= item.relativePos
		
		mergeBoundingBox :: !OSRect !OSRect -> OSRect
		mergeBoundingBox {rleft=lR,rtop=tR,rright=rR,rbottom=bR} {rleft=lB,rtop=tB,rright=rB,rbottom=bB}
			= {rleft=min lR lB,rtop=min tR tB,rright=max rR rB,rbottom=max bR bB}
	getRelativeBoundingBox _ boundBox
		= boundBox

/*	getLayoutItem id roots
		retrieves the position (Vector2) and size (Size) of the item identified by id.
		In case the item is a relative, it is removed from the [Root].
		In case the item is a root, it is removed only if it has an empty layout tree.
		The LayoutInfo classifies the ItemPos of the layout root of the retrieved item.
	getLayoutItem returns a runtime error in case no item could be identified.
*/
getLayoutItem :: !Id ![Root] -> (!LayoutInfo,!Vector2,!Size,![Root])
getLayoutItem id items=:[root:roots]
	| identifyRoot id root
		| isEmpty depends
			= (layoutInfo,corner,size,roots)
		// otherwise
			= (layoutInfo,corner,size,items)
	| inTree
		= (layoutInfo,rPos,rSize,[{root & rootTree=depends1}:roots])
	| otherwise
		# (layoutInfo,pos,size,roots)	= getLayoutItem id roots
		= (layoutInfo,pos,size,[root:roots])
where
	corner		= root.rootPos
	size		= root.rootItem.liItemSize
	depends		= root.rootTree
	(inTree,rPos,rSize,depends1)
				= getRelativeItem id depends
	layoutInfo	= case root.rootItem.liItemPos of
					(Fix,_)				-> LayoutFix
					(_,OffsetFun i f)	-> LayoutFun i f
					_					-> LayoutFrame
	
	getRelativeItem :: !Id ![Relative] -> (!Bool,!Vector2,!Size,![Relative])
	getRelativeItem id [item:items]
		| identifyRelative id item
			= (True,item.relativePos,item.relativeItem.liItemSize,items)
		| otherwise
			# (found,pos,size,items)	= getRelativeItem id items
			= (found,pos,size,[item:items])
	getRelativeItem _ items
		= (False,zero,zero,items)
getLayoutItem id _
	= layoutError "getLayoutItem" "Unknown Id"


/*	layoutItems is the actual layout algorithm.
	It calculates the precise position (in pixels) of each LayoutItem.
	The position is calculated from a zero origin.
	Assumptions:
	-	All LayoutItems have a layout element ItemPos.
	-	All relative references to previous elements have been identified (so LeftOfPrev --> LeftOf id and so on).
*/
layoutItems :: !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![LayoutItem] -> (!Size,![Root])
layoutItems hMargins=:(lMargin,rMargin) vMargins=:(tMargin,bMargin) itemSpaces reqSize minSize orientations layoutItems
	# reqSize		= if (reqSize<>zero) {w=reqSize.w-lMargin-rMargin,h=reqSize.h-tMargin-bMargin} reqSize
	# layoutItems	= sortLayoutItems layoutItems
	# (_,roots)		= stateMap2 (calcRootPosition itemSpaces orientations) layoutItems (0,[])
	  size			= calcAreaSize orientations roots reqSize minSize
	# roots			= stateMap2 (calcFramePosition hMargins vMargins orientations size) roots []
	  finalSize		= {w=lMargin+size.w+rMargin,h=tMargin+size.h+bMargin}
	= (finalSize,roots)


/*	sortLayoutItems sorts the list of item positions such that relatively laynout items 
	are placed immediately(!!) behind their target items. They are not placed in the rootTree
	of the target item. This is done by calcRootPosition. 
	sortLayoutItems failures:
	-	a cyclic dependency has been located: the Ids are printed and computation stops
	-	unknown references have been located: the Ids are printed and computation stops
*/
sortLayoutItems :: ![LayoutItem] -> [LayoutItem]
sortLayoutItems layoutItems
	# (lineItems,relItems)	= divide (\{liItemPos}->IsLine liItemPos) layoutItems
	# layoutItems			= lineItems++relItems
	# layoutItems			= sortLayoutItems` [] layoutItems
	= layoutItems
where
	sortLayoutItems` :: ![LayoutItem] ![LayoutItem] -> [LayoutItem]
	sortLayoutItems` done todo
		| isEmpty todo
			= reverse done
		# (item1,todo)		= hdtl todo
		  pos1				= item1.liItemPos
		  (isRelative,id2)	= IsRelative pos1
		| not isRelative
			= sortLayoutItems` [item1:done] todo
		| otherwise
			# (done,chain,todo)	= getItemPosChain id2 done [item1] todo
			= sortLayoutItems` (insertItemPosChain chain done) todo
	where
		getItemPosChain :: !Id ![LayoutItem] ![LayoutItem] ![LayoutItem] -> (![LayoutItem],![LayoutItem],![LayoutItem])
		getItemPosChain nextId done chain todo
			# in_chain				= contains (identifyLayoutItem nextId) chain
			| in_chain
				= layoutError "calculating layout" "cyclic dependency between Ids"
			# in_done				= contains (identifyLayoutItem nextId) done
			| in_done
				= (done,chain,todo)
			# (in_todo,next,todo)	= remove (identifyLayoutItem nextId) undef todo
			| not in_todo
				= layoutError "calculating layout" "reference to unknown Id"
			# nextPos				= next.liItemPos
			  (isRelative,id2)		= IsRelative nextPos
			| not isRelative
				= (done,[next:chain],todo)
			| otherwise
				= getItemPosChain id2 done [next:chain] todo
		
		insertItemPosChain :: ![LayoutItem] ![LayoutItem] -> [LayoutItem]
		insertItemPosChain chain=:[final:_] done
			| not isRelative
				= chain`++done
			| otherwise
				= insertchain id chain` done
		where
			(isRelative,id)	= IsRelative final.liItemPos
			chain`			= reverse chain
			
			insertchain :: !Id [LayoutItem] ![LayoutItem] -> [LayoutItem]
			insertchain id chain [item:items]
				| identifyLayoutItem id item
					= chain++[item:items]
				| otherwise
					= [item:insertchain id chain items]
			insertchain _ chain _
				= chain
		insertItemPosChain _ done				// this alternative will actually never be reached
			= done


/*	Calculate the positions of line oriented items and the space they occupy. 
	Place relatively placed items in the root tree of the item referred to.
	Items that are positioned at a fixed spot (Fix pos) are laid out relative to the given origin.
	Assumptions:
	-	All relative layout positions refer to existing elements which must occur in the done list.
	
	Note:	Renter = Right or Center,
			Corner = LeftTop, RightTop, LeftBottom or RightBottom
*/
calcRootPosition :: !(!Int,!Int) ![(ViewDomain,Origin)] !LayoutItem !(!Int,![Root]) -> (!Int,![Root])
calcRootPosition itemSpaces orientations item1 sDone=:(sizeY,done)
	| isFix					// {Fix}
		= (sizeY, [item1`:done])
		with
			(_,origin)		= hd orientations
			itemoffset		= case itemPosOffset fixpos orientations of
								Alt1Of2 v	= v
								otherwise	= layoutFatalError "calcRootPosition" "root item with ItemPos=Fix has illegal ItemOffset argument: OffsetAlign"
			pos				= itemoffset-toVector origin
			item1`			= {rootItem=item1,rootPos=pos,rootTree=[]}
	| isRelative && exists	// {LeftOf,RightTo,Above,Below}
		= (sizeY`,[item2`:done1])
		with
			(sizeY`,item2`)	= if (IsRelativeX pos1)
								 (calcXPosition itemSpaces orientations item1 id2 sizeY item2)
								 (calcYPosition itemSpaces orientations item1 id2 sizeY item2)
	| isRelative
		= layoutFatalError "calculating layout" "reference to unknown Id (not caught by sortLayoutItems)"
	| IsCorner pos1			// {LeftTop,RightTop,LeftBottom,RightBottom}
		= (sizeY, [item1`:done])
		with
			item1`			= {rootItem=item1,rootPos=zero,rootTree=[]}
	| otherwise				// {Left,Center,Right}
		= (max sizeY (sizeY+yOffset1+h), [{rootItem=item1,rootPos={zero & vy=sizeY+yOffset1},rootTree=[]}:done])
		with
			h				= item1.liItemSize.h
			itemoffset		= case itemPosOffset (snd pos1) orientations of
								Alt1Of2 v	= v
								otherwise	= layoutFatalError "calcRootPosition" "root item with ItemPos in {Left,Center,Right} has illegal ItemOffset argument: OffsetAlign"
			yOffset			= itemoffset.vy
			yOffset1		= if (sizeY==0) yOffset (snd itemSpaces+yOffset)
where
	pos1					= item1.liItemPos
	(isFix,fixpos)			= IsFix pos1
	(isRelative,id2)		= IsRelative pos1
	(exists,item2,done1)	= removeRoot id2 done
	
/*	calcXPosition calculates the position of item1 which is horizontally relative to the item identified by id2.
	This item is either item2 or occurs in the layout tree of item2.
	item1 is placed as a Relative in the layout tree of item2.
*/
	calcXPosition :: !(!Int,!Int) ![(ViewDomain,Origin)] !LayoutItem !Id !Int !Root -> (!Int,!Root)
	calcXPosition itemSpaces orientations item1 id2 sizeY item2=:{rootItem=root2,rootTree=tree2}
		| not ok
			= layoutFatalError "calcXPosition" "dependent item could not be found in rootTree"
		| otherwise
			= (	if (IsCorner pos2 || isFix2) sizeY (max (t+size1.h) sizeY)
			  ,	{item2 & rootTree=[depend:tree2]}
			  )
	where
		pos1				= item1.liItemPos;		size1		= item1.liItemSize;
		pos2				= root2.liItemPos;
		(isFix2,_)			= IsFix pos2
		l					= if (IsLeftOf pos1)
								(corner2.vx-size1.w-fst itemSpaces+if isVectorOffset v.vx zero)
								(corner2.vx+size2.w+fst itemSpaces+if isVectorOffset v.vx zero)
		t					= corner2.vy + dy
		dy					= if isVectorOffset							// determine vertical offset:
								v.vy									// take exact offset of vector
								(case align of
									AlignTop	= zero					// align top coordinates
									AlignCenter	= (size2.h-size1.h)/2	// center bodies of items
									AlignBottom	=  size2.h-size1.h		// align bottom coordinates
								)
		offset				= itemPosOffset (snd pos1) orientations
		isVectorOffset		= isAlt1Of2 offset;		v		= alt1Of2 offset
		isAlignOffset		= isAlt2Of2 offset;		align	= alt2Of2 offset;
		(ok,corner2,size2)	= getLayoutItemPosSize id2 item2
		depend				= {relativeItem=item1,relativePos={vx=l,vy=t}}
	
/*	calcYPosition calculates the position of item1 which is vertically relative to the item identified by id2.
	This item is either item2 or occurs in the layout tree of item2.
	item1 is placed as a Relative in the layout tree of item2.
*/
	calcYPosition :: !(!Int,!Int) ![(ViewDomain,Origin)] !LayoutItem !Id !Int !Root -> (!Int,!Root)
	calcYPosition itemSpaces orientations item1 id2 sizeY item2=:{rootItem=root2,rootTree=tree2}
		| not ok
			= layoutFatalError "calcXPosition" "dependent item could not be found in rootTree"
		| otherwise
			= (	if (IsCorner pos2 || isFix2) sizeY (max (t+size1.h) sizeY)
			  ,	{item2 & rootTree=[depend:tree2]}
			  )
	where
		pos1				= item1.liItemPos;		size1	= item1.liItemSize;
		pos2				= root2.liItemPos;
		(isFix2,_)			= IsFix pos2
		l					= corner2.vx + dx
		dx					= if isVectorOffset 						// determine horizontal offset:
								v.vx									// take exact offset of vector
								(case align of
									AlignLeft	= zero					// align left coordinates
									AlignCenter	= (size2.w-size1.w)/2	// align bodies of items
									AlignRight	=  size2.w-size1.w		// align right coordinates
								)
		t					= if (IsBelow pos1) 
								(corner2.vy+size2.h+snd itemSpaces+if isVectorOffset v.vy zero)
								(corner2.vy-size1.h-snd itemSpaces+if isVectorOffset v.vy zero)
		offset				= itemPosOffset (snd pos1) orientations
		isVectorOffset		= isAlt1Of2 offset;		v		= alt1Of2 offset;
		isAlignOffset		= isAlt2Of2 offset;		align	= alt2Of2 offset;
		(ok,corner2,size2)	= getLayoutItemPosSize id2 item2
		depend				= {relativeItem=item1,relativePos={vx=l,vy=t}}


/*	In case no requested size is given (requested size==zero), calculate the actual 
	width and height of the overall area. The overall area is the smallest enclosing 
	rectangle of the line and fix layout items, provided it fits the corner oriented items.
	In case of a requested size, yield this size.
*/
calcAreaSize :: ![(ViewDomain,Origin)] ![Root] !Size !Size -> Size
calcAreaSize orientations roots reqSize minimumSize
	| reqSize<>zero
		= stretchSize minimumSize reqSize
	| otherwise
		= stateMap2 (fitRootInArea origin orientations) roots minimumSize
where
	origin	= snd (hd orientations)
	
	stretchSize :: !Size !Size -> Size
	stretchSize	size1 size2
			= {w=max size1.w size2.w, h=max size1.h size2.h}
	
//	fitRootInArea stretches the Size argument such that the bounding box of the Root argument fits. 
	fitRootInArea :: !Origin ![(ViewDomain,Origin)] !Root !Size -> Size
	fitRootInArea origin orientations root frameSize
		= stretchSize frameSize {w=reqX,h=reqY}
	where
		corner			= root.rootPos
		size			= root.rootItem.liItemSize
		(loc,offset)	= root.rootItem.liItemPos
		v				= case itemPosOffset offset orientations of
							Alt1Of2 v	= v
							otherwise	= layoutFatalError "fitRootInArea" "root control has illegal ItemOffset argument"
		itemBoundBox	= getRootBoundingBox root
		(reqX,reqY)		= delimit loc itemBoundBox
		
		delimit :: !ItemLoc !OSRect -> (!Int,!Int)
		delimit Fix {rright,rbottom}
			| r`<=0 || b`<=0
				= (0,0)
			| otherwise
				= (r`,b`)
		where
			r`	= rright -origin.x
			b`	= rbottom-origin.y
		delimit LeftTop {rright,rbottom}
			= (rright-lefttop.vx,rbottom-lefttop.vy)
		where
			lefttop 	= corner-v
		delimit RightTop {rleft,rbottom}
			= (righttop.vx-rleft,rbottom-righttop.vy)
		where
			righttop	= corner+{zero & vx=size.w}-v
		delimit LeftBottom {rtop,rright}
			= (rright-leftbottom.vx,leftbottom.vy-rtop)
		where
			leftbottom	= corner+{zero & vy=size.h}-v
		delimit RightBottom {rleft,rtop}
			= (rightbottom.vx-rleft,rightbottom.vy-rtop)
		where
			rightbottom	= corner+toVector size-v
		delimit Left {rright,rbottom}
			= (rright-left,rbottom)
		where
			left		= corner.vx-v.vx
		delimit Center {rleft,rright,rbottom}
			= (rright-rleft,rbottom)
		delimit Right {rleft,rbottom}
			= (right-rleft,rbottom)
		where
			right		= corner.vx+size.w-v.vy


/*	calcFramePosition calculates the layout of all frame aligned items. In addition it adds the margin offsets
	to each item. 
*/
calcFramePosition :: !(!Int,Int) !(!Int,Int) ![(ViewDomain,Origin)] !Size !Root ![Root] -> [Root]
calcFramePosition hMargins=:(lMargin,_) vMargins=:(tMargin,_) orientations sizeArea=:{w=width,h=height} item done
	| IsRenter pos || IsCorner pos
		= [item`:done]
		with
			sizeItem	= item.rootItem.liItemSize
			widthLeft	= width-sizeItem.w
			v			= if (IsCorner pos)
							 (cornerShift  orientations pos sizeItem sizeArea)
							 {vx=lineShift orientations pos widthLeft,vy=0}
			shift		= {vx=v.vx+lMargin,vy=v.vy+tMargin}
			item`		= shiftRoot shift item
	| otherwise
		= [shiftRoot {vx=lMargin,vy=tMargin} item:done]
where
	pos	= item.rootItem.liItemPos
	
/*	lineShift determines the offset of centered and right-oriented elements.
	Note that this function assumes that ItemOffset in ItemPos is not OffsetAlign.
*/
	lineShift :: [(ViewDomain,Origin)] !ItemPos !Int -> Int
	lineShift orientations (Center,offset) space
		= case itemPosOffset offset orientations of
			Alt1Of2 {vx} = space/2+vx
	lineShift orientations (_,offset) space
		= case itemPosOffset offset orientations of
			Alt1Of2 {vx} = space+vx

/*	cornerShift determines the offset vector of the ItemPos argument.
	Note that this function assumes that ItemOffset in ItemPos is not OffsetAlign.
*/
	cornerShift :: [(ViewDomain,Origin)] !ItemPos !Size !Size -> Vector2
	cornerShift orientations (LeftTop,offset) _ _
		= case itemPosOffset offset orientations of
			Alt1Of2 v	= v
			otherwise	= layoutFatalError "cornerShift _ (LeftTop,_) _ _" "ItemOffset argument is an Alignment instead of a Vector2"
	cornerShift orientations (RightTop,offset) {w=wItem} {w}
		= case itemPosOffset offset orientations of
			Alt1Of2	v	= {v & vx=w-wItem+v.vx}
			otherwise	= layoutFatalError "cornerShift _ (RightTop,_) _ _" "ItemOffset argument is an Alignment instead of a Vector2"
	cornerShift orientations (LeftBottom,offset) {h=hItem} {h}
		= case itemPosOffset offset orientations of
			Alt1Of2 v	= {v & vy=h-hItem+v.vy}
			otherwise	= layoutFatalError "cornerShift _ (LeftBottom,_) _ _" "ItemOffset argument is an Alignment instead of a Vector2"
	cornerShift orientations (RightBottom,offset) {w=wItem,h=hItem} {w,h}
		= case itemPosOffset offset orientations of
			Alt1Of2 v	= {vx=w-wItem+v.vx,vy=h-hItem+v.vy}
			otherwise	= layoutFatalError "cornerShift _ (RightBottom,_) _ _" "ItemOffset argument is an Alignment instead of a Vector2"


//	itemPosOffset calculates the actual offset vector of the given ItemOffset value.

itemPosOffset :: !ItemOffset [(ViewDomain,Origin)] -> Alt2 Vector2 Alignment
itemPosOffset NoOffset _
	= Alt1Of2 zero
itemPosOffset (OffsetVector v) _
	= Alt1Of2 v
itemPosOffset (OffsetAlign a) _
	= Alt2Of2 a
itemPosOffset (OffsetFun i f) orientations
	| isBetween i 1 (length orientations)
		= Alt1Of2 (f (orientations!!(i-1)))
	| otherwise
		= layoutError "calculating OffsetFun" ("illegal ParentIndex value: "+++toString i)


//	ItemPos predicates.

IsFix :: !ItemPos -> (!Bool,!ItemOffset)
IsFix (Fix,offset)			= (True, offset)
IsFix _						= (False,NoOffset)

IsLine :: !ItemPos -> Bool
IsLine (Left,_)				= True
IsLine (Center,_)			= True
IsLine (Right,_)			= True
IsLine _					= False

IsRelative :: !ItemPos -> (!Bool,!Id)
IsRelative (LeftOf	id,_)	= (True,id)
IsRelative (RightTo	id,_)	= (True,id)
IsRelative (Above	id,_)	= (True,id)
IsRelative (Below	id,_)	= (True,id)
IsRelative _				= (False,sysId 0)

IsRelativeX :: !ItemPos		-> Bool
IsRelativeX (LeftOf	 _,_)	= True
IsRelativeX (RightTo _,_)	= True
IsRelativeX _				= False

IsRenter :: !ItemPos		-> Bool
IsRenter (Center,_)			= True
IsRenter (Right, _)			= True
IsRenter _					= False

IsCorner :: !ItemPos		-> Bool
IsCorner (LeftTop,    _)	= True
IsCorner (RightTop,	  _)	= True
IsCorner (LeftBottom, _)	= True
IsCorner (RightBottom,_)	= True
IsCorner _					= False

IsLeftOf :: !ItemPos		-> Bool
IsLeftOf (LeftOf _,_)		= True
IsLeftOf _					= False

IsBelow  :: !ItemPos		-> Bool
IsBelow  (Below _,_)		= True
IsBelow  _					= False


//	Auxiliary functions:

divide :: !(Cond x) ![x] -> (![x],![x])			// divide cond xs = (filter cond xs,filter (not o cond) xs)
divide f [x:xs]
	| f x		= ([x:yes],no)
	| otherwise	= (yes,[x:no])
where
	(yes,no)	= divide f xs
divide _ _
	= ([],[])
