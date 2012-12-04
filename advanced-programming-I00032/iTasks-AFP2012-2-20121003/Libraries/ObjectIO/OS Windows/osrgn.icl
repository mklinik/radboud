implementation module osrgn

import	StdBool, StdInt, StdList
from	ostypes import :: OSRect{..}
import	pictCCall_12, rgnCCall_12

::	OSRgnHandle
	:==	Int
::	OSPointH
	:==	Int

//	Region creation and disposal operations.
osnewrgn :: !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osnewrgn tb
	= winCreateRectRgn 0 0 1 1 tb

osnewrectrgn :: !OSRect !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osnewrectrgn {rleft,rtop,rright,rbottom} tb
	= winCreateRectRgn rleft rtop rright rbottom tb

osdisposergn :: !OSRgnHandle !*OSToolbox -> *OSToolbox
osdisposergn osrgn tb
	= winDeleteObject osrgn tb


//	Setting the shape of a Region.
osrectrgn :: !OSRect !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osrectrgn {rleft,rtop,rright,rbottom} osrgn tb
	= winSetRgnToRect rleft rtop rright rbottom osrgn tb

ospolyrgn :: !(!Int,!Int) ![(Int,Int)] !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
ospolyrgn base shape osrgn tb
	# (osrgn,tb)		= winCombineRgn osrgn osrgn osrgn RGN_DIFF tb
	| len==0
		= (osrgn,tb)
	| otherwise
		# (shapeH,tb)	= winAllocPolyShape len tb
		# tb			= setpolyshape shapeH 0 base shape tb
		# (prgn,tb)		= winCreatePolygonRgn shapeH len WINDING tb
		# (osrgn,tb)	= winCombineRgn osrgn prgn prgn RGN_COPY tb
		# tb			= winDeleteObject prgn tb
		# tb			= winFreePolyShape shapeH tb
		= (osrgn,tb)
where
	len					= length shape
	
	setpolyshape :: !OSPointH !Int !(!Int,!Int) ![(Int,Int)] !*OSToolbox -> *OSToolbox
	setpolyshape shapeH i (x,y) [(vx,vy):vs] tb
		# tb	= winSetPolyPoint i x y shapeH tb
		# tb	= setpolyshape shapeH (i+1) (x+vx,y+vy) vs tb
		= tb
	setpolyshape _ _ _ _ tb
		= tb


//	Combining the shapes of two Regions into a new Region.
ossectrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
ossectrgn rgn1 rgn2 tb
	# (rrgn,tb)	= winCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= winCombineRgn rrgn rgn1 rgn2 RGN_AND tb
	= (rrgn,tb)

osunionrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osunionrgn rgn1 rgn2 tb
	# (rrgn,tb)	= winCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= winCombineRgn rrgn rgn1 rgn2 RGN_OR tb
	= (rrgn,tb)

osdiffrgn :: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osdiffrgn rgn1 rgn2 tb
	# (rrgn,tb)	= winCreateRectRgn 0 0 1 1 tb
	# (rrgn,tb)	= winCombineRgn rrgn rgn1 rgn2 RGN_DIFF tb
	= (rrgn,tb)


//	Region property access functions.
osgetrgnbox	:: !OSRgnHandle !*OSToolbox -> (!Bool,!OSRect,!*OSToolbox)
osgetrgnbox rgn tb
	# (l,t, r,b, isRect,_,tb)	= winGetRgnBox rgn tb
	= (isRect,{rleft=l,rtop=t,rright=r,rbottom=b}, tb)

osisemptyrgn:: !OSRgnHandle !*OSToolbox -> (!Bool,!*OSToolbox)
osisemptyrgn rgn tb
	# (_,_,_,_,_,isempty,tb)	= winGetRgnBox rgn tb
	= (isempty,tb)
