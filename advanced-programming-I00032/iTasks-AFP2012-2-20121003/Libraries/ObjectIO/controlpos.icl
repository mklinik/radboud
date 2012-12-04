implementation module controlpos


import	StdBool, StdFunc, StdInt, StdList, StdMisc, StdTuple
import	commondef, controlrelayout, windowaccess
from	controllayout	import layoutControls
from	windowclipstate	import forceValidWindowClipState
from	windowdraw		import drawwindowlook`
from	windowupdate	import updatewindowbackgrounds
import	ospicture, osrgn, ostoolbox, ostypes, oswindow


controlposFatalError :: String String -> .x
controlposFatalError function error
	= fatalError function "controlpos" error

/*	movewindowviewframe moves the current view frame of the WindowHandle by the given Vector2. 
	movewindowviewframe assumes that the argument WindowHandle is a Window.
*/
movewindowviewframe :: !OSWindowMetrics !Vector2 !WIDS !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
movewindowviewframe wMetrics v wids=:{wPtr} wH=:{whWindowInfo,whItems=oldItems,whSize,whAtts,whSelect,whShow} tb
	| newOrigin==oldOrigin			// origin has not changed
		= (wH,tb)
	# (hasUpdate,tb)				= osWindowHasUpdateRect wPtr tb
	# tb							= setsliderthumb (hasHScroll && newOrigin.x<>oldOrigin.x) wMetrics wPtr True  (minx,newOrigin.x,maxx) vieww (toTuple whSize) tb
	# tb							= setsliderthumb (hasVScroll && newOrigin.y<>oldOrigin.y) wMetrics wPtr False (miny,newOrigin.y,maxy) viewh (toTuple whSize) tb
	# (noControls,oldItems)			= myisEmpty oldItems
	| noControls					// there are no controls: do only visual updates
		# windowInfo				= {windowInfo & windowOrigin=newOrigin}
		  wH						= {wH & whWindowInfo=WindowInfo windowInfo,whItems=oldItems}
		  (updArea,updAction)		= if (not lookInfo.lookSysUpdate || toMuch || hasUpdate)
		  								([newFrame],return []) (calcScrollUpdateArea oldOrigin newOrigin contentRect)
		  updState					= {oldFrame=posSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=updArea}
		# (wH,tb)					= drawwindowlook` wMetrics wPtr updAction updState wH tb
		= (wH,tb)
	| otherwise						// there are controls: recalculate layout and do visual updates
		# reqSize					= {w=contentSize.w-fst hMargins-snd hMargins,h=contentSize.h-fst vMargins-snd vMargins}
		# (oldItems`,oldItems,tb)	= getWElementHandles` wPtr oldItems tb
		# (_,newItems,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,newOrigin)] oldItems tb
		  windowInfo				= {windowInfo & windowOrigin=newOrigin}
		  wH						= {wH & whItems=newItems,whWindowInfo=WindowInfo windowInfo}
		# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
		  (whWindowInfo,wH)			= (\wH=:{whWindowInfo}->(whWindowInfo,wH)) wH
		# (isRect,areaRect,tb)		= case whWindowInfo of
		  								WindowInfo {windowClip={clipRgn}} -> osgetrgnbox clipRgn tb
		  								_                                 -> controlposFatalError "movewindowviewframe" "unexpected whWindowInfo field"
		# (updRgn,newItems,tb)		= relayoutControls wMetrics wPtr wH.whDefaultId False whSelect whShow (contentRect,zero,zero,oldItems`) 
									                                                                      (contentRect,zero,zero,wH.whItems) tb
		# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wids {wH & whItems=newItems} tb
		  (updArea,updAction)		= if (not lookInfo.lookSysUpdate || toMuch || not isRect || hasUpdate)
		  								([newFrame],return []) (calcScrollUpdateArea oldOrigin newOrigin areaRect)
		  updState					= {oldFrame=posSizeToRectangle oldOrigin contentSize,newFrame=newFrame,updArea=updArea}
		# (wH,tb)					= drawwindowlook` wMetrics wPtr updAction updState wH tb
		= (wH,tb)
where
	windowInfo						= getWindowInfoWindowData whWindowInfo
	(oldOrigin,domainRect,hasHScroll,hasVScroll,lookInfo)
									= (windowInfo.windowOrigin,windowInfo.windowDomain,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll,windowInfo.windowLook)
	hScroll							= if hasHScroll (Just (fromJust windowInfo.windowHScroll).scrollItemPtr) Nothing
	vScroll							= if hasVScroll (Just (fromJust windowInfo.windowVScroll).scrollItemPtr) Nothing
	domain							= rectToRectangle domainRect
	visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	contentRect						= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
	hRect							= osGetWindowHScrollRect wMetrics visScrolls (sizeToRect whSize)
	vRect							= osGetWindowVScrollRect wMetrics visScrolls (sizeToRect whSize)
	contentSize						= rectSize contentRect
	{w=w`,h=h`}						= contentSize
	(minx,maxx,vieww)				= (domainRect.rleft,domainRect.rright, contentSize.w)
	(miny,maxy,viewh)				= (domainRect.rtop, domainRect.rbottom,contentSize.h)
	newOrigin						= {	x = setBetween (oldOrigin.x+v.vx) minx (max minx (maxx-vieww))
									  ,	y = setBetween (oldOrigin.y+v.vy) miny (max miny (maxy-viewh))
									  }
	newFrame						= posSizeToRectangle newOrigin contentSize
	toMuch							= (abs (newOrigin.x-oldOrigin.x)>=w`) || (abs (newOrigin.y-oldOrigin.y)>=h`)
	(defMinW,defMinH)				= osMinWindowSize
	minSize							= {w=defMinW,h=defMinH}
	hMargins						= getWindowHMargins   IsWindow wMetrics whAtts
	vMargins						= getWindowVMargins   IsWindow wMetrics whAtts
	spaces							= getWindowItemSpaces IsWindow wMetrics whAtts
	
	setsliderthumb :: !Bool !OSWindowMetrics !OSWindowPtr !Bool !(!Int,!Int,!Int) !Int !(!Int,!Int) !*OSToolbox -> *OSToolbox
	setsliderthumb hasScroll wMetrics wPtr isHScroll scrollValues viewSize maxcoords tb
		| hasScroll					= osSetWindowSliderThumb wMetrics wPtr isHScroll osThumb hScroll vScroll hRect vRect maxcoords True tb
		| otherwise					= tb
	where
		(_,osThumb,_,_)				= toOSscrollbarRange scrollValues viewSize
	
/*	calcScrollUpdateArea p1 p2 area calculates the new update area that has to be updated. 
	Assumptions: p1 is the origin before scrolling,
	             p2 is the origin after  scrolling,
	             area is the visible area of the window view frame.
*/
	calcScrollUpdateArea :: !Point2 !Point2 !OSRect -> (![Rectangle],!St *Picture [OSRect])
	calcScrollUpdateArea oldOrigin newOrigin areaRect
		= (map rectToRectangle updArea,scroll {newOriginAreaRect & rright=rright+1,rbottom=rbottom+1} restArea v)
	where
		newOriginAreaRect			= addVector (toVector newOrigin) areaRect
		{rleft,rtop,rright,rbottom}	= newOriginAreaRect
		v							= toVector (oldOrigin-newOrigin)
		{vx,vy}						= v
		(updArea,restArea)			= if (vx<0 && vy<0)
										(	[{newOriginAreaRect & rleft=rright+vx,rbottom=rbottom+vy}
											,{newOriginAreaRect & rtop=rbottom+vy}
											]
										,	 {newOriginAreaRect & rright=rright+vx,rbottom=rbottom+vy}
										)
									 (if (vx<0 && vy>0)
									 	(	[{newOriginAreaRect & rleft=rright+vx,rtop=rtop+vy}
									 		,{newOriginAreaRect & rbottom=rtop+vy}
									 		]
									 	,	 {newOriginAreaRect & rtop=rtop+vy,rright=rright+vx}
									 	)
									 (if (vx<0)
									 	(	[{newOriginAreaRect & rleft=rright+vx}]
									 	,	 {newOriginAreaRect & rright=rright+vx}
									 	)
									 (if (vx>0 && vy<0)
									 	(	[{newOriginAreaRect & rright=rleft+vx,rbottom=rbottom+vy}
									 		,{newOriginAreaRect & rtop=rbottom+vy}
									 		]
									 	,	 {newOriginAreaRect & rleft=rleft+vx,rbottom=rbottom+vy}
									 	)
									 (if (vx>0 && vy>0)
									 	(	[{newOriginAreaRect & rright=rleft+vx,rtop=rtop+vy}
									 		,{newOriginAreaRect & rbottom=rtop+vy}
									 		]
									 	,	 {newOriginAreaRect & rleft=rleft+vx,rtop=rtop+vy}
									 	)
									 (if (vx>0)
									 	(	[{newOriginAreaRect & rright=rleft+vx}]
									 	,	 {newOriginAreaRect & rleft=rleft+vx}
									 	)
									 (if (vy<0)
									 	(	[{newOriginAreaRect & rtop=rbottom+vy}]
									 	,	{newOriginAreaRect & rbottom = rbottom+vy}
									 	)
									 (if (vy>0)
									 	(	[{newOriginAreaRect & rbottom=rtop+vy}]
									 	,	{newOriginAreaRect & rtop = rtop+vy}
									 	)
									 	(	[zero],newOriginAreaRect)
									 )))))))
		
		scroll :: !OSRect !OSRect !Vector2 !*Picture -> (![OSRect],!*Picture)
		scroll scrollRect restRect v picture
			# (updRect,picture)	= pictscroll2 scrollRect v picture
			| updRect==zero
				= ([],picture)
			| otherwise
				= ([restRect],picture)


/*	myisEmpty checks for lack of real controls, in contrast with isEmpty due to existence of W(List/Extend/Change)LSHandles. 
*/
myisEmpty :: ![WElementHandle .a .b] -> (!Bool,![WElementHandle .a .b])
myisEmpty [] = (True,[])
myisEmpty [wH: wHs]
	# (isEmpty, wH)		= myisEmptyItem wH
	| isEmpty
		# (isEmpty, wHs)	= myisEmpty wHs
		= (isEmpty, [wH: wHs])
	= (isEmpty, [wH: wHs])
where
	myisEmptyItem :: !(WElementHandle .ls .pst) -> (!Bool,!WElementHandle .ls .pst)
	myisEmptyItem (WItemHandle wiH)		= (False,WItemHandle wiH)
	myisEmptyItem (WListLSHandle wHs)
		# (isEmpty,wHs)			= myisEmpty wHs
		= (isEmpty, WListLSHandle wHs)
	myisEmptyItem (WExtendLSHandle {wExtendLS,wExtendItems})
		#   (isEmpty,wHs)			= myisEmpty wExtendItems
		= (isEmpty, WExtendLSHandle {wExtendLS=wExtendLS,wExtendItems=wHs})
	myisEmptyItem (WChangeLSHandle {wChangeLS,wChangeItems})
		#! (isEmpty,wHs)			= myisEmpty wChangeItems
		= (isEmpty, WChangeLSHandle {wChangeLS = wChangeLS, wChangeItems = wHs})
