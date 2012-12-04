implementation module windowvalidate


import	StdBool, StdList, StdFunc, StdTuple, StdMisc
import	osdocumentinterface, ospicture, ossystem, ostypes, oswindow
import	commondef, controllayout, keyfocus, StdControlAttribute, StdId, StdWindowAttribute, windowaccess
from	controlvalidate	import validateItemPos
from	StdSystem		import maxScrollWindowSize
from	iostate			import :: IOSt, ioStGetIdTable, ioStSetIdTable
from	layout			import itemPosOffset


windowvalidateError :: String String -> .x
windowvalidateError function message
	= error function "windowvalidate" message

windowvalidateFatalError :: String String -> .x
windowvalidateFatalError function message
	= fatalError function "windowvalidate" message


/*	validateWindowId checks whether the Id of the window/dialogue has already been bound.
	If so, Nothing is returned; otherwise a proper Id value for the window/dialogue is returned.
	The Id is not bound.
*/
validateWindowId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
validateWindowId Nothing ioState
	# (wId,ioState)				= openId ioState
	= (Just wId,ioState)
validateWindowId (Just id) ioState
	# (idtable,ioState)			= ioStGetIdTable ioState
	# (member,idtable)			= memberIdTable id idtable
	| member					= (Nothing,ioStSetIdTable idtable ioState)
	| otherwise					= (Just id,ioStSetIdTable idtable ioState)


/*	Validate the given window.
*/
validateWindow :: !OSWindowMetrics !OSDInfo !(WindowHandle .ls .pst) !(WindowHandles .pst) !*OSToolbox
		   -> (!Index,!Point2,!Size,!Vector2,!WindowHandle .ls .pst,  !WindowHandles .pst, !*OSToolbox)

validateWindow wMetrics osdInfo wH=:{whMode=mode,whKind=IsDialog,whItemNrs,whItems,whAtts} windows tb
	# atts						= filter isValidDialogAttribute whAtts
	  (index,atts,windows)		= validateWindowIndex mode				atts windows
	  (pos,  atts,windows)		= validateWindowPos   mode				atts windows
	  sizeAtt					= attrSize								atts			// Retrieve Window(View/Outer)Size (identical for Dialogs)
	  (hMargins,vMargins)		= attrMargins         IsDialog wMetrics	atts
	  spaces					= getWindowItemSpaces IsDialog wMetrics	atts
	  (defid,whItems)			= getOkId								atts whItems
	  (canid,whItems)			= getCancelId							atts whItems
	  (atts, whItems)			= validateWindowInitActive				atts whItems
	  reqSize					= determineRequestedSize zero sizeAtt
	  (minWidth,minHeight)		= osMinWindowSize
	  minSize					= {w=minWidth,h=minHeight}
	  domain					= sizeToRectangle reqSize
	# (derSize,items,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,zero)] whItems tb
	  (itemNrs,items)			= genWElementItemNrs whItemNrs items
	  (focusItems,items)		= getWElementKeyFocusIds True items
	  derSize					= determineRequestedSize derSize sizeAtt
	  domain					= sizeToRectangle derSize
	# okSize					= exactWindowSize wMetrics domain derSize False False IsDialog
	# (okPos,windows,tb)		= exactWindowPos wMetrics osdInfo okSize pos IsDialog mode windows tb
	= (	index
	  ,	okPos
	  ,	okSize
	  , zero
	  ,	{	wH	&	whItemNrs	= itemNrs
				,	whKeyFocus	= newFocusItems focusItems
				,	whItems		= items
				,	whSelect	= True
				,	whAtts		= atts
				,	whDefaultId	= defid
				,	whCancelId	= canid
				,	whSize		= okSize
		}
	  ,	windows
	  ,	tb
	  )

validateWindow wMetrics osdInfo wH=:{whKind=IsWindow,whItemNrs,whItems,whAtts} windows tb
	# atts						= filter isValidWindowAttribute whAtts
	  mode						= Modeless
	  (domain,atts)				= validateWindowDomain					atts
	  (maybe_hScroll,atts)		= validateWindowHScroll					atts
	  (maybe_vScroll,atts)		= validateWindowVScroll					atts
	  (sysLook,look,atts)		= validateWindowLook					atts
	  (reqSize,atts,tb)			= validateWindowSize wMetrics domain isMDI True (isJust maybe_hScroll,isJust maybe_vScroll) atts tb
	  (index,atts,windows)		= validateWindowIndex mode				atts windows
	  (pos,  atts,windows)		= validateWindowPos   mode				atts windows
	  (penAtts,atts)			= attrPen								atts
	  (hMargins,vMargins)		= attrMargins         IsWindow wMetrics	atts
	  spaces					= getWindowItemSpaces IsWindow wMetrics	atts
	  isAble					= attrSelectState						atts
	  (defid,whItems)			= getOkId								atts whItems
	  (canid,whItems)			= getCancelId							atts whItems
	  (atts, whItems)			= validateWindowInitActive				atts whItems
	  pen						= stateMap2 setPenAttribute (reverse penAtts) defaultPen
	# (derSize,items,tb)		= layoutControls wMetrics hMargins vMargins spaces reqSize minSize [(domain,domain.corner1)] whItems tb
	  (itemNrs,items)			= genWElementItemNrs whItemNrs items
	  (focusItems,items)		= getWElementKeyFocusIds True items
	  (origin,atts)				= validateOrigin derSize domain atts
	# okSize					= exactWindowSize wMetrics domain derSize (isJust maybe_hScroll) (isJust maybe_vScroll) IsWindow
	# (okPos,windows,tb)		= exactWindowPos wMetrics osdInfo okSize pos IsWindow mode windows tb
	  (hScroll,vScroll)			= validScrollInfos wMetrics okSize maybe_hScroll maybe_vScroll
	= (	index
	  ,	okPos
	  ,	okSize
	  , toVector (origin-domain.corner1)
	  ,	{	wH	&	whItemNrs	= itemNrs
				,	whKeyFocus	= newFocusItems focusItems
				,	whWindowInfo= WindowInfo
									{	windowDomain	= rectangleToRect domain
									,	windowOrigin	= domain.corner1
									,	windowHScroll	= hScroll
									,	windowVScroll	= vScroll
									,	windowLook		= {lookFun=look,lookPen=pen,lookSysUpdate=sysLook}
									,	windowClip		= {clipRgn=0,clipOk=False}
									}
				,	whItems		= items
				,	whSelect	= isAble
				,	whAtts		= atts
				,	whDefaultId	= defid
				,	whCancelId	= canid
				,	whSize		= okSize
		}
	  ,	windows
	  ,	tb
	  )
where
	minSize			= fromTuple osMinWindowSize
	isMDI			= getOSDInfoDocumentInterface osdInfo == MDI
	
	validScrollInfos :: OSWindowMetrics !Size !(Maybe ScrollFunction) !(Maybe ScrollFunction) -> (!Maybe ScrollInfo,!Maybe ScrollInfo)
	validScrollInfos wMetrics wSize maybe_hScroll maybe_vScroll
		= (mapMaybe (scrollInfo hScrollRect) maybe_hScroll,mapMaybe (scrollInfo vScrollRect) maybe_vScroll)
	where
		windowRect	= sizeToRect wSize
		hasScrolls	= (isJust maybe_hScroll,isJust maybe_vScroll)
		hScrollRect	= osGetWindowHScrollRect wMetrics hasScrolls windowRect
		vScrollRect	= osGetWindowVScrollRect wMetrics hasScrolls windowRect
		
		scrollInfo :: OSRect !ScrollFunction -> ScrollInfo
		scrollInfo r=:{rleft,rtop} scrollFun
			= {	scrollFunction	= scrollFun
			  ,	scrollItemPos	= {x=rleft,y=rtop}
  			  ,	scrollItemSize	= rectSize r
			  ,	scrollItemPtr	= OSNoWindowPtr
			  }

validateWindow wMetrics osdInfo wH=:{whKind=IsGameWindow,whWindowInfo} windows tb
    = (0,zero,okSize,zero,{wH & whSize=okSize},windows,tb)
where
	okSize	= (getWindowInfoGameWindowData whWindowInfo).gamewindowSize


determineRequestedSize :: Size !(Maybe Size) -> Size
determineRequestedSize size Nothing	= size
determineRequestedSize _ (Just size)= size


/*	validateWindowIndex validates the WindowIndex attribute. 
	The return Index is the validated Index. 
	The return WindowAttribute list does not contain a WindowIndex attribute.
*/
validateWindowIndex :: !WindowMode ![WindowAttribute *(.ls,.pst)] !(WindowHandles .pst)
						-> (!Index,![WindowAttribute *(.ls,.pst)], !WindowHandles .pst)
validateWindowIndex mode atts windows=:{whsWindows}
	= (okIndex,atts`,{windows & whsWindows=modal`++modeless`})
where
	(_,indexAtt,atts`)		= remove isWindowIndex (WindowIndex 0) atts
	index					= getWindowIndexAtt indexAtt
	(modal,modeless)		= uspan isModalWindow whsWindows
	(nrModals,modal`)		= ulength modal
	(nrModeless,modeless`)	= ulength modeless
	okIndex					= if (mode==Modal)
	  							 0													// Open modal windows frontmost
	  							 (setBetween index nrModals (nrModals+nrModeless))	// Open modeless windows behind the modal windows
	
	isModalWindow :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
	isModalWindow wsH
		# (mode,wsH)	= getWindowStateHandleWindowMode wsH
		= (mode==Modal,wsH)


/*	validateWindowPos validates the WindowPos attribute.
	If no WindowPos attribute is given then Nothing is returned.
	If the WindowPos is relative, it is verified that the window relates to an existing window.
	If this is not the case, then Nothing is returned.
	The resulting attribute list does not contain the WindowPos attribute anymore.
*/
validateWindowPos :: !WindowMode ![WindowAttribute *(.ls,.pst)] !(WindowHandles .pst)
			  -> (!Maybe ItemPos,![WindowAttribute *(.ls,.pst)], !WindowHandles .pst)
validateWindowPos mode atts windows
	| not hasPosAtt
		= (Nothing,atts`,windows)
	| not isRelative
		= (Just okItemPos,atts`,windows)
	| otherwise
		# (found,windows)	= hasWindowHandlesWindow (toWID relativeTo) windows
		= (if found (Just okItemPos) Nothing,atts`,windows)
where
	(hasPosAtt,posAtt,atts`)= remove isWindowPos undef atts
	itemPos					= getWindowPosAtt posAtt
	(isRelative,relativeTo)	= isRelativeItemPos itemPos
	okItemPos				= validateItemPos itemPos


/*	The result ({corner1=A,corner2=B},_) of validateWindowDomain is such that A<B (point A lies to 
	the left of and above point B). If either A.x==B.x or A.y==B.y then the ViewDomain is illegal and 
	the computation is aborted. 
	The default ViewDomain is maximal and positive, i.e.:
		{viewDomainRange & corner1=zero}.
*/
validateWindowDomain :: ![WindowAttribute .st] -> (!ViewDomain,![WindowAttribute .st])
validateWindowDomain atts
	# (hasDomain,domainAtt,atts)= remove isWindowViewDomain undef atts
	| not hasDomain
		= ({viewDomainRange & corner1=zero},atts)
	# domain					= getWindowViewDomainAtt domainAtt
	| isEmptyRectangle domain
		= windowvalidateError "validateWindowDomain" "Window has illegal ViewDomain argument"
	| otherwise
		= (validateViewDomain domain,atts)

validateViewDomain :: !ViewDomain -> ViewDomain
validateViewDomain domain
	= {corner1={x=setBetween dl rl rr,y=setBetween dt rt rb},corner2={x=setBetween dr rl rr,y=setBetween db rt rb}}
where
	{rleft=dl,rtop=dt,rright=dr,rbottom=db}	= rectangleToRect domain
	{rleft=rl,rtop=rt,rright=rr,rbottom=rb}	= rectangleToRect viewDomainRange


/*	validateWindowSize wMetrics viewDomain isMDI isResizable (hasHScroll,hasVScroll) atts
		takes care that the Window(View/Outer)Size attribute fits on the current screen.
		The Boolean  isMDI should be True iff the window belongs to a MDI process.
		The Boolean  isResizable should be True iff the window is resizable. 
		The Booleans hasHScroll hasVScroll should be True iff the window has the WindowHScroll, WindowVScroll
		attribute set respectively. 
		In addition, the WindowOuterSize attribute is mapped to WindowViewSize attribute.
*/
validateWindowSize :: !OSWindowMetrics !ViewDomain !Bool !Bool !(!Bool,!Bool) ![WindowAttribute .st] !*OSToolbox
																	-> (!Size,![WindowAttribute .st],!*OSToolbox)
validateWindowSize wMetrics domain isMDI isResizable hasScrolls atts tb
	| not hasSize
		= (pictSize,[WindowViewSize pictSize:atts],tb)
	with
		domainSize		= rectangleSize domain
		pictSize		= {w=min domainSize.w maxSize.w,h=min domainSize.h maxSize.h}
	| isWindowViewSize sizeAtt
		= (size1,snd (creplace isWindowViewSize (WindowViewSize size1) atts),tb)
	with
		size			= getWindowViewSizeAtt sizeAtt
		size1			= {w=setBetween size.w (fst minSize) maxSize.w,h=setBetween size.h (snd minSize) maxSize.h}
	| otherwise
		# ((dw,dh),tb)	= osStripOuterSize isMDI isResizable tb
		  (w,h)			= (outerSize.w-dw,outerSize.h-dh)
		  visScrolls	= osScrollbarsAreVisible wMetrics (rectangleToRect domain) (w,h) hasScrolls
		  viewSize		= rectSize (osGetWindowContentRect wMetrics visScrolls (sizeToRect {w=w,h=h}))
		# (_,_,atts)	= remove isWindowOuterSize undef atts
		# (_,_,atts)	= remove isWindowViewSize  undef atts
		= (viewSize,[WindowViewSize viewSize:atts],tb)
	with
		outerSize		= getWindowOuterSizeAtt sizeAtt
where
	(hasSize,sizeAtt)	= cselect (\att->isWindowViewSize att || isWindowOuterSize att) undef atts
	minSize				= osMinWindowSize
	maxSize				= maxScrollWindowSize


/*	validateOrigin takes care that the WindowOrigin attribute is a point in the rectangle
	formed by the left top of the (validated!) ViewDomain, and the width and height of the 
	(validated!) derived size.
*/
validateOrigin :: !Size !ViewDomain ![WindowAttribute .st] -> (!Point2,![WindowAttribute .st])
validateOrigin {w,h} domain=:{corner1={x=l,y=t},corner2={x=r,y=b}} atts
	# (_,domainAtt,atts)	= remove isWindowOrigin (WindowOrigin domain.corner1) atts
	  origin				= getWindowOriginAtt domainAtt
	= ({x=setBetween origin.x l (max l (r-w)),y=setBetween origin.y t (max t (b-h))},atts)


/*	validateWindow(H/V)Scroll removes the Window(H/V)Scroll attribute from the attribute list. 
*/
validateWindowHScroll :: ![WindowAttribute .st] -> (!Maybe ScrollFunction,![WindowAttribute .st])
validateWindowHScroll atts
	# (found,scrollAtt,atts)	= remove isWindowHScroll undef atts
	| found						= (Just (getWindowHScrollFun scrollAtt),atts)
	| otherwise					= (Nothing,atts)

validateWindowVScroll :: ![WindowAttribute .st] -> (!Maybe ScrollFunction,![WindowAttribute .st])
validateWindowVScroll atts
	# (found,scrollAtt,atts)	= remove isWindowVScroll undef atts
	| found						= (Just (getWindowVScrollFun scrollAtt),atts)
	| otherwise					= (Nothing,atts)


/*	validateWindowLook takes care that the optional WindowLook attribute is removed from the attribute list.
	If no attribute was present, then a default look is provided that paints the window with the background colour
	using standard window update mechanism.
*/
validateWindowLook :: ![WindowAttribute .st] -> (!Bool,!Look,![WindowAttribute .st])
validateWindowLook atts
	# (_,lookAtt,atts)	= remove isWindowLook (WindowLook True defaultlook) atts
	  (sysLook,lookFun)	= getWindowLookAtt lookAtt
	= (sysLook,lookFun,atts)
where
	defaultlook :: SelectState !UpdateState !*Picture -> *Picture
	defaultlook _ {updArea} picture = strictSeq (map unfill updArea) picture


//	Retrieve (View/Outer)Size, Margins, ItemSpaces, SelectState, and PenAttributes from the attribute list.

attrSize :: ![WindowAttribute .st] -> Maybe Size
attrSize atts
	| not hasSize			= Nothing
	| isWindowViewSize att	= Just (getWindowViewSizeAtt  att)
	| otherwise				= Just (getWindowOuterSizeAtt att)
where
	(hasSize,att)			= cselect (\att->isWindowViewSize att || isWindowOuterSize att) undef atts

attrMargins :: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!(!Int,!Int),!(!Int,!Int))
attrMargins wKind wMetrics atts
	= (getWindowHMargins wKind wMetrics atts,getWindowVMargins wKind wMetrics atts)

attrSelectState :: ![WindowAttribute .st] -> Bool
attrSelectState atts
	= enabled (getWindowSelectStateAtt (snd (cselect isWindowSelectState (WindowSelectState Able) atts)))

attrPen :: ![WindowAttribute .st] -> (![PenAttribute],![WindowAttribute .st])
attrPen atts
	# (_,penAtt,atts)	= remove isWindowPen (WindowPen []) atts
	= (getWindowPenAtt penAtt,atts)


/*	get(Ok/Cancel)Id select the Id of the Window(Ok/Cancel) attribute, and checks
	whether this Id corresponds with a (Custom)ButtonControl.
*/
getOkId :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst] -> (!Maybe Id,![WElementHandle .ls .pst])
getOkId atts itemHs
	| not hasid
		= (Nothing,itemHs)
	# (ok,itemHs)		= isOkOrCancelControlId id itemHs
	| ok
		= (Just id,itemHs)
	| otherwise
		= (Nothing,itemHs)
where
	(hasid,idAtt)		= cselect isWindowOk undef atts
	id					= getWindowOkAtt idAtt

getCancelId :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst] -> (!Maybe Id,![WElementHandle .ls .pst])
getCancelId atts itemHs
	| not hasid
		= (Nothing,itemHs)
	# (ok,itemHs)		= isOkOrCancelControlId id itemHs
	| ok
		= (Just id,itemHs)
	| otherwise
		= (Nothing,itemHs)
where
	(hasid,idAtt)		= cselect isWindowCancel undef atts
	id					= getWindowCancelAtt idAtt

isOkOrCancelControlId :: !Id ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
isOkOrCancelControlId id itemHs
	# (maybeKind,itemHs)	= getControlKind id itemHs
	| isNothing maybeKind
		= (False,itemHs)
	| otherwise
		# kind				= fromJust maybeKind
		= (kind==IsButtonControl || kind==IsCustomButtonControl,itemHs)


/*	validateWindowInitActive checks if the WindowInitActive attribute corresponds with an existing control.
	If this is not the case, the attribute is removed from the attribute list.
*/
validateWindowInitActive :: ![WindowAttribute *(.ls,.pst)] ![WElementHandle .ls .pst]
						-> (![WindowAttribute *(.ls,.pst)],![WElementHandle .ls .pst])
validateWindowInitActive atts itemHs
	| not hasAtt
		= (atts1,itemHs)
	# (kind,itemHs)		= getControlKind (getWindowInitActiveAtt att) itemHs
	| isNothing kind
		= (atts1,itemHs)
	| otherwise
		= (atts,itemHs)
where
	(hasAtt,att,atts1)	= remove isWindowInitActive undef atts

/*	getControlKind id itemHs
		returns (Just ControlKind) of the control in the item list. 
		If no such control could be found then Nothing is returned.
*/
getControlKind :: !Id ![WElementHandle .ls .pst] -> (!Maybe ControlKind,![WElementHandle .ls .pst])
getControlKind id [itemH:itemHs]
	# (maybe,itemH)	= getControlKind` id itemH
	| isJust maybe
		= (maybe,[itemH:itemHs])
	| otherwise
		# (maybe,itemHs)	= getControlKind id itemHs
		= (maybe,[itemH:itemHs])
where
	getControlKind` :: !Id !(WElementHandle .ls .pst) -> (!Maybe ControlKind,!WElementHandle .ls .pst)
	getControlKind` id (WItemHandle itemH=:{wItemId,wItemKind,wItems=itemHs})
		| isJust wItemId && fromJust wItemId==id
			= (Just wItemKind,WItemHandle itemH)
		| otherwise
			# (kind,itemHs)	= getControlKind id itemHs
			= (kind,WItemHandle {itemH & wItems=itemHs})
	getControlKind` id (WListLSHandle itemHs)
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WListLSHandle itemHs)
	getControlKind` id (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WExtendLSHandle {wExH & wExtendItems=itemHs})
	getControlKind` id (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (kind,itemHs)		= getControlKind id itemHs
		= (kind,WChangeLSHandle {wChH & wChangeItems=itemHs})
getControlKind _ _
	= (Nothing,[])


/*	exactWindowSize determines the exact size of a window.
	The size is extended to fit in sliderbars if requested (argument 4 and 5).
*/
exactWindowSize :: OSWindowMetrics ViewDomain !Size Bool Bool !WindowKind -> Size
exactWindowSize wMetrics domain wSize=:{w,h} hasHScroll hasVScroll wKind
	| wKind==IsDialog			= wSize
	| visHScroll && visVScroll	= {w=w`,h=h`}
	| visHScroll				= {wSize & h=h`}
	| visVScroll				= {wSize & w=w`}
	| otherwise					= wSize
where
	visHScroll					= hasHScroll && osScrollbarIsVisible (minmax domain.corner1.x domain.corner2.x) w
	visVScroll					= hasVScroll && osScrollbarIsVisible (minmax domain.corner1.y domain.corner2.y) h
	w`							= w+wMetrics.osmVSliderWidth
	h`							= h+wMetrics.osmHSliderHeight
// DvA: hmmm, gelijk aan oswindow:osGetWindowContentRect?


/*	exactWindowPos determines the exact position of a window.
	The size argument must be the exact size as calculated by exactWindowSize of the window.
	The ItemPos argument must be the validated(!) ItemPos attribute of the window.
*/
exactWindowPos :: !OSWindowMetrics !OSDInfo !Size !(Maybe ItemPos) !WindowKind !WindowMode !(WindowHandles .pst) !*OSToolbox
																	            -> (!Point2,!WindowHandles .pst, !*OSToolbox)
exactWindowPos wMetrics osdInfo exactSize=:{w,h} maybePos wKind wMode windows tb
	# exactSize = {w = w + osWindowFrameWidth + osWindowFrameWidth, h = h + osWindowFrameWidth + osWindowFrameWidth + osWindowTitleBarHeight - 1}
	| wKind==IsDialog && wMode==Modal
		= (pos,windows,tb1)
	with
		(screenRect,tb1)	= osScreenrect tb
		screenSize			= rectSize screenRect
		l					= screenRect.rleft + (screenSize.w-exactSize.w)/2
		t					= screenRect.rtop  + (screenSize.h-exactSize.h)/3
		pos					= {x=setBetween l screenRect.rleft screenRect.rright,y=setBetween t screenRect.rtop screenRect.rbottom}
	| isNothing maybePos
		= (zero,windows,tb)
	| otherwise
		# itemPos			= fromJust maybePos
		# (pos,windows,tb)	= getItemPosPosition wMetrics osdInfo exactSize itemPos windows tb
		# (pos,tb)			= setWindowInsideScreen pos exactSize tb
		= (pos,windows,tb)
where
/*	getItemPosPosition calculates the exact position of the given window. 
	getItemPosPosition does not check whether this position will place the window off screen.
*/
	getItemPosPosition :: !OSWindowMetrics !OSDInfo !Size !ItemPos !(WindowHandles .pst) !*OSToolbox
											            -> (!Point2,!WindowHandles .pst, !*OSToolbox)
	getItemPosPosition wMetrics osdInfo size itemPos windows=:{whsWindows=wsHs} tb
		| isRelative
			# (rect,tb)					= osScreenrect tb
			  screenDomain				= rectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			#! (before,after)			= uspan (unidentifyWindow (toWID relativeTo)) wsHs
			   (wPtr,wsH,after)			= case after of
		  									[]	-> windowvalidateFatalError "getItemPosPosition" "target window could not be found"
		  									[wsH:after]
		  										#  (wPtr,wsH) = wsH!wshIds.wPtr
		  										-> (wPtr,wsH,after)
//			   (relativeSize,wsH)		= getWindowStateHandleSize wsH
			   windows					= {windows & whsWindows=before++[wsH:after]}
			# ((relativeW,relativeH),tb)= osGetWindowSize wPtr tb		// size of outer window
			# ((relativeX,relativeY),tb)= osGetWindowPos  wPtr tb		// position of outer window
			# ((xDIx,xDIy),tb)			= getOSDInfoOffset osdInfo tb	// offset of outer (M/S/N)DI frame to content
			/* PA: do not use OSgetWindowViewFrameSize. 
			# ((relativeW,relativeH),tb)= OSgetWindowViewFrameSize wPtr tb
			*/
//			  (relativeW,relativeH)		= toTuple relativeSize
			  (exactW,exactH)			= (size.w,size.h)
			  offset					= itemPosOffset (snd itemPos) [(screenDomain,screenOrigin)]
			  isVectorOffset			= isAlt1Of2 offset;		v		= alt1Of2 offset;
			  isAlignOffset				= isAlt2Of2 offset;		align	= alt2Of2 offset;
			  (vx,vy)					= if isVectorOffset (toTuple v)					// Exact offset is the vector
			  							                    (0,0)						// otherwise, it is zero
			  (dx,dy)					= if isVectorOffset (0,0)						// Align offset is zero in case of vector
			  							 (if isRelativeX    (case align of
			  							 						AlignTop	= (0,0)
			  							 						AlignCenter	= (0,(relativeH-exactH)/2)
			  							 						AlignBottom = (0, relativeH-exactH)
			  							 					)
			  							 /*  isRelativeY */ (case align of
			  							 						AlignLeft	= (0,0)
			  							 						AlignCenter	= ((relativeW-exactW)/2,0)
			  							 						AlignRight  = ( relativeW-exactW,   0)
			  							 					)
			  							 )
			  pos						= case fst itemPos of
						  					(LeftOf  _)	-> {x=relativeX+vx+dx-xDIx-exactW,   y=relativeY+vy+dy-xDIy}
						  					(RightTo _)	-> {x=relativeX+vx+dx-xDIx+relativeW,y=relativeY+vy+dy-xDIy}
				  							(Above   _)	-> {x=relativeX+vx+dx-xDIx,          y=relativeY+vy+dy-xDIy-exactH}
			  								(Below   _)	-> {x=relativeX+vx+dx-xDIx,          y=relativeY+vy+dy-xDIy+relativeH}
			  								other       -> windowvalidateFatalError "getItemPosPosition" "unexpected ItemLoc alternative"
			= (pos,windows,tb)
	where
		(isRelative,relativeTo)		= isRelativeItemPos  itemPos
		isRelativeX					= isRelativeXItemPos itemPos
		isRelativeY					= isRelativeYItemPos itemPos
		
		unidentifyWindow :: !WID !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
		unidentifyWindow wid wsH
			# (ids,wsH)				= getWindowStateHandleWIDS wsH
			= (not (identifyWIDS wid ids),wsH)

	getItemPosPosition _ _ size itemPos windows tb
		| isAbsolute
			# (rect,tb)					= osScreenrect tb
			  screenDomain				= rectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			  v							= case itemPosOffset offset [(screenDomain,screenOrigin)] of
											Alt1Of2 v	= v
											otherwise	= windowvalidateFatalError "getItemPosPosition _ _ (Fix,offset)" "offset is illegal: OffsetAlign"
			= (movePoint v zero,windows,tb)
	where
		(isAbsolute,offset)				= isAbsoluteItemPos itemPos
	getItemPosPosition _ _ size itemPos windows tb
		| isCornerItemPos itemPos
			# (rect,tb)					= osScreenrect tb
			  screenDomain				= rectToRectangle rect
			  screenOrigin				= {x=rect.rleft,y=rect.rtop}
			  (exactW,exactH)			= toTuple size
			  {vx,vy}					= case itemPosOffset (snd itemPos) [(screenDomain,screenOrigin)] of
			  								Alt1Of2 v	= v
			  								_			= windowvalidateFatalError "getItemPosPosition _ _ (pos,offset)" "pos in {LeftTop,LeftBottom,RightTop,RightBottom}, but offset is illegal: OffsetAlign"
			  pos						= case (fst itemPos) of
					  						LeftTop		-> {x=rect.rleft +vx,       y=rect.rtop   +vy}
					  						RightTop	-> {x=rect.rright+vx-exactW,y=rect.rtop   +vy}
					  						LeftBottom	-> {x=rect.rleft +vx,       y=rect.rbottom+vy-exactH}
					  						RightBottom	-> {x=rect.rright+vx-exactW,y=rect.rbottom+vy-exactH}
			= (pos,windows,tb)

	getItemPosPosition _ _ _ _ windows tb
		= (zero,windows,tb)

/*	setWindowInsideScreen makes sure that a window at the given position and given size will be on screen.
*/
	setWindowInsideScreen :: !Point2 !Size !*OSToolbox -> (!Point2,!*OSToolbox)
	setWindowInsideScreen pos=:{x,y} size=:{w,h} tb
		# (screenRect,tb)		= osScreenrect tb
		  {w=screenW,h=screenH}	= rectSize screenRect
		  (x`,y`)				= (setBetween x screenRect.rleft (screenRect.rright-w),setBetween y screenRect.rtop (screenRect.rbottom-h))
		  pos					= if (w<=screenW && h<=screenH)	{x=x`,y=y`}			// window fits entirely on screen
		  						 (if (w<=screenW)				{x=x`,y=0 }			// window is to high
			  					 (if (h<=screenH)				{x=0, y=y`}			// window is to wide
			  					 (								zero)))				// window doesn't fit anyway
		= (pos,tb)



//	Predicates on ItemPos:
isRelativeItemPos :: !ItemPos -> (!Bool,Id)
isRelativeItemPos (LeftOf  id,_)	= (True, id)
isRelativeItemPos (RightTo id,_)	= (True, id)
isRelativeItemPos (Above   id,_)	= (True, id)
isRelativeItemPos (Below   id,_)	= (True, id)
isRelativeItemPos _					= (False,undef)

isRelativeXItemPos :: !ItemPos -> Bool
isRelativeXItemPos (LeftOf  _,_)	= True
isRelativeXItemPos (RightTo _,_)	= True
isRelativeXItemPos _				= False

isRelativeYItemPos :: !ItemPos -> Bool
isRelativeYItemPos (Above _,_)		= True
isRelativeYItemPos (Below _,_)		= True
isRelativeYItemPos _				= False

isAbsoluteItemPos :: !ItemPos -> (!Bool,ItemOffset)
isAbsoluteItemPos (Fix,offset)		= (True, offset)
isAbsoluteItemPos _					= (False,undef)

isCornerItemPos :: !ItemPos -> Bool
isCornerItemPos (LeftTop,_)			= True
isCornerItemPos (RightTop,_)		= True
isCornerItemPos (LeftBottom,_)		= True
isCornerItemPos (RightBottom,_)		= True
isCornerItemPos _					= False
