implementation module controlresize


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, controlrelayout, windowhandle, wstateaccess
from	StdControlAttribute	import isControlResize,       getControlResizeFun,
									isControlMinimumSize, getControlMinimumSizeAtt,
									isControlPos,         getControlPosAtt, isControlViewSize
from	controllayout		import layoutControls, layoutControls`
from	windowaccess		import getWItemCompoundInfo, getWItemEditInfo, getWItemSliderInfo, getWindowInfoWindowData,
									getWindowHMargins, getWindowVMargins, getWindowItemSpaces, identifyMaybeId
from	windowclipstate		import forceValidWindowClipState, forceValidWindowClipState`, invalidateCompoundClipState
from	windowdraw			import drawwindowlook, drawwindow`look
from	windowupdate		import updatewindowbackgrounds, updatewindowbackgrounds`
import	ossystem, ostoolbox, ostypes, oswindow


/*	resizeControls proceeds as follows:
	-	Apply to every control its ControlResizeFunction if applicable.
	-	If some controls have changed size or have been layout relative to the window view frame:
		-	Calculate the new layout.
		-	Reposition and resize the appropriate controls.
*/
resizeControls :: !OSWindowMetrics !Bool !Bool !WIDS !Origin !Size !Size !(WindowHandle .ls .pst) !*OSToolbox
																	  -> (!WindowHandle .ls .pst, !*OSToolbox)
resizeControls wMetrics isActive updateAll wids=:{wPtr} oldOrigin oldWSize newWSize 
			   wH=:{whWindowInfo,whItems=oldItems,whAtts,whDefaultId,whSelect,whShow} tb
	# (oldItems`,oldItems,tb)	= getWElementHandles` wPtr oldItems tb
	# (layoutChanged,newItems)	= calcNewControlsSize wMetrics originShifted oldWSize newWSize oldItems
	| not layoutChanged && newWSize.w<=oldWSize.w && newWSize.h<=oldWSize.h
		# wH					= {wH & whItems=newItems}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		| lookSysUpdate && not updateAll
			= (wH,tb)
		// otherwise
			# updState			= {oldFrame=oldFrame,newFrame=newFrame,updArea=[newFrame]}
			= drawwindowlook wMetrics wPtr id updState wH tb
	| otherwise
		# (_,newItems,tb)		= layoutControls wMetrics hMargins vMargins spaces newWSize minSize [(domain,newOrigin)] newItems tb
		  wH					= {wH & whItems=newItems}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,newItems,tb)	= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (sizeToRect oldWSize,zero,zero,oldItems`) 
								                                                                   (sizeToRect newWSize,zero,zero,wH.whItems) tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn wids {wH & whItems=newItems} tb
		  {x,y}					= oldOrigin
		  (oldw,oldh)			= toTuple oldWSize
		  (neww,newh)			= toTuple newWSize
		  updArea				= if (lookSysUpdate && not originShifted && (neww>oldw || newh>oldh) && isActive && not updateAll)
		  							(	(if (neww>oldw) [{ corner1={oldOrigin & x=x+oldw},corner2={x=x+neww,y=y+min oldh newh}}] [])
		  							++	(if (newh>oldh) [{ corner1={oldOrigin & y=y+oldh},corner2={x=x+neww,y=y+newh}}]          [])
		  							)
		  							[newFrame]
		  updState				= {oldFrame=oldFrame,newFrame=newFrame,updArea=updArea}
		= drawwindowlook wMetrics wPtr id updState wH tb
where
	originShifted				= oldOrigin<>newOrigin
	windowInfo					= getWindowInfoWindowData whWindowInfo
	(newOrigin,domainRect,lookInfo)
								= (windowInfo.windowOrigin,windowInfo.windowDomain,windowInfo.windowLook)
	domain						= rectToRectangle domainRect
	lookSysUpdate				= lookInfo.lookSysUpdate
	(defMinW,   defMinH)		= osMinWindowSize
	wKind						= wH.whKind
	hMargins					= getWindowHMargins   wKind wMetrics whAtts
	vMargins					= getWindowVMargins   wKind wMetrics whAtts
	spaces						= getWindowItemSpaces wKind wMetrics whAtts
	minSize						= {w=defMinW,h=defMinH}
	oldFrame					= posSizeToRectangle oldOrigin oldWSize
	newFrame					= posSizeToRectangle newOrigin newWSize


/*	calcNewControlsSize applies to a Control its ControlResizeFunction if it has one.
	The Boolean result holds iff some Control has changed its size or may cause change of layout.
*/
calcNewControlsSize :: !OSWindowMetrics !Bool !Size !Size ![WElementHandle .ls .pst]  -> (!Bool,![WElementHandle .ls .pst])
calcNewControlsSize wMetrics originShifted oldWSize newWSize []
	= (False,[])
calcNewControlsSize wMetrics originShifted oldWSize newWSize [itemH:itemHs]
	# (layoutChanged1,itemH)	= calcNewControlSize  wMetrics originShifted oldWSize newWSize itemH
	# (layoutChanged2,itemHs)	= calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
	= (layoutChanged1 || layoutChanged2,[itemH:itemHs])
where
	calcNewControlSize :: !OSWindowMetrics !Bool !Size !Size !(WElementHandle .ls .pst) -> (!Bool,!WElementHandle .ls .pst)
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WItemHandle itemH=:{wItemAtts})
		| not resizable
			= (isViewFrameSensitive || isOriginSensitive,WItemHandle itemH)
		| otherwise
			# (layoutChanged,itemH)	= calcNewWItemSize wMetrics originShifted (\oldCSize -> resizeF oldCSize oldWSize newWSize) itemH
			= (isViewFrameSensitive || isOriginSensitive || layoutChanged,WItemHandle itemH)
	where
		(resizable,resizeAtt)	= cselect isControlResize undef wItemAtts
		(hasPos,posAtt)			= cselect isControlPos undef wItemAtts
		itemPos					= getControlPosAtt posAtt
		resizeF					= getControlResizeFun resizeAtt
		isViewFrameSensitive	= if hasPos
									(case (fst itemPos) of
										LeftBottom	-> True
										RightTop	-> True
										RightBottom	-> True
										Center		-> True
										Right		-> True
										Fix			-> originShifted
										_			-> False
									)
									False
		isOriginSensitive		= if hasPos
									(case (snd itemPos) of
										OffsetFun _ _	-> True
										_				-> False
									)
									False
		
		calcNewWItemSize :: !OSWindowMetrics !Bool !(IdFun Size) !(WItemHandle .ls .pst) -> (!Bool,!WItemHandle .ls .pst)
		
/*	PA: the current size argument of the resize function must be the outer size, not the view size.
		Similar: the result size is also the outer size. 
*/		calcNewWItemSize wMetrics originShifted resizeF itemH=:{wItemKind=IsCompoundControl}
			# visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple oldSize) hasScrolls
			  oldFrameSize		= rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect oldSize))
			  newSize			= resizeF oldSize
			  newSize			= {w=max minSize.w newSize.w,h=max minSize.h newSize.h}
			  visScrolls		= osScrollbarsAreVisible wMetrics domainRect (toTuple newSize) hasScrolls
			  newFrameSize		= rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect newSize))
			| newFrameSize==oldFrameSize
				= (False,itemH)
			| otherwise
				# newOrigin		= calcNewOrigin origin domainRect newFrameSize
				  newInfo		= CompoundInfo {info & compoundOrigin=newOrigin}
				  (_,itemHs)	= calcNewControlsSize wMetrics (originShifted || newOrigin<>origin) oldFrameSize newFrameSize itemH.wItems
				  itemH			= {itemH & wItemSize=newSize,wItemAtts=replaceSizeAtt newFrameSize atts,wItemInfo=newInfo,wItems=itemHs}
				  itemH			= invalidateCompoundClipState itemH
				= (True,itemH)
		where
			atts				= itemH.wItemAtts
			oldSize				= itemH.wItemSize
			info				= getWItemCompoundInfo itemH.wItemInfo
			origin				= info.compoundOrigin
			domainRect			= info.compoundDomain
			hasScrolls			= (isJust info.compoundHScroll,isJust info.compoundVScroll)
			(defMinW,defMinH)	= osMinCompoundSize
			minSize				= getControlMinimumSizeAtt (snd (cselect isControlMinimumSize (ControlMinimumSize {w=defMinW,h=defMinH}) atts))
			
			calcNewOrigin :: !Point2 !OSRect !Size -> Point2		// This code also appears at windowdevice: windowStateSizeAction
			calcNewOrigin {x,y} {rleft,rtop,rright,rbottom} {w,h}
				= {x=x`,y=y`}
			where
				x`				= if (x+w>rright)  (max (rright -w) rleft) x
				y`				= if (y+h>rbottom) (max (rbottom-h) rtop ) y
		
		calcNewWItemSize wMetrics originShifted resizeF itemH=:{wItemKind=IsButtonControl}
			# itemH		= {itemH & wItemSize=newSize1,wItemAtts=replaceSizeAtt newSize1 itemH.wItemAtts}
			= (newSize1<>oldSize,itemH)
		where
			oldSize				= itemH.wItemSize
			newSize				= resizeF oldSize
			newSize1			= {w=max (osGetButtonControlMinWidth wMetrics) newSize.w,h=max 0 newSize.h}
		
		calcNewWItemSize _ originShifted resizeF itemH=:{wItemKind=IsCustomControl}
			# itemH		= {itemH & wItemSize=newSize1,wItemAtts=replaceSizeAtt newSize1 itemH.wItemAtts}
			= (newSize1<>oldSize,itemH)
		where
			oldSize				= itemH.wItemSize
			newSize				= resizeF oldSize
			newSize1			= {w=max 0 newSize.w,h=max 0 newSize.h}
		
		calcNewWItemSize _ originShifted resizeF itemH=:{wItemKind=IsCustomButtonControl}
			# itemH				= {itemH & wItemSize=newSize1,wItemAtts=replaceSizeAtt newSize1 itemH.wItemAtts}
			= (newSize1<>oldSize,itemH)
		where
			oldSize				= itemH.wItemSize
			newSize				= resizeF oldSize
			newSize1			= {w=max 0 newSize.w,h=max 0 newSize.h}
		
		calcNewWItemSize wMetrics originShifted resizeF itemH=:{wItemKind=IsEditControl}
			# itemH				= {itemH & wItemSize=newSize1,wItemInfo=editInfo,wItemAtts=replaceSizeAtt newSize1 itemH.wItemAtts}
			= (newSize1<>oldSize,itemH)
		where
			oldSize				= itemH.wItemSize
			newSize				= resizeF oldSize
			info				= getWItemEditInfo itemH.wItemInfo
			lineHeight			= wMetrics.osmHeight
			nrLines1			= max 1 (newSize.h/lineHeight)
			newSize1			= {w=max 0 newSize.w,h=nrLines1*lineHeight}
			editInfo			= EditInfo {info & editInfoWidth=newSize1.w,editInfoNrLines=nrLines1}
		
		calcNewWItemSize wMetrics originShifted resizeF itemH=:{wItemKind=IsLayoutControl}
			# newSize			= resizeF oldSize
			  newSize			= {w=max minSize.w newSize.w,h=max minSize.h newSize.h}
			| newSize==oldSize
				= (False,itemH)
			| otherwise
				# (_,itemHs)	= calcNewControlsSize wMetrics originShifted oldSize newSize itemH.wItems
				  itemH			= {itemH & wItemSize=newSize,wItemAtts=replaceSizeAtt newSize atts,wItems=itemHs}
				= (True,itemH)
		where
			atts				= itemH.wItemAtts
			oldSize				= itemH.wItemSize
			minSize				= getControlMinimumSizeAtt (snd (cselect isControlMinimumSize (ControlMinimumSize zero) atts))
		
		calcNewWItemSize wMetrics originShifted resizeF itemH=:{wItemKind=IsSliderControl}
			# itemH				= {itemH & wItemSize=newSize1,wItemInfo=sliderInfo,wItemAtts=replaceSizeAtt newSize1 itemH.wItemAtts}
			= (newSize1<>oldSize,itemH)
		where
			oldSize				= itemH.wItemSize
			newSize				= resizeF oldSize
			info				= getWItemSliderInfo itemH.wItemInfo
			horizontal			= info.sliderInfoDir==Horizontal
			newSize1			= if horizontal	{w=max newSize.w 0,h=wMetrics.osmHSliderHeight} {w=wMetrics.osmVSliderWidth,h=max newSize.h 0}
			sSize				= if horizontal newSize1.w newSize1.h
			sliderInfo			= SliderInfo {info & sliderInfoLength=sSize}
		
		calcNewWItemSize _ _ _ itemH
			= (False,itemH)
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WListLSHandle itemHs)
		# (layoutChanged,itemHs)	= calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		= (layoutChanged,WListLSHandle itemHs)
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (layoutChanged,itemHs)	= calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		= (layoutChanged,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	calcNewControlSize wMetrics originShifted oldWSize newWSize (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (layoutChanged,itemHs)	= calcNewControlsSize wMetrics originShifted oldWSize newWSize itemHs
		= (layoutChanged,WChangeLSHandle {wChH & wChangeItems=itemHs})

replaceSizeAtt :: !Size ![ControlAttribute .pst] -> [ControlAttribute .pst]
replaceSizeAtt size atts
//		= snd (creplace isControlSize sizeAtt atts)
	# (replaced,atts)	= creplace isControlViewSize sizeAtt atts
	| replaced			= atts
	| otherwise			= atts++[sizeAtt]
where
	sizeAtt				= ControlViewSize size

replaceSizeAtt` :: !Size ![ControlAttribute`] -> [ControlAttribute`]
replaceSizeAtt` size atts
	# (replaced,atts)	= creplace (\cAtt` -> case cAtt` of ControlViewSize` _ -> True; _ -> False) sizeAtt atts
	| replaced			= atts
	| otherwise			= atts++[sizeAtt]
where
	sizeAtt				= ControlViewSize` size

resizeControl :: !OSWindowMetrics !Bool !WIDS !Id !Size !WindowHandle` !*OSToolbox -> (!WindowHandle`, !*OSToolbox)
resizeControl wMetrics updateAll wids=:{wPtr} controlId newCSize wH=:{whWindowInfo`,whItems`=oldItems`,whAtts`,whDefaultId`,whSelect`,whShow`} tb
	#  oldVisScrolls			= osScrollbarsAreVisible wMetrics domainRect wSize` (hasHScroll,hasVScroll)
	   oldContent				= osGetWindowContentRect wMetrics oldVisScrolls (sizeToRect wSize)
	   oldContentSize			= rectSize oldContent
	# (layoutChanged,newItems`)	= replaceControlSize wMetrics controlId newCSize oldItems`
	| not layoutChanged
		= ({wH & whItems`=newItems`},tb)
	| otherwise
		# (_,newItems`,tb)		= layoutControls` wMetrics hMargins vMargins spaces wSize minSize [(domain,origin)] newItems` tb
		  wH					= {wH & whItems`=newItems`}
		# (wH,tb)				= forceValidWindowClipState` wMetrics True wPtr wH tb
		# (updRgn,tb)			= relayoutControls` wMetrics wPtr whDefaultId` False whSelect` whShow` (sizeToRect wSize,zero,zero,oldItems`) (sizeToRect wSize,zero,zero,wH.whItems`) tb
		# (wH,tb)				= updatewindowbackgrounds` wMetrics updRgn wids wH tb
		  {x,y}					= origin
		  (oldw,oldh)			= wSize`
		  updState				= {oldFrame=wFrame,newFrame=wFrame,updArea=[wFrame]}
		= drawwindow`look wMetrics wPtr id updState wH tb
where
	wSize						= wH.whSize`
	wSize`						= toTuple wSize
	windowInfo					= getWindowInfoWindowData whWindowInfo`
	(origin,domainRect,lookInfo,hasHScroll,hasVScroll)
								= (windowInfo.windowOrigin,windowInfo.windowDomain,windowInfo.windowLook,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
	domain						= rectToRectangle domainRect
	lookSysUpdate				= lookInfo.lookSysUpdate
	(defMinW,defMinH)			= osMinWindowSize
	wKind						= wH.whKind`
	hMargins					= getWindowHMargins`   wKind wMetrics whAtts`
	vMargins					= getWindowVMargins`   wKind wMetrics whAtts`
	spaces						= getWindowItemSpaces` wKind wMetrics whAtts`
	minSize						= {w=defMinW,h=defMinH}
	wFrame						= posSizeToRectangle origin wSize

/*	replaceControlSize updates the size of the indicated control.
	The Boolean result holds iff this control has been found.
*/
	replaceControlSize :: !OSWindowMetrics !Id !Size ![WElementHandle`] -> (!Bool,![WElementHandle`])
	replaceControlSize wMetrics controlId newCSize []
		= (False,[])
	replaceControlSize wMetrics controlId newCSize [itemH:itemHs]
		# (done,itemH)		= replaceControlSize` wMetrics controlId newCSize itemH
		| done				= (done,[itemH:itemHs])
		| otherwise
			# (done,itemHs)	= replaceControlSize wMetrics controlId newCSize itemHs
			= (done,[itemH:itemHs])
	where
		replaceControlSize` :: !OSWindowMetrics !Id !Size !WElementHandle` -> (!Bool,!WElementHandle`)
		replaceControlSize`  wMetrics controlId newCSize (WItemHandle` itemH=:{wItemKind`,wItemId`,wItemAtts`})
			| not (identifyMaybeId controlId wItemId`) || not okControl
				| not (isRecursiveControl wItemKind`)
					= (False,WItemHandle` itemH)
				| otherwise
					# (found,itemHs)	= replaceControlSize wMetrics controlId newCSize itemH.wItems`
					= (found,WItemHandle` {itemH & wItems`=itemHs})
			| otherwise
				= (True,WItemHandle` {itemH & wItemSize`=newCSize,wItemAtts`=replaceSizeAtt` newCSize wItemAtts`})
		where
			okControl		= case wItemKind` of
								IsCompoundControl     -> True
								IsCustomButtonControl -> True
								IsCustomControl       -> True
								IsLayoutControl       -> True
								_                     -> False
		
		replaceControlSize` wMetrics controlId newCSize (WRecursiveHandle` itemHs wKind)
			# (done,itemHs)	= replaceControlSize wMetrics controlId newCSize itemHs
			= (done,WRecursiveHandle` itemHs wKind)
