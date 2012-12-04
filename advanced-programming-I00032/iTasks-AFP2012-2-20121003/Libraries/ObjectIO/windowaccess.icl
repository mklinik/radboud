implementation module windowaccess


import	StdBool, StdEnum, StdInt, StdList, StdMisc, StdTuple
import	ossystem, ostypes, oswindow
import	commondef, keyfocus, windowhandle
from	StdControlAttribute	import isControlKeyboard
from StdWindowAttribute	import isWindowInitActive, getWindowInitActiveAtt,
									isWindowHMargin,   getWindowHMarginAtt,
									isWindowVMargin,   getWindowVMarginAtt,
									isWindowItemSpace, getWindowItemSpaceAtt
import	cast

//import StdDebug,dodebug
trace_n _ s :== s

windowaccessFatalError :: String String -> .x
windowaccessFatalError function error
	= fatalError function "windowaccess" error


initWindowHandle :: !Title !WindowMode !WindowKind !WindowInfo !*[WElementHandle .ls .pst] ![WindowAttribute *(.ls,.pst)] -> *WindowHandle .ls .pst
initWindowHandle title mode kind info itemHs atts
	= {	whMode		= mode
	  ,	whKind		= kind
	  ,	whTitle		= title
	  ,	whItemNrs	= [1..]
	  ,	whKeyFocus	= {kfItem=Nothing,kfItems=[]}
      , whWindowInfo= info
	  ,	whItems		= itemHs
	  ,	whShow		= True
	  ,	whSelect	= True
	  ,	whAtts		= atts
	  ,	whDefaultId	= Nothing
	  ,	whCancelId	= Nothing
	  ,	whSize		= zero
	  ,	whClosing	= False
	  }

/*  Access to the particular WindowInfo alternatives (partial functions!).
*/
getWindowInfoWindowData :: !WindowInfo -> WindowData
getWindowInfoWindowData (WindowInfo wData) = wData

getWindowInfoGameWindowData :: !WindowInfo -> GameWindowData
getWindowInfoGameWindowData (GameWindowInfo gwData) = gwData

//	Access to the additional WItemInfo field of a WItemHandle (partial functions!).

getWItemRadioInfo :: !(WItemInfo .ls .pst) -> RadioInfo *(.ls,.pst)
getWItemRadioInfo (RadioInfo info) = info

getWItemCheckInfo :: !(WItemInfo .ls .pst) -> CheckInfo *(.ls,.pst)
getWItemCheckInfo (CheckInfo info) = info

getWItemPopUpInfo :: !(WItemInfo .ls .pst) -> PopUpInfo *(.ls,.pst)
getWItemPopUpInfo (PopUpInfo info) = info

getWItemSliderInfo :: !(WItemInfo .ls .pst) -> SliderInfo *(.ls,.pst)
getWItemSliderInfo (SliderInfo info) = info

getWItemTextInfo :: !(WItemInfo .ls .pst) -> TextInfo
getWItemTextInfo (TextInfo info) = info

getWItemEditInfo :: !(WItemInfo .ls .pst) -> EditInfo
getWItemEditInfo (EditInfo info) = info

getWItemButtonInfo :: !(WItemInfo .ls .pst) -> ButtonInfo
getWItemButtonInfo (ButtonInfo info) = info

getWItemCustomButtonInfo :: !(WItemInfo .ls .pst) -> CustomButtonInfo
getWItemCustomButtonInfo (CustomButtonInfo info) = info

getWItemCustomInfo :: !(WItemInfo .ls .pst) -> CustomInfo
getWItemCustomInfo (CustomInfo info) = info

getWItemCompoundInfo :: !(WItemInfo .ls .pst) -> CompoundInfo
getWItemCompoundInfo (CompoundInfo info) = info

getWItemReceiverInfo :: !(WItemInfo .ls .pst) -> ReceiverHandle .ls .pst
getWItemReceiverInfo (ReceiverInfo info) = info


//	For internal identification of windows/dialogs Id and OSWindowPtr (Integer) can be used.

::	WID							// Identify a window/dialog either
	=	ById  !Id				// by its Id, or
	|	ByPtr !OSWindowPtr		// by its OSWindowPtr

class toWID x :: !x -> WID

instance toWID Id where
	toWID :: !Id -> WID
	toWID id = ById id
instance toWID Int where
	toWID :: !Int -> WID
	toWID wPtr = ByPtr wPtr
instance toWID WIDS where
	toWID :: !WIDS -> WID
	toWID {wPtr} = ByPtr wPtr

widById :: !WID -> Bool
widById (ById _)	= True
widById _			= False

widByPtr :: !WID -> Bool
widByPtr (ByPtr _)	= True
widByPtr _			= False

widGetId :: !WID -> Id
widGetId (ById id)	= id

widGetPtr :: !WID -> OSWindowPtr
widGetPtr (ByPtr ptr) = ptr

//identifyWindowIds :: !WID !WIDS -> Bool
identifyWIDS :: !WID !WIDS -> Bool
identifyWIDS (ById  id)  {wId}  = id==wId
identifyWIDS (ByPtr ptr) {wPtr} = ptr==wPtr

identifyMaybeId :: !Id !(Maybe Id) -> Bool
identifyMaybeId id (Just id`) = id==id`
identifyMaybeId _ _ = False


//	Transforming CursorShape to OS cursor codes (moved to oswindow):
/*
toCursorCode :: !CursorShape -> Int
toCursorCode StandardCursor	= CURSARROW
toCursorCode BusyCursor		= CURSBUSY
toCursorCode IBeamCursor	= CURSIBEAM
toCursorCode CrossCursor	= CURSCROSS
toCursorCode FatCrossCursor	= CURSFATCROSS
toCursorCode ArrowCursor	= CURSARROW
toCursorCode HiddenCursor	= CURSHIDDEN
*/

//	Calculating the view frame of window/compound with visibility of scrollbars.
/*	PA: moved to oswindow because of dependency on Mac and Windows.
getCompoundContentRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getCompoundContentRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| visHScroll && visVScroll	= {itemRect & rright=r`,rbottom=b`}
	| visHScroll				= {itemRect &           rbottom=b`}
	| visVScroll				= {itemRect & rright=r`           }
	| otherwise					= itemRect
where
	r`							= rright -osmVSliderWidth
	b`							= rbottom-osmHSliderHeight

getCompoundHScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getCompoundHScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| not visHScroll	= zero
	| otherwise			= {itemRect & rleft = itemRect.rleft - 1, rbottom = rbottom+1, rtop=b`, rright=r`+1}
where
	r`					= rright -osmVSliderWidth + 1
	b`					= rbottom-osmHSliderHeight + 1

getCompoundVScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getCompoundVScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| not visVScroll	= zero
	| otherwise			= {itemRect & rtop = itemRect.rtop, rright = rright + 1, rleft=r`,rbottom= b` + 1}//if visHScroll b` rbottom}
where
	r`					= rright -osmVSliderWidth + 1
	b`					= rbottom-osmHSliderHeight + 1


getWindowContentRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getWindowContentRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) itemRect=:{rright,rbottom}
	| visHScroll && visVScroll	= {itemRect & rright=r`,rbottom=b`}
	| visHScroll				= {itemRect &           rbottom=b`}
	| visVScroll				= {itemRect & rright=r`           }
	| otherwise					= itemRect
where
	r`							= rright -osmVSliderWidth //+1
	b`							= rbottom-osmHSliderHeight//+1

getWindowHScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getWindowHScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) {rleft,rtop,rright,rbottom}
	| not visHScroll	= zero
	| otherwise			= {rleft=rleft-1,rtop=b`,rright= /*if visVScroll*/ (r`+1) /*(rright+1)*/,rbottom=rbottom+1}
where
	r`					= rright -osmVSliderWidth  + 1
	b`					= rbottom-osmHSliderHeight + 1

getWindowVScrollRect :: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getWindowVScrollRect {osmHSliderHeight,osmVSliderWidth} (visHScroll,visVScroll) {rleft,rtop,rright,rbottom}
	| not visVScroll	= zero
	| otherwise			= {rleft=r`,rtop=rtop-1,rright=rright+1,rbottom= /*if visHScroll*/ (b`+1) /*(rbottom+1)*/}
where
	r`					= rright -osmVSliderWidth  + 1
	b`					= rbottom-osmHSliderHeight + 1
*/

//	Access operations on WindowStateHandles:

getWindowStateHandleWIDS :: !(WindowStateHandle .pst) -> *(!WIDS,!WindowStateHandle .pst)
getWindowStateHandleWIDS wsH=:{wshIds}
	= (wshIds,wsH)

getWindowStateHandleWindowMode :: !(WindowStateHandle .pst) -> *(!WindowMode,!WindowStateHandle .pst)
getWindowStateHandleWindowMode wsH=:{wshHandle=Just {wlsHandle={whMode}}}
	= (whMode,wsH)

getWindowStateHandleWindowKind :: !(WindowStateHandle .pst) -> *(!WindowKind,!WindowStateHandle .pst)
getWindowStateHandleWindowKind wsH=:{wshHandle=Just {wlsHandle={whKind}}}
	= (whKind,wsH)

getWindowStateHandleWindowTitle :: !(WindowStateHandle .pst) -> *(!Title,!WindowStateHandle .pst)
getWindowStateHandleWindowTitle wsH=:{wshHandle=Just {wlsHandle={whTitle}}}
	= (whTitle,wsH)

getWindowStateHandleItemNrs :: !(WindowStateHandle .pst) -> *(![Int],!WindowStateHandle .pst)
getWindowStateHandleItemNrs wsH=:{wshHandle=Just {wlsHandle={whItemNrs}}}
	= (whItemNrs,wsH)

getWindowStateHandleKeyFocus :: !(WindowStateHandle .pst) -> *(!*KeyFocus,!WindowStateHandle .pst)
getWindowStateHandleKeyFocus wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whKeyFocus}}}
	= (whKeyFocus,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whKeyFocus=newFocusItems []}}})

getWindowStateHandleWindowInfo :: !(WindowStateHandle .pst) -> *(!WindowInfo,!WindowStateHandle .pst)
getWindowStateHandleWindowInfo wsH=:{wshHandle=Just {wlsHandle={whWindowInfo}}}
	= (whWindowInfo,wsH)

getWindowStateHandleShow :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
getWindowStateHandleShow wsH=:{wshHandle=Just {wlsHandle={whShow}}}
	= (whShow,wsH)

getWindowStateHandleSelect :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
getWindowStateHandleSelect wsH=:{wshHandle=Just {wlsHandle={whSelect}}}
	= (whSelect,wsH)

getWindowStateHandleActive :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
getWindowStateHandleActive wsH=:{wshIds={wActive}}
	= (wActive,wsH)

getWindowStateHandleDefaultId :: !(WindowStateHandle .pst) -> *(!Maybe Id,!WindowStateHandle .pst)
getWindowStateHandleDefaultId wsH=:{wshHandle=Just {wlsHandle={whDefaultId}}}
	= (whDefaultId,wsH)

getWindowStateHandleCancelId :: !(WindowStateHandle .pst) -> *(!Maybe Id,!WindowStateHandle .pst)
getWindowStateHandleCancelId wsH=:{wshHandle=Just {wlsHandle={whCancelId}}}
	= (whCancelId,wsH)

getWindowStateHandleSize :: !(WindowStateHandle .pst) -> *(!Size,!WindowStateHandle .pst)
getWindowStateHandleSize wsH=:{wshHandle=Just {wlsHandle={whSize}}}
	= (whSize,wsH)

getWindowStateHandleClosing :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
getWindowStateHandleClosing wsH=:{wshHandle=Just {wlsHandle={whClosing}}}
	= (whClosing,wsH)

isWindowStateHandlePlaceHolder :: !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
isWindowStateHandlePlaceHolder wsH=:{wshHandle=Nothing}
	= (True,wsH)
isWindowStateHandlePlaceHolder wsH
	= (False,wsH)

identifyWindowStateHandle :: !WID !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
identifyWindowStateHandle wid wsH
	# (wids,wsH)	= getWindowStateHandleWIDS wsH
	= (identifyWIDS wid wids,wsH)

setWindowStateHandleWindowTitle :: !Title !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleWindowTitle title wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whTitle=title}}}

setWindowStateHandleItemNrs :: ![Int] !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleItemNrs itemNrs wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItemNrs=itemNrs}}}

setWindowStateHandleKeyFocus :: !*KeyFocus !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleKeyFocus keyFocus wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whKeyFocus=keyFocus}}}

setWindowStateHandleWindowInfo :: !WindowInfo !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleWindowInfo windowInfo wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whWindowInfo=windowInfo}}}

setWindowStateHandleShow :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleShow show wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whShow=show}}}

setWindowStateHandleSelect :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSelect select wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whSelect=select}}}

setWindowStateHandleActive :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleActive active wsH=:{wshIds=wids}
	= {wsH & wshIds={wids & wActive=active}}

setWindowStateHandleDefaultId :: !(Maybe Id) !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleDefaultId defaultId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whDefaultId=defaultId}}}

setWindowStateHandleCancelId :: !(Maybe Id) !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleCancelId cancelId wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whCancelId=cancelId}}}

setWindowStateHandleSize :: !Size !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSize size wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whSize=size}}}

setWindowStateHandleClosing :: !Bool !(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleClosing closing wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	= {wsH & wshHandle=Just {wlsH & wlsHandle={wH & whClosing=closing}}}


/*	Access operations on the margins and item space attributes of the window attributes.
	getWindow((H/V)Margin/ItemSpace)s type metrics atts
		retrieves the indicated attribute if present from the attribute list. If the attribute
		could not be found, the appropriate default value is returned. 
*/
getWindowHMargins :: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)
getWindowHMargins type wMetrics atts
	= getWindowHMarginAtt (snd (cselect isWindowHMargin (WindowHMargin defaultLeft defaultRight) atts))
where
	(defaultLeft,defaultRight)	= case type of
									IsDialog -> (wMetrics.osmHorMargin,wMetrics.osmHorMargin)
									other    -> (0,0)

getWindowVMargins :: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)
getWindowVMargins type wMetrics atts
	= getWindowVMarginAtt (snd (cselect isWindowVMargin (WindowVMargin defaultTop defaultBottom) atts))
where
	(defaultTop,defaultBottom)	= case type of
									IsDialog -> (wMetrics.osmVerMargin,wMetrics.osmVerMargin)
									other    -> (0,0)

getWindowItemSpaces :: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)
getWindowItemSpaces type wMetrics atts
	= getWindowItemSpaceAtt (snd (cselect isWindowItemSpace (WindowItemSpace defaultHor defaultVer) atts))
where
	(defaultHor,defaultVer)		= case type of
									IsDialog -> (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
									other    -> (0,0)

//	Search, get, and set WindowStateHandles.

getWindowHandlesWindows :: !(WindowHandles .pst) -> *(![WindowStateHandle .pst],!WindowHandles .pst)
getWindowHandlesWindows wHs=:{whsWindows=wsHs}
	= (wsHs,{wHs & whsWindows=[]})

setWindowHandlesWindows :: ![WindowStateHandle .pst] !(WindowHandles .pst) -> WindowHandles .pst
setWindowHandlesWindows wsHs wHs
	= {wHs & whsWindows=wsHs}

getWindowHandlesActiveWindow :: !(WindowHandles .pst) -> *(!Maybe WIDS,!WindowHandles .pst)
getWindowHandlesActiveWindow wHs=:{whsWindows=wsHs}
	# (found,wids,wsHs)	= access get_active_wids undef wsHs
	  wHs				= {wHs & whsWindows=wsHs}
	| found				= (Just wids,wHs)
	| otherwise			= (Nothing,  wHs)
where
	get_active_wids :: !(WindowStateHandle .pst) -> *(!*(!Bool,WIDS),!WindowStateHandle .pst)
	get_active_wids wsH
		# (wids,wsH)	= getWindowStateHandleWIDS wsH
		= ((wids.wActive,wids),wsH)

//	getWindowHandlesActiveModalDialog assumes that all modal dialogues are at the front of the list
getWindowHandlesActiveModalDialog :: !(WindowHandles .pst) -> *(!Maybe WIDS,!WindowHandles .pst)
getWindowHandlesActiveModalDialog wHs=:{whsWindows=[]}
	= (Nothing,wHs)
getWindowHandlesActiveModalDialog wHs=:{whsWindows=[wsH:wsHs]}
	# (mode,wsH)	= getWindowStateHandleWindowMode wsH
	| mode<>Modal
		= (Nothing,{wHs & whsWindows=[wsH:wsHs]})
	| otherwise
		# (wids,wsH)= getWindowStateHandleWIDS wsH
		= (Just wids,{wHs & whsWindows=[wsH:wsHs]})

hasWindowHandlesWindow :: !WID !(WindowHandles .pst) -> *(!Bool,!WindowHandles .pst)
hasWindowHandlesWindow wid wHs=:{whsWindows}
	# (found,windows)	= haswindow wid whsWindows
	= (found,{wHs & whsWindows=windows})
where
	haswindow :: !WID ![WindowStateHandle .pst] -> *(!Bool,![WindowStateHandle .pst])
	haswindow wid [wsH:wsHs]
		# (wIds,wsH)		= getWindowStateHandleWIDS wsH
		| identifyWIDS wid wIds
			= (True, [wsH:wsHs])
		| otherwise
			# (found,wsHs)	= haswindow wid wsHs
			= (found,[wsH:wsHs])
	haswindow _ _
		= (False,[])

getWindowHandlesWindow :: !WID !(WindowHandles .pst) -> *(!Bool,WindowStateHandle .pst,!WindowHandles .pst)
getWindowHandlesWindow wid wHs=:{whsWindows}
	# (ok,wsH,wsHs)	= getwindow wid whsWindows
	= (ok,wsH,{wHs & whsWindows=wsHs})
where
	getwindow :: !WID ![WindowStateHandle .pst] -> *(!Bool,WindowStateHandle .pst,![WindowStateHandle .pst])
	getwindow wid [wsH:wsHs]
		# (wIds,wsH)	= getWindowStateHandleWIDS wsH
		| identifyWIDS wid wIds
			= (True, wsH, [{wshIds=wIds,wshHandle=Nothing}:wsHs])
		| otherwise
			# (found,wsH`,wsHs) = getwindow wid wsHs
			= (found,wsH`,[wsH:wsHs])
	getwindow _ _
		= (False,undef,[])

removeWindowHandlesWindow :: !WID !(WindowHandles .pst) -> *(!Bool,WindowStateHandle .pst,!WindowHandles .pst)
removeWindowHandlesWindow wid wHs=:{whsWindows}
	# (ok,wsH,wsHs)	= uremove (identifyWindowStateHandle wid) undef whsWindows
	= (ok,wsH,{wHs & whsWindows=wsHs})
where
	identifyWindowStateHandle :: !WID !(WindowStateHandle .pst) -> *(!Bool,!WindowStateHandle .pst)
	identifyWindowStateHandle wid wsH
		# (windowIds,wsH)	= getWindowStateHandleWIDS wsH
		= (identifyWIDS wid windowIds,wsH)

setWindowHandlesWindow :: !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
setWindowHandlesWindow wsH wHs=:{whsWindows}
	# (isPlaceHolder,wsH)	= isWindowStateHandlePlaceHolder wsH
	| isPlaceHolder
		= windowaccessFatalError "setWindowHandlesWindow" "WindowStateHandle argument should not be a place holder"
	| otherwise
		#  (wIds,wsH)		= getWindowStateHandleWIDS wsH
		#! wsHs				= setwindow wIds wsH whsWindows		// PA: strictness added
		=  {wHs & whsWindows=wsHs}
where
	setwindow :: !WIDS !(WindowStateHandle .pst) ![WindowStateHandle .pst] -> [WindowStateHandle .pst]
	setwindow wids wsH [wsH`:wsHs]
		# (wids`,wsH`)		= getWindowStateHandleWIDS wsH`
		| wids<>wids`
			#! wsHs			= setwindow wids wsH wsHs
			= [wsH`:wsHs]
		# (isPlaceHolder,_)	= isWindowStateHandlePlaceHolder wsH`
		| isPlaceHolder
			= [wsH:wsHs]
		| otherwise
			= windowaccessFatalError "setWindowHandlesWindow" "place holder expected instead of WindowStateHandle"
	setwindow _ _ _
		= windowaccessFatalError "setWindowHandlesWindow" "place holder not found"

addBehindWindowHandlesWindow :: !WID !(WindowStateHandle .pst) !(WindowHandles .pst) -> *(!WIDS,!WindowHandles .pst)
addBehindWindowHandlesWindow behindWID wsH wHs=:{whsWindows}
	# (isPlaceHolder,wsH)	= isWindowStateHandlePlaceHolder wsH
	| isPlaceHolder
		= windowaccessFatalError "addBehindWindowHandlesWindow" "WindowStateHandle argument should not be a place holder"
	| otherwise
		# (behindWIDS,wsHs)	= stackBehind behindWID wsH whsWindows
		= (behindWIDS,{wHs & whsWindows=wsHs})
where
	stackBehind :: !WID !(WindowStateHandle .pst) ![WindowStateHandle .pst] -> *(!WIDS,![WindowStateHandle .pst])
	stackBehind behindWID wsH [wsH`:wsHs]
		# (wids`,wsH`)			= getWindowStateHandleWIDS wsH`
		| not (identifyWIDS behindWID wids`)
			# (behindWIDS,wsHs) = stackBehind behindWID wsH wsHs
			= (behindWIDS,[wsH`:wsHs])
		# (mode`,wsH`)	= getWindowStateHandleWindowMode wsH`
		| mode`==Modal
			# (behindWIDS,wsHs)	= stackBehindLastModal wsH wids` wsHs
			= (behindWIDS,[wsH`:wsHs])
		with
			stackBehindLastModal :: !(WindowStateHandle .pst) !WIDS ![WindowStateHandle .pst] -> *(!WIDS,![WindowStateHandle .pst])
			stackBehindLastModal wsH behindModal [wsH`:wsHs]
				# (wids`,wsH`)			= getWindowStateHandleWIDS wsH`
				# (mode`,wsH`)			= getWindowStateHandleWindowMode wsH`
				| mode`==Modal
					# (modalWIDS,wsHs)	= stackBehindLastModal wsH wids` wsHs
					= (modalWIDS,[wsH`:wsHs])
				| otherwise
					= (behindModal,[wsH,wsH`:wsHs])
			stackBehindLastModal wsH behindModal _
				= (behindModal,[wsH])
		| otherwise
			= (wids`,[wsH`,wsH:wsHs])
	stackBehind _ _ _
		= windowaccessFatalError "addBehindWindowHandlesWindow" "behind window could not be found"

addWindowHandlesWindow :: !Index !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
addWindowHandlesWindow index wsH wHs=:{whsWindows}
	#! wsHs	= insert (max 0 index) wsH whsWindows
	= {wHs & whsWindows=wsHs}
where
	insert :: !Index !u:x !v:[u:x] -> v:[u:x]
	insert 0 x ys
		= [x:ys]
	insert i x [y:ys]
		#! ys = insert (i-1) x ys
		= [y:ys]
	insert _ x _
		= [x]

addWindowHandlesActiveWindow :: !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst
addWindowHandlesActiveWindow wsH=:{wshIds=wids} wHs=:{whsWindows}
	# (mode,wsH)	= getWindowStateHandleWindowMode wsH
	| mode==Modal
		= {wHs & whsWindows=[{wsH & wshIds=widsActive}:whsWindows]}
	| otherwise
		#! wsHs		= addBehindLastModal False wsH whsWindows
		= {wHs & whsWindows=wsHs}
where
	widsActive	= {wids & wActive=True}
	
	addBehindLastModal :: !Bool !(WindowStateHandle .pst) ![WindowStateHandle .pst] -> [WindowStateHandle .pst]
	addBehindLastModal modalsExist wsH [wsH`:wsHs]
		# (mode`,wsH`)	= getWindowStateHandleWindowMode wsH`
		  isModal		= mode`==Modal
		| isModal
			#! wsHs		= addBehindLastModal (isModal || modalsExist) wsH wsHs
			= [wsH`:wsHs]
		| modalsExist
			= [wsH,wsH`:wsHs]
		| otherwise
			= [{wsH & wshIds=widsActive},wsH`:wsHs]
	addBehindLastModal modalsExist wsH _
		| modalsExist
			= [wsH]
		| otherwise
			= [{wsH & wshIds=widsActive}]


/*	disableWindowSystem disables all current windows.
	The return WIDS is the WIDS of the topmost active modal dialogue, if present. 
*/
disableWindowSystem :: !(WindowHandles .pst) !*OSToolbox -> *(!*(!Maybe WIDS,!WindowHandles .pst),!*OSToolbox)
disableWindowSystem windows=:{whsModal,whsWindows} tb
	| not whsModal
		# (wHs,tb)	= stateMap disablewindow whsWindows tb
		= ((Nothing,{windows & whsModal=True,whsWindows=wHs}),tb)
	# (activeWIDS,windows)	= getWindowHandlesActiveWindow windows
	| isNothing activeWIDS
		= windowaccessFatalError "disableWindowSystem" "no active window found"
	| otherwise
		= ((activeWIDS,windows),osDisableWindow (fromJust activeWIDS).wPtr (False,False) True tb)
where
	disablewindow :: !(WindowStateHandle .pst) !*OSToolbox -> *(!WindowStateHandle .pst,!*OSToolbox)
	disablewindow wsH tb
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (windowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
		# scrollInfo		= case windowInfo of
								WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
								other			-> (False,False)
		= (wsH,osDisableWindow wids.wPtr scrollInfo True tb)

/*	enableWindowSystem Nothing re-enables all current windows.
	enableWindowSystem (Just wids) re-enables the modal dialogue indicated by wids.
*/
enableWindowSystem :: !(Maybe WIDS) !(WindowHandles .pst) !*OSToolbox -> *(!WindowHandles .pst,!*OSToolbox)
enableWindowSystem Nothing windows=:{whsWindows} tb
	# (wHs,tb)	= stateMap enablewindow whsWindows tb
	= ({windows & whsModal=False,whsWindows=wHs},tb)
where
	enablewindow :: !(WindowStateHandle .pst) !*OSToolbox -> *(!WindowStateHandle .pst,!*OSToolbox)
	enablewindow wsH tb
		# (whSelect,wsH)		= getWindowStateHandleSelect wsH
		| not whSelect
			= (wsH,tb)
		| otherwise
			# (wids,wsH)		= getWindowStateHandleWIDS wsH
			# (windowInfo,wsH)	= getWindowStateHandleWindowInfo wsH
			  scrollInfo		= case windowInfo of
			  						WindowInfo info	-> (isJust info.windowHScroll,isJust info.windowVScroll)
			  						other			-> (False,False)
			= (wsH,osEnableWindow wids.wPtr scrollInfo True tb)
enableWindowSystem (Just wids) windows tb
	= (windows,osEnableWindow wids.wPtr (False,False) True tb)


/*	Checking WindowBounds:
*/
checkZeroWindowHandlesBound :: !(WindowHandles .pst) -> *(!Bool,!WindowHandles .pst)
checkZeroWindowHandlesBound wHs=:{whsNrWindowBound}
	= (zeroBound whsNrWindowBound,wHs)

decreaseWindowHandlesBound :: !(WindowHandles .pst) -> WindowHandles .pst
decreaseWindowHandlesBound wHs=:{whsNrWindowBound}
	= {wHs & whsNrWindowBound=decBound whsNrWindowBound}


// DvA

getWindowHandlesCursorInfo :: !(WindowHandles .pst) -> *(!CursorInfo,!WindowHandles .pst)
getWindowHandlesCursorInfo wHs=:{whsCursorInfo} = (whsCursorInfo,wHs)

setWindowHandlesCursorInfo :: !CursorInfo !(WindowHandles .pst) -> WindowHandles .pst
setWindowHandlesCursorInfo ci wHs = {wHs & whsCursorInfo = ci}


/*	getInitActiveControl retrieves the OSWindowPtr of the control that has the initial input focus.
	It is assumed that the control identified by the WindowInitActive attribute exists.
*/
getInitActiveControl :: !(WindowHandle .ls .pst) -> *(!OSWindowPtr,!WindowHandle .ls .pst)
getInitActiveControl wH=:{whItems=itemHs,whAtts}
	# (found,itemPtr,itemHs)			= getFocusWElementHandles initActiveId itemHs
	= (if found itemPtr OSNoWindowPtr,{wH & whItems=itemHs})
where
	(hasInitActiveAtt,initActiveAtt)	= cselect isWindowInitActive undef whAtts
	initActiveId						= if hasInitActiveAtt (Just (getWindowInitActiveAtt initActiveAtt)) Nothing
	
	getFocusWElementHandles :: !(Maybe Id) ![WElementHandle .ls .pst] -> (!Bool,!OSWindowPtr,![WElementHandle .ls .pst])
	getFocusWElementHandles initActiveId [itemH:itemHs]
		# (found,itemPtr,itemH)		= getFocusWElementHandle initActiveId itemH
		| found
			= (found,itemPtr,[itemH:itemHs])
		| otherwise
			# (found,itemPtr,itemHs)= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,[itemH:itemHs])
	where
		getFocusWElementHandle :: !(Maybe Id) !(WElementHandle .ls .pst) -> (!Bool,!OSWindowPtr,!WElementHandle .ls .pst)
		getFocusWElementHandle initActiveId (WItemHandle itemH=:{wItemId,wItemKind,wItemPtr,wItems})
			| isJust initActiveId && initActiveId==wItemId
				= (True,wItemPtr,WItemHandle itemH)
			| wItemKind==IsEditControl && isNothing initActiveId
				= (True,wItemPtr,WItemHandle itemH)
			// add editable popup checking...
			| otherwise
				# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId wItems
				= (found,itemPtr,WItemHandle {itemH & wItems=itemHs})
		getFocusWElementHandle initActiveId (WListLSHandle itemHs)
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WListLSHandle itemHs)
		getFocusWElementHandle initActiveId (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WExtendLSHandle {wExH & wExtendItems=itemHs})
		getFocusWElementHandle initActiveId (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,itemPtr,itemHs)	= getFocusWElementHandles initActiveId itemHs
			= (found,itemPtr,WChangeLSHandle {wChH & wChangeItems=itemHs})
	getFocusWElementHandles _ _
		= (False,OSNoWindowPtr,[])


/*	Determine the list of window items that can obtain the keyboard input focus.
*/
getWElementKeyFocusIds :: !Bool ![WElementHandle .ls .pst] -> (!*[FocusItem],![WElementHandle .ls .pst])
getWElementKeyFocusIds shownContext [itemH:itemHs]
	# (ids1,itemH)	= getWElementKeyFocusIds` shownContext itemH
	  (ids2,itemHs)	= getWElementKeyFocusIds  shownContext itemHs
	= (ids1++ids2,[itemH:itemHs])
where
	getWElementKeyFocusIds` :: !Bool !(WElementHandle .ls .pst) -> (!*[FocusItem],!WElementHandle .ls .pst)
	getWElementKeyFocusIds` shownContext (WItemHandle itemH)
		# (ids,itemH)	= getWItemKeyFocusIds itemH
		= (ids,WItemHandle itemH)
	where
		getWItemKeyFocusIds :: !(WItemHandle .ls .pst) -> (!*[FocusItem],!WItemHandle .ls .pst)
		getWItemKeyFocusIds itemH=:{wItemNr,wItemKind,wItemShow,wItemAtts,wItems,wItemInfo}
			#! focus = trace_n ("getWIKFIB",wItemNr,wItemKind) focus
			| wItemKind==IsEditControl
				= (focus,itemH)
			| isPopup && hasKeyAtt	//isEditable
				= (focus,itemH)
			| keySensitive && hasKeyAtt
				= (focus,itemH)
			| otherwise
				#! wItems = trace_n ("getWIKFIA",wItemNr,wItemKind) wItems
				# (focus,itemHs)= getWElementKeyFocusIds (shownContext/* && wItemShow*/) wItems
				  itemH			= {itemH & wItems=itemHs}
				= (focus,itemH)
		where
			focus				= [{focusNr=wItemNr,focusShow=shownContext}]
			hasKeyAtt			= contains isControlKeyboard wItemAtts
			keySensitive		= wItemKind == IsCustomControl || wItemKind == IsCompoundControl
			isPopup				= wItemKind == IsPopUpControl
//			isEditable			= isJust (getWItemPopUpInfo wItemInfo).popUpInfoEdit
	
	getWElementKeyFocusIds` shownContext (WListLSHandle itemHs)
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WListLSHandle itemHs)
	
	getWElementKeyFocusIds` shownContext (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	getWElementKeyFocusIds` shownContext (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (ids,itemHs)	= getWElementKeyFocusIds shownContext itemHs
		= (ids,WChangeLSHandle {wChH & wChangeItems=itemHs})

getWElementKeyFocusIds _ _
	= ([],[])


/*	Generate internal numbers for all WElementHandles which wItemNr==0.
*/
genWElementItemNrs :: ![Int] ![WElementHandle .ls .pst] -> (![Int],![WElementHandle .ls .pst])
genWElementItemNrs nrs [itemH:itemHs]
	# (nrs,itemH)	= genWElementNrs  nrs itemH
	  (nrs,itemHs)	= genWElementItemNrs nrs itemHs
	= (nrs,[itemH:itemHs])
where
	genWElementNrs :: ![Int] !(WElementHandle .ls .pst) -> (![Int],!WElementHandle .ls .pst)
	genWElementNrs nrs wItemH=:(WItemHandle itemH=:{wItemNr,wItemKind,wItems})
		# (nrs,itemHs)	= genWElementItemNrs nrs wItems
		| wItemNr<>0
			= (nrs,WItemHandle {itemH & wItems=itemHs})
		| otherwise
			# (nr,nrs)	= hdtl nrs
			= (nrs,WItemHandle {itemH & wItemNr=nr,wItems=itemHs})
	
	genWElementNrs nrs (WListLSHandle itemHs)
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WListLSHandle itemHs)
	
	genWElementNrs nrs (WExtendLSHandle wExH=:{wExtendItems=itemHs})
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WExtendLSHandle {wExH & wExtendItems=itemHs})
	
	genWElementNrs nrs (WChangeLSHandle wChH=:{wChangeItems=itemHs})
		# (nrs,itemHs)	= genWElementItemNrs nrs itemHs
		= (nrs,WChangeLSHandle {wChH & wChangeItems=itemHs})

genWElementItemNrs nrs _
	= (nrs,[])


getFinalModalLS :: !WID !FinalModalLS -> Maybe .ls
getFinalModalLS wid {fmWIDS,fmLS}
	| identifyWIDS wid fmWIDS
		= Just (cast fmLS)
	| otherwise
		= Nothing
