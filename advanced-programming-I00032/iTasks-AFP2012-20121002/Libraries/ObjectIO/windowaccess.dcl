definition module windowaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access operations to Window(State)Handle(s).
//	********************************************************************************

import	windowhandle
import	ossystem, ostoolbox
from	keyfocus	import :: FocusItem


/*	Initial window handle. The following arguments are passed:
*/
initWindowHandle :: !Title								// the window title
					!WindowMode							// the window mode
					!WindowKind							// the window kind
					!WindowInfo							// the window info
					!*[WElementHandle .ls .pst]			// the controls
					![WindowAttribute *(.ls,.pst)]		// the attributes
				 -> *WindowHandle .ls .pst


/*  Access to the particular WindowInfo alternatives (partial functions!).
*/
getWindowInfoWindowData		:: !WindowInfo -> WindowData
getWindowInfoGameWindowData	:: !WindowInfo -> GameWindowData


/*	Access to the particular WItemInfo alternatives. These functions are partially defined!
*/
getWItemRadioInfo		:: !(WItemInfo .ls .pst) -> RadioInfo  *(.ls,.pst)
getWItemCheckInfo		:: !(WItemInfo .ls .pst) -> CheckInfo  *(.ls,.pst)
getWItemPopUpInfo		:: !(WItemInfo .ls .pst) -> PopUpInfo  *(.ls,.pst)
getWItemSliderInfo		:: !(WItemInfo .ls .pst) -> SliderInfo *(.ls,.pst)
getWItemTextInfo		:: !(WItemInfo .ls .pst) -> TextInfo
getWItemEditInfo		:: !(WItemInfo .ls .pst) -> EditInfo
getWItemButtonInfo		:: !(WItemInfo .ls .pst) -> ButtonInfo
getWItemCustomButtonInfo:: !(WItemInfo .ls .pst) -> CustomButtonInfo
getWItemCustomInfo		:: !(WItemInfo .ls .pst) -> CustomInfo
getWItemCompoundInfo	:: !(WItemInfo .ls .pst) -> CompoundInfo
getWItemReceiverInfo	:: !(WItemInfo .ls .pst) -> ReceiverHandle .ls .pst


//	Window/dialog identification:
::	WID								// Identify a window/dialog either
	=	ById	!Id					// by its Id, or
	|	ByPtr	!OSWindowPtr		// by its OSWindowPtr

class toWID x :: !x -> WID

instance toWID Id
instance toWID Int
instance toWID WIDS

widById					:: !WID -> Bool				// WIDbyId   (ById  _)  = True; False
widByPtr				:: !WID -> Bool				// WIDbyPtr  (ByPtr _)  = True; False
widGetId				:: !WID -> Id				// WIDgetId  (ById  id) = id	(partial function)
widGetPtr				:: !WID -> OSWindowPtr		// WIDgetPtr (ByPtr ptr)= ptr	(partial function)
identifyWIDS			:: !WID !WIDS      -> Bool	// identifyWIDS (ById  id)  {wId}  = id ==wId
													// identifyWIDS (ByPtr ptr) {wPtr} = ptr==wPtr
identifyMaybeId			:: !Id !(Maybe Id) -> Bool	// identifyMaybeId id (Just id`) = id==id`
													// identifyMaybeId _ _ = False

//	Transforming CursorShape to OS cursor code:
//toCursorCode :: !CursorShape -> Int


/*	Given whether a CompoundControl/Window has a visible (Control/Window)HScroll (first Bool), (Control/Window)VScroll (second Bool),
	and the surrounding rectangle of the control/window:
	get(Compound/Window)ContentRect yields the Rect of the content part;
	get(Compound/Window)(H/V)ScrollRect yields the Rect of the horizontal/vertical scroll component.
PA: moved to oswindow because of dependency on Mac/Windows platform.
getCompoundContentRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getCompoundHScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getCompoundVScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect

getWindowContentRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getWindowHScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
getWindowVScrollRect	:: !OSWindowMetrics !(!Bool,!Bool) !Rect -> Rect
*/

/*	Access operations on WindowStateHandles:
	getWindowStateHandleWIDS
		returns the WIDS of the WindowStateHandle argument.
	getWindowStateHandle(WindowMode/WindowKind/WindowTitle/ItemNrs/KeyFocus/WindowInfo/Show/Select/DefaultId/CancelId)
		returns the (WindowMode/WindowKind/Title/item nrs/KeyFocus/Origin/WindowInfo/is shown/is Able/default Id/cancel Id) 
		of the WindowStateHandle argument.
		These functions fail when applied to a placeholder. 
	isWindowStateHandlePlaceHolder
		returns True iff the WindowStateHandle argument is a placeholder.
	identifyWindowStateHandle
		returns True iff the WindowStateHandle argument could be identified by the WID argument.
*/
getWindowStateHandleWIDS		:: !(WindowStateHandle .pst) -> *(!WIDS,		!WindowStateHandle .pst)
getWindowStateHandleWindowMode	:: !(WindowStateHandle .pst) -> *(!WindowMode,	!WindowStateHandle .pst)
getWindowStateHandleWindowKind	:: !(WindowStateHandle .pst) -> *(!WindowKind,	!WindowStateHandle .pst)
getWindowStateHandleWindowTitle	:: !(WindowStateHandle .pst) -> *(!Title,		!WindowStateHandle .pst)
getWindowStateHandleItemNrs		:: !(WindowStateHandle .pst) -> *(![Int],		!WindowStateHandle .pst)
getWindowStateHandleKeyFocus	:: !(WindowStateHandle .pst) -> *(!*KeyFocus,	!WindowStateHandle .pst)
getWindowStateHandleWindowInfo	:: !(WindowStateHandle .pst) -> *(!WindowInfo,	!WindowStateHandle .pst)
getWindowStateHandleShow		:: !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)
getWindowStateHandleSelect		:: !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)
getWindowStateHandleActive		:: !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)
getWindowStateHandleDefaultId	:: !(WindowStateHandle .pst) -> *(!Maybe Id,	!WindowStateHandle .pst)
getWindowStateHandleCancelId	:: !(WindowStateHandle .pst) -> *(!Maybe Id,	!WindowStateHandle .pst)
getWindowStateHandleSize		:: !(WindowStateHandle .pst) -> *(!Size,		!WindowStateHandle .pst)
getWindowStateHandleClosing		:: !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)
isWindowStateHandlePlaceHolder	:: !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)
identifyWindowStateHandle :: !WID  !(WindowStateHandle .pst) -> *(!Bool,		!WindowStateHandle .pst)

setWindowStateHandleWindowTitle	:: !Title		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleItemNrs		:: ![Int]		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleKeyFocus	:: !*KeyFocus	!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleWindowInfo	:: !WindowInfo	!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleShow		:: !Bool		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSelect		:: !Bool		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleActive		:: !Bool		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleDefaultId	:: !(Maybe Id)	!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleCancelId	:: !(Maybe Id)	!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleSize		:: !Size		!(WindowStateHandle .pst) -> WindowStateHandle .pst
setWindowStateHandleClosing		:: !Bool		!(WindowStateHandle .pst) -> WindowStateHandle .pst


/*	Access operations on the margins and item space attributes of the window attributes.
	getWindow((H/V)Margin/ItemSpace)s type metrics atts
		retrieves the indicated attribute if present from the attribute list. If the attribute
		could not be found, the appropriate default value is returned. 
*/
getWindowHMargins				:: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)
getWindowVMargins				:: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)
getWindowItemSpaces				:: !WindowKind !OSWindowMetrics ![WindowAttribute .st] -> (!Int,!Int)

/*	getWindowHandlesWindows
		removes all current WindowStateHandles from its argument. These HAVE to be restored with
	setWindowHandlesWindows
		which puts the argument WindowStateHandles back into its second argument.
	getWindowHandlesActiveWindow
		returns (Just WIDS) of the active window/dialogue if found, otherwise Nothing.
	getWindowHandlesActiveModalDialog
		returns (Just WIDS) of the active modal dialogue if found, otherwise Nothing.
	hasWindowHandlesWindow searches the indicated window.
		The Boolean result is True iff the window or its placeholder could be found; otherwise it is False.
	getWindowHandlesWindow gets the indicated window.
		If the window can not be found, then False, a dummy WindowStateHandle, and unchanged WindowHandles are returned.
		If the window could be found, then True, the WindowStateHandle, and WindowHandles from which the WindowStateHandle
			is removed are returned. The location of the WindowStateHandle is memorised by means of a place holder.
	removeWindowHandlesWindow removes and gets the indicated window.
		If the window can not be found, then False, a dummy WindowStateHandle, and unchanged WindowHandles are returned.
		If the window could be found, then True, the WindowStateHandle, and WindowHandles from which the WindowStateHandle
			is removed are returned.
	setWindowHandlesWindow sets the indicated window.
		The place holder in the WindowHandles is replaced by the given WindowStateHandle. 
		Exceptions:
			* the argument WindowStateHandle is a place holder
			* the WindowHandles contain a matching non place holder entry
			* the WindowHandles contain no matching place holder entry
		In all of these cases, the function aborts.
	addBehindWindowHandlesWindow adds a new window behind the indicated window (first argument).
		If the indicated window is modal, then the new window is added behind the last modal dialogue.
		The WIDS of the actual behind (window/modal dialogue) is returned.
		Exceptions:
			* the argument WindowStateHandle is a place holder
			* the behind window could not be found.
		In all of these cases, the function aborts.
	addWindowHandlesWindow adds a new window at the indicated position.
		If index<=0 then the new window is added at the front.
		If index>#windows then the new window is added at the end.
		For all other values the new window is added behind the element at the given index.
	addWindowHandlesActiveWindow adds a new window at the active position.
		If the window is modal, then the active position is in front of all other windows.
		If the window is modeless, then the active position is immediately behind the last modal window.
*/
getWindowHandlesWindows				::									!(WindowHandles .pst) -> *(![WindowStateHandle .pst],!WindowHandles .pst)
setWindowHandlesWindows				:: ![WindowStateHandle .pst]		!(WindowHandles .pst) -> WindowHandles .pst
getWindowHandlesActiveWindow		::									!(WindowHandles .pst) -> *(!Maybe WIDS,!WindowHandles .pst)
getWindowHandlesActiveModalDialog	::									!(WindowHandles .pst) -> *(!Maybe WIDS,!WindowHandles .pst)
hasWindowHandlesWindow				:: !WID								!(WindowHandles .pst) -> *(!Bool,!WindowHandles .pst)
getWindowHandlesWindow				:: !WID								!(WindowHandles .pst) -> *(!Bool,WindowStateHandle .pst,!WindowHandles .pst)
removeWindowHandlesWindow			:: !WID								!(WindowHandles .pst) -> *(!Bool,WindowStateHandle .pst,!WindowHandles .pst)
setWindowHandlesWindow				::        !(WindowStateHandle .pst)	!(WindowHandles .pst) -> WindowHandles .pst
addBehindWindowHandlesWindow		:: !WID   !(WindowStateHandle .pst)	!(WindowHandles .pst) -> *(!WIDS,!WindowHandles .pst)
addWindowHandlesWindow				:: !Index !(WindowStateHandle .pst)	!(WindowHandles .pst) -> WindowHandles .pst
addWindowHandlesActiveWindow		::        !(WindowStateHandle .pst) !(WindowHandles .pst) -> WindowHandles .pst

/*	(dis/en)ableWindowSystem toggle the select state of the current windows of an interactive process.
	disableWindowSystem should be used before a modal window is opened,
	enableWindowSystem  should be used after  a modal window has been closed.
*/
disableWindowSystem			::					!(WindowHandles .pst) !*OSToolbox -> *(!*(!Maybe WIDS,!WindowHandles .pst),!*OSToolbox)
enableWindowSystem			:: !(Maybe WIDS)	!(WindowHandles .pst) !*OSToolbox -> *(               !WindowHandles .pst, !*OSToolbox)


/*	Checking WindowBounds:
*/
checkZeroWindowHandlesBound	:: !(WindowHandles .pst) -> *(!Bool,!WindowHandles .pst)
decreaseWindowHandlesBound	:: !(WindowHandles .pst) ->          WindowHandles .pst

// DvA
getWindowHandlesCursorInfo	::				!(WindowHandles .pst) -> *(!CursorInfo,!WindowHandles .pst)
setWindowHandlesCursorInfo	:: !CursorInfo	!(WindowHandles .pst) -> WindowHandles .pst

//	Retrieve the FocusItems of the elements that can obtain the keyboard input focus.
getWElementKeyFocusIds		:: !Bool ![WElementHandle .ls .pst] -> (!*[FocusItem],![WElementHandle .ls .pst])

//	Retrieve the OSWindowPtr of the control that has the initial input focus.
getInitActiveControl		:: !(WindowHandle .ls .pst) -> *(!OSWindowPtr,!WindowHandle .ls .pst)

//	Generate internal item numbers for those WElementHandles that have none (wItemNr==0).
genWElementItemNrs			:: ![Int] ![WElementHandle .ls .pst] -> (![Int],![WElementHandle .ls .pst])

//	Dangerous!! Handle with extreme care!!
getFinalModalLS				:: !WID !FinalModalLS -> Maybe .ls
