definition module StdWindowAttribute


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdWindowAttribute specifies which WindowAttributes are valid for Windows
//	and Dialogs.
//	Basic comparison operations and retrieval functions are also included.
//	********************************************************************************


import StdWindowDef


/*	The following functions specify the valid attributes for each standard window.
*/

isValidWindowAttribute	:: !(WindowAttribute .st) -> Bool
/*	Window				(y = valid, . = invalid)
	WindowActivate		y | WindowInit			y | WindowPen			y |
	WindowCancel		. | WindowInitActive	y | WindowPos			y |
	WindowClose			y | WindowItemSpace		y | WindowSelectState	y |
	WindowCursor		y | WindowKeyboard		y | WindowViewDomain	y |
	WindowDeactivate	y | WindowLook			y | WindowViewSize		y |
	WindowHMargin		y | WindowMouse			y | WindowVMargin		y |
	WindowHScroll		y | WindowOk			. | WindowVScroll		y |
	WindowId			y | WindowOrigin		y |
	WindowIndex			y | WindowOuterSize		y |
*/

isValidDialogAttribute	:: !(WindowAttribute .st) -> Bool
/*	Dialog				(y = valid, . = invalid)
	WindowActivate		y | WindowInit			y | WindowPen			. |
	WindowCancel		y | WindowInitActive	y | WindowPos			y |
	WindowClose			y | WindowItemSpace		y | WindowSelectState	. |
	WindowCursor		. | WindowKeyboard		. | WindowViewDomain	. |
	WindowDeactivate	y | WindowLook			. | WindowViewSize		y |
	WindowHMargin		y | WindowMouse			. | WindowVMargin		y |
	WindowHScroll		. | WindowOk			y | WindowVScroll		. |
	WindowId			y | WindowOrigin		. |
	WindowIndex			y | WindowOuterSize		y |
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isWindowActivate			:: !(WindowAttribute .st) -> Bool
isWindowCancel				:: !(WindowAttribute .st) -> Bool
isWindowClose				:: !(WindowAttribute .st) -> Bool
isWindowCursor				:: !(WindowAttribute .st) -> Bool
isWindowDeactivate			:: !(WindowAttribute .st) -> Bool
isWindowHMargin				:: !(WindowAttribute .st) -> Bool
isWindowHScroll				:: !(WindowAttribute .st) -> Bool
isWindowId					:: !(WindowAttribute .st) -> Bool
isWindowIndex				:: !(WindowAttribute .st) -> Bool
isWindowInit				:: !(WindowAttribute .st) -> Bool
isWindowInitActive			:: !(WindowAttribute .st) -> Bool
isWindowItemSpace			:: !(WindowAttribute .st) -> Bool
isWindowKeyboard			:: !(WindowAttribute .st) -> Bool
isWindowLook				:: !(WindowAttribute .st) -> Bool
isWindowMouse				:: !(WindowAttribute .st) -> Bool
isWindowOk					:: !(WindowAttribute .st) -> Bool
isWindowOrigin				:: !(WindowAttribute .st) -> Bool
isWindowOuterSize			:: !(WindowAttribute .st) -> Bool
isWindowPen					:: !(WindowAttribute .st) -> Bool
isWindowPos					:: !(WindowAttribute .st) -> Bool
isWindowSelectState			:: !(WindowAttribute .st) -> Bool
isWindowViewDomain			:: !(WindowAttribute .st) -> Bool
isWindowViewSize			:: !(WindowAttribute .st) -> Bool
isWindowVMargin				:: !(WindowAttribute .st) -> Bool
isWindowVScroll				:: !(WindowAttribute .st) -> Bool


/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getWindowActivateFun		:: !(WindowAttribute .st) -> IdFun .st
getWindowCancelAtt			:: !(WindowAttribute .st) -> Id
getWindowCloseFun			:: !(WindowAttribute .st) -> IdFun .st
getWindowCursorAtt			:: !(WindowAttribute .st) -> CursorShape
getWindowDeactivateFun		:: !(WindowAttribute .st) -> IdFun .st
getWindowHMarginAtt			:: !(WindowAttribute .st) -> (Int,Int)
getWindowHScrollFun			:: !(WindowAttribute .st) -> ScrollFunction
getWindowIdAtt				:: !(WindowAttribute .st) -> Id
getWindowIndexAtt			:: !(WindowAttribute .st) -> Int
getWindowInitFun			:: !(WindowAttribute .st) -> IdFun .st
getWindowInitActiveAtt		:: !(WindowAttribute .st) -> Id
getWindowItemSpaceAtt		:: !(WindowAttribute .st) -> (Int,Int)
getWindowKeyboardAtt		:: !(WindowAttribute .st) -> ( KeyboardStateFilter
														 , SelectState
														 , KeyboardFunction .st
														 )
getWindowLookAtt			:: !(WindowAttribute .st) -> (Bool,Look)
getWindowMouseAtt			:: !(WindowAttribute .st) -> ( MouseStateFilter
														 , SelectState
														 , MouseFunction .st
														 )
getWindowOkAtt				:: !(WindowAttribute .st) -> Id
getWindowOriginAtt			:: !(WindowAttribute .st) -> Point2
getWindowOuterSizeAtt		:: !(WindowAttribute .st) -> Size
getWindowPenAtt				:: !(WindowAttribute .st) -> [PenAttribute]
getWindowPosAtt				:: !(WindowAttribute .st) -> ItemPos
getWindowSelectStateAtt		:: !(WindowAttribute .st) -> SelectState
getWindowViewDomainAtt		:: !(WindowAttribute .st) -> ViewDomain
getWindowViewSizeAtt		:: !(WindowAttribute .st) -> Size
getWindowVMarginAtt			:: !(WindowAttribute .st) -> (Int,Int)
getWindowVScrollFun			:: !(WindowAttribute .st) -> ScrollFunction
