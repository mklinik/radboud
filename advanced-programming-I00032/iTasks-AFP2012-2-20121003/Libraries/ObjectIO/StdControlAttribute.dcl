definition module StdControlAttribute


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdControlAttribute specifies which ControlAttributes are valid for each of the
//	standard controls.
//	Basic comparison operations and retrieval functions are also included.
//	********************************************************************************


import StdControlDef


/*	The following functions specify the valid attributes for each standard control.
*/

isValidButtonControlAttribute :: !(ControlAttribute .st) -> Bool
/*	ButtonControl		(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		y |
	ControlFunction		y |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	y |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		y |
	ControlId			y | ControlOuterSize	y |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. | ControlVScroll		. |
												  | ControlWidth		y |
*/

isValidCheckControlAttribute :: !(ControlAttribute .st) -> Bool
/*	CheckControl		(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		. |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. |	ControlVScroll		. |
												  | ControlWidth		. |
*/

isValidCompoundControlAttribute :: !(ControlAttribute .st) -> Bool
/*	CompoundControl		(y = valid, . = invalid)
	ControlActivate		y |	ControlKeyboard		y |	ControlPos			y |
	ControlDeactivate	y |	ControlLook			y |	ControlResize		y |
	ControlFunction		. |	ControlMinimumSize	y |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		y |	ControlMouse		y |	ControlViewDomain	y |
	ControlHScroll		y |	ControlOrigin		y |	ControlViewSize		y |
	ControlId			y | ControlOuterSize	y |	ControlVMargin		y |
	ControlItemSpace	y |	ControlPen			y |	ControlVScroll		y |
												  | ControlWidth		. |
*/

isValidCustomButtonControlAttribute :: !(ControlAttribute .st) -> Bool
/*	CustomButtonControl	(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		y |
	ControlFunction		y |	ControlMinimumSize	y |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	y |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			y |	ControlVScroll		. |
												  | ControlWidth		. |
*/

isValidCustomControlAttribute :: !(ControlAttribute .st) -> Bool
/*	CustomControl		(y = valid, . = invalid)
	ControlActivate		y |	ControlKeyboard		y |	ControlPos			y |
	ControlDeactivate	y |	ControlLook			. |	ControlResize		y |
	ControlFunction		. |	ControlMinimumSize	y |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		y |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			y |	ControlVScroll		. |
												  | ControlWidth		. |
*/

isValidEditControlAttribute :: !(ControlAttribute .st) -> Bool
/*	EditControl			(y = valid, . = invalid)
	ControlActivate		y |	ControlKeyboard		y |	ControlPos			y |
	ControlDeactivate	y |	ControlLook			. |	ControlResize		y |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. | ControlVScroll		. |
												  | ControlWidth		. |
*/

isValidLayoutControlAttribute :: !(ControlAttribute .st) -> Bool
/*	LayoutControl		(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		y |
	ControlFunction		. |	ControlMinimumSize	y |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			. |
	ControlHMargin		y |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		y |
	ControlId			y | ControlOuterSize	y |	ControlVMargin		y |
	ControlItemSpace	y |	ControlPen			. |	ControlVScroll		. | 
												  | ControlWidth		. |
*/

isValidPopUpControlAttribute :: !(ControlAttribute .st) -> Bool
/*	PopUpControl		(y = valid, . = invalid)
	ControlActivate		y |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	y |	ControlLook			. |	ControlResize		. |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. |	ControlVScroll		. | 
												  | ControlWidth		y |
*/

isValidRadioControlAttribute :: !(ControlAttribute .st) -> Bool
/*	RadioControl		(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		. |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. |	ControlVScroll		. | 
												  | ControlWidth		. |
*/

isValidSliderControlAttribute :: !(ControlAttribute .st) -> Bool
/*	SliderControl		(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		y |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	y |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. |	ControlVScroll		. |
												  | ControlWidth		. |
*/

isValidTextControlAttribute :: !(ControlAttribute .st) -> Bool
/*	TextControl			(y = valid, . = invalid)
	ControlActivate		. |	ControlKeyboard		. |	ControlPos			y |
	ControlDeactivate	. |	ControlLook			. |	ControlResize		. |
	ControlFunction		. |	ControlMinimumSize	. |	ControlSelectState	. |
	ControlHide			y |	ControlModsFunction	. |	ControlTip			y |
	ControlHMargin		. |	ControlMouse		. |	ControlViewDomain	. |
	ControlHScroll		. |	ControlOrigin		. |	ControlViewSize		. |
	ControlId			y | ControlOuterSize	. |	ControlVMargin		. |
	ControlItemSpace	. |	ControlPen			. |	ControlVScroll		. |
												  | ControlWidth		y |
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isControlActivate		:: !(ControlAttribute .st) -> Bool
isControlDeactivate		:: !(ControlAttribute .st) -> Bool
isControlFunction		:: !(ControlAttribute .st) -> Bool
isControlHide			:: !(ControlAttribute .st) -> Bool
isControlHMargin		:: !(ControlAttribute .st) -> Bool
isControlHScroll		:: !(ControlAttribute .st) -> Bool
isControlId				:: !(ControlAttribute .st) -> Bool
isControlItemSpace		:: !(ControlAttribute .st) -> Bool
isControlKeyboard		:: !(ControlAttribute .st) -> Bool
isControlLook			:: !(ControlAttribute .st) -> Bool
isControlMinimumSize	:: !(ControlAttribute .st) -> Bool
isControlModsFunction	:: !(ControlAttribute .st) -> Bool
isControlMouse			:: !(ControlAttribute .st) -> Bool
isControlOrigin			:: !(ControlAttribute .st) -> Bool
isControlOuterSize		:: !(ControlAttribute .st) -> Bool
isControlPen			:: !(ControlAttribute .st) -> Bool
isControlPos			:: !(ControlAttribute .st) -> Bool
isControlResize			:: !(ControlAttribute .st) -> Bool
isControlSelectState	:: !(ControlAttribute .st) -> Bool
isControlTip			:: !(ControlAttribute .st) -> Bool
isControlViewDomain		:: !(ControlAttribute .st) -> Bool
isControlViewSize		:: !(ControlAttribute .st) -> Bool
isControlVMargin		:: !(ControlAttribute .st) -> Bool
isControlVScroll		:: !(ControlAttribute .st) -> Bool
isControlWidth			:: !(ControlAttribute .st) -> Bool


/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getControlActivateFun	:: !(ControlAttribute .st) -> IdFun .st
getControlDeactivateFun	:: !(ControlAttribute .st) -> IdFun .st
getControlFun			:: !(ControlAttribute .st) -> IdFun .st
getControlHMarginAtt	:: !(ControlAttribute .st) -> (Int,Int)
getControlHScrollFun	:: !(ControlAttribute .st) -> ScrollFunction
getControlIdAtt			:: !(ControlAttribute .st) -> Id
getControlItemSpaceAtt	:: !(ControlAttribute .st) -> (Int,Int)
getControlKeyboardAtt	:: !(ControlAttribute .st) -> ( KeyboardStateFilter
													  , SelectState
													  , KeyboardFunction .st
													  )
getControlLookAtt		:: !(ControlAttribute .st) -> (Bool,Look)
getControlMinimumSizeAtt:: !(ControlAttribute .st) -> Size
getControlModsFun		:: !(ControlAttribute .st) -> ModifiersFunction .st
getControlMouseAtt		:: !(ControlAttribute .st) -> ( MouseStateFilter
													  , SelectState
													  , MouseFunction .st
													  )
getControlOriginAtt		:: !(ControlAttribute .st) -> Point2
getControlOuterSizeAtt	:: !(ControlAttribute .st) -> Size
getControlPenAtt		:: !(ControlAttribute .st) -> [PenAttribute]
getControlPosAtt		:: !(ControlAttribute .st) -> ItemPos
getControlResizeFun		:: !(ControlAttribute .st) -> ControlResizeFunction
getControlSelectStateAtt:: !(ControlAttribute .st) -> SelectState
getControlTipAtt		:: !(ControlAttribute .st) -> String
getControlViewDomainAtt	:: !(ControlAttribute .st) -> ViewDomain
getControlViewSizeAtt	:: !(ControlAttribute .st) -> Size
getControlVMarginAtt	:: !(ControlAttribute .st) -> (Int,Int)
getControlVScrollFun	:: !(ControlAttribute .st) -> ScrollFunction
getControlWidthAtt		:: !(ControlAttribute .st) -> ControlWidth
