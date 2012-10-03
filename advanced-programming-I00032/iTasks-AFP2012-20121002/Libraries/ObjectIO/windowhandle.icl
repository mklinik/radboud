implementation module windowhandle


import	StdBool, StdInt
import	StdControlDef, StdMaybe, StdWindowDef
import	commondef, keyfocus, receiverhandle
import	ospicture, ostypes

//import windowcursor
::	CursorInfo
	=	{	cInfoChanged	:: !Bool							// True if cLocalRgn or cMouseWasInRgn has changed
		,	cLocalRgn		:: !OSRgnHandle					// Background region of active window
		,	cMouseWasInRgn	:: !Bool							// Previous mouse was in background region
		,	cLocalShape		:: !CursorShape					// Cursor shape of active window
		,	cGlobalSet		:: !Bool							// Global cursor is set
		,	cGlobalShape	:: !CursorShape					// Global cursor shape
		}

::	*ControlState ls pst										// The internal implementation of a control
	:==	WElementHandle ls pst									// is a WElementHandle

::	*WindowHandles pst											// Windows currently are only dialogs
	=	{	whsWindows		:: *[*WindowStateHandle pst]		// The windows and their controls of a process
		,	whsCursorInfo	:: !CursorInfo						// The global cursor information
		,	whsNrWindowBound:: !Bound							// The maximum number of windows that are allowed to be opened
		,	whsModal		:: !Bool							// Flag: the window system is modal (used in combination with modal dialogues)
		,	whsFinalModalLS	:: !*[FinalModalLS]					// The final local states of terminated modal dialogs
		}
::	*FinalModalLS
	=	E. .ls:
		{	fmWIDS			:: !WIDS							// Its identification
		,	fmLS			:: ls								// The final local state
		}
::	*WindowStateHandle pst
	=	E. .ls:
		{	wshIds			:: !WIDS							// A window is identified by an Id and an OSWindowPtr
		,	wshHandle		:: *Maybe *(WindowLSHandle ls pst)	// If used as placeholder, Nothing; otherwise window with local state
		}
::	WIDS
	=	{	wId				:: Id								// Id  of window
		,	wPtr			:: !OSWindowPtr						// Ptr of window
		,	wActive			:: !Bool							// The window is the active window (True) or not (False)
		}
::	*WindowLSHandle ls pst
	=	{	wlsState		:: ls								// The local state of this window
		,	wlsHandle		:: *WindowHandle ls pst				// The window implementation
		}
::	*WindowHandle ls pst
	=	{	whMode			:: !WindowMode						// The window mode (Modal or Modeless)
		,	whKind			:: !WindowKind						// The window kind (Window or Dialog)
		,	whTitle			:: !Title							// The window title
		,	whItemNrs		:: [Int]							// The list of free system item numbers for all controls
		,	whKeyFocus		:: !*KeyFocus						// The item that has the keyboard input focus
		,	whWindowInfo	:: !WindowInfo						// Additional information about the window
		,	whItems			:: *[*WElementHandle ls pst]		// The window controls
		,	whShow			:: !Bool							// The visibility of the window (True iff visible)
		,	whSelect		:: !Bool							// The WindowSelectState==Able (by default True)
		,	whAtts			:: ![WindowAttribute *(ls,pst)]		// The window attributes
		,	whDefaultId		:: !Maybe Id						// The Id of the optional default button
		,	whCancelId		:: !Maybe Id						// The Id of the optional cancel  button
		,	whSize			:: !Size							// The exact size of the window
		,	whClosing		:: !Bool							// Flag: the window is being closed (True)
		}
::	WindowMode													// Modality of the window
	=	Modal													// Modal window (only for dialogs)
	|	Modeless												// Modeless window
::	WindowKind
	=	IsWindow												// Window kind
	|	IsDialog												// Dialog kind
	|	IsGameWindow											// Game window kind
::	LookInfo
	=	{	lookFun			:: !Look							// The Look function
		,	lookPen			:: !Pen								// The settings of the Pen
		,	lookSysUpdate	:: !Bool							// The system handles updates as much as possible
		}
::	WindowInfo
	=	WindowInfo			!WindowData
	|	GameWindowInfo		!GameWindowData
	|	NoWindowInfo
::	WindowData
	=	{	windowDomain	:: !OSRect							// The optional view domain of the window
		,	windowOrigin	:: !Point2							// The Origin of the view domain
		,	windowHScroll	:: !Maybe ScrollInfo				// The scroll data of the WindowHScroll attribute
		,	windowVScroll	:: !Maybe ScrollInfo				// The scroll data of the WindowVScroll attribute
		,	windowLook		:: !LookInfo						// The look and pen of the window
		,	windowClip		:: !ClipState						// The clipped elements of the window
		}
::  GameWindowData
    =   {   gamewindowDDPtr :: !DDPtr							// The handle to the game window
    	,	gamewindowSize	:: !Size							// The size of the game window
    	,	gamewindowCDepth:: !Int								// The colour depth
    	,	gamewindowFullScreen
    						:: !Bool							// Flag: the game is played full screen (True)
        }
::  DDPtr
    :== OSWindowPtr
::	ScrollInfo
	=	{	scrollFunction	:: !ScrollFunction					// The ScrollFunction of the (horizontal/vertical) scroll attribute
		,	scrollItemPos	:: !Point2							// The exact position of the scrollbar
		,	scrollItemSize	:: !Size							// The exact size of the scrollbar
		,	scrollItemPtr	:: !OSWindowPtr						// The OSWindowPtr of the scrollbar
		}
::	ClipState
	=	{	clipRgn			:: !OSRgnHandle						// The clipping region
		,	clipOk			:: !Bool							// Flag: the clipping region is valid
		}
::	*WElementHandle	ls	pst
	=	WItemHandle			*(WItemHandle		ls pst)
	|	WListLSHandle		*[WElementHandle	ls pst]
	|	WExtendLSHandle		*(WExtendLSHandle	ls pst)
	|	WChangeLSHandle		*(WChangeLSHandle	ls pst)
::	*WExtendLSHandle ls pst
	=	E. .ls1:
		{	wExtendLS		:: ls1
		,	wExtendItems	:: *[WElementHandle *(ls1,ls) pst]
		}
::	*WChangeLSHandle ls pst
	=	E. .ls1:
		{	wChangeLS		:: ls1
		,	wChangeItems	:: *[WElementHandle ls1 pst]
		}
::	*WItemHandle ls pst
	=	{	wItemId			:: Maybe Id							// If the control has a (ControlId id) attribute, then Just id; Nothing
		,	wItemNr			:: Int								// The internal nr of this control  (generated from whItemNrs)
		,	wItemKind		:: ControlKind						// The sort of control
		,	wItemShow		:: Bool								// The visibility of the control (True iff visible)
		,	wItemSelect		:: Bool								// The ControlSelectState==Able  (by default True)
		,	wItemInfo		:: WItemInfo ls pst					// Additional information of the control
		,	wItemAtts		:: [ControlAttribute *(ls,pst)]		// The control attributes
		,	wItems			:: *[WElementHandle ls pst]			// In case of	CompoundControl	: its control elements
																//				Otherwise		: []
		,	wItemVirtual	:: Bool								// The control is virtual (True) and should not be layn out
		,	wItemPos		:: Vector2							// The position of the item, relative to its parent (window/dialog/compound/layout)
		,	wItemSize		:: Size								// The exact size of the item
		,	wItemPtr		:: OSWindowPtr						// The ptr to the item (OSNoWindowPtr if no handle)
		,	wItemLayoutInfo	:: LayoutInfo						// Additional information on layout
		}
::	LayoutInfo													// The layout attribute of the layout root control is:
	=	LayoutFix												// ItemPos    = Fix
	|	LayoutFun ParentIndex OffsetFun							// ItemOffset = OffsetFun
	|	LayoutFrame												// any other attribute
::	WItemInfo		ls pst
	=	ButtonInfo			ButtonInfo							// In case of	ButtonControl	: the button information
	|	CheckInfo			(CheckInfo	  *(ls,pst))			// In case of	CheckControl	: the check items information
	|	CompoundInfo		CompoundInfo						// In case of	CompoundControl	: the compound control information
	|	CustomButtonInfo	CustomButtonInfo					// In case of	CustomButtonControl	: the custom button information
	|	CustomInfo			CustomInfo							// In case of	CustomControl		: the custom information
	|	EditInfo			EditInfo							// In case of	EditControl		: the edit text information
	|	PopUpInfo			(PopUpInfo	  *(ls,pst))			// In case of	PopUpControl	: the pop up information
	|	RadioInfo			(RadioInfo	  *(ls,pst))			// In case of	RadioControl	: the radio items information
	|	ReceiverInfo		(ReceiverHandle	ls pst)				// In case of	ReceiverControl	: the receiver information
	|	SliderInfo			(SliderInfo	  *(ls,pst))			// In case of	SliderControl	: the slider information
	|	TextInfo			TextInfo							// In case of	TextControl		: the text information
	|	NoWItemInfo												// No additional information
::	RadioInfo		st
	=	{	radioItems		:: [RadioItemInfo st]				// The radio items and their exact position (initially zero)
		,	radioLayout		:: RowsOrColumns					// The layout of the radio items
		,	radioIndex		:: Int								// The currently selected radio item (1<=radioIndex<=length radioItems)
		}
::	RadioItemInfo	st
	=	{	radioItem		:: (String,Int,IdFun st)			// The RadioItem of the definition
		,	radioItemPos	:: !Vector2							// The exact position of the item
		,	radioItemSize	:: Size								// The exact size of the item
		,	radioItemPtr	:: OSWindowPtr						// The OSWindowPtr of the item
		}
::	CheckInfo		st
	=	{	checkItems		:: [CheckItemInfo st]				// The check items and their exact position (initially zero)
		,	checkLayout		:: RowsOrColumns					// The layout of the check items
		}
::	CheckItemInfo	st
	=	{	checkItem		:: (String,Int,MarkState,IdFun st)	// The CheckItem of the definition
		,	checkItemPos	:: !Vector2							// The exact position of the item
		,	checkItemSize	:: Size								// The exact size of the item
		,	checkItemPtr	:: OSWindowPtr						// The OSWindowPtr of the item
		}
::	PopUpInfo		st
	=	{	popUpInfoItems	:: [PopUpControlItem st]			// The pop up items
		,	popUpInfoIndex	:: Index							// The currently selected pop up item (1<=popUpInfoIndex<=length popUpInfoItems)
		,	popUpInfoEdit	:: Maybe PopUpEditInfo				// If the pop up is editable: the PopUpEditInfo, otherwise Nothing
		}
::	PopUpEditInfo
	=	{	popUpEditText	:: String							// The current content of the editable pop up
		,	popUpEditPtr	:: OSWindowPtr						// The OSWindowPtr of the editable pop up
		}
::	SliderInfo		st
	=	{	sliderInfoDir	:: Direction						// The direction of the slider
		,	sliderInfoLength:: Int								// The length (in pixels) of the slider
		,	sliderInfoState	:: SliderState						// The current slider state
		,	sliderInfoAction:: SliderAction st					// The action of the slider
		}
::	TextInfo
	=	{	textInfoText	:: !String							// The content of the text control
		}
::	EditInfo
	=	{	editInfoText	:: !String							// The content of the edit control
		,	editInfoWidth	:: Int								// The width (in pixels) of the edit item
		,	editInfoNrLines	:: NrLines							// The nr of complete visible lines of the edit item
		}
::	ButtonInfo
	=	{	buttonInfoText	:: !String							// The title of the button control
		}
::	CustomButtonInfo
	=	{	cButtonInfoLook	:: LookInfo							// The look of the custom button control
		}
::	CustomInfo
	=	{	customInfoLook	:: LookInfo							// The look of the custom control
		}
::	CompoundInfo
	=	{	compoundDomain	:: OSRect							// The optional view domain of the compound control
		,	compoundOrigin	:: Point2							// The Origin of the view domain
		,	compoundHScroll	:: Maybe ScrollInfo					// The scroll data of the ControlHScroll attribute
		,	compoundVScroll	:: Maybe ScrollInfo					// The scroll data of the ControlVScroll attribute
		,	compoundLookInfo:: CompoundLookInfo					// The look information of the compound control
		}
::	CompoundLookInfo
	=	{	compoundLook	:: LookInfo							// The look of the compound control
		,	compoundClip	:: ClipState						// The clipped elements of the compound control
		}
::	ControlKind
	=	IsButtonControl
	|	IsCheckControl
	|	IsCompoundControl
	|	IsCustomButtonControl
	|	IsCustomControl
	|	IsEditControl
	|	IsLayoutControl
	|	IsPopUpControl
	|	IsRadioControl
	|	IsSliderControl
	|	IsTextControl
	|	IsOtherControl ControlType								// Of other controls the ControlType


//	Equalities on simple types:

instance == WIDS where
	(==) wids wids` = wids.wPtr==wids`.wPtr && wids.wId==wids`.wId
instance == WindowMode where
	(==) Modal		mode = case mode of
								Modal		-> True
								_			-> False
	(==) Modeless	mode = case mode of
								Modeless	-> True
								_			-> False
instance == WindowKind where
	(==) IsWindow		kind = case kind of
								IsWindow		-> True
								_				-> False
	(==) IsDialog		kind = case kind of
								IsDialog		-> True
								_				-> False
	(==) IsGameWindow	kind = case kind of
								IsGameWindow	-> True
								_				-> False
instance == LayoutInfo where
	(==) LayoutFix       info = case info of
								LayoutFix		-> True
								_				-> False
	(==) (LayoutFun i _) info = case info of
								LayoutFun j _	-> i==j
								_				-> False
	(==) LayoutFrame     info = case info of
								LayoutFrame		-> True
								_				-> False
instance == ControlKind where
	(==) IsButtonControl		kind = case kind of
											IsButtonControl			-> True
											_						-> False
	(==) IsCheckControl			kind = case kind of
											IsCheckControl			-> True
											_						-> False
	(==) IsCompoundControl		kind = case kind of
											IsCompoundControl		-> True
											_						-> False
	(==) IsCustomButtonControl	kind = case kind of
											IsCustomButtonControl	-> True
											_						-> False
	(==) IsCustomControl		kind = case kind of
											IsCustomControl			-> True
											_						-> False
	(==) IsEditControl			kind = case kind of
											IsEditControl			-> True
											_						-> False
	(==) IsLayoutControl			kind = case kind of
											IsLayoutControl			-> True
											_						-> False
	(==) IsPopUpControl			kind = case kind of
											IsPopUpControl			-> True
											_						-> False
	(==) IsRadioControl			kind = case kind of
											IsRadioControl			-> True
											_						-> False
	(==) IsSliderControl		kind = case kind of
											IsSliderControl			-> True
											_						-> False
	(==) IsTextControl			kind = case kind of
											IsTextControl			-> True
											_						-> False
	(==) (IsOtherControl type1)	kind = case kind of
											IsOtherControl type2	-> type1==type2
											_						-> False
instance toString ControlKind where
	toString IsButtonControl			= "IsButtonControl"
	toString IsCheckControl				= "IsCheckControl"
	toString IsCompoundControl			= "IsCompoundControl"
	toString IsCustomButtonControl		= "IsCustomButtonControl"
	toString IsCustomControl			= "IsCustomControl"
	toString IsEditControl				= "IsEditControl"
	toString IsLayoutControl			= "IsLayoutControl"
	toString IsPopUpControl				= "IsPopUpControl"
	toString IsRadioControl				= "IsRadioControl"
	toString IsSliderControl			= "IsSliderControl"
	toString IsTextControl				= "IsTextControl"
	toString (IsOtherControl type)		= "(IsOtherControl "+++type+++")"

/*	The given ControlKind corresponds with a custom-drawn control.
*/
isCustomisedControl :: !ControlKind -> Bool
isCustomisedControl IsCustomButtonControl	= True
isCustomisedControl IsCustomControl			= True
isCustomisedControl _						= False

/*	The given ControlKind corresponds with a control that contains other controls (CompoundControl).
*/
isRecursiveControl :: !ControlKind -> Bool
isRecursiveControl IsCompoundControl	= True
isRecursiveControl IsLayoutControl		= True
isRecursiveControl _					= False


//	Conversion functions from ControlState to WElementHandle, and vice versa:

wElementHandleToControlState :: !*(WElementHandle .ls .pst) -> *ControlState .ls .pst
wElementHandleToControlState wH = wH

controlStateToWElementHandle :: !*(ControlState .ls .pst) -> *WElementHandle .ls .pst
controlStateToWElementHandle wH = wH

