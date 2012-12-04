definition module wstate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	windowhandle
import	ostoolbox


/*	The WindowHandle` data type.
	This type is a subtype of the WindowHandle data type. The WindowHandle` data type takes the projection of those fields of 
	the (WindowHandle ls pst) data type that do not depend on the type variables {ls,pst}.
*/
::	WindowHandle`
	=	{	whMode`				:: WindowMode					// The window mode (Modal or Modeless)
		,	whKind`				:: WindowKind					// The window kind (Window or Dialog)
		,	whTitle`			:: Title						// The window title
		,	whItemNrs`			:: [Int]						// The list of free system item numbers for all controls
		,	whKeyFocus`			:: KeyFocus						// The item that has the keyboard input focus
		,	whWindowInfo`		:: WindowInfo					// Additional information about the Window (Nothing for Dialogs)
		,	whItems`			:: [WElementHandle`]			// The window controls
		,	whShow`				:: Bool							// The visibility of the window (True iff visible)
		,	whSelect`			:: Bool							// The WindowSelect==Able (by default True)
		,	whAtts`				:: [WindowAttribute`]			// The window attributes
		,	whDefaultId`		:: Maybe Id						// The Id of the optional default button
		,	whCancelId`			:: Maybe Id						// The Id of the optional cancel  button
		,	whSize`				:: Size							// The exact size of the window
		,	whClosing`			:: Bool							// Flag: the window is being closed (True)
		}
::	WElementHandle`
	=	WItemHandle`			WItemHandle`
	|	WRecursiveHandle`		[WElementHandle`] WRecursiveKind
::	WRecursiveKind
	=	IsWListLSHandle
	|	IsWExtendLSHandle
	|	IsWChangeLSHandle
::	WItemHandle`
	=	{	wItemId`			:: Maybe Id						// If the control has a (ControlId id) attribute, then Just id; Nothing
		,	wItemNr`			:: Int							// The internal nr of this control  (generated from whIds)
		,	wItemKind`			:: ControlKind					// The sort of control
		,	wItemShow`			:: Bool							// The visibility of the control (True iff visible)
		,	wItemSelect`		:: Bool							// The ControlSelectState==Able  (by default True)
		,	wItemInfo`			:: WItemInfo`					// Additional information of the control
		,	wItemAtts`			:: [ControlAttribute`]			// The control attributes
		,	wItems`				:: [WElementHandle`]			// In case of	CompoundControl	: its control elements
																//				Otherwise		: []
		,	wItemVirtual`		:: Bool							// The control is virtual (True) and should not be layn out
		,	wItemPos`			:: !Vector2						// The position of the item, relative to its parent (window/dialog/compound/layout)
		,	wItemSize`			:: Size							// The exact size of the item
		,	wItemPtr`			:: OSWindowPtr					// The ptr to the item (OSNoWindowPtr if no handle)
		,	wItemLayoutInfo`	:: LayoutInfo					// Additional information on layout
		}
::	WItemInfo`
	=	ButtonInfo`				ButtonInfo						// In case of	ButtonControl		: the button information
	|	CheckInfo`				CheckInfo`						// In case of	CheckControl		: the check items information
	|	CompoundInfo`			CompoundInfo					// In case of	CompoundControl		: the compound control information
	|	CustomButtonInfo`		CustomButtonInfo				// In case of	CustomButtonControl	: the custom button information
	|	CustomInfo`				CustomInfo						// In case of	CustomControl		: the custom information
	|	EditInfo`				EditInfo						// In case of	EditControl			: the edit text information
	|	PopUpInfo`				PopUpInfo`						// In case of	PopUpControl		: the pop up information
	|	RadioInfo`				RadioInfo`						// In case of	RadioControl		: the radio items information
	|	SliderInfo`				SliderInfo`						// In case of	SliderControl		: the slider information
	|	TextInfo`				TextInfo						// In case of	TextControl			: the text information
	|	NoWItemInfo`											// No additional information
::	RadioInfo`
	=	{	radioItems`			:: [RadioItemInfo`]				// The radio items and their exact position (initially zero)
		,	radioLayout`		:: RowsOrColumns				// The layout of the radio items
		,	radioIndex`			:: Int							// The currently selected radio item (1<=radioIndex<=length radioItems)
		}
::	RadioItemInfo`
	=	{	radioItem`			:: (String,Int)					// The text of the item
		,	radioItemPos`		:: !Vector2						// The exact position of the item
		,	radioItemSize`		:: Size							// The exact size of the item
		,	radioItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	CheckInfo`
	=	{	checkItems`			:: [CheckItemInfo`]				// The check items and their exact position (initially zero)
		,	checkLayout`		:: RowsOrColumns				// The layout of the check items
		}
::	CheckItemInfo`
	=	{	checkItem`			:: (String,Int,MarkState)		// The text and mark of the item
		,	checkItemPos`		:: !Vector2						// The exact position of the item
		,	checkItemSize`		:: Size							// The exact size of the item
		,	checkItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	PopUpInfo`
	=	{	popUpInfoItems`		:: [String]						// The pop up items
		,	popUpInfoIndex`		:: Index						// The currently selected pop up item (1<=popUpInfoIndex<=length popUpInfoItems)
		,	popUpInfoEdit`		:: Maybe PopUpEditInfo			// If the pop up is editable: the PopUpEditInfo, otherwise Nothing
		}
::	SliderInfo`
	=	{	sliderInfoDir`		:: Direction					// The direction of the slider
		,	sliderInfoLength`	:: Int							// The length (in pixels) of the slider
		,	sliderInfoState`	:: SliderState					// The current slider state
		}
::	WindowAttribute`
	=	WindowActivate`
	|	WindowCancel`		Id
	|	WindowClose`
	| 	WindowCursor`		CursorShape
	|	WindowDeactivate`
	|	WindowHMargin`		Int Int
	|	WindowHScroll`		ScrollFunction
	|	WindowId`			Id
	|	WindowIndex`		Int
	|	WindowInit`
	|	WindowInitActive`	Id
	|	WindowItemSpace`	Int Int
	|	WindowKeyboard`		SelectState
	|	WindowLook`			Bool Look
	|	WindowMouse`		SelectState
	|	WindowOk`			Id
	|	WindowOrigin`		Point2
	|	WindowOuterSize`	Size
	|	WindowPen`			[PenAttribute]
	|	WindowPos`			ItemPos
	|	WindowSelectState`	SelectState
	|	WindowViewDomain`	ViewDomain
	|	WindowViewSize`		Size
	|	WindowVMargin`		Int Int
	|	WindowVScroll`		ScrollFunction
::	ControlAttribute`
	=	ControlActivate`
	|	ControlDeactivate`
	|	ControlFunction`
	|	ControlHide`
	|	ControlHMargin`		Int Int
	|	ControlHScroll`		ScrollFunction
	|	ControlId`			Id
	|	ControlItemSpace`	Int Int
	|	ControlKeyboard`	SelectState
	|	ControlLook`		Bool Look
	|	ControlMinimumSize`	Size
	|	ControlModsFunction`
	|	ControlMouse`		SelectState
	|	ControlOrigin`		Point2
	|	ControlOuterSize`	Size
	|	ControlPen`			[PenAttribute]
	|	ControlPos`			ItemPos
	|	ControlResize`		ControlResizeFunction
	|	ControlSelectState`	SelectState
	|	ControlTip`			String
	|	ControlViewDomain`	ViewDomain
	|	ControlViewSize`	Size
	|	ControlVMargin`		Int Int
	|	ControlVScroll`		ScrollFunction
	|	ControlWidth`		ControlWidth


//	Retrieving the projection type from several arguments.
retrieveWindowHandle`		::				!*(WindowStateHandle  .pst) !*OSToolbox -> (!WindowHandle`,    !*WindowStateHandle   .pst, !*OSToolbox)
retrieveWindowHandle2		::				!*(WindowStateHandle  .pst) !*OSToolbox -> (!WindowHandle2,    !*WindowStateHandle   .pst, !*OSToolbox)
getWindowHandle`			:: !OSWindowPtr !*(WindowHandle   .ls .pst)	!*OSToolbox -> (!WindowHandle`,	   !*WindowHandle    .ls .pst, !*OSToolbox)
getWElementHandles`			:: !OSWindowPtr !*[WElementHandle .ls .pst]	!*OSToolbox -> (![WElementHandle`],!*[WElementHandle .ls .pst],!*OSToolbox)
getWItemHandle`				:: !OSWindowPtr !*(WItemHandle    .ls .pst)	!*OSToolbox -> (! WItemHandle`,    ! *WItemHandle    .ls .pst, !*OSToolbox)

//	Replacing the projection type to several arguments.
insertWindowHandle`			:: !WindowHandle`		!*(WindowStateHandle  .pst) -> *WindowStateHandle   .pst
insertWindowHandle2			:: !WindowHandle2 		!*(WindowStateHandle  .pst) -> *WindowStateHandle   .pst
setWindowHandle`			:: !WindowHandle`		!*(WindowHandle   .ls .pst) -> *WindowHandle    .ls .pst
setWElementHandles`			:: ![WElementHandle`]	!*[WElementHandle .ls .pst] -> *[WElementHandle .ls .pst]

::	WindowHandle2
	=	{	whWindowInfo2		:: WindowInfo					// Additional information about the Window (Nothing for Dialogs)
		,	whItems2			:: [WElementHandle2]			// The window controls
		,	whSize2				:: Size							// The exact size of the window
		}

::	WElementHandle2
	=	WItemHandle2			WItemHandle2
	|	WRecursiveHandle2		[WElementHandle2] WRecursiveKind

::	WItemHandle2
	=	{	wItemId2			:: Maybe Id						// If the control has a (ControlId id) attribute, then Just id; Nothing
		,	wItemKind2			:: ControlKind					// The sort of control
		,	wItemShow2			:: Bool							// The visibility of the control (True iff visible)
		,	wItemInfo2			:: WItemInfo`					// Additional information of the control
		,	wItems2				:: [WElementHandle2]			// In case of	CompoundControl	: its control elements
																//				Otherwise		: []
		,	wItemPos2			:: !Vector2						// The position of the item, relative to its parent (window/dialog/compound/layout)
		,	wItemSize2			:: Size							// The exact size of the item
		,	wItemPtr2			:: OSWindowPtr					// The ptr to the item (OSNoWindowPtr if no handle)
		}
