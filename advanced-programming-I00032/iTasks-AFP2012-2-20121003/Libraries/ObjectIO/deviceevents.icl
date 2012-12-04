implementation module deviceevents

import	osevent, ospicture, ostypes
import	receivermessage, timertable
from	windowhandle	import :: WIDS
from	receiverhandle	import :: InetEvent`, :: EndpointRef`, :: InetReceiverCategory`


::	MsgEvent
	=	QASyncMessage		!QASyncMessage
	|	ASyncMessage		!ASyncMessage
	|	SyncMessage			!SyncMessage

::	SchedulerEvent
	=	ScheduleOSEvent		!OSEvent ![Int]
	|	ScheduleMsgEvent	!MsgEvent
	|	ScheduleTimerEvent	!TimerEvent

::	DeviceEvent
 //	Menu events:
	=	MenuTraceEvent			!MenuTraceInfo				// Menu item has been selected
	|	ToolbarSelection		!ToolbarSelectInfo			// Toolbar item has been selected
 //	Receiver events:
	|	ReceiverEvent			!MsgEvent					// A (bi/uni)directional (a)synchronous message event
	|	InternetEvent !(!InetEvent`, !EndpointRef`, !InetReceiverCategory`, !Int)	// MW11++
 //	Timer events:
	|	TimerEvent				!TimerEvent					// A timer event
 //	Window/Dialog events:
	|	CompoundScrollAction	!CompoundScrollActionInfo	// Scrolling should occur in a compound control
	|	ControlGetKeyFocus		!ControlKeyFocusInfo		// Control has obtained keyboard focus
	|	ControlKeyboardAction	!ControlKeyboardActionInfo	// Keyboard action in a control
	|	ControlLooseKeyFocus	!ControlKeyFocusInfo		// Control has lost keyboard focus
	|	ControlMouseAction		!ControlMouseActionInfo		// Mouse action in a control
	|	ControlSelection		!ControlSelectInfo			// Control has been selected
	|	ControlSliderAction		!ControlSliderInfo			// Slider control has been selected
	|	WindowActivation		!WIDS						// Window with id has been made activate
	|	WindowCANCEL			!WIDS						// The Cancel button has been pressed
	|	WindowDeactivation		!WIDS						// Window with id has been made inactivate
	|	WindowInitialise		!WIDS						// Window with id can evaluate its initialisation action
	|	WindowKeyboardAction	!WindowKeyboardActionInfo	// Keyboard action in a window
	|	WindowMouseAction		!WindowMouseActionInfo		// Mouse action in a window
	|	WindowOK				!WIDS						// The Ok button has been pressed
	|	WindowRequestClose		!WIDS						// Window with id should be closed
	|	WindowScrollAction		!WindowScrollActionInfo		// Scrolling should occur in a window
	|	WindowSizeAction		!WindowSizeActionInfo		// Window has obtained a new size
	|	WindowUpdate			!UpdateInfo					// Window and its controls should be updated
 //	Process events:
	|	ProcessRequestClose									// The process should be closed
	|	ProcessRequestOpenFiles	!OpenFilesInfo				// The process should open files
	|	ProcessRequestClipboardChanged
::	MenuTraceInfo
	=	{	mtId				:: !Id						// The Id of the menu that contains the menu item
		,	mtParents			:: ![Int]					// The submenus starting from mtId that contain the menu item (zero based index)
		,	mtItemNr			:: !Int						// The menu item that has been selected (zero based index)
		,	mtModifiers			:: !Modifiers				// The modifiers that were pressed at the moment of selection
		}
::	ToolbarSelectInfo
	=	{	tbsItemNr			:: !Int						// The item nr of the selected toolbar item
		}
::	CompoundScrollActionInfo
	=	{	csaWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the compound control
		,	csaItemNr			:: !Int						// The wItemNr  of the compound control
		,	csaItemPtr			:: !OSWindowPtr				// The wItemPtr of the compound control
		,	csaSliderMove		:: !SliderMove				// The user action on the compound control
		,	csaDirection		:: !Direction				// The direction of the scrollbar that is being selected
		}
::	ControlKeyFocusInfo
	=	{	ckfWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the control
		,	ckfItemNr			:: !Int						// The wItemNr  of the control
		,	ckfItemPtr			:: !OSWindowPtr				// The wItemPtr of the control
		}
::	ControlKeyboardActionInfo
	=	{	ckWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the control
		,	ckItemNr			:: !Int						// The wItemNr  of the control
		,	ckItemPtr			:: !OSWindowPtr				// The wItemPtr of the control
		,	ckKeyboardState		:: !KeyboardState			// The KeyboardState of the action
		}
::	ControlMouseActionInfo
	=	{	cmWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the control
		,	cmItemNr			:: !Int						// The wItemNr  of the control
		,	cmItemPtr			:: !OSWindowPtr				// The wItemPtr of the control
		,	cmMouseState		:: !MouseState				// The MouseState of the action
		}
::	ControlSelectInfo
	=	{	csWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the control
		,	csItemNr			:: !Int						// The wItemNr  of the selected control
		,	csItemPtr			:: !OSWindowPtr				// The wItemPtr of the selected control
		,	csMoreData			:: !Int						// Additional data (index in case of PopUpControls; otherwise zero)
		,	csModifiers			:: !Modifiers				// The modifiers that were active when the control was selected
		}
::	ControlSliderInfo
	=	{	cslWIDS				:: !WIDS					// The Id/Ptr of the window/dialogue that contains the slider
		,	cslItemNr			:: !Int						// The wItemNr  of the selected slider
		,	cslItemPtr			:: !OSWindowPtr				// The wItemPtr of the selected slider
		,	cslSliderMove		:: !SliderMove				// The user action on the slider
		}
::	WindowKeyboardActionInfo
	=	{	wkWIDS				:: !WIDS					// The Id/Ptr of the window
		,	wkKeyboardState		:: !KeyboardState			// The KeyboardState of the action
		}
::	WindowMouseActionInfo
	=	{	wmWIDS				:: !WIDS					// The Id/Ptr of the window
		,	wmMouseState		:: !MouseState				// The MouseState of the action
		}
::	WindowScrollActionInfo
	=	{	wsaWIDS				:: !WIDS					// The Id/Ptr of the window
		,	wsaSliderMove		:: !SliderMove				// The user action on the window
		,	wsaDirection		:: !Direction				// The direction of the scrollbar that is being selected
		}
::	WindowSizeActionInfo
	=	{	wsWIDS				:: !WIDS					// The Id/Ptr of the window
		,	wsSize				:: !Size					// The new size of the window (including scrollbars)
		,	wsUpdateAll			:: !Bool					// The complete content of the window must be redrawn
		}
::	UpdateInfo
	=	{	updWIDS				:: !WIDS					// The Id of the window/dialogue to be updated
		,	updWindowArea		:: !OSRect					// The area of the window/dialogue to be updated (case zero, no update)
		,	updControls			:: ![ControlUpdateInfo]		// For each control to be updated: its item nr and area (in window coordinates)
		,	updGContext			:: !Maybe OSPictContext		// The graphics context to be used
		}
::	ControlUpdateInfo
	=	{	cuItemNr			:: !Int						// The wItemNr of the control
		,	cuItemPtr			:: !OSWindowPtr				// The wItemPtr to the control (can be OSNoWindowPtr)
		,	cuArea				:: !OSRect					// The update area of the control (in window coordinates)
		}
::	OpenFilesInfo
	:==	[String]											// The names of the files to be opened


//	Basic access functions on events:
getMsgEventRecLoc :: !MsgEvent -> RecLoc
getMsgEventRecLoc (QASyncMessage {qasmRecLoc})	= qasmRecLoc
getMsgEventRecLoc (ASyncMessage   {asmRecLoc})	=  asmRecLoc
getMsgEventRecLoc (SyncMessage     {smRecLoc})	=   smRecLoc
