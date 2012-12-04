definition module iostate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	osactivaterequests, osdocumentinterface, osevent, osguishare, ossystem, ostime, ostypes
import	devicefunctions, devicesystemstate
from	processstack		import :: ProcessStack, :: ProcessKind(..), :: ProcessShowState
from	receivertable		import :: ReceiverTable
from	roundrobin			import :: RR
from	timertable			import :: TimerTable


::	*IOSt l
::	*PSt  l
	=	{	ls			:: !l						// The local (and private) data of the process
		,	io			:: !*IOSt l					// The IOSt environment of the process
		}

::	*CProcesses										// The 'context-free' processes administration
	:==	RR *CProcess								//	is a round-robin
::	*CProcess										// The context-free process
	=	E. .l:
		{	localState	:: !*Maybe l				//	its local state
		,	localIOSt	:: !*IOSt l					//	its context-free IOSt
		}

::	RuntimeState
	=	Running										// The process is running
	|	Blocked !SystemId							// The process is blocked for the process with given id
	|	Closed										// The process is closed
::	GUIShare
	:==	OSGUIShare

::	ActivateRequests	:== [OSActivateRequest]
::	DoubleDownDist		:== Int
::	InputTrack										// Input being tracked:
	=	{	itWindow	:: !OSWindowPtr				// the parent window
		,	itControl	:: !Int						// zero if parent window, otherwise item nr of control (>0)
		,	itKind		:: !InputTrackKind			// the input kinds being tracked
		}
::	InputTrackKind									// Input source kinds:
	=	{	itkMouse	:: !Bool					// mouse
		,	itkKeyboard	:: !Bool					// keyboard
		,	itkChar		:: !Int						// key that is being tracked
		,	itkSlider	:: !Maybe SliderTrackInfo	// extra slider tracking info
		}
::	SliderTrackInfo									// Slider being tracked:
	=	{	stiControl	:: !OSWindowPtr				// control being tracked (OSNoWindowPtr if window)
		,	stiPart		:: !Int						// part of slider being tracked
		,	stiHilite	:: !Bool					// currently highlighted
		,	stiDirection:: !Direction				// slider direction
		,	stiIsControl:: !Bool					// is slider control
		}
::	ClipboardState
	=	{	cbsCount	:: !Int						// ScrapCount of last access
		}


//	Access-rules on the IOSt:

emptyIOSt					:: !SystemId !(Maybe SystemId) !(Maybe GUIShare) !DocumentInterface !ProcessKind 
								![ProcessAttribute (PSt .l)] !*(IdFun (PSt .l)) !(Maybe SystemId)
							-> IOSt .l

ioStButtonFreq				:: !Int !Point2 !OSWindowPtr	!(IOSt .l) -> (!Int,!IOSt .l)
ioStSetDoubleDownDist		:: !DoubleDownDist				!(IOSt .l) -> IOSt .l
ioStGetInputTrack			:: !(IOSt .l) -> (!Maybe InputTrack,			!IOSt .l)
ioStGetProcessAttributes	:: !(IOSt .l) -> (![ProcessAttribute (PSt .l)],	!IOSt .l)
ioStGetInitIO				:: !(IOSt .l) -> (!*IdFun (PSt .l),				!IOSt .l)
ioStClosed					:: !(IOSt .l) -> (!Bool,						!IOSt .l)
ioStGetRuntimeState			:: !(IOSt .l) -> (!RuntimeState,				!IOSt .l)
ioStGetIOIsModal			:: !(IOSt .l) -> (!Maybe SystemId,				!IOSt .l)
ioStGetIdTable				:: !(IOSt .l) -> (!*IdTable,					!IOSt .l)
ioStGetReceiverTable		:: !(IOSt .l) -> (!*ReceiverTable,				!IOSt .l)
ioStGetTimerTable			:: !(IOSt .l) -> (!*TimerTable,					!IOSt .l)
ioStGetOSTime				:: !(IOSt .l) -> (!OSTime,						!IOSt .l)
ioStGetActivateRequests		:: !(IOSt .l) -> (!ActivateRequests,			!IOSt .l)
ioStGetEvents				:: !(IOSt .l) -> (!*OSEvents,					!IOSt .l)
ioStGetWorld				:: !(IOSt .l) -> (!*World,						!IOSt .l)
ioStGetCProcesses			:: !(IOSt .l) -> (!CProcesses,					!IOSt .l)
ioStGetProcessStack			:: !(IOSt .l) -> (!ProcessStack,				!IOSt .l)
ioStGetDocumentInterface	:: !(IOSt .l) -> (!DocumentInterface,			!IOSt .l)
ioStGetOSDInfo				:: !(IOSt .l) -> (!OSDInfo,						!IOSt .l)
ioStGetProcessKind			:: !(IOSt .l) -> (!ProcessKind,					!IOSt .l)
ioStGetIOId					:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
ioStGetMaxIONr				:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
ioStNewMaxIONr				:: !(IOSt .l) -> (!SystemId,					!IOSt .l)
ioStGetParentId				:: !(IOSt .l) -> (!Maybe SystemId,				!IOSt .l)
ioStGetGUIShare				:: !(IOSt .l) -> (!Maybe GUIShare,				!IOSt .l)
ioStGetSubProcessIds		:: !(IOSt .l) -> (![SystemId],					!IOSt .l)
ioStGetIdSeed				:: !(IOSt .l) -> (!Int,							!IOSt .l)
ioStGetClipboardState		:: !(IOSt .l) -> (!ClipboardState,				!IOSt .l)
ioStGetOSWindowMetrics		:: !(IOSt .l) -> (!OSWindowMetrics,				!IOSt .l)
ioStGetDeviceFunctions		:: !(IOSt .l) -> (![DeviceFunctions (PSt .l)],	!IOSt .l)
ioStGetRcvDisabled			:: !(IOSt .l) -> (!Bool,						!IOSt .l)	/* MW11++ */

ioStSetInputTrack			:: !(Maybe InputTrack)				!(IOSt .l) -> IOSt .l
ioStSetProcessAttributes	:: ![ProcessAttribute (PSt .l)]		!(IOSt .l) -> IOSt .l
ioStSetInitIO				:: !*(IdFun (PSt .l))				!(IOSt .l) -> IOSt .l
ioStSetRuntimeState			:: !RuntimeState					!(IOSt .l) -> IOSt .l
ioStSetIOIsModal			:: !(Maybe SystemId)	 			!(IOSt .l) -> IOSt .l
ioStSetIdTable				:: !*IdTable						!(IOSt .l) -> IOSt .l
ioStSetReceiverTable		:: !*ReceiverTable					!(IOSt .l) -> IOSt .l
ioStSetTimerTable			:: !*TimerTable						!(IOSt .l) -> IOSt .l
ioStSetOSTime				:: !OSTime							!(IOSt .l) -> IOSt .l
ioStSetActivateRequests		:: !ActivateRequests				!(IOSt .l) -> IOSt .l
ioStSetEvents				:: !*OSEvents						!(IOSt .l) -> IOSt .l
ioStSetWorld				:: !*World							!(IOSt .l) -> IOSt .l
ioStSetCProcesses			:: !CProcesses						!(IOSt .l) -> IOSt .l
ioStSetProcessStack			:: !ProcessStack					!(IOSt .l) -> IOSt .l
selectIOSt					::									!(IOSt .l) -> IOSt .l
ioStSetOSDInfo				:: !OSDInfo							!(IOSt .l) -> IOSt .l
ioStSetMaxIONr				:: !SystemId						!(IOSt .l) -> IOSt .l
ioStSetSubProcessIds		:: ![SystemId]						!(IOSt .l) -> IOSt .l
ioStSetIdSeed				:: !Int								!(IOSt .l) -> IOSt .l
ioStSetClipboardState		:: !ClipboardState					!(IOSt .l) -> IOSt .l
ioStSetDeviceFunctions		:: !(DeviceFunctions (PSt .l))		!(IOSt .l) -> IOSt .l
ioStRemoveDeviceFunctions	:: !Device							!(IOSt .l) -> IOSt .l
ioStSetRcvDisabled			:: !Bool							!(IOSt .l) -> IOSt .l /* MW11++*/

ioStSwapIO					:: !(![*World],!CProcesses)	!(IOSt .l) -> (!(![*World],!CProcesses),!IOSt .l)

ioStLastInteraction			::									!(IOSt .l) -> (!Bool,	 !IOSt .l)
ioStHasDevice				:: !Device							!(IOSt .l) -> (!Bool,	 !IOSt .l)
ioStHasDevices				::									!(IOSt .l) -> (!Bool,	 !IOSt .l)
ioStGetDevices				::									!(IOSt .l) -> (![Device],!IOSt .l)

ioStGetDevice				:: !Device							!(IOSt .l) -> (!Bool,DeviceSystemState (PSt .l),!IOSt .l)
//	ioStGetDevice removes the indicated device and returns True if found. If not found the device is undefined!
ioStSetDevice				:: !(DeviceSystemState (PSt .l))	!(IOSt .l) -> IOSt .l
//	ioStSetDevice stores the indicated device.  
getIOToolbox				::									!(IOSt .l) -> (!*OSToolbox, !IOSt .l)
setIOToolbox				:: !*OSToolbox						!(IOSt .l) -> IOSt .l
appIOToolbox				:: !.(IdFun *OSToolbox)				!(IOSt .l) -> IOSt .l
accIOToolbox				:: !.(St *OSToolbox .x)				!(IOSt .l) -> (!.x, !IOSt .l)
