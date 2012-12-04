implementation module iostate


import	StdBool, StdFunc, StdList, StdMisc
import	commondef, devicefunctions, devicesystemstate, processstack, receivertable, timertable
import	osactivaterequests, osdocumentinterface, osevent, osguishare, osmouse, ossystem, ostime, ostoolbox, ostypes
from	roundrobin	import :: RR, emptyRR, notodoRR


iostateFatalError :: String String -> .x
iostateFatalError function error
	= fatalError function "iostate" error


::	*PSt l
	=	{	ls				:: !l								// The local (and private) data of the process
		,	io				:: !*IOSt l							// The IOSt environment of the process
		}

::	*CProcesses													// The 'context-free' processes administration
	:==	RR *CProcess											//	is a round-robin
::	*CProcess													// The context-free process
	=	E. .l:
		{	localState		:: !*Maybe l						//	its local state
		,	localIOSt		:: !*IOSt l							//	its context-free IOSt
		}
::	*IOSt l
	= !	{	ioevents		:: !*OSEvents						// The event stream environment
		,	ioworld			:: !*[*World]						// The world environment
		,	ioprocesses		:: !*CProcesses						// All other processes
		,	iodevices		:: !*[DeviceSystemState (PSt l)]	// The GUI device states of the process
		,	ioreceivertable	:: !*ReceiverTable					// The table of the current whereabouts of receivers
		,	iotimertable	:: !*TimerTable						// The table of all currently active timers
		,	ioidtable		:: !*IdTable						// The table of all bound Ids
		,	ioinit			:: !*IdFun (PSt l)					// The initialisation functions of the process
		,	iotoolbox		:: !*OSToolbox						// The Mac continuation value
		,	ioid			:: !SystemId						// The Id of the process
		,	ionr			:: !SystemId						// The max SystemId of all processes
		,	ioparent		:: !Maybe SystemId					// If the process is a subprocess, then Just parentId, otherwise Nothing
		,	ioguishare		:: !Maybe GUIShare					// If the process shares GUI components, then Just _, otherwise Nothing
		,	iosubids		:: ![SystemId]						// The ids of the subprocesses of the process
		,	ioidseed		:: !Int								// The global id generating number (actually the World)
		,	iodevicefuncs	:: ![DeviceFunctions  (PSt l)]		// The currently active device functions
		,	ioatts			:: ![ProcessAttribute (PSt l)]		// The attributes of the process
		,	ioruntime		:: !RuntimeState					// The runtime state of the process
		,	iostack			:: !ProcessStack					// The stacking order of all processes
		,	ioosdinfo		:: !OSDInfo							// The OS document interface information of the process
		,	iokind			:: !ProcessKind						// The kind of the process (interactive or virtual)
		,	ioismodal		:: !Maybe SystemId					// If a process has some modal windows, then Just id, otherwise Nothing
		,	ioostime		:: !OSTime							// The current OSTime
		,	ioactrequest	:: !ActivateRequests				// The issued activation requests
		,	iobutton		:: !ButtonFreqState					// The state of double MouseDowns
//PA---	,	iokeytrack		:: !Maybe KeyTrack					// If the process is handling Key(Repeat/Up), then Just _, otherwise Nothing
		,	ioinputtrack	:: !Maybe InputTrack				// The process is handling mouse/key input flags
		,	ioclipboard		:: !ClipboardState					// The state of the clipboard
		,	iooswmetrics	:: !OSWindowMetrics					// The window metrics
		,	iorcvdisabled	:: !Bool							// to check, whether a receiver was disabled (explicitly or via close) (MW11++)
		}
::	GUIShare
	:==	OSGUIShare
::	RuntimeState
	=	Running													// The process is running
	|	Blocked !SystemId										// The process is blocked for the process with given id
	|	Closed													// The process is closed

::	ActivateRequests	:== [OSActivateRequest]
::	ButtonFreqState
	=	{	bfstime		:: !Int									// Last time of a MouseDown
		,	bfsfreq		:: !ButtonFreq							// Nr of DoubleMouseDowns (modulo 3)
		,	bfsdddist	:: !DoubleDownDist						// The maximum distance for two MouseDowns
		,	bfspos		:: !Point2								// Last position MouseDown
		,	bfswindow	:: !OSWindowPtr							// Window in which last MouseDown occurred
		}
::	ButtonFreq			:== Int
::	DoubleDownDist		:== Int
//::	KeyTrack			:==	Int								// Message field of the Event of the key being tracked
::	InputTrack													// Input being tracked:
	=	{	itWindow	:: !OSWindowPtr							// the parent window
		,	itControl	:: !Int									// zero if parent window, otherwise item nr of control (>0)
		,	itKind		:: !InputTrackKind						// the input kinds being tracked
		}
::	InputTrackKind												// Input source kinds:
	=	{	itkMouse	:: !Bool								// mouse
		,	itkKeyboard	:: !Bool								// keyboard
		,	itkChar		:: !Int									// key that is being tracked
		,	itkSlider	:: !Maybe SliderTrackInfo				// extra slider tracking info
		}
::	SliderTrackInfo												// Slider being tracked:
	=	{	stiControl	:: !OSWindowPtr							// control being tracked (OSNoWindowPtr if window)
		,	stiPart		:: !Int									// part of slider being tracked
		,	stiHilite	:: !Bool								// currently highlighted
		,	stiDirection:: !Direction							// slider direction
		,	stiIsControl:: !Bool								// is slider control
		}
::	ClipboardState
	=	{	cbsCount	:: !Int									// ScrapCount of last access
		}

//	Access rules to the IOSt:

//	Creation of an initial, empty IOSt:

emptyIOSt :: !SystemId !(Maybe SystemId) !(Maybe GUIShare) !DocumentInterface !ProcessKind 
				![ProcessAttribute (PSt .l)] !*(IdFun (PSt .l)) !(Maybe SystemId)
			-> IOSt .l
emptyIOSt ioId parentId guishare documentInterface processKind processAtts initIO modalId
	# tb				= OSNewToolbox
	# (wMetrics,tb)		= osDefaultWindowMetrics tb
	= {	ioevents		= osNewEvents
	  ,	ioworld			= []
	  ,	ioprocesses		= emptyRR
	  ,	iodevices		= []
	  ,	iotimertable	= initialTimerTable
	  ,	ioreceivertable	= initialReceiverTable
	  ,	ioidtable		= initialIdTable
	  ,	ioinit			= initIO
	  ,	iotoolbox		= tb
	  ,	ioid			= ioId
	  ,	ionr			= nullSystemId
	  ,	ioparent		= parentId
	  ,	ioguishare		= guishare
	  ,	iosubids		= []
	  ,	ioidseed		= 0
	  ,	iodevicefuncs	= []
	  ,	ioatts			= processAtts
	  ,	ioruntime		= Running
	  ,	iostack			= emptyProcessStack
	  ,	ioosdinfo		= emptyOSDInfo documentInterface
	  ,	iokind			= processKind
	  ,	ioismodal		= modalId
	  ,	ioostime		= fromInt 0
	  ,	ioactrequest	= []
	  ,	iobutton		= InitButtonFreqState
	  ,	ioinputtrack	= Nothing
	  ,	ioclipboard		= InitClipboardState
	  ,	iooswmetrics	= wMetrics
	  , iorcvdisabled	= False // MW11++
	  }


//	Access to the ButtonFreqState:

InitButtonFreqState	:==	{	bfstime		= 0
						,	bfsfreq		= 0
						,	bfsdddist	= 5
						,	bfspos		= zero
						,	bfswindow	= OSNoWindowPtr
						}

ioStButtonFreq :: !Int !Point2 !OSWindowPtr !(IOSt .l) -> (!Int,!IOSt .l)
ioStButtonFreq now pos curWindow ioState
	# (bfs,ioState)		= getButtonFreq ioState
	  newbfs			= {bfs & bfstime=now, bfspos=pos, bfswindow=curWindow}
	| curWindow<>bfs.bfswindow
		= (1,setButtonFreq {newbfs & bfsfreq=1} ioState)
	# (double,ioState)	= accIOToolbox osGetDoubleClickTime ioState
	  oldpos			= bfs.bfspos
	  oldfreq			= bfs.bfsfreq
	  ddDist`			= dist oldpos.x pos.x + dist oldpos.y pos.y
	  dTime				= now-bfs.bfstime
	| dTime>double || ddDist`>bfs.bfsdddist
		= (1,setButtonFreq {newbfs & bfsfreq=1} ioState)
	| otherwise
		# newfreq		= oldfreq+1
		= (newfreq,setButtonFreq {newbfs & bfsfreq=newfreq} ioState)
where
	getButtonFreq :: !(IOSt .l) -> (!ButtonFreqState, !IOSt .l)
	getButtonFreq ioState=:{iobutton} = (iobutton, ioState)
	
	setButtonFreq :: !ButtonFreqState !(IOSt .l) -> IOSt .l
	setButtonFreq bfs ioState = {ioState & iobutton=bfs}

ioStSetDoubleDownDist :: !DoubleDownDist !(IOSt .l) -> IOSt .l
ioStSetDoubleDownDist ddDist ioState=:{iobutton}
	| ddDist==iobutton.bfsdddist
		= ioState
	| otherwise
		= {ioState & iobutton={ioState.iobutton & bfsdddist=max 0 ddDist}}


//	Access rules to InputTrack:

ioStGetInputTrack :: !(IOSt .l) -> (!Maybe InputTrack,!IOSt .l)
ioStGetInputTrack ioState=:{ioinputtrack} = (ioinputtrack, ioState)

ioStSetInputTrack :: !(Maybe InputTrack) !(IOSt .l) -> IOSt .l
ioStSetInputTrack inputtrack ioState = {ioState & ioinputtrack=inputtrack}


//	Access rules to IOAttributes:

ioStGetProcessAttributes :: !(IOSt .l) -> (![ProcessAttribute (PSt .l)], !IOSt .l)
ioStGetProcessAttributes ioState=:{ioatts} = (ioatts, ioState)

ioStSetProcessAttributes :: ![ProcessAttribute (PSt .l)] !(IOSt .l) -> IOSt .l
ioStSetProcessAttributes atts ioState = {ioState & ioatts=atts}


//	Access rules to the initial actions:

ioStGetInitIO :: !(IOSt .l) -> (!*IdFun (PSt .l), !IOSt .l)
ioStGetInitIO ioState=:{ioinit} = (ioinit,{ioState & ioinit=id})

ioStSetInitIO :: !*(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
ioStSetInitIO initIO ioState = {ioState & ioinit=initIO}


//	Access rules to RuntimeState:

ioStClosed :: !(IOSt .l) -> (!Bool,!IOSt .l)
ioStClosed ioState=:{ioruntime=Closed}	= (True,ioState)
ioStClosed ioState						= (False,ioState)

ioStGetRuntimeState :: !(IOSt .l) -> (!RuntimeState, !IOSt .l)
ioStGetRuntimeState ioState=:{ioruntime} = (ioruntime, ioState)

ioStSetRuntimeState :: !RuntimeState !(IOSt .l) -> IOSt .l
ioStSetRuntimeState runtime ioState = {ioState & ioruntime=runtime}


//	Access rules to IOIsModal:

ioStGetIOIsModal :: !(IOSt .l) -> (!Maybe SystemId, !IOSt .l)
ioStGetIOIsModal ioState=:{ioismodal} = (ioismodal, ioState)

ioStSetIOIsModal :: !(Maybe SystemId) !(IOSt .l) -> IOSt .l
ioStSetIOIsModal optId ioState = {ioState & ioismodal=optId}


//	Access rules to IdTable:

ioStGetIdTable :: !(IOSt .l) -> (!*IdTable,!IOSt .l)
ioStGetIdTable ioState=:{ioidtable}
	= (ioidtable, {ioState & ioidtable=initialIdTable})

ioStSetIdTable :: !*IdTable !(IOSt .l) -> IOSt .l
ioStSetIdTable idTable ioState
	= {ioState & ioidtable=idTable}


//	Access rules to ReceiverTable:

ioStGetReceiverTable :: !(IOSt .l) -> (!*ReceiverTable,!IOSt .l)
ioStGetReceiverTable ioState=:{ioreceivertable}
	= (ioreceivertable, {ioState & ioreceivertable=initialReceiverTable})

ioStSetReceiverTable :: !*ReceiverTable !(IOSt .l) -> IOSt .l
ioStSetReceiverTable ioreceivertable ioState
	= {ioState & ioreceivertable=ioreceivertable}


//	Access rules to TimerTable:

ioStGetTimerTable :: !(IOSt .l) -> (!*TimerTable,!IOSt .l)
ioStGetTimerTable ioState=:{iotimertable}
	= (iotimertable, {ioState & iotimertable=initialTimerTable})

ioStSetTimerTable :: !*TimerTable !(IOSt .l) -> IOSt .l
ioStSetTimerTable tt ioState
	= {ioState & iotimertable=tt}


//	Access rules to OSTime:

ioStGetOSTime :: !(IOSt .l) -> (!OSTime,!IOSt .l)
ioStGetOSTime ioState=:{ioostime} = (ioostime,ioState)

ioStSetOSTime :: !OSTime !(IOSt .l) -> IOSt .l
ioStSetOSTime ostime ioState = {ioState & ioostime=ostime}


//	Access rules to ActivateRequests:

ioStGetActivateRequests :: !(IOSt .l) -> (!ActivateRequests, !IOSt .l)
ioStGetActivateRequests ioState=:{ioactrequest} = (ioactrequest, ioState)

ioStSetActivateRequests :: !ActivateRequests !(IOSt .l) -> IOSt .l
ioStSetActivateRequests ioReqs ioState = {ioState & ioactrequest=ioReqs}


//	Access rules to the OSEvents environment:

ioStGetEvents :: !(IOSt .l) -> (!*OSEvents, !IOSt .l)
ioStGetEvents ioState=:{ioevents}
	= (ioevents,{ioState & ioevents=osNewEvents})

ioStSetEvents :: !*OSEvents !(IOSt .l) -> IOSt .l
ioStSetEvents es ioState
	= {ioState & ioevents=es}


//	Access rules to the World environment:

ioStGetWorld :: !(IOSt .l) -> (!*World, !IOSt .l)
ioStGetWorld ioState=:{ioworld=[w:ws]}
	= (w,{ioState & ioworld=ws})

ioStSetWorld :: !*World !(IOSt .l) -> IOSt .l
ioStSetWorld w ioState=:{ioworld=ws}
	= {ioState & ioworld=[w:ws]}


//	Access rules to CProcesses:

ioStGetCProcesses :: !(IOSt .l) -> (!CProcesses, !IOSt .l)
ioStGetCProcesses ioState=:{ioprocesses}
	= (ioprocesses,{ioState & ioprocesses=emptyRR})

ioStSetCProcesses :: !CProcesses !(IOSt .l) -> IOSt .l
ioStSetCProcesses processes ioState
	= {ioState & ioprocesses=processes}


//	Access to the ProcessStack of the IOSt:

ioStGetProcessStack :: !(IOSt .l) -> (!ProcessStack, !IOSt .l)
ioStGetProcessStack ioState=:{iostack}
	= (iostack,ioState)

ioStSetProcessStack :: !ProcessStack !(IOSt .l) -> IOSt .l
ioStSetProcessStack ioStack ioState
	= {ioState & iostack=ioStack}

selectIOSt :: !(IOSt .l) -> IOSt .l
selectIOSt ioState=:{ioid,iostack}
	= {ioState & iostack=selectProcessShowState ioid iostack}


//	Access rules to DocumentInterface:

ioStGetDocumentInterface :: !(IOSt .l) -> (!DocumentInterface, !IOSt .l)
ioStGetDocumentInterface ioState=:{ioosdinfo}
	= (getOSDInfoDocumentInterface ioosdinfo, ioState)


//	Access rules to OSDInfo:

ioStGetOSDInfo :: !(IOSt .l) -> (!OSDInfo,!IOSt .l)
ioStGetOSDInfo ioState=:{ioosdinfo}
	= (ioosdinfo, ioState)

ioStSetOSDInfo :: !OSDInfo !(IOSt .l) -> IOSt .l
ioStSetOSDInfo osdInfo ioState
	= {ioState & ioosdinfo=osdInfo}


//	Access rules to ProcessKind:

ioStGetProcessKind :: !(IOSt .l) -> (!ProcessKind, !IOSt .l)
ioStGetProcessKind ioState=:{iokind}
	= (iokind, ioState)


//	Swapping of IOSt environments:

ioStSwapIO :: !(![*World],!CProcesses) !(IOSt .l) -> (!(![*World],!CProcesses),!IOSt .l)
ioStSwapIO (world`,cprocesses`) ioState=:{ioworld,ioprocesses}
	= ((ioworld,ioprocesses),{ioState & ioworld=world`,ioprocesses=cprocesses`})


//	Access to the SystemId of the IOSt:

ioStGetIOId :: !(IOSt .l) -> (!SystemId,!IOSt .l)
ioStGetIOId ioState=:{ioid}
	= (ioid,ioState)


//	Access to the max SystemId of the IOSt:

ioStGetMaxIONr :: !(IOSt .l) -> (!SystemId,!IOSt .l)
ioStGetMaxIONr ioState=:{ionr}
	= (ionr,ioState)

ioStSetMaxIONr :: !SystemId !(IOSt .l) -> IOSt .l
ioStSetMaxIONr maxId ioState
	= {ioState & ionr=maxId}

ioStNewMaxIONr :: !(IOSt .l) -> (!SystemId,!IOSt .l)
ioStNewMaxIONr ioState=:{ionr}
	= (newMaxId, {ioState & ionr=maxId1})
where
	(maxId1,newMaxId) = incrSystemId ionr


//	Access to the parent Id of the IOSt:

ioStGetParentId :: !(IOSt .l) -> (!Maybe SystemId,!IOSt .l)
ioStGetParentId ioState=:{ioparent}
	= (ioparent,ioState)


//	Access to the subprocess flag of the IOSt:

ioStGetGUIShare :: !(IOSt .l) -> (!Maybe GUIShare,!IOSt .l)
ioStGetGUIShare ioState=:{ioguishare}
	= (ioguishare,ioState)


//	Access to the SystemIds of the subprocess of the IOSt:

ioStGetSubProcessIds :: !(IOSt .l) -> (![SystemId],!IOSt .l)
ioStGetSubProcessIds ioState=:{iosubids}
	= (iosubids,ioState)

ioStSetSubProcessIds :: ![SystemId] !(IOSt .l) -> IOSt .l
ioStSetSubProcessIds ids ioState
	= {ioState & iosubids=ids}


//	Access to the global seed integer to generate all Ids (see StdId):

ioStGetIdSeed :: !(IOSt .l) -> (!Int,!IOSt .l)
ioStGetIdSeed ioState=:{ioidseed}
	= (ioidseed,ioState)

ioStSetIdSeed :: !Int !(IOSt .l) -> IOSt .l
ioStSetIdSeed seed ioState
	= {ioState & ioidseed=seed}


//	Access to the ClipboardState of the IOSt:

InitClipboardState	:==	{cbsCount=0}

ioStGetClipboardState :: !(IOSt .l) -> (!ClipboardState, !IOSt .l)
ioStGetClipboardState ioState=:{ioclipboard}
	= (ioclipboard,ioState)

ioStSetClipboardState :: !ClipboardState !(IOSt .l) -> IOSt .l
ioStSetClipboardState clipboard ioState
	= {ioState & ioclipboard=clipboard}


//	Access to the OSWindowMetrics of the IOSt:

ioStGetOSWindowMetrics :: !(IOSt .l) -> (!OSWindowMetrics,!IOSt .l)
ioStGetOSWindowMetrics ioState=:{iooswmetrics}
	= (iooswmetrics,ioState)


//	Access to the DeviceFunctions:

ioStGetDeviceFunctions :: !(IOSt .l) -> (![DeviceFunctions (PSt .l)],!IOSt .l)
ioStGetDeviceFunctions ioState=:{iodevicefuncs}
	= (iodevicefuncs,ioState)

ioStSetDeviceFunctions :: !(DeviceFunctions (PSt .l)) !(IOSt .l) -> IOSt .l
ioStSetDeviceFunctions funcs=:{dDevice} ioState=:{iodevicefuncs}
	= {ioState & iodevicefuncs=setdevicefunctions (priorityDevice dDevice) dDevice funcs iodevicefuncs}
where
	setdevicefunctions :: !Int !Device !(DeviceFunctions .pst) ![DeviceFunctions .pst] -> [DeviceFunctions .pst]
	setdevicefunctions p device funcs fs=:[dfunc=:{dDevice}:dfuncs]
		| device==dDevice
			= [funcs:dfuncs]
		| p>priorityDevice dDevice
			= [funcs:fs]
		| otherwise
			#! fs	= setdevicefunctions p device funcs dfuncs
			= [dfunc:fs]
	setdevicefunctions _ _ funcs []
		= [funcs]

ioStRemoveDeviceFunctions :: !Device !(IOSt .l) -> IOSt .l
ioStRemoveDeviceFunctions device ioState=:{iodevicefuncs}
	= {ioState & iodevicefuncs=removedevicefunctions device iodevicefuncs}
where
	removedevicefunctions :: !Device ![DeviceFunctions .pst] -> [DeviceFunctions .pst]
	removedevicefunctions device [dfunc=:{dDevice}:dfuncs]
		| device==dDevice
			= dfuncs
		| otherwise
			#! dfuncs	= removedevicefunctions device dfuncs
			= [dfunc:dfuncs]
	removedevicefunctions _ []
		= []

//	Access to the DeviceSystemStates:

ioStLastInteraction :: !(IOSt .l) -> (!Bool,!IOSt .l)
ioStLastInteraction ioState
	# (processes,ioState)	= ioStGetCProcesses ioState
	  (empty,processes)		= notodoRR processes
	# ioState				= ioStSetCProcesses processes ioState
	= (empty,ioState)

ioStHasDevice :: !Device !(IOSt .l) -> (!Bool,!IOSt .l)
ioStHasDevice d ioState=:{iodevices}
	# (ok,ds)	= devicesHaveDevice d iodevices
	= (ok,{ioState & iodevices=ds})
where
	devicesHaveDevice :: !Device !*[DeviceSystemState .pst] -> (!Bool,!*[DeviceSystemState .pst])
	devicesHaveDevice d [dState:dStates]
		# (d`,dState)		= toDevice dState
		| d`==d
			= (True,[dState:dStates])
		| otherwise
			# (ok,dStates)	= devicesHaveDevice d dStates
			= (ok,[dState:dStates])
	devicesHaveDevice _ []
		= (False,[])

ioStHasDevices :: !(IOSt .l) -> (!Bool,!IOSt .l)
ioStHasDevices ioState=:{iodevices=[]}
	= (False,ioState)
ioStHasDevices ioState
	= (True,ioState)

ioStGetDevices :: !(IOSt .l) -> (![Device],!IOSt .l)
ioStGetDevices ioState=:{iodevices}
	#! (devices,ds)	= accessList toDevice iodevices
	= (devices,{ioState & iodevices=ds})

ioStGetDevice :: !Device !(IOSt .l) -> (!Bool,DeviceSystemState (PSt .l),!IOSt .l)
ioStGetDevice d ioState=:{iodevices}
	# (found,device,ds)	= devicesGetDevice d iodevices
	= (found,device,{ioState & iodevices=ds})
where
	devicesGetDevice :: !Device !*[DeviceSystemState .pst] -> (!Bool,DeviceSystemState .pst,!*[DeviceSystemState .pst])
	devicesGetDevice d [dState:dStates]
		# (d`,dState)				= toDevice dState
		| d`==d
			= (True,dState,dStates)
		| otherwise
			# (found,device,dStates)= devicesGetDevice d dStates
			= (found,device,[dState:dStates])
	devicesGetDevice d []
		= (False,undef,[])

ioStSetDevice :: !(DeviceSystemState (PSt .l)) !(IOSt .l) -> IOSt .l
ioStSetDevice d ioState=:{iodevices}
	#! (device,d)	= toDevice d
	#! ds	 		= devicesSetDevice (priorityDevice device) device d iodevices
	= {ioState & iodevices=ds}
where
	devicesSetDevice :: !Int !Device !(DeviceSystemState .pst) !*[DeviceSystemState .pst] -> *[DeviceSystemState .pst]
	devicesSetDevice p device dState2 [dState1:dStates]
		# (device1,dState1)	= toDevice dState1
		| device1==device
			= iostateFatalError "ioStSetDevice" (toString device+++" already present") //[dState2:dStates]
		| p>priorityDevice device1
			= [dState2,dState1:dStates]
		| otherwise
			#! dStates	= devicesSetDevice p device dState2 dStates
			= [dState1:dStates]
	devicesSetDevice _ _ dState []
		= [dState]

// MW11..
ioStGetRcvDisabled	:: !(IOSt .l) -> (!Bool, !IOSt .l)
ioStGetRcvDisabled ioState=:{iorcvdisabled}
	= (iorcvdisabled, ioState)

ioStSetRcvDisabled	:: !Bool !(IOSt .l) -> IOSt .l
ioStSetRcvDisabled iorcvdisabled ioState
	= {ioState & iorcvdisabled=iorcvdisabled}
// ..MW11

getIOToolbox :: !(IOSt .l) -> (!*OSToolbox,!IOSt .l)
getIOToolbox ioState=:{iotoolbox}
	= (iotoolbox,{ioState & iotoolbox=OSDummyToolbox})

setIOToolbox :: !*OSToolbox !(IOSt .l) -> IOSt .l
setIOToolbox tb ioState
	= {ioState & iotoolbox=tb}

appIOToolbox :: !.(IdFun *OSToolbox) !(IOSt .l) -> IOSt .l
appIOToolbox f ioState=:{iotoolbox}
	#! tb	= f iotoolbox
	=  {ioState & iotoolbox=tb}

accIOToolbox :: !.(St *OSToolbox .x) !(IOSt .l) -> (!.x,!IOSt .l)
accIOToolbox f ioState=:{iotoolbox}
	#! (x,tb)	= f iotoolbox
	=  (x,{ioState & iotoolbox=tb})
