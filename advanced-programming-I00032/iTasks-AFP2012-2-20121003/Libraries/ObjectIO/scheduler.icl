implementation module scheduler


import	StdBool, StdList, StdTuple
import	osevent, ossystem, ostime, ostoolbox
import	commondef, devicefunctions, iostate, processstack, roundrobin, timertable, world
from	StdProcessDef		import :: ProcessInit
from	StdPSt				import accPIO, appPIO
from	StdProcessAttribute	import isProcessKindAttribute


::	*Environs
	=	{	envsEvents		:: !*OSEvents
		,	envsWorld		:: !*World
		}
::	*Context
	=	{	cEnvs			:: !*Environs			// The global environments
		,	cProcessStack	:: !ProcessStack		// The global process stack
		,	cMaxIONr		:: !SystemId			// The global maximum system number
		,	cProcesses		:: !*CProcesses			// All processes
		,	cModalProcess	:: !Maybe SystemId		// The SystemId of the interactive process that has a modal window
		,	cReceiverTable	:: !*ReceiverTable		// The global receiver-process table
		,	cTimerTable		:: !*TimerTable			// The table of all currently active timers
		,	cIdTable		:: !*IdTable			// The table of all bound Ids
		,	cOSTime			:: !OSTime				// The current OSTime
		,	cIdSeed			:: !Int					// The global id generating number (actually the World)
		,	cOSToolbox		:: !*OSToolbox			// The toolbox environment
		}

schedulerFatalError :: String String -> .x
schedulerFatalError rule message
	= error rule "scheduler" message


/*	Retrieval operations:			*/

//	On Context:

contextGetProcessStack :: !Context -> (!ProcessStack,!Context)
contextGetProcessStack context=:{cProcessStack}
	= (cProcessStack,context)

contextGetProcesses :: !Context -> (!CProcesses,!Context)
contextGetProcesses context=:{cProcesses}
	= (cProcesses,{context & cProcesses=emptyRR})

contextGetSleepTime :: !Context -> (!Int,!Context)
contextGetSleepTime context=:{cTimerTable=tt,cReceiverTable}
	# (maybe_sleep,tt)		= getTimeIntervalFromTimerTable tt
	# (maybe_receiver,rt)	= getActiveReceiverTableEntry cReceiverTable
	  sleep					= if (isJust maybe_receiver) OSNoSleep						// a receiver with a non-empty message queue exists
			  				 (if (isJust maybe_sleep)	(snd (fromJust maybe_sleep))	// a timer with given interval is waiting
			  				 							//OSNoSleep)	// DvA: need to test here wether tracking or not...
			  				 							OSLongSleep)					// neither a receiver nor timer
	= (sleep,{context & cTimerTable=tt,cReceiverTable=rt})

contextGetOSEvents :: !Context -> (!OSEvents,!Context)
contextGetOSEvents context=:{cEnvs=envs=:{envsEvents=es}}
	= (es,{context & cEnvs={envs & envsEvents=osNewEvents}})

contextSetOSEvents :: !(!OSEvents,!Context) -> Context
contextSetOSEvents (osEvents,context=:{cEnvs=envs})
	= {context & cEnvs={envs & envsEvents=osEvents}}


//	On RuntimeState:

rsIsBlocked :: !RuntimeState -> (!Bool,SystemId)
rsIsBlocked (Blocked ioid)	= (True, ioid)
rsIsBlocked _				= (False,nullSystemId)

rsIsClosed :: !RuntimeState -> Bool
rsIsClosed Closed			= True
rsIsClosed _				= False


//	Starting an interactive process.

initContext :: !*(ProcessInit (PSt .l)) !String !.l !DocumentInterface !ProcessKind !*World -> (!Context,!*OSToolbox)
initContext ioDefInit ioDefAbout local documentInterface ioKind world
	# w						= loadWorld world
	# world					= storeWorld w world
	# initEnvs				= {envsEvents=osNewEvents,envsWorld=world}
	# tb					= osInitToolbox OSNewToolbox
	# (ostime,tb)			= osGetTime tb
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= initSystemId
		,	cProcesses		= toRR [] [openLocalIO ioState local]
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb
	  )
where
	initModalId				= Nothing
	show					= ioKind==InteractiveProcess
	ioStack					= [{psId=initSystemId,psShow=show,psKind=ioKind}]
	ioState					= createNewIOSt [] ioDefInit ioDefAbout initSystemId Nothing Nothing ShareGUI documentInterface ioKind

initContext` :: !*World -> (!Context,!*OSToolbox)
initContext` world
	# w					= loadWorld world
	# world				= storeWorld w world
	# initEnvs			= {envsEvents=osNewEvents,envsWorld=world}
	# tb				= osInitToolbox OSNewToolbox
	# (ostime,tb)		= osGetTime tb
	= (	{	cEnvs			= initEnvs
		,	cProcessStack	= ioStack
		,	cMaxIONr		= initSystemId
		,	cProcesses		= toRR [] []
		,	cModalProcess	= initModalId
		,	cReceiverTable	= initialReceiverTable
		,	cTimerTable		= initialTimerTable
		,	cIdTable		= initialIdTable
		,	cOSTime			= ostime
		,	cIdSeed			= w
		,	cOSToolbox		= OSNewToolbox
		}
	  ,	tb
	  )
where
	initModalId			= Nothing
	ioStack				= []

createNewIOSt :: ![ProcessAttribute (PSt .l)] !*(ProcessInit (PSt .l)) String !SystemId !(Maybe SystemId) 
					!(Maybe GUIShare) !Bool !DocumentInterface !ProcessKind
	-> IOSt .l
createNewIOSt pAtts ioDefInit ioDefAbout nr parentId guishare isSubProcess documentInterface ioKind
	= emptyIOSt nr parentId guishare documentInterface ioKind pAtts ioDefInit Nothing


//	Handling events until termination of all interactive processes.

handleEvents :: !Context !*OSToolbox -> (!Context,!*OSToolbox)
handleEvents context tb
	# (_,context)	= handleContextOSEvent osNullEvent context
	= osHandleEvents terminate contextGetOSEvents contextSetOSEvents contextGetSleepTime handleContextOSEvent (context,tb)
where
	terminate :: !Context -> (!Bool,!Context)
	terminate context=:{cProcessStack}
		= (isEmpty cProcessStack,context)


//	Closing a final context. 

closeContext :: !Context !*OSToolbox -> *World
closeContext {cProcessStack,cEnvs={envsWorld}} tb
	| isEmpty cProcessStack	= storeWorld 42 envsWorld
	| otherwise				= schedulerFatalError "closeContext" "not a final Context"


//	Handling events while condition holds.

chandleEvents :: !(St Context Bool) !Context !*OSToolbox -> (!Context,!*OSToolbox)	// PA: swapped order of last 2 args and result
chandleEvents cond context tb
	= osHandleEvents terminate contextGetOSEvents contextSetOSEvents contextGetSleepTime handleContextOSEvent (context,tb)
where
	terminate :: !Context -> (!Bool,!Context)
	terminate context
		# (continue,context)	= cond context
		= (not continue,context)

handleContextOSEvent :: !OSEvent !Context -> (![Int],!Context)
handleContextOSEvent osEvent context=:{cEnvs=envs=:{envsEvents=osEvents},cProcessStack,cProcesses,cReceiverTable,cTimerTable,cOSTime,cOSToolbox}
//	PA: shift the time in the timertable.
	# (ostime,tb)				= osGetTime cOSToolbox
	  timeshift					= toInt (ostime-cOSTime)
	  timertable				= shiftTimeInTimerTable timeshift cTimerTable
//	PA: determine whether a TimerEvent or ASyncMessage can be generated
	  (schedulerEvent,receivertable,timertable,osEvents)
	  							= toSchedulerEvent osEvent cReceiverTable timertable cOSTime osEvents
	  processes					= resetRR cProcesses
	  (_,oldTopIO)				= topShowProcessShowState cProcessStack
	# context					= {context & cEnvs			= {envs & envsEvents=osEvents}
										   , cProcesses		= processes
										   , cReceiverTable	= receivertable
										   , cTimerTable	= timertable
										   , cOSTime		= ostime
										   , cOSToolbox		= tb
								  }
	# (schedulerEvent,context)	= handleEventForContext False schedulerEvent context
	  replyToOS					= case schedulerEvent of
	  								(ScheduleOSEvent _ reply)	-> reply
	  								_							-> []
	# (newStack,context)		= contextGetProcessStack context
	  (newTopIOVis,newTopIO)	= topShowProcessShowState newStack
	| oldTopIO==newTopIO || not newTopIOVis
		= (replyToOS,context)
	| otherwise
		# (processes,context)	= contextGetProcesses context
		# (newStack,processes)	= activateTopOfGroups newTopIO newStack (resetRR processes)
		= (replyToOS,{context & cProcessStack=newStack,cProcesses=processes})


/*	PA: new function:
	in case a non-urgent, removable system event has been caught, check if a TimerEvent or ASyncMessage 
	can be generated instead. If both a TimerEvent and an ASyncMessage are available, use the OSTime to 
	decide which one to choose.
*/
zerotimelimit :: OSTime
zerotimelimit =: fromInt (max 1 (OStickspersecond/20))

//import StdDebug,dodebug

toSchedulerEvent :: !OSEvent !*ReceiverTable !*TimerTable !OSTime !*OSEvents -> (!SchedulerEvent,!*ReceiverTable,!*TimerTable,!*OSEvents)
toSchedulerEvent osevent receivertable timertable osTime osEvents
	| eventIsUrgent
//		= trace_n (showEvent osevent) (schedulerEvent,receivertable,timertable,osEvents)
		= (schedulerEvent,receivertable,timertable,osEvents)
	# (maybe_timer,timertable)		= getTimeIntervalFromTimerTable timertable
	  (zerotimer,interval)			= fromJust maybe_timer
//	  (zerotimer`,interval)			= fromJust maybe_timer
//	  zerotimer						= isJust maybe_timer && zerotimer`
	  sure_timer					= isJust maybe_timer && interval<=0
	  (maybe_receiver,receivertable)= getActiveReceiverTableEntry receivertable
	  sure_receiver					= isJust maybe_receiver
	| not sure_timer && not sure_receiver
		= (schedulerEvent,receivertable,timertable,osEvents)
	# (timerEvent,timertable`)		= toTimerEvent timertable
	  (asyncEvent,receivertable`)	= toASyncEvent (fromJust maybe_receiver) receivertable
	# osEvents`						= checkOSZeroTimerEvent zerotimer osTime osevent osEvents
//	# osEvents`						= checkOSZeroTimerEvent maybe_timer osTime osevent osEvents
	| sure_timer && sure_receiver
//		# osEvents`						= checkOSZeroTimerEvent zerotimer osTime osevent osEvents
		| isEven (toInt osTime)
			= (timerEvent,receivertable,timertable`,osEvents`)
		// otherwise
			= (asyncEvent,receivertable`,timertable,osEvents`)
	| sure_timer
//		# osEvents`						= checkOSZeroTimerEvent zerotimer osTime osevent osEvents
		= (timerEvent,receivertable,timertable`,osEvents`)
	| otherwise
		= (asyncEvent,receivertable`,timertable,osEvents)
where
	eventIsUrgent					= osEventIsUrgent osevent
	schedulerEvent					= ScheduleOSEvent osevent []

//	In case the original event is a virtual zero timer event:
//		check if another should be inserted in the OSEvents to circumvent the event system call. 
//	In case the original event is a non urgent event:
//		check if an initial virtual zero timer event must be inserted to start circumventing event system calls.
	checkOSZeroTimerEvent :: !Bool !OSTime !OSEvent !*OSEvents -> *OSEvents
//	checkOSZeroTimerEvent :: !(Maybe (Bool,Int)) !OSTime !OSEvent !*OSEvents -> *OSEvents
	checkOSZeroTimerEvent zerotimer osTime osevent osEvents
//	checkOSZeroTimerEvent maybe_timer osTime osevent osEvents
		| not zerotimer
			= osEvents
		| isNothing maybe_zerotimer_start
			= osAppendEvents [createOSZeroTimerEvent osTime] osEvents
		| osTime-zerotimer_start<=zerotimelimit
			= osAppendEvents [osevent] osEvents
		| otherwise
			= osEvents
/*
		| isJust maybe_zerotimer_start && zerotimer
			| osTime-zerotimer_start<=zerotimelimit
				= osAppendEvents [osevent] osEvents
			// otherwise
				= osEvents
		| isNothing maybe_zerotimer_start && zerotimer
			= osAppendEvents [createOSZeroTimerEvent osTime] osEvents
//		| osTime-zerotimer_start<=zerotimelimit
//			= osAppendEvents [osevent] osEvents
		| otherwise
			= osEvents
*/
	where
//		(zerotimer,_)			= fromJust maybe_timer
		maybe_zerotimer_start	= getOSZeroTimerStartTime osevent
		zerotimer_start			= fromJust maybe_zerotimer_start
	
//	The receiver for which an ASyncMessage is generated is placed behind all other receivers, 
//	creating a round-robin order. Its asynchronous message queue length field is decreased.
	toASyncEvent :: !Id !*ReceiverTable -> (!SchedulerEvent,!*ReceiverTable)
	toASyncEvent rid receivertable
		#! (maybeRTE,receivertable)	= getReceiverTableEntry rid receivertable
		#! rte						= fromJust maybeRTE
		#! rte						= {rte & rteASMCount=rte.rteASMCount-1}
		#! receivertable			= setReceiverTableEntry rte (snd (removeReceiverFromReceiverTable rid receivertable))
		= (ScheduleMsgEvent (ASyncMessage {asmRecLoc=rte.rteLoc}),receivertable)
	
//	The timer for which a TimerEvent is generated is determined by getActiveTimerInTable.
//	This function already takes care of fairness using a round robin scheme.
	toTimerEvent :: !*TimerTable -> (!SchedulerEvent,!*TimerTable)
	toTimerEvent timertable
		# (maybeTimerEvent,timertable)	= getActiveTimerInTimerTable timertable
		= (ScheduleTimerEvent (fromJust maybeTimerEvent),timertable)


handleEventForContext :: !Bool !SchedulerEvent !Context -> (!SchedulerEvent,!Context)
handleEventForContext eventDone schedulerEvent context=:{cProcesses=processes}
	# (notodo,processes)	= notodoRR processes
	| notodo
		= (schedulerEvent,{context & cProcesses=processes})
	# (process,processes)	= getcurrentRR processes
	# (quitted,process)		= processQuitted process
	# (isModal,process)		= processModal   process
	| quitted && not isModal
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=processes}
	| quitted
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=adddoneRR process processes}
	| otherwise
		# (eventDone,schedulerEvent,process,context)
						= handleEventForLocalIO eventDone schedulerEvent process {context & cProcesses=processes}
		= handleEventForContext eventDone schedulerEvent {context & cProcesses=adddoneRR process context.cProcesses}
where
	processQuitted :: !CProcess -> (!Bool,!CProcess)
	processQuitted localIO=:{localIOSt}
		# (closed,ioState) = ioStClosed localIOSt
		= (closed,{localIO & localIOSt=ioState})
	
	processModal :: !CProcess -> (!Bool,!CProcess)
/*	processModal localIO=:{localIOSt}
		# (optModal,ioState)= ioStGetIOIsModal localIOSt
		# (myId,ioState)	= ioStGetIOId ioState
		= (isJust optModal && myId==fromJust optModal,{localIO & localIOSt=ioState})
*/	processModal localIO=:{localIOSt}
		# (notEmpty,ioState)	= ioStHasDevices localIOSt
		= (notEmpty,{localIO & localIOSt=ioState})

handleEventForLocalIO :: !Bool !SchedulerEvent !CProcess !Context
					 -> (!Bool,!SchedulerEvent,!CProcess,!Context)
handleEventForLocalIO eventDone schedulerEvent {localState=opt_local,localIOSt=ioState} context
	# (runtime,ioState)						= ioStGetRuntimeState ioState
	| fst (rsIsBlocked runtime)
		= (eventDone,schedulerEvent,{localState=opt_local, localIOSt=ioState},context)
	# (initIO,ioState)						= ioStGetInitIO ioState
	# (dummies,pState)						= cSwitchIn (fromJust opt_local) context ioState
	# pState								= initIO pState
	# (ioKind,pState)						= accPIO ioStGetProcessKind pState
	| ioKind==VirtualProcess
		# pState							= closeVirtualProcess pState
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
	# (closed,pState)						= accPIO ioStClosed pState
	| closed
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
	| otherwise
		# (deviceFunctions,pState)			= accPIO ioStGetDeviceFunctions pState
		  ioFunctions						= [(df.dEvent,df.dDoIO) \\ df<-deviceFunctions]
		# (eventDone,schedulerEvent,pState)	= handleEventForDevices ioFunctions eventDone schedulerEvent pState
		# (local,context,ioState)			= cSwitchOut dummies pState
		= (eventDone,schedulerEvent,{localState=Just local,localIOSt=ioState},context)
where
	closeVirtualProcess :: !(PSt .l) -> PSt .l
	closeVirtualProcess pState=:{io}
		# (subids,ioState)		= ioStGetSubProcessIds io
		| not (isEmpty subids)
			= {pState & io=ioState}
		| otherwise
			# (ioStack,ioState)	= ioStGetProcessStack ioState
			# (nr,ioState)		= ioStGetIOId ioState
			  (_, ioStack)		= removeProcessShowState nr ioStack
			# ioState			= ioStSetProcessStack ioStack ioState
			# ioState			= removeIOIdFromParentProcess nr ioState
			# ioState			= ioStSetRuntimeState Closed ioState
			= {pState & io=ioState}

cSwitchIn :: !.l !Context !(IOSt .l) -> (!(![*World],!CProcesses),!PSt .l)
cSwitchIn local {cEnvs={envsEvents,envsWorld},cProcessStack,cMaxIONr,cProcesses,cModalProcess,cReceiverTable,cTimerTable,cIdTable,cOSTime,cIdSeed} ioState
	# ioState				= ioStSetProcessStack cProcessStack ioState
	# ioState				= ioStSetEvents envsEvents ioState
	# ioState				= ioStSetMaxIONr cMaxIONr ioState
	# (ioContext,ioState)	= ioStSwapIO ([envsWorld],cProcesses) ioState
	# ioState				= ioStSetIOIsModal cModalProcess ioState
	# ioState				= ioStSetIdTable cIdTable ioState
	# ioState				= ioStSetReceiverTable cReceiverTable ioState
	# ioState				= ioStSetTimerTable cTimerTable ioState
	# ioState				= ioStSetOSTime cOSTime ioState
	# ioState				= ioStSetIdSeed cIdSeed ioState
	# pState				= {ls=local,io=ioState}
	= (ioContext,pState)

cSwitchOut :: !(![*World],!CProcesses) !(PSt .l) -> (!.l,!Context,!IOSt .l)
cSwitchOut ioContext {ls,io}
	# (ostime,   ioState)		= ioStGetOSTime			io
	# (tt,       ioState)		= ioStGetTimerTable		ioState
	# (idseed,   ioState)		= ioStGetIdSeed			ioState
	# (ridlocs,  ioState)		= ioStGetReceiverTable	ioState
	# (idtable,  ioState)		= ioStGetIdTable		ioState
	# (modalId,  ioState)		= ioStGetIOIsModal		ioState
	# (ioContext,ioState)		= ioStSwapIO ioContext	ioState
	# (worlds,processes)		= ioContext
	# (maxIONr,  ioState)		= ioStGetMaxIONr		ioState
	# (ioStack,  ioState)		= ioStGetProcessStack	ioState
	# (es,	     ioState)		= ioStGetEvents			ioState
	# envs						= {envsEvents=es,envsWorld=hd worlds}
	# context					= {	cEnvs			= envs
								  ,	cProcessStack	= ioStack
								  ,	cMaxIONr		= maxIONr
								  ,	cProcesses		= processes
								  ,	cModalProcess	= modalId
								  ,	cReceiverTable	= ridlocs
								  ,	cTimerTable		= tt
								  ,	cIdTable		= idtable
								  ,	cOSTime			= ostime
								  ,	cIdSeed			= idseed
								  ,	cOSToolbox		= OSNewToolbox
								  }
	= (ls,context,ioState)

/*	handleEventForDevices in sequence lets the devices handle the scheduler event until it is handled
	or the process is terminated (ioStClosed returns True).
	Before handing over the event to the device DoIOFunction, the device first maps the event to a
	device event if possible using its EventFunction. 
*/	
handleEventForDevices :: ![(!EventFunction (PSt .l),!DoIOFunction (PSt .l))] !Bool !SchedulerEvent (PSt .l)
																		 -> (!Bool,!SchedulerEvent, PSt .l)
handleEventForDevices [(mapDeviceEvent,doDeviceIO):doIOs] eventDone schedulerEvent pState
	| eventDone
		= (eventDone,schedulerEvent,pState)
	# (closed,pState)				= accPIO ioStClosed pState
	| closed
		= (True,schedulerEvent,pState)
	# (forThisDevice,okDeviceEvent,schedulerEvent,pState)
									= mapDeviceEvent schedulerEvent pState
	| not forThisDevice
		= handleEventForDevices doIOs eventDone schedulerEvent pState
	| isNothing okDeviceEvent
		= handleEventForDevices doIOs True schedulerEvent pState
	| otherwise
		# (deviceEvent,pState)		= doDeviceIO (fromJust okDeviceEvent) pState
		# schedulerEvent			= mergeMsgEventIntoSchedulerEvent deviceEvent schedulerEvent
		= handleEventForDevices doIOs True schedulerEvent pState
where
	mergeMsgEventIntoSchedulerEvent :: !DeviceEvent !SchedulerEvent -> SchedulerEvent
	mergeMsgEventIntoSchedulerEvent (ReceiverEvent msgEvent) _	= ScheduleMsgEvent msgEvent
	mergeMsgEventIntoSchedulerEvent _ schedulerEvent			= schedulerEvent
handleEventForDevices _ eventDone schedulerEvent pState
	= (eventDone,schedulerEvent,pState)

handleOneEventForDevices :: !SchedulerEvent !(PSt .l) -> (!Bool,!SchedulerEvent,!PSt .l)
handleOneEventForDevices schedulerEvent pState
	# (deviceFunctions,pState)	= accPIO ioStGetDeviceFunctions pState
	  ioFunctions				= [(df.dEvent,df.dDoIO) \\ df<-deviceFunctions]
	= handleEventForDevices ioFunctions False schedulerEvent pState


/*	Creating interactive processes other than the initial interactive process.
	Throughout the I/O library it is assumed that all new process groups and all new data sharing
	processes are created AFTER the current interactive process. This is done for two reasons:
	-	locating offspring processes becomes more efficient (when you need to quit them). 
	-	it provides a simple additional means of communication between parent and child processes
		(piggybacking the Event` data item).
	Virtual processes (created with addVirtualProcess) evaluate only the initial actions. It is
	assumed by the system that the initial actions do not create any devices instances or set the 
	AppleMenuTitle. This MUST be taken care of by the system programmer (the system will abort if so). 
	Interactive processes (created with addInteractiveProcess) do create devices. 
*/

ShareGUI			:==	True
NotShareGUI			:==	False

//	Create a virtual process that will create other interactive processes.

addVirtualProcess :: !*(ProcessInit (PSt .l)) String .l !(PSt .l`) -> PSt .l`
addVirtualProcess ioDefInit ioDefAbout local pState
	# (nr,ioState)			= ioStNewMaxIONr pState.io
	# (parentId,ioState)	= ioStGetIOId					ioState
	# (guishare,ioState)	= getGUIShare     ShareGUI		ioState
	# ioState				= addSubProcessId ShareGUI nr	ioState
	# (ioStack, ioState)	= ioStGetProcessStack			ioState
	  ioStack				= pushProcessShowState {psId=nr,psShow=False,psKind=VirtualProcess} ioStack
	# ioState				= ioStSetProcessStack ioStack	ioState
	# (processes,ioState)	= ioStGetCProcesses				ioState
	# newIOSt				= createNewIOSt [] ioDefInit ioDefAbout nr (Just parentId) guishare ShareGUI NDI VirtualProcess
	# process				= openLocalIO newIOSt local
	# ioState				= ioStSetCProcesses (inserttodoRR process processes) ioState
	= {pState & io=ioState}


//	Create a data sharing interactive process.

addInteractiveProcess :: ![ProcessAttribute (PSt .l)] !*(ProcessInit (PSt .l)) String .l !Bool !DocumentInterface !(PSt .l`) -> PSt .l`
addInteractiveProcess pAtts ioDefInit ioDefAbout local isSubProcess documentInterface pState
	# (nr,ioState)			= ioStNewMaxIONr pState.io
	# (parentId,ioState)	= ioStGetIOId						ioState
	# (guishare,ioState)	= getGUIShare     isSubProcess		ioState
	# ioState				= addSubProcessId isSubProcess nr	ioState
	# (ioStack, ioState)	= ioStGetProcessStack				ioState
	  ioStack				= pushProcessShowState {psId=nr,psShow=True,psKind=InteractiveProcess} ioStack
	# ioState				= ioStSetProcessStack ioStack		ioState
	# (processes,ioState)	= ioStGetCProcesses					ioState
	  parent				= if isSubProcess (Just parentId) Nothing
	  pAtts					= filter (isProcessKindAttribute documentInterface) pAtts
	# newIOSt				= createNewIOSt pAtts ioDefInit ioDefAbout nr parent guishare isSubProcess documentInterface InteractiveProcess
	# process				= openLocalIO newIOSt local
	# ioState				= ioStSetCProcesses (inserttodoRR process processes) ioState
	= {pState & io=ioState}

openLocalIO :: !(IOSt .l) !.l -> CProcess
openLocalIO ioState local
	= {	localState	= Just local
	  ,	localIOSt	= ioState
	  }

getGUIShare :: !Bool !(IOSt .l) -> (!Maybe GUIShare,!IOSt .l)
getGUIShare isSubProcess ioState
	| not isSubProcess
		= (Nothing,ioState)
	# (guishare,ioState)	= ioStGetGUIShare ioState
	| isJust guishare
		= (guishare,ioState)
	# (ioKind,ioState)		= ioStGetProcessKind ioState
	| ioKind==VirtualProcess
		= (guishare,ioState)
	| otherwise
		= (guishare,ioState)

addSubProcessId :: !Bool !SystemId !(IOSt .l) -> IOSt .l
addSubProcessId isSubProcess nr ioState
	| not isSubProcess
		= ioState
	| otherwise
		# (subids,ioState)	= ioStGetSubProcessIds ioState
		  ioState			= ioStSetSubProcessIds [nr:subids] ioState
		= ioState


//	Make the proper interactive process active.

activateTopOfGroups :: !SystemId !ProcessStack !CProcesses -> (!ProcessStack,!CProcesses)
activateTopOfGroups topIONr ioStack processes
	# (emptytodo,processes)			= notodoRR processes
	| emptytodo
		= (ioStack,processes)
	# (process,processes)			= getcurrentRR processes
	  (activated,ioStack,process)	= activateTopProcess topIONr ioStack process
	| activated
		= (ioStack,inserttodoRR process processes)
	| otherwise
		# (ioStack,processes)		= activateTopOfGroups  topIONr ioStack processes
		= (ioStack,inserttodoRR process processes)
where
	activateTopProcess :: !SystemId !ProcessStack !CProcess -> (!Bool,!ProcessStack,!CProcess)
	activateTopProcess topIONr ioStack process=:{localIOSt=ioState}
		# (nr,ioState)			= ioStGetIOId ioState
		| nr<>topIONr
			= (False,ioStack ,{process & localIOSt=ioState})
		| otherwise
			# ioState			= ioStSetProcessStack ioStack ioState
			# (ioStack,ioState)	= ioStGetProcessStack ioState
			= (True,ioStack,{process & localIOSt=ioState})


/*	Quit this interactive or virtual process.
	It should be an impossible situation for the system to have to quit a blocked process (the guard should never hold).
	Quitting a process involves the following:
	-	Set the RuntimeState to Closed (quitProcess is the only function that does this)
	-	Force all sub processes to quit
	-	Inform the parent process about its termination
	-	Remove the process from the ProcessStack
	-	Close all devices
*/
quitProcess :: !(PSt .l) -> PSt .l
quitProcess pState
	# (rs,pState)					= accPIO ioStGetRuntimeState pState
	| fst (rsIsBlocked rs)
		= schedulerFatalError "quitProcess" "closeProcess applied to blocked process"
	| rsIsClosed rs
		= pState
	| otherwise
		# (deviceFunctions,pState)	= accPIO ioStGetDeviceFunctions pState
		# pState					= strictSeq [df.dClose \\ df<-deviceFunctions] pState
		# ioState					= ioStSetRuntimeState Closed     pState.io
		# (nr,ioState)				= ioStGetIOId                    ioState
		# (subids,ioState)			= ioStGetSubProcessIds           ioState
		# ioState					= quitSubProcesses subids        ioState
		# ioState					= removeIOIdFromParentProcess nr ioState
		# (ioStack,ioState)			= ioStGetProcessStack            ioState
		  (_,ioStack)				= removeProcessShowState nr      ioStack
		# ioState					= ioStSetProcessStack ioStack    ioState
		# (osdinfo,ioState)			= ioStGetOSDInfo                 ioState
		# ioState					= appIOToolbox (osCloseOSDInfo osdinfo) ioState
		= {pState & io=ioState}


/*	quitSubProcesses searches for all processes in the current process administration
	with the given ids. Each of these processes is forced to quit by setting the initialisation
	functions to [appPIO quitProcess]. In this way all recursive descendent processes will
	be quitted as well.
*/
quitSubProcesses :: ![SystemId] !(IOSt .l) -> IOSt .l
quitSubProcesses ids ioState
	# (processes,ioState)	= ioStGetCProcesses ioState
	  (_,processes)			= quitLocalSubProcesses ids processes
	# ioState				= ioStSetCProcesses processes ioState
	= ioState
where
	quitLocalSubProcesses :: ![SystemId] !CProcesses -> (![SystemId],!CProcesses)
	quitLocalSubProcesses ids processes
		| isEmpty ids
			= (ids,processes)
		| otherwise
			# (done,todo)	= fromRR processes
			  (ids,done)	= quitLocalSubProcesses` ids done
			  (ids,todo)	= quitLocalSubProcesses` ids todo
			= (ids,toRR done todo)
	where
		quitLocalSubProcesses` :: ![SystemId] !*[CProcess] -> (![SystemId],!*[CProcess])
		quitLocalSubProcesses` [] processes
			= ([],processes)
		quitLocalSubProcesses` ids []
			= (ids,[])
		quitLocalSubProcesses` ids [process=:{localState,localIOSt=ioState}:processes]
			# (ioid,ioState)		= ioStGetIOId ioState
			  (hadId,_,ids)			= remove ((==) ioid) nullSystemId ids
			| hadId
				# (subids,ioState)	= ioStGetSubProcessIds ioState
				# ioState			= ioStSetInitIO quitProcess ioState
				# process			= {localState=localState,localIOSt=ioState}
				# (ids,processes)	= quitLocalSubProcesses` (ids++subids) processes
				= (ids,[process:processes])
			| otherwise
				# process			= {localState=localState,localIOSt=ioState}
				# (ids,processes)	= quitLocalSubProcesses` ids processes
				= (ids,[process:processes])


/*	removeIOIdFromParentProcess searches for the parent process in the current process
	administration. It is a fatal error not to find this process. In the administration
	of the parent process the child process id is removed.
*/
removeIOIdFromParentProcess :: !SystemId !(IOSt .l) -> IOSt .l
removeIOIdFromParentProcess me ioState
	# (opt_parent,ioState)	= ioStGetParentId ioState
	| isNothing opt_parent
		= ioState
	# parent				= fromJust opt_parent
	# (locals,ioState)		= ioStGetCProcesses ioState
	# (done,locals)			= removeIOIdFromLocals me parent locals
	# ioState				= ioStSetCProcesses locals ioState
	| done
		= ioState
	| otherwise
		= schedulerFatalError "CloseProcess" "parent process could not be located"
where
	removeIOIdFromLocals :: !SystemId !SystemId !CProcesses -> (!Bool,!CProcesses)
	removeIOIdFromLocals me parent locals
		# (done,todo)			= fromRR locals
		  (removed,done)		= removeIOIdFromLocals` me parent done
		| removed
			= (removed,toRR done todo)
		| otherwise
			# (removed, todo)	= removeIOIdFromLocals` me parent todo
			= (removed,toRR done todo)
	where
		removeIOIdFromLocals` :: !SystemId !SystemId !*[CProcess] -> (!Bool,!*[CProcess])
		removeIOIdFromLocals` me parent [process=:{localState,localIOSt=ioState}:processes]
			# (ioid,ioState)			= ioStGetIOId ioState
			| parent==ioid
				# (subids,ioState)		= ioStGetSubProcessIds ioState
				  (_,_,subids)			= remove ((==) me) (dummy "removeIOIdFromLocals") subids
				# ioState				= ioStSetSubProcessIds subids ioState
				# process				= {localState=localState,localIOSt=ioState}
				= (True,[process:processes])
			| otherwise
				# process				= {localState=localState,localIOSt=ioState}
				# (removed,processes)	= removeIOIdFromLocals` me parent processes
				= (removed,[process:processes])
		removeIOIdFromLocals` _ _ _
			= (False,[])


//	Function threading operations:

::	SwitchError
	=	SwitchToYourself
	|	SwitchToDoesNotExist
	|	SwitchToReceiverDoesNotExist
	|	SwitchReceiverUnable
	|	SwitchEndsUpInDeadlock

/*	cswitchProcess processId msgEvent pstate switches to PSt identified by processId, lets that process 
	handle the msgEvent, and switches back to pstate.
	Nothing SwitchError is returned if no exceptions were detected. 
	Exceptions are:
	-	SwitchToYourself:				processId is the current PSt. 
	-	SwitchToDoesNotExist:			no current process can be identified by processId. 
	-	SwitchToReceiverDoesNotExist:	process  exists, but receiver doesn't.
	-	SwitchReceiverUnable:			receiver exists, but is not Able.
	-	SwitchEndsUpInDeadlock:			if this process would block for the process with 
										id processId to get unblocked, a deadlock would occur.
	In all exceptional cases the PSt remains unchanged.
	Observe that in case Nothing is returned, the ps PSt component may have changed value.
*/
cswitchProcess :: !SystemId !SchedulerEvent !(PSt .l) -> (!Maybe SwitchError,![SemiDynamic],!PSt .l)
cswitchProcess processId message pState
	| processId==returnId
		= (Just SwitchToYourself,    [],pState1)
	| not switchToExists
		= (Just SwitchToDoesNotExist,[],pState2)
	with
		context2						= {context1 & cProcesses=groups2}
		pState2							= switchToPSt typeIOSt returnId context2 local
	| inDeadlock
		= (Just SwitchEndsUpInDeadlock,[],pState2)
	with
		context2						= {context1 & cProcesses=groups3}
		pState2							= switchToPSt typeIOSt returnId context2 local
	| otherwise
		= (checkSyncMessageError message1,getSyncMessageResponse message1,pState2)
	with
		context2						= {context1 & cProcesses=groups3}
		(context3,_)					= chandleEvents (processIsBlocked processId) context2 OSNewToolbox
		(groups4,context4)				= contextGetProcesses context3
		context5						= {context4 & cProcesses=resetRR groups4}
		(message1,context6)				= handleEventForContext False message context5
		pState2							= switchToPSt typeIOSt returnId context6 local
where
	(returnId,pState1)					= accPIO ioStGetIOId pState
	(local,context,ioState)= switchFromPSt pState1
	(groups,context1)					= contextGetProcesses context
	ioState1							= ioStSetRuntimeState (Blocked processId) ioState
	(typeIOSt, ioState3)				= typeIsIOSt  ioState1
	blockedLocalIO						= {localState=Nothing,localIOSt=ioState3}
	groups1								= adddoneRR blockedLocalIO groups
	(switchToExists,groups2)			= turnRRToProcessInGroups processId groups1
	(inDeadlock,groups3)				= checkDeadlock  returnId processId groups2
	
	switchToPSt :: !(UnguardType (IOSt .l)) !SystemId !Context .l -> PSt .l
	switchToPSt typeIOSt returnId context=:{cProcesses} local
		# (_,groups)					= turnRRToProcessInGroups returnId cProcesses
		  (group,groups)				= getcurrentRR groups
		  (gDone,gToDo)					= fromRR groups
		  {localIOSt=blockedIO}			= group
		  blockedIO						= castType typeIOSt blockedIO
		  context						= {context & cProcesses=toRR gDone gToDo}
		  (_,pState)					= cSwitchIn local context (ioStSetRuntimeState Running blockedIO)
		= pState
	
	checkSyncMessageError :: !SchedulerEvent -> Maybe SwitchError
	checkSyncMessageError (ScheduleMsgEvent (SyncMessage {smError}))
		| isEmpty smError
			= Nothing
		| not (isSingleton smError)
			= schedulerFatalError "checkSyncMessageError" "more than one MessageError returned"
		| otherwise
			= case (hd smError) of
				ReceiverUnable	-> Just SwitchReceiverUnable
				ReceiverUnknown	-> Just SwitchToReceiverDoesNotExist
	checkSyncMessageError _
		= Nothing
	
	getSyncMessageResponse :: !SchedulerEvent -> [SemiDynamic]
	getSyncMessageResponse (ScheduleMsgEvent (SyncMessage {smResp}))
		= smResp
	getSyncMessageResponse _
		= []

typeIsIOSt :: !(IOSt .l) -> (UnguardType (IOSt .l),!IOSt .l)
typeIsIOSt ioState = (Unguard,ioState)

typeIsLocal :: !(IOSt .l) -> (UnguardType (Maybe .l),!IOSt .l)
typeIsLocal ioState = (Unguard,ioState)

//	PA: appContext added.
appContext :: !.(IdFun Context) !(PSt .l) -> PSt .l
appContext fun pState
	# (returnId,pState)			= accPIO ioStGetIOId pState
	# (local,context,ioState)	= switchFromPSt pState
	# (groups,context)			= contextGetProcesses context
	# (typeIOSt, ioState)		= typeIsIOSt  ioState
	# (typeLocal,ioState)		= typeIsLocal ioState
	# localIO					= {localState=Just local,localIOSt=ioState}
	# groups					= adddoneRR localIO groups
	# context					= {context & cProcesses=groups}
	# context					= fun context
	# (groups,context)			= contextGetProcesses context
	# context					= {context & cProcesses=resetRR groups}
	# pState					= switchToPSt typeIOSt typeLocal returnId context
	= pState

accContext :: !.(St Context .x) !(PSt .l) -> (!.x, !PSt .l)
accContext fun pState
	# (returnId,pState)			= accPIO ioStGetIOId pState
	# (local,context,ioState)	= switchFromPSt pState
	# (groups,context)			= contextGetProcesses context
	# (typeIOSt, ioState)		= typeIsIOSt  ioState
	# (typeLocal,ioState)		= typeIsLocal ioState
	# localIO					= {localState=Just local,localIOSt=ioState}
	# groups					= adddoneRR localIO groups
	# context					= {context & cProcesses=groups}
	# (x, context)				= fun context
	# (groups,context)			= contextGetProcesses context
	# context					= {context & cProcesses=resetRR groups}
	# pState					= switchToPSt typeIOSt typeLocal returnId context
	= (x, pState)

switchToPSt :: !(UnguardType (IOSt .l)) !(UnguardType (Maybe .l)) !SystemId !Context -> PSt .l
switchToPSt typeIOSt typeLocal returnId context=:{cProcesses}
	| not found				= schedulerFatalError "accContext" "interactive process not found"
	| closed				= snd (cSwitchIn (fromJust local1) {context1 & cModalProcess=Nothing} ioState2)
	| otherwise				= snd (cSwitchIn (fromJust local1)  context1 ioState2)
where
	(found,groups)			= turnRRToProcessInGroups returnId cProcesses
	(gDone,gToDo)			= fromRR groups
	(group,gToDo1)			= hdtl gToDo
	{localState=local,localIOSt=ioState}
							= group
	ioState1				= castType typeIOSt ioState
	local1					= castType typeLocal   local
	groups1					= toRR gDone gToDo1
	context1				= {context & cProcesses=groups1}
	(closed,ioState2)		= ioStClosed ioState1

switchFromPSt :: !(PSt .l) -> (!.l,!Context,!IOSt .l)
switchFromPSt pState
	= cSwitchOut ([],emptyRR) pState

turnRRToProcessInGroups :: !SystemId !*CProcesses -> (!Bool,!*CProcesses)
turnRRToProcessInGroups id gs
	= turnRRToProcess id (resetRR gs)
where
	turnRRToProcess :: !SystemId !*CProcesses -> (!Bool,!*CProcesses)
	turnRRToProcess id locals
		# (notodo,locals)	= notodoRR locals
		| notodo
			= (False,locals)
		# (local,locals)	= getcurrentRR locals
		  (found,local)		= turnRRToProcess` id local
		| found
			= (True,inserttodoRR local locals)
		| otherwise
			= turnRRToProcess id (adddoneRR local locals)
	where
		turnRRToProcess` :: !SystemId !*CProcess -> (!Bool,!*CProcess)
		turnRRToProcess` id l=:{localIOSt=ioState}
			# (ioid,ioState)	= ioStGetIOId ioState
			= (id==ioid,{l & localIOSt=ioState})


//	A deadlock situation arises if this process would be blocked.

checkDeadlock :: !SystemId !SystemId !*CProcesses -> (!Bool,!*CProcesses)
checkDeadlock returnId switchToId gs
	= checkDeadlock` [returnId] switchToId gs
where
	checkDeadlock` :: ![SystemId] !SystemId *CProcesses -> (!Bool,!*CProcesses)
	checkDeadlock` blockedprocs nextproc gs
		# ((nextprocfound,opt_id),gs)	= checkBlockedProcess nextproc gs
		| not nextprocfound
			= (False,gs)
		| isNothing opt_id
			= (False,gs)
		# nextproc						= fromJust opt_id
		  blockedprocs					= [nextproc:blockedprocs]
		  occurs						= contains ((==) nextproc) blockedprocs
		| occurs
			= (True, gs)
		| otherwise
			= checkDeadlock` blockedprocs nextproc gs

/*	checkBlockedProcess id groups
		locates the interactive process identified by id in groups and checks if the process is blocked.
		If this is the case then the id is returned of the process for which this process is waiting.
		If this is not the case, then no id is returned.
*/
checkBlockedProcess :: !SystemId !*CProcesses -> (!Result SystemId,!*CProcesses)
checkBlockedProcess nextproc groups
	= accessLocals (checkInLocal nextproc) groups
where
	checkInLocal :: !SystemId !*CProcess -> (!Result SystemId,!CProcess)
	checkInLocal nextproc localIO=:{localIOSt=ioState}
		# (blocked,ioState)	= checkProcess nextproc ioState
		= (blocked,{localIO & localIOSt=ioState})
	where
		checkProcess :: !SystemId !(IOSt .l) -> (!Result SystemId,!IOSt .l)
		checkProcess ioid ioState
			# (ioid`,ioState)		= ioStGetIOId ioState
			| ioid<>ioid`
				= ((False,Nothing),ioState)
			# (runtime,ioState)		= ioStGetRuntimeState ioState
			  (isBlocked,blockedFor)= rsIsBlocked runtime
			| isBlocked
				= ((True,Just blockedFor),ioState)
			| otherwise
				= ((True,Nothing),ioState)


/*	The process with SystemId id has a (Blocked id`) RuntimeState.
*/
processIsBlocked :: !SystemId !Context -> (!Bool,!Context)
processIsBlocked id context=:{cProcesses}
	# ((procfound,opt_id),groups)	= checkBlockedProcess id cProcesses
	  context						= {context & cProcesses=groups}
	| procfound						= (isJust opt_id,context)
	| otherwise						= (procfound,context)


::	Result r
	:==	(	!Bool			// object is found
		,	!Maybe r		// optional access information
		)

/*	Threading f::(IOSt .l .p) -> (Result r,IOSt .l .p) through *CProcesses 
	applies f to every IOSt member ioState of CProcesses until fst (f ioState) = (True,r) is done
	by defining the function gLocals:
	
		gLocals :: *CProcesses -> (Result r, *CProcesses)
		gLocals locals = accessLocals f locals
*/
accessLocals :: !(St CProcess (Result r)) !*CProcesses -> (!Result r,!*CProcesses)
accessLocals accLocal locals
	# (lsDone,lsToDo)	= fromRR locals
	  (rDone,lsDone) 	= accessLocalIOs accLocal lsDone
	| fst rDone
		= (rDone,toRR lsDone lsToDo)
	| otherwise
		# (rToDo,lsToDo)= accessLocalIOs accLocal lsToDo
		= (rToDo,toRR lsDone lsToDo)
where
	accessLocalIOs :: !(St CProcess (Result r)) ![*CProcess] -> (!Result r, ![*CProcess])
	accessLocalIOs accLocal [local:locals]
		# (r, local)		= accLocal local
		| fst r
			= (r, [local:locals])
		| otherwise
			# (rs,locals)	= accessLocalIOs accLocal locals
			= (rs,[local:locals])
	accessLocalIOs _ []
		= ((False,Nothing),[])

/*	The function castType is used to let the type checker assign the type
	determined by the first argument to the expression of the second argument. 
	This function contains abc code because it can't be typed conventionally. (RWS)
*/
::	UnguardType p
	=	Unguard

castType :: (UnguardType .p) .y -> .p
castType _ y = code {
	pop_a 1
	jmp_eval
	}
