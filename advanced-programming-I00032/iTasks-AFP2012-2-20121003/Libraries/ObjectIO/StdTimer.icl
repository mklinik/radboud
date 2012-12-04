implementation module StdTimer


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, id, iostate, timeraccess, timerdefaccess, timerdevice, timertable
import	StdId, StdTimerAttribute, StdTimerElementClass
from StdPSt import appPIO, accPIO


stdTimerFatalError :: String String -> .x
stdTimerFatalError function error
	= fatalError function "StdTimer" error


//	Open timer:

class Timers tdef where
	openTimer	:: .ls !.(tdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getTimerType::      .(tdef .ls .pst)               -> TimerType

instance Timers (Timer t) | TimerElements t where
	openTimer ls (Timer period items atts) pState
		# pState				= timerFunctions.dOpen pState
		# (maybe_okId,pState)	= accPIO (validateTimerId maybeId) pState
		| isNothing maybe_okId
			= (ErrorIdsInUse,pState)
		# (ok,timers,ioState)	= ioStGetTimerHandles pState.io
		| not ok				// This condition should never hold: TimerDevice has just been 'installed'
			= stdTimerFatalError "openTimer (Timer)" "could not retrieve TimerSystemState from IOSt"
		# (pid,ioState)			= ioStGetIOId ioState
		# pState				= {pState & io=ioState}
		  id					= fromJust maybe_okId
		# (ts,pState)			= timerElementToHandles items pState
		  itemHs				= map timerElementStateToTimerElementHandle ts
		# (it,ioState)			= ioStGetIdTable pState.io
		# (rt,ioState)			= ioStGetReceiverTable ioState
		  (ok,itemHs,rt,it)		= bindTimerElementIds pid id itemHs rt it
		| not ok
			# ioState			= ioStSetDevice (TimerSystemState timers) ioState
			# ioState			= ioStSetIdTable it ioState
			# ioState			= ioStSetReceiverTable rt ioState
			= (ErrorIdsInUse,{pState & io=ioState})
		| otherwise
			= (NoError,pState2)
		with
			tH					= {	tId			= id
								  ,	tSelect		= ableTimer
								  ,	tPeriod		= max 0 period
								  ,	tFun		= f
								  ,	tItems		= itemHs
								  }
			tsH					= TimerLSHandle {tState=ls1,tHandle=tH}
			tLoc				= {	tlIOId		= pid
								  ,	tlDevice	= TimerDevice
								  ,	tlParentId	= id
								  ,	tlTimerId	= id
								  }
			it1					= snd (addIdToIdTable id {idpIOId=pid,idpDevice=TimerDevice,idpId=id} it)
			ioState1			= addAbleTimerToTimerTable ableTimer tLoc period ioState
			ioState2			= ioStSetReceiverTable rt ioState1
			ioState3			= ioStSetIdTable it1 ioState2
			ioState4			= ioStSetDevice (TimerSystemState {timers & tTimers=[tsH:timers.tTimers]}) ioState3
			pState1				= {pState & io=ioState4}
			(ls1,pState2)		= timerInit (ls,pState1)
	where
		(hasIdAtt,idAtt)		= cselect isTimerId undef atts
		maybeId					= if hasIdAtt (Just (getTimerIdAtt idAtt)) Nothing
		ableTimer				= enabled (getTimerSelectStateAtt (snd (cselect isTimerSelectState (TimerSelectState Able) atts)))
		f						= getTimerFun (snd (cselect isTimerFunction (TimerFunction (\_ st->st)) atts))
		timerInit				= getTimerInitFun (snd (cselect isTimerInit (TimerInit id) atts))
		
		validateTimerId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
		validateTimerId Nothing ioState
			# (tId,ioState)			= openId ioState
			= (Just tId,ioState)
		validateTimerId (Just id) ioState
			# (it,ioState)			= ioStGetIdTable ioState
			# (member,it)			= memberIdTable id it
			| member				= (Nothing,ioStSetIdTable it ioState)
			| otherwise				= (Just id,ioStSetIdTable it ioState)
		
		addAbleTimerToTimerTable :: !Bool !TimerLoc !TimerInterval !(IOSt .l) -> IOSt .l
		addAbleTimerToTimerTable True tLoc period ioState
			# (tt,ioState)			= ioStGetTimerTable ioState
			# (_,tt)				= addTimerToTimerTable tLoc period tt
			= ioStSetTimerTable tt ioState
		addAbleTimerToTimerTable _ _ _ ioState
			= ioState
	
	getTimerType _
		= "Timer"

eqTimerStateHandleId :: !Id !(TimerStateHandle .pst) -> (!Bool,!TimerStateHandle .pst)
eqTimerStateHandleId id tsH=:(TimerLSHandle {tHandle={tId}})
	= (id==tId,tsH)


//	Close timer:

closeTimer :: !Id !(IOSt .l) -> IOSt .l
closeTimer id ioState
	# (ok,tHs,ioState)		= ioStGetTimerHandles ioState
	| not ok
		= ioState
	| otherwise
		# (pid,ioState)		= ioStGetIOId ioState
		# (rt,ioState)		= ioStGetReceiverTable ioState
		# (tt,ioState)		= ioStGetTimerTable ioState
		# (it,ioState)		= ioStGetIdTable ioState
		  (rt,tt,it,tsHs)	= closetimer id pid rt tt it tHs.tTimers
		# ioState			= ioStSetIdTable it ioState
		# ioState			= ioStSetReceiverTable rt ioState
		# ioState			= ioStSetTimerTable tt ioState
		  tHs				= {tHs & tTimers=tsHs}
		# ioState			= ioStSetDevice (TimerSystemState tHs) ioState
		= ioState
where
	closetimer :: !Id !SystemId !*ReceiverTable !*TimerTable !*IdTable ![TimerStateHandle .pst]
							-> (!*ReceiverTable,!*TimerTable,!*IdTable,![TimerStateHandle .pst])
	closetimer id pid rt tt it [tsH:tsHs]
		# (eqid,tsH)		= eqTimerStateHandleId id tsH
		| eqid
			# (tt1,rt1,it1)	= disposeElementIds pid tsH tt rt it
			= (rt1,tt1,it1,tsHs)
		| otherwise
			# (rt,tt,it,tsHs)	= closetimer id pid rt tt it tsHs
			= (rt,tt,it,[tsH:tsHs])
	where
		disposeElementIds :: !SystemId !(TimerStateHandle .pst) !*TimerTable !*ReceiverTable !*IdTable
															-> (!*TimerTable,!*ReceiverTable,!*IdTable)
		disposeElementIds pid (TimerLSHandle {tHandle={tId,tItems}}) tt rt it
			# (tt,rt,it)	= unbindTimerElementIds pid tItems (tt,rt,it)
			= (snd (removeTimerFromTimerTable teLoc tt),rt,snd (removeIdFromIdTable tId it))
		where
			teLoc			= {tlIOId=pid,tlDevice=TimerDevice,tlParentId=tId,tlTimerId=tId}
	closetimer _ _ rt tt it _
		= (rt,tt,it,[])


//	Get the Ids and TimerTypes of all timers:

getTimers :: !(IOSt .l) -> (![(Id,TimerType)],!IOSt .l)
getTimers ioState
	# (ok,tHs,ioState)		= ioStGetTimerHandles ioState
	| not ok
		= ([],ioState)
	| otherwise
		# (idtypes,timers)	= getidtypes tHs.tTimers
		  tHs				= {tHs & tTimers=timers}
		# ioState			= ioStSetDevice (TimerSystemState tHs) ioState
		= (idtypes,ioState)
where
	getidtypes :: ![TimerStateHandle .pst] -> (![(Id,TimerType)],![TimerStateHandle .pst])
	getidtypes [TimerLSHandle tlsH=:{tHandle=tH}:tsHs]
		# (idtype, tH)		= getidtype  tH
		  (idtypes,tsHs)	= getidtypes tsHs
		= ([idtype:idtypes],[TimerLSHandle {tlsH & tHandle=tH}:tsHs])
	where
		getidtype :: !(TimerHandle .ls .pst) -> ((Id,TimerType),!TimerHandle .ls .pst)
		getidtype tH=:{tId}
			= ((tId,"Timer"),tH)
	getidtypes _
		= ([],[])


//	Enabling and Disabling of timers:

enableTimer :: !Id !(IOSt .l) -> IOSt .l
enableTimer id ioState
	= changeTimer id enabletimer ioState
where
	enabletimer :: TimerLoc !*TimerTable !(TimerStateHandle .pst) -> (!*TimerTable, !TimerStateHandle .pst)
	enabletimer teLoc tt tlsH=:(TimerLSHandle tsH=:{tHandle=tH=:{tSelect,tPeriod}})
		| tSelect
			= (tt,tlsH)
		| otherwise
			# (_,tt)	= addTimerToTimerTable teLoc tPeriod tt
			= (tt,TimerLSHandle {tsH & tHandle={tH & tSelect=True}})

disableTimer :: !Id !(IOSt .l) -> IOSt .l
disableTimer id ioState
	= changeTimer id disabletimer ioState
where
	disabletimer :: TimerLoc !*TimerTable !(TimerStateHandle .pst) -> (!*TimerTable, !TimerStateHandle .pst)
	disabletimer teLoc tt tlsH=:(TimerLSHandle tsH=:{tHandle=tH=:{tSelect}})
		| not tSelect
			= (tt,tlsH)
		| otherwise
			# (_,tt)= removeTimerFromTimerTable teLoc tt
			= (tt,TimerLSHandle {tsH & tHandle={tH & tSelect=False}})


//	Get the SelectState of timers:

getTimerSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getTimerSelectState id ioState
	# (ok,tHs,ioState)			= ioStGetTimerHandles ioState
	| not ok
		= (Nothing,ioState)
	| otherwise
		# (maybe_select,timers)	= gettimerselect id tHs.tTimers
		  tHs					= {tHs & tTimers=timers}
		# ioState				= ioStSetDevice (TimerSystemState tHs) ioState
		= (maybe_select,ioState)
where
	gettimerselect :: !Id ![TimerStateHandle .pst] -> (!Maybe SelectState, ![TimerStateHandle .pst])
	gettimerselect id [tsH=:(TimerLSHandle {tHandle={tId,tSelect}}):tsHs]
		| id==tId
			= (Just (if tSelect Able Unable),[tsH:tsHs])
		| otherwise
			# (optselect,tsHs)	= gettimerselect id tsHs
			= (optselect,[tsH:tsHs])
	gettimerselect _ _
		= (Nothing,[])


//	Set the TimerInterval of timers:

setTimerInterval :: !Id !TimerInterval !(IOSt .l) -> IOSt .l
setTimerInterval id interval ioState
	= changeTimer id (settimerinterval interval) ioState
where
	settimerinterval :: !TimerInterval !TimerLoc !*TimerTable !(TimerStateHandle .pst) -> (!*TimerTable, !TimerStateHandle .pst)
	settimerinterval period teLoc tt tlsH=:(TimerLSHandle tsH=:{tHandle=tH=:{tSelect,tPeriod}})
		# period		= max 0 period
		| period==tPeriod
			= (tt,tlsH)
		# tlsH			= TimerLSHandle {tsH & tHandle={tH & tPeriod=period}}
		| not tSelect
			= (tt,tlsH)
		| otherwise
			# (_,tt)	= setIntervalInTimerTable teLoc period tt
			= (tt,tlsH)


//	Get the TimerInterval of timers:

getTimerInterval :: !Id !(IOSt .l) -> (!Maybe TimerInterval,!IOSt .l)
getTimerInterval id ioState
	# (ok,tHs,ioState)		= ioStGetTimerHandles ioState
	| not ok
		= (Nothing,ioState)
	# (optinterval,timers)	= gettimerinterval id tHs.tTimers
	  tHs					= {tHs & tTimers=timers}
	# ioState				= ioStSetDevice (TimerSystemState tHs) ioState
	= (optinterval,ioState)
where
	gettimerinterval :: !Id ![TimerStateHandle .pst] -> (!Maybe TimerInterval, ![TimerStateHandle .pst])
	gettimerinterval id [tsH=:(TimerLSHandle {tHandle={tId,tPeriod}}):tsHs]
		| id==tId
			= (Just tPeriod,[tsH:tsHs])
		| otherwise
			# (optselect,tsHs)	= gettimerinterval id tsHs
			= (optselect,  [tsH:tsHs])
	gettimerinterval _ _
		= (Nothing,[])


ioStGetTimerHandles :: !(IOSt .l) -> (!Bool,TimerHandles (PSt .l), !IOSt .l)
ioStGetTimerHandles ioState
	# (found,tDevice,ioState) = ioStGetDevice TimerDevice ioState
	| not found
		= (False,undef,ioState)
	| otherwise
		= (True,timerSystemStateGetTimerHandles tDevice,ioState)


//	General TimerHandle changing function:

::	DeltaTimerStateHandle pst
	:== TimerLoc -> *TimerTable -> *((TimerStateHandle pst) -> *(*TimerTable,TimerStateHandle pst))

changeTimer :: !Id !(DeltaTimerStateHandle (PSt .l)) !(IOSt .l) -> IOSt .l
changeTimer id f ioState
	# (ok,tHs,ioState)	= ioStGetTimerHandles ioState
	| not ok
		= ioState
	| otherwise
		# (tt,  ioState)= ioStGetTimerTable   ioState
		# (ioid,ioState)= ioStGetIOId         ioState
		  (tt,tHs)		= changetimerdevice ioid id f tt tHs
		# ioState		= ioStSetDevice (TimerSystemState tHs) ioState
		# ioState		= ioStSetTimerTable tt ioState
		= ioState
where
	changetimerdevice :: SystemId !Id (DeltaTimerStateHandle .pst) !*TimerTable !(TimerHandles .pst)
															   -> (!*TimerTable, !TimerHandles .pst)
	changetimerdevice ioid id f tt timers=:{tTimers=tsHs}
		# (tt,tsHs)		= changetimerstatehandles ioid id f tt tsHs
		= (tt,{timers & tTimers=tsHs})
	where
		changetimerstatehandles :: SystemId !Id (DeltaTimerStateHandle .pst) !*TimerTable ![TimerStateHandle .pst]
																		 -> (!*TimerTable,![TimerStateHandle .pst])
		changetimerstatehandles ioid id f tt [tsH=:(TimerLSHandle {tHandle={tId}}):tsHs]
			| id==tId
				= (tt1,[tsH1:tsHs])
			with
				teLoc		= {tlIOId=ioid,tlDevice=TimerDevice,tlParentId=id,tlTimerId=id}
				(tt1,tsH1)	= f teLoc tt tsH
			| otherwise
				= (tt1,[tsH:tsHs1])
			with
				(tt1,tsHs1)	= changetimerstatehandles ioid id f tt tsHs
		changetimerstatehandles _ _ _ tt []
			= (tt,[])
