implementation module timerevent


import	StdBool, StdClass
import	deviceevents, timeraccess
from	commondef	import fatalError, ucontains, :: UCond
from	iostate		import :: PSt{..}, :: IOSt, ioStHasDevice, ioStGetDevice, ioStSetDevice, ioStGetIOId
from	StdPSt		import accPIO


timereventFatalError :: String String -> .x
timereventFatalError function error
	= fatalError function "timerevent" error


/*	The timerEvent function determines whether the given SchedulerEvent can be applied
	to a timer of this process. These are the following cases:
	*	ScheduleTimerEvent: the timer event belongs to this process and device
	*	ScheduleMsgEvent:   the message event belongs to this process and device
	timerEvent assumes that it is not applied to an empty IOSt.
*/
timerEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
timerEvent schedulerEvent pState
	# (hasDevice,pState)	= accPIO (ioStHasDevice TimerDevice) pState
	| not hasDevice			// This condition should never occur: TimerDevice must have been 'installed'
		= timereventFatalError "TimerFunctions.dEvent" "could not retrieve TimerSystemState from IOSt"
	| otherwise
		= timerEvent schedulerEvent pState
where
	timerEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
	timerEvent schedulerEvent=:(ScheduleTimerEvent te=:{teLoc}) pState=:{io=ioState}
		# (ioid,ioState)	= ioStGetIOId ioState
		| teLoc.tlIOId<>ioid || teLoc.tlDevice<>TimerDevice
			= (False,Nothing,schedulerEvent,{pState & io=ioState})
		# (_,timer,ioState)	= ioStGetDevice TimerDevice ioState
		# timers			= timerSystemStateGetTimerHandles timer
		  (found,timers)	= lookForTimer teLoc.tlParentId timers
		# ioState			= ioStSetDevice (TimerSystemState timers) ioState
		# pState			= {pState & io=ioState}
		| found
			#! deviceEvent	= TimerEvent te
			= (True,Just deviceEvent,schedulerEvent,pState)
		| otherwise
			= (False,Nothing,schedulerEvent,pState)
	where
		lookForTimer :: !Id !(TimerHandles .pst) -> (!Bool,!TimerHandles .pst)
		lookForTimer parent timers=:{tTimers=tHs}
			# (found,tHs)	= ucontains (identifyTimerStateHandle parent) tHs
			= (found,{timers & tTimers=tHs})
	
	timerEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState
		# (ioid,pState)		= accPIO ioStGetIOId pState
		  recloc			= case msgEvent of
							  	(QASyncMessage {qasmRecLoc}) -> qasmRecLoc
							  	(ASyncMessage  { asmRecLoc}) -> asmRecLoc
							  	(SyncMessage   {  smRecLoc}) -> smRecLoc
		| ioid==recloc.rlIOId && TimerDevice==recloc.rlDevice
			= (True,Just (ReceiverEvent msgEvent),schedulerEvent,pState)
		| otherwise
			= (False,Nothing,schedulerEvent,pState)
	
	timerEvent schedulerEvent pState
		= (False,Nothing,schedulerEvent,pState)
