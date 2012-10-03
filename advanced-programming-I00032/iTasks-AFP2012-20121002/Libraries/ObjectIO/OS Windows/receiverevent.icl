implementation module receiverevent

import	StdBool
import	deviceevents, iostate, /*MW11*/ clCrossCall_12 
from StdPSt import accPIO


/*	receiverEvent filters the appropriate events for the receiver device.
	These are only the message events (as long as receivers do not contain timers).
	receiverEvent assumes that it is not applied to an empty IOSt.
*/
receiverEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
receiverEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState
	# (ioid,pState)	= accPIO ioStGetIOId pState
	  recloc		= case msgEvent of
		  				(QASyncMessage {qasmRecLoc}) -> qasmRecLoc
		  				(ASyncMessage  { asmRecLoc}) -> asmRecLoc
		  				(SyncMessage   {  smRecLoc}) -> smRecLoc
	| ioid==recloc.rlIOId && ReceiverDevice==recloc.rlDevice
		= (True, Just (ReceiverEvent msgEvent),schedulerEvent,pState)
	| otherwise
		= (False,Nothing,schedulerEvent,pState)
// MW11..
receiverEvent schedulerEvent=:(ScheduleOSEvent {ccMsg=CcWmINETEVENT,p1,p2,p3,p4} _) pState
	= (True, Just (InternetEvent (p1,p2,p3,p4)), schedulerEvent, pState)
// ..MW11
receiverEvent schedulerEvent pState
	= (False,Nothing,schedulerEvent,pState)
