implementation module osevent

import	StdBool, StdList, StdMisc, StdTuple
import	clCrossCall_12, ostime, ostoolbox, ostypes
from	commondef	import hdtl, fatalError
from	StdMaybe	import :: Maybe(..)
//import	StdDebug, tracetypes	// PA: for tracing purposes


oseventFatalError :: String String -> .x
oseventFatalError function error
	= fatalError function "osevent" error


/*	The OSEvents environment keeps track of delayed events. 
*/
::	*OSEvents
	:== [OSEvent]


osAppendEvents :: !*[OSEvent] !OSEvents -> OSEvents
osAppendEvents newEvents osEvents
	= osEvents ++ newEvents

osInsertEvents :: !*[OSEvent] !OSEvents -> OSEvents
osInsertEvents newEvents osEvents
	= newEvents ++ osEvents

osIsEmptyEvents :: !OSEvents -> (!Bool,!OSEvents)
osIsEmptyEvents []
	= (True,  [])
osIsEmptyEvents osEvents
	= (False, osEvents)

osRemoveEvent :: !OSEvents -> (!OSEvent,!OSEvents)
osRemoveEvent [osEvent:osEvents]
	= (osEvent,osEvents)
osRemoveEvent []
	= oseventFatalError "osRemoveEvent" "OSEvents argument is empty"

/*	PA: does not seem to be used.
osCopyEvents :: !OSEvents -> (!OSEvents,!OSEvents)
osCopyEvents []
	= ([],[])
osCopyEvents [e:es]
	= ([e:es1],[e:es2])
where
	(es1,es2)	= osCopyEvents es
*/

osNewEvents :: OSEvents
osNewEvents = []


::	OSEvent
	:==	CrossCallInfo
::	OSSleepTime		// The max time the process allows multi-tasking
	:== Int

osNullEvent :: OSEvent
osNullEvent
	=	{	ccMsg	= CcWmIDLETIMER
		,	p1		= 0
		,	p2		= 0
		,	p3		= 0
		,	p4		= 0
		,	p5		= 0
		,	p6		= 0
		}

// OSLongSleep :: OSSleepTime
OSLongSleep	:== 2^15-1
// OSNoSleep :: OSSleepTime
OSNoSleep	:== 0

osHandleEvents :: !(.s -> (Bool,.s)) !(.s -> (OSEvents,.s)) !((OSEvents,.s) -> .s) !(.s -> (Int,.s)) !(OSEvent -> .s -> ([Int],.s)) !(!.s,!*OSToolbox) -> (!.s,!*OSToolbox)

osHandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)
	# (terminate,state)			= isFinalState state
	| terminate
		= (state,tb)
	# (osEvents,state)			= getOSEvents state
	# (noDelayEvents,osEvents)	= osIsEmptyEvents osEvents
	| noDelayEvents
		# state					= setOSEvents (osEvents,state)
		# (sleep,state)			= getSleepTime state
		  getEventCci			= {ccMsg=CcRqDOMESSAGE,p1=toInt (sleep<>OSLongSleep),p2=sleep,p3=0,p4=0,p5=0,p6=0}
		# (_,state,tb)			= issueCleanRequest (rccitoevent handleOSEvent) getEventCci state tb
		= osHandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)
		with
			rccitoevent :: !(OSEvent -> .s -> ([Int],.s)) !OSEvent !.s !*OSToolbox -> (!OSEvent,!.s,!*OSToolbox)
			rccitoevent handleOSEvent osEvent=:{ccMsg} state tb
//				# (reply,state)	= handleOSEvent (trace_n ("CcRqDOMESSAGE-->"+++toCleanCrossCallInfoString osEvent) osEvent) state
				# (reply,state)	= handleOSEvent osEvent state
				= (setReplyInOSEvent reply,state,tb)
	| otherwise
		# (osEvent,osEvents)	= osRemoveEvent osEvents
		# state					= setOSEvents (osEvents,state)
//		# (_,state)				= handleOSEvent (trace_n ("DelayedEvent-->"+++toCleanCrossCallInfoString osEvent) osEvent) state
		# (_,state)				= handleOSEvent osEvent state
		= osHandleEvents isFinalState getOSEvents setOSEvents getSleepTime handleOSEvent (state,tb)

setReplyInOSEvent :: ![Int] -> OSEvent
setReplyInOSEvent reply
	| isEmpty reply	= return0Cci
	# (e1,reply)	= hdtl reply
	| isEmpty reply	= return1Cci e1
	# (e2,reply)	= hdtl reply
	| isEmpty reply	= return2Cci e1 e2
	# (e3,reply)	= hdtl reply
	| isEmpty reply	= return3Cci e1 e2 e3
	# (e4,reply)	= hdtl reply
	| isEmpty reply	= return4Cci e1 e2 e3 e4
	# (e5,reply)	= hdtl reply
	| isEmpty reply	= return5Cci e1 e2 e3 e4 e5
	# (e6,_)		= hdtl reply
	| isEmpty reply	= return6Cci e1 e2 e3 e4 e5 e6
	| otherwise		= oseventFatalError "setReplyInOSEvent" "number of reply codes > 6"

osEventIsUrgent :: !OSEvent -> Bool
osEventIsUrgent {ccMsg}
	= case ccMsg of
		CcWmDRAWCLIPBOARD	-> False	// PA: in a future version, use this event to evaluate a clipboard callback function.
		CcWmIDLETIMER		-> False
		CcWmTIMER			-> False
		CcWmZEROTIMER		-> False
		_					-> True


/*	createOS(Dea/A)ctivateWindowEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateWindowEvent :: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSActivateWindowEvent wPtr tb = (Rq1Cci CcWmACTIVATE wPtr,tb)

createOSDeactivateWindowEvent :: !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateWindowEvent wPtr tb = (Rq1Cci CcWmDEACTIVATE wPtr,tb)

/*	createOS(Dea/A)ctivateControlEvent creates the event the platform would generate for a genuine (de)activate event. */
createOSActivateControlEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSActivateControlEvent wPtr cPtr tb = (Rq2Cci CcWmSETFOCUS wPtr cPtr,tb)

createOSDeactivateControlEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSDeactivateControlEvent wPtr cPtr tb = (Rq2Cci CcWmKILLFOCUS wPtr cPtr,tb)

/*	createOSLoose(Mouse/Key)Event creates the event for reporting loss of mouse/keyboard input (virtual event). */
createOSLooseMouseEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseMouseEvent wPtr cPtr tb = (Rq2Cci CcWmLOSTMOUSE wPtr cPtr,tb)

createOSLooseKeyEvent :: !OSWindowPtr !OSWindowPtr !*OSToolbox -> (!OSEvent,!*OSToolbox)
createOSLooseKeyEvent wPtr cPtr tb = (Rq2Cci CcWmLOSTKEY wPtr cPtr,tb)

/*	createOSZeroTimerEvent  creates the event for reporting continued zero timer (virtual event).
	getOSZeroTimerStartTime returns the registered time in the virtual event. Zero if wrong argument.
*/
createOSZeroTimerEvent :: !OSTime -> OSEvent
createOSZeroTimerEvent zeroStart = Rq1Cci CcWmZEROTIMER (toInt zeroStart)

getOSZeroTimerStartTime :: !OSEvent -> Maybe OSTime
getOSZeroTimerStartTime {ccMsg,p1}
	| ccMsg==CcWmZEROTIMER
		= Just (fromInt p1)
	| otherwise
		= Nothing
