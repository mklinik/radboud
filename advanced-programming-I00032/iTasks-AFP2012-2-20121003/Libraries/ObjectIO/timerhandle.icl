implementation module timerhandle


import	StdBool, StdList
import	commondef, receiverhandle, receivertable
import	StdTimerDef


::	*TimerElementState ls pst								// The internal implementation of a timer
	:==	*TimerElementHandle ls pst							// is a TimerElementHandle

::	*TimerHandles pst
	=	{	tTimers	:: !*[*TimerStateHandle pst]			// The timers of a process
		}
::	*TimerStateHandle pst
	=	E. .ls: TimerLSHandle !*(TimerLSHandle ls pst)		// A timer with local state
::	*TimerLSHandle ls pst
	=	{	tState	:: ls									// The local state of this timer
		,	tHandle	:: !*TimerHandle ls pst					// The timer implementation
		}
::	*TimerHandle ls pst
	=	{	tId		:: !Id									// The Id attribute or generated system Id of the timer
		,	tSelect	:: !Bool								// The TimerSelect==Able (by default True)
		,	tPeriod	:: !Int									// The interval time in ticks
		,	tFun	:: !TimerFunction *(ls,pst)				// The TimerFunction, optionally with local state
		,	tItems	:: !*[*TimerElementHandle ls pst]		// The elements of the timer
		}
::	*TimerElementHandle ls pst
	=	TimerReceiverHandle	*(TimerReceiverHandle	ls pst)
	|	TimerListLSHandle	*[TimerElementHandle	ls pst]
	|	TimerElimLSHandle	*[TimerElementHandle	ls pst]
	|	TimerIntroLSHandle	*(TimerIntroLSHandle	ls pst)
	|	TimerExtendLSHandle	*(TimerExtendLSHandle	ls pst)
	|	TimerChangeLSHandle	*(TimerChangeLSHandle	ls pst)
::	TimerReceiverHandle ls pst
	=	{	tReceiverHandle	:: ReceiverHandle ls pst
		,	tReceiverAtts	:: [TimerAttribute *(ls,pst)]
		}
::	*TimerIntroLSHandle	ls pst
	=	E. .ls1:
		{	tIntroLS		:: ls1
		,	tIntroItems		:: *[*TimerElementHandle ls1 pst]
		}
::	*TimerExtendLSHandle	ls pst
	=	E. .ls1:
		{	tExtendLS		:: ls1
		,	tExtendItems	:: *[*TimerElementHandle *(ls1,ls) pst]
		}
::	*TimerChangeLSHandle	ls pst
	=	E. .ls1:
		{	tChangeLS		:: ls1
		,	tChangeItems	:: *[*TimerElementHandle ls1 pst]
		}


//	Conversion functions from TimerElementState to TimerElementHandle, and vice versa:

timerElementHandleToTimerElementState :: !*(TimerElementHandle .ls .pst) -> *TimerElementState .ls .pst
timerElementHandleToTimerElementState tHs = tHs

timerElementStateToTimerElementHandle :: !*(TimerElementState .ls .pst) -> *TimerElementHandle .ls .pst
timerElementStateToTimerElementHandle tHs = tHs
