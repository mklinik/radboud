definition module StdTimer


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdTimer specifies all timer operations.
//	********************************************************************************


import	StdTimerElementClass, StdMaybe
from	StdSystem	import ticksPerSecond
from	iostate		import :: PSt, :: IOSt


class Timers tdef where
	openTimer	:: .ls !.(tdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getTimerType::      .(tdef .ls .pst)               -> TimerType
/*	Open a new timer.
	This function has no effect in case the interactive process already contains a 
	timer with the same Id. In case TimerElements are opened with duplicate Ids, the
	timer will not be opened. Negative TimerIntervals are set to zero.
	In case the timer does not have an Id, it will obtain an Id which is fresh with 
	respect to the current set of timers. The Id can be reused after closing this 
	timer.
*/

instance Timers (Timer t)	| TimerElements	t


closeTimer :: !Id !(IOSt .l) -> IOSt .l
/*	closeTimer closes the timer with the indicated Id.
*/


getTimers  ::     !(IOSt .l) -> (![(Id,TimerType)],!IOSt .l)
/*	getTimers returns the Ids and TimerTypes of all currently open timers.
*/


enableTimer			:: !Id !(IOSt .l) -> IOSt .l
disableTimer		:: !Id !(IOSt .l) -> IOSt .l
getTimerSelectState	:: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
/*	(en/dis)ableTimer (en/dis)ables the indicated timer.
	getTimerSelectState yields the SelectState of the indicated timer. If the timer 
	does not exist, then Nothing is yielded.
*/


setTimerInterval	:: !Id !TimerInterval	!(IOSt .l) -> IOSt .l
getTimerInterval	:: !Id					!(IOSt .l)
					-> (!Maybe TimerInterval,!IOSt .l)
/*	setTimerInterval
		sets the TimerInterval of the indicated timer. 
		Negative TimerIntervals are set to zero.
	getTimerInterval
		yields the TimerInterval of the indicated timer. 
		If the timer does not exist, then Nothing is yielded.
*/
