definition module timertable

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import StdMaybe
import systemid, device
import StdTimerDef

::	TimerTable									// The table of all active timers
::	TimerLoc
	=	{	tlIOId			:: !SystemId		// Id of parent process
		,	tlDevice		:: !Device			// Device kind of parent
		,	tlParentId		:: !Id				// Id of parent device instance
		,	tlTimerId		:: !Id				// Id of the timer itself
		}
::	TimerEvent
	=	{	teLoc			:: !TimerLoc		// The timer that should be evaluated
		,	teNrInterval	:: !NrOfIntervals	// The nr of timer intervals that have elapsed
		}

/*	initialTimerTable yields an empty TimerTable.
	The Integer argument is the current tick count of the system.
*/
initialTimerTable :: *TimerTable

/*	addTimerToTimerTable adds a new timer entry to the TimerTable.
	The Boolean result is True iff no duplicate timer entry was found, otherwise it is False.
*/
addTimerToTimerTable :: !TimerLoc !TimerInterval !*TimerTable -> (!Bool,!*TimerTable)

/*	removeTimerFromTimerTable removes a timer from the TimerTable.
	The Boolean result is True iff an entry was actually removed, otherwise it is False.
*/
removeTimerFromTimerTable :: !TimerLoc !*TimerTable -> (!Bool,!*TimerTable)

/*	setIntervalInTimerTable changes the timerinterval of the given timer in the TimerTable.
	If the timer was not present in the table, then nothing happens (the Boolean result is False).
	If the timer was present, its entry has been changed (the Boolean result is True).
*/
setIntervalInTimerTable :: !TimerLoc !TimerInterval !*TimerTable -> (!Bool,!*TimerTable)

/*	shiftTimeInTimerTable dt shifts the TimerTable dt (>0) ticks forward in time. 
*/
shiftTimeInTimerTable :: !Int !*TimerTable -> *TimerTable

/*	getActiveTimerInTimerTable determines the next timer that should be evaluated given the current
	TimerTable. Such a timer is any timer with a negative or zero elapsed time. 
	If such a timer could be found, then getActiveTimerInTimerTable returns its timer location and 
		number of fully elapsed timer intervals. The timer in question is placed behind all further
		timers, creating a round-robin evaluation order.
	If such a timer could not be found, then Nothing is returned. 
*/
getActiveTimerInTimerTable :: !*TimerTable -> (!Maybe TimerEvent,!*TimerTable)

/*	getTimeIntervalFromTimerTable returns the (Just (zerotimer,time)) interval that can be 
	waited for the next timer to become active. The Boolean zerotimer holds iff the time
	returned belongs to a zero timer.
	If there are no timers, then Nothing is returned.
*/
getTimeIntervalFromTimerTable :: !*TimerTable -> (!Maybe (Bool,Int),!*TimerTable)
