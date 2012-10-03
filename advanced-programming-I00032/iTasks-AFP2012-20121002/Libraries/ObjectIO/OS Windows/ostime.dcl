definition module ostime

//	Clean Object I/O library, version 1.2

import StdOverloaded
import ostoolbox

::	OSTime

osMaxTime :: OSTime

osGetTime			::			!*OSToolbox -> (!OSTime,!*OSToolbox)
//	osGetTime returns the current OS time

osWait				:: !Int	.x	!*OSToolbox -> (.x,		!*OSToolbox)
//	osWait waits atleast the given time (in milliseconds).

osGetBlinkInterval	::			!*OSToolbox -> (!Int,	!*OSToolbox)
//	osGetBlinkInterval returns the recommended blink interval time of a cursor (in milliseconds).

osGetCurrentTime	::			!*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
//	osGetCurrentTime returns current (hours,minutes,seconds).

osGetCurrentDate	::			!*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
//	osGetCurrentTime returns current (year,month,day,day_of_week).

instance -       OSTime		// Calculate difference between arg 1 and arg 2
instance <       OSTime		// True iff arg 1 < arg 2
instance toInt   OSTime		// Coerce OSTime to Integer (always positive or zero)
instance fromInt OSTime		// Coerce Int to OSTime (Integer will be made zero if negative)
