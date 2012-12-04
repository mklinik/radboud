definition module StdTime


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdTime contains time related operations.
//	********************************************************************************

from	StdOverloaded	import class <
from	ostick			import :: Tick
from	StdLibMisc		import :: Time(..), :: Date(..)


wait				:: !Int .x -> .x

/*	wait n x suspends the evaluation of x modally for n ticks.
	If n<=0, then x is evaluated immediately.
*/

instance < Tick

intPlusTick			::	!Int  !Tick	-> Tick
tickDifference		::	!Tick !Tick	-> Int

class TimeEnv env where
	getBlinkInterval:: !*env -> (!Int,	!*env)
	getCurrentTime	:: !*env -> (!Time,	!*env)
	getCurrentDate	:: !*env -> (!Date,	!*env)
	getCurrentTick	:: !*env -> (!Tick,	!*env)
/*	getBlinkInterval
		returns the time interval in ticks that should elapse between blinks of 
		e.g. a cursor. This interval may be changed by the user while the 
		interactive process is running!
	getCurrentTime
		returns the current Time.
	getCurrentDate
		returns the current Date.
	getCurrentTick
		returns the current Tick.
*/

instance TimeEnv World
