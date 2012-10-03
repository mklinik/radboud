definition module timerdefaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access function to timerDefinitions.
//	********************************************************************************

import	StdTimerDef


timerDefGetAttributes	:: !(Timer t .ls .pst) -> [TimerAttribute *(.ls,.pst)]
timerDefSetAbility		:: !SelectState					!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetInterval		:: !TimerInterval				!(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetFunction		:: !(TimerFunction *(.ls,.pst))	!(Timer t .ls .pst) -> Timer t .ls .pst
