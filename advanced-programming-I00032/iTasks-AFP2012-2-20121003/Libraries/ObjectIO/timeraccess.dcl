definition module timeraccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	devicesystemstate, timerhandle
from	id				import :: IdTable
from	receivertable	import :: ReceiverTable
from	timertable		import :: TimerTable



/*	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
*/
bindTimerElementIds		:: !SystemId !Id !*[*TimerElementHandle .ls .pst] !*ReceiverTable !*IdTable
							   -> (!Bool,!*[*TimerElementHandle .ls .pst],!*ReceiverTable,!*IdTable)

/*	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates.
*/
unbindTimerElementIds	:: !SystemId !*[*TimerElementHandle .ls .pst] !(!*TimerTable,!*ReceiverTable,!*IdTable)
																	-> (!*TimerTable,!*ReceiverTable,!*IdTable)

identifyTimerStateHandle:: !Id !*(TimerStateHandle .pst) -> *(!Bool,!*TimerStateHandle .pst)
