definition module devicesystemstate

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import	device
import	menuhandle
import	processhandle
import	receiverhandle
import	timerhandle
import	windowhandle

::	*DeviceSystemState pst
	=	MenuSystemState     *(MenuHandles     pst)
	|	ProcessSystemState  *(ProcessHandles  pst)
	|	ReceiverSystemState *(ReceiverHandles pst)
	| 	TimerSystemState    *(TimerHandles    pst)
	|	WindowSystemState   *(WindowHandles   pst)

toDevice								:: !(DeviceSystemState .pst) -> (!Device,!DeviceSystemState .pst)

/*	The following are PARTIAL access functions:
*/
menuSystemStateGetMenuHandles			:: !(DeviceSystemState .pst) -> *MenuHandles     .pst
processSystemStateGetProcessHandles		:: !(DeviceSystemState .pst) -> *ProcessHandles  .pst
receiverSystemStateGetReceiverHandles	:: !(DeviceSystemState .pst) -> *ReceiverHandles .pst
timerSystemStateGetTimerHandles			:: !(DeviceSystemState .pst) -> *TimerHandles    .pst
windowSystemStateGetWindowHandles		:: !(DeviceSystemState .pst) -> *WindowHandles   .pst
