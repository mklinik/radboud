implementation module devicesystemstate


import commondef, device
import timerhandle
import menuhandle
import processhandle
import windowhandle
import receiverhandle


devicesystemstateFatalError :: String String -> .x
devicesystemstateFatalError rule error
	= fatalError rule "devicesystemstate" error


::	*DeviceSystemState pst
	=	MenuSystemState		*(MenuHandles     pst)
	|	ProcessSystemState	*(ProcessHandles  pst)
	|	ReceiverSystemState	*(ReceiverHandles pst)
	| 	TimerSystemState	*(TimerHandles    pst)
	|	WindowSystemState	*(WindowHandles   pst)

toDevice :: !(DeviceSystemState .pst) -> (!Device,!DeviceSystemState .pst)
toDevice ds=:(MenuSystemState     _) = (MenuDevice,    ds)
toDevice ds=:(ProcessSystemState  _) = (ProcessDevice, ds)
toDevice ds=:(ReceiverSystemState _) = (ReceiverDevice,ds)
toDevice ds=:(TimerSystemState    _) = (TimerDevice,   ds)
toDevice ds=:(WindowSystemState   _) = (WindowDevice,  ds)


menuSystemStateGetMenuHandles :: !(DeviceSystemState .pst) -> *MenuHandles .pst
menuSystemStateGetMenuHandles (MenuSystemState msHs)
	= msHs
menuSystemStateGetMenuHandles _
	= devicesystemstateFatalError "menuSystemStateGetMenuHandles" "argument is no MenuSystemState"

processSystemStateGetProcessHandles :: !(DeviceSystemState .pst) -> *ProcessHandles .pst
processSystemStateGetProcessHandles (ProcessSystemState psHs)
	= psHs
processSystemStateGetProcessHandles _
	= devicesystemstateFatalError "processSystemStateGetProcessHandles" "argument is no ProcessSystemState"

receiverSystemStateGetReceiverHandles :: !(DeviceSystemState .pst) -> *ReceiverHandles .pst
receiverSystemStateGetReceiverHandles (ReceiverSystemState rsHs)
	= rsHs
receiverSystemStateGetReceiverHandles _
	= devicesystemstateFatalError "receiverSystemStateGetReceiverHandles" "argument is no ReceiverSystemState"

timerSystemStateGetTimerHandles :: !(DeviceSystemState .pst) -> *TimerHandles .pst
timerSystemStateGetTimerHandles (TimerSystemState tsHs)
	= tsHs
timerSystemStateGetTimerHandles _
	= devicesystemstateFatalError "timerSystemStateGetTimerHandles" "argument is no TimerSystemState"

windowSystemStateGetWindowHandles :: !(DeviceSystemState .pst) -> *WindowHandles .pst
windowSystemStateGetWindowHandles (WindowSystemState wsHs)
	= wsHs
windowSystemStateGetWindowHandles _
	= devicesystemstateFatalError "windowSystemStateGetWindowHandles" "argument is no WindowSystemState"
