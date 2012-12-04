definition module menuevent


//	Clean Object I/O library, version 1.2

/*	menuevent defines the DeviceEventFunction for the menu device.
	This function is placed in a separate module because it is platform dependent.
*/


import	deviceevents, devicesystemstate, menuhandle
from	iostate	import :: PSt
from	osmenu	import :: OSTrackPopUpMenu


menuEvent		:: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)

/*	For pop up menu's an alternative way to determine the DeviceEvent is required:
*/
popUpMenuEvent	:: !OSTrackPopUpMenu !(MenuStateHandle .ps) !*OSToolbox -> (!Maybe DeviceEvent, !MenuStateHandle .ps, !*OSToolbox)

//menuHandlesGetMenuStateHandles :: !(MenuHandles .pst) -> (![MenuStateHandle .pst], !MenuHandles .pst)
// PA: not used
