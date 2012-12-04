definition module windowevent


//	Clean Object I/O library, version 1.2

/*	windowevent defines the DeviceEventFunction for the window device.
	This function is placed in a separate module because it is platform dependent.
*/


import	deviceevents
from	iostate import :: PSt


windowEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
