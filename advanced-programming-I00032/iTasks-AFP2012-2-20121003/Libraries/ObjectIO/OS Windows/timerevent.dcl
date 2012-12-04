definition module timerevent


//	Clean Object I/O library, version 1.2

/*	timerevent defines the DeviceEventFunction for the timer device.
	This function is placed in a separate module because it is platform dependent.
*/


import	deviceevents
from	iostate import :: PSt


timerEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
