definition module processevent


//	Clean Object I/O library, version 1.2

//	processevent defines the DeviceEventFunction for the process device.
//	This function is placed in a separate module because it is platform dependent.


import	deviceevents
from	iostate import :: PSt


processEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
