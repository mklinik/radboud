definition module receiverevent


//	Clean Object I/O library, version 1.2

//	receiverevent defines the DeviceEventFunction for the receiver device.
//	This function is placed in a separate module because it is platform dependent.


import	deviceevents
from	iostate import :: PSt


receiverEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
