definition module device

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import StdOverloaded, StdString

::	Device						// The set of devices
	=	TimerDevice
	|	MenuDevice
	|	WindowDevice
	|	ReceiverDevice
	|	ProcessDevice

/* RWS +++ from deviceaccess */
instance ==			Device
instance toString	Device

priorityDevice	:: !Device -> Int
devices			:: [Device]		// The device list in order of descending priority
