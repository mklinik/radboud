implementation module devicefunctions


import	deviceevents


::	DeviceFunctions pst								// The major device callback functions:
	=	{	dDevice	:: Device						//	The device kind
		,	dShow	:: ShowFunction  pst			//	Show the device and its instances
		,	dHide	:: HideFunction  pst			//	Hide the device and its instances
		,	dEvent	:: EventFunction pst			//	Map an OSEvent to a DeviceEvent
		,	dDoIO	:: DoIOFunction  pst			//	Handle a DeviceEvent for this device
		,	dOpen	:: OpenFunction  pst			//	Open the initial device
		,	dClose	:: CloseFunction pst			//	Close the device and its instances
		}
::	ShowFunction  pst	:==	pst -> pst
::	HideFunction  pst	:==	pst -> pst
::	OpenFunction  pst	:==	pst -> pst
::	CloseFunction pst	:==	pst -> pst
::	EventFunction pst	:==	SchedulerEvent
						 ->	pst
						 ->	*(Bool, Maybe DeviceEvent, SchedulerEvent, pst)
::	DoIOFunction  pst	:==	DeviceEvent
						 -> pst
						 -> *(DeviceEvent, pst)
