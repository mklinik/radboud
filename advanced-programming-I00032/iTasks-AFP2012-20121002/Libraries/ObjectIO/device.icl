implementation module device

import StdOverloaded, StdString

::	Device						// The set of devices
	= 	TimerDevice
	|	MenuDevice
	|	WindowDevice
	|	ReceiverDevice
	|	ProcessDevice

instance == Device where
	(==) TimerDevice		device	= case device of
										TimerDevice		-> True
										_				-> False
	(==) MenuDevice 		device	= case device of
										MenuDevice		-> True
										_				-> False
	(==) WindowDevice		device	= case device of
										WindowDevice	-> True
										_				-> False
	(==) ReceiverDevice		device	= case device of
										ReceiverDevice	-> True
										_				-> False
	(==) ProcessDevice		device	= case device of
										ProcessDevice	-> True
										_				-> False
	(==) _					_		= False

instance toString Device where
	toString TimerDevice		= "TimerDevice"
	toString MenuDevice			= "MenuDevice"
	toString WindowDevice		= "WindowDevice"
	toString ReceiverDevice		= "ReceiverDevice"
	toString ProcessDevice		= "ProcessDevice"

priorityDevice :: !Device -> Int
priorityDevice ReceiverDevice	= 6
priorityDevice TimerDevice		= 4
priorityDevice MenuDevice		= 3
priorityDevice WindowDevice		= 2
priorityDevice ProcessDevice	= 0

devices	:: [Device]
devices							// The device list in order of descending priority
	= [	ReceiverDevice
	  ,	TimerDevice
	  ,	MenuDevice
	  ,	WindowDevice
	  ,	ProcessDevice
	  ]
