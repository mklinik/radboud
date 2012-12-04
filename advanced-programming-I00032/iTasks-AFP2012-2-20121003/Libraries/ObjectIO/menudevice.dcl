definition module menudevice


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	devicefunctions
from	iostate import :: IOSt, :: PSt


menuFunctions			:: DeviceFunctions (PSt .l)

ioStIsActive			:: !(IOSt .l) -> (!Bool,!IOSt .l)
activateMenuSystem		:: !(IOSt .l) -> IOSt .l
