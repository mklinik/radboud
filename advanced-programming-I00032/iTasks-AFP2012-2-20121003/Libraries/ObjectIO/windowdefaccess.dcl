definition module windowdefaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access functions to (Window/Dialog)Definitions.
//	********************************************************************************

import	StdWindowAttribute


getWindowAttributes			:: !(Window c .ls .pst) -> (![WindowAttribute *(.ls,.pst)], !Window c .ls .pst)
getDialogAttributes			:: !(Dialog c .ls .pst) -> (![WindowAttribute *(.ls,.pst)], !Dialog c .ls .pst)
setWindowAttributes			:: ![WindowAttribute *(.ls,.pst)] !(Window c .ls .pst) -> Window c .ls .pst
setDialogAttributes			:: ![WindowAttribute *(.ls,.pst)] !(Dialog c .ls .pst) -> Dialog c .ls .pst

isAllWindowsAttribute		:: !(WindowAttribute .st) -> Bool
isWindowOnlyAttribute		:: !(WindowAttribute .st) -> Bool
isDialogOnlyAttribute		:: !(WindowAttribute .st) -> Bool
