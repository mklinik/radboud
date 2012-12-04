definition module StdProcess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdProcess contains the process creation and manipulation functions.
//	********************************************************************************


import	StdProcessDef
from	iostate import :: PSt, :: IOSt


/*	General process topology creation functions:
*/

class Processes pdef where
	startProcesses	:: !*pdef !*World   -> *World
	openProcesses	:: !*pdef !(PSt .l) -> PSt .l
/*	(start/open)Processes creates an interactive process topology specified by
		the pdef argument. 
		All interactive processes can communicate with each other by means of the 
		file system or by message passing. 
	startProcesses terminates as soon as all interactive processes that are 
		created by startProcesses and their child processes have terminated. 
	openProcesses schedules the interactive processes specified by the pdef argument 
		to be created. 
*/

instance Processes [*pdef]	| Processes pdef
instance Processes Process

startIO :: !DocumentInterface !.l !(ProcessInit (PSt .l))
							  ![ProcessAttribute (PSt .l)]
			!*World -> *World
/*	startIO creates one process group of one interactive process. 
*/


//	Process access operations:

closeProcess		:: !(PSt .l) -> PSt .l
/*	closeProcess removes all abstract devices that are held in the interactive 
	process.
	If the interactive process has processes that share its GUI then these will also
	be closed recursively. As a result evaluation of this interactive process 
	including GUI sharing processes will terminate.
*/


hideProcess			:: !(PSt .l) -> PSt .l
showProcess			:: !(PSt .l) -> PSt .l
/*	If the interactive process is active, hideProcess hides the interactive process, 
	and showProcess makes it visible. Note that hiding an interactive process does 
	NOT disable the process but simply makes it invisible.
*/

getProcessWindowPos	:: !(IOSt .l) -> (!Point2,!IOSt .l)
/*	getProcessWindowPos returns the current position of the ProcessWindow.
*/

getProcessWindowSize:: !(IOSt .l) -> (!Size,!IOSt .l)
/*	getProcessWindowSize returns the current size of the ProcessWindow.
*/
