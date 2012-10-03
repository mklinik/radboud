definition module windowdispose


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	windowhandle
from	iostate				import :: InputTrack, :: PSt
from	windowaccess		import :: WID
import	osdocumentinterface, osevent, ostoolbox, oswindow


disposeWindow				:: !WID !(PSt .l) -> PSt .l
/*	disposeWindow disposes all system resources associated with the indicated window if it exists.
*/

//disposeCursorInfo			:: !CursorInfo !(IOSt .l) -> IOSt .l
/*	disposeCursorInfo disposes all system resources associated with the given CursorInfo.
	PA: not yet implemented
*/

disposeWindowStateHandle	:: !OSDInfo !(Maybe InputTrack) !(OSEvent -> .s -> ([Int],.s)) !*(!*WindowStateHandle .pst,.s) !*OSToolbox
      -> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS],!OSDInfo,!Maybe InputTrack),!*(!*WindowStateHandle .pst,.s),!*OSToolbox)
//disposeWindowStateHandle :: !OSDInfo !(Maybe InputTrack) !(OSEvent -> .s -> ([Int],.s)) !*(!OSDInfo,!*WindowStateHandle .pst,.s) !*OSToolbox
//			-> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS],!Maybe InputTrack),!*(!OSDInfo,!*WindowStateHandle .pst,.s),!*OSToolbox)
/*	disposeWindowStateHandle disposes all system resources associated with the given WindowStateHandle.
	It returns the freed receiver ids, control ids, delayed (de)activation event, and the final local modal dialog state.
	(When timers are also part of windows, timer ids will also be returned.)
*/

disposeWItemHandle			::  !OSWindowPtr !Point2 !(WItemHandle .ls .pst) !*OSToolbox
                    -> (![Id],![Id],!IdFun *OSToolbox,!WItemHandle .ls .pst, !*OSToolbox)
/*	disposeWItemHandle returns all freed receiver ids and a function that (recursively) disposes all system 
	resources associated with the given WItemHandle. 
	(When timers are also part of windows, timer ids will also be returned.)
	The OSWindowPtr argument must identify the parent window.
	The Point2 argument must be the exact position of the parent GUI element of this WItemHandle.
*/
