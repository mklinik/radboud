definition module windowcreate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

from	iostate		import :: PSt, :: IOSt
import	windowhandle
import	osfont, ossystem, oswindow


/*	Open a window.
	The Id             argument is the validated Id of the window/dialogue.
	The WindowLSHandle argument should be an initialised handle of the new window. 
	The window/dialogue will be filled with concrete controls and a concrete window will be created. 
	After opening the window its optional initial actions are also evaluated.
*/
openwindow			:: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> PSt .l
openmodalwindow		:: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!Maybe .ls,!PSt .l)

/*	createModalDialogControls wMetrics wPtr windows
		Replaces the OSWindowPtr of the modal dialog that is identified by a zero OSWindowPtr from windows.
		If such a modal dialog could not be found, then a runtime error is generated.
		Then it takes care that the controls of the indicated modal dialog are created.
	The return OSWindowPtr is of the control that has the initial input focus.
	NOTE: this function is also used in windowevent.icl
*/
createModalDialogControls :: !OSWindowMetrics !OSWindowPtr !*(WindowHandles .pst) !*OSToolbox
										  -> (!OSWindowPtr, !*WindowHandles .pst, !*OSToolbox)

/*	bufferDelayedEvents buffers the activate/deactivate events.
*/
bufferDelayedEvents	:: ![DelayActivationInfo] !(IOSt .l) -> IOSt .l

/*	WindowBound-checks for normal windows.
*/
checkZeroWindowBound:: !(IOSt .l) -> (!Bool,!IOSt .l)
decreaseWindowBound	:: !(IOSt .l) -> IOSt .l
