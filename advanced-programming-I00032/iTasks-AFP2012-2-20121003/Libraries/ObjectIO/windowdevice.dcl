definition module windowdevice


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


from	iostate		import :: PSt
import	ossystem, ostoolbox
import	devicefunctions, windowhandle


windowFunctions			:: DeviceFunctions (PSt .l)

windowStateSizeAction	:: !OSWindowMetrics !Bool !WindowSizeActionInfo !(WindowStateHandle .pst) !*OSToolbox
																	 -> (!WindowStateHandle .pst, !*OSToolbox)
/*	windowStateSizeAction wMetrics isActive info
		resizes the argument window. The isActive Boolean is True iff the window is the active window. 
*/

//-- PA: exported because oswindow:osCreateModalDialog requires this function.

windowStateInitialiseIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
						-> (!WindowStateHandle (PSt .l),  PSt .l)
