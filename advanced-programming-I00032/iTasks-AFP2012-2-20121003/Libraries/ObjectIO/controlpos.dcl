definition module controlpos


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	ossystem, ostoolbox
import	windowhandle


/*	movewindowviewframe moves the current view frame of the WindowHandle by the given Vector. 
*/
movewindowviewframe	:: !OSWindowMetrics !Vector2 !WIDS !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst, !*OSToolbox)

