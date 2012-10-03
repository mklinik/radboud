definition module windowupdate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Update actions on windows/dialogues and controls.
//	********************************************************************************


import	ospicture, ossystem, ostoolbox, ostypes
import	windowhandle, wstate
from	deviceevents	import :: UpdateInfo


/*	updatewindow redraws the controls indicated by the UpdateInfo.
*/
updatewindow :: !OSWindowMetrics !UpdateInfo !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)

/*	updatewindowbackgrounds(`) redraws the (window/compound) backgrounds that are inside the OSRgnHandle argument.
	After redrawing the OSRgnHandle argument is disposed!
*/
updatewindowbackgrounds :: !OSWindowMetrics !OSRgnHandle !WIDS !(WindowHandle .ls .pst) !*OSToolbox
															-> (!WindowHandle .ls .pst, !*OSToolbox)
updatewindowbackgrounds`:: !OSWindowMetrics !OSRgnHandle !WIDS  !WindowHandle` !*OSToolbox
															-> (!WindowHandle`,!*OSToolbox)

/*	updaterectcontrols redraws the controls that are element of the argument window and inside the OSRect.
*/
updaterectcontrols :: !OSWindowMetrics !OSRect !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
													     -> (!WindowHandle .ls .pst, !*OSToolbox)
