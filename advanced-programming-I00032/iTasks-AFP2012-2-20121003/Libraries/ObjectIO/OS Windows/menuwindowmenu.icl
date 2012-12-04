implementation module menuwindowmenu

//	The definition and implementation of the WindowMenu. 
//	PA: implementation not required on Windows platform.

from	iostate import :: IOSt, :: PSt
import	windowhandle


/*	openWindowMenu creates the WindowMenu. This menu contains atleast the following elements:
	-	MenuItem "&Cascade":
			Reorder the current list of windows from left-top to right-bottom.
	-	MenuItem "Tile &Horizontally":
			Reorder the current list of windows from top to bottom.
	-	MenuItem "&Tile Vertically":
			Reorder the current list of windows from left to right.
	-	MenuSeparator
	-	RadioMenu:
			Display all current open windows (hidden and shown). Selection activates and
			shows the indicated window.
*/
openWindowMenu :: !(PSt .l) -> PSt .l
openWindowMenu pState = pState
	
/*	addWindowToWindowMenu adds a new item to the RadioMenu of the WindowMenu if present. 
	The Id argument is the id of the window that should be added, and the Title argument its title. 
*/
addWindowToWindowMenu :: !Id !Title !(PSt .l) -> PSt .l
addWindowToWindowMenu windowId windowTitle pState = pState

/*	removeWindowFromWindowMenu removes the window entry from the WindowMenu if present.
*/
removeWindowFromWindowMenu :: !Id !(IOSt .l) -> IOSt .l
removeWindowFromWindowMenu wId ioState = ioState

changeWindowInWindowMenu :: !Id !String !(IOSt .l) -> IOSt .l
changeWindowInWindowMenu wId title ioState = ioState

/*	validateWindowActivateForWindowMenu takes care that if this interactive process is an MDI process,
	and the WindowLSHandle represents a Windows instance that the WindowActivate function of the
	WindowLSHandle will select the proper RadioMenuItem of the WindowMenu if present before any other 
	actions are taken.
*/
validateWindowActivateForWindowMenu` :: !Id !Bool ![WindowAttribute *(.ls,PSt .p)] -> [WindowAttribute *(.ls,PSt .p)]
validateWindowActivateForWindowMenu` wId isMDI atts = atts

/*	PA: apparantly this function is not required.
validateWindowActivateForWindowMenu :: !Id !(WindowLSHandle .ls (PSt .l)) !(IOSt .l)
										-> (!WindowLSHandle .ls (PSt .l),  !IOSt .l)
validateWindowActivateForWindowMenu wId dlsH=:{wlsHandle=dH=:{whAtts,whKind}} ioState = (dlsH,ioState)
*/
