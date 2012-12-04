definition module controlresize


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	ossystem
import	wstate


resizeControls :: !OSWindowMetrics !Bool !Bool !WIDS !Origin !Size !Size !(WindowHandle .ls .pst) !*OSToolbox
																	  -> (!WindowHandle .ls .pst, !*OSToolbox)
/*	resizeControls wMetrics isActive updateAll wids oldOrigin oldSize newSize theWindow
		recalculates the size and positions of the controls of the window.
		The wMetrics		argument are the window metrics of the platform.
		The isActive		argument isActive is True iff theWindow is the active window.
		The updateAll		argument updateAll is True if theWindow must be completely updated.
		The wids			argument identifies the window.
		The oldOrigin		argument must be the old origin of the window.
		The oldSize			argument must be the old size of the window (excluding scrollbars). (Note: can be larger than old view frame!)
		The newSize			argument must be the new size of the window (excluding scrollbars). (Note: can be larger than new view frame!)
		The theWindow		argument must not be a window placeholder. 
	resizeControls assumes that the window itself already has the proper new size.
*/

resizeControl :: !OSWindowMetrics !Bool !WIDS !Id !Size !WindowHandle` !*OSToolbox
													-> (!WindowHandle`,!*OSToolbox)
/*	resizeControls wMetrics updateAll wids controlId newSize theWindow
		updates the size of the control indicated by controlId to newSize and recalculates the positions of all controls iff updateAll is set. 
		The wMetrics		argument are the window metrics of the platform.
		The updateAll		argument updateAll is True if theWindow must be completely updated.
		The wids			argument identifies the window.
		The controlId		argument identifies the control.
		The newSize			argument is the new size of the control.
		The theWindow		argument must not be a window placeholder. 
*/
