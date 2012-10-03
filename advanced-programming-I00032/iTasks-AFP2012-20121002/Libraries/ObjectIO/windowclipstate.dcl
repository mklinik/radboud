definition module windowclipstate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	wstate
import	ossystem


disposeClipState:: !ClipState !*OSToolbox -> *OSToolbox
/*	disposeClipState disposes the system resources associated with the ClipState.
	The ClipState should not be used anymore after this operation.
*/

validateWindowClipState   :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
validateWindowClipState`  :: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,        !*OSToolbox)
forceValidWindowClipState :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
forceValidWindowClipState`:: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle`           !*OSToolbox -> (!WindowHandle`,        !*OSToolbox)
/*	validateWindowClipState(`) wMetrics allClipStates wPtr windowHandle
		checks that the optional ClipState of the window is valid (clipOk field is True).
		If not, the ClipState is recalculated and the former value is disposed(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
	forceValidWindowClipState(`) wMetrics allClipStates wPtr windowHandle
		always recalculates the ClipState and disposes the former value(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
*/

invalidateWindowClipState   :: !(WindowHandle .ls .pst)	-> WindowHandle .ls .pst
invalidateWindowClipState`  :: !WindowHandle`			-> WindowHandle`
/*	validateWindowClipState(`) invalidate the ClipState of the given window.
*/

validateCompoundClipState   :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
validateCompoundClipState`  :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !WItemHandle`           !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
forceValidCompoundClipState :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
forceValidCompoundClipState`:: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !WItemHandle`           !*OSToolbox -> (!WItemHandle`,        !*OSToolbox)
/*	validateCompoundClipState(`) wMetrics allClipStates wPtr parentPos defaultId isVisible compoundControl
		checks that the optional ClipState of the compound control is valid (clipOk field is True).
		If not, the ClipState is recalculated and the former value is disposed(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
	forceValidCompoundClipState(`) wMetrics allClipStates wPtr parentPos defaultId isVisible compoundControl
		always recalculates the ClipState and disposes the former value(!!).
		In case allClipStates holds, then this is done recursively for all compound controls.
	The isVisible argument is True iff the argument compound control is currently visible.
	The parentPos argument must be exact position of the parent GUI element of the argument control.
*/

invalidateCompoundClipState :: !(WItemHandle .ls .pst) -> WItemHandle .ls .pst
invalidateCompoundClipState`:: !WItemHandle`		   -> WItemHandle`
/*	invalidateCompoundClipState(`) invalidate the ClipState of the given compound control.
*/
