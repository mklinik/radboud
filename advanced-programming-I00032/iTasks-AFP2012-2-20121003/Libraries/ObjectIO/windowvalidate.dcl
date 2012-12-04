definition module windowvalidate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Window validation.
//	********************************************************************************


import	windowhandle
import	osdocumentinterface, ossystem, ostoolbox
from	iostate				import :: IOSt


validateWindowId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
/*	validateWindowId checks whether the Id of the window/dialogue has already been bound.
	If so, Nothing is returned; otherwise a proper Id value for the window/dialogue is returned.
	The Id is not bound.
*/

validateWindow :: !OSWindowMetrics !OSDInfo !(WindowHandle .ls .pst) !(WindowHandles .pst) !*OSToolbox
				->	(	!Index
					,	!Point2
					,	!Size
					,	!Vector2
					,	!WindowHandle  .ls .pst
					,	!WindowHandles .pst
					,	!*OSToolbox
					)
/*	validateWindow validates the given WindowHandle.
	It assumes that the Ids of the window and controls have already been validated.
	It returns the valid Index, its position and size, and the origin offset of the window.
*/

exactWindowSize :: OSWindowMetrics ViewDomain !Size Bool Bool !WindowKind -> Size
/*	exactWindowSize determines the exact size of a window/dialog (argument 6).
	The Size argument is the view frame size. This size is extended to fit in scroll bars if requested 
	(argument 4 - horizontal - and 5 - vertical -).
	The Size result can then be used in exactWindowPos below.
	Arguments 1,2, 4, and 5 are evaluated only if WindowKind==IsWindow.
*/

exactWindowPos :: !OSWindowMetrics !OSDInfo !Size !(Maybe ItemPos) !WindowKind !WindowMode !(WindowHandles .pst) !*OSToolbox
																	           -> (!Point2, !WindowHandles .pst, !*OSToolbox)
/*	exactWindowPos determines the exact position of a window.
	The size argument must be the exact size of the window.
	The ItemPos argument must be a valid ItemPos attribute. It should not be one of (LeftOf/RightTo/Above/Below)Prev. 
*/

validateViewDomain :: !ViewDomain -> ViewDomain
/*	validateViewDomain ensures that the ViewDomain is inside viewDomainRange. 
	The result value {corner1,corner2} is such that corner1<corner2 (corner1 is left and above of corner2).
*/
