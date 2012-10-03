definition module osrgn

//	Clean Object I/O library, version 1.2

import ostoolbox, ostypes

::	OSRgnHandle
	:==	Int

//	Region creation and disposal operations.
osnewrgn	::                                        !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osnewrectrgn:: !OSRect                                !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osdisposergn::                           !OSRgnHandle !*OSToolbox -> *OSToolbox

//	Setting the shape of a region to a rectangle or a polygon.
/*	PA: used nowhere.
osrectrgn	:: !OSRect                   !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
*/
ospolyrgn	:: !(!Int,!Int) ![(Int,Int)] !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)

/*	Combining the shapes of the two argument regions into a new region.
	The argument regions are not changed.
*/
ossectrgn	:: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osunionrgn	:: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)
osdiffrgn	:: !OSRgnHandle !OSRgnHandle !*OSToolbox -> (!OSRgnHandle,!*OSToolbox)

/*	Region property access functions.
	osgetrgnbox  retrieves the bounding box of the region. The Bool is True iff
				 the bounding box equals the region.
	osisemptyrgn determines whether the region is empty (its bounding box is empty). 
*/
osgetrgnbox	:: !OSRgnHandle !*OSToolbox -> (!Bool,!OSRect,!*OSToolbox)
osisemptyrgn:: !OSRgnHandle !*OSToolbox -> (!Bool,		  !*OSToolbox)
