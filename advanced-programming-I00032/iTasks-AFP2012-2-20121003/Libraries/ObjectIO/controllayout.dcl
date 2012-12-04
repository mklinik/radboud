definition module controllayout


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	ossystem, ostoolbox, ostypes
import	wstate


layoutControls	:: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle .ls .pst] !*OSToolbox
																									-> (!Size,![WElementHandle .ls .pst],!*OSToolbox)
layoutControls`	:: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle`] !*OSToolbox
																									-> (!Size,![WElementHandle`],!*OSToolbox)
/*	layoutControls(`) calculates the layout of the controls given the horizontal and vertical margins, item spaces, 
	prefered size, minimum size of the area in which the controls are positioned. The list of (ViewDomain,Point2) contains
	the current view domains and origins of all parent objects in ascending order. 
	The result size is the actual size.
*/

calcControlsSize:: !OSWindowMetrics !(!Int,!Int) !(!Int,!Int) !(!Int,!Int) !Size !Size ![(ViewDomain,Point2)] ![WElementHandle .ls .pst] !*OSToolbox
																															   -> (!Size,!*OSToolbox)
/*	calcControlsSize is identical to layoutControls, except that only the size is calculated. 
*/
