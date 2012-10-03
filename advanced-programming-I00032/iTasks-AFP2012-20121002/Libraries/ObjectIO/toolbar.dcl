definition module toolbar

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import iostate

/*	openToolbar
		creates the requested toolbar for a MDI or SDI process. 
		openToolbar has no effect in case IOSt belongs to a NDI process. 
		openToolbar causes a run-time error if a toolbar already exists.
*/
openToolbar		:: !(IOSt .l) -> IOSt .l
