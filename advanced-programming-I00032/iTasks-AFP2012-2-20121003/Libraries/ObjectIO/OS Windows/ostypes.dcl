definition module ostypes

//	********************************************************************************
//	Clean Standard Object I/O library, version 1.2.2
//	
//	Standard types for the OS layer
//
//	Author: Peter Achten
//	Modified: 8 October 2001 for Clean 2.0
//	********************************************************************************

::	OSPictContext					// PA: moved from ospicture by DvA
	:== Int // HDC
::	HDC								// PA: moved from pictCCall_12
	:== Int
::	OSRect							// A OSRect is supposed to be an ordered rectangle with
	=	{	rleft		:: !Int		// rleft<=rright && rtop<=rbottom
		,	rtop		:: !Int
		,	rright		:: !Int
		,	rbottom		:: !Int
		}
::	OSWindowPtr
	:== Int // HWND
::	HWND
	:==	Int

OSNoWindowPtr	:== -1

::	DelayActivationInfo
	=	DelayActivatedWindow	OSWindowPtr				// the window has become active
	|	DelayDeactivatedWindow	OSWindowPtr				// the window has become inactive
	|	DelayActivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become active
	|	DelayDeactivatedControl	OSWindowPtr OSWindowPtr	// the control (@2) in window (@1) has become inactive
