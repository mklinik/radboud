implementation module ostypes

::	OSPictContext					// PA: moved from ospicture by DvA
	:== Int // HDC
::	HDC								// PA: moved from pictCCall_12
	:== Int
::	OSRect							// A OSRect is supposed to be an ordered rectangle with
	=	{	rleft	:: !Int			// rleft<=rright && rtop<=rbottom
		,	rtop	:: !Int
		,	rright	:: !Int
		,	rbottom	:: !Int
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
