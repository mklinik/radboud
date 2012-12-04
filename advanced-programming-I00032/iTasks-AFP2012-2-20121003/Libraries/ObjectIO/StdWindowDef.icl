implementation module StdWindowDef


import	StdIOCommon, StdPictureDef


::	Dialog c ls pst = Dialog Title (c ls pst) [WindowAttribute *(ls,pst)]
::	Window c ls pst = Window Title (c ls pst) [WindowAttribute *(ls,pst)]

::	WindowAttribute	st							// Default:
 //	Attributes for Windows and Dialogs:
	=	WindowActivate		(IdFun st)			// id
	|	WindowClose			(IdFun st)			// user can't close window
	|	WindowDeactivate	(IdFun st)			// id
	|	WindowHMargin		Int Int				// system dependent
	|	WindowId			Id					// system defined id
	|	WindowIndex			Int					// open front-most
	|	WindowInit			(IdFun st)			// no actions after opening window
	|	WindowInitActive	Id					// system dependent
	|	WindowItemSpace		Int Int				// system dependent
	|	WindowOuterSize		Size				// screen size
	|	WindowPos			ItemPos				// system dependent
	|	WindowViewSize		Size				// screen size
	|	WindowVMargin		Int Int				// system dependent
 //	Attributes for Dialog only:	
	|	WindowCancel		Id					// no cancel  (Custom)ButtonControl
	|	WindowOk			Id					// no default (Custom)ButtonControl
 //	Attributes for Windows only:	
	| 	WindowCursor		CursorShape			// no change of cursor
	|	WindowHScroll		ScrollFunction		// no horizontal scrolling
	|	WindowKeyboard		KeyboardStateFilter SelectState (KeyboardFunction st)
												// no keyboard input
	|	WindowLook			Bool Look			// show system dependent background
	|	WindowMouse			MouseStateFilter    SelectState (MouseFunction    st)
												// no mouse input
	|	WindowOrigin		Point2				// left top of picture domain
	|	WindowPen			[PenAttribute]		// default pen attributes
	|	WindowSelectState	SelectState			// Able
	|	WindowViewDomain	ViewDomain			// {zero,max range}
	|	WindowVScroll		ScrollFunction		// no vertical   scrolling

::	WindowType
	:==	String
