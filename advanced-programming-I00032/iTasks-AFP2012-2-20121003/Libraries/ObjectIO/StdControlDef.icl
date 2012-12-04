implementation module StdControlDef


import	StdIOCommon, StdPictureDef


::	ButtonControl       ls pst = ButtonControl       String                                           [ControlAttribute *(ls,pst)]
::	CheckControl        ls pst = CheckControl        [CheckControlItem *(ls,pst)] RowsOrColumns       [ControlAttribute *(ls,pst)]
::	CompoundControl   c ls pst = CompoundControl     (c ls pst)                                       [ControlAttribute *(ls,pst)]
::	CustomButtonControl ls pst = CustomButtonControl Size Look                                        [ControlAttribute *(ls,pst)]
::	CustomControl       ls pst = CustomControl       Size Look                                        [ControlAttribute *(ls,pst)]
::	EditControl         ls pst = EditControl         String ControlWidth NrLines                      [ControlAttribute *(ls,pst)]
::	LayoutControl     c ls pst = LayoutControl       (c ls pst)                                       [ControlAttribute *(ls,pst)]
::	PopUpControl        ls pst = PopUpControl        [PopUpControlItem *(ls,pst)]               Index [ControlAttribute *(ls,pst)]
::	RadioControl        ls pst = RadioControl        [RadioControlItem *(ls,pst)] RowsOrColumns Index [ControlAttribute *(ls,pst)]
::	SliderControl       ls pst = SliderControl       Direction ControlWidth SliderState               (SliderAction     *(ls,pst))
                                                                                                      [ControlAttribute *(ls,pst)]
::	TextControl         ls pst = TextControl         String                                           [ControlAttribute *(ls,pst)]

::	CheckControlItem st	:==	(String, Maybe ControlWidth, MarkState, IdFun st)
::	PopUpControlItem st	:==	(String,                                IdFun st)
::	RadioControlItem st	:==	(String, Maybe ControlWidth,            IdFun st)
::	NrLines				:==	Int
::	RowsOrColumns
	=	Rows			Int
	|	Columns			Int
::	ControlWidth									// The width of the control:
	=	PixelWidth		Int							// the exact number of pixels
	|	TextWidth		String						// the exact string width in dialog font
	|	ContentWidth	String						// width of the control as if string is its content

::	ControlAttribute st                             // Default:
 // General control attributes:
	=	ControlActivate     (IdFun             st)  // id
	|	ControlDeactivate   (IdFun             st)  // id
	|	ControlFunction     (IdFun             st)  // id
	|	ControlHide                                 // initially visible
	|	ControlId           Id                      // no id
	|	ControlKeyboard     KeyboardStateFilter SelectState (KeyboardFunction st)
		                                            // no keyboard input/overruled
	|	ControlMinimumSize  Size                    // zero
	|	ControlModsFunction (ModifiersFunction st)  // ControlFunction
	|	ControlMouse        MouseStateFilter    SelectState (MouseFunction st)
		                                            // no mouse input/overruled
	|	ControlPen			[PenAttribute]			// default pen attributes
	|	ControlPos          ItemPos                 // (RightTo previous,zero)
	|	ControlResize       ControlResizeFunction   // no resize
	|	ControlSelectState  SelectState             // control Able
	|	ControlTip          String                  // no tip
	|	ControlWidth		ControlWidth			// system derived
 //	For CompoundControls only:
	|	ControlHMargin      Int Int                 // system dependent
	|	ControlHScroll      ScrollFunction          // no horizontal scrolling
	|	ControlItemSpace    Int Int                 // system dependent
	|	ControlLook         Bool Look               // control has white background
	|	ControlOrigin       Point2                  // Left top of ViewDomain
	|	ControlOuterSize	Size					// enclose elements
	|	ControlViewDomain   ViewDomain              // {zero,max range}
	|	ControlViewSize     Size                    // enclose elements
	|	ControlVMargin      Int Int                 // system dependent
	|	ControlVScroll      ScrollFunction          // no vertical   scrolling

::	ControlResizeFunction
	:==	Size ->                                     // current control outer size
		Size ->                                     // old     parent  view  size
		Size ->                                     // new     parent  view  size
		Size                                        // new     control outer size
::	ControlType
	:==	String
