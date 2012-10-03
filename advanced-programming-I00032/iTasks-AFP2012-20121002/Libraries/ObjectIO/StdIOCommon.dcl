definition module StdIOCommon


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdIOCommon defines common types and access functions for the I/O library.
//	********************************************************************************


import	StdOverloaded, StdString
import	StdBitmap, StdIOBasic, StdKey, StdMaybe
from	id			import	:: Id, :: RId, :: R2Id, instance toString Id, instance == Id, 
						instance == RId, instance == R2Id


/*	The SelectState and MarkState types.				*/

::	SelectState		=	Able | Unable
::	MarkState		=	Mark | NoMark

enabled		:: !SelectState	-> Bool			// @1 == Able
marked		:: !MarkState	-> Bool			// @1 == Mark

instance ==		  SelectState				// Constructor equality
instance ==		  MarkState					// Constructor equality
instance ~		  SelectState				// Able <-> Unable
instance ~		  MarkState					// Mark <-> NoMark
instance toString SelectState
instance toString MarkState



/*	The KeyboardState type.								*/

::	KeyboardState
	=	CharKey		Char		KeyState	// ASCII character input
	|	SpecialKey	SpecialKey	KeyState Modifiers
											// Special key input
	|	KeyLost								// Key input lost while key was down
::	KeyState
	=	KeyDown		IsRepeatKey				// Key is down
	|	KeyUp								// Key goes up
::	IsRepeatKey								// Flag on key down:
	:==	Bool								// True iff key is repeating
::	Key
	=	IsCharKey	 Char
	|	IsSpecialKey SpecialKey
::	KeyboardStateFilter						// Predicate on KeyboardState:
	:==	KeyboardState -> Bool				// evaluate KeyFunction only if True

getKeyboardStateKeyState:: !KeyboardState -> KeyState	// KeyUp   if KeyLost
getKeyboardStateKey		:: !KeyboardState -> Maybe Key	// Nothing if KeyLost

instance ==		  KeyboardState				// Equality on KeyboardState
instance ==		  KeyState					// Equality on KeyState
instance toString KeyboardState
instance toString KeyState


/*	The MouseState type.								*/

::	MouseState
	=	MouseMove	Point2 Modifiers		// Mouse is up     (position,modifiers)
	|	MouseDown	Point2 Modifiers Int	// Mouse goes down (and nr down)
	|	MouseDrag	Point2 Modifiers		// Mouse is down   (position,modifiers)
	|	MouseUp		Point2 Modifiers		// Mouse goes up   (position,modifiers)
	|	MouseLost							// Mouse input lost while mouse was down
::	ButtonState
 	=	ButtonStillUp						// MouseMove
 	|	ButtonDown							// MouseDown _ _ 1
	|	ButtonDoubleDown					//			 _ _ 2
	|	ButtonTripleDown					//           _ _ >2
	|	ButtonStillDown						// MouseDrag
 	|	ButtonUp							// MouseUp/MouseLost
::	MouseStateFilter						// Predicate on MouseState:
	:==	MouseState -> Bool					// evaluate MouseFunction only if True

getMouseStatePos		:: !MouseState	-> Point2		// zero        if MouseLost
getMouseStateModifiers	:: !MouseState	-> Modifiers	// NoModifiers if MouseLost
getMouseStateButtonState:: !MouseState	-> ButtonState	// ButtonUp    if MouseLost

instance ==		  MouseState				// Equality on MouseState
instance ==		  ButtonState				// Constructor equality
instance toString MouseState
instance toString ButtonState

/*	The SliderState type.								*/

::	SliderState
	=	{	sliderMin	:: !Int
		,	sliderMax	:: !Int
		,	sliderThumb	:: !Int
		}

instance == SliderState						// @1.sliderMin   == @2.sliderMin
											// @1.sliderMax   == @2.sliderMax
											// @1.sliderThumb == @2.sliderThumb
instance toString SliderState


/*	The UpdateState type.								*/

::	UpdateState
	=	{	oldFrame	:: !ViewFrame
		,	newFrame	:: !ViewFrame
		,	updArea		:: !UpdateArea
		}
::	ViewDomain			:==	Rectangle
::	ViewFrame			:==	Rectangle
::	UpdateArea			:==	[ViewFrame]

instance toString UpdateState

rectangleToUpdateState	:: !Rectangle -> UpdateState
											// r -> {oldFrame=newFrame=r,updArea=[r]}

/*	viewDomainRange defines the minimum and maximum values for ViewDomains.
	viewFrameRange  defines the minimum and maximum values for ViewFrames.
*/
viewDomainRange			:: ViewDomain
viewFrameRange			:: ViewFrame


/*	Modifiers indicates the meta keys that have been pressed (True) or not (False).
*/
::	Modifiers
	=	{	shiftDown	:: !Bool			// True iff shift   down
		,	optionDown	:: !Bool			// True iff option  down
		,	commandDown	:: !Bool			// True iff command down
		,	controlDown	:: !Bool			// True iff control down
		,	altDown		:: !Bool			// True iff alt     down
		}

//	Constants to check which of the Modifiers are down.

NoModifiers	:==	{shiftDown	= False
				,optionDown	= False
				,commandDown= False
				,controlDown= False
				,altDown	= False
				}
ShiftOnly	:==	{shiftDown	= True
				,optionDown	= False
				,commandDown= False
				,controlDown= False
				,altDown	= False
				}
OptionOnly	:== {shiftDown	= False
				,optionDown	= True
				,commandDown= False
				,controlDown= False
				,altDown	= True
				}
CommandOnly	:== {shiftDown	= False
				,optionDown	= False
				,commandDown= True
				,controlDown= True
				,altDown	= False
				}
ControlOnly	:== {shiftDown	= False
				,optionDown	= False
				,commandDown= True
				,controlDown= True
				,altDown	= False
				}
AltOnly		:==	{shiftDown	= False
				,optionDown	= True
				,commandDown= False
				,controlDown= False
				,altDown	= True
				}

instance ==			Modifiers
instance toString	Modifiers


/*	The layout language used for windows and controls.	*/
::	ItemPos
	:==	(	ItemLoc
		,	ItemOffset
		)
::	ItemLoc
 //	Absolute:
	=	Fix
 //	Relative to corner:
	|	LeftTop
	|	RightTop
	|	LeftBottom
	|	RightBottom
 //	Relative in next line:
	|	Left
	|	Center
	|	Right
 //	Relative to other item:
	|	LeftOf	Id
	|	RightTo	Id
	|	Above	Id
	|	Below	Id
 //	Relative to previous item:
	|	LeftOfPrev
	|	RightToPrev
	|	AbovePrev
	|	BelowPrev
::	ItemOffset
	=	NoOffset							// Shorthand for OffsetVector zero
	|	OffsetVector Vector2				// A constant offset vector
	|	OffsetAlign  Alignment				// Offset depends on size of other item
	|	OffsetFun    ParentIndex OffsetFun	// Offset depends on orientation
::	Alignment
	=	AlignLeft							// Align left  edges below each other
	|	AlignRight							// Align right edges below each other
	|	AlignCenter							// Align centers below/next to each other
	|	AlignTop							// Align top    edges next to each other
	|	AlignBottom							// Align bottom edges next to each other
::	ParentIndex
	:== Int									// The number of parents (1..)
::	OffsetFun
	:==	(ViewDomain,Point2) -> Vector2		// Current view domain and origin

instance	zero	 ItemOffset				// zero == NoOffset
instance	==		 ItemLoc				// Constructor and value equality
instance	toString ItemLoc				// Constructor and value as String


/*	The Direction type.								*/

::	Direction
	=	Horizontal
	|	Vertical

instance	==		 Direction				// Constructor equality
instance	toString Direction				// Constructor as String


/*	The CursorShape type.							*/

::	CursorShape
	=	StandardCursor
	|	BusyCursor
	|	IBeamCursor
	|	CrossCursor
	|	FatCrossCursor
	|	ArrowCursor
	|	HiddenCursor

instance	==		 CursorShape			// Constructor equality
instance	toString CursorShape			// Constructor as String


/*	Document interface of interactive processes.	*/

::	DocumentInterface
	=	NDI									// No       Document Interface
	|	SDI									// Single   Document Interface
	|	MDI									// Multiple Document Interface

instance	==		 DocumentInterface		// Constructor equality
instance	toString DocumentInterface		// Constructor as String


/*	Process attributes.									*/

::	ProcessAttribute st									// Default:
	=	ProcessActivate			(IdFun st)				// No action on activate
	|	ProcessDeactivate		(IdFun st)				// No action on deactivate
	|	ProcessClose			(IdFun st)				// Process is closed
 //	Attributes for (M/S)DI process only:
	|	ProcessOpenFiles		(ProcessOpenFilesFunction st)
														// Request to open files
	|	ProcessClipboardChanged	(IdFun st)
	|	ProcessWindowPos		ItemPos					// Platform dependent
	|	ProcessWindowSize		Size					// Platform dependent
	|	ProcessWindowResize		(ProcessWindowResizeFunction st)
														// Platform dependent
 	|	ProcessToolbar			[ToolbarItem st]		// Process has no toolbar
 //	Attributes for MDI processes only:
	|	ProcessNoWindowMenu								// Process has WindowMenu

::	ProcessWindowResizeFunction st
	:==	Size											// Old ProcessWindow size
	 ->	Size											// New ProcessWindow size
	 ->	st -> st
::	ProcessOpenFilesFunction st
	:==	[String]										// The filenames to open
	 -> st -> st

::	ToolbarItem st
	=	ToolbarItem Bitmap (Maybe String) (IdFun st)
	|	ToolbarSeparator


/*	Frequently used function types.						*/

::	ModifiersFunction	st	:==	Modifiers		->	st -> st
::	MouseFunction		st	:== MouseState		->	st -> st
::	KeyboardFunction	st	:== KeyboardState	->	st -> st
::	SliderAction		st	:==	SliderMove		->	st -> st
::	SliderMove
	=	SliderIncSmall
	|	SliderDecSmall
	|	SliderIncLarge
	|	SliderDecLarge
	|	SliderThumb Int

instance toString SliderMove


/*	Scrolling function.									*/

::	ScrollFunction
	:==	ViewFrame		->					// Current	view
		SliderState		->					// Current	state of scrollbar
		SliderMove		->					// Action of the user
		Int									// New thumb value

stdScrollFunction :: !Direction !Int -> ScrollFunction
/*	stdScrollFunction direction d implements standard scrolling behaviour:
	- direction indicates scrolling for Horizontal or Vertical scroll bar.
	- d         is the stepsize with which to scroll (taken absolute).
	stdScrollFunction lets the system scroll as follows:
	- Slider(Inc/Dec)Small: d
	- Slider(Inc/Dec)Large: viewFrame size modulo d
	- SliderThumb x:        x modulo d
*/


/*	Standard GUI object rendering function.				*/

::	Look
	:==	SelectState ->						// Current SelectState of GUI object
		UpdateState ->						// The area to be rendered
		*Picture	-> *Picture				// The rendering action

stdUnfillNewFrameLook:: SelectState !UpdateState !*Picture -> *Picture
stdUnfillUpdAreaLook :: SelectState !UpdateState !*Picture -> *Picture
/*	Two convenience functions for simple Look functions:
	stdUnfillNewFrameLook _ {newFrame} = unfill newFrame
	stdUnfillUpdAreaLook  _ {updArea}  = seq (map unfill updArea)
*/


/*	Common error report types.							*/

::	ErrorReport						// Usual cause:
	=	NoError						// Everything went allright
	|	ErrorViolateDI				// Violation against DocumentInterface
	|	ErrorIdsInUse				// Object contains Ids that are bound
	|	ErrorUnknownObject			// Object can not be found
	|	ErrorNotifierOpen			// It was tried to open a second send notifier
	|	OtherError !String			// Other kind of error

instance	==			ErrorReport	// Constructor equality
instance	toString	ErrorReport	// Constructor as String

::	OkBool							// True iff the operation was successful
	:==	Bool
