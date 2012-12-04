implementation module StdIOCommon


import	StdArray, StdBool, StdFunc, StdInt, StdList, StdOverloaded, StdString
import	id, StdBitmap, StdIOBasic, StdKey, StdMaybe
from commondef import stateMap2


/*	The SelectState type.								*/

::	SelectState
	=	Able | Unable

instance == SelectState where
	(==) Able	select = enabled select
	(==) Unable	select = not (enabled select)
instance ~ SelectState where
	(~) Able	= Unable
	(~) Unable	= Able
instance toString SelectState where
	toString Able   = "Able"
	toString Unable = "Unable"

enabled :: !SelectState -> Bool
enabled Able	= True
enabled _		= False


/*	The MarkState type.									*/

::	MarkState
	=	Mark | NoMark

instance == MarkState where
	(==) Mark	mark = marked mark
	(==) NoMark	mark = not (marked mark)
instance ~ MarkState where
	(~) Mark	= NoMark
	(~) _		= Mark
instance toString MarkState where
	toString Mark   = "Mark"
	toString NoMark = "NoMark"

marked :: !MarkState -> Bool
marked Mark   	= True
marked _		= False


/*	The KeyboardState type.								*/

::	KeyboardState
	=	CharKey		Char		KeyState			// ASCII character input
	|	SpecialKey	SpecialKey	KeyState Modifiers	// Special key input
	|	KeyLost										// Key input lost while key was down
::	KeyState
	=	KeyDown		IsRepeatKey						// Key is down
	|	KeyUp										// Key goes up
::	IsRepeatKey										// Flag on key down:
	:==	Bool										// True iff key is repeating
::	Key
	=	IsCharKey	 Char
	|	IsSpecialKey SpecialKey
::	KeyboardStateFilter								// Predicate on KeyboardState:
	:==	KeyboardState -> Bool						// evaluate KeyFunction only if predicate holds and SelectState is Able

getKeyboardStateKeyState :: !KeyboardState -> KeyState
getKeyboardStateKeyState (CharKey    _ kstate  ) = kstate
getKeyboardStateKeyState (SpecialKey _ kstate _) = kstate
getKeyboardStateKeyState KeyLost                 = KeyUp

getKeyboardStateKey :: !KeyboardState -> Maybe Key
getKeyboardStateKey (CharKey    char  _) = Just (IsCharKey   char)
getKeyboardStateKey (SpecialKey key _ _) = Just (IsSpecialKey key)
getKeyboardStateKey KeyLost              = Nothing

instance == KeyboardState where
	(==) (CharKey char key) keySt			= case keySt of
												(CharKey char` key`)			-> char==char` && key==key`
												_								-> False
	(==) (SpecialKey spec key mods) keySt	= case keySt of
												(SpecialKey spec` key` mods`)	-> spec==spec` && key==key` && mods==mods`
												_								-> False
	(==) KeyLost					keySt	= case keySt of
												KeyLost							-> True
												_								-> False
instance == KeyState where
	(==) KeyUp				key	= case key of
									KeyUp				-> True
									_					-> False
	(==) (KeyDown repeat)	key	= case key of
									(KeyDown repeat`)	-> repeat==repeat`
									_					-> False
instance toString KeyboardState where
	toString (CharKey char keystate)
		= brackify ("CharKey "+++toString char+++" "+++brackify ("ASCII: "+++toString (toInt char))+++" "+++toString keystate)
	toString (SpecialKey special keystate modifiers)
		= brackify ("SpecialKey "+++itemsList " " [toString special,toString keystate,toString modifiers])
	toString KeyLost
		= "KeyLost"
instance toString KeyState where
	toString (KeyDown isRepeat)	= brackify ("KeyDown "+++fromBool isRepeat)
	toString KeyUp				= "KeyUp"


/*	The MouseState type.								*/

::	MouseState
	=	MouseMove	Point2 Modifiers			// Mouse is up		(position & modifiers)
	|	MouseDown	Point2 Modifiers Int		// Mouse goes down	(position & modifiers & nr double down)
	|	MouseDrag	Point2 Modifiers			// Mouse is down	(position & modifiers)
	|	MouseUp		Point2 Modifiers			// Mouse goes up	(position & modifiers)
	|	MouseLost								// Mouse input lost while mouse was down
::	ButtonState
 	=	ButtonStillUp							// MouseMove
 	|	ButtonDown								// MouseDown 1
	|	ButtonDoubleDown						//			 2
	|	ButtonTripleDown						//           >2
	|	ButtonStillDown							// MouseDrag
 	|	ButtonUp								// MouseUp
::	MouseStateFilter							// Predicate on MouseState:
	:==	MouseState -> Bool						// Evaluate MouseFunction only if predicate holds and SelectState is Able

getMouseStatePos :: !MouseState -> Point2
getMouseStatePos (MouseMove pos _)			= pos
getMouseStatePos (MouseDown pos _ _)		= pos
getMouseStatePos (MouseDrag pos _)			= pos
getMouseStatePos (MouseUp   pos _)			= pos
getMouseStatePos MouseLost					= zero

getMouseStateModifiers :: !MouseState -> Modifiers
getMouseStateModifiers (MouseMove _ mods)	= mods
getMouseStateModifiers (MouseDown _ mods _)	= mods
getMouseStateModifiers (MouseDrag _ mods)	= mods
getMouseStateModifiers (MouseUp   _ mods)	= mods
getMouseStateModifiers MouseLost			= NoModifiers

getMouseStateButtonState:: !MouseState	-> ButtonState
getMouseStateButtonState (MouseMove _ _)	= ButtonStillUp
getMouseStateButtonState (MouseDown _ _ nr)	= if (nr==1) ButtonDown 
											 (if (nr==2) ButtonDoubleDown
											 			 ButtonTripleDown
											 )
getMouseStateButtonState (MouseDrag _ _)	= ButtonStillDown
getMouseStateButtonState (MouseUp   _ _)	= ButtonUp
getMouseStateButtonState MouseLost			= ButtonUp

instance == MouseState where
	(==) (MouseMove pos mods)	 mouseSt	= case mouseSt of
												(MouseMove pos` mods`)		-> pos==pos` && mods==mods`
												_							-> False
	(==) (MouseDown pos mods nr) mouseSt	= case mouseSt of
												(MouseDown pos` mods` nr`)	-> pos==pos` && mods==mods` && nr==nr`
												_							-> False
	(==) (MouseDrag pos mods)	 mouseSt	= case mouseSt of
												(MouseDrag pos` mods`)		-> pos==pos` && mods==mods`
												_							-> False
	(==) (MouseUp   pos mods)	 mouseSt	= case mouseSt of
												(MouseUp pos` mods`)		-> pos==pos` && mods==mods`
												_							-> False
	(==) MouseLost				 mouseSt	= case mouseSt of
												MouseLost					-> True
												_							-> False
instance == ButtonState where
	(==) ButtonStillUp		button	= case button of
										ButtonStillUp		-> True
										_					-> False
	(==) ButtonDown			button	= case button of
										ButtonDown			-> True
										_					-> False
	(==) ButtonDoubleDown	button	= case button of
										ButtonDoubleDown	-> True
										_					-> False
	(==) ButtonTripleDown	button	= case button of
										ButtonTripleDown	-> True
										_					-> False
	(==) ButtonStillDown	button	= case button of
										ButtonStillDown		-> True
										_					-> False
	(==) ButtonUp			button	= case button of
										ButtonUp			-> True
										_					-> False
instance toString MouseState where
	toString (MouseMove	pos modifiers)		= brackify ("MouseMove "+++itemsList " " [toString pos,toString modifiers])
	toString (MouseDown	pos modifiers nr)	= brackify ("MouseDown "+++itemsList " " [toString pos,toString modifiers,toString nr])
	toString (MouseDrag	pos modifiers)		= brackify ("MouseDrag "+++itemsList " " [toString pos,toString modifiers])
	toString (MouseUp   pos modifiers)		= brackify ("MouseUp "  +++itemsList " " [toString pos,toString modifiers])
	toString MouseLost						= "MouseLost"
instance toString ButtonState where
	toString ButtonStillUp					= "ButtonStillUp"
	toString ButtonDown						= "ButtonDown"
	toString ButtonDoubleDown				= "ButtonDoubleDown"
	toString ButtonTripleDown				= "ButtonTripleDown"
	toString ButtonStillDown				= "ButtonStillDown"
	toString ButtonUp						= "ButtonUp"



/*	The SliderState type.								*/

::	SliderState
	=	{	sliderMin	:: !Int
		,	sliderMax	:: !Int
		,	sliderThumb	:: !Int
		}

instance == SliderState where								// Equality on SliderState
	(==) s1 s2 = s1.sliderMin==s2.sliderMin && s1.sliderMax==s2.sliderMax && s1.sliderThumb==s2.sliderThumb
instance toString SliderState where
	toString {sliderMin,sliderThumb,sliderMax}
		= curlify (itemsList "," (map recordFieldtoString (zip2	["sliderMin","sliderThumb","sliderMax"]
																[ sliderMin,  sliderThumb,  sliderMax ])))


/*	The UpdateState type.								*/

::	UpdateState
	=	{	oldFrame	:: !ViewFrame
		,	newFrame	:: !ViewFrame
		,	updArea		:: !UpdateArea
		}
::	ViewDomain			:==	Rectangle
::	ViewFrame			:==	Rectangle
::	UpdateArea			:==	[ViewFrame]

instance toString UpdateState where
	toString {oldFrame,newFrame,updArea}
		= curlify (itemsList "," ["oldFrame="+++toString oldFrame
								 ,"newFrame="+++toString newFrame
								 ,"updArea=" +++squarify (itemsList "," (map toString updArea))
								 ]
				  )

rectangleToUpdateState :: !Rectangle -> UpdateState
rectangleToUpdateState frame
	= {oldFrame=frame,newFrame=frame,updArea=[frame]}

/*	viewDomainRange defines the minimum and maximum values for ViewDomains.
	viewFrameRange  defines the minimum and maximum values for ViewFrames.
*/
viewDomainRange :: ViewDomain
viewDomainRange
	= {	corner1 = {x = 0-(2^30),y = 0-(2^30)}
	  ,	corner2 = {x =    2^30 ,y =    2^30 }
	  }

viewFrameRange :: ViewFrame
viewFrameRange
	= {	corner1 = {x = 1-(2^31),y = 1-(2^31)}
	  ,	corner2 = {x = (2^31)-1,y = (2^31)-1}
	  }


/*	Modifiers indicates the meta keys that have been pressed (True) or not (False).	*/

::	Modifiers
	=	{	shiftDown	:: !Bool
		,	optionDown	:: !Bool
		,	commandDown	:: !Bool
		,	controlDown	:: !Bool
		,	altDown		:: !Bool
		}

NoModifiers	:==	{shiftDown=False,optionDown=False,commandDown=False,controlDown=False,altDown=False}
ShiftOnly	:==	{shiftDown=True	,optionDown=False,commandDown=False,controlDown=False,altDown=False}
OptionOnly	:== {shiftDown=False,optionDown=True, commandDown=False,controlDown=False,altDown=True }
CommandOnly	:== {shiftDown=False,optionDown=False,commandDown=True, controlDown=True, altDown=False}
ControlOnly	:== {shiftDown=False,optionDown=False,commandDown=True, controlDown=True, altDown=False}
AltOnly		:==	{shiftDown=False,optionDown=True, commandDown=False,controlDown=False,altDown=True }

instance == Modifiers where
	(==) m1 m2 = m1.shiftDown   == m2.shiftDown
			  && m1.optionDown  == m2.optionDown
			  && m1.commandDown == m2.commandDown
			  && m1.controlDown == m2.controlDown
			  && m1.altDown     == m2.altDown
instance toString Modifiers where
	toString {shiftDown,optionDown,commandDown,controlDown,altDown}
		= curlify (itemsList "," (flatten [	if shiftDown   ["shiftDown"]   []
										  ,	if optionDown  ["optionDown"]  []
										  ,	if commandDown ["commandDown"] []
										  ,	if controlDown ["controlDown"] []
										  ,	if altDown     ["altDown"]     []
										  ]))


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
::	Alignment								// Only makes sense for ItemLocs that are relative to other items:
	=	AlignLeft							// Align left  edges below each other
	|	AlignRight							// Align right edges below each other
	|	AlignCenter							// Align centers below/next to each other
	|	AlignTop							// Align top    edges next to each other
	|	AlignBottom							// Align bottom edges next to each other
::	ParentIndex
	:== Int									// The number of parents (1..)
::	OffsetFun
	:==	(ViewDomain,Point2) -> Vector2		// Current view domain and origin

instance zero ItemOffset where
	zero = NoOffset

instance == ItemLoc where
	(==) Fix				itemLoc	= case itemLoc of
										Fix				-> True
										_				-> False
	(==) LeftTop			itemLoc	= case itemLoc of
										LeftTop			-> True
										_				-> False
	(==) RightTop			itemLoc	= case itemLoc of
										RightTop		-> True
										_				-> False
	(==) LeftBottom			itemLoc	= case itemLoc of
										LeftBottom		-> True
										_				-> False
	(==) RightBottom		itemLoc	= case itemLoc of
										RightBottom		-> True
										_				-> False
	(==) Left				itemLoc	= case itemLoc of
										Left			-> True
										_				-> False
	(==) Center				itemLoc	= case itemLoc of
										Center			-> True
										_				-> False
	(==) Right				itemLoc	= case itemLoc of
										Right			-> True
										_				-> False
	(==) (LeftOf	id1)	itemLoc	= case itemLoc of
										LeftOf	id2		-> id1==id2
										_				-> False
	(==) (RightTo	id1)	itemLoc	= case itemLoc of
										RightTo	id2		-> id1==id2
										_				-> False
	(==) (Above		id1)	itemLoc	= case itemLoc of
										Above	id2		-> id1==id2
										_				-> False
	(==) (Below		id1)	itemLoc	= case itemLoc of
										Below	id2		-> id1==id2
										_				-> False
	(==) LeftOfPrev			itemLoc	= case itemLoc of
										LeftOfPrev		-> True
										_				-> False
	(==) RightToPrev		itemLoc	= case itemLoc of
										RightToPrev		-> True
										_				-> False
	(==) AbovePrev			itemLoc	= case itemLoc of
										AbovePrev		-> True
										_				-> False
	(==) BelowPrev			itemLoc	= case itemLoc of
										BelowPrev		-> True
										_				-> False
instance toString ItemLoc where
	toString Fix			= "Fix"
	toString LeftTop		= "LeftTop"
	toString RightTop		= "RightTop"
	toString LeftBottom		= "LeftBottom"
	toString RightBottom	= "RightBottom"
	toString Left			= "Left"
	toString Center			= "Center"
	toString Right			= "Right"
	toString (LeftOf  id)	= brackify ("LeftOf " +++ toString id)
	toString (RightTo id)	= brackify ("RightTo "+++ toString id)
	toString (Above   id)	= brackify ("Above "  +++ toString id)
	toString (Below   id)	= brackify ("Below "  +++ toString id)
	toString LeftOfPrev		= "LeftOfPrev"
	toString RightToPrev	= "RightToPrev"
	toString AbovePrev		= "AbovePrev"
	toString BelowPrev		= "BelowPrev"


/*	The Direction type.									*/

::	Direction
	=	Horizontal
	|	Vertical

instance == Direction where
	(==) Horizontal direction	= case direction of
									Horizontal	-> True
									_			-> False
	(==) Vertical	direction	= case direction of
									Vertical	-> True
									_			-> False
instance toString Direction where
	toString Horizontal = "Horizontal"
	toString Vertical   = "Vertical"


/*	The CursorShape type.							*/

::	CursorShape
	=	StandardCursor
	|	BusyCursor
	|	IBeamCursor
	|	CrossCursor
	|	FatCrossCursor
	|	ArrowCursor
	|	HiddenCursor

instance == CursorShape where
	(==) StandardCursor	cursor	= case cursor of
									StandardCursor	-> True
									_				-> False
	(==) BusyCursor		cursor	= case cursor of
									BusyCursor		-> True
									_				-> False
	(==) IBeamCursor	cursor	= case cursor of
									IBeamCursor		-> True
									_				-> False
	(==) CrossCursor	cursor	= case cursor of
									CrossCursor		-> True
									_				-> False
	(==) FatCrossCursor	cursor	= case cursor of
									FatCrossCursor	-> True
									_				-> False
	(==) ArrowCursor	cursor	= case cursor of
									ArrowCursor		-> True
									_				-> False
	(==) HiddenCursor	cursor	= case cursor of
									HiddenCursor	-> True
									_				-> False
instance toString CursorShape where
	toString StandardCursor	= "StandardCursor"
	toString BusyCursor		= "BusyCursor"
	toString IBeamCursor	= "IBeamCursor"
	toString CrossCursor	= "CrossCursor"
	toString FatCrossCursor	= "FatCrossCursor"
	toString ArrowCursor	= "ArrowCursor"
	toString HiddenCursor	= "HiddenCursor"


/*	Document interface type of interactive processes.	*/

::	DocumentInterface
	=	NDI														// No       Document Interface
	|	SDI														// Single   Document Interface
	|	MDI														// Multiple Document Interface

instance == DocumentInterface where
	(==) NDI xdi	= case xdi of
						NDI	-> True
						_	-> False
	(==) SDI xdi	= case xdi of
						SDI	-> True
						_	-> False
	(==) MDI xdi	= case xdi of
						MDI	-> True
						_	-> False
instance toString DocumentInterface where
	toString NDI = "NDI"
	toString SDI = "SDI"
	toString MDI = "MDI"


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
	:==	[String]										// The file names to open
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

instance toString SliderMove where
	toString SliderIncSmall  = "SliderIncSmall"
	toString SliderDecSmall  = "SliderDecSmall"
	toString SliderIncLarge  = "SliderIncLarge"
	toString SliderDecLarge  = "SliderDecLarge"
	toString (SliderThumb x) = brackify ("SliderThumb "+++toString x)


/*	Scrolling function.									*/

::	ScrollFunction
	:==	ViewFrame		->					// Current	view
		SliderState		->					// Current	state of scrollbar
		SliderMove		->					// Action of the user
		Int									// New thumb value

stdScrollFunction :: !Direction !Int -> ScrollFunction
stdScrollFunction direction d
	= stdScrollFunction` direction d
where
	stdScrollFunction` :: !Direction !Int !ViewFrame !SliderState !SliderMove -> Int
	stdScrollFunction` direction d viewFrame {sliderThumb=x} move
		# d				= abs d
		  viewFrameSize	= rectangleSize viewFrame
		  edge			= if (direction==Horizontal) viewFrameSize.w viewFrameSize.h
		= case move of
			SliderIncSmall	-> x+d
			SliderDecSmall	-> x-d
			SliderIncLarge	-> x+edge/d*d
			SliderDecLarge	-> x-edge/d*d
			SliderThumb x	-> x/d*d


/*	Standard GUI object rendering function.				*/

::	Look
	:==	SelectState ->						// Current SelectState of GUI object
		UpdateState ->						// The area to be rendered
		*Picture	-> *Picture				// The rendering action

stdUnfillNewFrameLook :: SelectState !UpdateState !*Picture -> *Picture
stdUnfillNewFrameLook _ {newFrame} picture = unfill newFrame picture

stdUnfillUpdAreaLook :: SelectState !UpdateState !*Picture -> *Picture
stdUnfillUpdAreaLook _ {updArea} picture = stateMap2 unfill updArea picture


/*	Common error report types.							*/

::	ErrorReport													// Usual cause:
	=	NoError													// No error
	|	ErrorViolateDI											// Violation against document interface kind
	|	ErrorIdsInUse											// Object definition contains Ids that are already in use
	|	ErrorUnknownObject										// Object can not be found
	|	ErrorNotifierOpen										// It was tried to open a second send notifier // MW0++
	|	OtherError !String										// Other kind of error

instance == ErrorReport where
	(==) NoError			error	= case error of
										NoError				-> True
										_					-> False
	(==) ErrorViolateDI		error	= case error of
										ErrorViolateDI		-> True
										_					-> False
	(==) ErrorIdsInUse		error	= case error of
										ErrorIdsInUse		-> True
										_					-> False
	(==) ErrorUnknownObject	error	= case error of
										ErrorUnknownObject	-> True
										_					-> False
// MW11..
	(==) ErrorNotifierOpen	error	= case error of
										ErrorNotifierOpen	-> True
										_					-> False
// ..MW11
	(==) (OtherError e1)	error	= case error of
										OtherError e2		-> e1==e2
										_					-> False
instance toString ErrorReport where
	toString NoError			= "NoError"
	toString ErrorViolateDI		= "ErrorViolateDI"
	toString ErrorIdsInUse		= "ErrorIdsInUse"
	toString ErrorUnknownObject	= "ErrorUnknownObject"
	toString ErrorNotifierOpen	= "ErrorNotifierOpen" // MW11++
	toString (OtherError s)		= brackify ("OtherError "+++toString s)


//	Some handy functions for toString:
curlify  x = "{"+++x+++"}"
brackify x = "("+++x+++")"
squarify x = "["+++x+++"]"

recordFieldtoString :: (String,a) -> String | toString a	// recordFieldtoString f v -> f=v
recordFieldtoString (field,value) = field+++"="+++toString value

itemsList :: !String ![String] -> String	// itemsList c [a0,...an] -> a0 c a1 c ... c an
itemsList separator [x:xs]
	= x+++itemsList` xs
where
	itemsList` [x:xs]	= separator+++x+++itemsList` xs
	itemsList` _		= ""
itemsList _ _
	= ""

// MW11..
::	OkBool									// iff True, the operation was successful
	:==	Bool
// ..MW11
