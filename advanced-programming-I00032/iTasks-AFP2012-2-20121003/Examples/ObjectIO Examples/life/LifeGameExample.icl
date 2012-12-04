module LifeGameExample

//	**************************************************************************************************
//
//	This is the LifeGame program.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdEnv, StdIO
import Life, Help

::	Life
	=	{	gen	:: !Generation
		,	size:: !CellSize
		}
initialLife
	=	{	gen	= makeGeneration
		,	size= StartCellSize
		}

Start :: *World -> *World
Start world
	= startLife (openIds 6 world)

startLife :: ([Id],*World) -> *World
startLife ([eraseID,playID,haltID,stepID,windowID,timerID],world)
	= startIO SDI initialLife
				  initialise
				  [ProcessClose closeProcess]
				  world
where
//	initialise creates the gui of the life.
	initialise pst
		# (error,pst)	= openTimer undef timer pst
		| error<>NoError
			= abort "LifeGameExample could not open timer."
		# (error,pst)	= openMenu undef file pst
		| error<>NoError
			= abort "LifeGameExample could not open Life menu."
		# (error,pst)	= openMenu undef options pst
		| error<>NoError
			= abort "LifeGameExample could not open Options menu."
		# (error,pst)	= openMenu undef commands pst
		| error<>NoError
			= abort "LifeGameExample could not open Commands menu."
		# (size, pst)	= accPIO getProcessWindowSize pst
		# (error,pst)	= openWindow undef (window size) pst
		| error<>NoError
			= abort "LifeGameExample could not open Life window."
		| otherwise
			= pst
	
//	window defines the window that displays the universe and its inhabitants.
	window size	= Window "Life" NilLS
					[	WindowId			windowID
					,	WindowClose			(noLS closeProcess)
					,	WindowMouse			onlyMouseDown Able (noLS1 track)
					,	WindowViewDomain	(getViewDomain StartCellSize)
					,	WindowViewSize		size
					,	WindowOrigin		zero
					,	WindowHScroll 		(stdScrollFunction Horizontal StartCellSize)
					,	WindowVScroll		(stdScrollFunction Vertical   StartCellSize)
					,	WindowLook			True (look initialLife)
					,	WindowPen			[PenBack Black]
					]
	
//	timer defines the timer that calculates subsequent life generations.
	timer	= Timer 0 NilLS
				[	TimerId				timerID
				,	TimerSelectState	Unable
				,	TimerFunction		(noLS1 (\_->step))
				]

//	file defines the "File" menu, containing only the quit command to terminate the program.
	file	= Menu "&File"
				(	MenuItem "&About LifeGameExample..."
										[MenuFunction (noLS (showAbout "Life" "LifeHelp"))]
				:+:	MenuSeparator		[]
				:+:	MenuItem "&Quit"	[MenuShortKey 'q',MenuFunction (noLS closeProcess)]
				)	[]

//	options defines the "Options" menu to set the size of the displayed cells.
	options	= Menu "&Options"
				(	SubMenu "Cell Size" 
		  			(	RadioMenu
		  				[	(title (2^i),Nothing,Just (char i),noLS (newsize (2^i)))
		  				\\	i<-[0..4]
						]	4 []
		  			)	[]
				)	[]
	where
		title size	= toString size +++ " * " +++ toString size
		char  i		= toChar (fromChar '1'+i)
	
//	commands defines the "Commands" menu to run and halt the computations of life generations.
	commands= Menu "&Commands"
				(	MenuItem "&Erase Cells"	[MenuId eraseID,MenuShortKey 'e',MenuFunction (noLS erase)]
		  		:+:	MenuItem "&Play"		[MenuId playID, MenuShortKey 'p',MenuFunction (noLS play)]
				:+:	MenuItem "&Halt"		[MenuId haltID, MenuShortKey 'h',MenuFunction (noLS halt), MenuSelectState Unable]
				:+:	MenuItem "&Step"		[MenuId stepID, MenuShortKey 's',MenuFunction (noLS step)]
				)	[]
	
//	play starts the computation of successive generations given the current set of life cells.
	play :: (PSt Life) -> PSt Life
	play life
		= appListPIO
			[	disableWindowMouse	windowID
			,	disableMenuElements [eraseID,playID,stepID]
			,	enableMenuElements	[haltID]
			,	enableTimer			timerID
			]	life
	
//	halt stops the computation of successive generations, but does not change the current generation. 
	halt :: (PSt Life) -> PSt Life
	halt life
		= appListPIO
			[	enableWindowMouse	windowID
			,	disableMenuElements	[haltID]
			,	enableMenuElements	[eraseID,playID,stepID]
			,	disableTimer		timerID
			]	life
	
//	step calculates the next generation and displays it.
	step :: (PSt Life) -> PSt Life
	step life=:{ls=state=:{gen,size},io}
		# state		= {state & gen=next}
		# io		= appWindowPicture windowID render io
		# io		= setWindowLook windowID False (True,look state) io
		= {life & ls=state, io=io}
	where
		(next,died)	= lifeGame gen
		render		= drawCells (drawCell size) next o (drawCells (eraseCell size) died)
	
//	erase sets the current generation to empty and clears the window.
	erase :: (PSt Life) -> PSt Life
	erase life=:{ls=state,io}
		# state		= {state & gen=makeGeneration}
		# io		= setWindowLook windowID True (True,look state) io
		= {life & ls=state, io=io}
	
//	newsize changes the size in which life cells are rendered and redraws the window.
	newsize :: Int (PSt Life) -> PSt Life
	newsize newSize life=:{ls=state=:{size=oldSize},io}
		# state			= {state & size=newSize}
		# (viewframe,io)= getWindowViewFrame windowID io
  		  oldOrigin		= viewframe.corner1
		  newOrigin		= {x=oldOrigin.x/oldSize*newSize,y=oldOrigin.y/oldSize*newSize}
		# io			= setWindowLook windowID False (True,look {state & gen=makeGeneration}) io
		# io			= setWindowViewDomain windowID (getViewDomain newSize) io
		# io			= moveWindowViewFrame windowID (toVector newOrigin-toVector oldOrigin) io
		# io			= setWindowLook windowID True (True,look state) io
		= {life & ls=state, io=io}
	
//	The window look:
	look :: Life SelectState UpdateState *Picture -> *Picture
	look {gen,size} _ {newFrame} picture
		# picture	= unfill    newFrame			picture
		# picture	= drawCells (drawCell size) gen	picture
		= picture
	
//	The window mouse accepts only MouseDown user actions:
	onlyMouseDown :: MouseState -> Bool
	onlyMouseDown (MouseDown _ _ _) = True
	onlyMouseDown (MouseDrag _ _)	= True
	onlyMouseDown _					= False
	
//	The window mouse action places and removes alive cells:
	track :: MouseState (PSt Life) -> PSt Life
	track mouse life=:{ls=state=:{gen,size},io}
		| modifiers.commandDown
			# state		= {state & gen=removeCell cell gen}
			# io		= appWindowPicture windowID (eraseCell size cell) io
			# io		= setWindowLook windowID False (True,look state) io
			= {life & ls=state, io=io}
		| otherwise
			# state		= {state & gen=insertCell cell gen}
			# io		= appWindowPicture windowID (drawCell size cell) io
			# io		= setWindowLook windowID False (True,look state) io
			= {life & ls=state, io=io}
	where
		(pos,modifiers)	= case mouse of
							(MouseDown pos mods _) -> (pos,mods)
							(MouseDrag pos mods)   -> (pos,mods)
		cell			= makeLifeCell pos size
	
//	Given the size in which to render life cells, getViewDomain calculates the corresponding ViewDomain:
	getViewDomain :: CellSize -> ViewDomain
	getViewDomain size
		= {corner1={x=size*left,y=size*top},corner2={x=size*right,y=size*bottom}}
	where
		{corner1={x=left,y=top},corner2={x=right,y=bottom}}	= Universe

//	Program constants.

Universe		:==	{corner1={x=(-1000),y=(-1000)},corner2={x=1000,y=1000}}
StartCellSize	:== 8
