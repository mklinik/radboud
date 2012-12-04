module Turing

//	**************************************************************************************************
//
//	This program is a Turing machine interpreter and programming environment.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdEnv, StdIO
import tm, showtm, tmdialog, tmfile, Help


Speed1	:== ticksPerSecond / 3
Speed2	:== ticksPerSecond / 6
Speed3	:== ticksPerSecond / 12
Speed4	:== ticksPerSecond / 20
Speed5	:== 0


Start :: *World -> *World
Start world
	# (tmids,world)	= openTmIds world
	# (ids,  world)	= openIds 3 world
	= startTuring tmids ids world

startTuring :: TmIds [Id] *World -> *World
startTuring tmids ids world
	= startIO MDI initTuring initialise
		[	ProcessClose		DoQuit
		,	ProcessOpenFiles	DoOpenFiles
		]	world
where
	[runItemId,contItemId,timerID:_]
				= ids
	{windowID,tapeWdID,fileMenuId,saveItemId,machineMenuId,stepItemId,haltItemId}
				= tmids
	initTuring	= {	tmstate	= {	turing	= {	transitions	= []
										  ,	tape		= {	content	= ""
						  								  ,	head	= 0
														  }
										  ,	state		= ""
										  }
							  ,	transition	= 0
							  ,	command		= None
							  }
				  ,	name	= ""
				  ,	delay	= Speed3
				  ,	saved	= True
				  ,	tmids	= tmids
				  }
	
	initialise :: (PSt Tm) -> PSt Tm
	initialise pst=:{ls={tmstate=tm=:{turing={tape}}}}
		# (error,pst)	= openMenu undef file pst
		| error<>NoError
			= abort "Turing could not open file menu"
		# (error,pst)	= openMenu undef machine pst
		| error<>NoError
			= abort "Turing could not open machine menu"
		# (error,pst)	= openWindow undef trswd pst
		| error<>NoError
			= abort "Turing could not open transitions window"
		# (error,pst)	= openWindow undef tapewd pst
		| error<>NoError
			= abort "Turing could not open tape window"
		# (error,pst)	= openTimer undef timer pst
		| error<>NoError
			= abort "Turing could not open timer"
		| otherwise
			= pst
	where
		file	= Menu "File" 
					(	MenuItem "New"			[MenuShortKey 'N',MenuFunction (noLS DoNew)]
					:+:	MenuItem "Open..."		[MenuShortKey 'O',MenuFunction (noLS DoOpen)]
					:+:	MenuItem "Save"			[MenuShortKey 'S',MenuFunction (noLS DoSave),MenuId saveItemId,MenuSelectState Unable]
					:+:	MenuItem "Save As..."	[MenuFunction (noLS DoSaveAs)]
					:+:	MenuSeparator			[]
					:+:	MenuItem "Help..."		[MenuShortKey 'H',MenuFunction (noLS Help)]
					:+:	MenuSeparator			[]
					:+:	MenuItem "Quit"			[MenuShortKey 'Q',MenuFunction (noLS DoQuit)]
					)
					[	MenuId	fileMenuId
					]
		machine	= Menu "Machine" 
					(	MenuItem "Step"			[MenuShortKey 'T',MenuFunction (noLS DoStep),    MenuId stepItemId,MenuSelectState Unable]
					:+:	MenuItem "Run"			[MenuShortKey 'R',MenuFunction (noLS DoRun),     MenuId runItemId, MenuSelectState Unable]
					:+:	MenuItem "Halt"			[MenuShortKey '.',MenuFunction (noLS DoHalt),    MenuId haltItemId,MenuSelectState Unable]
					:+:	MenuItem "Continue"		[MenuShortKey ',',MenuFunction (noLS DoContinue),MenuId contItemId,MenuSelectState Unable]
					:+:	MenuSeparator			[]
					:+:	SubMenu "Speed" 
						(	RadioMenu
								[	("Very Slow",Nothing,Just '1',noLS (SetDelay Speed1))			// VerSId
								,	("Slow"     ,Nothing,Just '2',noLS (SetDelay Speed2))			// SlowId
								,	("Normal"   ,Nothing,Just '3',noLS (SetDelay Speed3))			// NormId
								,	("Fast"     ,Nothing,Just '4',noLS (SetDelay Speed4))			// FastId
								,	("Very Fast",Nothing,Just '5',noLS (SetDelay Speed5))			// VerFId
								] 3	[]
						)	[]
					)
					[	MenuId	machineMenuId
					]
		trswd	= Window "Turing Machine" NilLS
					[	WindowId			windowID
					,	WindowViewDomain	{zero & corner2={x=MaxX,y=265}}
					,	WindowViewSize		{w=500,h=265}
					,	WindowLook			True (tmLook tm)
					,	WindowMouse			tmMouseFilter Able (noLS1 EditTransitions)
					,	WindowClose			(noLS DoQuit)
					,	WindowHScroll		(stdScrollFunction Horizontal 24)
					,	WindowVScroll		(stdScrollFunction Vertical   8)
					]
		tapewd	= Window "Tape" NilLS
					[	WindowId			tapeWdID
					,	WindowViewDomain	{zero & corner2={x=MaxX,y=92}}
					,	WindowViewSize		{w=500,h=92}
					,	WindowLook			True (tpLook tape)
					,	WindowMouse			tpMouseFilter Able (noLS1 EditTape)
					,	WindowClose			(noLS DoQuit)
					,	WindowHScroll		(stdScrollFunction Horizontal 24)
					,	WindowVScroll		(stdScrollFunction Vertical   8)
					,	WindowPos			(Below windowID,zero)
					]
		timer	= Timer Speed3 NilLS [TimerId timerID,TimerSelectState Unable,TimerFunction (noLS1 TimerStep)]
	
	//	Open a new empty Turing machine.
	DoNew :: (PSt Tm) -> PSt Tm
	DoNew pst=:{ls={saved}}
		| saved		= MakeNewTuring pst
		| otherwise	= SaveBeforeClose "opening a new Turing machine" MakeNewTuring pst
	
	MakeNewTuring :: (PSt Tm) -> PSt Tm
	MakeNewTuring pst=:{ls=tm=:{delay},io}
		# io	= setWindowLook  tapeWdID True (True,tpLook inittape)							io
		# io	= setWindowLook  windowID True (True,tmLook inittmstate)						io
		# io	= setWindowTitle windowID "Turing Machine"										io
		# io	= disableMenuElements [saveItemId,stepItemId,runItemId,haltItemId,contItemId]	io
		= {pst & ls=inittm,io=io}
	where
		inittape	= {content="",head=0}
		inittmstate	= {	turing		= {	transitions	= []
									  ,	tape		= inittape
									  ,	state		= ""
									  }
					  ,	transition	= 0
					  ,	command		= None
					  }
		inittm		= {tm & tmstate	= inittmstate
						  ,	name	= ""
						  ,	delay	= delay
						  ,	saved	= True
					  }
	
	//	Save the Turing machine.
	DoSave :: (PSt Tm) -> PSt Tm
	DoSave pst=:{ls=tm=:{tmstate={turing},name},io}
		# (success,io)	= WriteTuringToFile turing name io
		| success
			# pst		= {pst & ls={tm & saved=True},io=io}
			# pst		= appPIO (disableMenuElements [saveItemId]) pst
			= pst
		| otherwise
			= Alert "The Turing machine has not been saved." "The file could not be opened." {pst & io=io}
	
	//	Save the Turing machine in a new file.
	DoSaveAs :: (PSt Tm) -> PSt Tm
	DoSaveAs pst=:{ls=tm=:{name,tmstate={turing}}}
		# (fname,pst)					= selectOutputFile "Save T.M. As:" (RemovePath name) pst
		| isNothing fname
			= pst
		# fname							= fromJust fname
		| RemovePath fname==HelpFile
			= Alert "The Turing machine cannot be saved to" ("the help file \'"+++HelpFile+++"\'.") pst
		# (success,pst)					= WriteTuringToFile turing fname pst
		| not success
			= Alert "The Turing machine has not been saved." "The file could not be opened." pst
		# pst							= appPIO (setWindowTitle windowID (RemovePath fname)) pst
		# pst							= appPIO (disableMenuElements [saveItemId]) pst
		| otherwise
			= {pst & ls={tm & name=fname,saved=True}}
	
	//	Load a Turing machine from a file.
	DoOpen :: (PSt Tm) -> PSt Tm
	DoOpen pst=:{ls={saved}}
		| saved		= EvtOpenTuring pst
		| otherwise	= SaveBeforeClose "opening an other Turing machine" EvtOpenTuring pst
	where
		EvtOpenTuring :: (PSt Tm) -> PSt Tm
		EvtOpenTuring pst
			# (filename,pst)		= selectInputFile pst
			| isNothing filename	= pst
			| otherwise				= OpenTuringFile (fromJust filename) pst
	
	//	DoOpenFiles opens the first file of the argument list.
	DoOpenFiles :: [String] (PSt Tm) -> PSt Tm
	DoOpenFiles names pst=:{ls={saved}}
		| saved		= OpenTuringFile (hd names) pst
		| otherwise	= SaveBeforeClose "opening an other Turing machine" (OpenTuringFile (hd names)) pst
	
	//	OpenTuringFile parses the file found at the path argument and if successful opens it.
	OpenTuringFile :: String (PSt Tm) -> PSt Tm
	OpenTuringFile name pst=:{ls=tm}
		# fname					= RemovePath name
		  fstring				= " \'"+++fname+++"\'"
		| fname==HelpFile
			= Alert ("The help file"+++fstring) "cannot be opened as a T.M." pst
		# ((status,turing),pst)	= ReadTuring name pst
		| status==0
			# tmstate			= {turing=turing,transition=0,command=None}
			# pst				= appListPIO 
									[	enableMenuElements  [runItemId,stepItemId]
									,	disableMenuElements [saveItemId]
									,	setWindowLook		tapeWdID True (True,tpLook turing.tape)
									,	setWindowLook		windowID True (True,tmLook tmstate)
									,	setWindowTitle		windowID (RemovePath name)
									]	pst
			= {pst & ls={tm & tmstate=tmstate,name=name,saved=True}}
		| otherwise
			# (msg1,msg2)		= if (status>0)		("Parse error in line "+++toString status,"of file"+++fstring+++".")
								 (if (status==(-1))	("Unexpected end of file",fstring+++".")
								 					("The file"+++fstring,"could not be opened."))
			= Alert msg1 msg2 pst
	
	
	//	The Help command.
	Help :: (PSt Tm) -> PSt Tm
	Help pst = showHelp HelpFile pst
	
	
	//	Let the Turing machine do one step (transition).
	DoStep :: (PSt Tm) -> PSt Tm
	DoStep pst=:{ls=tm=:{tmstate=tmstate=:{turing={tape={head},state},transition}},io}
		| state=="halt" || state=="error"
			= pst
		# tmstate	= Step tmstate
		  tm		= {tm & tmstate=tmstate}
		  newtrn	= tmstate.transition
		  newstate	= tmstate.turing.state
		  newcom	= tmstate.command
		  newtape	= tmstate.turing.tape
		# io		= setWindowLook    windowID False (True,tmLook tmstate) io
		# io		= appWindowPicture windowID (ShowNextState newstate o ShowTransition transition newtrn) io
		# io		= setWindowLook    tapeWdID False (True,tpLook newtape) io
		# io		= appWindowPicture tapeWdID (ShowNewTape newcom head) io
		| newstate<>"halt" && newstate<>"error"
			= {pst & ls={tm & tmstate=tmstate},io=io}
		| otherwise
			# io	= disableMenuElements [stepItemId,haltItemId,contItemId] io
			= {pst & ls={tm & tmstate=tmstate},io=io}
	
	
	//	Let the T.M. run until the haltstate is reached.
	DoRun :: (PSt Tm) -> PSt Tm
	DoRun pst=:{ls=tm=:{tmstate={turing}},io}
		# io	= disableWindowMouse	tapeWdID				io
		# io	= disableWindowMouse	windowID				io
		# io	= enableMenuElements	[haltItemId]			io
		# io	= disableMenuElements	[stepItemId,runItemId]	io
		# io	= disableMenus    		[fileMenuId]			io
		# io	= appWindowPicture		tapeWdID EraseError		io
		# io	= enableTimer	     	timerID					io
		= {pst & ls={tm & tmstate={tm.tmstate & turing={turing & state="S"}}},io=io}
	
	
	//	Halt a running T.M.
	DoHalt :: (PSt Tm) -> PSt Tm
	DoHalt pst=:{io}
		# io	= enableWindowMouse		tapeWdID							io
		# io	= enableWindowMouse		windowID							io
		# io	= enableMenuElements	[stepItemId,runItemId,contItemId]	io
		# io	= enableMenus			[fileMenuId]						io
		# io	= disableTimer			timerID								io
		= {pst & io=io}
	
	
	//	Continue a halted T.M.
	DoContinue :: (PSt Tm) -> PSt Tm
	DoContinue pst=:{io}
		# io	= disableWindowMouse	tapeWdID				io
		# io	= disableWindowMouse	windowID				io
		# io	= disableMenuElements	[stepItemId,runItemId]	io
		# io	= disableMenus 			[fileMenuId]			io
		# io	= enableTimer			timerID					io
		= {pst & io=io}
	
	
	//	Set the speed (delay) of a (possibly running) T.M.
	SetDelay :: Int (PSt Tm) -> PSt Tm
	SetDelay delay pst=:{ls=tm,io}
		= {pst & ls={tm & delay=delay},io=setTimerInterval timerID delay io}
	
	
	//	Quit the program.
	DoQuit :: (PSt Tm) -> PSt Tm
	DoQuit pst=:{ls={saved}}
		| saved		= closeProcess pst
		| otherwise	= SaveBeforeClose "quitting" closeProcess pst
	
	
	//	When a MouseDown event occurs (filtered by tmMouseFilter) the T.M. can be edited (handled by EditTransitions).
	tmMouseFilter :: MouseState -> Bool
	tmMouseFilter (MouseDown _ _ _)	= True
	tmMouseFilter _					= False
	
	EditTransitions :: MouseState (PSt Tm) -> PSt Tm
	EditTransitions (MouseDown mpos _ _) pst=:{ls=tm=:{tmstate={turing={transitions}}},io}
		| ontrans			= AlterTransition transnr pst
		| onstate			= AlterState pst
		| otherwise			= pst
	where
		(nr,ontrans,onstate)= ClickedInWindow mpos
		lasttrans			= NrOfTransitions transitions
		transnr				= if (nr>lasttrans) lasttrans nr
	
	
	//	When a MouseDown event occurs (filtered by tpMouseFilter) the head can be moved or the tape content changed (handled by EditTape).
	tpMouseFilter :: MouseState -> Bool
	tpMouseFilter (MouseDown _ _ _)	= True
	tpMouseFilter _					= False
	
	EditTape :: MouseState (PSt Tm) -> PSt Tm
	EditTape (MouseDown mpos {commandDown} _) pst=:{ls=tm=:{tmstate=tmst=:{turing}},io}
		| not ontape
			= pst
		| commandDown
			# tape		= MoveHead newpos turing.tape
			# (frame,io)= getWindowViewFrame tapeWdID io
			# io		= setWindowLook tapeWdID False (True,tpLook tape) io
			# io		= appWindowPicture tapeWdID (ShowHeadMove {tape & head=oldpos} newpos frame.corner1.x frame.corner2.x) io
			= {pst & ls={tm & tmstate={tmst & turing={turing & tape=tape}}},io=io}
		| otherwise
			= AlterCell (min newpos (NrOfCells turing.tape.content)) pst
	where
		oldpos			= turing.tape.head
		(newpos,ontape)	= ClickedInTapeWd mpos
	
	
	//	The window update and activate functions.
	tmLook :: TmState SelectState UpdateState *Picture -> *Picture
	tmLook {turing={transitions,state},transition} _ _ picture
		# picture	= SetTuringFont picture
		# picture	= ShowTransitions transitions state picture
		# picture	= ShowTransition  transition transition	picture
		= picture
	
	tpLook :: Tape SelectState UpdateState *Picture -> *Picture
	tpLook tape selectState updState=:{updArea} picture
		# picture	= SetTuringFont picture
		# picture	= seq [ShowTapePart tape updRect.corner1.x updRect.corner2.x o unfill updRect \\ updRect<-updArea] picture
		= picture
	
	
	//	The step function for the Timer device (used by the Run command).
	TimerStep :: NrOfIntervals (PSt Tm) -> PSt Tm
	TimerStep _ pst=:{ls=tm=:{tmstate={turing={state}}},io}
		| state<>"halt" && state<>"error"
			= DoStep pst
		| otherwise
			# io	= disableTimer        	timerID					io
			# io	= enableWindowMouse		tapeWdID				io
			# io	= enableWindowMouse		windowID				io
			# io	= disableMenuElements	[haltItemId,contItemId]	io
			# io	= enableMenuElements	[runItemId]				io
			# io	= enableMenus			[fileMenuId]			io
			= {pst & io=io}
