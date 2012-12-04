module worm

//	**************************************************************************************************
//
//	The famous Unix game 'worm' (or 'snake').
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO
import wormshow, wormstate, Help, Notice


//	GUI constants.
HelpFile		:== "WormHelp"
HiScoresFile	:== "wormhi"
NrOfHiScores	:== 8

//	The local state.
::	*Local
	=	{	hifile	:: *File
		,	state	:: State
		}

//	Start of the program.
Start :: *World -> *World
Start world
	# (hiscores,world)	= readHiScores HiScoresFile world
	# (ids,     world)	= openIds 8 world
	= startWorm hiscores ids world

startWorm :: !(!*File,!HiScores) ![Id] !*World -> *World
startWorm (hifile,best) ids world
	= startIO SDI {hifile=hifile,state=InitState best} (initialise ids) [] world

initialise :: [Id] !(PSt Local) -> PSt Local
initialise ids pst=:{ls={state={best}}}
	# (error,pst)	= openWindow undef window pst
	| error<>NoError
		= abort "worm could not open window."
	# (error,pst)	= openMenu undef filemenu pst
	| error<>NoError
		= abort "worm could not open file menu."
	# (error,pst)	= openMenu undef optionsmenu pst
	| error<>NoError
		= abort "worm could not open options menu."
	# (error,pst)	= openTimer undef timer pst
	| error<>NoError
		= abort "worm could not open timer."
	| otherwise
		# (seed,pst)= getNewRandomSeed pst
		# pst		= initFoodSupply seed pst
		# pst		= initWindowPicture pst
		= pst
where
	[fileID,playID,haltID,quitID,levelID,contID,windowID,timerID:_]
		= ids		// The global Ids

	initFoodSupply :: RandomSeed (PSt Local) -> PSt Local
	initFoodSupply seed pst=:{ls=ls=:{state=state=:{worm,gamelevel}}}
		# foods				= FoodSupply seed
		  (food,foods)		= NewFood worm gamelevel foods
		= {pst & ls={ls & state={state & food=food,foodsupply=foods}}}
	
	initWindowPicture :: (PSt Local) -> PSt Local
	initWindowPicture pst
		= appPIO (appWindowPicture windowID (setPenFontSize WormFontSize)) pst
	where
		setPenFontSize :: Int *Picture -> *Picture
		setPenFontSize size pict
			# (font,pict)		= getPenFont pict
			  fontDef			= getFontDef font
			# ((_,font),pict)	= openFont {fontDef & fSize=WormFontSize} pict
			# pict				= setPenFont font pict
			= pict
	
	filemenu	= Menu "&File" 
					(	MenuItem "&Play"			[MenuId playID,MenuShortKey 'r',MenuFunction (noLS Play)]
					:+:	MenuItem "&Halt"			[MenuId haltID,MenuShortKey '.',MenuFunction (noLS Halt),MenuSelectState Unable]
					:+:	MenuSeparator				[]
					:+: MenuItem "&About Worm..."	[MenuFunction (noLS (showAbout "Worm" HelpFile))]
					:+:	MenuItem "H&elp"			[MenuFunction (noLS (showHelp HelpFile))]
					:+:	MenuSeparator				[]
					:+:	MenuItem "&Quit"			[MenuId quitID,MenuShortKey 'q',MenuFunction (noLS Quit)]
					)	[MenuId fileID]
	optionsmenu	= Menu "&Options"
					(	RadioMenu
						[	("&Slow"  ,Nothing,Just '1',noLS (SetSpeed EasySpeed)  )
						,	("&Medium",Nothing,Just '2',noLS (SetSpeed MediumSpeed))
						,	("&Fast"  ,Nothing,Just '3',noLS (SetSpeed HardSpeed)  )
						]	1	[]
					:+:	MenuSeparator []
					:+:	MenuItem "&High Scores" [MenuShortKey 'h',MenuFunction (noLS ShowBest)]
					)
					[	MenuId	levelID
					]
	window		= Window "Worm" NilLS
					[	WindowId			windowID
					,	WindowClose			(noLS Quit)
					,	WindowKeyboard		KeyFilter Unable (noLS1 MakeTurn)
					,	WindowPen			[PenBack WormBackGroundColour]
					,	WindowViewDomain	{zero & corner2={x=488,y=303}}
					,	WindowLook			True (UpdateWindow (InitState best))
					]
	timer		= Timer EasySpeed NilLS [TimerId timerID, TimerSelectState Unable, TimerFunction (noLS1 OneStep)]
	
	//	The update function for the playfield window.
	UpdateWindow :: State SelectState UpdateState *Picture -> *Picture
	UpdateWindow state=:{gamelevel,food,points,worm,lives} _ {updArea} pict
		# pict	= seq (map unfill updArea) pict
		= DrawGame gamelevel food points worm lives pict
	
	//	The function for the Play command.
	Play :: (PSt Local) -> PSt Local
	Play pst=:{ls=ls=:{state},io}
		# io	= disableMenus			[levelID]						io
		# io	= disableMenuElements	[playID,quitID]					io
		# io	= enableMenuElements	[haltID]						io
		# io	= setTimerInterval		timerID state.gamelevel.speed	io
		# io	= enableWindowKeyboard	windowID						io
		# io	= enableTimer			timerID							io
		# io	= appWindowPicture		windowID (DrawGame initlevel newfood initpoints initworm initlives)
																		io
		# io	= setWindowCursor		windowID HiddenCursor			io
		= {pst & ls={ls & state=initstate},io=io}
	where
		initlevel		= InitLevel state.gamelevel.fix
		initworm		= NewWorm initlevel
		(newfood,foods1)= NewFood initworm initlevel state.foodsupply
		initpoints		= 0
		initlives		= NrOfWorms
		initstate		= {state & gamelevel	= initlevel
								 , food			= newfood
								 , foodsupply	= foods1
								 , grow			= 0
								 , points		= initpoints
								 , dir			= rightKey
								 , worm			= initworm
								 , lives		= initlives
						  }
	
	//	The functions for the Halt/Continue command(s).
	Halt :: (PSt Local) -> PSt Local
	Halt pst=:{io}
		# io			= setWindowCursor		windowID StandardCursor	io
		# io			= disableWindowKeyboard	windowID				io
		# io			= disableTimer			timerID					io
		# io			= enableMenuElements	[quitID]				io
		# io			= closeMenuElements		fileID [haltID]			io
		# pst			= {pst & io=io}
		# (error,pst)	= openMenuElements		fileID 1 undef continue	pst
		| error<>NoError
			= abort "Worm was not able to open 'Continue' menu item"
		| otherwise
			= pst
	where
		continue		= MenuItem "Continue" [MenuId contID, MenuShortKey '.', MenuFunction (noLS Continue)]
		
		Continue :: (PSt Local) -> PSt Local
		Continue pst=:{io}
			# io			= enableWindowKeyboard	windowID				io
			# io			= enableTimer			timerID					io
			# io			= setWindowCursor		windowID HiddenCursor	io
			# io			= disableMenuElements	[quitID]				io
			# io			= closeMenuElements		fileID [contID]			io
			# pst			= {pst & io=io}
			# (error,pst)	= openMenuElements		fileID 1 undef halt		pst
			| error<>NoError
				= abort "Worm could not open MenuItem 'Halt'"
			| otherwise
				= pst
		where
			halt			= MenuItem "Halt" [MenuId haltID, MenuShortKey '.', MenuFunction (noLS Halt)]
	
	//	The function for the Quit command: stop the program and write the high scores to file.
	Quit :: (PSt Local) -> PSt Local
	Quit pst=:{ls=ls=:{hifile,state={best}}}
		# pst				= {pst & ls={ls & hifile=undef}}
		# pst				= closeProcess pst
		= writeHiScores hifile best pst
	
	//	Set a new speed (called when one of the Options commands is chosen).
	SetSpeed :: Int (PSt Local) -> PSt Local
	SetSpeed fix pst=:{ls=ls=:{state}}
		= {pst & ls={ls & state={state & gamelevel={state.gamelevel & fix=fix,speed=fix}}}}
	
	//	Show the high scores.
	ShowBest :: (PSt Local) -> PSt Local
	ShowBest pst=:{ls={state={best}}}
		= showHiScores "Worm High Scores:" best pst
	
	//	The MakeTurn function is called when an arrow key is pressed.
	KeyFilter :: KeyboardState -> Bool
	KeyFilter (SpecialKey key (KeyDown _) _)
		= isMember key [downKey,leftKey,rightKey,upKey]
	KeyFilter _
		= False
	
	MakeTurn :: KeyboardState (PSt Local) -> PSt Local
	MakeTurn (SpecialKey key _ _) pst=:{ls=ls=:{state=state=:{dir}}}
		| (dir==upKey   || dir==downKey)  && (key==leftKey || key==rightKey)	= OneStep 1 {pst & ls={ls & state={state & dir=key}}}
		| (dir==leftKey || dir==rightKey) && (key==upKey   || key==downKey )	= OneStep 1 {pst & ls={ls & state={state & dir=key}}}
		| otherwise																= pst
	
	//	The function for the Timer device: do one step of the worm game.
	OneStep :: NrOfIntervals (PSt Local) -> PSt Local
	OneStep _ pst=:{ls=ls=:{state=state=:{gamelevel,food,foodsupply,grow,points,dir,worm,best,lives}},io}
		| newlevel<>curlevel	= SwitchLevel gamelevel foodsupply points2 points best lives pst
		# state					= {state & food=food1,foodsupply=foods1,grow=grow1,points=points2,worm=worm1}
		| collide				= NextLife {pst & ls={ls & state=state}}
		# io					= appWindowPicture windowID (DrawStep scored food food1 points2 (hd worm) head tail) io
		| not scored			= {pst & ls={ls & state=state},io=io}
		| otherwise				= {pst & ls={ls & state=state},io=beep io}
	where
		(head,tail,worm1)		= StepWorm dir grow worm
		scored					= head==food.pos
		collide					= Collision gamelevel worm head
		value					= food.value
		(food1,foods1)			= if scored (NewFood worm1 gamelevel foodsupply) (food,foodsupply)
		grow1					= if scored (grow+value*3/2) (max 0 (grow-1))
		points1					= if scored (points+value*(length worm1)/2) points
		points2					= if collide (max 0 (points1-100)) points1
		curlevel				= points /PointsPerLevel
		newlevel				= points2/PointsPerLevel
		
		Collision :: Level Worm Segment -> Bool
		Collision level worm head
			=  (not (InRectangle head {corner1={x=1,y=1},corner2={x=SizeX,y=SizeY}}))
			|| (any (InRectangle head) level.obstacles)
			|| (isMember head worm)
		where
			InRectangle :: Point2 Obstacle -> Bool
			InRectangle {x,y} {corner1={x=lx,y=ty},corner2={x=rx,y=by}}
				= x>=lx && x<=rx && y>=ty && y<=by
		
		StepWorm :: SpecialKey Grow Worm -> (Segment,Segment,Worm)
		StepWorm dir 0 worm
			= (head,tail,[head:worm1])
		where
			(tail,worm1)= GetAndRemoveLast worm
			head		= NewHead dir (hd worm)
			
			GetAndRemoveLast :: ![.x] -> (.x,![.x])
			GetAndRemoveLast [x]
				= (x,[])
			GetAndRemoveLast [x:xs]
				= (x1,[x:xs1])
			where
				(x1,xs1)	= GetAndRemoveLast xs
		StepWorm dir _ worm
			= (head,zero,[head:worm])
		where
			head	= NewHead dir (hd worm)
		
		NewHead :: SpecialKey Segment -> Segment
		NewHead key segment=:{x,y}
			| key==upKey	= {segment & y=y-1}
			| key==downKey	= {segment & y=y+1}
			| key==leftKey	= {segment & x=x-1}
			| key==rightKey	= {segment & x=x+1}
			| otherwise		= abort ("NewHead applied to unknown SpecialKey: "+++toString key)
		
		SwitchLevel :: Level [Food] Points Points HiScores Lives (PSt Local) -> PSt Local
		SwitchLevel curlevel foods newPoints oldPoints high lives pst=:{ls,io}
			# (id,io)	= openId io
			= NextLevelAnimation id {pst & ls={ls & state=newstate},io=io}
		where	
			newlevel		= if (newPoints>oldPoints) (IncreaseLevel curlevel) (DecreaseLevel curlevel)
			initworm		= NewWorm newlevel
			(newfood,foods1)= NewFood initworm newlevel foods
			newstate		= {	gamelevel	= newlevel
							  ,	food		= newfood
							  ,	foodsupply	= foods1
							  ,	grow		= 0
							  ,	points		= newPoints
							  ,	dir			= rightKey
							  ,	worm		= initworm
							  ,	best		= high
							  ,	lives		= if (newPoints>oldPoints) (lives+1) (lives-1)
							  }
			
			NextLevelAnimation :: Id (PSt Local) -> PSt Local
			NextLevelAnimation id pst=:{io}
				# io			= disableWindowKeyboard windowID	io
				# io			= disableTimer			timerID		io
				# pst			= {pst & io=io}
				# (error,pst)	= openTimer (nrAnimationSteps,-1) (Timer (ticksPerSecond/30) NilLS
																	[	TimerId			id
																	,	TimerFunction	BetweenLevels
																	]) pst
				| error<>NoError= abort "Worm could not open timer for animation"
				| otherwise		= pst
			where
				nrAnimationSteps= 40
				
				BetweenLevels :: NrOfIntervals ((Int,Int),PSt Local) -> (*(Int,Int),PSt Local)
				BetweenLevels _ ((animationStep,step),pst=:{ls={state={gamelevel,food,points,worm,lives}},io})
					| animationStep<=1
						= ((2,1),pst)
					| animationStep<=nrAnimationSteps
						# io		= appWindowPicture windowID (DrawAnimation animationStep step) io
						= ((animationStep+step,step),{pst & io=io})
					| otherwise
						# io		= appWindowPicture		windowID (DrawGame gamelevel food points worm lives)	io
						# io		= enableTimer			timerID													io
						# io		= closeTimer			id														io
						# io		= enableWindowKeyboard	windowID												io
						= ((animationStep,step),{pst & io=io})
		
		NextLife :: (PSt Local) -> PSt Local
		NextLife pst=:{ls=ls=:{state=state=:{gamelevel,foodsupply,points,best,worm,lives}},io}
			| lives>0
				# (id,io)	= openId io
				= DeadWormAlert id worm {pst & ls={ls & state={state & food       = newfood
																	 , foodsupply = foods1
																	 , grow       = 0
																	 , dir        = rightKey
																	 , worm       = newworm
																	 , lives      = lives-1
															  }
												   }
											 , io=io
										}
			with
				(newfood,foods1)= NewFood newworm gamelevel foodsupply
				newworm			= NewWorm gamelevel
				
				DeadWormAlert :: Id Worm (PSt Local) -> PSt Local
				DeadWormAlert id worm pst=:{io}
					# io			= disableTimer			timerID		io
					# io			= disableWindowKeyboard	windowID	io
					# pst			= {pst & io=io}
					# (error,pst)	= openTimer {corpse=worm} (Timer (ticksPerSecond/30) NilLS [TimerId id,TimerFunction DeadWorm]) pst
					| error<>NoError= abort "Worm could not open a timer for desintegration."
					| otherwise		= pst
				where
					DeadWorm :: NrOfIntervals (*DeadWormSt,PSt Local) -> (*DeadWormSt,PSt Local)
					DeadWorm _ (deadWormSt=:{corpse=[segment:rest]},pst=:{io})
						# io	= appWindowPicture windowID (EraseSegment segment) io
						= ({deadWormSt & corpse=rest},{pst & io=io})
					DeadWorm _ (deadWormSt,pst=:{ls={state={gamelevel,food,points,worm,lives}},io})
						# io	= appWindowPicture		windowID (DrawGame gamelevel food points worm lives)	io
						# io	= enableTimer			timerID													io
						# io	= closeTimer			id														io
						# io	= enableWindowKeyboard	windowID												io
						= (deadWormSt,{pst & io=io})
			
			# io	= enableMenus			[levelID]		io
			# io	= enableMenuElements	[playID,quitID]	io
			# io	= disableMenuElements	[haltID]		io
			# io	= disableTimer			timerID			io
			# io	= disableWindowKeyboard	windowID		io
			# io	= setWindowCursor		windowID StandardCursor io
			| itsAHighScore NrOfHiScores points best
				# (ids,io)	= openIds 3 io
				= snd (openModalDialog undef (dialog ids) {pst & io=io})
			with
				dialog [overId,okId,editId:_]
					= Dialog "Game Over"
						(	TextControl	  "Game Over with a new high score!"	[ControlPos (Left,zero)]
						:+:	TextControl	  "Your name:"							[ControlPos (Left,zero)]
						:+:	EditControl   "" (PixelWidth (hmm 45.0)) 1			[ControlId	editId]
						:+:	ButtonControl "OK"									[ControlPos (Center,zero),ControlFunction (noLS OverOK)]
						)
						[	WindowId		overId
						,	WindowOk		okId
						,	WindowItemSpace	(hmm 6.0) (vmm 6.0)
						]
				where
					OverOK :: (PSt Local) -> PSt Local
					OverOK pst=:{ls=ls=:{state}}
						# (maybeDialog,pst)		= accPIO (getWindow overId) pst
						| isNothing maybeDialog	= abort "OK button could not retrieve WState of 'Game Over' dialog."
						# name					= fromJust (snd (getControlText editId (fromJust maybeDialog)))
						| name==""				= closeWindow overId pst
						| otherwise				= closeWindow overId (addscore name pst)
					where
						addscore :: String (PSt Local) -> PSt Local
						addscore name pst=:{ls=ls=:{state}}
							# best	= addScore NrOfHiScores {name=name,score=state.points} state.best
							= {pst & ls={ls & state={state & best=best}}}
			| otherwise
				= openNotice (Notice ["Game Over, no high score."] (NoticeButton "OK" id) []) {pst & io=io}

::	DeadWormSt
	=	{	corpse	:: Worm		// The corpse of the worm that desintegrates
		}
