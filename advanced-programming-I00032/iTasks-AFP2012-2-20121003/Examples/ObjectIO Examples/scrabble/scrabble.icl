module	scrabble


//	**************************************************************************************************
//
//	This program implements the Scrabble game (without blank letters).
//	Original program written by Paul de Mast in the functional programming language Amanda.
//	This program is the translated and adapted version to Clean 1.3.2.
//	It uses the Clean Standard Object I/O library 1.2.
//	
//	**************************************************************************************************


import	StdEnv, StdIO
import	board, graphics, state, language, systemsettings
import	Help, ListBox


/***************************************************************************************************************
	The Start rule creates the GUI of the scrabble game and the initial program state.
***************************************************************************************************************/
Start :: *World -> *World
Start world
	# (ids,     world)	= openIds 12 world
	# (wordlist,world)	= readtree  world
	= startIO SDI (initstate wordlist) (initGUI ids) [] world

initGUI :: [Id] (PSt *State) -> PSt *State
initGUI ids pst=:{ls={player1={kind=kind1},player2={kind=kind2},strength}}
	# (error,pst)	= openWindow undef (wdef (hd ids)) pst
	| error<>NoError
		= abort "Scrabble could not open window."
	# (error,pst)	= openMenu undef (scrabblemenu (kind1,kind2)) pst
	| error<>NoError
		= abort "Scrabble could not open menu."
	# (error,pst)	= openMenu undef (strengthmenu strength) pst
	| error<>NoError
		= abort "Scrabble could not open menu."
	# (error,pst)	= openMenu undef helpmenu pst
	| error<>NoError
		= abort "Scrabble could not open menu."
	# (error,pst)	= openTimer undef timer pst
	| error<>NoError
		= abort "Scrabble could not open timer."
	| otherwise
		# pst	= initialisestate pst
		# pst	= scrabblepanel pst
		# pst	= arbitrate pst
		= pst
where
	[scrabbleId,computerId,letterboxId,boardId,player1scoreId,letters1Id,player2scoreId,letters2Id,displayId,directionId,editId,okId:_]
		= ids		// These are the global Ids
//	The Scrabble window:
	wdef id	= Window "Scrabble" NilLS
				[	WindowId			id
				,	WindowHMargin		10 10
				,	WindowVMargin		10 10
				,	WindowItemSpace		10 10
				,	WindowPen			[PenBack rbBoardGrey]
				,	WindowOk			okId
				,	WindowViewSize		{w=10+squarewidth*4+10+boardwidth+10+displaywidth+10,h=10+squareheight*15+10}
				,	WindowLook			True (\_ {updArea}->seq (map unfill updArea))
				]
	
//	The Scrabble menu:
	scrabblemenu (kind1,kind2)
		= Menu scrabblemenutitle 
			(	SubMenu playersmenutitle
				(	RadioMenu
					[	(computer+++"/"+++person,  Nothing,Nothing, noLS (setplayerkinds Computer Person  ))
					,	(computer+++"/"+++computer,Nothing,Just 'c',noLS (setplayerkinds Computer Computer))
					,	(person  +++"/"+++person,  Nothing,Just 'p',noLS (setplayerkinds Person   Person  ))
					,	(person  +++"/"+++computer,Nothing,Nothing, noLS (setplayerkinds Person   Computer))
					] 	initmark []
				)	[]
			:+:	MenuItem newgametitle
				[	MenuShortKey 'n'
				,	MenuFunction (noLS new)
				]
			:+:	MenuItem quitgametitle
				[	MenuShortKey 'q'
				,	MenuFunction (noLS quit)
				]
			)	[]
	where
		computer	= toString Computer
		person		= toString Person
		initmark
			| kind1==Computer&& kind2==Person	= 1
			| kind1==Computer&& kind2==Computer	= 2
			| kind1==Person  && kind2==Person	= 3
			| kind1==Person  && kind2==Computer	= 4
		
		setplayerkinds :: Playerkind Playerkind (PSt *State) -> PSt *State
		setplayerkinds s1 s2 pst=:{ls=t=:{player1,player2}}
			= new {pst & ls={t & player1={player1 & kind=s1},player2={player2 & kind=s2}}}
		
		new :: (PSt *State) -> PSt *State
		new pst
			# pst	= initialisestate pst
			# pst	= scrabblepanel pst
			# pst	= arbitrate pst
			= pst
		
		quit :: (PSt *State) -> PSt *State
		quit pst=:{ls={wordsadded,lexicon}}
			| not wordsadded
				= closeProcess pst
			| otherwise
				= snd (openModalDialog undef save pst)
		where
			save	= Dialog "" 
						(	ListLS
							[	TextControl line [ControlPos (Left,zero)]
							\\	line <- save_notice_text
							]
						:+:	ButtonControl save_notice_yes
							[	ControlFunction (noLS (\pst->closeProcess (writetree lexicon pst)))
							,	ControlPos		(Center,zero)
							]
						:+:	ButtonControl save_notice_no
							[	ControlFunction (noLS closeProcess)
							]
						)	[]

//	The Strength menu:
	strengthmenu strength
		= Menu strengthmenutitle
			(	RadioMenu
				[	(toString Maximum,         Nothing,Nothing,noLS (setstrength Maximum         ))
				,	(toString MediumStrength,  Nothing,Nothing,noLS (setstrength MediumStrength  ))
				,	(toString EasyStrength,    Nothing,Nothing,noLS (setstrength EasyStrength    ))
				,	(toString VeryEasyStrength,Nothing,Nothing,noLS (setstrength VeryEasyStrength))
				,	(toString First,           Nothing,Nothing,noLS (setstrength First           ))
				]
				initstrength []
			)	[]
	where
		initstrength
			| strength==Maximum			= 1
			| strength==MediumStrength	= 2
			| strength==EasyStrength	= 3
			| strength==VeryEasyStrength= 4
			| strength==First			= 5
		
		setstrength :: Strength (PSt *State) -> PSt *State
		setstrength nst pst=:{ls=t}
			= {pst & ls={t & strength=nst}}

//	The Help menu:
	helpmenu
		= Menu "Help"
			(	MenuItem "About Scrabble"	[MenuFunction (noLS (showAbout "Scrabble" helpfilename))]
			:+:	MenuItem "Help"				[MenuFunction (noLS (showHelp helpfilename))]
			)	[]

//	The timer is the computer player:
	timer
		= Timer 0 NilLS
			[	TimerSelectState	Unable
			,	TimerFunction		(noLS1 computer)
			,	TimerId				computerId
			]
	
//	The scrabble game is played in a Dialog:
	scrabblepanel :: (PSt *State) -> PSt *State
	scrabblepanel pst=:{ls={lexicon,player1,player2,player,letterbox,boardinput},io}
		# (wordId,io)		= openId io
		# io				= closeAllControls scrabbleId io
		# pst				= {pst & io=io}
		# (error,pst)		= openControls scrabbleId undef (controls wordId) pst
		| error<>NoError	= abort ("Scrabble could not reopen Scrabble controls: "+++toString error)
		| otherwise			= pst
	where
		controls wordId
			=	letterboxcontrol
			:+:	boardcontrol
			:+:	playercontrol Player1 player1.letters letters1Id player1scoreId [ControlPos (RightToPrev,     OffsetVector {zero & vx=10})]
			:+:	playercontrol Player2 player2.letters letters2Id player2scoreId [ControlPos (Below letters1Id,OffsetVector {zero & vy=10})]
			:+:	displaycontrol
			:+:	textinputcontrol
			:+:	directioncontrol
			:+:	buttoncontrol
		where
			letterboxcontrol
				=	CustomControl sizeletterbox (letterboxlook letterbox)
						[	ControlSelectState	Unable
						,	ControlPen			[PenBack rbBackground]
						,	ControlId			letterboxId
						]
			boardcontrol
				=	CustomControl {w=boardwidth,h=boardheight} (boardlook initboard boardinput)
						[	ControlSelectState	(if personplaying Able Unable)
						,	ControlMouse		boardFilter Able (noLS1 boardfeel)
						,	ControlId			boardId
						,	ControlPen			[PenBack rbBoardGrey]
						]
			playercontrol player letters lettersid scoreid pos
				= 	TextControl (toString player+++":")	pos
				:+:	CustomControl sizeletters (playerletterslook letters)
						[	ControlSelectState	Unable
						,	ControlPos			(BelowPrev,OffsetVector zero)
						,	ControlPen			[PenBack rbBackground]
						,	ControlId			lettersid
						]
				:+:	TextControl (scrabbledialogscore+++":")
						[	ControlPos			(RightToPrev,OffsetVector {vx=10,vy=0-sizeletters.h})
						]
				:+:	TextControl "     0"
						[	ControlId			scoreid
						,	ControlPos			(BelowPrev,OffsetVector zero)
						]
			displaycontrol
				=	CustomControl  {w=displaywidth,h=displayheight} (displaylook (scrabbledialoginittext lexicon))
						[	ControlPos			(Below letters2Id,OffsetVector {zero & vy=20})
						,	ControlSelectState	Unable
						,	ControlId			displayId
						,	ControlPen			[PenBack Black]
						]
			textinputcontrol
				=	TextControl (scrabbledialogword+++":")
						[	ControlPos			(BelowPrev,OffsetVector {zero & vy=20})
						,	ControlId			wordId
						,	ControlSelectState	(if personplaying Able Unable)
						]
				:+:	EditControl "" (PixelWidth 80) 1
						[	ControlId			editId
						,	ControlPos			(RightToPrev,OffsetVector {zero & vx=5})
						,	ControlSelectState	(if personplaying Able Unable)
						]
			directioncontrol
				=	TextControl scrabbledialogdirection
						[	ControlPos			(Below wordId,zero)
						,	ControlSelectState	(if personplaying Able Unable)
						]
				:+:	RadioControl [(toString Horizontal,Nothing,id),(toString Vertical,Nothing,id)] (Columns 1) 1
						[	ControlPos			(Below editId,zero)
						,	ControlId			directionId
						,	ControlSelectState	(if personplaying Able Unable)
						]
			buttoncontrol
				=	ButtonControl scrabbledialogplaceword
						[	ControlId			okId
						,	ControlPos			(RightTo editId,zero)
						,	ControlSelectState	(if personplaying selectstateplaceword Unable)
						,	ControlFunction		(noLS placeword)
						]
		sizeletterbox	= {w=squarewidth*4,h=squareheight*15}
		sizeletters		= {w=squarewidth*7,h=squareheight}
		personplaying	= player1.kind==Person || player2.kind==Person
		selectstateplaceword
			| player==Player1 && player1.kind==Person	= Able
			| player==Player2 && player2.kind==Person	= Able
			| otherwise									= Unable
		
		boardFilter :: MouseState -> Bool
		boardFilter (MouseDown _ _ _)	= True
		boardFilter _					= False
		
		boardfeel :: MouseState (PSt *State) -> PSt *State
		boardfeel (MouseDown pos _ _) pst=:{ls=state=:{board,boardinput=oldpos}}
			= appPIO (	setControlLooks [(boardId,False,(True,boardlook board pos))] 
					 o	appControlPicture boardId (drawfocus True pos o drawfocus False oldpos)
					 ) {pst & ls={state & boardinput=pos}}

//	The user request the placement of a word:
	placeword :: (PSt *State) -> PSt *State
	placeword pst
		# (maybeScrabble,pst)	= accPIO (getWindow scrabbleId) pst
		| isNothing maybeScrabble
			= abort "placeword could not retrieve WState from Scrabble window."
		| otherwise
			# pst				= placeword` (fromJust maybeScrabble) pst
			# pst				= setActiveControl editId pst
			# pst				= appPIO (setEditControlSelection editId 1 0) pst
			= pst
	
	placeword` :: WState (PSt *State) -> PSt *State
	placeword` info pst=:{ls=t=:{	board
								,	playmode
								,	dimensions=(minx,maxx,miny,maxy)
								,	player
								,	player1
								,	player2
								,	letterbox
								,	lexicon
								,	random
								,	boardinput={x,y}
								}
					    ,io}
		| lastword==""
			= arbitrate {pst & ls=nt,io=drawplayerletters player newplayerletters (drawcommunication displayId text io)}
		with
			nt		= {t2 & random=rs1,letterbox=restletterbox}
			t2		= case player of
						Player1	-> {t1 & playmode= EndPlayer1
									   , player1 = {t1.player1 & letters=newplayerletters,placedword=False}}
						_		-> {t1 & playmode= EndPlayer2
									   , player2 = {t1.player2 & letters=newplayerletters,placedword=False}}
			
			text	= [toString player+++exchanges_letters]
			(restletterbox,newplayerletters,rs1)
					= grab (playerletters++letterbox) 7 random
		
		| not (isMemberDictionary lastword lexicon)
			# (addWordsId,io)	= openId io
			# (listboxId, io)	= openListBoxId io
			# (listbox,   io)	= ListBoxControl (length unknownwords+1) [lastword:unknownwords] [] listboxId
									[	ControlPos		(Center,zero)
									,	ControlViewSize {w=100,h=50}
									] io
			= snd (openModalDialog undef (newwordspanel addWordsId listbox [lastword:unknownwords]) {pst & ls=t1,io=io})
		
		| outsideboard
			= {pst & ls=t1,io=drawcommunication displayId text io}
		with
			text	= [	toString player+++":" : placement_error lastword (i+1,j+1) ]
		
		| not (isEmpty missingletters)
			= {pst & ls=t1,io=drawcommunication displayId text io}
		with
			text	= [	toString player+++":" : missing_letters_error missingletters ]
		
		| not possible
			= {pst & ls=t1,io=drawcommunication displayId [ toString player+++":" : anonymous_placement_error ] io}
		
		| isEmpty usedletters
			= {pst & ls=t1,io=drawcommunication displayId [ toString player+++":" : no_letters_used_error ] io}
		
		| not (isEmpty unknownwords)
			# (addWordsId,io)	= openId io
			# (listboxId, io)	= openListBoxId io
			# (listbox,   io)	= ListBoxControl (length unknownwords+1) [lastword:unknownwords] [] listboxId
									[	ControlPos		(Center,zero)
									,	ControlViewSize	{w=100,h=50}
									] io
			= snd (openModalDialog undef (newwordspanel addWordsId listbox unknownwords) {pst & ls=t1,io=io})
		
		| otherwise
			# io	= redrawboard boardId nb {x=x,y=y} io
			# io	= drawcommunication displayId text io
			# io	= drawplayerinfo player totalscore newplayerletters io
			= arbitrate {pst & ls=nt,io=io}
		with
			nt					= case player of
									Player1	-> {nt1 & player1	= setplayer newplayerletters totalscore True nt1.player1
													, playmode	= EndPlayer1}
									_		-> {nt1 & player2	= setplayer newplayerletters totalscore True nt1.player2
													, playmode	= EndPlayer2}
			setplayer letters score placed player
								= {player & letters=letters,points=score,placedword=placed}
	    	nt1 				= {t1  & letterbox	= restletterbox
									   , dimensions	= newdimensions
									   , board		= nb
									   , random		= rs1
								  }
			newplayerletters	= remainingletters++replenishletters
			(restletterbox,replenishletters,rs1)
								= grab letterbox (7-length remainingletters) random
			text				= nr_new_words_placed ((length newwords)+1) [lastword:newwords]
	where
		direction			= if (fromJust (snd (getRadioControlSelection directionId info))==1)
								Horizontal
								Vertical
	    lastword			= fromJust (snd (hd (getControlTexts [editId] info)))
		t1					= {t & direction=direction}
		
		(playerletters,playerscore)
							= playerinfo
		playerinfo
			| player==Player1	= (player1.letters,player1.points)
			| otherwise		= (player2.letters,player2.points)
		newdimensions
			| direction==Horizontal
							= (min i minx, max (i+wordlength-1) maxx, min j miny, max j maxy)
			| otherwise		= (min i minx, max i maxx, min j miny, max (j+wordlength-1) maxy)
		
		outsideboard		= (direction==Horizontal && ((i+wordlength<minx)||(i>maxx+1)||(j<miny-1)||(j>maxy+1)))
								||
							  (direction==Vertical && ((i<minx-1)||(i>maxx+1)||(j+wordlength<miny-1)||(j>maxy+1)))
								||
							  (isEmpty newwords && length usedletters==wordlength && not firstturn)
		
		unknownwords		= filter (\word->not (isMemberDictionary word lexicon)) newwords
		
		wordlength			= size lastword
		firstturn			= player1.points+player2.points==0
		
		totalscore			= if (length usedletters==7) (playerscore+score+50) (playerscore+score)
		missingletters		= removeMembers usedletters playerletters
		remainingletters	= removeMembers playerletters usedletters
		(nb,possible,usedletters,score,newwords)
							= tryaddword board lastword (i,j) direction
		(i,j)				= abs2rel (x,y)


//	arbitrate determines who's to play:
	arbitrate :: (PSt *State) -> PSt *State
	arbitrate pst=:{ls=t=:{playmode,player,player1,player2,letterbox},io}
		| isEmpty letterbox && not player1.placedword && not player2.placedword
			# io	= disableTimer computerId  io
			# io	= drawcommunication displayId [text] io
			= {pst & io=io}
		with
			text		= if (player1.points>player2.points) (toString Player1+++has_won)
						( if (player2.points>player1.points) (toString Player2+++has_won)
															 is_a_draw
						)
		
		| (player==Player1 && playmode==EndPlayer1 && player2.kind==Computer) ||
		  (player==Player2 && playmode==EndPlayer2 && player1.kind==Computer)
			# io	= drawcommunication displayId [toString nextplayer+++is_move] io
			# io	= drawletterbox letterboxId letterbox io
			# io	= disableControls [okId] io
			# io	= enableTimer computerId io
			= {pst & ls=nt,io=io}
		with
			(boardletters,t1)	= getboardletters t
			playerletters		= if (nextplayer==Player1) player1.letters player2.letters
			initprogress		= Letter firstletter initplacing
			sortedletters		= sort (filter ((<>) ' ') (removeDup (playerletters++boardletters)))
			firstletter			= if (isEmpty sortedletters) '@' (hd sortedletters)
			nt					= {t1 &	progress	= initprogress
									  ,	player		= nextplayer
									  ,	playmode	= Playing
								  }
		
		| playmode==EndPlayer1 || playmode==EndPlayer2
			# io	= drawcommunication displayId [toString nextplayer+++is_move] io
			# io	= drawletterbox letterboxId letterbox io
			# io	= enableControls [okId] io
			# io	= disableTimer computerId io
			= {pst & ls={t & player=nextplayer,playmode=Playing},io=io}
		
		| otherwise
			= pst
	where
		nextplayer	= otherplayer player
	

//	The computer player (a timer) determines a move:
	computer :: NrOfIntervals (PSt *State) -> PSt *State
	computer _ pst=:{ls=t=:{	board
						   ,	dimensions
						   ,	player
						   ,	player1
						   ,	player2
						   ,	strength
						   ,	playmode
						   ,	lexicon
						   ,	letterbox
						   ,	progress
						   ,	random
						   ,	boardinput
						   }
				   ,io}
		| notyetready progress
			= {pst & ls=nt, io=drawprogress displayId player progress newplacing io}
		with
			(newplacing,t2)	= getnewplacing t1
			nt				= {t2 & progress=newprogress}
	
			getnewplacing :: *State -> (Placing,*State)
			getnewplacing t=:{	board
							 ,	dimensions
							 ,	player
							 ,	player1
							 ,	player2
							 ,	strength
							 ,	lexicon
							 ,	progress
							 }
				| isMember (getletter progress) playerletters
					= (newmaximumplacings board lexicon playerletters dimensions progress strength firstturn,t)
				| otherwise
					= (newmaximumplacing board lexicon playerletters (horpos,verpos) progress strength firstturn,t)
			where
				playerletters	= case player of
									Player1	-> player1.letters
									_		-> player2.letters
				horpos			= getfreehorpositions board (getletter progress)
				verpos			= getfreeverpositions board (getletter progress)
				firstturn		= player1.points+player2.points==0
			
			newprogress
				| lastletter<>'z' && newletter<>'@'
								= Letter newletter newplacing
				| otherwise		= Finish newplacing
			where
				lastletter		= getletter progress
				nextletters		= dropWhile (\l->(l<=lastletter)) (sort (filter ((<>) ' ') (removeDup (playerletters++boardletters))))
				newletter		= if (isEmpty nextletters) '@' (hd nextletters)
			
		| wordfound
			# io	= redrawboard boardId nb boardinput io
			# io	= drawcommunication displayId (nr_new_words_placed ((length newwords)+1) [w:newwords]) io
			# io	= drawplayerinfo player totalscore newplayerletters io
			= arbitrate {pst & ls=ntready,io=io}
			
		| otherwise
			# io	= drawcommunication displayId [toString Computer+++exchanges_letters] io
			# io	= drawplayerletters player newplayerletters io
			= arbitrate {pst & ls=ntready,io=io}
			
	where
		ntready
			| player==Player1		= {nt1 & player1 = {nt1.player1 & letters=newplayerletters,points=totalscore,placedword=wordfound}
										   , playmode= EndPlayer1}
			| otherwise				= {nt1 & player2 = {nt1.player2 & letters=newplayerletters,points=totalscore,placedword=wordfound}
										   , playmode= EndPlayer2}
		nt1							= {t1  & board		= nb
										   , letterbox	= restletterbox
										   , dimensions	= newdimensions
										   , random		= rs1
									  }
		(boardletters,t1)			= getboardletters t
		placing						= getplacing progress
		w							= placing.word
		r							= placing.dir
		pos							= placing.pos
		(i,j)						= pos
		wordlength					= size w
		wordfound					= wordlength>0
		(minx,maxx,miny,maxy)		= dimensions
		newdimensions
			| not wordfound			= dimensions
			| r==Horizontal			= (min i minx, max (i+wordlength-1) maxx, min j miny, max j maxy)
			| otherwise				= (min i minx, max i maxx, min j miny, max (j+wordlength-1) maxy)
		newplayerletters
			| not wordfound			= replenishletters
			| otherwise				= remainingletters++replenishletters
		(restletterbox,replenishletters,rs1)
									= grabletters
		grabletters
			| not wordfound			= grab (playerletters++letterbox) 7 random
			| otherwise				= grab letterbox (7-length remainingletters) random
		
		(playerletters,playerscore)	= playerinfo
		playerinfo
			| player==Player1		= (player1.letters,player1.points)
			| otherwise				= (player2.letters,player2.points)
		totalscore					= playerscore+score
		remainingletters			= removeMembers playerletters usedletters
		
		(nb,_,usedletters,score,newwords)
									= tryaddword board w pos r
	
	newwordspanel addWordsId listbox words
		= Dialog addwordstitle
			(	TextControl		announce1		[ControlPos (Center,zero)]
			:+:	TextControl		announce2		[ControlPos (Center,zero)]
			:+:	listbox
			:+:	ButtonControl	addwords_no		[ControlPos (Center,zero),ControlFunction (noLS (closeWindow addWordsId))]
			:+:	ButtonControl	addwords_yes	[ControlFunction (noLS add)]
			)
			[	WindowId	addWordsId
			]
	where
		(announce1,announce2)	= addwordsheading (length words)
		
		add :: (PSt *State) -> PSt *State
		add pst=:{ls=t=:{lexicon}}
			# pst	= {pst & ls={t & lexicon=seq (map addToDictionary words) lexicon,wordsadded=True}}
			# pst	= closeWindow addWordsId pst
			# pst	= placeword pst
			= pst
		
//	Auxiliary functions:
	drawplayerletters :: Player [Char] (IOSt .l) -> IOSt .l
	drawplayerletters player letters io
		| player==Player1	= drawplayer1letters letters1Id letters io
		| otherwise			= drawplayer2letters letters2Id letters io
	
	drawplayerinfo :: Player Int [Char] (IOSt .l) -> IOSt .l
	drawplayerinfo player score letters io
		| player==Player1	= drawplayer1score player1scoreId score (drawplayer1letters letters1Id letters io)
		| otherwise			= drawplayer2score player2scoreId score (drawplayer2letters letters2Id letters io)
