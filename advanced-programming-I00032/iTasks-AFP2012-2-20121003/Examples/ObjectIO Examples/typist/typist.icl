module typist


//	**************************************************************************************************
//
//	This program creates two interactive processes:
//	-	One process presents a simple text window in which text can be typed (this module).
//	-	The other process keeps track of the number of typed keys per minute (monitor module).
//	Communication is done by means of message passing.
//	In a future distributed version the two processes can be run on different processors.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO
import monitor


::	Typist							// The local process state
	=	{	firstKeyTyped	:: Bool	// The first key is typed in the typist window
		}


NrOfLines			:== 10
NrOfMaxCharsPerLine	:== 30


Start :: *World -> *World
Start world
	# (font,    world)	= accScreenPicture openDialogFont world
	# (metrics, world)	= accScreenPicture (getFontMetrics font) world
	# (rId,     world)	= openRId world
	# (ids,     world)	= openIds 5 world
	= startProcesses (typistProcess font metrics rId ids) world

typistProcess :: Font FontMetrics (RId MonitorMessage) [Id] -> Process
typistProcess font metrics rId ids=:[wId,editId,mId,runId,tId]
	= Process SDI {firstKeyTyped=False} initIO [ProcessClose quit]
where
//	initIO initialises the typist process.
	initIO typist
		# (error,typist)		= openWindow undef window typist
		| error<>NoError
			= abort "Typist could not open window."
		# (error,typist)		= openMenu undef menu typist
		| error<>NoError
			= abort "Typist could not open menu."
		# (error,typist)		= openTimer undef timer typist
		| error<>NoError
			= abort "Typist could not open timer."
		| otherwise
			# (Just pos,typist)	= accPIO (getWindowPos wId) typist
			  monitorpos		= (LeftTop,OffsetVector {pos & vy=pos.vy+wSize.h})
			= openMonitor monitorpos rId typist
	where
	//	window is the single document of the typist process. 
	//	Keyboard information is sent to the monitor process.
		window	= Window "Typist window" 
					(	EditControl "" (PixelWidth (wSize.w-2*metrics.fMaxWidth)) NrOfLines
					[	ControlKeyboard		keyFilter Able (noLS1 sendKeys)
					,	ControlId			editId
					,	ControlSelectState	Unable
					,	ControlPos			(Center,zero)
					,	ControlTip			"Type your text in here"
					,	ControlResize		(\_ _ newWindowSize->newWindowSize)
					]
					)
					[	WindowId			wId
					,	WindowViewSize		wSize
					]
		where
	//		Filter only non-repeating character keys to the monitor process.
			keyFilter (CharKey _ (KeyDown repeat))
				= not repeat
			keyFilter (SpecialKey key (KeyDown repeat) _)
				= isMember key [backSpaceKey,deleteKey] && not repeat
			keyFilter _
				= False
			
	//		Key messages start with BeginSession so that monitoring will start after the first key hit.
	//		Only after the first key hit the timer stopwatch is activated.
			sendKeys :: KeyboardState (PSt Typist) -> PSt Typist
			sendKeys keyboard typist=:{ls=local=:{firstKeyTyped}}
				| firstKeyTyped
					# (_,typist)	= asyncSend rId (KeyHit char) typist
					= typist
				| otherwise
					# local			= {local & firstKeyTyped=True}
					# typist		= {typist & ls=local}
					# (_,typist)	= asyncSend rId BeginSession typist
					# typist		= appPIO (enableTimer tId) typist
					# (_,typist)	= asyncSend rId (KeyHit char) typist
					= typist
			where
				char				= case (fromJust (getKeyboardStateKey keyboard)) of
										IsCharKey    c -> c
										IsSpecialKey c -> '\b'
		
		wSize	= {	w	= (NrOfMaxCharsPerLine+2)*metrics.fMaxWidth
				  ,	h	= (NrOfLines+2)*(fontLineHeight metrics)
				  }
		
	//	menu defines the commands of the typist process. 
		menu	= Menu "File"
					(	MenuItem "Run"  [MenuShortKey 'r', MenuFunction (noLS run), MenuId runId]
					:+:	MenuSeparator	[]
					:+:	MenuItem "Quit" [MenuShortKey 'q', MenuFunction (noLS quit)]
					)
					[	MenuId	mId
					]
		where
	//		run starts a session by enabling the edit control that receives the keyboard input.
			run :: (PSt Typist) -> PSt Typist
			run typist=:{ls,io}
				# io	= setControlText editId "" io
				# io	= enableControl  editId io
				# io	= disableMenuElements [runId] io
				= {typist & ls={ls & firstKeyTyped=False},io=io}
	
	//	timer will end a typing session after 60 seconds. The timer is enabled by sendKeys.	
		timer	= Timer (60*ticksPerSecond) NilLS
					[	TimerId				tId
					,	TimerSelectState	Unable
					,	TimerFunction		(noLS1 endOfSession)
					]
		where
	//		The monitor process is notified of the end of the session by receiving the EndSession message.
			endOfSession :: NrOfIntervals (PSt Typist) -> PSt Typist
			endOfSession _ typist=:{io}
				# io		= disableTimer tId io
				# io		= disableControl editId io
				# io		= enableMenuElements [runId] io
				# (_,typist)= asyncSend rId EndSession {typist & io=io}
				= typist
	
//	quit closes boths processes. 
	quit :: (PSt Typist) -> PSt Typist
	quit typist
		# (_,typist)	= syncSend rId Quit typist
		# typist		= closeProcess typist
		= typist
