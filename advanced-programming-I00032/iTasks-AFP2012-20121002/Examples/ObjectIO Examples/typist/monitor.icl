implementation module monitor


//	**************************************************************************************************
//
//	This module defines a process creation function that tracks the number of typed keys in a typing
//	session. 
//	This module has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO


::	Count
	=	{	oks			:: Int		// nr of OK keys
		,	bads		:: Int		// nr of correction keys
		}
::	Monitor
	=	{	count		:: Count	// current  registration of keys
		,	counts		:: [Count]	// previous registration of keys
		,	time		:: Int		// current tracking time since start (in seconds)
		,	tracking	:: Bool		// monitor is currently tracking
		}
::	MonitorMessage					// The message type of the monitor process
	=	BeginSession				// Begin a typist session
	|	KeyHit Char					// Register key stroke
	|	EndSession					// End a typist session
	|	Quit						// Close the monitor process

openMonitor :: ItemPos (RId MonitorMessage) (PSt .l) -> PSt .l
openMonitor pos monitorId pst
	# (font,   pst) = accPIO (accScreenPicture openDialogFont) pst
	# (metrics,pst) = accPIO (accScreenPicture (getFontMetrics font)) pst
	# (ids,    pst) = accPIO (openIds 2) pst
	= openProcesses (monitorProcess pos font metrics monitorId ids) pst

monitorProcess :: ItemPos Font FontMetrics (RId MonitorMessage) [Id] -> Process
monitorProcess pos font metrics monitorId ids=:[wId,tId]
	= Process SDI initLocal initIO []
where
	initcount	= {oks=0,bads=0}
	initcounts	= []
	initLocal	= {count=initcount,counts=initcounts,time=0,tracking=False}
	
//	initIO initialises the monitor process.
	initIO pst
		# (error,pst)		= openWindow undef window pst
		| error<>NoError	= abort "monitor could not open window."
		# (error,pst)		= openTimer undef timer pst
		| error<>NoError	= abort "monitor could not open timer."
		# (error,pst)		= openReceiver undef receiver pst
		| error<>NoError	= abort "monitor could not open receiver."
		| otherwise			= pst
	where
	//	window is the single document of the monitor process.
		window	= Window "Monitor" NilLS
					[	WindowId			wId
					,	WindowPos			pos
					,	WindowViewDomain	pDomain
					,	WindowLook			True (monitorlook initLocal)
					,	WindowInit			(noLS (appPIO (appWindowPicture wId (setPenFont font))))
					]
		
		pDomain	= {corner1=zero,corner2={x=WindowWidth,y=WindowHeight}}
	
	//	The timer gathers per second the number of good and bad key hits.
	//	The monitor window is updated by drawing only the new diagram bars.
		timer	= Timer ticksPerSecond NilLS
					[	TimerId				tId
					,	TimerSelectState	Unable
					,	TimerFunction		(noLS1 (showKeyHits False))
					]
		
	//	The receiver is the interface of the monitor process to the typist process.
		receiver :: *Receiver MonitorMessage .ls (PSt Monitor)
		receiver= Receiver monitorId (noLS1 receive) []
	
//	monitorlook defines the look of the monitor window. 
	monitorlook :: Monitor SelectState UpdateState *Picture -> *Picture
	monitorlook {counts,tracking} _ _ picture
		# picture	= drawBackground  picture
		# picture	= drawSecondsLine picture
		# picture	= drawKeyHitsLine picture
		# picture	= seq (snd (smap drawKeyHitColumn (0,counts))) picture
		| tracking	= picture
		| otherwise	= drawTotalAndAverage font metrics counts picture
	
	showKeyHits :: Bool NrOfIntervals (PSt Monitor) -> PSt Monitor
	showKeyHits final dt monitor=:{ls=local=:{count,counts,time},io}
		# io		= appWindowPicture wId (seq drawfs) io
		# io		= setWindowLook wId final (True,monitorlook newlocal) io
		= {monitor & ls=newlocal,io=io}
	where
		missedcounts= if (dt>1) (repeatn (dt-1) {oks=0,bads=0}) []
		newcounts	= [count:missedcounts]
		newlocal	= {local & count=initcount,counts=counts++newcounts,time=time+dt}
		drawfs		= snd (smap drawKeyHitColumn (time,newcounts))
	
	receive :: MonitorMessage (PSt Monitor) -> PSt Monitor

//	Starting a tracking session enables the timer and clears all previous tracking information.
	receive BeginSession monitor
		# local	= {initLocal & tracking=True}
		# io	= enableTimer  tId monitor.io
		# io	= setWindowLook wId True (True,monitorlook local) io
		= {monitor & ls=local,io=io}

//	For each key hit, only administrate whether it is a good or bad key hit.
	receive (KeyHit char) monitor
		= appPLoc (incCount char) monitor
	where
		incCount :: Char Monitor -> Monitor
		incCount c local=:{count}
			| c=='\b'	= {local & count={count & bads=count.bads + 1}}
			| otherwise	= {local & count={count & oks =count.oks  + 1}}

//	Ending a session disables the timer and presents the number and average of key hits. 
	receive EndSession monitor=:{ls=local=:{time}}
		# monitor	= {monitor & ls={local & tracking=False}}
		# monitor	= showKeyHits True (60-time) monitor
		# monitor	= appPIO (disableTimer tId) monitor
		= monitor

//	Quit closes the monitor process.
	receive Quit monitor
		= closeProcess monitor


//	The drawing functions:

drawBackground :: *Picture -> *Picture
drawBackground picture
	= unfill {corner1=zero,corner2={x=WindowWidth,y=WindowHeight}} picture

drawSecondsLine :: *Picture -> *Picture
drawSecondsLine picture
	# picture	= setPenPos {x=GraphX,y=GraphY} picture
	# picture	= draw {vx=GraphWidth,vy=0} picture
	= seq (map drawSecond [0,10..MaxNrOfSeconds]) picture
where
	drawSecond :: Int *Picture -> *Picture
	drawSecond i picture
		# picture	= drawAt {x=x,y=GraphY} {vx=0,vy=AxisMarkSize}		picture
		# picture	= drawAt {x=x,y=GraphY+SecondsOffset} (toString i)	picture
		= picture
	where
		x	= GraphX+i*SecondsWidth

drawKeyHitsLine :: *Picture -> *Picture
drawKeyHitsLine picture
	# picture	= setPenPos {x=GraphX,y=GraphY} picture
	# picture	= draw {vx=0,vy=0-GraphHeight}  picture
	= seq (map drawKeyHit [0,2..MaxNrKeyHits]) picture
where
	drawKeyHit :: Int *Picture -> *Picture
	drawKeyHit i picture
		# picture	= drawAt {x=GraphX-AxisMarkSize,y=y} {vx=AxisMarkSize,vy=0}	picture
		# picture	= drawAt {x=x,y=y} (toString i)								picture
		= picture
	where
		x	= GraphX-KeyHitsOffset
		y	= GraphY-i*KeyHitHeight

drawTotalAndAverage :: Font FontMetrics [Count] *Picture -> *Picture
drawTotalAndAverage font metrics counts picture
	| isEmpty counts	= picture
	| otherwise			= totalAndAverage font metrics counts picture
where
	totalAndAverage :: Font FontMetrics [Count] *Picture -> *Picture
	totalAndAverage font metrics=:{fMaxWidth} counts picture
		# (totalW,  picture)	= getFontStringWidth font totalT				picture
		# (averageW,picture)	= getFontStringWidth font averageT				picture
		# (sumW,    picture)	= getFontStringWidth font sumT					picture
		# picture				= drawAverage	height							picture
		# picture				= setPenPos		{x=GraphX,y=summaryY}			picture
		# picture				= draw			totalT							picture
		# picture				= movePenPos	{vx=0-totalW,vy=lineHeight}		picture
		# picture				= draw			averageT						picture
		# picture				= setPenPos		{x=GraphX+(max totalW averageW)+fMaxWidth,y=summaryY}
																				picture
		# picture				= draw			sumT							picture
		# picture				= movePenPos	{vx=0-sumW,vy=lineHeight}		picture
		# picture				= draw			(toString (round 1 average))	picture
		= picture
	where
		lineHeight				= fontLineHeight metrics
		summaryY				= WindowHeight-SummaryMargin
		seconds					= length counts
		total					= foldr (+) 0 (map (\{oks,bads}->oks-bads) counts)
		average					= toReal total/toReal seconds
		height					= toInt (average*toReal KeyHitHeight)
		sumT					= toString total
		averageT				= "Average:"
		totalT					= "Total:"
		
		drawAverage :: Int *Picture -> *Picture
		drawAverage height picture
			| height<=0	= picture
			| otherwise	= appXorPicture (drawLine {x=GraphX,y=y} {x=GraphX+GraphWidth,y=y}) picture
		where
			y			= GraphY-height

drawKeyHitColumn :: (Int,Count) -> (Int,IdFun *Picture)
drawKeyHitColumn (i,count)
	= (i+1,drawColumn i count)
where
	drawColumn :: Int Count *Picture -> *Picture
	drawColumn i {oks,bads} picture
		# picture	= fill			 {corner1={x=leftX,y=yOk-1},corner2={x=rightX,y=GraphY}} picture
		# picture	= setPenColour	 Red													 picture
		# picture	= fill			 {corner1={x=leftX,y=yBad}, corner2={x=rightX,y=GraphY}} picture
		# picture	= drawSeparation {x=leftX,y=yBad} {x=rightX,y=yBad} bads				 picture
		# picture	= setPenColour	 Black													 picture
		= picture
	where
		yBad		= GraphY-bads*KeyHitHeight
		yOk			= GraphY-hits*KeyHitHeight
		leftX		= GraphX+i *  SecondsWidth
		rightX		= leftX +     SecondsWidth
		hits		= oks+bads
		
		drawSeparation :: Point2 Point2 Int *Picture -> *Picture
		drawSeparation a b badHits picture
			| badHits==0	= picture
			| otherwise		= drawLine a b (setPenColour White picture)


//	Application constants:

KeyHitHeight	:== 10;		KeyHitsOffset :== 20
MaxNrKeyHits	:== 10
SecondsWidth	:== 4;		SecondsOffset :== 15
MaxNrOfSeconds	:== 60
	
SummaryMargin	:== 20
	
TopMargin		:== 20
LeftMargin		:== 60		// note: LeftMargin  >KeyHitsOffset
RightMargin		:== 30
BottomMargin	:== 60		// note: BottomMargin>SecondsOffset+SummaryMargin
	
GraphWidth		:== MaxNrOfSeconds*SecondsWidth
GraphHeight		:== MaxNrKeyHits  *KeyHitHeight
GraphX			:== LeftMargin
GraphY			:== TopMargin +GraphHeight
AxisMarkSize	:== 3
WindowWidth		:== LeftMargin+RightMargin +GraphWidth
WindowHeight	:== TopMargin +BottomMargin+GraphHeight

		
//	General functions:

round :: !Int !Real -> Real
round decimals r
	| decimals<=0	= toReal (toInt r)
	# p				= toReal (10^decimals)
	| otherwise		= toReal (toInt (r*p))/p

smap :: ((.s,.x) -> (.s,.y)) !(!.s,![.x]) -> (!.s,![.y])
smap f (s,[x:xs])
	# (s,y ) = f (s,x)
	# (s,ys) = smap f (s,xs)
	= (s,[y:ys])
smap _ (s,_)
	= (s,[])
