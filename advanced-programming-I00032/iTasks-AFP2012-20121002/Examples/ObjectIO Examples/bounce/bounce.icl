module bounce


//	**************************************************************************************************
//
//	A program that creates two interactive processes that bounce balls in an open-ended barrel.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO
import bounceDraw


::	Message									// The message type:
	=	BallsArrive [Ball]					// balls that have crossed process border
	|	BounceOpened						// the other bounce process has been created
	|	QuitBounce							// quit the bounce process
::	Local									// The local program state:
	=	{	talkTo	:: !RId Message			// who to play with
		,	barrel	:: !Barrel				// the shape of the barrel
		,	balls	:: ![Ball]				// the balls in the barrel
		}
::	*Bounce
	:==	PSt Local							// Synonym for PSt


//	Create the initial interactive process:

Start :: *World -> *World
Start world
	# (rIdA,world)	= openRId world
	# (rIdB,world)	= openRId world
	# (wIdA,world)	= openId  world
	# (wIdB,world)	= openId  world
	# (tIdA,world)	= openId  world
	# (tIdB,world)	= openId  world
	= startProcesses [	bounce wIdB tIdB rIdB rIdA "Bounce B" RightTop rightBarrelSetUp
					 ,	bounce wIdA tIdA rIdA rIdB "Bounce A" LeftTop  leftBarrelSetUp
					 ]	world

bounce :: Id Id (RId Message) (RId Message) Title ItemLoc (Barrel,[Ball]) -> Process
bounce wId tId me you name itemLoc (barrel,balls)
	= Process SDI initLocal initIO [ProcessClose quit]
where
	barrelDomain			= barrel.bDomain
	barrelSize				= rectangleSize barrelDomain
	maxSize					= maxFixedWindowSize
	windowSize				= {w=min barrelSize.w (maxSize.w/2),h=min barrelSize.h (maxSize.h/2)}
	splitWalls				= splitWallsInBarrel barrel
	(initLocal`,initLocal)	= shareLocal {talkTo=you,barrel=barrel,balls=balls}
	
	initIO :: Bounce -> Bounce
	initIO pst
		# (error,pst)	= openWindow undef window pst
		| error<>NoError
			= abort "bounce could not open window."
		# (error,pst)	= openMenu undef menu pst
		| error<>NoError
			= abort "bounce could not open menu."
		# (error,pst)	= openTimer undef timer pst
		| error<>NoError
			= abort "bounce could not open timer."
		# (error,pst)	= openReceiver undef receiver pst
		| error<>NoError
			= abort "bounce could not open receiver."
		| otherwise
			= pst
	where
	//	window defines the window that displays the barrel and the current balls.
		window	= Window name NilLS
						[	WindowId		wId
						,	WindowLook		False (updateBalls initLocal`)
						,	WindowViewSize	windowSize
						,	WindowPos		(itemLoc,NoOffset)
						]
		
	//	menu defines the bounce menu. It contains only the quit command to terminate the application.
		menu	= Menu name
					(	MenuItem "About Bounce..."	[MenuFunction (noLS bounceHelp)]
					:+:	MenuSeparator				[]
					:+:	MenuItem "Quit"				[MenuFunction (noLS quit),MenuShortKey 'q']
					)	[]
		
	//	timer defines the timer that calculates the movements of the current balls.
		timer	= Timer 0 NilLS 
					[	TimerId			tId
					,	TimerFunction	(noLS1 (bounceBalls splitWalls))
					]
		where
			bounceBalls :: !(![SingleWall],![SingleWall]) NrOfIntervals Bounce -> Bounce
			bounceBalls splitWalls _ bounce=:{ls=local=:{talkTo,balls,barrel},io}
				# (windowSize,io)	= getWindowViewSize wId io
				  scale				= scaleSize windowSize barrelSize
				  eraseOld			= map (eraseBall scale base) balls
				  drawNew			= map (drawBall  scale base) ins
				  local				= {local & balls=ins}
				# (local`,local)	= shareLocal local
				# io				= appWindowPicture wId (seq (eraseOld++drawNew)) io
				# io				= setWindowLook wId False (False,updateBalls local`) io
				# bounce			= {bounce & ls=local,io=io}
				| isEmpty outs		= bounce
				| otherwise			= snd (syncSend talkTo (BallsArrive outs) bounce)
			where
				nextBallPos			= nextBallPositions splitWalls balls
				ballsMoved			= map moveBall nextBallPos
				domain				= barrel.bDomain
				base				= domain.corner1
				barrelSize			= rectangleSize domain
				(ins,outs)			= splitBallsInBarrel domain ballsMoved
		
	//	receiver defines the receiver that will receive new balls and termination requests.
		receiver :: *Receiver Message .ls Bounce
		receiver = Receiver me (noLS1 (receive splitWalls)) []
		where
			receive :: !(![SingleWall],![SingleWall]) !Message !Bounce -> Bounce
			receive (horizontal,vertical) (BallsArrive newBalls) bounce=:{ls}
				#!	newBalls = map correctBall newBalls
				=	{bounce & ls={ls & balls=newBalls++ls.balls}}
			where
				correctBall :: !Ball -> Ball
				correctBall ball=:{bCenter,bSpeed}
					# ball = {ball & bCenter=movePoint (~bSpeed) bCenter}
					# ball = checkVerticalWalls   vertical   ball
					# ball = checkHorizontalWalls horizontal ball
					# ball = moveBall ball
					= ball
			receive _ BounceOpened bounce
				= appPIO (enableTimer tId) bounce
			receive _ QuitBounce bounce
				= closeProcess bounce
	
	updateBalls :: Local SelectState UpdateState *Picture -> *Picture
	updateBalls {balls,barrel} _ {oldFrame,newFrame,updArea} picture
		# picture	= drawBarrel area scale barrel picture
		# picture	= seq (map (drawBall scale domain.corner1) balls) picture
		= picture
	where
		domain		= barrel.bDomain
		windowSize	= rectangleSize newFrame
		barrelSize	= rectangleSize domain
		scale		= scaleSize windowSize barrelSize
		area		= if (oldFrame==newFrame) updArea [newFrame]
	
	quit :: Bounce -> Bounce
	quit bounce=:{ls={talkTo}}
		= closeProcess (snd (syncSend talkTo QuitBounce bounce))
	
//	bounceHelp opens a dialog that tells something about this application.
	bounceHelp :: Bounce -> Bounce
	bounceHelp bounce
		# (okId,bounce)		= accPIO openId bounce
		# (dId, bounce)		= accPIO openId bounce
		# dDef				= Dialog "About bounce"
								(	TextControl   "This is a Clean program"
														[ControlPos (Center,NoOffset)]
								:+:	ButtonControl "Ok"	[ControlId okId
														,ControlPos (Center,NoOffset)
														,ControlFunction (noLS (closeWindow dId))
														]
								)
								[	WindowOk okId
								,	WindowId dId
								]
		# ((error,_),bounce)= openModalDialog undef dDef bounce
		| error<>NoError	= abort "bounce could not open About bounce dialog."
		| otherwise			= bounce


//	Determine which balls are inside and which are outside the barrel:

splitBallsInBarrel :: !ViewDomain ![Ball] -> (![Ball],![Ball])
splitBallsInBarrel domain balls
	= seq (map (ballInOrOut domain) balls) ([],[])
where
	ballInOrOut :: !ViewDomain !Ball !(![Ball],![Ball]) -> (![Ball],![Ball])
	ballInOrOut {corner1={x=left,y=top},corner2={x=right,y=bottom}} ball=:{bCenter} (ins,outs)
		| between bCenter.x left right && between bCenter.y top bottom
			= ([ball:ins],outs)
		| otherwise
			= (ins,[ball:outs])

nextBallPositions :: !(![SingleWall],![SingleWall]) ![Ball] -> [Ball]
nextBallPositions (horizontal,vertical) balls
	= map (checkHorizontalWalls horizontal) (
	  map (checkVerticalWalls   vertical)   (
	  computeNextBallPositions [] balls))
where
	computeNextBallPositions :: ![Ball] ![Ball] -> [Ball]
	computeNextBallPositions ballsDone [ball:balls]
		= computeNextBallPositions [ballDone:newBallsDone] newBalls
	where
		(newBallsDone,newBalls,ballDone) = checkBallCollisions ballsDone balls ball
		
		checkBallCollisions :: ![Ball] ![Ball] !Ball -> (![Ball],![Ball],!Ball)
		checkBallCollisions balls1 balls2 ball
			= (newBalls1,newBalls2,ball2)
		where
			(newBalls1,ball1)	= checkBallCollision balls1 ball
			(newBalls2,ball2)	= checkBallCollision balls2 ball1
			
			checkBallCollision :: ![Ball] !Ball -> (![Ball],!Ball)
			checkBallCollision [ball2=:{bCenter=center2,bRadius=radius2,bSpeed=step2}:others]
								ball1=:{bCenter=center1,bRadius=radius1,bSpeed=step1}
				| dist (moveBall ball1).bCenter center2<=toReal (radius1+radius2)
					# (others,ball1)	= checkBallCollision others {ball1 & bSpeed=step2}
					= ([{ball2 & bSpeed=step1}:others],ball1)
				| otherwise
					# (others,ball1)	= checkBallCollision others ball1
					= ([ball2:others],ball1)
			checkBallCollision others ball
				= (others,ball)
	computeNextBallPositions ballsDone _
		= ballsDone

checkHorizontalWalls :: ![SingleWall] !Ball -> Ball
checkHorizontalWalls [((a,b),interior):walls] ball=:{bCenter,bRadius,bSpeed}
	| interior<>startInterior	= checkHorizontalWalls walls ball
	| not collision				= checkHorizontalWalls walls ball1
	| otherwise					= ball1
where
	c							= (moveBall ball).bCenter
	speed1						= if collision {bSpeed & vy=0-bSpeed.vy} bSpeed
	collision					= (between c.x (a.x-bRadius) (b.x+bRadius))
								  &&	(sign bSpeed.vy<>interior)
								  		&&	(if posSign (a.y+signRadius>=c.y)
								  						(a.y+signRadius<=c.y))
	signRadius					= interior*bRadius
	posSign						= interior>0
	startInterior				= sign (bCenter.y-a.y)
	ball1						= {ball & bSpeed=speed1}
checkHorizontalWalls _ ball
	= ball

checkVerticalWalls :: ![SingleWall] !Ball -> Ball
checkVerticalWalls [((a,b),interior):walls] ball=:{bCenter,bRadius,bSpeed}
	| interior<>startInterior	= checkVerticalWalls walls ball
	| not collision				= checkVerticalWalls walls ball1
	| otherwise					= ball1
where
	c							= (moveBall ball).bCenter
	speed1						= if collision {bSpeed & vx=0-bSpeed.vx} bSpeed
	collision					= (between c.y (a.y-bRadius) (b.y+bRadius))
								  &&	((sign bSpeed.vx<>interior)
										&&	(if posSign (a.x+signRadius>=c.x)
														(a.x+signRadius<=c.x)))
	signRadius					= interior*bRadius
	posSign						= interior>0
	startInterior				= sign (bCenter.x-a.x)
	ball1						= {ball & bSpeed=speed1}
checkVerticalWalls _ ball		= ball

moveBall :: !Ball -> Ball
moveBall ball=:{bCenter,bSpeed}
	= {ball & bCenter=movePoint bSpeed bCenter}

shareLocal :: !Local -> (!Local,!*Local)
shareLocal local=:{talkTo,barrel,balls}
	= (local, {talkTo=talkTo,barrel=barrel,balls=balls})
