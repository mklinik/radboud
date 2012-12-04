module Hanoi

//	**************************************************************************************************
//
//	This program shows the Towers of Hanoi algorithm graphically.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdEnv, StdIO

::	Tower	:==	[Int]
::	Moves	:== [Int]
::	Towers
	=	{	moves	:: !Moves
		,	tower1	:: !Tower
		,	tower2	:: !Tower
		,	tower3	:: !Tower
		}
::	TowerPos
	=	{	pos		:: Int
		,	tower	:: Tower
		}

ViewDomain	:== {corner1={x=50,y=0},corner2={x=480,y=180}}
Speed1		:== ticksPerSecond / 2
Speed2		:== ticksPerSecond / 3
Speed3		:== ticksPerSecond / 6
Speed4		:== ticksPerSecond / 12
Speed5		:== 0

MinDisks	:== 2
MaxDisks	:== 10
XOffs		:==	inc MaxDisks * 10

//	Starting the program

Start :: *World -> *World
Start world
	= startHanoi (openIds 5 world)

startHanoi :: ([Id],*World) -> *World
startHanoi ([runID,haltID,contID,timerID,windowID:_],world)
	= startIO SDI
			(initTowers 0)						// The initial local  process state
			initialise							// The initialisation action
			[ProcessClose closeProcess]			// Only default process attributes
			world
where
	initialise :: (PSt Towers) -> PSt Towers
	initialise pst
		# (error,pst)	= openWindow undef window pst
		| error<>NoError
			= abort "Hanoi could not open window."
		# (error,pst)	= openMenu undef menu pst
		| error<>NoError
			= abort "Hanoi could not open menu."
		# (error,pst)	= openTimer undef timer pst
		| error<>NoError
			= abort "Hanoi could not open timer."
		| otherwise
			= pst
	where
		menu	= Menu "&Hanoi"
					(	SubMenu "&Run (nr disks)"
						(	RadioMenu
						[	("&"+++toString i,Nothing,Nothing,noLS (run i)) \\ i<-[MinDisks..MaxDisks]	]
						1	[]
						)
						[	MenuId	runID	]
					:+:	MenuItem "Halt"		[	MenuId			haltID
											,	MenuShortKey	'.'
											,	MenuSelectState	Unable
											,	MenuFunction	(noLS halt)
											]
					:+:	MenuItem "Continue"	[	MenuId			contID
											,	MenuShortKey	','
											,	MenuSelectState	Unable
											,	MenuFunction	(noLS continue)
											]
					:+:	SubMenu "&Speed" 
						(	RadioMenu
						[	("V&ery Slow", Nothing, Just 'A', noLS (setSpeed Speed1))
						,	("&Slow"     , Nothing, Just 'S', noLS (setSpeed Speed2))
						,	("&Normal"   , Nothing, Just 'D', noLS (setSpeed Speed3))
						,	("&Fast"     , Nothing, Just 'F', noLS (setSpeed Speed4))
						,	("&Very Fast", Nothing, Just 'G', noLS (setSpeed Speed5))
						] 3 []
						)	[]
					:+:	MenuSeparator	[]
					:+:	MenuItem "&Quit" [MenuShortKey 'q', MenuFunction (noLS closeProcess)]
					)	[]
		
		window	= Window "Hanoi" NilLS
					[	WindowId			windowID
					,	WindowViewDomain	ViewDomain
					,	WindowLook			True (look (initTowers 0))
					]
		
		timer	= Timer Speed3 NilLS
					[	TimerId				timerID
					,	TimerSelectState	Unable
					,	TimerFunction		(noLS1 stepHanoi)
					]
		
	//	The function for the Run command.
		run :: Int (PSt Towers) -> PSt Towers
		run nr_disks hanoi=:{io}
			# io	= disableMenuElements [runID,contID] io
			# io	= enableMenuElements [haltID] io
			# io	= enableTimer timerID io
			# io	= setWindowLook windowID True (True,look towers) io
			= {hanoi & ls=towers,io=io}
		where
			towers	= initTowers nr_disks
		
	//	The function for the Halt command.
		halt :: (PSt Towers) -> PSt Towers
		halt hanoi=:{io}
			# io	= enableMenuElements [runID,contID] io
			# io	= disableMenuElements [haltID] io
			# io	= disableTimer timerID io
			= {hanoi & io=io}
		
	//	The function for the Continue command.
		continue :: (PSt Towers) -> PSt Towers
		continue hanoi=:{io}
			# io	= disableMenuElements [runID,contID] io
			# io	= enableMenuElements [haltID] io
			# io	= enableTimer timerID io
			= {hanoi & io=io}
		
	//	Set the speed of a (possibly running) Hanoi simulation.
		setSpeed :: Int (PSt Towers) -> PSt Towers
		setSpeed speed hanoi=:{io}
			# io	= setTimerInterval timerID speed io
			= {hanoi & io=io}
		
	//	The timer function: take a move from the list of all moves and show it in the window.
		stepHanoi :: NrOfIntervals (PSt Towers) -> PSt Towers
		stepHanoi _ hanoi=:{ls={moves=[]},io}
			# io				= enableMenuElements  [runID]  io
			# io				= disableMenuElements [haltID] io
			# io				= disableTimer timerID io
			= {hanoi & io=io}
		stepHanoi _ hanoi=:{ls,io}
			# (drawf,towers)	= changeTowers ls
			# io				= appWindowPicture windowID drawf io
			# io				= setWindowLook windowID False (True,look towers) io
			= {hanoi & ls=towers,io=io}
		where
			changeTowers :: Towers -> (IdFun *Picture,Towers)
			changeTowers towers=:{moves=[1,2:moves],tower1=[f1:r1],tower2=t2}
				= (drawMove 1 2 f1 (length r1) (length t2),{towers & moves=moves,tower1=r1,tower2=[f1:t2]})
			changeTowers towers=:{moves=[1,3:moves],tower1=[f1:r1],tower3=t3}
				= (drawMove 1 3 f1 (length r1) (length t3),{towers & moves=moves,tower1=r1,tower3=[f1:t3]})
			changeTowers towers=:{moves=[2,1:moves],tower2=[f2:r2],tower1=t1}
				= (drawMove 2 1 f2 (length r2) (length t1),{towers & moves=moves,tower2=r2,tower1=[f2:t1]})
			changeTowers towers=:{moves=[2,3:moves],tower2=[f2:r2],tower3=t3}
				= (drawMove 2 3 f2 (length r2) (length t3),{towers & moves=moves,tower2=r2,tower3=[f2:t3]})
			changeTowers towers=:{moves=[3,1:moves],tower3=[f3:r3],tower1=t1}
				= (drawMove 3 1 f3 (length r3) (length t1),{towers & moves=moves,tower3=r3,tower1=[f3:t1]})
			changeTowers towers=:{moves=[3,2:moves],tower3=[f3:r3],tower2=t2}
				= (drawMove 3 2 f3 (length r3) (length t2),{towers & moves=moves,tower3=r3,tower2=[f3:t2]})
			
			drawMove :: Int Int Int Int Int *Picture -> *Picture
			drawMove start end disk lenfr lento picture
				# picture	= eraseDisk	{corner1={x=fx-w,y=fy},corner2={x=fx+w,y=fy+10}} picture
				# picture	= drawDisk	{corner1={x=tx-w,y=ty},corner2={x=tx+w,y=ty+10}} picture
				= picture
			where
				tx	= end  *XOffs;		ty	= 10+10*(MaxDisks-lento) 
				fx	= start*XOffs;		fy	= 10+10*(MaxDisks-lenfr) 
				w	= disk *5


//	The initial Towers value, given the number of disks

initTowers :: Int -> Towers
initTowers nr_disks
	= {	moves	= hanoi nr_disks 1 2 3
	  ,	tower1	= [1..nr_disks]
	  ,	tower2	= []
	  ,	tower3	= []
	  }
where
	hanoi :: Int Int Int Int -> Moves		// The function that calculates the list of disk moves
	hanoi n start end via
		| n==0		= []
		| otherwise	= hanoi m start via end ++ [start,end : hanoi m via end start]
	where
		m			= n-1

//	The update function: erase the window and redraw the towers

look :: Towers SelectState UpdateState *Picture -> *Picture
look {tower1,tower2,tower3} _ {newFrame} picture
	# picture	= unfill newFrame			picture
	# picture	= draw {pos=1,tower=tower1}	picture
	# picture	= draw {pos=2,tower=tower2}	picture
	# picture	= draw {pos=3,tower=tower3}	picture
	= picture

instance Drawables TowerPos where
	draw :: !TowerPos !*Picture -> *Picture
	draw {pos,tower} picture
		= drawName pos (drawTower pos (MaxDisks-length tower) tower picture)
	where
		drawTower :: !Int !Int !Tower !*Picture -> *Picture
		drawTower nr i [f:r] picture
			= drawTower nr (i+1) r (drawDisk {corner1={x=x-w,y=y},corner2={x=x+w,y=y+10}} picture)
		where
			x	= nr*XOffs
			w	= f *5
			y	= 20+10*i
		drawTower _ _ _ picture
			= picture
		
		drawName :: !Int !*Picture -> *Picture
		drawName nr picture
			| nr==1	= draw "from" (setPenPos {x=  XOffs-14,y=y} picture)
			| nr==2	= draw "to"   (setPenPos {x=2*XOffs-6, y=y} picture)
			| nr==3	= draw "via"  (setPenPos {x=3*XOffs-9, y=y} picture)
		where
			y		= 35+10*MaxDisks
	
	drawAt :: !Point2 !TowerPos !*Picture -> *Picture
	drawAt _ tower picture
		= draw tower picture
	
	undraw :: !TowerPos !*Picture -> *Picture
	undraw {pos,tower} picture
		= unfill {corner1={x=x-w/2,y=0},corner2={x=x+w/2,y=35+10*MaxDisks}} picture
	where
		x	= pos*XOffs
		w	= 10*MaxDisks
	
	undrawAt :: !Point2 !TowerPos !*Picture -> *Picture
	undrawAt _ tower picture
		= undraw tower picture

eraseDisk :: Rectangle *Picture -> *Picture
eraseDisk rectangle picture
	= unfill rectangle picture

drawDisk :: Rectangle *Picture -> *Picture
drawDisk rectangle picture
	# picture	= setPenColour	(RGB {r=MaxRGB/2,g=MaxRGB*3/5,b=MaxRGB*7/10}) picture
	# picture	= fill			rectangle	picture
	# picture	= setPenColour	Black		picture
	# picture	= draw			rectangle	picture
	= picture
