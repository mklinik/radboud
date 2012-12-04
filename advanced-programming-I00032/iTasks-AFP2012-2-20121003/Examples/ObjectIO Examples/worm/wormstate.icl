implementation module wormstate

import	StdBool, StdEnum, StdList
import	StdIOCommon, StdSystem
import	Highscore, Random

//	The worm data types.

::	State
	=	{	gamelevel	:: Level
		,	food		:: Food
		,	foodsupply	:: [Food]
		,	grow		:: Grow
		,	points		:: Points
		,	dir			:: SpecialKey
		,	worm		:: Worm
		,	best		:: HiScores
		,	lives		:: Lives
		}
::	Level
	=	{	fix			:: Int
		,	speed		:: Int
		,	level		:: Int
		,	obstacles	:: [Obstacle]
		}
::	Food
	=	{	value		:: Int
		,	pos			:: Point2
		}
::	Grow		:== Int
::	Obstacle	:== Rectangle
::	Segment		:== Point2
::	Worm		:== [Segment]
::	Points		:== Int
::	Lives		:== Int

SizeX			:== 45
SizeY			:== 26

NrOfWorms		:== 4
NrOfLevels		:== 8
PointsPerLevel	:== 500
StartLevel		:== 0

EasySpeed		:== ticksPerSecond/6
MediumSpeed		:== ticksPerSecond/9
HardSpeed		:== ticksPerSecond/18
Accelation		:==	ticksPerSecond/18


//	Initial State.
InitState :: HiScores -> State
InitState best
	= {	gamelevel	= initlevel
	  ,	food		= initfood
	  ,	foodsupply	= []
	  ,	grow		= 0
	  ,	points		= 0
	  ,	dir			= rightKey
	  ,	worm		= initworm
	  ,	best		= best
	  ,	lives		= NrOfWorms
	  }
where
	initfood		= {value=0,pos={x=(-10),y=(-10)}}
	initlevel		= InitLevel EasySpeed
	initworm		= NewWorm initlevel


//	Make a new initial worm.
NewWorm :: Level -> Worm
NewWorm {Level | level}
	= [{x=x,y=y}\\x<-[5,4..1]]
where
	y	= StartHeights!!(level rem NrOfLevels)

StartHeights :: [Int]
StartHeights =: [13,5,13,13,13,1,1,14]


//	Construct the next level.
InitLevel :: Int -> Level
InitLevel fix
	= {fix=fix,speed=fix,level=StartLevel,obstacles=Obstacles!!StartLevel}

DecreaseLevel :: Level -> Level
DecreaseLevel curlevel=:{speed,level}
	# level	= level-1
	  speed	= if (level rem NrOfLevels==0 && level<>0) (speed+Accelation) speed
	= {	curlevel &	fix			= speed
				 ,	speed		= speed
				 ,	level		= level
				 ,	obstacles	= Obstacles!!(level rem NrOfLevels)
	  }

IncreaseLevel :: Level -> Level
IncreaseLevel curlevel=:{speed,level}
	# speed	= if (level rem NrOfLevels==0 && level<>0) (speed-Accelation) speed
	  level	= level+1
	= {	curlevel &	fix			= speed
				 ,	speed		= speed
				 ,	level		= level
				 ,	obstacles	= Obstacles!!(level rem NrOfLevels)
	  }

Obstacles :: [[Obstacle]]
Obstacles =: [	[]
			 ,	[{corner1={x=12,y=11},corner2={x=34,y=16}}]
			 ,	[{corner1={x=12,y= 1},corner2={x=34,y= 3}}, {corner1={x=12,y=24},corner2={x=34,y=26}}]
			 ,	[{corner1={x= 7,y= 7},corner2={x=38,y= 9}}, {corner1={x= 7,y=17},corner2={x=38,y=19}}]
			 ,	[{corner1={x= 1,y= 1},corner2={x=18,y=10}}, {corner1={x=28,y=17},corner2={x=45,y=26}}]
			 ,	[{corner1={x=14,y= 3},corner2={x=15,y=24}}, {corner1={x=30,y= 3},corner2={x=31,y=24}}]
			 ,	[{corner1={x= 3,y=13},corner2={x=43,y=14}}, {corner1={x=22,y= 3},corner2={x=24,y=24}}]
			 ,	[{corner1={x= 3,y= 3},corner2={x=20,y=12}}, {corner1={x=26,y=15},corner2={x=43,y=24}}]
			 ]



//	Generate a food supply.
FoodSupply :: RandomSeed -> [Food]
FoodSupply seed
	= [{value=value,pos=pos}:FoodSupply seed3]
where
	(random1,seed1)	= random seed
	(random2,seed2)	= random seed1
	(random3,seed3)	= random seed2
	foodx			= (IncMod random2 (SizeX-2))+1
	foody			= (IncMod random3 (SizeY-2))+1
	pos				= {x=foodx,y=foody}
	value			= IncMod random1 9
	
	IncMod a b		= (a rem b)+1

//	Think of some new random food.
NewFood :: Worm Level [Food] -> (Food, [Food])
NewFood worm level=:{obstacles} [food=:{pos}:foods]
	| isMember pos worm || any (InRectangle pos) obstacles	= NewFood worm level foods
	| otherwise												= (food, foods)
where
	InRectangle :: Point2 Obstacle -> Bool
	InRectangle {x,y} {corner1={x=lx,y=ty},corner2={x=rx,y=by}}
		= x>=lx && x<=rx && y>=ty && y<=by
