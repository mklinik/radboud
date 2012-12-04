definition module wormstate

import	StdInt
import	StdIOCommon, StdSystem
import	Highscore, Random

::	State
	=	{	gamelevel	:: Level			// The current level of the game
		,	food		:: Food				// The current piece of food
		,	foodsupply	:: [Food]			// The food supply (infinite list of random food)
		,	grow		:: Grow				// The amount of growth of the worm
		,	points		:: Points			// The current score of the player
		,	dir			:: SpecialKey		// The current direction of the worm
		,	worm		:: Worm				// The worm itself
		,	best		:: HiScores			// The HighScore information
		,	lives		:: Lives			// The current nr of remaining lives
		}
::	Level
	=	{	fix			:: Int				// The initial speed (TimerInterval) of the game
		,	speed		:: Int				// The current speed (TimerInterval) of the game ([fix,fix-0.05sec..0])
		,	level		:: Int				// The current level ([0..])
		,	obstacles	:: [Obstacle]		// The obstacles of this level
		}
::	Food
	=	{	value		:: Int				// The growing power of this food
		,	pos			:: Point2			// The location of this food
		}
::	Grow				:== Int
::	Obstacle			:== Rectangle
::	Segment				:== Point2
::	Worm				:== [Segment]
::	Points				:== Int
::	Lives				:== Int

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

InitState		:: HiScores				-> State

NewWorm			:: Level				-> Worm
InitLevel		:: Int					-> Level
DecreaseLevel	:: Level				-> Level
IncreaseLevel	:: Level				-> Level

FoodSupply		:: RandomSeed			-> [Food]
NewFood			:: Worm Level [Food]	-> (Food, [Food])
