implementation module state


import	StdBool, StdList
import	StdPStClass
import	graphics, board, language
import	Random


::	State
	=	{	playmode		:: Playmode				//	The playing mode
		,	board			:: Board				//	The playing board
		,	player			:: Player				//	Who's move is it (Player1 or Player2)
		,	player1			:: Playerinfo			//	The information of player 1
		,	player2			:: Playerinfo			//	The information of player 2
		,	direction		:: Direction			//	Last direction chosen by a Person
		,	letterbox		:: [Char]				//	The current set of available letters
		,	lexicon			:: Tree					//	The current list of valid words
		,	wordsadded		:: Bool					//	Words have been added by the players
		,	dimensions		:: (Int,Int,Int,Int)	//	The smallest enclosing rectangle surrounding the layn words
		,	strength		:: Strength				//	Strength of the Computer player
		,	progress		:: Progress				//	The progress of a Computer player
		,	random			:: [Int]				//	An infinite random Integer list
		,	boardinput		:: Point2				//	op dit moment de lokale toestand van board; moet eigenlijk in State
		}
::	Playerinfo
	=	{	kind			:: Playerkind			//	The kind of the player
		,	letters			:: [Char]				//	Letters of player
		,	points			:: Int					//	Score of player
		,	placedword		:: Bool					//	Player placed a word in last turn (initially True)
		}


/***************************************************************************************************************
	The value initdimensions will cause the computer player to place its first word at the center position of 
	the board (7,7).
****************************************************************************************************************/
initdimensions :: (Int,Int,Int,Int)
initdimensions = (8,6,8,6)


/***************************************************************************************************************
	The initstate fixes the starting players. 
****************************************************************************************************************/
initstate :: Tree -> *State
initstate wordlist
	= {	playmode		= EndPlayer2
	  ,	board			= initboard
	  ,	player			= Player2
	  ,	player1			= {	kind		= Person
						  ,	letters		= []
						  ,	points		= 0
						  ,	placedword	= True
						  }
	  ,	player2			= {	kind		= Computer
						  ,	letters		= []
						  ,	points		= 0
						  ,	placedword	= True
						  }
	  ,	direction		= Horizontal
	  ,	letterbox		= letterbox
	  ,	lexicon			= wordlist
	  ,	wordsadded		= False
	  ,	dimensions		= initdimensions
	  ,	strength		= Maximum
	  ,	progress		= Letter 'a' initplacing
	  ,	random			= []
	  ,	boardinput		= {x=boardwidth/2,y=boardheight/2}
	  }


/***************************************************************************************************************
	When starting a new game the word list should not be read in again because this takes to long. 
	For this purpose the function initialisestate is used.
****************************************************************************************************************/
initialisestate :: (PSt *State) -> PSt *State
initialisestate pst=:{ls=t=:{random=rs,player1,player2},io}
	# (rs,io)					= getRandomList rs io
	  (letterbox,letters1,rs)	= grab letterbox 7 rs
	  (letterbox,letters2,rs)	= grab letterbox 7 rs
	= {pst & ls={t	& playmode		= EndPlayer2
					, player		= Player2
					, player1		= {player1 & letters=letters1,points=0}
					, player2		= {player2 & letters=letters2,points=0}
					, board			= initboard
					, letterbox		= letterbox
					, dimensions	= initdimensions
					, random		= rs
				}
	       , io=io
	  }
where
	getRandomList :: ![Int] !*env -> (![Int],!*env) | TimeEnv env
	getRandomList rs env
		| not (isEmpty rs)
			= (rs,env)
		| otherwise
			# (seed,env)	= getNewRandomSeed env
			= (random_list seed,env)
	where
		random_list :: !RandomSeed -> [Int]
		random_list seed
			# (r,seed)	= random seed
			= [r : random_list seed]

getboardletters :: *State -> ([Char],*State)
getboardletters t=:{board}
	= (getplacedletters board,t)
