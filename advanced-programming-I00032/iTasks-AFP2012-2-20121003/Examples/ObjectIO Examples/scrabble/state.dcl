definition module state


from	StdPSt	import :: PSt
import	board


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
		,	points			:: Int					//	Points of player
		,	placedword		:: Bool					//	Player placed a word in last turn (initially True)
		}

initstate		:: Tree  -> *State
initialisestate	:: (PSt *State) -> PSt *State
getboardletters	:: *State -> ([Char],*State)
