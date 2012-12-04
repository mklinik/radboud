definition module types


import	StdString, StdClass, StdFile
import	StdIOCommon
import	dictionary


/***************************************************************************************************************
	Basic type definitions.
****************************************************************************************************************/
::	Playmode			=	EndPlayer1 | EndPlayer2 | Playing
::	Playerkind			=	Computer | Person
::	Player				=	Player1 | Player2
::	Strength			=	Maximum | First | Strength Real
::	Word				:==	String
::	Position			:==	(!Int,!Int)

MediumStrength			:==	Strength 0.5
EasyStrength			:==	Strength 0.25
VeryEasyStrength		:==	Strength 0.125

instance == Playmode
instance == Playerkind
instance == Player
instance == Strength
instance == Placing

otherplayer :: !Player -> Player


/***************************************************************************************************************
	The type Progress is by the computer player function when determining a move. The computer player checks in 
	alfabetic order all words starting with a particular letter.
	
	Words starting with a particular letter are handled quickly when the starting letter does not occur on the 
	letter bar. In that case the positions on the board are checked if they are valid as a starting position for 
	the word (horizontal and vertical are handled separately).
	
	For words starting with a particular letter on the letter bar more board positions need to be examined.
****************************************************************************************************************/
::	Progress
	=	Letter Char Placing
	|	Finish Placing
::	Placing
	=	{	word	:: Word
		,	pos		:: Position
		,	dir		:: Direction
		,	score	:: Int
		}
initplacing			:: Placing
getplacing			:: !Progress -> Placing
getletter			:: !Progress -> Char
notyetready			:: !Progress -> Bool


/***************************************************************************************************************
	The Tree type stores the lexicon. 
****************************************************************************************************************/

::	Tree :== Dictionary

readtree			:: !*env			-> (!Tree,!*env)	| FileEnv env
writetree			:: !Tree !*env		-> *env				| FileEnv env
/*
wordsstartingwith	:: !Char !Tree		-> [Word]
seek				:: !Tree !String	-> Bool
sizetree			:: !Tree			-> Int
depthtree			:: !Tree			-> Int
addwordstotree		:: !Tree ![Word]	-> Tree
*/