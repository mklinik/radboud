definition module Highscore

//	**************************************************************************************************
//
//	General utility for reading/writing high scores to Files and displaying current high scores.
//
//	This module has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import	StdString
from	StdFile	import class FileSystem
from	StdPSt	import :: PSt

::	HiScores
	:== [HiScore]
::	HiScore
	=	{	name	:: !String
		,	score	:: !Int
		}

readHiScores	:: !String !*env				-> (!(!*File,!HiScores),!*env) | FileSystem env
//	Reads high score file from disk.

writeHiScores	:: !*File  !HiScores !*env		-> *env | FileSystem env
//	Writes high scores to disk.

itsAHighScore	:: !Int !Int !HiScores			-> Bool
//	Determines whether, given the number of high scores, a given score is actually a new high score.

addScore		:: !Int !HiScore !HiScores		-> HiScores
//	Add, given the number of high scores, a HiScore to the current list of high scores.

showHiScores	:: String !HiScores !(PSt *l)	-> PSt *l
//	Display current high scores to user in a modal dialog with given Id.
