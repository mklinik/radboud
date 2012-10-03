implementation module Highscore


/*	General utility for reading/writing high scores to Files and displaying current high scores.
	This module has been written in Clean 2.0 and uses the Object I/O library, version 1.2.2.
*/


import	StdBool, StdEnum, StdFile, StdInt, StdList, StdMisc, StdString, StdTuple
import	StdId, StdSystem, StdWindow

::	HiScores
	:== [HiScore]
::	HiScore
	=	{	name	:: !String
		,	score	:: !Int
		}

//	Read in the high scores:
readHiScores :: !String !*env -> (!(!*File,!HiScores),!*env) | FileSystem env
readHiScores fname env
	# (exists,file,env)		= fopen fpath FReadData env
	| exists
		# (highs,file)		= readHighs file
		= ((file,highs),env)
	| otherwise
		# (_,create,env)	= fopen fpath FWriteData env
		= ((create,[]),env)
where
	fpath = homepath fname
	
	readHighs :: !*File -> (!HiScores,!*File)
	readHighs file
		| sfend file
			= ([],file)
		# (name, file)		= freads file 13
		# (ok,hi,file)		= freadi file
		| not ok
			= ([],file)
		# (ok,_, file)		= freadc file
		| not ok
			= ([],file)
		| otherwise
			# (rest, file)	= readHighs file
			= ([{name=name,score=hi}:rest],file)

//	Write the high scores:
writeHiScores :: !*File !HiScores !*env -> *env | FileSystem env
writeHiScores file highs env
	# (ok,file)	= freopen file FWriteData
	| not ok	= abort "Could not reopen file.\n"
	| otherwise	= snd (fclose (file<<<highs) env)

instance <<< HiScore where
	(<<<) :: !*File !HiScore -> *File
	(<<<) f {name,score}
		=	f<<<take13 name<<<score<<<'\n'

take13 :: !String -> String
take13 string = (string+++"             ")%(0,12)

instance <<< [x] | <<< x where
	(<<<) :: !*File ![x] -> *File | <<< x
	(<<<) f [x:xs]	= f<<<x<<<xs
	(<<<) f _		= f


//	Determine whether, given the number of high scores, a given score is actually a new high score:
itsAHighScore :: !Int !Int !HiScores -> Bool
itsAHighScore nrOfHiScores score scores
	| score==0						= False
	| length scores<nrOfHiScores	= True
	| otherwise						= isItReallyAHighScore score scores
where
	isItReallyAHighScore :: !Int !HiScores -> Bool
	isItReallyAHighScore score` [{score}:hiscores]
		= score`>score || isItReallyAHighScore score` hiscores
	isItReallyAHighScore _ _
		= False


//	Add a HiScore to the current list of high scores:
addScore :: !Int !HiScore !HiScores -> HiScores
addScore nrOfHighScores hi hiscores
	= take nrOfHighScores (addscore hi hiscores)
where
	addscore :: !HiScore !HiScores -> HiScores
	addscore hi` hiscores=:[hi:his]
		| hi.score>hi`.score	= [hi  : addscore hi` his]
		| otherwise				= [hi` : hiscores]
	addscore hi` _				= [hi`]


//	Display high scores in a modal dialog to the user:
showHiScores :: String !HiScores !(PSt *l) -> PSt *l
showHiScores header highs pState=:{io}
	# (okId,io)	= openId io
	# (wId, io)	= openId io
	= snd (openModalDialog undef (dialog wId okId) {pState & io=io})
where
	dialog id okId
		= Dialog "High Scores"
			(	TextControl header	[ControlPos (Center,zero)]
			:+:	text
			:+:	ButtonControl "OK"	[ControlId okId,ControlPos (Center,zero),ControlFunction (noLS (closeWindow id))]
			)
			[	WindowId	id
			,	WindowOk	okId
			]
	text= if (isEmpty highs)
			(ListLS [TextControl "No high scores available." []])
			(ListLS [	TextControl (toString hi+++". "+++take13 name+++" "+++toString score) [ControlPos (Left,zero)]
					\\	(hi,{name,score}) <- zip2 [1..] highs
					])
