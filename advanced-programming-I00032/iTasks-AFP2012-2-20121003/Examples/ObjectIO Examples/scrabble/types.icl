implementation module types


import	StdEnv, StdDebug, StdIOCommon
import	dictionary, language


/***************************************************************************************************************
	Type definitions.
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

instance == Playmode where
	(==) :: !Playmode !Playmode -> Bool
	(==) EndPlayer1	mode	= case mode of
								EndPlayer1	-> True
								_			-> False
	(==) EndPlayer2	mode	= case mode of
								EndPlayer2	-> True
								_			-> False
	(==) Playing	mode	= case mode of
								Playing		-> True
								_			-> False
instance == Playerkind where
	(==) :: !Playerkind !Playerkind -> Bool
	(==) Computer	Computer= True
	(==) Person		Person	= True
	(==) _			_		= False
instance == Player where
	(==) :: !Player !Player -> Bool
	(==) Player1 Player1 = True
	(==) Player2 Player2 = True
	(==) _		 _		 = False
instance == Strength where
	(==) :: !Strength !Strength -> Bool
	(==) Maximum		strength	= case strength of
										Maximum			-> True
										_				-> False
	(==) First			strength	= case strength of
										First			-> True
										_				-> False
	(==) (Strength s1)	strength	= case strength of
										(Strength s2)	-> s1==s2
										_				-> False
instance == Placing where
	(==) :: !Placing !Placing -> Bool
	(==) p1 p2 = p1.word==p2.word && p1.pos==p2.pos && p1.dir==p2.dir && p1.score==p2.score

otherplayer :: !Player -> Player
otherplayer Player1 = Player2
otherplayer Player2 = Player1


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

initplacing :: Placing
initplacing = {word="",pos=(0,0),dir=Horizontal,score=0}

getplacing :: !Progress -> Placing
getplacing (Letter _ p)	= p
getplacing (Finish p)	= p

getletter :: !Progress -> Char
getletter (Letter l _)	= l
getletter (Finish _)	= 'z'
	
notyetready :: !Progress -> Bool
notyetready (Finish _)	= False
notyetready _			= True


/***************************************************************************************************************
	The Tree type stores the lexicon. 
****************************************************************************************************************/

::	Tree :== Dictionary

readtree :: !*env -> (!Tree,!*env) | FileEnv env
readtree filesEnv
	# (words,filesEnv)	= accFiles (readwords (applicationpath lexiconfilename)) filesEnv
	= (sortlistToDictionary words,filesEnv)
where
	readwords :: !String !*Files -> (![Word],!*Files)
	readwords filename filesEnv
		# (ok,f,filesEnv)	= fopen filename FReadText filesEnv
		| not ok
			= trace_n ("Warning: could not open file '"+++filename+++"' for reading") ([],filesEnv)
		| otherwise
			# (lines,f)		= readlines f
			# (_,filesEnv)	= fclose f filesEnv
			= (lines,filesEnv)
	where
		readlines :: !*File -> (![Word],!*File)
		readlines f
			| sfend f
				= ([],f)
			# (line, f)	= freadline f
			# (lines,f)	= readlines f
			  length	= size line
			| length>1
				= ([line%(0,length-2):lines],f)	// remove '\n'
			| otherwise
				= (lines,f)

writetree :: !Tree !*env -> *env | FileEnv env
writetree b filesEnv
	= appFiles (writewords (applicationpath lexiconfilename) (allMembers b)) filesEnv
where
	writewords :: !String [String] !*Files -> *Files
	writewords filename woorden filesEnv
		# (ok,f,filesEnv)	= fopen filename FWriteText filesEnv
		| not ok
			= trace_n ("Warning: could not open file '"+++filename+++"' for writing") filesEnv
		# (ok,f)			= writelines woorden f
		# (_,filesEnv)		= fclose f filesEnv
		| not ok
			= trace_n ("Error occurred while writing file '"+++filename+++"'") filesEnv
		| otherwise
			= filesEnv
	where
		writelines :: ![String] !*File -> (!Bool,!*File)
		writelines [w:ws] f
			# f			= f<<<w<<<'\n'
			# (error,f)	= ferror f
			| error
				= (False,f)
			| otherwise
				= writelines ws f
		writelines _ f
			= (True,f)
