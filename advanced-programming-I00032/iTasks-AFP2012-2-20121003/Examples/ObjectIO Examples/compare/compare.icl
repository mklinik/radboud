module compare


//	**************************************************************************************************
//
//	A program in which two text files can be compared char by char.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************


import StdEnv, StdIO


::	Local
	=	{	name1	:: String
		,	name2	:: String
		}

noFilesSelected
	= { name1="", name2="" }


Start :: *World -> *World
Start world
    = startIO NDI noFilesSelected initIO [] world

initIO :: (PSt Local) -> PSt Local
initIO pst
	# (showid,pst)	= accPIO openId pst
	# (_,pst)		= openDialog undef (dialog showid) pst
	= pst
where
	dialog showid
		= Dialog "Compare"
			(	ButtonControl "&Compare..."
					[	ControlFunction (noLS compare)
					,	ControlTip		"Compare two files"
					]
			:+:	ButtonControl "Compare &again"
					[	ControlFunction (noLS again)
					,	ControlTip		"Compare the files again"
					]
			:+:	ButtonControl "&Quit"
					[	ControlFunction (noLS closeProcess)
					,	ControlTip		"Quit compare"
					]
			)	[]
	where
		compare :: (PSt Local) -> PSt Local
		compare pst
			# (maybeFirstFile,pst)		= selectInputFile pst
			| isNothing maybeFirstFile
				= {pst & ls=noFilesSelected}
			# (maybeSecondFile,pst)		= selectInputFile pst
			| isNothing maybeSecondFile
				= {pst & ls=noFilesSelected}
			| otherwise
				# pst	= {pst & ls={name1=fromJust maybeFirstFile,name2=fromJust maybeSecondFile}}
				= showdifference pst
		
		again :: (PSt Local) -> PSt Local
		again pst=:{ls={name1,name2}}
			| name1=="" || name2==""	= compare pst
			| otherwise					= showdifference pst
		
		showdifference :: (PSt Local) -> PSt Local
		showdifference pst=:{ls={name1,name2}}
			# pst						= closeWindow showid pst
			# (files,pst)				= openfilepair (name1,name2) pst
			  (maybeDifference,files)	= comparefilepair 1 files
			# pst						= closefilepair files pst
			| isNothing maybeDifference
				= appPIO beep pst
			# (error,pst)				= openDialog undef (dialog (fromJust maybeDifference)) pst
			| error<>NoError
				= abort "Could not open dialog."
			| otherwise
				= pst
		where
			dialog (i,line1,line2)		= Dialog "Difference found"
											(	ListLS
											[	TextControl ("Difference at line "+++toString i) []
											,	TextControl line1 [ControlPos (Left,NoOffset)]
											,	TextControl line2 [ControlPos (Left,NoOffset)]
											])
											[	WindowId	showid
											,	WindowClose	(noLS (closeWindow showid))
											]

openfilepair :: (String,String) *env -> ((*File,*File), *env) | FileSystem env
openfilepair (fname1,fname2) env
	# (ok,f1,env)	= fopen fname1 FReadText env
	| not ok		= abort ("Could not open "+++fname1)
	# (ok,f2,env)	= fopen fname2 FReadText env
	| not ok		= abort ("Could not open "+++fname2)
	| otherwise		= ((f1,f2),env)

closefilepair :: (*File,*File) *env -> *env | FileSystem env
closefilepair (f1,f2) env
	# (ok,env)		= fclose f1 env
	| not ok		= abort "Could not close first file."
	# (ok,env)		= fclose f2 env
	| not ok		= abort "Could not close second file."
	| otherwise		= env

comparefilepair :: Int (*File,*File) -> (Maybe (Int,String,String), (*File,*File))
comparefilepair i (f1,f2)
	| sfend f1 && sfend f2	= (Nothing,(f1,f2))
	# (line1,f1)			= freadline f1
	# (line2,f2)			= freadline f2
	| line1<>line2			= (Just (i,line1,line2),(f1,f2))
	| otherwise				= comparefilepair (i+1) (f1,f2)
