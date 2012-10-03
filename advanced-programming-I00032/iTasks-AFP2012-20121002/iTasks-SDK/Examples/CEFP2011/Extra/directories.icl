implementation module directories

// some simple utility tasks for directory access and text file creation

import StdFile
import FilePath, Directory, StdString
import _WinBase

import iTasks

derive class iTask MaybeError, DirPath, DirFile

derive bimap Maybe, (,)

:: DirPath 		=	DirPath Absolute Relative
:: Absolute		:==	String						// absolute path 
:: Relative		:==	[String]					// relative path			
:: DirFile 		= 	TextFile  FileName 	// only files of this particular kind are shown			
				|	Directory FileName
:: FileName		:== String

instance toString DirPath 
where
	toString (DirPath absPath names) = absPath +++ foldl (\x y -> x +++ (toString pathSeparator) +++ y) ""  names

// -------------

getInitialPath :: DirFile -> Task DirPath
getInitialPath (Directory "") = getInitialPath (Directory "temp")
getInitialPath (Directory name) 
	=						accWorld getCurrentDirectory 
		>>= \(Ok dir) ->	mkDir (DirPath dir [name])
where
	mkDir dirPath=:(DirPath dir [name])
		=					readDir dirPath 
			>>= \(ok,_) ->  if ok  (return dirPath)
								   (newDir (DirPath dir []) (Directory name) >>| return dirPath)
getInitialPath _ = getInitialPath (Directory "temp")

newDir :: DirPath DirFile -> Task Bool
newDir (DirPath abs rel) (Directory name)
	=						accWorld (createDirectory (toString (DirPath abs (rel ++ [name]))))
		>>= \result ->		case result of
								(Ok _) -> return True
								_	-> return False
newDir _ _ = return False

removeDir :: DirPath DirFile -> Task Bool
removeDir (DirPath abs rel) (Directory name)
	=						accWorld (removeDirectory (toString (DirPath abs (rel ++ [name]))))
		>>= \result ->		case result of
								(Ok _) -> return True
								_	-> return False
removeDir _ _ 	=			return False


readDir :: DirPath -> Task (Bool,[DirFile])
readDir dirPath 
	=						accWorld (readDirectory (toString dirPath))
		>>= \result ->		case result of
								(Error _) -> return (False,[])
								(Ok names) -> return (True,[TextFile name  \\ name <- names | takeExtension name == "txt"] ++
														   [Directory name \\ name <- names | takeExtension name == "" && name <> "." && name <> ".."])


getParent		:: DirPath -> Task DirPath
getParent dirPath=:(DirPath abs rel) 
| length rel > 1 = return (DirPath abs (init rel))
= return dirPath


openDirectory	:: DirPath DirFile -> Task DirPath
openDirectory dirPath=:(DirPath abs rel) (Directory name) = return (DirPath abs (rel ++ [name])) 
openDirectory dirPath  _ = return dirPath

safeTextFile :: DirPath DirFile String -> Task Bool
safeTextFile dirPath (TextFile fileName) text 
	= 						accWorld (safeFileMonad dirPath fileName text)
where
	safeFileMonad :: DirPath String String *World -> (Bool,*World)
	safeFileMonad dirPath fileName text world 
	# (ok,file,world)  	= fopen (toString dirPath +++ toString pathSeparator +++ fileName) FWriteText world
	| not ok			= (False,world)
	# file				= fwrites text file
	= fclose file world
safeTextFile _ _ _ = return False 

readTextFile :: DirPath DirFile  -> Task (Bool,String)
readTextFile dirPath (TextFile fileName)  
	= 						accWorld (readFileMonad (toString dirPath +++ toString pathSeparator +++ fileName))
where
	readFileMonad :: String  *World -> ((Bool,String),*World)
	readFileMonad fileName world 
	# (ok,file,world)  	= fopen fileName FReadText world
	| not ok			= ((False,""),world)
	# (text,file)		= freads file 1000000
	| text == ""		= ((False,""),world)
	# (ok,world)		= fclose file world
	| not ok			= ((False,""),world)
	= ((True,text),world)
readTextFile _ _ = return (False,"")

deleteTextFile :: DirPath DirFile  -> Task Bool
deleteTextFile dirPath (TextFile fileName) 
	=				accWorld (deleteFileA (toString dirPath +++ toString pathSeparator +++ fileName))
		>>= \i ->	return (i == 0)

deleteTextFile _ _ = return False







	
	