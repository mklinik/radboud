definition module directories

// some simple utility tasks for directory access and text file creation

import iTasks


:: DirFile 		= 	TextFile  FileName 					
				|	Directory FileName
:: FileName		:== String

derive class iTask DirFile, DirPath

getInitialPath 	:: DirFile -> Task DirPath
newDir 			:: DirPath DirFile -> Task Bool
removeDir 		:: DirPath DirFile -> Task Bool
openDirectory	:: DirPath DirFile -> Task DirPath
readDir 		:: DirPath -> Task (Bool,[DirFile])
getParent		:: DirPath -> Task DirPath

safeTextFile 	:: DirPath DirFile String -> Task Bool
readTextFile 	:: DirPath DirFile  -> Task (Bool,String)
deleteTextFile 	:: DirPath DirFile  -> Task Bool



:: DirPath 		=	DirPath Absolute Relative
:: Absolute		:==	String						// absolute path 
:: Relative		:==	[String]					// relative path			

