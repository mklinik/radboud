implementation module FilePath

import StdArray
import StdList
import StdTuple
import StdString
import FilePath

import Text
import OS

pathSeparator :: Char
pathSeparator = OS_PATH_SEPARATOR

pathSeparators :: [Char]
pathSeparators = ['\\', '/']

extSeparator :: Char
extSeparator = '.'

(</>) infixr 5 :: !FilePath !FilePath -> FilePath
(</>) x y = (addTrailingPathSeparator x) +++ y

splitExtension :: !FilePath -> (String, String)
splitExtension path = 
	case lastIndexOf {extSeparator} path of
		-1 -> (path, "")
		i  -> (subString 0 i path, subString (i+1) (size path - i - 1) path)

takeExtension :: !FilePath -> String
takeExtension path = snd (splitExtension path)

dropExtension :: !FilePath -> String
dropExtension path = fst (splitExtension path)

addExtension :: !FilePath !String -> FilePath
addExtension path "" = path
addExtension path ext | path.[size path - 1] == extSeparator = path +++ ext
addExtension path ext = path +++ {extSeparator} +++ ext

replaceExtension :: !FilePath !String -> FilePath
replaceExtension path ext = addExtension (dropExtension path) ext

hasTrailingPathSeparator :: !FilePath -> Bool
hasTrailingPathSeparator path = isMember (path.[size path - 1]) pathSeparators

addTrailingPathSeparator :: !FilePath -> FilePath
addTrailingPathSeparator path = if (hasTrailingPathSeparator path) path (path +++ {pathSeparator})

splitFileName  :: !FilePath -> (String, String)
splitFileName path = 
	case lastIndexOf {pathSeparator} path of
		-1 -> (path, "")
		i  -> (subString 0 i path, subString (i+1) (size path - i - 1) path)

takeDirectory :: !FilePath -> FilePath
takeDirectory path = fst (splitFileName path) 

dropDirectory :: !FilePath -> String
dropDirectory path =
	case lastIndexOf {pathSeparator} path of
		-1	-> path
		i	-> (subString (i+1) (size path - i - 1) path)

