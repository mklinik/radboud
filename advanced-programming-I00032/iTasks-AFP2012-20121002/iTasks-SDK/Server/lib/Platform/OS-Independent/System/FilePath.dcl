definition module FilePath
/**
	Module for manipulation of file and directory paths
*/

:: FilePath :== String

/**
* Returns the default platform path separator
*/
pathSeparator :: Char

/**
* Returns a list of all allowed platform path separators
*/
pathSeparators :: [Char]

/**
* Returns the default file extension separator
*/
extSeparator :: Char

/**
* Concatenates two paths
*/
(</>) infixr 5 :: !FilePath !FilePath -> FilePath

/**
* Split a FilePath into filename and extension. The result does not include the extension separator (.).
*/
splitExtension :: !FilePath -> (String, String)

/**
* Take the extension of a FilePath, excluding the separator
*/
takeExtension :: !FilePath -> String

/**
* Remove the extension and extension separator of a FilePath
*/
dropExtension :: !FilePath -> String

/**
* Add an extension to a FilePath
*/
addExtension :: !FilePath !String -> FilePath

/**
* Replace the extension of a FilePath
*/
replaceExtension :: !FilePath !String -> FilePath

/**
* Take the directory part of a FilePath. If the FilePath is a directory, 
* the result is the parent directory.
*/
takeDirectory :: !FilePath -> FilePath

/**
* Drop the directory part of a FilePath. Keep only the filename.
*/
dropDirectory :: !FilePath -> String
