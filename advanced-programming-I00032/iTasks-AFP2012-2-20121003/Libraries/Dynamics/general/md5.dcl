definition module md5

from StdFile import class FileSystem;
from StdMaybe import :: Maybe;

getMd5DigestFromFile   :: String *f -> (String,*f) | FileSystem f
getMd5DigestFromFile_   :: String *f -> (Maybe String,*f) | FileSystem f
	//creates the message digest for the file named in the parameter

getMd5DigestFromString :: String -> String
	//creates the message digest of the parameter string

