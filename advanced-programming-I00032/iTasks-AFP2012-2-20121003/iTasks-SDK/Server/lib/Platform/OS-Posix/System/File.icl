implementation module File

//StdEnv
import StdArray
import StdFile
import StdList
import StdString

import Time
import Error
import Void
import OSError
import _Pointer
from _Posix import qualified stat, unlink, rename, sizeOfStat, unpackStat, S_IFDIR, S_IFMT
from _Posix import :: Stat(..)

CHUNK_SIZE :== 1024

instance toString FileError
where
	toString CannotOpen = "Cannot open"
	toString CannotClose = "Cannot close"
	toString IOError = "I/O error"

readFile :: !String !*env -> (!MaybeError FileError String, !*env) | FileSystem env
readFile filename env = withFile filename FReadData readAll env

readAll :: !*File -> (!MaybeError FileError String, !*File)
readAll file
# (result, file) = readAcc file []
= case result of
	Error e	   = (Error e, file)
	Ok contents = (Ok ((foldr (+++) "" (reverse contents))), file)
where
	readAcc :: *File [String] -> (MaybeError FileError [String], *File)
	readAcc file acc
		# (str,file)	= freads file CHUNK_SIZE
		# (err,file)	= ferror file
		| err			= (Error IOError,file)
		# (eof,file)	= fend file
		| eof			= (Ok [str:acc],file)
		| otherwise		= readAcc file [str:acc]			

writeFile :: !String !String !*env -> (!MaybeError FileError Void, !*env) | FileSystem env
writeFile filename contents env = 
	withFile filename FWriteData (\file -> (Ok Void, fwrites contents file)) env

withFile :: !String Int (*File -> (!MaybeError FileError a,!*File)) !*env 
			-> (!MaybeError FileError a, !*env) | FileSystem env
withFile filename filemode operation env
# (ok,file,env)	= fopen filename filemode env
| not ok			= (Error CannotOpen, env)
# (result,file)		= operation file
| isError result 	= (result, env)
# (ok,env)	 		= fclose file env
| not ok			= (Error CannotClose, env)
= (Ok (fromOk result), env)

fileExists ::  !String !*World -> (!Bool, !*World)
fileExists path world 
	# buf			= createArray '_Posix'.sizeOfStat '\0'
	# (ok,world)	= '_Posix'.stat (packString path) buf world
	| ok == 0		= (True, world)
					= (False, world)
	
deleteFile :: !String !*World -> (!MaybeOSError Void, !*World)
deleteFile path world
	# (ok,world)	= '_Posix'.unlink (packString path) world
	| ok <> 0		= getLastOSError world
					= (Ok Void, world)

getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)
getFileInfo path world 
 	# buf           = createArray '_Posix'.sizeOfStat '\0'
    # (ok,world)    = '_Posix'.stat (packString path) buf world
	| ok <> 0		= getLastOSError world
	# stat			= '_Posix'.unpackStat buf
	# (ctime,world)	= toLocalTime (Timestamp stat.st_ctimespec) world //NOT RELIABLE ctime is actually inode change time
	# (mtime,world) = toLocalTime (Timestamp stat.st_mtimespec) world
	# (atime,world) = toLocalTime (Timestamp stat.st_atimespec) world
	= (Ok { directory = (stat.st_mode bitand '_Posix'.S_IFMT) == '_Posix'.S_IFDIR
		  , creationTime = ctime 
		  , lastModifiedTime = mtime 
		  , lastAccessedTime = atime 
		  , sizeHigh = stat.st_blocks * stat.st_blksize 
		  , sizeLow = stat.st_size
		  }, world)

moveFile :: !String !String !*World -> (!MaybeOSError Void, !*World)
moveFile oldpath newpath world
	# (ret,world)	= '_Posix'.rename (packString oldpath) (packString newpath) world
	| ret == 0
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
