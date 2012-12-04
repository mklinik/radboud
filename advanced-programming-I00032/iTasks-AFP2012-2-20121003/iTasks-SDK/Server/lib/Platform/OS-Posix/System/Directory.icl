implementation module Directory

import StdArray, StdBool, StdClass, StdInt, StdChar, StdString

import Void
import FilePath
import OSError

import _Posix
import _Pointer

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
createDirectory path world
	# (ret,world)	= mkdir (packString path) 493 world // 493 = 0755 in octal
	| ret == 0
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
removeDirectory path world
	# (ret,world)	= rmdir (packString path) world
	| ret == 0
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
readDirectory path world
	# (dirptr,world)	= opendir path world
	| dirptr == 0
		= getLastOSError world
	# (entries,world)	= readEntries dirptr world
	# (ret,world)		= closedir dirptr world
	| ret == 0
		= (Ok entries, world)
	| otherwise
		= getLastOSError world
where
	readEntries :: !Pointer !*World -> (![String],!*World)
	readEntries dirptr world
		# (entryptr,world)	= readdir dirptr world
		| entryptr == 0
			= ([],world)
		# (entry,world)		= readEntry entryptr world
		# (entries,world)	= readEntries dirptr world
		= ([entry:entries],world)
	
	readEntry :: !Pointer !*World -> (!String,!*World) 
	readEntry entryptr world
		= (derefString (entryptr + DIRENT_D_NAME_OFFSET), world)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)
getCurrentDirectory world
	# buf			= createArray MAXPATHLEN '\0'
	# (ptr,world)	= getcwd buf MAXPATHLEN world
	| ptr == 0
		= getLastOSError world
	| otherwise
		= (Ok {c \\ c <-: buf | c <> '\0'},world)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
setCurrentDirectory path world 
	# (ret,world)	= chdir (packString path) world
	| ret == 0
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
