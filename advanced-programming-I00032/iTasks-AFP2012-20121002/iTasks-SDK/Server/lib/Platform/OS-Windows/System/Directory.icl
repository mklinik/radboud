implementation module Directory

import StdArray, StdBool, StdClass, StdInt, StdChar, StdString

import Void
import File
import FilePath
import OSError

import qualified _Windows
import _Pointer

createDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
createDirectory path world
	# (ok,world)	= '_Windows'.createDirectoryA (packString path) '_Windows'.NULL world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

removeDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
removeDirectory path world
	# (ok,world)	= '_Windows'.removeDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world

readDirectory :: !FilePath !*World -> (!MaybeOSError [FilePath], !*World)
readDirectory path world
	# win32FindData = createArray '_Windows'.WIN32_FIND_DATA_size_bytes '\0'
	# (handle, world) = '_Windows'.findFirstFileA (packString (path </> "*.*")) win32FindData world
	| handle == '_Windows'.INVALID_HANDLE_VALUE = getLastOSError world
	# (entry, world)	= readEntry win32FindData world
	# (entries,world)	= readEntries handle win32FindData world
	# (ok,world) = '_Windows'.findClose handle world
	| not ok = getLastOSError world
	= (Ok [entry:entries], world)
where
	readEntries :: !'_Windows'.HANDLE !'_Windows'.LPWIN32_FIND_DATA !*World -> (![String],!*World)
	readEntries handle win32FindData world
		# (ok,world)	= '_Windows'.findNextFileA handle win32FindData world
		| not ok
			= ([],world)
		# (entry,world)		= readEntry win32FindData world
		# (entries,world)	= readEntries handle win32FindData world
		= ([entry:entries],world)
	
	readEntry :: !'_Windows'.LPWIN32_FIND_DATA !*World -> (!String,!*World) 
	readEntry win32FindData world 
		= (unpackString (win32FindData % ('_Windows'.WIN32_FIND_DATA_cFileName_bytes_offset, '_Windows'.WIN32_FIND_DATA_cFileName_bytes_offset + '_Windows'.MAX_PATH - 1)), world)

getCurrentDirectory :: !*World -> (!MaybeOSError FilePath, !*World)
getCurrentDirectory world
	# buf			= createArray '_Windows'.MAX_PATH '\0'
	# (res,world)	= '_Windows'.getCurrentDirectoryA '_Windows'.MAX_PATH buf world
	| res == 0
		= getLastOSError world
	| otherwise
		= (Ok (unpackString buf),world)

setCurrentDirectory :: !FilePath !*World -> (!MaybeOSError Void, !*World)
setCurrentDirectory path world 
	# (ok,world)	= '_Windows'.setCurrentDirectoryA (packString path) world
	| ok
		= (Ok Void, world)
	| otherwise
		= getLastOSError world
