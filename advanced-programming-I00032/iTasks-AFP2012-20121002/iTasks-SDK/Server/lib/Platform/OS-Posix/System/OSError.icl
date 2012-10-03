implementation module OSError

import Error, _Pointer
from _Posix import qualified errno, strerr

getLastOSError :: *World -> (MaybeOSError .a, *World)
getLastOSError world 
	# (errno,world) = '_Posix'.errno world
	= (Error (errno, message errno),world)
where
	message :: !Int -> String
	message errno
		# ptr = '_Posix'.strerr errno
		= derefString ptr

getLastOSErrorCode :: *World -> (MaybeOSErrorCode .a, *World)
getLastOSErrorCode world 
	# (errno,world) = '_Posix'.errno world
	= (Error errno, world)
