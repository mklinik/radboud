implementation module Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString

//Data
import Void
import Maybe
import Either

//System
import FilePath
import OSError
import _Pointer

import _Windows

import Text

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world
	# commandLine = packString (foldr (\a b -> a +++ " " +++ b) "" (map escape [path:args]))
	# startupInfo = { createArray STARTUPINFO_size_int 0
	  			 	& [STARTUPINFO_cb_int_offset] 		 = STARTUPINFO_size_bytes
				 	, [STARTUPINFO_dwFlags_int_offset]	 = STARTF_USESTDHANDLES
					}
	# processInformation = createArray PROCESS_INFORMATION_size_int 0
	# (ok, world) = case mCurrentDirectory of
		Just dir	-> createProcessA_dir (packString path) commandLine 0 0 True DETACHED_PROCESS 0 (packString dir) startupInfo processInformation world
		Nothing 	-> createProcessA (packString path) commandLine 0 0 True DETACHED_PROCESS 0 0 startupInfo processInformation world
	| not ok = getLastOSError world
	# processHandle = { processHandle = processInformation.[PROCESS_INFORMATION_hProcess_int_offset]
					  , threadHandle = processInformation.[PROCESS_INFORMATION_hThread_int_offset]
					  }
	= (Ok processHandle, world)
	where
		escape :: !String -> String
		escape s | indexOf " " s == -1                                    = s
				 | size s >= 2 && s.[0] == '"' && (s.[size s - 1] == '"') = s
				 | otherwise                                              = "\"" +++ s +++ "\""

checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess handle=:{processHandle} world
	# (ok, exitCode, world)		= getExitCodeProcess processHandle world
	| not ok					= getLastOSError world
	| exitCode == STILL_ACTIVE	= (Ok Nothing, world)
	# (mbError,world)			= closeProcessHandle handle world
	= (Ok (Just exitCode), world)

waitForProcess :: !ProcessHandle !*World -> (MaybeOSError Int, *World)
waitForProcess handle=:{processHandle} world
	# (res, world)			= waitForSingleObject processHandle INFINITE world
	# (ok, exitCode, world) = getExitCodeProcess processHandle world
	| not ok = getLastOSError world	
	# (mbError,world)		= closeProcessHandle handle world
	= (Ok exitCode, world)

closeProcessHandle :: !ProcessHandle !*World -> (MaybeOSError Void, *World)
closeProcessHandle handle world
	# (ok,world) = closeHandle handle.processHandle world
	| not ok = getLastOSError world
	# (ok, world) = closeHandle handle.threadHandle world
	| not ok = getLastOSError world
	= (Ok Void, world)

callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of
		Error e		= (Error e,world)
		Ok handle	= waitForProcess handle world
		