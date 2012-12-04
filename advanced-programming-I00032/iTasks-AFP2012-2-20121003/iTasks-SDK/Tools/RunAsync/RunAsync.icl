module RunAsync

import StdEnv
import StdMaybe
import ArgEnv
import TCPIP

import FilePath
from File import ::FileError, writeFile, moveFile
from Maybe import qualified ::Maybe(..)
import Error
from JSON_NG import ::JSONNode, generic JSONEncode, toJSON, instance toString JSONNode
from OSError import ::OSError(..), ::OSErrorCode, ::OSErrorMessage, ::MaybeOSError(..)
from Process import callProcess
import Text
import Void

Start :: *World -> *World
Start world
	# options = parseCommandLine
	# world = case options.mode of
		Timeout msec 		= world
		Process path args	= process path args options.outfile world
		_ 					= world
	//# world = notify options world
	= world

:: Options =
	{ mode		:: !Mode
	, hostname	:: !String	
	, port		:: !Int
	, url		:: !String
	, taskId	:: !Int
	, outfile	:: !Maybe String
	}
	
:: AsyncResult = 
	{ success	:: !Bool
	, exitcode	:: !Int
	, message	:: !String
	}
	
derive JSONEncode AsyncResult

:: Mode = None
		| Timeout Int
		| Process !FilePath ![String]

defaultOptions :: Options
defaultOptions = 
	{ mode		= None
	, hostname	= "localhost"
	, port		= 80
	, url		= "/services/json/tasks/%t/refresh"
	, taskId	= 0
	, outfile	= Nothing
	}

parseCommandLine :: Options
parseCommandLine 
	# commandLine = getCommandLine
	= parse commandLine 1 (size commandLine - 1) defaultOptions
where
	parse :: {{#Char}} !Int !Int !Options -> Options
	parse args i n options
	| i > n = options
	| i < n && args.[i] == "--hostname"
		= parse args (i + 2) n { options & hostname = args.[i + 1]}
	| i < n && args.[i] == "--port"
		= parse args (i + 2) n { options & port = (toInt args.[i + 1])}
	| i < n && args.[i] == "--url" 
		= parse args (i + 2) n { options & url = args.[i + 1]}
	| i < n && args.[i] == "--taskid" 
		= parse args (i + 2) n { options & taskId = (toInt args.[i + 1])}
	| i < n && args.[i] == "--timeout"
		= parse args (i + 2) n { options & mode = Timeout (toInt args.[i + 1])}
	| i < n && args.[i] == "--outfile" 
		= parse args (i + 2) n { options & outfile = Just (args.[i + 1])}
	| i < n && args.[i] == "--process"
		= { options & mode = Process args.[i + 1] [args.[x] \\ x <- [i + 2 .. n]] }
	= abort ("Invalid argument: " +++ args.[i])

process :: !FilePath ![String] !(Maybe FilePath) !*World -> *World
process path args outfile world
	# (res, world) = call path args world
	| isNothing outfile = world
	# tempfile = (fromJust outfile) +++ ".tmp"
	# (_, world) = writeFile tempfile (toString (toJSON res)) world
	# (_, world) = moveFile tempfile (fromJust outfile) world
	= world
where
	call :: !FilePath ![String] !*World -> *(AsyncResult, *World)
	call path args world
		# (res, world) = callProcess path args 'Maybe'.Nothing world
		| isError res = ({ AsyncResult | success = False, exitcode = -1, message = snd (fromError res)}, world)
		= ({ AsyncResult | success = True, exitcode = fromOk res, message = ""}, world)
	
notify :: !Options *World -> *World
notify options world
	# url				= replaceSubString "%t" (toString options.taskId) options.url
	  request			= "GET " +++ url +++ " HTTP/1.0\r\n\r\n"
	  ticksPerSecond	= 1000
	# (mbIPAddr, world)					= lookupIPAddress options.hostname world
	| isNothing mbIPAddr 				= world
	
	# (tReport, mbDuplexChan, world) 	= connectTCP_MT (Just (5*ticksPerSecond))
											(fromJust mbIPAddr,options.port) world
	| tReport <> TR_Success = world
	
	# { sChannel=sc, rChannel=rc }		= fromJust mbDuplexChan
	# (sc, world)						= send (toByteSeq request) sc world
	# (bs, rc, world)					= receive rc world
	# world								= closeRChannel rc world
	# world								= closeChannel sc world
	= world
