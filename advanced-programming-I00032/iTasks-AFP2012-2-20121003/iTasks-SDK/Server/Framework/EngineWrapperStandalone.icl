implementation module EngineWrapperStandalone

import StdFile, StdInt, StdList, StdChar, StdBool, StdString
import TCPIP, tcp, HTTP, HttpServer, CommandLine, Func

import Engine, IWorld

//Wrapper instance for TCP channels with IWorld
instance ChannelEnv IWorld
where
	channelEnvKind iworld=:{IWorld|world}
		# (kind,world) = channelEnvKind world
		= (kind,{IWorld|iworld & world = world})
	
	mb_close_inet_receiver_without_id b (endpoint,cat) iworld=:{IWorld|world}
		= {IWorld|iworld & world = mb_close_inet_receiver_without_id b (endpoint,cat) world}
	
	channel_env_get_current_tick iworld=:{IWorld|world}
		# (tick,world) = channel_env_get_current_tick world
		= (tick,{IWorld|iworld & world = world})
		
startEngine :: a !*World -> *World | Publishable a
startEngine publishable world
	# (opts,world)			= getCommandLine world
	# (app,world)			= determineAppName world
	# (mbSDKPath,world)		= determineSDKPath SEARCH_PATHS world
	// Show server name
	# world					= show (infoline app) world
	//Check options
	# port 					= fromMaybe DEFAULT_PORT (intOpt "-port" opts)
	# debug					= boolOpt "-debug" opts
	# help					= boolOpt "-help" opts
	# sdkOpt				= stringOpt "-sdk" opts
	//If -help option is given show help and stop
	| help					= show instructions world
	//Check sdkpath
	# mbSDKPath				= maybe mbSDKPath Just sdkOpt //Commandline SDK option overrides found paths
	| isNothing mbSDKPath	= show sdkpatherror world
	//Normal execution
	# world					= show (running port) world
	# options				= [HTTPServerOptPort port, HTTPServerOptDebug debug]
	# iworld				= initIWorld (fromJust mbSDKPath) world
	# iworld				= http_startServer options (engine publishable) iworld
	= finalizeIWorld iworld
where
	infoline :: !String -> [String]
	infoline app	= ["*** " +++ app +++ " HTTP server ***",""]
	
	instructions :: [String]
	instructions =
		["Available commandline options:"
		," -help        : Show this message and exit" 
		," -sdk <path>  : Use <path> as location of the iTasks SDK"
		," -port <port> : Set port number (default " +++ toString DEFAULT_PORT +++ ")"
		," -debug       : Run server in debug mode"
		,""
		]
	
	sdkpatherror :: [String]
	sdkpatherror =
		["Oops! Could not find the iTasks SDK."
		,"The server needs to know the location of the SDK to serve static content"
		,"and run its various utility programs."
		,""
		,"Please put the \"iTasks-SDK\" folder in one of the search locations"
		,"or use the -sdk commandline flag to set the path."
		,"Example: -sdk C:\\Users\\johndoe\\Desktop\\Clean2.3\\iTasks-SDK"
		,""
		,"Tried to find a folder named \"iTasks-SDK\" in the following search locations:"
		:SEARCH_PATHS]
		
	running :: !Int -> [String]
	running port = ["Running at http://localhost" +++ (if (port == 80) "/" (":" +++ toString port +++ "/"))]
	
	show :: ![String] !*World -> *World
	show lines world
		# (console,world)	= stdio world
		# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
		# (_,world)			= fclose console world
		= world
		
	boolOpt :: !String ![String] -> Bool
	boolOpt key opts = isMember key opts
	
	intOpt :: !String ![String] -> Maybe Int
	intOpt key []	= Nothing
	intOpt key [_]	= Nothing
	intOpt key [n,v:r]
		| n == key && isInteger v	= Just (toInt v)
									= intOpt key [v:r]
	where								
		isInteger v = and (map isDigit (fromString v))

	stringOpt :: !String [String] -> Maybe String
	stringOpt key [] = Nothing
	stringOpt key [_] = Nothing
	stringOpt key [n,v:r]
		| n == key	= Just v
					= stringOpt key [v:r]


	
