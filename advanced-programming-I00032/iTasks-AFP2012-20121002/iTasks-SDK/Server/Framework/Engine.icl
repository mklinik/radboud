implementation module Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, Func
from StdFunc import o, seqList, ::St
import	Map, Time, CommandLine, Environment, Error, File, FilePath, Directory, HTTP, OSError, Text, MIME, UrlEncoding
import	Util, HtmlUtil
import	IWorld
import	WebService

CLEAN_HOME_VAR	:== "CLEAN_HOME"

// The iTasks engine consist of a set of HTTP request handlers
engine :: publish -> [(!String -> Bool,!HTTPRequest *IWorld -> (!HTTPResponse, !*IWorld))] | Publishable publish
engine publishable
	= taskHandlers (publishAll publishable) ++ defaultHandlers
where
	taskHandlers published
		= [((==) (URL_PREFIX +++ url), webService task defaultFormat) \\ {url,task=TaskWrapper task,defaultFormat} <- published]	
	
	defaultHandlers
		= [((==) "/stop", handleStopRequest)
		  ,(startsWith URL_PREFIX, handleStaticResourceRequest)
		  ]

initIWorld :: !FilePath !*World -> *IWorld
initIWorld sdkPath world
	# (appName,world) 			= determineAppName world
	# (appPath,world)			= determineAppPath world
	# appDir					= takeDirectory appPath
	# dataDir					= appDir </> appName +++ "-data"
	# (res,world)				= getFileInfo appPath world
	| isError res				= abort "Cannot get executable info."
	# tm						= (fromOk res).lastModifiedTime
	# build						= strfTime "%Y%m%d-%H%M%S" tm
	# (timestamp,world)			= time world
	# (currentDateTime,world)	= currentDateTimeWorld world
	# (_,world)					= ensureDir "data" dataDir world
	# tmpDir					= dataDir </> "tmp-" +++ build
	# (_,world)					= ensureDir "tmp" tmpDir world
	# storeDir					= dataDir </> "store-"+++ build
	# (exists,world)			= ensureDir "store" storeDir world
	= {IWorld
	  |application			= appName
	  ,build				= build
	  ,appDirectory			= appDir
	  ,sdkDirectory			= sdkPath
	  ,dataDirectory		= dataDir
	  ,config				= defaultConfig
	  ,taskTime				= 0
	  ,timestamp			= timestamp
	  ,currentDateTime		= currentDateTime
	  ,currentUser			= AnonymousUser ""
	  ,currentInstance		= 0
	  ,nextTaskNo			= 0
	  ,localShares			= newMap
	  ,localLists			= newMap
	  ,readShares			= []
	  ,outdated				= False
	  ,sessions				= newMap
	  ,uis					= newMap
	  ,world				= world
	  }
where
	defaultConfig :: Config
	defaultConfig =
		{ sessionTime		= 3600
		, smtpServer		= "localhost"
		}
		
	padZero :: !Int -> String
	padZero number = (if (number < 10) "0" "") +++ toString number

	ensureDir :: !String !FilePath *World -> (!Bool,!*World)
	ensureDir name path world
		# (exists, world) = fileExists path world
		| exists = (True,world)
		# (res, world) = createDirectory path world
		| isError res = abort ("Cannot create " +++ name +++ " directory" +++ path +++ " : "  +++ snd (fromError res))
		= (False,world)

finalizeIWorld :: !*IWorld -> *World
finalizeIWorld iworld=:{IWorld|world} = world

// Request handler which serves static resources from the application directory,
// or a system wide default directory if it is not found locally.
// This request handler is used for serving system wide javascript, css, images, etc...
handleStaticResourceRequest :: !HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
handleStaticResourceRequest req iworld=:{IWorld|sdkDirectory,world}
	# (appPath,world)		= determineAppPath world
	# path					= subString (size URL_PREFIX) (size req.req_path) req.req_path
	# filename				= sdkDirectory </> "Client" +++ filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))]
							   	,rsp_data = fromOk mbContent}, {IWorld|iworld & world = world})
	# filename				= takeDirectory appPath </> "Static" +++ filePath path
	# type					= mimeType filename
	# (mbContent, world)	= readFile filename world
	| isOk mbContent 		= ({rsp_headers = fromList [("Status","200 OK"),
											   ("Content-Type", type),
											   ("Content-Length", toString (size (fromOk mbContent)))											   
											   ]
							   	,rsp_data = fromOk mbContent},{IWorld|iworld & world = world})						   								 	 							   
	= (notFoundResponse req,{IWorld|iworld & world = world})
where
	//Translate a URL path to a filesystem path
	filePath path	= ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) path
	mimeType path	= extensionToMimeType (takeExtension path)

handleStopRequest :: HTTPRequest *IWorld -> (!HTTPResponse,!*IWorld)
handleStopRequest req iworld = ({newHTTPResponse & rsp_headers = fromList [("X-Server-Control","stop")], rsp_data = "Server stopped..."}, iworld) //Stop

path2name path = last (split "/" path)

publish :: String ServiceFormat (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url format task = {url = url, task = TaskWrapper task, defaultFormat = format}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" WebApp (\_ -> task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" WebApp task]
	
instance Publishable [PublishedTask]
where
	publishAll list = list

// Determines the server executables path
determineAppPath :: !*World -> (!FilePath, !*World)
determineAppPath world
	# ([arg:_],world) = getCommandLine world
	| dropDirectory arg <> "ConsoleClient.exe" = (arg,world) //toCanonicalPath arg world
	//Using dynamic linker:	
	# (res, world)				= getCurrentDirectory world	
	| isError res				= abort "Cannot get current directory."	
	# currentDirectory			= fromOk res
	# (res, world)				= readDirectory currentDirectory world	
	| isError res				= abort "Cannot read current directory."	
	# batchfiles				= [f \\ f <- fromOk res | takeExtension f == "bat" ]
	| isEmpty batchfiles		= abort "No dynamic linker batch file found."	
	# (infos, world)			= seqList (map getFileInfo batchfiles) world	
	| any isError infos	 		= abort "Cannot get file information."	
	= (currentDirectory </> (fst o hd o sortBy cmpFileTime) (zip2 batchfiles infos), world)	
	where		
		cmpFileTime (_,Ok {FileInfo | lastModifiedTime = x})
					(_,Ok {FileInfo | lastModifiedTime = y}) = mkTime x > mkTime y
	
// Determines the server executables name
determineAppName :: !*World -> (!String,!*World)
determineAppName world 
	# (appPath, world) = determineAppPath world
	= ((dropExtension o dropDirectory) appPath, world)

determineSDKPath :: ![FilePath] !*World -> (!Maybe FilePath, !*World)
determineSDKPath paths world
	//Try environment var first
	# (mbCleanHome,world) = getEnvironmentVariable CLEAN_HOME_VAR world
	= case mbCleanHome of
		Nothing			= searchPaths paths world
		Just cleanHome	= searchPaths [cleanHome] world
where	
	searchPaths [] world = (Nothing, world)
	searchPaths [p:ps] world
		# (mbInfo,world) = getFileInfo path world
		= case mbInfo of
			Ok info	| info.directory	= (Just path,world)
			_							= searchPaths ps world
	where
		path = (p </> "iTasks-SDK")
	
