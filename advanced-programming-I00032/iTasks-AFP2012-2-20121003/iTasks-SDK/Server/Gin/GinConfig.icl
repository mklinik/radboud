implementation module GinConfig

import Maybe
import StdFile
from StdFunc import iter

import Directory
from File import qualified fileExists, readFile, writeFile
from FilePath import takeDirectory, </>
import OSError

import CommandLine
from Engine import determineAppName

import iTasks

derive gEq				GinConfig	
derive gVisualizeText	GinConfig
derive gVisualizeHtml	GinConfig
derive gVisualizeEditor	GinConfig
derive gUpdate			GinConfig
derive gDefaultMask		GinConfig
derive JSONEncode		GinConfig
derive JSONDecode		GinConfig

derive bimap (,), Maybe
	
ginDefaultConfig :: !*World -> (GinConfig, *World)
ginDefaultConfig world
# (cleanPath, world)	= getCleanPath world
# (iTasksPath, world)	= getITasksPath world
# config = 	{ cleanPath		= cleanPath
          	, iTasksPath	= iTasksPath
			, userPath		= iTasksPath </> "Examples" </> "Gin" </> "Workflows"
			, searchPaths	= [ iTasksPath </> "Server" </> "API" </> "Core"
							  , iTasksPath </> "Server" </> "API" </> "Common"
							  ]
			}
= (config, world)
where
	getCleanPath :: *World -> (String, *World)
	getCleanPath world
	# (args,world) = getCommandLine world
	# appPath = hd args
	= (iter 3 takeDirectory appPath, world)

	getITasksPath :: *World -> (String, *World)
	getITasksPath world
	# (res, world) = getCurrentDirectory world
	= (takeDirectory (fromOk res), world)

ginLoadConfig :: !*World -> (!Maybe GinConfig, !*World)
ginLoadConfig world
	# (filename, world) = ginConfigFilename world
	# (res,world) = 'File'.readFile filename world
	| isError res = (Nothing, world)
	= (fromJSON (fromString (fromOk res)),world)
	
ginStoreConfig :: !GinConfig !*World -> *World
ginStoreConfig config world
	# (filename, world) = ginConfigFilename world
	# (_, world) = 'File'.writeFile filename (toString (toJSON config)) world
	= world

ginConfigFilename :: *World -> (!String, *World) 
ginConfigFilename world
# (appName, world) = determineAppName world
= (appName +++ "-gin-config.json", world)

derive gVerify GinConfig
/*gVerify{|GinConfig|} val vst = customWorldVerify Nothing check val vst
where
	check config iworld =: { world }
	# (mErr,world) = ginCheckConfig config world 
	# wpr = case mErr of
		Nothing  -> WPRValid Nothing
		Just err -> WPRInvalid err
	= (wpr, { iworld & world = world } )*/

ginCheckConfig :: !GinConfig !*World -> (Maybe String, *World)
ginCheckConfig config world
# (ok, world) = 'File'.fileExists (config.cleanPath </> "CleanIDE.exe") world
| not ok = (Just "Clean path incorrect", world) 
# (ok, world) = 'File'.fileExists (config.iTasksPath </> "Server" </> "iTasks.dcl") world
| not ok = (Just "iTasks path incorrect", world)
# (ok, world) = 'File'.fileExists config.userPath world
| not ok = (Just "user path incorrect", world)
= (Nothing, world)
