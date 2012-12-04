implementation module StdFileSelect

import StdFunc, StdMaybe, StdTuple
import osfileselect
import scheduler

class FileSelectEnv env where
	selectInputFile ::                 !*env -> (!Maybe String,!*env)
	selectOutputFile:: !String !String !*env -> (!Maybe String,!*env)
	selectDirectory ::                 !*env -> (!Maybe String,!*env)

instance FileSelectEnv World where
	selectInputFile :: !*World -> (!Maybe String,!*World)
	selectInputFile world
		# (initContext,tb)			= initContext` world
		# tb						= osInitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= osSelectinputfile handleOSEvent initContext tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)
	
	selectOutputFile :: !String !String !*World -> (!Maybe String,!*World)
	selectOutputFile prompt filename world
		# (initContext,tb)			= initContext` world
		# tb						= osInitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= osSelectoutputfile handleOSEvent initContext prompt filename tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)
	
	selectDirectory :: !*World -> (!Maybe String,!*World)
	selectDirectory world
		# (initContext,tb)			= initContext` world
		# tb						= osInitialiseFileSelectors tb
		# (ok,name,doneContext,tb)	= osSelectdirectory handleOSEvent initContext tb
		= (if ok (Just name) Nothing,closeContext doneContext tb)

handleOSEvent :: !OSEvent !Context -> Context
handleOSEvent osEvent context
	= snd (handleContextOSEvent osEvent context)
