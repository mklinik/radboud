implementation module StdPStClass

import	StdFile, StdTuple
import	iostate, StdFileSelect, StdTime
from	scheduler			import handleOneEventForDevices
from	StdPSt				import accPIO, appPIO
import	osfileselect, ostoolbox


/*	PSt and IOSt are environment instances of the class FileSystem (see StdFile).
*/
instance FileSystem (PSt .l) where
	fopen fName fMode pState
		# ((ok,file),pState)	= accFiles (fopen` fName fMode) pState
		= (ok,file,pState)
	fclose file pState
		= accFiles (fclose file) pState
	stdio pState
		= accFiles stdio pState
	sfopen fName fMode pState
		# ((ok,sfile),pState)	= accFiles (sfopen` fName fMode) pState
		= (ok,sfile,pState)

instance FileSystem (IOSt .l) where
	fopen fName fMode ioState
		# ((ok,file),ioState)	= accFiles (fopen` fName fMode) ioState
		= (ok,file,ioState)
	fclose file ioState
		= accFiles (fclose file) ioState
	stdio ioState
		= accFiles stdio ioState
	sfopen fName fMode ioState
		# ((ok,sfile),ioState)	= accFiles (sfopen` fName fMode) ioState
		= (ok,sfile,ioState)

fopen` :: !{#Char} !Int !*Files -> (!(!Bool,!*File),!*Files)
fopen` fName fMode files
	# (ok,file,files)	= fopen fName fMode files
	= ((ok,file),files)

sfopen` :: !{#Char} !Int !*Files -> (!(!Bool,!File),!*Files)
sfopen` fName fMode files
	# (ok,file,files)	= sfopen fName fMode files
	= ((ok,file),files)


/*	PSt and IOSt are environment instances of the class FileEnv (see StdFile).
*/
instance FileEnv (PSt .l) where
	accFiles accfun pState = accPIO (accFiles accfun) pState
	appFiles appfun pState = appPIO (appFiles appfun) pState

instance FileEnv (IOSt .l) where
	accFiles accfun io
		# (world,io)	= ioStGetWorld io
		# (x,world)		= accFiles accfun world
		# io			= ioStSetWorld world io
		= (x,io)
	appFiles appfun io
		# (world,io)	= ioStGetWorld io
		# world			= appFiles appfun world
		# io			= ioStSetWorld world io
		= io

/*	PSt is an environment instance of the class FileSelectEnv (see StdFileSelect).
*/
instance FileSelectEnv (PSt .l) where
	selectInputFile pState
		# (tb,pState)			= accPIO getIOToolbox pState
		# tb					= osInitialiseFileSelectors tb
		# (ok,name,pState,tb)	= osSelectinputfile handleOSEvent pState tb
		# pState				= appPIO (setIOToolbox tb) pState
		= (if ok (Just name) Nothing,pState)
	selectOutputFile prompt originalName pState
		# (tb,pState)			= accPIO getIOToolbox pState
		# tb					= osInitialiseFileSelectors tb
		# (ok,name,pState,tb)	= osSelectoutputfile handleOSEvent pState prompt originalName tb
		# pState				= appPIO (setIOToolbox tb) pState
		= (if ok (Just name) Nothing,pState)
	selectDirectory pState
		# (tb,pState)			= accPIO getIOToolbox pState
		# tb					= osInitialiseFileSelectors tb
		# (ok,name,pState,tb)	= osSelectdirectory handleOSEvent pState tb
		# pState				= appPIO (setIOToolbox tb) pState
		= (if ok (Just name) Nothing,pState)


//	handleOSEvent turns handleOneEventForDevices into the form required by osSelect(in/out)putfile.
handleOSEvent :: !OSEvent !(PSt .l) -> PSt .l
handleOSEvent osEvent pState
	= thd3 (handleOneEventForDevices (ScheduleOSEvent osEvent []) pState)


/*	PSt and IOSt are environment instances of the class TimeEnv (see StdTime).
*/
instance TimeEnv (PSt .l) where
	getBlinkInterval pState = accPIO getBlinkInterval pState
	getCurrentTime   pState = accPIO getCurrentTime   pState
	getCurrentDate   pState = accPIO getCurrentDate   pState
	getCurrentTick   pState = accPIO getCurrentTick   pState

instance TimeEnv (IOSt .l) where
	getBlinkInterval io
		# (world,io)	= ioStGetWorld io
		  (blink,world)	= getBlinkInterval world
		= (blink,ioStSetWorld world io)
	getCurrentTime io
		# (world,io)	= ioStGetWorld io
		  (time,world)	= getCurrentTime world
		= (time,ioStSetWorld world io)
	getCurrentDate io
		# (world,io)	= ioStGetWorld io
		  (date,world)	= getCurrentDate world
		= (date, ioStSetWorld world io)
	getCurrentTick io
		# (world,io)	= ioStGetWorld io
		  (tick,world)	= getCurrentTick world
		= (tick, ioStSetWorld world io)
