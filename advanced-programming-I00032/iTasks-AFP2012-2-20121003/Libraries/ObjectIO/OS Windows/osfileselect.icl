implementation module osfileselect


import	StdBool, StdInt
import	clCrossCall_12, osevent
from	clCCall_12	import winMakeCString, winGetCStringAndFree, winReleaseCString, :: CSTR
from	commondef	import fatalError
import code from "cCrossCallFileSelectors_121."


osfileselectFatalError :: String String -> .x
osfileselectFatalError function error
	= fatalError function "osfileselect" error


osInitialiseFileSelectors :: !*OSToolbox -> *OSToolbox
osInitialiseFileSelectors _
	= code
	{
		.inline InstallCrossCallFileSelectors
			ccall InstallCrossCallFileSelectors "I-I"
		.end
	}

osSelectinputfile :: !(OSEvent->.s->.s) !.s !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
osSelectinputfile handleOSEvent state tb
	# (rcci,state,tb)	= issueCleanRequest (callback handleOSEvent) (Rq0Cci CcRqFILEOPENDIALOG) state tb
	# (ok,name,tb)		= getinputfilename rcci tb
	= (ok,name,state,tb)
where
	getinputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getinputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (pathname,tb)	= winGetCStringAndFree ptr tb
			= (True,pathname,tb)
	getinputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getinputfilename {ccMsg} _
		= osfileselectFatalError "osSelectinputfile" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

osSelectoutputfile :: !(OSEvent->.s->.s) !.s !String !String !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
osSelectoutputfile handleOSEvent state prompt filename tb
	# (promptptr,  tb)	= winMakeCString prompt   tb
	# (filenameptr,tb)	= winMakeCString filename tb
	# (rcci,state, tb)	= issueCleanRequest (callback handleOSEvent) (Rq2Cci CcRqFILESAVEDIALOG promptptr filenameptr) state tb
	# tb				= winReleaseCString promptptr   tb
	# tb				= winReleaseCString filenameptr tb
	# (ok,name,tb)		= getoutputfilename rcci tb
	= (ok,name,state,tb)
where
	getoutputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getoutputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (path,tb) = winGetCStringAndFree ptr tb
			= (True,path,tb)
	getoutputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getoutputfilename {ccMsg} _
		= osfileselectFatalError "osSelectoutputfile" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

osSelectdirectory :: !(OSEvent->.s->.s) !.s !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
osSelectdirectory handleOSEvent state tb
	# (rcci,state,tb)	= issueCleanRequest (callback handleOSEvent) (Rq0Cci CcRqDIRECTORYDIALOG) state tb
	# (ok,name,tb)		= getinputfilename rcci tb
	= (ok,name,state,tb)
where
	getinputfilename :: !CrossCallInfo !*OSToolbox -> (!Bool,!String,!*OSToolbox)
	getinputfilename {ccMsg=CcRETURN2,p1=ok,p2=ptr} tb
		| ok==0
			= (False,"",tb)
		| otherwise
			# (pathname,tb)	= winGetCStringAndFree ptr tb
			= (True,pathname,tb)
	getinputfilename {ccMsg=CcWASQUIT} tb
		= (False,"",tb)
	getinputfilename {ccMsg} _
		= osfileselectFatalError "osSelectdirectory" ("unexpected ccMsg field of return CrossCallInfo ("+++toString ccMsg+++")")

//	callback lifts a function::(OSEvent -> .s -> .s) to
//        a crosscallfunction::(CrossCallInfo -> .s -> *OSToolbox -> (CrossCallInfo,.s,*OSToolbox))
callback :: !(OSEvent->.s->.s) !CrossCallInfo !.s !*OSToolbox -> (!CrossCallInfo,!.s,!*OSToolbox)
callback handleOSEvent cci state tb = (return0Cci,handleOSEvent cci state,tb)
