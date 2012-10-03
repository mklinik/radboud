implementation module ostoolbox

import	StdBool, StdClass, StdInt, StdMisc, StdTuple
import	clCrossCall_12
import	code from "cCrossCallFont_121."// PA: moved to ostcp, "cCrossCallTCP_121.obj", "cTCP_121.obj"


::	OSToolbox
	:==	Int
	
// OSNewToolbox :: *OSToolbox
OSNewToolbox :== 0

// RWS ??? add success bool
osInitToolbox :: !*OSToolbox -> *OSToolbox		// PA: strictness added
osInitToolbox toolbox
	| toolbox<>0
		= abort "osInitToolbox reinitialised\n"
	# (ok,toolbox)	= winInitOs
	| not ok
		= toolbox // PA: don't abort, otherwise you can't do startIO twice. 
	//	= abort "osInitToolbox failed\n"
	| otherwise
		# toolbox	= winBeginOs toolbox
		# toolbox	= osInstallFont toolbox		// Install font info cross call handling
	//	# toolbox	= osInstallTCP  toolbox		// Install tcp cross call handling	(PA: moved to StdEventTCP)
		= toolbox

osInstallFont :: !*OSToolbox -> *OSToolbox
osInstallFont _
	= code
	{
		.inline InstallCrossCallFont
			ccall InstallCrossCallFont "I-I"
		.end
	}
/*	PA: moved to ostcp
osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP tb
	= snd (IssueCleanRequest2 (\_ tb->(Return0Cci,tb)) (Rq0Cci CcRqCREATETCPWINDOW) (osInstallTCP tb))

osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP _
	= code
	{
		.inline InstallCrossCallTCP
			ccall InstallCrossCallTCP "I-I"
		.end
	}
*/

// RWS ??? ugly
// OSDummyToolbox :: *OSToolbox
OSDummyToolbox :== 0

// PA: moved from world to ostoolbox
worldGetToolbox :: !*World -> (!*OSToolbox,!*World)
worldGetToolbox world
	= (OSNewToolbox,world)

worldSetToolbox :: !*OSToolbox !*World -> *World
worldSetToolbox _ world
	= world
