implementation module ostcptoolbox

import	StdInt, StdTuple
import StdMaybe

import	TCPDef,TCPChannelClass
import	tcp, ostick
import	clCrossCall_12
import	code from "cCrossCallTCP_121."	// PA: moved from ostoolbox

OSinstallTCP :: !*OSToolbox -> *OSToolbox
OSinstallTCP tb
	= snd (issueCleanRequest2 (\_ tb->(return0Cci,tb)) (Rq0Cci CcRqCREATETCPWINDOW) (osInstallTCP tb))

osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP _
	= code {
		ccall InstallCrossCallTCP "I-I"
	}
