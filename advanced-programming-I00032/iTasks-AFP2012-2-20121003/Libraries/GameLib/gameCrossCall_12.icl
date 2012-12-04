implementation module gameCrossCall_12

import	StdList, StdTuple
from	commondef			import fatalError
from	windowCrossCall_12	import winFakePaint
import	clCrossCall_12, gameintrface_12
import	ostypes
import code from "cCrossCallGame_121.",
				 "cGameLib_121.",
				 "cOSGameLib_121."
//				 "ddutil.obj"          
//				 "dsutil.obj"
import code from library "ddraw_library"
import code from library "dsound_library"


gameCrossCall_12FatalError :: String String -> .x
gameCrossCall_12FatalError function error
	= fatalError function "gameCrossCall_12" error

//	Initialisation of game. This function must be called before any game cross call can be done!
WinInitialiseGame :: !*OSToolbox -> *OSToolbox
WinInitialiseGame tb
	= code
	{
		.inline InstallCrossCallGame
			ccall InstallCrossCallGame "I-I"
		.end
	}

  //----------------------------------------------//
 //    Game related crosscalls                   //
//----------------------------------------------//

WinCreateGameWindow :: !Bool !(!Int,!Int) !Int !*OSToolbox -> (![DelayActivationInfo],!OSWindowPtr,!*OSToolbox)
WinCreateGameWindow fullscreen size bpp tb
	# createcci		= {ccMsg=CcRqCREATEGAMEWINDOW,p1=w,p2=h,p3=bpp,p4=toInt fullscreen,p5=0,p6=0}
	# (returncci,delay_info,tb)
					= issueCleanRequest OScreateGameWindowCallback createcci [] tb
	  wPtr			= case returncci.ccMsg of
						CcRETURN1   -> returncci.p1
						CcWASQUIT   -> OSNoWindowPtr
						_           -> gameCrossCall_12FatalError "WinCreateGameWindow" "Expected CcRETURN1 value."
	= (reverse delay_info,wPtr,tb)
where
    (w,h)           = size

    OScreateGameWindowCallback :: !CrossCallInfo ![DelayActivationInfo] !*OSToolbox
                              -> (!CrossCallInfo,![DelayActivationInfo],!*OSToolbox)
    OScreateGameWindowCallback {ccMsg=CcWmPAINT,p1=hwnd} s tb
        = (return0Cci, s, winFakePaint hwnd tb)
    OScreateGameWindowCallback {ccMsg=CcWmACTIVATE,p1=hwnd} delay_info tb
        = (return0Cci, [DelayActivatedWindow hwnd:delay_info], tb)
    OScreateGameWindowCallback {ccMsg=CcWmDEACTIVATE,p1=hwnd} delay_info tb
        = (return0Cci, [DelayDeactivatedWindow hwnd:delay_info], tb)
    OScreateGameWindowCallback {ccMsg=CcWmCREATE,p1=hwnd} delay_info tb
        = (return0Cci, delay_info, tb)
    OScreateGameWindowCallback {ccMsg=CcWmSIZE,p1=hwnd,p2=width,p3=height} s tb
        = (return0Cci, s, tb)
    OScreateGameWindowCallback {ccMsg} s tb
        = gameCrossCall_12FatalError "WinCreateGameWindowCallback" ("unknown message type ("+++toString ccMsg+++")")

WinRunGameEngine :: !(CrossCallInfo -> .(.s -> .(*OSToolbox -> *(.CrossCallInfo,.s,*OSToolbox)))) !.s !Int !Int !Int !*OSToolbox
                 -> (!.s,!*OSToolbox)
WinRunGameEngine handleGameEvents initState a b c tb
    # (finalOScci,finalState,tb) = issueCleanRequest handleGameEvents (Rq3Cci CcRqRUNGAME a b c) initState tb
    = (finalState,tb)

WinInitGameObject :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinInitGameObject objtype subtype x y tb
    # tb = snd (issueCleanRequest2 (errorCallback2 "WinInitGameObject") (Rq4Cci CcRqCREATEGAMEOBJECT objtype subtype x y) tb)
    = (0, tb)

// modified 01/11/99
WinCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinCreateUserEvent ev evpar1 evpar2 target subtarget time tb
    # tb = snd (issueCleanRequest2 (errorCallback2 "WinCreateUserEvent") (Rq6Cci CcRqUSERGAMEEVENT ev evpar1 evpar2 target subtarget time) tb)
    = (0, tb)

WinPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinPlaySoundSample id vol pan freq delay tb
    # tb = snd (issueCleanRequest2 (errorCallback2 "WinCreateUserEvent") (Rq5Cci CcRqPLAYSOUNDSAMPLE id vol pan freq delay) tb)
    = (0, tb)
