implementation module ostcp

import	StdInt, StdTuple
import	StdTCPDef, StdTime
import	StdChannels
import	tcp, ostick
import	code from "cTCP_121.", library "wsock_library"
import	clCrossCall_12
import	code from "cCrossCallTCP_121."	// PA: moved from ostoolbox

// PA: moved from ostoolbox:
OSinstallTCP :: !*OSToolbox -> *OSToolbox
OSinstallTCP tb
	= snd (issueCleanRequest2 (\_ tb->(return0Cci,tb)) (Rq0Cci CcRqCREATETCPWINDOW) (osInstallTCP tb))

osInstallTCP :: !*OSToolbox -> *OSToolbox
osInstallTCP _
	= code {
		ccall InstallCrossCallTCP "I-I"
	}
// ...PA

os_eom :: !EndpointRef !*env -> (!Bool, !*env)
os_eom er e
// check for eom
	= IF_INT_64_OR_32 (os_eom64 er e) (os_eom32 er e);

os_eom64 :: !EndpointRef !*env -> (!Bool, !*env)
os_eom64 er e
	= code inline {
		ccall os_eom "p:I:A"
	}

os_eom32 :: !EndpointRef !*env -> (!Bool, !*env)
os_eom32 er e
	= code inline {
		ccall os_eom "I:I:A"
	}
		
os_disconnected :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected er e
// check for disconnected
	= IF_INT_64_OR_32 (os_disconnected64 er e) (os_disconnected32 er e);

os_disconnected64 :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected64 er e
	= code inline {
		ccall os_disconnected "p:I:A"
	}

os_disconnected32 :: !EndpointRef !*env -> (!Bool, !*env)
os_disconnected32 er e
	= code inline {
		ccall os_disconnected "I:I:A"
	}

os_connectrequestavailable :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable er e
	= IF_INT_64_OR_32 (os_connectrequestavailable64 er e) (os_connectrequestavailable32 er e);

os_connectrequestavailable64 :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable64 er e
	= code inline {
		ccall os_connectrequestavailable "p:I:A"
	}

os_connectrequestavailable32 :: !EndpointRef !*env -> (!Bool, !*env)
os_connectrequestavailable32 er e
	= code inline {
		ccall os_connectrequestavailable "I:I:A"
	}

os_connectTCP :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP onlyForMac block time addr e
	= IF_INT_64_OR_32 (os_connectTCP64 onlyForMac block time addr e) (os_connectTCP32 onlyForMac block time addr e);

os_connectTCP64 :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP64 onlyForMac block time addr e
	= code inline {
		ccall os_connectTCPC "IIIIII:VIIp:A"
	}

os_connectTCP32 :: !Int !Bool !(!Bool, !Int) !(!Int,!Int) !*env -> (!(!InetErrCode,!Bool,!EndpointRef), !*env)
os_connectTCP32 onlyForMac block time addr e
	= code inline {
		ccall os_connectTCPC "IIIIII:VIII:A"
	}

os_select_inetevents :: !EndpointRef !InetReceiverCategory !Int !Bool !Bool !Bool !*env -> *env
os_select_inetevents endpointRef receiverType referenceCount get_receive_events get_sendable_events alreadyEom env
	= IF_INT_64_OR_32
		(os_select_inetevents64 endpointRef receiverType referenceCount get_receive_events get_sendable_events alreadyEom env)
		(os_select_inetevents32 endpointRef receiverType referenceCount get_receive_events get_sendable_events alreadyEom env);

os_select_inetevents64 :: !EndpointRef !InetReceiverCategory !Int !Bool !Bool !Bool !*env -> *env
os_select_inetevents64 endpointRef receiverType referenceCount get_receive_events get_sendable_events alreadyEom env
	= code inline {
		ccall os_select_inetevents "pIIIII:V:A"
	}

os_select_inetevents32 :: !EndpointRef !InetReceiverCategory !Int !Bool !Bool !Bool !*env -> *env
os_select_inetevents32 endpointRef receiverType referenceCount get_receive_events get_sendable_events alreadyEom env
	= code inline {
		ccall os_select_inetevents "IIIIII:V:A"
	}

getMbStopTime :: !(Maybe Timeout) !*env -> (!(!Bool, !Int), !*env) | ChannelEnv env
getMbStopTime Nothing env
	=((False,0), env)
getMbStopTime (Just timeout) env
	# (now, env) = getCurrentTick env
	= ((True, timeout + (unpack_tick now)), env)
