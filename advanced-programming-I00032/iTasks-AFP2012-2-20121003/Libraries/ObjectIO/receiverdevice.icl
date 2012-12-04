implementation module receiverdevice


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	StdReceiver
import	devicefunctions, iostate, receiveraccess, receiverevent, receiverid
from	commondef	import fatalError, uremove, :: UCond, strictSeq, accessList
from	StdPSt		import appPIO, accPIO


receiverdeviceFatalError :: String String -> .x
receiverdeviceFatalError rule error
	= fatalError rule "receiverdevice" error

receiverFunctions :: DeviceFunctions (PSt .l)
receiverFunctions
	= {	dDevice	= ReceiverDevice
	  ,	dShow	= id
	  ,	dHide	= id
	  ,	dEvent	= receiverEvent
	  ,	dDoIO	= receiverIO
	  ,	dOpen	= receiverOpen
	  ,	dClose	= receiverClose
	  }

receiverOpen :: !(PSt .l) -> PSt .l
receiverOpen pState=:{io=ioState}
	# (hasReceiver,ioState)	= ioStHasDevice ReceiverDevice ioState
	| hasReceiver
		= {pState & io=ioState}
	| otherwise
		# ioState			= ioStSetDevice (ReceiverSystemState {rReceivers=[]}) ioState
		# ioState			= ioStSetDeviceFunctions receiverFunctions ioState
		= {pState & io=ioState}

receiverClose :: !(PSt .l) -> PSt .l
receiverClose pState=:{io=ioState}
	# ioState					= callReceiverCloseFunctions ioState
	# ioState					= ioStSetRcvDisabled True ioState
	# (found,rDevice,ioState)	= ioStGetDevice ReceiverDevice ioState
	| not found
		= {pState & io=ioState}
	| otherwise
		# rHs					= (receiverSystemStateGetReceiverHandles rDevice).rReceivers
		  rIds					= map (\{rHandle={rId}}->rId) rHs
		# (idtable,ioState)		= ioStGetIdTable ioState
		# ioState				= ioStSetIdTable (snd (removeIdsFromIdTable rIds idtable)) ioState
		# ioState				= unbindRIds rIds ioState
		# ioState				= ioStRemoveDeviceFunctions ReceiverDevice ioState
		= {pState & io=ioState}
where
	callReceiverCloseFunctions :: !(IOSt .l) -> (IOSt .l)
	callReceiverCloseFunctions ioState
		# (found,rDevice,ioState)	= ioStGetDevice ReceiverDevice ioState
		| not found
			= ioState
		| otherwise
			# rHs					= (receiverSystemStateGetReceiverHandles rDevice).rReceivers
			# (funcs,rHs)			= accessList getCloseFunc rHs
			# ioState				= ioStSetDevice (ReceiverSystemState {rReceivers=rHs}) ioState
			= strictSeq funcs ioState
	where
		getCloseFunc :: !(ReceiverStateHandle (PSt .l)) -> (!IdFun (IOSt .l),!ReceiverStateHandle (PSt .l))
		getCloseFunc rsH=:{rHandle={rInetInfo=Nothing, rConnected}}
			= (strictSeq (map closeReceiver rConnected),rsH)
		getCloseFunc rsH=:{rHandle={rInetInfo=Just (_,_,_,closeFun),rConnected}}
			= (appIOToolbox closeFun o strictSeq (map closeReceiver rConnected),rsH)

/*	The receiver handles three cases of message events (for the time being timers are not included in receivers):
	- QASyncMessage:
		this is a request to add the given asynchronous message to the indicated 
		receiver. Globally the size of the asynchronous message queue has already been
		increased. 
	- ASyncMessage:
		this is a request to handle the first asynchronous message available in the
		asynchronous message queue. Globally the size of the asynchronous message queue
		has already been decreased.
	- SyncMessage:
		this is a request to handle the given synchronous message.
*/
receiverIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
receiverIO deviceEvent pState=:{io=ioState}
	# (found,rDevice,ioState)	= ioStGetDevice ReceiverDevice ioState
	| not found					// This condition should not occur: dDoIO function should be applied only iff dEvent filters message
		= receiverdeviceFatalError "receiverIO" "could not retrieve ReceiverSystemState from IOSt"
	| otherwise
		# receivers				= receiverSystemStateGetReceiverHandles rDevice
		= receiverIO deviceEvent receivers.rReceivers {pState & io=ioState}
where
	receiverIO :: !DeviceEvent !*[ReceiverStateHandle (PSt .l)] !(PSt .l) -> (!DeviceEvent,!PSt .l)
	receiverIO deviceEvent=:(ReceiverEvent (QASyncMessage event)) rsHs pState
		= (deviceEvent,receiverASyncIO event rsHs pState)
	where
		receiverASyncIO :: !QASyncMessage !*[ReceiverStateHandle (PSt .l)] !(PSt .l) -> PSt .l
		receiverASyncIO event=:{qasmRecLoc={rlReceiverId},qasmMsg} rsHs pState=:{io=ioState}
			#! rsHs			= qMessage rlReceiverId qasmMsg rsHs
			#  ioState		= ioStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
			#  pState		= {pState & io=ioState}
			=  pState
		where
			qMessage :: !Id !SemiDynamic !*[ReceiverStateHandle .pst] -> *[ReceiverStateHandle .pst]
			qMessage rid msg [rsH=:{rHandle=rH}:rsHs]
				| receiverIdentified rid rH
					#! rH		= receiverAddASyncMessage rid msg rH
					=  [{rsH & rHandle=rH}:rsHs]
				| otherwise
					# rsHs		= qMessage rid msg rsHs
					= [rsH:rsHs]
			qMessage _ _ []
				=	[]

	receiverIO deviceEvent=:(ReceiverEvent (ASyncMessage event)) rsHs pState
		= (deviceEvent,letOneReceiverDoIO rl rsHs pState)
	where
		rl	= event.asmRecLoc
		
		letOneReceiverDoIO :: !RecLoc !*[ReceiverStateHandle (PSt .l)] !(PSt .l) -> PSt .l
		letOneReceiverDoIO {rlParentId} rsHs pState
			= pState2
		where
			dummy			= receiverdeviceFatalError "receiverIO (ReceiverEvent (ASyncMessage _))" "receiver could not be found"
			(_,rsH1,rsHs1)	= uremove (identifyReceiverStateHandle rlParentId) dummy rsHs
			pState1			= appPIO (ioStSetDevice receivers) pState
			(rsH2,pState2)	= letReceiverDoIO rsH1 pState1
			receivers		= ReceiverSystemState {rReceivers=rsHs1++[rsH2]}
			
			letReceiverDoIO :: !*(ReceiverStateHandle .pst) .pst -> (!*ReceiverStateHandle .pst,.pst)
			letReceiverDoIO rsH=:{rState=ls,rHandle=rH=:{rASMQ=[msg:tailQ],rFun}} pState
				# (ls,_,pState)	= rFun msg (ls,pState)
				= ({rState=ls,rHandle={rH & rASMQ=tailQ}},pState)
			letReceiverDoIO _ _
				= receiverdeviceFatalError "letReceiverDoIO" "message queue of target receiver is empty"

	receiverIO (ReceiverEvent (SyncMessage event)) rsHs pState
		# (lastProcess,pState)	= accPIO ioStLastInteraction pState
		# (event,pState)		= receiverSyncIO lastProcess event rsHs pState
		= (ReceiverEvent (SyncMessage event),pState)
	where
		receiverSyncIO :: !Bool !SyncMessage !*[ReceiverStateHandle (PSt .l)] !(PSt .l) -> (!SyncMessage,!PSt .l)
		receiverSyncIO lastProcess event rsHs pState
			| not found
				= (event1,pState2)
				with
					event1	= if lastProcess {event & smError=[ReceiverUnknown]} event
			| isEmpty error
				= ({event & smResp=resp},  pState2)
			| otherwise
				= ({event & smError=error},pState2)
		where
			pState1		= {pState & io=ioStSetDevice (ReceiverSystemState {rReceivers=rsHs1}) pState.io}
			(found,error,resp,rsHs1,pState2)
						= applyReceiverFunction event rsHs pState1
			
			applyReceiverFunction :: !SyncMessage !*[ReceiverStateHandle .pst] .pst -> (!Bool,[MessageError],[SemiDynamic],*[ReceiverStateHandle .pst],.pst)
			applyReceiverFunction event=:{smRecLoc={rlReceiverId}} [rsH=:{rState=ls,rHandle=rH}:rsHs] ps
				| not (receiverIdentified rlReceiverId rH)
					= (found,error,resp,[rsH:rsHs1],ps1)
					with
						(found,error,resp,rsHs1,ps1) = applyReceiverFunction event rsHs ps
				| enabled rH.rSelect
					= (True,[],resp,[{rState=ls1,rHandle=rH1}:rsHs],ps1)
					with
						(resp,rH1,(ls1,ps1))	= receiverHandleSyncMessage event rH (ls,ps)
				| otherwise
					= (True,[ReceiverUnable],[],[rsH:rsHs],ps)
			applyReceiverFunction _ [] ps
				= (False,[],[],[],ps)
	//	MW11..
	receiverIO deviceEvent=:(InternetEvent event) rsHs pState
		= (deviceEvent,letOneReceiverDoInetEvent event rsHs pState)
	// 	..MW11
	receiverIO _ _ _
		= receiverdeviceFatalError "receiverIO" "device event passed receiver event filter without handling"

// MW11..
letOneReceiverDoInetEvent (eventCode,endpointRef,inetReceiverCategory,misc) rsHs pState
	# (opt_rsH,rsHs)	= selectReceiver (endpointRef,inetReceiverCategory) rsHs
	  (nothing,opt_rsH)	= u_isNothing opt_rsH
	| nothing
/* PA: receiver device must be restored, because it is removed from the IOSt
		= pState		// No ioSetReceiverDevice needed, because nothing has been changed
*/		= appPIO (ioStSetDevice (ReceiverSystemState {rReceivers=rsHs})) pState
	# eventInfo			= (eventCode,endpointRef,misc)
	# rsH				= fromJust opt_rsH
	# (rSelect,emptyASMQ,inetRId,rsH)
						= (\rsH=:{rHandle=rH=:{rSelect,rASMQ}}->(rSelect,isEmpty rASMQ,getInetReceiverRId rH,rsH)) rsH
	| enabled rSelect && emptyASMQ
		 = applyInetEvent eventInfo rsH rsHs pState	// apply the event immediately
	// receiver is unable, so queue the event via asyncSend to handle it later
	# receivers			= ReceiverSystemState {rReceivers=[rsH:rsHs]} // left at the beginning
	  pState			= appPIO (ioStSetDevice receivers) pState
	# (sR,pState)		= asyncSend inetRId eventInfo pState
	| sR<>SendOk
		= abort "receiverdevice: I have a bug (78)"	
	| otherwise
		= pState
where
	selectReceiver :: !(!EndpointRef`,!InetReceiverCategory`) !*[ReceiverStateHandle .pst]
					   -> (!*Maybe (ReceiverStateHandle .pst),!*[ReceiverStateHandle .pst])
	selectReceiver receiverId=:(endpointRef,type) [rsH=:{rHandle={rInetInfo=Just (epr,tp,_,_)}}:rsHs]
		| endpointRef==epr && type==tp
			= (Just rsH,rsHs)
		| otherwise
			# (opt_rsH, rsHs) = selectReceiver receiverId rsHs
			= (opt_rsH,[rsH:rsHs])
	selectReceiver receiverId [rsH:rsHs]
		# (opt_rsH, rsHs) = selectReceiver receiverId rsHs
		= (opt_rsH,[rsH:rsHs])
	selectReceiver _ []
		= (Nothing,[])
	

applyInetEvent :: !InetReceiverASMQType !*(ReceiverStateHandle *(PSt .l)) *[ReceiverStateHandle *(PSt .l)] !*(PSt .l) -> PSt .l
applyInetEvent eventInfo rsH=:{rState,rHandle} rsHs pState
	= pState2
where
	pState1				= appPIO (ioStSetDevice receivers) pState
	(rState2,pState2)	= receiverApplyInetEvent eventInfo rHandle (rState,pState1)
	receivers			= ReceiverSystemState {rReceivers=[{rsH & rState=rState2}:rsHs]} // left at the beginning
// ..MW11

identifyReceiverStateHandle :: !Id !(ReceiverStateHandle .pst) -> *(!Bool,!ReceiverStateHandle .pst)
identifyReceiverStateHandle id rsH=:{rHandle={rId}}
	= (id==rId,rsH)
