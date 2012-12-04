implementation module StdEventTCP

import StdEnv
import TCPChannelClass, TCPDef, TCPChannels,StdTCPDef
import StdReceiver
import StdId, StdPSt, StdPStClass
import tcp, ostcp, ostcptoolbox , tcp_bytestreams
import commondef, receiverid, receiverdevice, receiverdefaccess, receiveraccess

import TCPEvent

/*	PSt is an environment instance of the class ChannelEnv	*/

instance ChannelEnv (PSt .l) where
	channelEnvKind env
		= (PST, env)
	mb_close_inet_receiver_without_id reallyDoIt id_pair pSt=:{io}
		= { pSt & io = mb_close_inet_receiver_without_id True id_pair io }
	channel_env_get_current_tick pst
		= accPIO channel_env_get_current_tick pst

/*	IOSt is an environment instance of the class ChannelEnv	*/

instance ChannelEnv (IOSt .l) where
	channelEnvKind env
		= (IOST, env)
	mb_close_inet_receiver_without_id False _ ioState
		= ioState
	mb_close_inet_receiver_without_id True id_pair ioState
		#! (closed,ioState)			= ioStClosed ioState
		| closed
			= ioState
		# (found,receivers,ioState)	= ioStGetDevice ReceiverDevice ioState
		| not found			// PA: guard added
			= ioState
		# rsHs						= (receiverSystemStateGetReceiverHandles receivers).rReceivers
		  (found,rsH,rsHs)			= uremove (inetReceiverStateIdentified1 id_pair) undef rsHs
		# ioState					= ioStSetDevice (ReceiverSystemState {rReceivers=rsHs}) ioState
		| not found
			= ioState
		| otherwise
			# {rId=id,rConnected=connectedIds,rInetInfo=inetInfo}
									= rsH.rHandle
			# (idtable,ioState)		= ioStGetIdTable ioState
			# ioState				= ioStSetIdTable (snd (removeIdFromIdTable id idtable)) ioState
			# ioState				= unbindRId id ioState
			# ioState				= ioStSetRcvDisabled True ioState // MW11++
			# ioState				= seq (map closeReceiver connectedIds) ioState
			  (_,_,_,closeFun)		= fromJust inetInfo
			# ioState				= appIOToolbox closeFun ioState
			= ioState
	where
		inetReceiverStateIdentified1 :: !(!EndpointRef`, !InetReceiverCategory`) !(ReceiverStateHandle .ps) -> (!Bool,!ReceiverStateHandle .ps)
		inetReceiverStateIdentified1 x rsH=:{rHandle} = (inetReceiverIdentified x rHandle,rsH)
	channel_env_get_current_tick io
		# (world,io)	= ioStGetWorld io
//		  (tick,world)	= getCurrentTick world
		  (tick,world)	= tcp_getcurrenttick world
		= (tick, ioStSetWorld world io)

::	CharRcvMessage x	= Message x | Dummy
::	*CharRcvLocalState ls l p
	= 	{	c_ls			::	ls					// local state where "callback" works on
		,	dummyPending	::	!Bool				// whether a Dummy message is pending in the message queue
		,	eomHappened		::	!Bool				// whether (Message EOM) was received
		,	strings			::	![String]			// the buffer for all received strings: 
													// It holds: and (map (\str -> (size str)>0) strings)
		,	allocBytes		::	!Int				// ==sum (map size strings)
		,	index			::	!Int				// (hd strings).[index] is the next character to apply to callback
													// It holds: 0<=index<size (hd strings) || (isEmpty strings && index==0)
		,	mbMaxIterations	::	!Maybe Int			// as specified in TCP_CharReceiver
		,	callback		::	ReceiverFunction (ReceiveMsg Char) *(ls,PSt l) // dito
		,	charRcvRId		::	RId (CharRcvMessage (ReceiveMsg String)) // the id of the "ordinary" receiver
		,	tcpRcvId		::	!Id					// the id of the tcp receiver
		,	tcpRcvDisabled	::	!Bool				// whether the tcp receiver is disabled
		}

instance Receivers TCP_ListenerReceiver where
	openReceiver ls (TCP_ListenerReceiver id tcp_listener f rAttributes) pSt
		# endpointRef		= unpack_tcplistener tcp_listener
		= open_RChan_or_Listener close_listener ls id rAttributes endpointRef 0 (handleConnectRequest f) ListenerReceiver pSt
	where
		handleConnectRequest f (IE_CONNECTREQUEST,endpointRef,_) (ls,ps=:{io})
			# (_, mbHostDuplexChan,_,io) = receive_MT (Just 0) (pack_tcplistener endpointRef) io
			  (isNothin`,mbHostDuplexChan) = u_isNothing mbHostDuplexChan
			| isNothin`
				= (ls,{ ps & io=io})
			= f (Received (fromJust mbHostDuplexChan)) (ls,{ ps & io=io})

	getReceiverType _
		= "TCP_ListenerReceiver"

instance Receivers TCP_Receiver where
	openReceiver ls (TCP_Receiver id tcp_RChan f rAttributes) pSt
		# (endpointRef,maxSize)		= unpack_tcprchan tcp_RChan
		= open_RChan_or_Listener close_tcprchan ls id rAttributes endpointRef maxSize (handleReceiveEvent id maxSize f) RChanReceiver pSt
				
	getReceiverType _
		= "TCP_Receiver"

/*	about the TCP_CharReceiver

	Two receivers are opened: One TCP_Receiver and one ordinary receiver. The tcp receiver will
	send strings to the ordinary receiver, which will apply the callback function of the TCP_CharReceiver
	definition. The ordinary receiver will buffer the strings in a list. It will send to itself a dummy
	message, to continue in applying repeatedly the callback function. It will en/disable the tcp receiver
	for flow control.

	The two callback functions for the receivers are tcpCallback and charCallback 
*/

instance Receivers TCP_CharReceiver where
	openReceiver ls (TCP_CharReceiver charRcvId tcp_rchan mbNrIterations callback attributes) pSt
		#!	(tcpRcvId, pSt)		= accPIO openId pSt
			selectState			= getSelectState attributes
			(errReport, pSt)	= openReceiver	(USt charRcvRId)
												(TCP_Receiver tcpRcvId (clearMaxSize tcp_rchan) tcpCallback 
															[ReceiverSelectState selectState]) pSt
		|	errReport<>NoError
			= (errReport, pSt)
		#!	connected = getConnectedIds attributes // MW11 was getConnected
		= openReceiver 	(initialState tcpRcvId)
						(Receiver	charRcvRId charCallback
									[	ReceiverConnectedReceivers [tcpRcvId: connected]
									,   ReceiverSelectState selectState]) pSt
	  where
		charRcvRId		=	toRId (fromId charRcvId)
		initialState tcpRcvId	=	{	c_ls			= ls
									,	dummyPending	= False
									,	eomHappened		= False
									,	strings			= []
									,	allocBytes		= 0
									,	index			= 0
									,	mbMaxIterations	= mbNrIterations
									,	callback		= callback
									,	charRcvRId		= charRcvRId
									,	tcpRcvId		= tcpRcvId
									,	tcpRcvDisabled	= False
									}
		
		tcpCallback (Received byteSeq) (USt charRcvId, pSt)
			|	byteSeqSize byteSeq==0
				= (USt charRcvId, pSt)
			#!	(_, pSt)	= asyncSend charRcvId (Message (Received (toString byteSeq))) pSt
			= (USt charRcvId, pSt)
		tcpCallback EOM (USt charRcvId, pSt)
			#!	(_, pSt)	= asyncSend charRcvId (Message EOM) pSt
			= (USt charRcvId, pSt)
	
	getReceiverType _
		= "TCP_CharReceiver"

open_RChan_or_Listener :: !.(EndpointRef -> *OSToolbox -> *OSToolbox) .ls !Id ![ReceiverAttribute .st] !EndpointRef !Int !(ReceiverFunction m *(.ls,PSt .l)) !Int 
							(PSt .l) -> (!ErrorReport, !PSt .l)
open_RChan_or_Listener closeFun ls id rAttributes endpointRef maxSize callbackFun receiverType pSt
	# ((referenceCount, _, hasSendableNotifier, aborted), pSt)
	  					= getEndpointDataC endpointRef pSt
	# (errReport, pSt)	= openReceiverGeneral
									(newInetStateHandle ls callbackFun maxSize (closeFun endpointRef))
									id rAttributes endpointRef receiverType
									pSt
	| errReport<>NoError
		= (errReport, pSt)
	# pSt				= os_select_inetevents endpointRef receiverType referenceCount True hasSendableNotifier aborted pSt
	| receiverType<>RChanReceiver
	  	= (errReport, pSt)
	// send an EOM if the TCP receive channel is closed
	# (isEom, pSt)	= os_eom endpointRef pSt
	| not isEom
	  	= (errReport, pSt)
	# (_,pSt)	= asyncSend (toReceiveEventRId id) EOM pSt
	= (errReport, pSt)
where
	toReceiveEventRId :: Id -> RId (ReceiveMsg ByteSeq)
	toReceiveEventRId id
		= toRId (fromId id) // dangerous stuff

handleReceiveEvent :: !Id !Int ((ReceiveMsg ByteSeq) -> *(.ls, PSt .a) -> *(.ls, PSt .a)) (!InetEvent, !EndpointRef, !Int) *(.ls, PSt .a) -> *(.ls, PSt .a)
handleReceiveEvent _ maxSize f (IE_RECEIVED,endpointRef,_) (ls,ps=:{io})
	# (_,mbTCPData,_,io) = receive_MT (Just 0) (pack_tcprchan (endpointRef,maxSize)) io
	| isNothing mbTCPData
		= (ls,{ps & io=io})
	= f (Received (fromJust mbTCPData)) (ls,{ps & io=io})
handleReceiveEvent id maxSize f event=:(IE_EOM,endpointRef,misc) (ls,pSt)
	// check, whether there is still some data ready to be read. (Can happen on the Mac, since events don't
	// necessarily keep their order (reentrancy))
	# (dataAvailable, pSt)	= data_availableC endpointRef pSt
	| not dataAvailable
		# (ls, pSt=:{io})	= f EOM (ls,pSt)
		  io				= closeReceiver id io
		= (ls, { pSt & io=io })
	# (ls,pSt)				= handleReceiveEvent id maxSize f (IE_RECEIVED,endpointRef,misc) (ls, pSt)
	= handleReceiveEvent id maxSize f event (ls,pSt)

THRESHOLD1	:==	10000								// if allocBytes>THRESHOLD1 then the tcp receiver will be disabled
THRESHOLD2	:== THRESHOLD1/2						// if allocBytes<THRESHOLD2 && tcpRcvDisabled then the tcp receiver
													// will be enabled again

charCallback:: !(CharRcvMessage (ReceiveMsg String)) !*(!*CharRcvLocalState .ls .l .p,!*PSt .l)
			 -> *(!*CharRcvLocalState .ls .l .p,!PSt .l)
charCallback Dummy (state, pSt)
	= applyCharCBF { state & dummyPending=False } pSt
charCallback (Message (Received string)) (state, pSt)
	#!	newAllocBytes	= state.allocBytes+(size string)
		state	= { state & strings=state.strings++[string], allocBytes=newAllocBytes}
	|	newAllocBytes>THRESHOLD1
		= applyCharCBF { state & tcpRcvDisabled=True } (appPIO (disableReceivers [state.tcpRcvId]) pSt)
	|	newAllocBytes<THRESHOLD2 && state.tcpRcvDisabled
		= applyCharCBF { state & tcpRcvDisabled=False } (appPIO (enableReceivers [state.tcpRcvId]) pSt)
	= applyCharCBF state pSt
charCallback (Message EOM) (state=:{strings}, pSt)
	= applyCharCBF { state & eomHappened=True } pSt

applyCharCBF :: !*(CharRcvLocalState .a .l .c) !(PSt .l) -> *(!*CharRcvLocalState .a .l .c,!PSt .l)
applyCharCBF state=:{dummyPending, strings, index, mbMaxIterations, callback, charRcvRId, c_ls} pSt
	|	isEmpty strings
		=  checkEOM2 state pSt
	#!	string			= hd strings
		nrCharsInString	= (size string) - index
		nrCharsToApply	= case mbMaxIterations of
							(Just maxIterations)	-> min nrCharsInString maxIterations
							_						-> nrCharsInString
	#!	pSt				= appPIO (ioStSetRcvDisabled False) pSt
						// set a flag  in the ioState to False
		(newIndex, (c_ls, pSt))
						= loop (rIdtoId charRcvRId) callback nrCharsToApply index string (c_ls, pSt)
						// apply the callback function nrCharsToApply characters, this can possibly not succeed,
						// because the callback function closed/disabled this receiver 
		nrCharsApplied	= newIndex-index
						// the number of characters that WERE applied
	|	nrCharsApplied==nrCharsInString && isEmpty (tl strings)
		= checkEOM2 { state & c_ls=c_ls, strings=[], allocBytes=0, index=0 } pSt
	// there has to be a Dummy in the receivers queue, because there is still some data left in the local state
	#!	pSt	= case dummyPending of
				False	-> snd (asyncSend charRcvRId Dummy pSt)
				_		-> pSt
		state	= { state & c_ls=c_ls, dummyPending=True }
	|	nrCharsApplied<nrCharsInString
		// not all characters in string were handled
		= ({ state & index=newIndex }, pSt)
	// it holds: nrCharsApplied==nrCharsInString && (NOT (isEmpty (tl strings)))
	// "the buffer "strings" has not been emptied
	= ({ state & strings=tl strings, allocBytes=state.allocBytes-(size string), index=0 }, pSt)
  where
	loop :: !Id !(ReceiverFunction (ReceiveMsg Char) (.ls,!PSt .l)) !Int !Int !String !(.ls,!PSt .l)
		 -> (!Int, !(.ls,!PSt .l))
	loop charRcvId callback nrCharsToApply index string ls_pSt=:(ls, pSt=:{io})
		// applies string.[index] .. string.[index+nrCharsToApply-1] to the callback function
		|	nrCharsToApply==0
			= (index, ls_pSt)
		#!	(receiverPossiblyDisabled, io)	= ioStGetRcvDisabled io
		//	this flag is set, when an arbitrary receiver is closed or disabled or when closeProcess happend
		|	not receiverPossiblyDisabled
			= loop	charRcvId callback (dec nrCharsToApply) (inc index) string 
					(callback (Received string.[index]) (ls, { pSt & io=io }))
		// it is possible, that the receiver was disabled/closed. Check this
		#!	io	= ioStSetRcvDisabled False io
			(mbSelectState, io)	= getReceiverSelectState charRcvId io
		|	isJust mbSelectState && (fromJust mbSelectState)==Able
			= loop	charRcvId callback (dec nrCharsToApply) (inc index) string 
					(callback (Received string.[index]) (ls, { pSt & io=io }))
		= (index, (ls, { pSt & io=io }))
	
	checkEOM2 state=:{eomHappened, callback, charRcvRId} pSt
		// when all bytes in the buffer "strings" have been applied to the callback function, then
		// this function is called to possibly apply EOM to the callback function
		|	not eomHappened
			= (state, pSt)
		#!	(c_ls, pSt)	= callback EOM (state.c_ls, pSt)
		= ({ state & c_ls=c_ls }, appPIO (closeReceiver (rIdtoId charRcvRId)) pSt)

openSendNotifier :: .ls !(SendNotifier *(ch .a) .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!*(ch .a),!PSt .l) | accSChannel ch & Send ch
openSendNotifier ls (SendNotifier sChan f rAttributes) pSt
	#!	(chan, sChan)	= accSChannel getChan sChan
		((referenceCount, hasReceiveNotifier, hasSendableNotifier, aborted), pSt)
	  					= getEndpointDataC chan.bEndpointRef pSt
	|	hasSendableNotifier
		=	(ErrorNotifierOpen, sChan, pSt)
	# (chan, sChan, pSt) = open_channel_id chan sChan pSt
	  channel_id = toId chan.bId
	#	(errReport,pSt)	= openReceiverGeneral
							(	newInetStateHandle ls (handleSendableEvent channel_id f) 0
									(close_tcpschan_receiver chan.bEndpointRef)
							)
							channel_id rAttributes chan.bEndpointRef SChanReceiver
							pSt
	| errReport<>NoError
		= (errReport, sChan, pSt)
	#!	pSt				= os_select_inetevents chan.bEndpointRef SChanReceiver referenceCount hasReceiveNotifier True aborted pSt
	#!	(isDisconnected, sChan, pSt)
						= disconnected sChan pSt
	|	not isDisconnected
		= (errReport, sChan, pSt)
	#!	(_,pSt)	= asyncSend (toSendEventRId channel_id) Disconnected pSt
	= (errReport, sChan, pSt)
  where
	toSendEventRId :: Id -> RId SendEvent
	toSendEventRId id
		= toRId (fromId id)  // dangerous stuff

	// bId is initialized with Id 0 in 
	open_channel_id :: Buffered_SChan *(ch .a) *(PSt .l) -> *(Buffered_SChan,*(ch .a),*(PSt .l)) | accSChannel ch
	open_channel_id chan sChan pSt
		| chan.bId<>0
			= (chan, sChan, pSt)
			# (channel_id, pSt) = openId pSt
			# (chan, sChan)	= accSChannel (update_channel_id channel_id) sChan
			= (chan, sChan, pSt)

	update_channel_id :: !Id !(TCP_SChannel_ .a) -> (Buffered_SChan,TCP_SChannel_ .a);
	update_channel_id channel_id tcp_schan
		# chan = unpack_tcpschan tcp_schan
		  chan = {chan & bId=fromId channel_id}
		= (chan, pack_tcpschan chan)

getChan :: !(TCP_SChannel_ .a) -> (Buffered_SChan,TCP_SChannel_ .a);
getChan tcp_schan
		#!	chan	= unpack_tcpschan tcp_schan
		= (chan, pack_tcpschan chan)

openReceiverGeneral :: .(Id -> .(SelectState -> .([Id] -> .(.a -> .(.b -> ReceiverStateHandle (PSt .l))))))
						!Id [ReceiverAttribute .st] .a .b !(PSt .l)
					->	(ErrorReport,!PSt .l)
openReceiverGeneral createStateHandleFunc id rAttributes endpointRef isReceiver pState
	# pState					= appPIO (appIOToolbox OSinstallTCP) pState
	# (pState=:{io=ioState})	= receiverFunctions.dOpen pState // MW11++
	# (rt,ioState)				= ioStGetReceiverTable ioState
	  (maybe_parent,rt)			= getReceiverTableEntry id rt
	# ioState					= ioStSetReceiverTable rt ioState	// PA++
	| isJust maybe_parent
	= (ErrorIdsInUse, { pState & io=ioState })
	# (found,receivers,ioState)	= ioStGetDevice ReceiverDevice ioState
	| not found					// PA: condition should never occur as ReceiverDevice has just been 'installed'
	= fatalError "openReceiverGeneral" "StdEventTCP" "could not retrieve ReceiverSystemState from IOSt"
	# rsHs						= (receiverSystemStateGetReceiverHandles receivers).rReceivers
	  rsH						= createStateHandleFunc id select closeAlso endpointRef isReceiver 
	  ioState					= ioStSetDevice (ReceiverSystemState {rReceivers=[rsH:rsHs]}) ioState
	  ioState					= bindRId id select id ReceiverDevice ioState
	= (NoError, { pState & io=ioState })
where
	select					= getSelectState rAttributes
// MW11 was	closeAlso				= getConnected rAttributes
	closeAlso				= getConnectedIds rAttributes

getConnectedIds rAtts
	= case [ids \\ (ReceiverConnectedReceivers ids)<-rAtts] of
		[] -> []
		[h:_] -> h

getSelectState rAtts
	= case [selectState \\ (ReceiverSelectState selectState)<-rAtts] of
		[] -> Able
		[h:_] -> h

::	*USt x = USt x

newInetStateHandle :: .ls (ReceiverFunction m *(.ls,.pst)) !Int !(IdFun *OSToolbox) !Id !SelectState ![Id] !EndpointRef` !InetReceiverCategory`
	-> *ReceiverStateHandle .pst
newInetStateHandle ls rFun maxSize closeFun id select connectedIds endpointRef isReceiver
	= {	rState	=	USt ls
	  ,	rHandle	=	{	rId			= id
					  ,	rASMQ		= []
					  ,	rSelect		= Able
					  ,	rOneWay		= True
					  ,	rFun		= onewaytotriple (wrapUSt rFun)
					  , rInetInfo	= Just (endpointRef, isReceiver, maxSize, closeFun)
				  	  , rConnected	= connectedIds
					}
	  }
where
	wrapUSt rFun msg (USt ls,pst)
		# (ls,pst) = rFun msg (ls,pst)
		= (USt ls,pst)

handleSendableEvent :: !Id (SendEvent -> *(.ls, PSt .l) -> *(.ls, PSt .l)) (!InetEvent, !EndpointRef, !Int) *(.ls, PSt .l) -> *(.ls, PSt .l)
handleSendableEvent	_ f (IE_SENDABLE,_,_) (ls,ps)
	= f Sendable (ls,ps)
handleSendableEvent	id f (IE_DISCONNECTED,_,_) (ls,ps)
	# (ls,ps=:{io})	= f Disconnected (ls,ps)
	  io			= closeReceiver id io
	= (ls, { ps & io=io } )

close_tcpschan_receiver :: !EndpointRef !*OSToolbox	-> *OSToolbox
close_tcpschan_receiver endpointRef tb
	# ((referenceCount,hr,_,aborted),tb)	= getEndpointDataC endpointRef tb
	  tb							= setEndpointDataC endpointRef referenceCount hr False aborted tb
	= tb

closeSendNotifier :: !*(ch .a) !(IOSt .l) -> (!*(ch .a), !IOSt .l) | accSChannel ch
closeSendNotifier chan io
	#!	(ch,chan)	= accSChannel getChan chan 
		((referenceCount, hasReceiveNotifier, hasSendableNotifier, aborted), io)
	  						= getEndpointDataC ch.bEndpointRef io
	|	not hasSendableNotifier
	= (chan, io)
	#!	io					= closeReceiver (toId ch.bId) io
		io					= setEndpointDataC ch.bEndpointRef referenceCount hasReceiveNotifier False aborted io
	= (chan, io)

lookupIPAddress_async :: !String !(InetLookupFunction (PSt .l)) !(PSt .l) -> PSt .l
lookupIPAddress_async inetAddr lookupFunction pSt
	# pSt							= appPIO (appIOToolbox OSinstallTCP) pSt		// PA: added
	# ((errCode,endpointRef),pSt)	= lookupHost_asyncC (inetAddr+++"\0") pSt
	| errCode<>0
		= lookupFunction Nothing pSt
	# (recId, pSt)		= accPIO openId pSt
	  (_,pSt)			= openReceiverGeneral
										(newInetStateHandle recId (handleDNREvent lookupFunction) 0 id)
										recId [ReceiverSelectState Able] endpointRef DNSReceiver
										pSt
	= pSt

handleDNREvent :: ((Maybe IPAddress) -> (PSt .a) -> PSt .a) (!InetEvent, !EndpointRef, !Int) (Id, PSt .a) -> (Id, PSt .a)
handleDNREvent f (IE_IPADDRESSFOUND,_,ipAddr) (recId,ps=:{io})
	#  io				= closeReceiver recId io
	= (recId, f (Just (pack_ipaddr ipAddr)) { ps & io=io })
handleDNREvent f (IE_IPADDRESSNOTFOUND,_,_) (recId,ps=:{io})
	#  io				= closeReceiver recId io
	= (recId, f Nothing { ps & io=io } )

connectTCP_async :: !(!IPAddress,!Port) !(InetConnectFunction (PSt .l)) !(PSt .l) -> PSt .l
connectTCP_async (inetHost,inetPort) callback pSt
	# pSt								= appPIO (appIOToolbox OSinstallTCP) pSt		// PA: added
	# destination						= (unpack_ipaddr inetHost, inetPort)
	  ((errCode,endpointRef), pSt)		= os_connectTCP_async PST destination pSt
	| errCode<>0
		= callback Nothing pSt
	# (recId, pSt)		= accPIO openId pSt
	  (_,pSt)			= openReceiverGeneral
											(newInetStateHandle recId (handleConnectEvent callback) 0 id)
											recId [ReceiverSelectState Able] endpointRef ConnectReceiver
											pSt
	= pSt

handleConnectEvent :: ((Maybe TCP_DuplexChannel) -> *((PSt .a) -> PSt .a)) (!InetEvent, !EndpointRef, !Int) (Id, PSt .a) -> (Id, !PSt .a)
handleConnectEvent f (IE_ASYNCCONNECTCOMPLETE,endpointRef,_) (recId,ps)
	# io				= ps.io
	  io				= appIOToolbox (setEndpointDataC endpointRef 2 False False False) io
	  (duplexChan, io)	= createDuplexChan endpointRef io
	  io				= closeReceiver recId io
	= (recId, f (Just duplexChan) { ps & io=io })
handleConnectEvent f (IE_ASYNCCONNECTFAILED,endpointRef,_) (recId,ps)
	# io				= ps.io
	  io				= appIOToolbox (setEndpointDataC endpointRef 0 False False False) io
	  io				= appIOToolbox (garbageCollectEndpointC endpointRef) io
	  io				= closeReceiver recId io
	= (recId, f Nothing { ps & io=io })

createDuplexChan endpointRef env
	# (id, env)	= openId env
	  tcp_SChan	=	{	bEndpointRef	=	endpointRef
					,	bNormal			=	emptyBuffer
					,	bUnsent			=	0
					,	bId				=	fromId id
					}
	= ({sChannel= pack_tcpschan tcp_SChan, rChannel=pack_tcprchan (endpointRef,0)}, env)
  where
	emptyBuffer = {bPackets=[], bBegin=0}
