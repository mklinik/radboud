implementation module StdStringChannels

import	StdEnv
import	StdPSt, StdPStClass
import	StdChannels, StdTCPDef, StdTCPChannels, StdReceiver, StdEventTCP
import	receiverdefaccess

////////////////////// StringChannels to receive ////////////////////////////////////

::	*StringRChannel_ a	
	=	{	tcp_rchan		::	!TCP_RChannel
		,	readPhase		::	!ReadPhase
		,	receivedStrings	::	![String]
		,	maxSize			::	!Int
		}
::	*StringRChannel		:== StringRChannel_ String
::	*StringRChannels	=	StringRChannels *[StringRChannel]
::	*StringChannelReceiver ls ps 	
	=	StringChannelReceiver
			(RId (ReceiveMsg String)) StringRChannel	
			(ReceiverFunction (ReceiveMsg String)		*(ls,ps))
			[ReceiverAttribute							*(ls,ps)]

toStringRChannel	::	TCP_RChannel -> StringRChannel
toStringRChannel tcp_rchan
	= { tcp_rchan=tcp_rchan, readPhase=ReadingLength 0, receivedStrings=[], maxSize=0 }

//	a StringChannel has three states:
::	*ReadPhase	= ReadingLength !Int 				// the channel is currently reading the length of the next
													// string. The integer is the length that has been read
													// up until now.
				| ReadingString !*{#Char} !Int !Int // the channel is currently receiving the next string
													// For (ReadingString toFill filled s) it holds:
													//	 s==size toFill=="the size that the next receivable string will have"
													// filled characters already have been received and written into toFill
				| EndOfMessages						// the underlying tcp channel is eom or a string that is too big is received



addString	::	!(!String, !Int) !ReadPhase !Int -> (![String], !ReadPhase)
/*	addString (s,i) rp maxSize
		performs a state transition on the ReadPhase which reflects the receiving of (s % (i,(size s)-1))
*/
addString _ readPhase=:EndOfMessages _
	= ([], readPhase)
addString (string, stringBegin) readPhase=:(ReadingLength l) maxSize
	|	stringBegin>=size string
		= ([], readPhase)
	#!	chList	= [ string.[i] \\ i<-[stringBegin..(size string)-1] ]
		digits	= takeWhile isDigit chList
		newL	= seq (map transition digits) l
				// converts character list into an integer
	|	newL>maxSize && maxSize>0
		= ([], EndOfMessages)
	|	length digits < size string - stringBegin
		// the whole number has been read
		= addString (string, stringBegin+(length digits)+1) (ReadingString (createArray newL ' ') newL newL) maxSize
	// there are still some digits unread
	= ([], ReadingLength newL)
  where
	transition digit int
		= 10*int + (digitToInt digit)
addString (string, stringBegin) readPhase=:(ReadingString wholeString stillToReceive total) maxSize
	|	stringBegin>=size string
		= ([], readPhase)
	#!	readyBytes	= (size string) - stringBegin
	|	stillToReceive<=readyBytes
		// wholeString can be filled totally
		#!	wholeString	= strcpy 	wholeString	(total-stillToReceive)
									string 		stringBegin
									stillToReceive
			newBegin	= stringBegin + stillToReceive + 1
			(otherStrings, readPhase)	= addString (string, newBegin) (ReadingLength 0) maxSize
		= ([wholeString:otherStrings], readPhase)
	// wholeString can not be filled totally
	#!	wholeString	= strcpy 	wholeString	(total-stillToReceive)
								string		stringBegin
								readyBytes
	= ([], ReadingString wholeString (stillToReceive-readyBytes) total)

/*	For a StringChannelReceiver two receivers are opened: one tcp receiver and one "ordinary" receiver.
	The latter receives strings from the tcp receiver which contains the ReadPhase state.
*/
instance Receivers StringChannelReceiver where
	openReceiver ls (StringChannelReceiver id {tcp_rchan, readPhase, maxSize} callback attributes) pSt
		#!	(isEom, readPhase)	= isEOM readPhase
			(tcpRcvId, pSt)		= accPIO openId pSt
			(errReport, pSt)	= openReceiver	(id, tcpRcvId, readPhase)
												(TCP_Receiver tcpRcvId tcp_rchan tcpCallback []) pSt
		|	errReport<>NoError
			= (errReport, pSt)
// MW11 was		#!	connected = getConnected attributes
		#!	connected = getConnectedIds attributes
			(errReport, pSt)	= openReceiver ls (Receiver id (checkEOM id callback)
									[ReceiverConnectedReceivers [tcpRcvId: connected] :
									 attributes]) pSt
		|	not isEom
			= (errReport, pSt)
		#!	pSt	= snd (asyncSend id EOM pSt)
		= (errReport, pSt)
	  where
		isEOM EndOfMessages = (True, EndOfMessages)
		isEOM readphase		= (False, readphase)
		tcpCallback	(Received byteSeq) ((id, tcpRcvId, readPhase), pSt)
			#	(newStrings, readPhase)	= addString (toString byteSeq, 0) readPhase maxSize
				(isEom, readPhase)	= isEOM readPhase
			|	isEom
				= ((id, tcpRcvId, readPhase), snd (asyncSend id EOM pSt)) 
											// this will also close the tcp receiver in the end
			#!	pSt	= seq (map transition newStrings) pSt
			= ((id, tcpRcvId, readPhase), pSt)
		  where
		  	transition string pSt
			 	= snd (asyncSend id (Received string) pSt)
		tcpCallback EOM (ls=:(id,_,_),pSt)
			#!	(_, pSt)	= asyncSend id EOM pSt
			= (ls, pSt)
		checkEOM id callback EOM ls_pSt
			#!	(ls,pSt)	= callback EOM ls_pSt
				pSt			= appPIO (closeReceiver (rIdtoId id)) pSt
			= (ls, pSt)
		checkEOM id callback m ls_pSt
			= callback m ls_pSt
		
	getReceiverType _	= "StringChannelReceiver"
		
strcpy	::	!*{#Char} !Int !{#Char} !Int !Int -> *{#Char}
strcpy dest destBegin src srcBegin nrOfBytes
	|	nrOfBytes<=0
		= dest
	= strcpy { dest & [destBegin] = src.[srcBegin] } (inc destBegin) src (inc srcBegin) (dec nrOfBytes)


instance Receive StringRChannel_ where
	receive_MT mbTimeout rchan=:{receivedStrings, maxSize} env
		|	isJust mbTimeout && (fromJust mbTimeout)<0
			= (TR_Expired, Nothing, rchan, env)
		|	not (isEmpty receivedStrings)
			= (TR_Success, Just (Cast (hd receivedStrings)), { rchan & receivedStrings=tl receivedStrings }, env)
		#!	(timeBeforeReceive, env)	= getCurrentTick env
			(toReport, mbByteSeq, tcp_rchan, env)	= receive_MT mbTimeout rchan.tcp_rchan env
		|	toReport==TR_Expired
			= (toReport, Nothing, { rchan & tcp_rchan=tcp_rchan }, env)
		|	toReport==TR_NoSuccess
			= (toReport, Nothing, { rchan & tcp_rchan=tcp_rchan, readPhase=EndOfMessages }, env)
		#!	(newStrings, readPhase)	= addString (toString (fromJust mbByteSeq), 0) rchan.readPhase maxSize
		|	not (isEmpty newStrings)
			= (	TR_Success, Just (Cast (hd newStrings))
			  ,	{ rchan & tcp_rchan=tcp_rchan, readPhase=readPhase, receivedStrings=tl newStrings }
			  , env
			  )
		#!	(timeAfterReceive, env)	= getCurrentTick env
			usedTime		= tickDifference timeAfterReceive timeBeforeReceive
			newMbTimeout	= if (isNothing mbTimeout) mbTimeout (Just ((fromJust mbTimeout)-usedTime))
		= receive_MT newMbTimeout { rchan & tcp_rchan=tcp_rchan, readPhase=readPhase } env
			
	receiveUpTo max ch env
		= receiveUpToGeneral [] max ch env
	
	available rchan=:{maxSize} env
		|	not (isEmpty rchan.receivedStrings)
			= (True, rchan, env)
		#!	(isAvailable, tcp_rchan, env)	= available rchan.tcp_rchan env
		|	not isAvailable
			= (False, { rchan & tcp_rchan=tcp_rchan }, env)
		#!	(byteSeq, tcp_rchan, env)	= receive tcp_rchan env
			string	= toString byteSeq
			(newStrings, readPhase)	= addString (string, 0) rchan.readPhase maxSize
		= (	not (isEmpty newStrings)
		  ,	{ rchan & tcp_rchan=tcp_rchan, readPhase=readPhase, receivedStrings=newStrings }
		  ,	env
		  )
	
	eom rchan env
		|	not (isEmpty rchan.receivedStrings)
			= (False, rchan, env)
		#!	(isEom, rchan)	= isEOM rchan
		| isEom
			= (True, rchan, env)
		#!	(isTCPEom, tcp_rchan, env)	= eom rchan.tcp_rchan env
		|	isTCPEom
			= (isTCPEom, { rchan & tcp_rchan=tcp_rchan, readPhase=EndOfMessages }, env)
		= (isTCPEom, { rchan & tcp_rchan=tcp_rchan }, env)
	  where
		isEOM rchan=:{readPhase=EndOfMessages}	= (True, rchan)
		isEOM rchan								= (False, rchan)

receiveUpToGeneral akku max ch env
	|	max<=0
		= (u_reverse akku, ch, env)
	#!	(tReport, mbData, ch, env)	= receive_MT (Just 0) ch env
	|	tReport<>TR_Success
		= (u_reverse akku, ch, env)
	= receiveUpToGeneral [fromJust mbData:akku] (dec max) ch env

instance closeRChannel			StringRChannel_
  where
	closeRChannel {tcp_rchan} env
		= closeRChannel tcp_rchan env


instance MaxSize			StringRChannel_
  where
	setMaxSize newMaxSize ch=:{readPhase=ReadingString _ _ currentStringSize, maxSize}
		|	currentStringSize>maxSize && maxSize>0
			= { ch & readPhase=EndOfMessages, maxSize=max newMaxSize 0}
	setMaxSize newMaxSize ch=:{maxSize}
		= { ch & maxSize=max newMaxSize 0 }
	getMaxSize ch=:{maxSize}
		= (maxSize, ch)
	clearMaxSize ch
		= { ch & maxSize=0 }
	
////////////////////// StringChannels to send ////////////////////////////////////

::	*StringSChannel_ a	=	StringSChannel_ TCP_SChannel
::	*StringSChannel		:== StringSChannel_ String
::	*StringSChannels	=	StringSChannels *[StringSChannel]

toStringSChannel	::	TCP_SChannel -> StringSChannel
toStringSChannel tcp_schan
	= StringSChannel_ tcp_schan

stringToByteSeq	::	!String -> ByteSeq
stringToByteSeq string
	= toByteSeq (toString (size string)+++" "+++string+++NL13)

NL13 :== "\xD"	// carriage return	

instance Send				StringSChannel_
  where
	send_MT mbTimeout stringPolymorph ch env
		= nsend_MT mbTimeout [stringPolymorph] ch env
	nsend_MT mbTimeout stringListPolymorph (StringSChannel_ tcp_schan) env
		#!	(toReport, sentBytes, tcp_schan, env)
				= nsend_MT mbTimeout (map stringToByteSeq (CastToStringList stringListPolymorph)) tcp_schan env
		= (toReport, sentBytes, StringSChannel_ tcp_schan, env)
	flushBuffer_MT mbTimeout (StringSChannel_ tcp_schan) env
		#!	(tReport, sentBytes, tcp_schan, env)
				= flushBuffer_MT mbTimeout tcp_schan env
		= (tReport, sentBytes, StringSChannel_ tcp_schan, env)
	closeChannel_MT mbTimeout (StringSChannel_ tcp_schan) env
		= closeChannel_MT mbTimeout tcp_schan env
	abortConnection (StringSChannel_ tcp_schan) env
		= abortConnection tcp_schan env
	
	disconnected (StringSChannel_ tcp_schan) env
		#!	(isDisconnected, tcp_schan, env)
				= disconnected tcp_schan env
		= (isDisconnected, StringSChannel_ tcp_schan, env)

	bufferSize (StringSChannel_ tcp_schan)
		#!	(buffSize, tcp_schan)
				= bufferSize tcp_schan
		= (buffSize, StringSChannel_ tcp_schan)

/////////////////////////////////////////////////////////////////////////////////

// for openSendNotifier, closeSendNotifier
instance accSChannel 		StringSChannel_
  where
	accSChannel f (StringSChannel_ tcp_schan)
		#!	(x, tcp_schan)	= f tcp_schan
		= (x, (StringSChannel_ tcp_schan))

// for selectChannel
instance SelectSend		StringSChannels
  where
	accSChannels f (StringSChannels channels)
		= accSChannelsA f channels []
	  where
		accSChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, StringSChannels channels)
		accSChannelsA f [StringSChannel_ tcp_schan:channels] akku
			#!	(x, tcp_schan)	= f tcp_schan
			= accSChannelsA f channels [(x,StringSChannel_ tcp_schan): akku]
	appDisconnected n (StringSChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(isDisconnected, channel, env)	= disconnected channel env
		= (isDisconnected, StringSChannels (l ++ [channel:r]), env)

instance SelectReceive			StringRChannels
  where
	accRChannels f (StringRChannels channels)
		= accRChannelsA f channels []
	  where
		accRChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, StringRChannels channels)
		accRChannelsA f [chan=:{tcp_rchan}:channels] akku
			#!	(x, TCP_RCHANNEL tcp_rchan)	= f (TCP_RCHANNEL tcp_rchan)
			= accRChannelsA f channels [(x,{ chan & tcp_rchan=tcp_rchan}): akku]
	getRState n (StringRChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(state, channel, env)	= getState channel env
		= (state, StringRChannels (l ++ [channel:r]), env)
	  where
		getState channel env
			#!	(isAvailable, channel, env) = available channel env
			|	isAvailable
				= (Just SR_Available, channel, env)
			#!	(isEom, channel, env) = eom channel env
			|	isEom
				= (Just SR_EOM, channel, env)
			= (Nothing, channel, env)

instance getNrOfChannels 	StringRChannels
  where
	getNrOfChannels (StringRChannels channels)
		#!	(n, channels)	= u_length channels
		= (n, StringRChannels channels)

u_reverse::![.a] -> [.a]
u_reverse list = reverse_ list []
where 
	reverse_ [hd:tl] list	= reverse_ tl [hd:list]
	reverse_ [] list		= list

u_length l
	= u_length_ l [] 0
  where
	u_length_ [] akku n
		= (n, u_reverse akku)
	u_length_ [h:t] akku n
		= u_length_ t [h:akku] (inc n)

CastToStringList	::	!.a -> [String]
CastToStringList x 
	= Cast x

Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}

// MW11..
getConnectedIds rAtts
	# l = [ids \\ (ReceiverConnectedReceivers ids)<-rAtts]
	| isEmpty l
		= []
	= hd l
// .. MW11
