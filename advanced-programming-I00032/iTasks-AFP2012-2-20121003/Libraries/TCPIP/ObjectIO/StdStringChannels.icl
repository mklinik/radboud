implementation module StdStringChannels

import	StdEnv
import	StdPSt, StdId, StdPStClass, StdReceiver
import	TCPChannelClass, TCPDef, TCPChannels, TCPEvent
import	receiverdefaccess

import TCPStringChannels, TCPStringChannelsInternal
import StdEventTCP

from	tcp import class ChannelEnv (channel_env_get_current_tick)

::	*StringChannelReceiver ls ps 	
	=	StringChannelReceiver
			(RId (ReceiveMsg String)) StringRChannel	
			(ReceiverFunction (ReceiveMsg String)		*(ls,ps))
			[ReceiverAttribute							*(ls,ps)]

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
		
getConnectedIds rAtts
	= case [ids \\ (ReceiverConnectedReceivers ids)<-rAtts] of
		[] -> []
		[h:_] -> h
