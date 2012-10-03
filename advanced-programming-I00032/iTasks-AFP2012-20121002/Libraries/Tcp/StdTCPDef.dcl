definition module StdTCPDef

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdTCPDef provides basic definitions for using TCP.
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdReceiverDef
from	StdMaybe		import :: Maybe
from	StdOverloaded	import class ==, class toString
from	StdChannels		import :: SendEvent, :: ReceiveMsg, :: DuplexChannel
from	tcp				import :: IPAddress, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_


::	*TCP_SChannel		:==	TCP_SChannel_ ByteSeq
::	*TCP_RChannel		:==	TCP_RChannel_ ByteSeq
::	*TCP_Listener		:== TCP_Listener_ *(IPAddress, TCP_DuplexChannel)

::	Port				:== Int

::	*TCP_DuplexChannel	:== DuplexChannel *TCP_SChannel_ *TCP_RChannel_ ByteSeq

::	ByteSeq
//	A sequence of bytes

instance toString	ByteSeq
instance ==			ByteSeq
toByteSeq		::	!x			-> ByteSeq	| toString x
byteSeqSize		::	!ByteSeq	-> Int
//	byteSeqSize returns the size in bytes

instance toString IPAddress	
//	returns ip address in dotted decimal form


//	********************************************************************************
//	for event driven processing
//	********************************************************************************

//	To receive byte sequences
::	*TCP_Receiver ls pst
 =	 TCP_Receiver
		Id TCP_RChannel	
		(ReceiverFunction (ReceiveMsg ByteSeq) *(ls,pst))
		[ReceiverAttribute                     *(ls,pst)]

::	SendNotifier sChannel ls pst
 =	SendNotifier
		sChannel
		(ReceiverFunction SendEvent *(ls,pst))
		[ReceiverAttribute          *(ls,pst)]

//	To accept new connections
::	*TCP_ListenerReceiver ls pst	
 =	 TCP_ListenerReceiver
		Id TCP_Listener	
		(*(ReceiveMsg *(IPAddress,TCP_DuplexChannel)) -> *(*(ls,pst) -> *(ls,pst)))
		[ReceiverAttribute *(ls,pst)]

//	To receive characters
::	*TCP_CharReceiver ls pst	
 =	 TCP_CharReceiver
		Id TCP_RChannel (Maybe NrOfIterations)
		(ReceiverFunction (ReceiveMsg Char) *(ls,pst))
		[ReceiverAttribute                  *(ls,pst)]

/*	For efficency the receiver function of a TCP_CharReceiver will be called from
	a loop. Within this loop no other events can be handled. The NrOfIterations
	parameter limits the maximum number of iterations.
*/
::	NrOfIterations			:== Int
::	InetLookupFunction  st	:== (Maybe IPAddress)          -> st -> st
::	InetConnectFunction st 	:== *(Maybe TCP_DuplexChannel) -> *(st -> st)

//	********************************************************************************
//	for multiplexing
//	********************************************************************************

:: *TCP_RChannels = TCP_RChannels *[TCP_RChannel]	
:: *TCP_SChannels = TCP_SChannels *[TCP_SChannel]
:: *TCP_Listeners = TCP_Listeners *[TCP_Listener]

::	*PrimitiveRChannel
 =	TCP_RCHANNEL TCP_RChannel
 |	TCP_LISTENER TCP_Listener

::	SelectResult
 =	SR_Available
 |	SR_EOM
 |	SR_Sendable
 |	SR_Disconnected
