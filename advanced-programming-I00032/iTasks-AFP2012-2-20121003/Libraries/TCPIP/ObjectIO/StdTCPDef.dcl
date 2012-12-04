definition module StdTCPDef

from	StdMaybe		import :: Maybe

import TCPDef

import	StdReceiverDef
from id import ::Id

from	TCPChannelClass	import :: DuplexChannel
import StdChannels
from	tcp				import :: IPAddress, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_

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
