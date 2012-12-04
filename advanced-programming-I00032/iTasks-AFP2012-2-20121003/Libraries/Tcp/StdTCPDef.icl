implementation module StdTCPDef

import	StdEnv
import	StdReceiverDef, StdMaybe, StdChannels
import	tcp

::	*TCP_SChannel			:==	TCP_SChannel_ ByteSeq
::	*TCP_RChannel			:==	TCP_RChannel_ ByteSeq
::	*TCP_Listener			:== TCP_Listener_ *(IPAddress, TCP_DuplexChannel)

::	Port					:== Int

::	*TCP_DuplexChannel		:== DuplexChannel *TCP_SChannel_ *TCP_RChannel_ ByteSeq
::	ByteSeq
		=	{	data		::	!{#Char}
			}

instance toString ByteSeq
  where
	toString {data }
		= data

instance ==			ByteSeq
  where
  	(==) bs1 bs2 = (toString bs1) == (toString bs2)
 
toByteSeq		::	!x			-> ByteSeq	| toString x
toByteSeq x
	= { data=toString x }

byteSeqSize		::	!ByteSeq	-> Int
byteSeqSize {data}
	= size data

instance toString IPAddress
  where
	toString inetHost
		= toDottedDecimal (unpack_ipaddr inetHost)

///////////// for asynchronous processing /////////////////////////

::	*TCP_Receiver ls ps 	
	=	TCP_Receiver
			Id TCP_RChannel	
			(ReceiverFunction (ReceiveMsg ByteSeq)	*(ls,ps))
			[ReceiverAttribute							*(ls,ps)]

::	SendNotifier sChannel ls ps
	=	SendNotifier
			sChannel
			(ReceiverFunction SendEvent		*(ls,ps))
			[ReceiverAttribute				*(ls,ps)]

::	*TCP_ListenerReceiver ls ps 	
	=	TCP_ListenerReceiver
			Id TCP_Listener	
			(*(ReceiveMsg *(IPAddress,TCP_DuplexChannel)) -> *(*(ls,ps) -> *(ls,ps)))
			[ReceiverAttribute							*(ls,ps)]

::	*TCP_CharReceiver ls ps 	
	=	TCP_CharReceiver
			Id TCP_RChannel (Maybe NrOfIterations)
			(ReceiverFunction (ReceiveMsg Char)		*(ls,ps))
			[ReceiverAttribute							*(ls,ps)]

::	NrOfIterations			:== Int
::	InetLookupFunction  ps	:== (Maybe IPAddress) -> ps -> ps
::	InetConnectFunction ps 	:== *(Maybe TCP_DuplexChannel) -> *(ps -> ps)

//////////////////////////////// for multiplexing //////////////////////////////////

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
