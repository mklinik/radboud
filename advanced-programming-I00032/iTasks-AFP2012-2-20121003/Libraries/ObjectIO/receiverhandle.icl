implementation module receiverhandle


import	receivermessage
import	ostoolbox


::	*ReceiverHandles pst
	=	{	rReceivers	:: *[*ReceiverStateHandle pst]
		}
::	*ReceiverStateHandle pst
	=	E. .ls:
		{	rState		:: ls							// The local state of the receiver
		,	rHandle		:: ReceiverHandle ls pst		// The receiver handle
		}
::	ReceiverHandle ls pst
	=	E. m r:
		{	rId			:: Id							// The id of the receiver
		,	rASMQ		:: [m]							// The asynchronous message queue of the receiver
		,	rSelect		:: SelectState					// The current SelectState of the receiver
		,	rOneWay		:: Bool							// Flag: True iff receiver is uni-directional
		,	rFun		:: RHandleFunction ls m r pst	// If rOneWay then [r]==[], otherwise [r]==[_]
		,	rInetInfo	:: !Maybe (EndpointRef`,InetReceiverCategory`,Int,IdFun *OSToolbox)
														// For internet receivers
		,	rConnected	:: ![Id]						// storing the argument of the ReceiverCloseAlsoReceivers attribute
		}
::	RHandleFunction ls m r pst
	:==	m -> *(ls,pst) -> *(ls,[r],pst)

::	InetReceiverASMQType	:== (!InetEvent`,!EndpointRef`,!Int)

::	InetEvent`				:== Int
::	EndpointRef`			:==	Int
::	InetReceiverCategory`	:==	Int
