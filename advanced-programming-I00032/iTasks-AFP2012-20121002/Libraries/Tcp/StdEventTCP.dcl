definition module StdEventTCP

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdEventTCP provides functions for using event driven TCP
//	Author: Martin Wierich
//	Modified: 15 October 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdChannels, StdId, StdTCPDef
from	StdReceiver		import class Receivers
from	StdPSt			import :: IOSt, :: PSt
from	StdIOCommon		import :: ErrorReport
from	tcp_bytestreams import :: TCP_SCharStream_

instance ChannelEnv (PSt  .l), (IOSt .l)

instance Receivers		TCP_ListenerReceiver
instance Receivers 		TCP_Receiver
instance Receivers		TCP_CharReceiver

openSendNotifier		::	.ls !(SendNotifier *(ch .a) .ls (PSt .l))
							!(PSt .l)
						->	(!ErrorReport,!*(ch .a),!PSt .l)
						|	accSChannel ch & Send ch
/*	opens a send notifier, which informs the application, that sending on the
	channel is again possible due to flow conditions. Possible error reports are
	NoError and ErrorNotifierOpen
*/

closeSendNotifier		::	!*(ch .a) !(IOSt .l)
						-> (!*(ch .a), !IOSt .l)
						|	accSChannel ch
/*	closes a send notifier. This function will be called implicitly if a send
	channel is closed, so there is no need to do it explicitly then.
*/

lookupIPAddress_async	::	!String !(InetLookupFunction (PSt .l)) !(PSt .l)
						->	PSt .l
/*	lookupIPAddress_async asynchronously looks up an IP address. The String can be 
	in dotted decimal form or alphanumerical. The InetLookupFunction will be called
	with the IP address, if this address was found, otherwise with Nothing.
*/

connectTCP_async		::	!(!IPAddress,!Port) !(InetConnectFunction (PSt .l))
							!(PSt .l)
						->	PSt .l
/*	connectTCP_async asynchronously tries to establish a new connection. The
	InetConnectFunction will be called with the new duplex channel if this attempt
	was succesful, otherwise with Nothing
*/

class accSChannel ch	::	(TCP_SChannel -> (x, TCP_SChannel)) *(ch .a)
					 	->	(x, *(ch .a))
/*	This overloaded function supports the openSendNotifier function. It applies an
	access function on the underlying TCP_SChannel
*/

instance accSChannel TCP_SChannel_
instance accSChannel TCP_SCharStream_
