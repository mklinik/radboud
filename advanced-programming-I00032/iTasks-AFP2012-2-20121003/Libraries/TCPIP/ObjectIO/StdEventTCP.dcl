definition module StdEventTCP

import TCPChannelClass,TCPDef,StdTCPDef
from StdPSt	import :: IOSt, :: PSt
from StdIOCommon import :: ErrorReport
from TCPEvent import class accSChannel
from StdReceiver import class Receivers

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
