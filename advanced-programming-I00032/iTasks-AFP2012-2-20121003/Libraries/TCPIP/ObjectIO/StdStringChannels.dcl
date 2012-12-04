definition module StdStringChannels

import	StdString
import	TCPDef, StdTCPDef, TCPChannelClass, TCPEvent
from	StdIOCommon		import :: ErrorReport
from	StdReceiver		import :: ReceiverType, :: RId, class Receivers(..),::PSt
from	TCPChannels import class getNrOfChannels, class SelectReceive, class SelectSend

import TCPStringChannels

::	*StringChannelReceiver ls pst 	
 =	StringChannelReceiver
		(RId (ReceiveMsg String)) StringRChannel	
		(ReceiverFunction (ReceiveMsg String) *(ls,pst))
		[ReceiverAttribute                    *(ls,pst)]

instance Receivers 		StringChannelReceiver
