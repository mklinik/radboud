definition module StdStringChannels

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdStringChannels provides channel instances to send and receive Strings.
//	These channels use their own protocol above TCP.
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdString
import	StdTCPDef, StdChannels, StdEventTCP
from	StdReceiver		import :: ReceiverType, :: RId, class Receivers(..);
from	StdTCPChannels	import class getNrOfChannels, class SelectReceive, class SelectSend

/*	If a string via a StringChannel is sent, then first the length of the string is
	sent, and then the string itself, e.g. sending the string "abc" will result in
	"3 abc\xD"
*/

//	********************************************************************************
//	StringChannels to receive
//	********************************************************************************

::	*StringRChannel_ a
::	*StringRChannel		:== StringRChannel_ String
::	*StringRChannels	=	StringRChannels *[StringRChannel]
::	*StringChannelReceiver ls pst 	
 =	StringChannelReceiver
		(RId (ReceiveMsg String)) StringRChannel	
		(ReceiverFunction (ReceiveMsg String) *(ls,pst))
		[ReceiverAttribute                    *(ls,pst)]

toStringRChannel		:: TCP_RChannel -> StringRChannel

instance Receivers 		StringChannelReceiver
instance Receive		StringRChannel_
instance closeRChannel	StringRChannel_
instance MaxSize		StringRChannel_


//	********************************************************************************
//	StringChannels to send
//	********************************************************************************

::	*StringSChannel_ a
::	*StringSChannel		:== StringSChannel_ String
::	*StringSChannels	=	StringSChannels *[StringSChannel]

toStringSChannel		::	TCP_SChannel -> StringSChannel

instance Send StringSChannel_

//	For openSendNotifier, closeSendNotifier
instance accSChannel 		StringSChannel_

//	For selectChannel
instance SelectSend			StringSChannels
instance SelectReceive		StringRChannels
instance getNrOfChannels 	StringRChannels
