implementation module StdTCPDef

from	StdMaybe		import :: Maybe

import TCPDef

from StdReceiverDef import ::ReceiverAttribute,::ReceiverFunction
from id import ::Id

from	TCPChannelClass	import :: DuplexChannel
import StdChannels
from	tcp				import :: IPAddress, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_
