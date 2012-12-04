definition module StdTCPChannels

import	StdString
import	TCPDef, StdChannels
from	tcp_bytestreams	import :: TCP_RCharStream_{..}, :: TCP_SCharStream_{..}
from	StdIOBasic		import :: :^:(:^:), :: Void(Void)

import	TCPChannels

instance SelectReceive Void
instance SelectReceive (:^: *x *y)		| SelectReceive, getNrOfChannels x 
										& SelectReceive y
	
instance SelectSend Void
instance SelectSend (:^: *x *y)			| SelectSend, getNrOfChannels x
										& SelectSend y

instance getNrOfChannels Void
instance getNrOfChannels (:^: *x *y)	| getNrOfChannels x & getNrOfChannels y
