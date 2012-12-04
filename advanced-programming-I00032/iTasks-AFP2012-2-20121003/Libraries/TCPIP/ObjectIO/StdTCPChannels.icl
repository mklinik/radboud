implementation module StdTCPChannels

import	StdEnv
import	TCPDef, StdChannels
import	tcp, ostcp, ostick, tcp_bytestreams

from StdIOBasic	import :: :^:(..), :: Void

import TCPChannels

instance SelectSend (:^: *x *y)			| SelectSend, getNrOfChannels x & SelectSend y
  where
	accSChannels f (l_channels :^: r_channels)
		#!	(l, l_channels)	= accSChannels f l_channels
			(r, r_channels)	= accSChannels f r_channels
		= (l++r, l_channels :^: r_channels)
	appDisconnected n (l_channels :^: r_channels) env
		#!	(l_length, l_channels)	= getNrOfChannels l_channels
		|	n<l_length
			#!	(result, l_channels, env)	= appDisconnected n l_channels env
			= (result, l_channels :^: r_channels, env)
		#!	(result, r_channels, env)	= appDisconnected (n-l_length) r_channels env
		= (result, l_channels :^: r_channels, env)

instance SelectSend Void
  where
	accSChannels _ void
		= ([],void)
	appDisconnected _ _ _
		= abort "StdTCPChannels: error: tried to apply appDisconnected to an object of type Void"

instance SelectReceive (:^: *x *y)			| SelectReceive, getNrOfChannels x & SelectReceive y
  where
	accRChannels f (l_channels :^: r_channels)
		#!	(l, l_channels)	= accRChannels f l_channels
			(r, r_channels)	= accRChannels f r_channels
		= (l++r, l_channels :^: r_channels)
	getRState n (l_channels :^: r_channels) env
		#!	(l_length, l_channels)	= getNrOfChannels l_channels
		|	n<l_length
			#!	(result, l_channels, env)	= getRState n l_channels env
			= (result, l_channels :^: r_channels, env)
		#!	(result, r_channels, env)	= getRState (n-l_length) r_channels env
		= (result, l_channels :^: r_channels, env)

instance SelectReceive Void
  where
	accRChannels _ void
		= ([],void)
	getRState _ void env
		= (Nothing, void, env)

instance getNrOfChannels (:^: *x *y)			| getNrOfChannels x & getNrOfChannels y
  where
	getNrOfChannels (l :^: r)
		#!	(nl, l)	= 	getNrOfChannels l
			(nr, r)	= 	getNrOfChannels r
		= (nl+nr, l :^: r)

instance getNrOfChannels Void
  where
	getNrOfChannels void
		= (0, void)
