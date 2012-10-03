definition module StdChannels

import TCPChannelClass

::	ReceiveMsg m		=	Received m
						|	EOM				// receiving "EOM" will automatically close the receiver
::	SendEvent			=	Sendable
						|	Disconnected	// receiving "Disconnected" will automatically close the receiver
