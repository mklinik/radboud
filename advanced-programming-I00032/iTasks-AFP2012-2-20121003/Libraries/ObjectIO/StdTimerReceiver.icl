implementation module StdTimerReceiver


import	StdTuple, StdList
import	commondef, id, receiveraccess, receiverdefaccess, StdReceiverAttribute, StdTimerElementClass, timerhandle


instance TimerElements (Receiver m) where
	timerElementToHandles (Receiver rid f atts) pState
		= (	[timerElementHandleToTimerElementState
// MW11 was				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle id (getSelectState atts) f
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle id (getSelectState atts) (getConnectedIds atts) f
									 ,	tReceiverAtts	= [TimerId id:map ReceiverAttToTimerAtt atts]
									 })
			]
		  ,	pState
		  )
	where
		id	= rIdtoId rid
	
	getTimerElementType _ = "Receiver"

instance TimerElements (Receiver2 m r) where
	timerElementToHandles (Receiver2 rid f atts) pState
		= (	[timerElementHandleToTimerElementState
// MW11 was				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle2 id (getSelectState atts) f
				(TimerReceiverHandle {	tReceiverHandle	= newReceiverHandle2 id (getSelectState atts) (getConnectedIds atts) f
									 ,	tReceiverAtts	= [TimerId id:map ReceiverAttToTimerAtt atts]
									 })
			]
		  ,	pState
		  )
	where
		id	= r2IdtoId rid
	
	getTimerElementType _ = "Receiver2"

/* MW11
getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (Select isReceiverSelectState (ReceiverSelectState Able) rAtts))
*/

ReceiverAttToTimerAtt :: !(ReceiverAttribute .ps) -> TimerAttribute .ps
ReceiverAttToTimerAtt (ReceiverSelectState s)
	= TimerSelectState s

// MW11..
getSelectState :: ![ReceiverAttribute .ps] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))

getConnectedIds :: ![ReceiverAttribute .ps] -> [Id]
getConnectedIds rAtts
	= getReceiverConnectedReceivers (snd (cselect isReceiverConnectedReceivers (ReceiverConnectedReceivers []) rAtts))
// .. MW11
