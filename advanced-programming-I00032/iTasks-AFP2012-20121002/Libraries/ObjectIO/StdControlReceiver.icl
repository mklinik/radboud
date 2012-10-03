implementation module StdControlReceiver


import	StdTuple
import	StdControlClass, StdReceiverAttribute, windowhandle
import	ostypes
from	commondef		import cselect, :: Cond
from	receiveraccess	import newReceiverHandle, newReceiverHandle2


instance Controls (Receiver m) where
	controlToHandles (Receiver rid f atts) pState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= Just id
				,	wItemNr			= 0
				,	wItemKind		= IsOtherControl "Receiver"
				,	wItemShow		= False
				,	wItemSelect		= enabled select
// MW11 was				,	wItemInfo		= ReceiverInfo (newReceiverHandle id select f)
				,	wItemInfo		= ReceiverInfo (newReceiverHandle id select [] f)
				,	wItemAtts		= []
				,	wItems			= []
				,	wItemVirtual	= True
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= LayoutFix
				})
			]
		  ,	pState
		  )
	where
		id		= rIdtoId rid
		select	= getSelectState atts
	
	getControlType _
		= "Receiver"

instance Controls (Receiver2 m r) where
	controlToHandles (Receiver2 r2id f atts) pState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= Just id
				,	wItemNr			= 0
				,	wItemKind		= IsOtherControl "Receiver2"
				,	wItemShow		= False
				,	wItemSelect		= enabled select
// MW11 was				,	wItemInfo		= ReceiverInfo (newReceiverHandle2 id select f)
				,	wItemInfo		= ReceiverInfo (newReceiverHandle2 id select [] f)
				,	wItemAtts		= []
				,	wItems			= []
				,	wItemVirtual	= True
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= LayoutFix
				})
			]
		  ,	pState
		  )
	where
		id		= r2IdtoId r2id
		select	= getSelectState atts
	
	getControlType _
		= "Receiver2"

getSelectState :: ![ReceiverAttribute .pst] -> SelectState
getSelectState rAtts
	= getReceiverSelectStateAtt (snd (cselect isReceiverSelectState (ReceiverSelectState Able) rAtts))
