implementation module receiveraccess

import	StdBool, StdInt, StdList
import	id, receiverdefaccess, receiverhandle
import	cast, semidynamic


// MW11 added connectedIds
newReceiverStateHandle :: !Id .ls !SelectState ![Id] !(ReceiverFunction m *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle id localState select connectedIds f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle id select connectedIds f	
	  }

// MW11 added rInetInfo,rConnected
newReceiverHandle :: !Id !SelectState ![Id] !(ReceiverFunction m *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle id select connectedIds f
	= {	rId			= id
	  ,	rASMQ		= []
	  ,	rSelect		= select
	  ,	rOneWay		= True
	  ,	rFun		= onewaytotriple f
	  , rInetInfo	= Nothing
  	  , rConnected	= connectedIds
	  }

onewaytotriple :: !(ReceiverFunction m *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)
onewaytotriple f m (ls,pst)
	# (ls,pst)	= f m (ls,pst)
	= (ls,[],pst)

// MW11 added connectedIds
newReceiverStateHandle2 :: !Id .ls !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle2 id localState select connectedIds f
	= {	rState	= localState
	  ,	rHandle	= newReceiverHandle2 id select connectedIds f
	  }

// MW11 added rInetInfo,rConnected
newReceiverHandle2 :: !Id !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle2 id select connectedIds f
	= {	rId			= id
	  ,	rASMQ		= []
	  ,	rSelect		= select
	  ,	rOneWay		= False
	  ,	rFun		= twowaytotriple f
	  , rInetInfo	= Nothing
  	  , rConnected	= connectedIds
	  }

twowaytotriple :: !(Receiver2Function m r *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)
twowaytotriple f m (ls,pst)
	# (r, (ls,pst))	= f m (ls,pst)
	= (ls,[r],pst)


//	Functions moved from receiverhandle:

receiverIdentified :: !Id !(ReceiverHandle .ls .pst) -> Bool
receiverIdentified id {rId}
	= id==rId

// MW11..
inetReceiverIdentified :: !(!EndpointRef`, !InetReceiverCategory`) !(ReceiverHandle .ls .pst) -> Bool
inetReceiverIdentified _ {rInetInfo=Nothing}
	= False
inetReceiverIdentified (epR1,type1) {rInetInfo=Just (epR2,type2,_,_)}
	= epR1==epR2 && type1==type2

inetReceiverIdentifiedWithId :: !(!Id, !InetReceiverCategory`) !(ReceiverHandle .ls .pst) -> Bool
inetReceiverIdentifiedWithId _ {rInetInfo=Nothing}
	= False
inetReceiverIdentifiedWithId (id,category) {rId, rInetInfo=Just (_,rCategory,_,_)}
	= id==rId && category==rCategory
// ..MW11

receiverSetSelectState :: !SelectState !*(ReceiverStateHandle .pst) -> *ReceiverStateHandle .pst
receiverSetSelectState select rsH=:{rHandle=rH}
	= {rsH & rHandle={rH & rSelect=select}}

receiverHandleSyncMessage :: !SyncMessage !(ReceiverHandle .ls .pst) !*(.ls,.pst) -> ([SemiDynamic],ReceiverHandle .ls .pst,*(.ls,.pst))
receiverHandleSyncMessage {smRecLoc={rlReceiverId},smMsg} rH=:{rFun} (ls,pst)
	| not (receiverIdentified rlReceiverId rH)
		= ([],rH,(ls,pst))
	# maybe_content	= getDynamic rlReceiverId smMsg
	| isNothing maybe_content
		= ([],rH,(ls,pst))
	# (ls,resp,pst)	= rFun (cast (fromJust maybe_content)) (ls,pst)
	| isEmpty resp
		= ([],rH,(ls,pst))
	| otherwise	
		= ([setDynamic rlReceiverId (hd resp) smMsg],rH,(ls,pst))

receiverAddASyncMessage :: !Id !SemiDynamic !(ReceiverHandle .ls .pst) -> ReceiverHandle .ls .pst
receiverAddASyncMessage id sd rH=:{rASMQ}
	| receiverIdentified id rH
		# maybe_content	= getDynamic id sd
		| isNothing maybe_content
			= rH
		// otherwise
			= {rH & rASMQ=rASMQ++[cast (fromJust maybe_content)]}
	| otherwise
		= rH

// MW11..
receiverApplyInetEvent :: !InetReceiverASMQType !(ReceiverHandle .ls .pst) !*(.ls,.pst) -> *(.ls,.pst)
receiverApplyInetEvent eventInfo rH=:{rFun,rInetInfo=Just _} (ls,pst)
	# (ls,_,pst)	= rFun (cast eventInfo) (ls,pst)
	= (ls,pst)

getInetReceiverRId :: !(ReceiverHandle .ls .ps) -> RId InetReceiverASMQType
// converts an Id into an RId
getInetReceiverRId {rId}
	= toRId (fromId rId)
// ..MW11
