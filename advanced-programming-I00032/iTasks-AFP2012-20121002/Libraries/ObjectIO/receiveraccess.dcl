definition module receiveraccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	StdReceiverDef, receiverhandle

// MW11 added [Id]
newReceiverStateHandle	:: !Id .ls !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverStateHandle .pst
newReceiverStateHandle2	:: !Id .ls !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverStateHandle .pst

newReceiverHandle		:: !Id     !SelectState ![Id] !(ReceiverFunction  m   *(.ls,.pst)) -> ReceiverHandle .ls .pst
newReceiverHandle2		:: !Id     !SelectState ![Id] !(Receiver2Function m r *(.ls,.pst)) -> ReceiverHandle .ls .pst
// .. MW11

onewaytotriple			:: !(ReceiverFunction  m   *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)
twowaytotriple			:: !(Receiver2Function m r *(.ls,.pst)) m !*(.ls,.pst) -> *(.ls,[r],.pst)


//	Functions moved from receiverhandle

receiverIdentified			:: !Id										!(ReceiverHandle .ls .pst)	-> Bool
// MW11..
inetReceiverIdentified		:: !(!EndpointRef`, !InetReceiverCategory`)	!(ReceiverHandle .ls .pst)	-> Bool
inetReceiverIdentifiedWithId:: !(!Id, !InetReceiverCategory`)			!(ReceiverHandle .ls .pst)	-> Bool
// .. MW11
receiverSetSelectState		:: !SelectState								!*(ReceiverStateHandle .pst)-> *ReceiverStateHandle .pst
receiverHandleSyncMessage	:: !SyncMessage		!(ReceiverHandle .ls .pst) !*(.ls,.pst)
								-> ([SemiDynamic],ReceiverHandle .ls .pst,  *(.ls,.pst))
receiverAddASyncMessage		:: !Id !SemiDynamic	!(ReceiverHandle .ls .pst) -> ReceiverHandle .ls .pst
// MW11..
receiverApplyInetEvent		:: !InetReceiverASMQType !(ReceiverHandle .ls .pst) !*(.ls,.pst) -> *(.ls,.pst)
getInetReceiverRId			:: !(ReceiverHandle .ls .pst) -> RId InetReceiverASMQType
// ..MW11
