definition module receiverdefaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access functions to ReceiverDefinitions.
//	********************************************************************************

import	StdReceiverAttribute


receiverDefAttributes		:: !(Receiver  m   .ls .pst) -> [ReceiverAttribute *(.ls,.pst)]
receiver2DefAttributes		:: !(Receiver2 m r .ls .pst) -> [ReceiverAttribute *(.ls,.pst)]

receiverDefRId				:: !(Receiver  m   .ls .pst) -> RId  m
receiver2DefR2Id			:: !(Receiver2 m r .ls .pst) -> R2Id m r

receiverDefSelectState		:: !(Receiver  m   .ls .pst) -> SelectState
receiver2DefSelectState		:: !(Receiver2 m r .ls .pst) -> SelectState

receiverDefFunction			:: !(Receiver  m   .ls .pst) -> ReceiverFunction  m   *(.ls,.pst)
receiver2DefFunction		:: !(Receiver2 m r .ls .pst) -> Receiver2Function m r *(.ls,.pst)

receiverDefSetAbility		:: !SelectState !(Receiver  m   .ls .pst) -> Receiver  m   .ls .pst
receiver2DefSetAbility		:: !SelectState !(Receiver2 m r .ls .pst) -> Receiver2 m r .ls .pst

receiverDefSetFunction		:: !(ReceiverFunction  m   *(.ls,.pst))  !(Receiver  m   .ls .pst) -> Receiver  m   .ls .pst
receiver2DefSetFunction		:: !(Receiver2Function m r *(.ls,.pst))  !(Receiver2 m r .ls .pst) -> Receiver2 m r .ls .pst
