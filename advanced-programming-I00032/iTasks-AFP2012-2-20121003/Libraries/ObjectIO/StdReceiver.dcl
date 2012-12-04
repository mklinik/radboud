definition module StdReceiver

//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdReceiver specifies all receiver operations.
//	********************************************************************************

import	StdReceiverDef, StdMaybe
from	iostate	import :: PSt, :: IOSt
from	id		import class ==, :: R2Id, :: RId


//	Open uni- and bi-directional receivers:

class Receivers rdef where
	openReceiver   :: .ls !*(rdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getReceiverType::      *(rdef .ls .pst)               -> ReceiverType
/*	openReceiver
		opens the given receiver if no receiver currently exists with the given 
		R(2)Id. The R(2)Id has to be used to send messages to this receiver. 
	getReceiverType
		returns the type of the receiver (see also getReceivers).
*/

instance Receivers (Receiver  msg)
instance Receivers (Receiver2 msg resp)


closeReceiver			:: !Id !(IOSt .l) -> IOSt .l
/*	closeReceiver closes the indicated uni- or bi-directional receiver.
	Invalid Ids have no effect.
*/

getReceivers			:: !(IOSt .l) -> (![(Id,ReceiverType)], !IOSt .l)
/*	getReceivers returns the Ids and ReceiverTypes of all currently open uni- or 
	bi-directional receivers of this interactive process.
*/

enableReceivers			:: ![Id] !(IOSt .l) ->                      IOSt .l
disableReceivers		:: ![Id] !(IOSt .l) ->                      IOSt .l
getReceiverSelectState	:: ! Id	 !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
/*	(en/dis)ableReceivers
		(en/dis)able the indicated uni- or bi-directional receivers. 
		Note that this implies that in case of synchronous message passing messages 
		can fail (see the comments of syncSend and syncSend2 below). Invalid Ids 
		have no effect. 
	getReceiverSelectState
		yields the current SelectState of the indicated receiver. In case the 
		receiver does not exist, Nothing is returned.
*/

//	Inter-process communication:

//	Message passing status report:
::	SendReport
	=	SendOk
	|	SendUnknownReceiver
	|	SendUnableReceiver
	|	SendDeadlock
	|	OtherSendReport !String

instance ==       SendReport
instance toString SendReport

asyncSend :: !(RId msg) msg !(PSt .l) -> (!SendReport, !PSt .l)
/*	asyncSend posts a message to the receiver indicated by the argument RId. In case
	the indicated receiver belongs to this process, the message is simply buffered. 
	asyncSend is asynchronous: the message will at some point be received by the 
	indicated receiver. 
	The SendReport can be one of the following alternatives:
	-	SendOk:	No exceptional situation has occurred. The message has been sent. 
				Note that even though the message has been sent, it cannot be 
				guaranteed that the message will actually be handled by the 
				indicated receiver because it might become closed, forever disabled,
				or flooded with synchronous messages.
	-	SendUnknownReceiver:
				The indicated receiver does not exist. 
	-	SendUnableReceiver:
				Does not occur: the message is always buffered, regardless whether 
				the indicated receiver is Able or Unable. Note that in case the 
				receiver never becomes Able, the message will not be handled.
	-	SendDeadlock:
				Does not occur.
*/

syncSend :: !(RId msg) msg !(PSt .l) -> (!SendReport, !PSt .l)
/*	syncSend posts a message to the receiver indicated by the argument RId. In case 
	the indicated receiver belongs to the current process, the corresponding 
	ReceiverFunction is applied directly to the message argument and current process
	state. 
	syncSend is synchronous: this interactive process blocks evaluation until the 
	indicated receiver has received the message. 
	The SendReport can be one of the following alternatives:
	-	SendOk:	No exceptional situation has occurred. The message has been sent and
				handled by the indicated receiver. 
	-	SendUnknownReceiver:
				The indicated receiver does not exist. 
	-	SendUnableReceiver:
				The receiver exists, but its ReceiverSelectState is Unable. 
				Message passing is halted. The message is not sent. 
	-	SendDeadlock:
				The receiver is involved in a synchronous, cyclic communication 
				with the current process. Blocking the current process would result 
				in a deadlock situation. Message passing is halted to circumvent the
				deadlock. The message is not sent.
*/

syncSend2 :: !(R2Id msg resp) msg  !(PSt .l)
	-> (!(!SendReport,!Maybe resp), !PSt .l)
/*	syncSend2 posts a message to the receiver indicated by the argument R2Id. In 
	case the indicated receiver belongs to the current process, the corresponding 
	Receiver2Function is applied directly to the message argument and current 
	process state. 
	syncSend2 is synchronous: this interactive process blocks until the indicated 
	receiver has received the message. 
	The SendReport can be one of the following alternatives:
	-	SendOk:	No exceptional situation has occurred. The message has been sent and
				handled by the indicated receiver. The response of the receiver is 
				returned as well as (Just response).
	-	SendUnknownReceiver:
				The indicated receiver does not exist. 
	-	SendUnableReceiver:
				The receiver exists, but its ReceiverSelect is Unable. 
				Message passing is halted. The message is not sent. 
	-	SendDeadlock:
				The receiver is involved in a synchronous, cyclic communication 
				with the current process. Blocking the current process would result 
				in a deadlock situation. Message passing is halted to circumvent the
				deadlock. The message is not sent.
	In all other cases than SendOk, the optional response is Nothing.
*/
