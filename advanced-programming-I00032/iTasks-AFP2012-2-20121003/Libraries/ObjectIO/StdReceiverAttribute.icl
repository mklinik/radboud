implementation module StdReceiverAttribute


import StdReceiverDef


/*	The following functions specify the valid attributes for each standard receiver.
*/

isValidReceiverAttribute :: !(ReceiverAttribute .st) -> Bool
isValidReceiverAttribute (ReceiverInit _)			= True
isValidReceiverAttribute (ReceiverSelectState _)	= True
isValidReceiverAttribute _							= False

isValidReceiver2Attribute :: !(ReceiverAttribute .st) -> Bool
isValidReceiver2Attribute (ReceiverInit _)			= True
isValidReceiver2Attribute (ReceiverSelectState _)	= True
isValidReceiver2Attribute _							= False


isReceiverInit			:: !(ReceiverAttribute .st) -> Bool
isReceiverInit				(ReceiverInit _)		= True
isReceiverInit				_						= False

isReceiverSelectState	:: !(ReceiverAttribute .st) -> Bool
isReceiverSelectState		(ReceiverSelectState _)	= True
isReceiverSelectState		_						= False

isReceiverConnectedReceivers	:: !(ReceiverAttribute .st) 	-> Bool // MW11++
isReceiverConnectedReceivers	(ReceiverConnectedReceivers _)	= True
isReceiverConnectedReceivers 	_ 								= False

getReceiverInitFun :: !(ReceiverAttribute .st) -> IdFun .st
getReceiverInitFun (ReceiverInit f) = f

getReceiverSelectStateAtt :: !(ReceiverAttribute .st) -> SelectState
getReceiverSelectStateAtt (ReceiverSelectState s) = s

getReceiverConnectedReceivers	:: !(ReceiverAttribute .st) -> [Id] // MW11++
getReceiverConnectedReceivers (ReceiverConnectedReceivers ids) = ids