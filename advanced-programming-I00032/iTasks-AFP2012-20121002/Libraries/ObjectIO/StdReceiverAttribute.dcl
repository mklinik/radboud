definition module StdReceiverAttribute


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdReceiverAttribute specifies which ReceiverAttributes are valid for each of 
//	the standard receivers.
//	Basic comparison operations and retrieval functions are also included.
//	********************************************************************************


import StdReceiverDef


/*	The following functions specify the valid attributes for each standard receiver.
*/

isValidReceiverAttribute :: !(ReceiverAttribute .st) -> Bool
/*	Receiver			(y = valid, . = invalid)
	ReceiverInit	y | ReceiverSelectState	y |
*/

isValidReceiver2Attribute :: !(ReceiverAttribute .st) -> Bool
/*	Receiver2			(y = valid, . = invalid)
	ReceiverInit	y | ReceiverSelectState	y |
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isReceiverInit				:: !(ReceiverAttribute .st) -> Bool
isReceiverSelectState		:: !(ReceiverAttribute .st) -> Bool
isReceiverConnectedReceivers:: !(ReceiverAttribute .st) -> Bool

/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getReceiverInitFun				:: !(ReceiverAttribute .st) -> IdFun .st
getReceiverSelectStateAtt		:: !(ReceiverAttribute .st) -> SelectState
getReceiverConnectedReceivers	:: !(ReceiverAttribute .st) -> [Id]
