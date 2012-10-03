definition module receivertable


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import id, systemid, device
import StdMaybe, StdIOCommon


::	ReceiverTable									// The table of all receivers
::	ReceiverTableEntry
	=	{	rteLoc				:: !RecLoc			// The location of the receiver
		,	rteSelectState		:: !SelectState		// The current SelectState of the receiver
		,	rteASMCount			:: !Int				// The current size of asynchronous message queue of the receiver
		}
::	RecLoc
	=	{	rlIOId				:: !SystemId		// Id of parent process
		,	rlDevice			:: !Device			// Device kind of parent
		,	rlParentId			:: !Id				// Id of parent device instance
		,	rlReceiverId		:: !Id				// R(2)Id of the receiver itself
		}

initialReceiverTable			:: *ReceiverTable	// initialReceiverTable yields an empty ReceiverTable

/*	addReceiverToReceiverTable adds a new receiver entry to the ReceiverTable.
	The Boolean result is True iff no duplicate receiver entry was found, otherwise it is False.
*/
addReceiverToReceiverTable		:: !ReceiverTableEntry !*ReceiverTable -> (!Bool,!*ReceiverTable)

/*	removeReceiverFromReceiverTable removes a receiver identified by Id from the ReceiverTable.
	The Boolean result is True iff an entry was actually removed, otherwise it is False.
*/
removeReceiverFromReceiverTable	:: !Id !*ReceiverTable -> (!Bool,!*ReceiverTable)

/*	getReceiverTableEntry returns the receiver identified by Id from the ReceiverTable.
	If such a receiver could be found, then (Just ReceiverTableEntry) is returned, otherwise Nothing.
*/
getReceiverTableEntry			:: !Id !*ReceiverTable -> (!Maybe ReceiverTableEntry,!*ReceiverTable)

/*	setReceiverTableEntry replaces the current ReceiverTableEntry that has an identical Id. 
	If such an entry could not be found, then the new entry is added behind all other entries.
*/
setReceiverTableEntry			:: !ReceiverTableEntry !*ReceiverTable -> *ReceiverTable

/*	getActiveReceiverTableEntry returns (Just Id) of the first receiver in the ReceiverTable which
	rteSelectState==Able and rteASMCount>0. 
	If such an entry could not be found, then Nothing is returned.
*/
getActiveReceiverTableEntry		:: !*ReceiverTable -> (!Maybe Id,!*ReceiverTable)
