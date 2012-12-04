implementation module receivertable


import StdBool, StdInt, StdMisc
import StdMaybe, commondef, id, systemid, device


::	ReceiverTable								// The table of all receivers
	:==	[ReceiverTableEntry]
::	ReceiverTableEntry
	=	{	rteLoc			:: !RecLoc			// The location of the receiver
		,	rteSelectState	:: !SelectState		// The current SelectState of the receiver
		,	rteASMCount		:: !Int				// The current size of asynchronous message queue of the receiver
		}
::	RecLoc
	=	{	rlIOId			:: !SystemId		// Id of parent process
		,	rlDevice		:: !Device			// Device kind of parent
		,	rlParentId		:: !Id				// Id of parent device instance
		,	rlReceiverId	:: !Id				// R(2)Id of the receiver itself
		}


initialReceiverTable :: *ReceiverTable			// initialReceiverTable yields an empty ReceiverTable
initialReceiverTable
	= []

eqReceiverId :: !Id !RecLoc -> Bool				// receivers are added uniquely by their rlReceiverId
eqReceiverId id {rlReceiverId}
	= id==rlReceiverId


/*	addReceiverToReceiverTable adds a new receiver entry to the ReceiverTable.
	The Boolean result is True iff no duplicate receiver entry was found, otherwise it is False.
*/
addReceiverToReceiverTable :: !ReceiverTableEntry !*ReceiverTable -> (!Bool,!*ReceiverTable)
addReceiverToReceiverTable rte=:{rteLoc={rlReceiverId}} receivers
	= add rlReceiverId rte receivers
where
	add :: !Id !ReceiverTableEntry !*[ReceiverTableEntry] -> (!Bool,!*[ReceiverTableEntry])
	add rid entry [rte=:{rteLoc}:rtes]
		| eqReceiverId rid rteLoc
			= (False,[rte:rtes])
		| otherwise
			# (isnew,rtes) = add rid entry rtes
			= (isnew,[rte:rtes])
	add _ entry _
		= (True,[entry])


/*	removeReceiverFromReceiverTable removes a receiver identified by Id from the ReceiverTable.
	The Boolean result is True iff an entry was actually removed, otherwise it is False.
*/
removeReceiverFromReceiverTable :: !Id !*ReceiverTable -> (!Bool,!*ReceiverTable)
removeReceiverFromReceiverTable rid receivers
	= (found,receivers`)
where
	(found,_,receivers`) = remove (\{rteLoc}->eqReceiverId rid rteLoc) undef receivers


/*	getReceiverTableEntry returns the receiver identified by Id from the ReceiverTable.
	If such a receiver could be found, then (Just ReceiverTableEntry) is returned, otherwise Nothing.
*/
getReceiverTableEntry :: !Id !*ReceiverTable -> (!Maybe ReceiverTableEntry,!*ReceiverTable)
getReceiverTableEntry rid [rte=:{rteLoc}:rtes]
	| eqReceiverId rid rteLoc
		= (Just rte,[rte:rtes])
	| otherwise
		# (maybeRTE,rtes)	= getReceiverTableEntry rid rtes
		= (maybeRTE,[rte:rtes])
getReceiverTableEntry _ []
	= (Nothing,[])


/*	setReceiverTableEntry replaces the current ReceiverTableEntry that has an identical Id. 
	If such an entry could not be found, then the new entry is added behind all other entries.
*/
setReceiverTableEntry :: !ReceiverTableEntry !*ReceiverTable -> *ReceiverTable
setReceiverTableEntry rte=:{rteLoc={rlReceiverId}} receivers
	= set rlReceiverId rte receivers
where
	set :: !Id !ReceiverTableEntry !*[ReceiverTableEntry] -> *[ReceiverTableEntry]
	set rid new [rte=:{rteLoc}:rtes]
		| eqReceiverId rid rteLoc
			= [new:rtes]
		| otherwise
			# rtes	= set rid new rtes
			= [rte:rtes]
	set _ new _
		= [new]


/*	getActiveReceiverTableEntry returns (Just Id) of the first receiver in the ReceiverTable which
	rteSelectState==Able and rteASMCount>0. 
	If such an entry could not be found, then Nothing is returned.
*/
getActiveReceiverTableEntry :: !*ReceiverTable -> (!Maybe Id,!*ReceiverTable)
getActiveReceiverTableEntry [rte=:{rteSelectState,rteASMCount,rteLoc}:rtes]
	| enabled rteSelectState && rteASMCount>0
		#! id = rteLoc.rlReceiverId
		=  (Just id,[rte:rtes])
	| otherwise
		# (maybeId,rtes)	= getActiveReceiverTableEntry rtes
		= (maybeId,[rte:rtes])
getActiveReceiverTableEntry []
	= (Nothing,[])
