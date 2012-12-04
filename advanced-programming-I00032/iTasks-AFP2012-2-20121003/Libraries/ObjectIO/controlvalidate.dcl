definition module controlvalidate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Control validation.
//	********************************************************************************


import wstate


validateControlTitle	:: !String		-> String
validateSliderState		:: !SliderState	-> SliderState

getWElementControlIds	::			![WElementHandle .ls .pst]	-> (![Id],![WElementHandle .ls .pst])
getWElementControlIds`	::			![WElementHandle`]			-> [Id]

//	Id occurrence checks on [WElementHandle .ls .pst] and [WElementHandle`].
noDuplicateControlIds	::			![WElementHandle .ls .pst]	-> (!Bool,![WElementHandle .ls .pst])
noDuplicateControlIds`	::			![WElementHandle`]			-> Bool
disjointControlIds		:: ![Id]	![WElementHandle .ls .pst]	-> (!Bool,![WElementHandle .ls .pst])
disjointControlIds`		:: ![Id]	![WElementHandle`]			-> Bool


/*	Bind all free R(2)Ids that are contained in the WElementHandles.
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
bindReceiverControlIds	:: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable -> (![WElementHandle .ls .pst],!*ReceiverTable)

/*	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
*/
controlIdsAreConsistent :: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable !*IdTable
							   -> (!Bool,![WElementHandle .ls .pst],!*ReceiverTable,!*IdTable)


/*	validateItemPos checks if the OffsetAlign argument of ItemPos matches the ItemLoc argument.
*/
validateItemPos			:: !ItemPos -> ItemPos
