implementation module controlvalidate


import	StdBool, StdInt, StdList
import	ospicture, oswindow
import	commondef, windowhandle, wstate
from	windowaccess import getWItemReceiverInfo


controlvalidateFatalError :: String String -> .x
controlvalidateFatalError function error
	= fatalError function "controlvalidate" error


//	Validate the title of a control.

validateControlTitle :: !String -> String
validateControlTitle string
	= removeSpecialChars OSControlTitleSpecialChars string


//	Validate the settings of a slider.

validateSliderState :: !SliderState -> SliderState
validateSliderState {sliderMin=sMin, sliderMax=sMax, sliderThumb=thumb}
	= {	sliderMin	= min`
	  ,	sliderMax	= max`
	  ,	sliderThumb	= setBetween thumb min` max`
	  }
where
	min` = min sMin sMax
	max` = max sMin sMax

//	Collect all Ids of the given [WElementHandle].

getWElementControlIds :: ![WElementHandle .ls .pst] -> (![Id],![WElementHandle .ls .pst])
getWElementControlIds itemHs = getWElementControlIdsAppend itemHs []
where
	getWElementControlIdsAppend :: ![WElementHandle .ls .pst] ![Id] -> (![Id],![WElementHandle .ls .pst])
	getWElementControlIdsAppend [itemH:itemHs] idst
		# (ids2,itemHs)	= getWElementControlIdsAppend itemHs idst
		# (ids12,itemH)	= getWElementIdsAppend itemH ids2
		= (ids12,[itemH:itemHs])
	where
		getWElementIdsAppend :: !(WElementHandle .ls .pst) ![Id] -> (![Id],!WElementHandle .ls .pst)
		getWElementIdsAppend (WItemHandle itemH) idst
			# (ids,itemH)	= getWElementIdsAppend` itemH idst
			= (ids,WItemHandle itemH)
		where
			getWElementIdsAppend` :: !(WItemHandle .ls .pst) ![Id] -> (![Id],!WItemHandle .ls .pst)
			getWElementIdsAppend` itemH=:{wItemId,wItems} idst
				# (ids,itemHs)		= getWElementControlIdsAppend wItems idst
				# itemH				= {itemH & wItems=itemHs}
				| isJust wItemId	= ([fromJust wItemId:ids],itemH)
				| otherwise			= (ids,itemH)
		
		getWElementIdsAppend (WListLSHandle itemHs) idst
			# (ids,itemHs)	= getWElementControlIdsAppend itemHs idst
			= (ids,WListLSHandle itemHs)
		
		getWElementIdsAppend (WExtendLSHandle wExH=:{wExtendItems=itemHs}) idst
			# (ids,itemHs)	= getWElementControlIdsAppend itemHs idst
			= (ids,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		getWElementIdsAppend (WChangeLSHandle wChH=:{wChangeItems=itemHs}) idst
			# (ids,itemHs)	= getWElementControlIdsAppend itemHs idst
			= (ids,WChangeLSHandle {wChH & wChangeItems=itemHs})
	getWElementControlIdsAppend _ idst
		= (idst,[])


//	Collect all Ids of the given [WElementHandle`].

getWElementControlIds` :: ![WElementHandle`] -> [Id]
getWElementControlIds` itemHs
	= getWElementControlIdsAppend` itemHs []
where
	getWElementControlIdsAppend` :: ![WElementHandle`] ![Id] -> [Id]
	getWElementControlIdsAppend` [itemH:itemHs] idst
		= getWElementIdsAppend itemH (getWElementControlIdsAppend` itemHs idst)
	where
		getWElementIdsAppend :: !WElementHandle` ![Id] -> [Id]
		getWElementIdsAppend (WItemHandle` itemH) idst
			= getWElementIdsAppend` itemH idst
		where
			getWElementIdsAppend` :: !WItemHandle` ![Id] -> [Id]
			getWElementIdsAppend` itemH=:{wItemId`,wItems`} idst
				# ids				= getWElementControlIdsAppend` wItems` idst
				| isJust wItemId`	= [fromJust wItemId`:ids]
				| otherwise			= ids
		
		getWElementIdsAppend (WRecursiveHandle` itemHs _) idst
			= getWElementControlIdsAppend` itemHs idst
	getWElementControlIdsAppend` _ idst
		= idst


//	Id occurrence checks on [WElementHandle .ls .pst] and [WElementHandle`].

//	There are no duplicate (ControlId id) attributes:

noDuplicateControlIds :: ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
noDuplicateControlIds itemHs
	# (ids,itemHs)	= getWElementControlIds itemHs
	= (noDuplicates ids, itemHs)

noDuplicateControlIds` :: ![WElementHandle`] -> Bool
noDuplicateControlIds` itemHs
	= noDuplicates (getWElementControlIds` itemHs)


//	The list of Ids does not occur in any (ControlId id) attribute:

disjointControlIds :: ![Id] ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
disjointControlIds ids itemHs
	# (ids`,itemHs)	= getWElementControlIds itemHs
	= (disjointLists ids ids`,itemHs)

disjointControlIds` :: ![Id] ![WElementHandle`] -> Bool
disjointControlIds` ids itemHs
	= disjointLists ids (getWElementControlIds` itemHs)


/*	Bind all free R(2)Ids that are contained in the WElementHandles.
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
bindReceiverControlIds :: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable -> (![WElementHandle .ls .pst],!*ReceiverTable)
bindReceiverControlIds ioId wId [itemH:itemHs] rt
	# (itemH, rt) = bindReceiverControlIds` ioId wId itemH  rt
	# (itemHs,rt) = bindReceiverControlIds  ioId wId itemHs rt
	= ([itemH:itemHs],rt)
where
	bindReceiverControlIds` :: !SystemId !Id !(WElementHandle .ls .pst) !*ReceiverTable
										  -> (!WElementHandle .ls .pst, !*ReceiverTable)
	bindReceiverControlIds` ioId wId (WItemHandle itemH=:{wItemKind,wItemInfo,wItems,wItemSelect}) rt
		| not (isReceiverControl wItemKind)
			# (itemHs,rt1)	= bindReceiverControlIds ioId wId wItems rt
			  itemH1		= {itemH & wItems=itemHs}
			= (WItemHandle itemH1,rt1)
		| otherwise
			# recLoc		= {rlIOId=ioId,rlDevice=WindowDevice,rlParentId=wId,rlReceiverId=id}
			# rte			= {rteLoc=recLoc,rteSelectState=if wItemSelect Able Unable,rteASMCount=0}
			# (_,rt)		= addReceiverToReceiverTable rte rt
			= (WItemHandle itemH,rt)
	where
		rH					= getWItemReceiverInfo wItemInfo
		id					= rH.rId
		
		isReceiverControl :: !ControlKind -> Bool
		isReceiverControl (IsOtherControl type)	= type=="Receiver" || type=="Receiver2"
		isReceiverControl _						= False
	
	bindReceiverControlIds` ioId wId (WListLSHandle itemHs) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
		= (WListLSHandle itemHs,rt)
	
	bindReceiverControlIds` ioId wId (WExtendLSHandle wExH=:{wExtendItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wExtendItems rt
		= (WExtendLSHandle {wExH & wExtendItems=itemHs},rt)
	
	bindReceiverControlIds` ioId wId (WChangeLSHandle wChH=:{wChangeItems}) rt
		# (itemHs,rt)	= bindReceiverControlIds ioId wId wChangeItems rt
		= (WChangeLSHandle {wChH & wChangeItems=itemHs},rt)

bindReceiverControlIds _ _ [] rt
	= ([],rt)


/*	controlIdsAreConsistent checks whether the WElementHandles contain (R(2))Ids that have already been
	associated with open receivers or other I/O objects and if there are no duplicate Ids. 
	The ReceiverTable is not changed if there are duplicate (R(2))Ids; otherwise all (R(2))Ids have been bound.
*/
controlIdsAreConsistent :: !SystemId !Id ![WElementHandle .ls .pst] !*ReceiverTable !*IdTable
							   -> (!Bool,![WElementHandle .ls .pst],!*ReceiverTable,!*IdTable)
controlIdsAreConsistent ioId wId itemHs rt it
	# (ids,itemHs)	= getWElementControlIds itemHs
	# (ok,it)		= okMembersIdTable ids it
	| not ok
		= (False,itemHs,rt,it)
	# idParent		= {idpIOId=ioId,idpDevice=WindowDevice,idpId=wId}
	  (ok,it)		= addIdsToIdTable [(id,idParent) \\ id<-ids] it
	  (itemHs,rt)	= bindReceiverControlIds ioId wId itemHs rt
	| not ok
		= controlvalidateFatalError "controlIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise
		= (True,itemHs,rt,it)


/*	validateItemPos checks if the OffsetAlign argument of ItemPos matches the ItemLoc argument.
*/
validateItemPos :: !ItemPos -> ItemPos
validateItemPos itemPos=:(itemLoc,OffsetAlign align)
	| itemLocHorizontal itemLoc && alignsHorizontally align || itemLocVertical itemLoc && alignsVertically align
		= itemPos
	| otherwise
		= (itemLoc,zero)
where
	itemLocHorizontal :: !ItemLoc -> Bool
	itemLocHorizontal (LeftOf  _)	= True
	itemLocHorizontal (RightTo _)	= True
	itemLocHorizontal LeftOfPrev	= True
	itemLocHorizontal RightToPrev	= True
	itemLocHorizontal _				= False
	
	itemLocVertical :: !ItemLoc -> Bool
	itemLocVertical (Above _)		= True
	itemLocVertical (Below _)		= True
	itemLocVertical AbovePrev		= True
	itemLocVertical BelowPrev		= True
	itemLocVertical _				= False
	
	alignsHorizontally :: !Alignment -> Bool
	alignsHorizontally AlignTop		= True
	alignsHorizontally AlignCenter	= True
	alignsHorizontally AlignBottom	= True
	alignsHorizontally _			= False
	
	alignsVertically :: !Alignment -> Bool
	alignsVertically AlignLeft		= True
	alignsVertically AlignCenter	= True
	alignsVertically AlignRight		= True
	alignsVertically _				= False
validateItemPos itemPos
	= itemPos
