definition module menuaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	menuhandle


//	Access operations on MenuHandles:
menuHandlesGetMenus			::							!*(MenuHandles .pst) -> (!*[MenuStateHandle .pst],	!*MenuHandles .pst)
menuHandlesGetKeys			::							!*(MenuHandles .pst) -> (![Char],					!*MenuHandles .pst)
menuHandlesGetEnabled		::							!*(MenuHandles .pst) -> (!Bool,						!*MenuHandles .pst)
menuHandlesGetNrMenuBound	::							!*(MenuHandles .pst) -> (!Bound,					!*MenuHandles .pst)
menuHandlesGetPopUpId		::							!*(MenuHandles .pst) -> (!Maybe Id,					!*MenuHandles .pst)

menuHandlesSetMenus			:: !*[MenuStateHandle .pst]	!*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetKeys			:: ![Char]					!*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetEnabled		:: !Bool					!*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetNrMenuBound	:: !Bound					!*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetPopUpId		:: !(Maybe Id)				!*(MenuHandles .pst) -> *MenuHandles .pst

//	Access operations on MenuStateHandle:
menuStateHandleGetHandle	::							!*(MenuStateHandle .pst) -> (!OSMenu,	!*MenuStateHandle .pst)
menuStateHandleGetMenuId	::							!*(MenuStateHandle .pst) -> (!Id,		!*MenuStateHandle .pst)
menuStateHandleGetOSMenuNr	::							!*(MenuStateHandle .pst) -> (!OSMenuNr,	!*MenuStateHandle .pst)
menuStateHandleGetTitle		::							!*(MenuStateHandle .pst) -> (!Title,	!*MenuStateHandle .pst)
menuStateHandleGetSelect	::							!*(MenuStateHandle .pst) -> (!Bool,		!*MenuStateHandle .pst)

menuStateHandleSetHandle	:: !OSMenu					!*(MenuStateHandle .pst) -> *MenuStateHandle .pst
menuStateHandleSetTitle		:: !Title					!*(MenuStateHandle .pst) -> *MenuStateHandle .pst
menuStateHandleSetSelect	:: !Bool					!*(MenuStateHandle .pst) -> *MenuStateHandle .pst


/*	menuIdsAreConsistent checks whether the MenuElementHandles contain (R(2))Ids that have already been
	associated with open receivers and if there are no duplicate Ids. 
	Neither the ReceiverTable nor the IdTable are changed if there are duplicate (R(2))Ids; 
	otherwise all (R(2))Ids have been bound.
*/
menuIdsAreConsistent :: !SystemId !Id !*[MenuElementHandle .ls .pst] !*ReceiverTable !*IdTable
							-> (!Bool,!*[MenuElementHandle .ls .pst],!*ReceiverTable,!*IdTable)

//	Convert a RadioMenuItem to the MenuItemHandle alternative of MenuElementHandle:
radioMenuItemToMenuElementHandle :: !(MenuRadioItem *(.ls,.pst)) -> *MenuElementHandle .ls .pst
