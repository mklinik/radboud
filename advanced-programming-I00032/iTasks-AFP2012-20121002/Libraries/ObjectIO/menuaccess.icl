implementation module menuaccess


import StdBool, StdList, StdTuple
import commondef, menuhandle, receivertable, receiverhandle
import osmenu


menuaccessFatalError :: String String -> .x
menuaccessFatalError function error
	= fatalError function "menuaccess" error


//	Access operations on MenuHandles:
menuHandlesGetMenus :: !*(MenuHandles .pst) -> (!*[MenuStateHandle .pst], !*MenuHandles .pst)
menuHandlesGetMenus mHs=:{mMenus}
	= (mMenus,{mHs & mMenus=[]})

menuHandlesGetKeys :: !*(MenuHandles .pst) -> (![Char], !*MenuHandles .pst)
menuHandlesGetKeys mHs=:{mKeys}
	= (mKeys,mHs)

menuHandlesGetEnabled :: !*(MenuHandles .pst) -> (!Bool, !*MenuHandles .pst)
menuHandlesGetEnabled mHs=:{mEnabled}
	= (mEnabled,mHs)

menuHandlesGetNrMenuBound :: !*(MenuHandles .pst) -> (!Bound, !*MenuHandles .pst)
menuHandlesGetNrMenuBound mHs=:{mNrMenuBound}
	= (mNrMenuBound,mHs)

menuHandlesGetPopUpId :: !*(MenuHandles .pst) -> (!Maybe Id, !*MenuHandles .pst)
menuHandlesGetPopUpId mHs=:{mPopUpId}
	= (mPopUpId,mHs)

menuHandlesSetMenus :: !*[MenuStateHandle .pst] !*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetMenus mMenus mHs
	= {mHs & mMenus=mMenus}

menuHandlesSetKeys :: ![Char] !*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetKeys mKeys mHs
	= {mHs & mKeys=mKeys}

menuHandlesSetEnabled :: !Bool !*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetEnabled mEnabled mHs
	= {mHs & mEnabled=mEnabled}

menuHandlesSetNrMenuBound :: !Bound !*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetNrMenuBound mNrMenuBound mHs
	= {mHs & mNrMenuBound=mNrMenuBound}

menuHandlesSetPopUpId :: !(Maybe Id) !*(MenuHandles .pst) -> *MenuHandles .pst
menuHandlesSetPopUpId mPopUpId mHs
	= {mHs & mPopUpId=mPopUpId}


//	Access operations on MenuStateHandle:
menuStateHandleGetHandle :: !*(MenuStateHandle .pst) -> (!OSMenu,!*MenuStateHandle .pst)
menuStateHandleGetHandle msH=:(MenuLSHandle {mlsHandle={mHandle}}) = (mHandle,msH)

menuStateHandleGetMenuId :: !*(MenuStateHandle .pst) -> (!Id,!*MenuStateHandle .pst)
menuStateHandleGetMenuId msH=:(MenuLSHandle {mlsHandle={mMenuId}}) = (mMenuId,msH)

menuStateHandleGetOSMenuNr :: !*(MenuStateHandle .pst) -> (!OSMenuNr,!*MenuStateHandle .pst)
menuStateHandleGetOSMenuNr msH=:(MenuLSHandle {mlsHandle={mOSMenuNr}}) = (mOSMenuNr,msH)

menuStateHandleGetTitle :: !*(MenuStateHandle .pst) -> (!Title,!*MenuStateHandle .pst)
menuStateHandleGetTitle msH=:(MenuLSHandle {mlsHandle={mTitle}}) = (mTitle,msH)

menuStateHandleGetSelect :: !*(MenuStateHandle .pst) -> (!Bool,!*MenuStateHandle .pst)
menuStateHandleGetSelect msH=:(MenuLSHandle {mlsHandle={mSelect}}) = (mSelect,msH)


menuStateHandleSetHandle :: !OSMenu !*(MenuStateHandle .pst) -> *MenuStateHandle .pst
menuStateHandleSetHandle menu (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mHandle=menu}}

menuStateHandleSetTitle :: !Title !*(MenuStateHandle .pst) -> *MenuStateHandle .pst
menuStateHandleSetTitle title (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mTitle=title}}

menuStateHandleSetSelect :: !Bool !*(MenuStateHandle .pst) -> *MenuStateHandle .pst
menuStateHandleSetSelect select (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mSelect=select}}


/*	menuIdsAreConsistent checks whether the MenuElementHandles contain (R(2))Ids that have already been
	associated with open receivers and if there are no duplicate Ids. 
	Neither the ReceiverTable nor the IdTable are changed if there are duplicate (R(2))Ids; 
	otherwise all (R(2))Ids have been bound.
*/
menuIdsAreConsistent :: !SystemId !Id !*[MenuElementHandle .ls .pst] !*ReceiverTable !*IdTable
							-> (!Bool,!*[MenuElementHandle .ls .pst],!*ReceiverTable,!*IdTable)
menuIdsAreConsistent ioId menuId itemHs rt it
	# (itemHs,ids)	= stateMap getMenuElementMenuId itemHs []
	# (ok,it)		= okMembersIdTable ids it
	| not ok
		= (False,itemHs,rt,it)
	# (ok,it)		= addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=MenuDevice,idpId=menuId})) ids) it
	# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
	| not ok
		= menuaccessFatalError "menuIdsAreConsistent" "could not add all Ids to IdTable"
	| otherwise
		= (True,itemHs,rt,it)
where
	getMenuElementMenuId :: !*(MenuElementHandle .ls .pst) ![Id] -> *(!*MenuElementHandle .ls .pst,![Id])
	getMenuElementMenuId itemH=:(MenuItemHandle {mItemId}) ids
		| isNothing mItemId		= (itemH,ids)
		| otherwise				= (itemH,[fromJust mItemId:ids])
	getMenuElementMenuId itemH=:(MenuReceiverHandle _) ids
		= (itemH,ids)
	getMenuElementMenuId (SubMenuHandle itemH=:{mSubMenuId,mSubItems=itemHs}) ids
		# (itemHs,ids)			= stateMap getMenuElementMenuId itemHs ids
		  subH					= SubMenuHandle {itemH & mSubItems=itemHs}
		| isNothing mSubMenuId	= (subH,ids)
		| otherwise				= (subH,[fromJust mSubMenuId:ids])
	getMenuElementMenuId (RadioMenuHandle itemH=:{mRadioId,mRadioItems=itemHs}) ids
		# (itemHs,ids)			= stateMap getMenuElementMenuId itemHs ids
		  radioH				= RadioMenuHandle {itemH & mRadioItems=itemHs}
		| isNothing mRadioId	= (radioH,ids)
		| otherwise				= (radioH,[fromJust mRadioId:ids])
	getMenuElementMenuId itemH=:(MenuSeparatorHandle {mSepId}) ids
		| isNothing mSepId		= (itemH,ids)
		| otherwise				= (itemH,[fromJust mSepId:ids])
	getMenuElementMenuId (MenuListLSHandle itemHs) ids
		# (itemHs,ids)			= stateMap getMenuElementMenuId itemHs ids
		= (MenuListLSHandle itemHs,ids)
	getMenuElementMenuId (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) ids
		# (itemHs,ids)			= stateMap getMenuElementMenuId itemHs ids
		= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},ids)
	getMenuElementMenuId (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) ids
		# (itemHs,ids)			= stateMap getMenuElementMenuId itemHs ids
		= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},ids)
	
/*	bindReceiverMenuIds binds all R(2)Ids in the MenuElementState list. 
	It assumes that it has already been checked that no R(2)Id is already bound in the ReceiverTable.
*/
	bindReceiverMenuIds :: !SystemId !Id !*[MenuElementHandle .ls .pst] !*ReceiverTable
									 -> (!*[MenuElementHandle .ls .pst],!*ReceiverTable)
	bindReceiverMenuIds ioId menuId [itemH:itemHs] rt
		# (itemH, rt)	= bindReceiverMenuId` ioId menuId itemH rt
		# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
		= ([itemH:itemHs],rt)
	where
		bindReceiverMenuId` :: !SystemId !Id !*(MenuElementHandle .ls .pst) !*ReceiverTable
										  -> (!*MenuElementHandle .ls .pst, !*ReceiverTable)
		bindReceiverMenuId` ioId menuId itemH=:(MenuReceiverHandle rH=:{mReceiverHandle={rId,rSelect}}) rt
			= (itemH,snd (addReceiverToReceiverTable rte rt))
		where
			rte	= {	rteLoc			= {	rlIOId		= ioId
									  ,	rlDevice	= MenuDevice
									  ,	rlParentId	= menuId
									  ,	rlReceiverId= rId
									  }
				  ,	rteSelectState	= rSelect
				  ,	rteASMCount		= 0
				  }
		bindReceiverMenuId` ioId menuId (SubMenuHandle itemH=:{mSubItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (SubMenuHandle {itemH & mSubItems=itemHs},rt)
		bindReceiverMenuId` ioId menuId (MenuListLSHandle itemHs) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuListLSHandle itemHs,rt)
		bindReceiverMenuId` ioId menuId (MenuExtendLSHandle	mExH=:{mExtendItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},rt)
		bindReceiverMenuId` ioId menuId (MenuChangeLSHandle	mChH=:{mChangeItems=itemHs}) rt
			# (itemHs,rt)	= bindReceiverMenuIds ioId menuId itemHs rt
			= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},rt)
		bindReceiverMenuId` _ _ itemH rt
			= (itemH,rt)
	bindReceiverMenuIds _ _ itemHs rt
		= (itemHs,rt)

//	Convert a RadioMenuItem to the MenuItemHandle alternative of MenuElementHandle:
radioMenuItemToMenuElementHandle :: !(MenuRadioItem *(.ls,.pst)) -> *MenuElementHandle .ls .pst
radioMenuItemToMenuElementHandle (title,optId,optShortKey,f)
	= MenuItemHandle {	mItemId		= optId
					 ,	mItemKey	= optShortKey
					 ,	mItemTitle	= title
					 ,	mItemSelect	= True
					 ,	mItemMark	= False
					 ,	mItemAtts	= [MenuFunction f]
					 ,	mOSMenuItem	= OSNoMenuItem
					 }
