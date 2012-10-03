implementation module StdMenuElement


import	StdBool, StdChar, StdFunc, StdList, StdMisc, StdTuple
import	commondef, iostate, menuaccess, mstate
import	osmenu

stdMenuElementFatalError :: String String -> .x
stdMenuElementFatalError function error
	= fatalError function "StdMenuElement" error


/*	The function isOkMenuElementId can be used to filter out the proper IdParent records.
*/
isOkMenuElementId :: !SystemId !(.x,!Maybe IdParent) -> (!Bool,(.x,Id))
isOkMenuElementId ioId (x,Just {idpIOId,idpDevice,idpId})
	= (ioId==idpIOId && idpDevice==MenuDevice,(x,idpId))
isOkMenuElementId _ _
	= (False,undef)

/*	The following functions conveniently retrieve IdParents from the IOSt.
*/
ioStGetIdParent :: !Id !(IOSt .l) -> (!Maybe IdParent,!IOSt .l)
ioStGetIdParent id ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent id idtable
	= (maybeParent,ioStSetIdTable idtable ioState)

ioStGetIdParents :: ![Id] !(IOSt .l) -> (![Maybe IdParent],!IOSt .l)
ioStGetIdParents ids ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParents,idtable)= getIdParents ids idtable
	= (maybeParents,ioStSetIdTable idtable ioState)

/*	gatherMenuIds collects all first Ids (menu element Ids) that belong to the same second Id (MenuId).
	gatherMenuIds` does the same, except that not only menu element Ids are collected, but also their data item.
*/
gatherMenuIds :: ![(Id,Id)] -> [([Id],Id)]
gatherMenuIds [(itemId,mId):ids]
	= [([itemId:itemIds],mId):itemIds_mIds]
where
	(itemIds,ids`)	= gatherElementsIds mId ids
	itemIds_mIds	= gatherMenuIds ids`
	
	gatherElementsIds :: !Id ![(Id,Id)] -> ([Id],[(Id,Id)])
	gatherElementsIds mId [(itemId,mId`):ids]
		| mId==mId`	= ([itemId:itemIds],ids`)
		| otherwise	= (itemIds,[(itemId,mId`):ids`])
	where
		(itemIds,ids`)	= gatherElementsIds mId ids
	gatherElementsIds _ _
		= ([],[])
gatherMenuIds []
	= []

gatherMenuIds` :: ![((Id,.x),Id)] -> [([(Id,.x)],Id)]
gatherMenuIds` [((itemId,x),mId):ids]
	= [([(itemId,x):itemIds],mId):itemIds_mIds]
where
	(itemIds,ids`)	= gatherElementsIds mId ids
	itemIds_mIds	= gatherMenuIds` ids`
	
	gatherElementsIds :: !Id ![((Id,.x),Id)] -> ([(Id,.x)],[((Id,.x),Id)])
	gatherElementsIds mId [((itemId,x),mId`):ids]
		| mId==mId`	= ([(itemId,x):itemIds],ids`)
		| otherwise	= (itemIds,[((itemId,x),mId`):ids`])
	where
		(itemIds,ids`)	= gatherElementsIds mId ids
	gatherElementsIds _ _
		= ([],[])
gatherMenuIds` []
	= []

eqMenuLSHandleId :: !Id !*(MenuStateHandle .pst) -> (!Bool,!*MenuStateHandle .pst)
eqMenuLSHandleId id msH
	# (id`,msH)	= menuStateHandleGetMenuId msH
	= (id==id`,msH)

retrieveMenuHandle` :: !*(MenuStateHandle .pst) -> (!MenuHandle`,!*MenuStateHandle .pst)
retrieveMenuHandle` (MenuLSHandle mlsH=:{mlsHandle=mH})
	# (mH`,mH)	= getMenuHandle` mH
	= (mH`,MenuLSHandle {mlsH & mlsHandle=mH})

insertMenuHandle` :: !MenuHandle` !*(MenuStateHandle .pst) -> *MenuStateHandle .pst
insertMenuHandle` mH` (MenuLSHandle mlsH=:{mlsHandle=mH})
	= MenuLSHandle {mlsH & mlsHandle=setMenuHandle` mH` mH}


//	The MState menu representation record:

::	MState
	=	{	mRep	:: !MenuHandle`
		,	mTb		:: !.Int
		}


getMenu :: !Id !(IOSt .l) -> (!Maybe MState, !IOSt .l)
getMenu menuId ioState
	# (ok,ioState)				= ioStHasDevice MenuDevice ioState
	| not ok
		= (Nothing,ioState)
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# menus						= menuSystemStateGetMenuHandles mDevice
	# (mHs,menus)				= menuHandlesGetMenus menus
	# (found,mState,mHs)		= getMState menuId mHs
	# menus						= menuHandlesSetMenus mHs menus
	# ioState					= ioStSetDevice (MenuSystemState menus) ioState
	| found
		= (Just {mRep=mState,mTb=0},ioState)
	| otherwise
		= (Nothing,ioState)
where
	getMState :: !Id !*[MenuStateHandle .pst] -> (!Bool,MenuHandle`,!*[MenuStateHandle .pst])
	getMState menuId [msH:msHs]
		# (yes,msH)					= eqMenuLSHandleId menuId msH
		| yes
			# (msH`,msH)			= retrieveMenuHandle` msH
			= (yes,msH`,[msH:msHs])
		| otherwise
			# (found,mstate,msHs)	= getMState menuId msHs
			= (found,mstate,[msH:msHs])
	getMState _ []
		= (False,undef,[])

getParentMenu :: !Id !(IOSt .l) -> (!Maybe MState, !IOSt .l)
getParentMenu itemId ioState
	# (maybeParent,ioState)	= ioStGetIdParent itemId ioState
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent				= fromJust maybeParent
	# (ioId,ioState)		= ioStGetIOId ioState
	| ioId==parent.idpIOId && parent.idpDevice==MenuDevice
		= getMenu parent.idpId ioState
	| otherwise
		= (Nothing,ioState)

setMenu :: !Id !(IdFun *MState) !(IOSt .l) -> IOSt .l
setMenu menuId f ioState
	# (ok,ioState)				= ioStHasDevice MenuDevice ioState
	| not ok
		= ioState
	# (osdinfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar	// This condition should never occur
		= stdMenuElementFatalError "setMenu" "OSMenuBar could not be retrieved from OSDInfo"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found					// This condition should never occur
		= stdMenuElementFatalError "setMenu" "MenuSystemState could not be retrieved from IOSt"
	| otherwise
		# menus					= menuSystemStateGetMenuHandles mDevice
		# (mHs,menus)			= menuHandlesGetMenus menus
		# (mHs,ioState)			= accIOToolbox (setMState osMenuBar menuId f mHs) ioState
		# menus					= menuHandlesSetMenus mHs menus
		# ioState				= ioStSetDevice (MenuSystemState menus) ioState
		= ioState
where
	setMState :: !OSMenuBar !Id !(IdFun *MState) !*[MenuStateHandle .pst] !*OSToolbox -> (!*[MenuStateHandle .pst],!*OSToolbox)
	setMState osMenuBar menuId f [msH:msHs] tb
		# (yes,msH)				= eqMenuLSHandleId menuId msH
		| yes
			# (msH`,msH)		= retrieveMenuHandle` msH
			# {mRep=msH`,mTb=tb}= f {mRep=msH`,mTb=tb}
			# msH				= insertMenuHandle` msH` msH
			# tb				= osDrawMenuBar osMenuBar tb
			= ([msH:msHs],tb)
		| otherwise
			# (msHs,tb)			= setMState osMenuBar menuId f msHs tb
			= ([msH:msHs],tb)
	setMState _ _ _ [] tb
		= ([],tb)


//	Enabling and Disabling of menu elements:

enableMenuElements :: ![Id] !(IOSt .l) -> IOSt .l
enableMenuElements ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  ids_mIds				= filterMap (isOkMenuElementId ioId) (zip2 ids idparents)
	  ids_mIds				= gatherMenuIds ids_mIds
	| isEmpty ids_mIds
		= ioState
	| otherwise
		= strictSeq [setMenu mId (changeMenuItemsSelect (map (\id->(id,True)) ids)) \\ (ids,mId)<-ids_mIds] ioState

disableMenuElements :: ![Id] !(IOSt .l) -> IOSt .l
disableMenuElements ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  ids_mIds				= filterMap (isOkMenuElementId ioId) (zip2 ids idparents)
	  ids_mIds				= gatherMenuIds ids_mIds
	| isEmpty ids_mIds
		= ioState
	| otherwise
		= strictSeq [setMenu mId (changeMenuItemsSelect (map (\id->(id,False)) ids)) \\ (ids,mId)<-ids_mIds] ioState

changeMenuItemsSelect :: ![(Id,Bool)] !*MState -> *MState
changeMenuItemsSelect idSelects mState
	= changeMenuItems RecurseAll setItemAbility idSelects mState
where
	setItemAbility :: !Bool !OSMenu !Int !MenuElementHandle` !*OSToolbox -> (!MenuElementHandle`,!*OSToolbox)
	
	setItemAbility enabled menu itemNr (SubMenuHandle` itemH=:{mSubHandle`}) tb
		# index = itemNr													// DvA: ???
		| enabled	= (submenu,osEnableSubMenu  menu mSubHandle` index tb)	// DvA
		| otherwise	= (submenu,osDisableSubMenu menu mSubHandle` index tb)	// DvA
	where
		submenu		= SubMenuHandle` {itemH & mSubSelect`=enabled}
	
	setItemAbility enabled menu itemNr (RadioMenuHandle` itemH=:{mRadioItems`,mRadioSelect`}) tb
		| enabled	= (radiomenu,enableAbleItems menu mRadioItems` itemNr tb)
		| otherwise	= (radiomenu,disableAllItems menu mRadioItems` itemNr tb)
	where
		radiomenu	= RadioMenuHandle` {itemH & mRadioSelect`=enabled}
		
		enableAbleItems :: !OSMenu ![MenuElementHandle`] !Int !*OSToolbox -> *OSToolbox
		enableAbleItems menu [MenuItemHandle` itemH=:{mItemSelect`,mOSMenuItem`}:itemHs] itemNr tb
			# tb			= enableAbleItems menu itemHs (itemNr+1) tb
			| mItemSelect`	= osEnableMenuItem menu mOSMenuItem` itemNr tb
			| otherwise		= tb
		enableAbleItems _ _ _ tb
			= tb
		
		disableAllItems :: !OSMenu ![MenuElementHandle`] !Int !*OSToolbox -> *OSToolbox
		disableAllItems menu [MenuItemHandle` itemH=:{mItemSelect`,mOSMenuItem`}:itemHs] itemNr tb
			# tb			= disableAllItems menu itemHs (itemNr+1) tb
			# tb			= osDisableMenuItem menu mOSMenuItem` itemNr tb
			= tb
		disableAllItems _ _ _ tb
			= tb
//		disableAllItems menu itemHs itemNr tb
//			= StateMap2 (\(MenuItemHandle` {mOSMenuItem`}) tb->OSDisableMenuItem menu mOSMenuItem` tb) itemHs tb
	
	setItemAbility enabled menu itemNr (MenuItemHandle` itemH=:{mOSMenuItem`,mItemId`}) tb
		| enabled	= (menuitem,osEnableMenuItem  menu mOSMenuItem` itemNr tb)
		| otherwise	= (menuitem,osDisableMenuItem menu mOSMenuItem` itemNr tb)
		where
			menuitem= MenuItemHandle` {itemH & mItemSelect`=enabled}
	
	setItemAbility _ _ _ itemH tb
		= (itemH,tb)

//	Marking and Unmarking of MenuItems only:

markMenuItems :: ![Id] !(IOSt .l) -> IOSt .l
markMenuItems ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  ids_mIds				= filterMap (isOkMenuElementId ioId) (zip2 ids idparents)
	  ids_mIds				= gatherMenuIds ids_mIds
	| isEmpty ids_mIds		= ioState
	| otherwise				= strictSeq [setMenu mId (changeMenuItemsMark (map (\id->(id,True)) ids)) \\ (ids,mId)<-ids_mIds] ioState

unmarkMenuItems :: ![Id] !(IOSt .l) -> IOSt .l
unmarkMenuItems ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  ids_mIds				= filterMap (isOkMenuElementId ioId) (zip2 ids idparents)
	  ids_mIds				= gatherMenuIds ids_mIds
	| isEmpty ids_mIds
		= ioState
	| otherwise
		= strictSeq [setMenu mId (changeMenuItemsMark (map (\id->(id,False)) ids)) \\ (ids,mId)<-ids_mIds] ioState

changeMenuItemsMark :: ![(Id,Bool)] !*MState -> *MState
changeMenuItemsMark idMarks mState
	= changeMenuItems RecurseSubMenuOnly setItemMark idMarks mState
where
	setItemMark :: !Bool !OSMenu !Int !MenuElementHandle` !*OSToolbox -> (!MenuElementHandle`,!*OSToolbox)
	setItemMark marked menu itemNr (MenuItemHandle` itemH) tb
		= (MenuItemHandle` {itemH & mItemMark`=marked},osMenuItemCheck marked menu itemH.mOSMenuItem` itemNr itemNr tb)
	setItemMark _ _ _ itemH tb
		= (itemH,tb)


//	Changing the Title of menu elements:

setMenuElementTitles :: ![(Id,Title)] !(IOSt .l) -> IOSt .l
setMenuElementTitles id_titles ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	  (ids,_)					= unzip id_titles
	# (idparents,ioState)		= ioStGetIdParents ids ioState
	  id_titles_mIds			= filterMap (isOkMenuElementId ioId) (zip2 id_titles idparents)
	  id_titles_mIds			= gatherMenuIds` id_titles_mIds
	| isEmpty id_titles_mIds	= ioState
	| otherwise					= strictSeq [setMenu mId (changeMenuItems RecurseAll setItemTitle id_titles) \\ (id_titles,mId)<-id_titles_mIds] ioState
where
	setItemTitle :: !Title !OSMenu !Int !MenuElementHandle` !*OSToolbox -> (!MenuElementHandle`,!*OSToolbox)
	setItemTitle title menu itemNr (SubMenuHandle` itemH) tb
		= (SubMenuHandle` {itemH & mSubTitle`=title},osChangeMenuItemTitle menu itemH.mSubHandle` itemNr (osValidateMenuItemTitle Nothing title) tb)
	setItemTitle title menu itemNr (MenuItemHandle` itemH=:{mOSMenuItem`,mItemKey`}) tb
		= (MenuItemHandle` {itemH & mItemTitle`=title},osChangeMenuItemTitle menu mOSMenuItem` itemNr validTitle tb)
	where
		validTitle	= osValidateMenuItemTitle mItemKey` title
	setItemTitle _ _ _ itemH tb
		= (itemH,tb)


//	Changing the selected menu item of a RadioMenu by its Id:

selectRadioMenuItem :: !Id !Id !(IOSt .l) -> IOSt .l
selectRadioMenuItem id itemId ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent id ioState
	| not (fst (isOkMenuElementId ioId (id,maybeParent)))
		= ioState
	| otherwise
		= setMenu (fromJust maybeParent).idpId (changeMenuItems RecurseAll selectradiomenuitem [(id,itemId)]) ioState
where
	selectradiomenuitem :: !Id !OSMenu !Int !MenuElementHandle` !*OSToolbox -> (!MenuElementHandle`,!*OSToolbox)
	selectradiomenuitem itemId menu itemNr (RadioMenuHandle` radioH=:{mRadioIndex`=now,mRadioItems`}) tb
		| isEmpty mRadioItems`
			= (RadioMenuHandle` radioH,tb)
		# new				= getRadioMenuItemIndex itemId mRadioItems` 1
		| new==0
			= (RadioMenuHandle` radioH,tb)
		| otherwise
			# (items,(_,tb))= stateMap (setmark menu new itemNr) mRadioItems` (1,tb)
			= (RadioMenuHandle` {radioH & mRadioIndex`=new,mRadioItems`=items},tb)
	where
		getRadioMenuItemIndex :: !Id ![MenuElementHandle`] !Index -> Index
		getRadioMenuItemIndex id [MenuItemHandle` {mItemId`}:itemHs] index
			| isNothing mItemId` || fromJust mItemId`<>id	= getRadioMenuItemIndex id itemHs (index+1)
			| otherwise										= index
		getRadioMenuItemIndex _ _ _
			= 0
	selectradiomenuitem _ _ _ itemH tb
		= (itemH,tb)

setmark :: !OSMenu !Index !Int !MenuElementHandle` !(!Int,!*OSToolbox) -> (!MenuElementHandle`,!(!Int,!*OSToolbox))
setmark menu new rootNr (MenuItemHandle` itemH=:{mOSMenuItem`}) (index,tb)
	= (MenuItemHandle` {itemH & mItemMark`=marked},(index+1,osMenuItemCheck marked menu mOSMenuItem` index (rootNr+index-1) tb))
where
	marked	= index==new


//	Changing the selected menu item of a RadioMenu by its index:

selectRadioMenuIndexItem :: !Id !Index !(IOSt .l) -> IOSt .l
selectRadioMenuIndexItem id index ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent id ioState
	| not (fst (isOkMenuElementId ioId (id,maybeParent)))
		= ioState
	| otherwise
		= setMenu (fromJust maybeParent).idpId (changeMenuItems RecurseAll selectradiomenuindexitem [(id,index)]) ioState
where
	selectradiomenuindexitem :: !Index !OSMenu !Int !MenuElementHandle` !*OSToolbox -> (!MenuElementHandle`,!*OSToolbox)
	selectradiomenuindexitem new menu itemNr (RadioMenuHandle` radioH=:{mRadioIndex`=now,mRadioItems`}) tb
		| isEmpty mRadioItems`
			= (RadioMenuHandle` radioH,tb)
		# new				= setBetween new 1 (length mRadioItems`)
		| new==now
			= (RadioMenuHandle` radioH,tb)
		| otherwise
			# (items,(_,tb))= stateMap (setmark menu new itemNr) mRadioItems` (1,tb)
			= (RadioMenuHandle` {radioH & mRadioIndex`=new,mRadioItems`=items},tb)
	selectradiomenuindexitem _ _ _ itemH tb
		= (itemH,tb)


//	Now do it!

::	ItemChange x
	:==	(	!Id
		,	x
		)
::	DeltaItemHandle x
	:==	x OSMenu Int MenuElementHandle` *OSToolbox -> *(MenuElementHandle`,*OSToolbox)
::	Recurse
	:==	(	!Bool	// Visit SubMenu elements
		,	!Bool	// Visit RadioMenu elements
		)

RecurseAll			:==	(True, True )
RecurseSubMenuOnly	:==	(True, False)
RecurseRadioOnly	:== (False,True )
RecurseNone			:==	(False,False)

changeMenuItems :: !Recurse (DeltaItemHandle x) ![ItemChange x] !*MState -> *MState
changeMenuItems recurse change changes {mRep=mH,mTb}
	# (_,items,_,tb)	= changeMenuItems` mH.mHandle` recurse change changes mH.mItems` 1 mTb
	= {mRep={mH & mItems`=items},mTb=tb}
where
	changeMenuItems` :: !OSMenu !Recurse (DeltaItemHandle x) ![ItemChange x] ![MenuElementHandle`] !Int !*OSToolbox
														 -> (![ItemChange x],![MenuElementHandle`],!Int,!*OSToolbox)
	changeMenuItems` menu recurse change changes mItems itemNr tb
		| isEmpty changes || isEmpty mItems
			= (changes,mItems,itemNr,tb)
		| otherwise
			# (h,hs)					= hdtl mItems
			# (changes,h, itemNr,tb)	= changeMenuItem`  menu recurse change changes h  itemNr tb
			# (changes,hs,itemNr,tb)	= changeMenuItems` menu recurse change changes hs itemNr tb
			= (changes,[h:hs],itemNr,tb)
	where
		changeMenuItem` :: !OSMenu !Recurse (DeltaItemHandle x) ![ItemChange x] !MenuElementHandle` !Int !*OSToolbox
															-> (![ItemChange x],!MenuElementHandle`,!Int,!*OSToolbox)
		
		changeMenuItem` menu recurse change changes h=:(MenuItemHandle` itemH=:{mItemId`}) itemNr tb
			# (anItem,(_,x),changes)	= case mItemId` of
											Nothing	-> (False,(undef,undef),changes)
											Just id	-> remove (eqfst2id id) (id,undef) changes
			| not anItem
				= (changes,h,itemNr+1,tb)
			| otherwise
				# (h,tb)				= change x menu itemNr h tb
				= (changes,h,itemNr+1,tb)
		
		changeMenuItem` menu recurse=:(visitSubMenus,_) change changes h=:(SubMenuHandle` subH=:{mSubHandle`,mSubMenuId`,mSubItems`}) itemNr tb
			# (anItem,(_,x),changes)	= case mSubMenuId` of
											Nothing	-> (False,(undef,undef),changes)
											Just id	-> remove (eqfst2id id) (id,undef) changes
			| not anItem && not visitSubMenus
				= (changes,h,itemNr+1,tb)
			# (changes,subItems,_,tb)	= changeMenuItems` mSubHandle` recurse change changes mSubItems` 1 tb
			  h							= SubMenuHandle` {subH & mSubItems`=subItems}
			| not anItem
				= (changes,h,itemNr+1,tb)
			| otherwise
				# (h,tb)				= change x menu itemNr h tb
				= (changes,h,itemNr+1,tb)
		
		changeMenuItem` menu recurse=:(_,visitRadios) change changes h=:(RadioMenuHandle` radioH=:{mRadioId`,mRadioItems`}) itemNr tb
			# (anItem,(_,x),changes)	= case mRadioId` of
											Nothing	-> (False,(undef,undef),changes)
											Just id	-> remove (eqfst2id id) (id,undef) changes
			  nrItems					= length mRadioItems`
			| not anItem && not visitRadios
				= (changes,h,itemNr+nrItems,tb)
			# (changes,items,_,tb)		= changeMenuItems` menu recurse change changes mRadioItems` itemNr tb
			  h							= RadioMenuHandle` {radioH & mRadioItems`=items}
			| not anItem
				= (changes,h,itemNr+nrItems,tb)
			| otherwise
				# (h,tb)				= change x menu itemNr h tb
				= (changes,h,itemNr+nrItems,tb)
		
		changeMenuItem` menu recurse change changes (MenuRecursiveHandle` items recKind) itemNr tb
			# (changes,items,itemNr,tb)	= changeMenuItems` menu recurse change changes items itemNr tb
			= (changes,MenuRecursiveHandle` items recKind,itemNr,tb)
		
		changeMenuItem` _ _ _ changes h=:(MenuReceiverHandle` _) itemNr tb
			= (changes,h,itemNr,tb)
		changeMenuItem` _ _ _ changes h=:(MenuSeparatorHandle` _) itemNr tb
			= (changes,h,itemNr+1,tb)


/*	Read access operations on MState:	*/

statemapMenuElementHandles` :: !(Cond x) (MenuElementHandle` x -> x) !x ![MenuElementHandle`] -> x
statemapMenuElementHandles` cond f s itemHs
	| cond s || isEmpty itemHs
		= s
	| otherwise
		# (itemH,itemHs)= hdtl itemHs
		# s				= statemapMenuElementHandle`  cond f s itemH
		# s				= statemapMenuElementHandles` cond f s itemHs
		= s
where
	statemapMenuElementHandle` :: (Cond x) (MenuElementHandle` x -> x) x !MenuElementHandle` -> x
	statemapMenuElementHandle` cond f s (MenuRecursiveHandle` itemHs _)	= statemapMenuElementHandles` cond f s itemHs
	statemapMenuElementHandle` cond f s itemH							= f itemH s


//	Yield the list of Ids and MenuElementTypes of all menu items of this Menu.

getMenuElementTypes :: !MState -> [(MenuElementType,Maybe Id)]
getMenuElementTypes mstate=:{mRep={mItems`}}
	= flatten (map getitemtypes mItems`)

getitemtypes :: !MenuElementHandle` -> [(MenuElementType,Maybe Id)]
getitemtypes (MenuItemHandle`      {mItemId`})		= [("MenuItem",		mItemId`)]
getitemtypes (MenuReceiverHandle`  {mReceiverId`})	= [("MenuReceiver",	Just mReceiverId`)]
getitemtypes (SubMenuHandle`       {mSubMenuId`})	= [("SubMenu",		mSubMenuId`)]
getitemtypes (RadioMenuHandle`     {mRadioId`})		= [("RadioMenu",	mRadioId`)]
getitemtypes (MenuSeparatorHandle` {mSepId`})		= [("MenuSeparator",mSepId`)]
getitemtypes (MenuRecursiveHandle` itemHs _)		= flatten (map getitemtypes  itemHs)


//	Yield the list of Ids and MenuElementTypes of all menu items of this (Sub/Radio)Menu.

getCompoundMenuElementTypes :: !Id !MState -> [(MenuElementType,Maybe Id)]
getCompoundMenuElementTypes id mstate=:{mRep={mItems`}}
	= snd (getcompmenuitemtypes id mItems`)
where
	getcompmenuitemtypes :: !Id ![MenuElementHandle`] -> (!Bool,![(MenuElementType,Maybe Id)])
	getcompmenuitemtypes id [itemH:itemHs]
		| inItem		= (True,ids)
		| otherwise		= getcompmenuitemtypes id itemHs
	where
		(inItem,ids)	= getsubtypes id itemH
		
		getsubtypes :: !Id !MenuElementHandle` -> (!Bool,![(MenuElementType,Maybe Id)])
		getsubtypes id (SubMenuHandle` {mSubMenuId`,mSubItems`})
			| isJust mSubMenuId` && fromJust mSubMenuId`==id	= (True,flatten (map getitemtypes mSubItems`))
			| otherwise											= getcompmenuitemtypes id mSubItems`
		getsubtypes id (RadioMenuHandle` {mRadioId`,mRadioItems`})
			| isJust mRadioId` && fromJust mRadioId`==id		= (True,flatten (map getitemtypes mRadioItems`))
			| otherwise											= getcompmenuitemtypes id mRadioItems`
		getsubtypes id (MenuRecursiveHandle` itemHs _)			= getcompmenuitemtypes id itemHs
		getsubtypes _ _											= (False,[])
	getcompmenuitemtypes _ _
		= (False,[])


// fst3thd3 :: !(.a,.b..c) -> (.a,.c)				// (t1,t3) of (t1,t2,t3)
fst3thd3 tuple :== (t1,t3) where (t1,_,t3) = tuple
// snd3thd3	:: !(.a,.b,.c) -> (.b,.c)				// (t2,t3) of (t1,t2,t3)
snd3thd3 tuple :== (t2,t3) where (_,t2,t3) = tuple
eqfst2id id1 (id2,_)	= id1==id2
eqfst3id id1 (id2,_,_)	= id1==id2


getSelectedRadioMenuItems :: ![Id] !MState -> [(!Index,!Maybe Id)]
getSelectedRadioMenuItems ids mstate=:{mRep={mItems`}}
	= map snd3thd3 (snd (getselectedradioitems mItems` (ids,map (\id->(id,defaultIndex,defaultValue)) ids)))
where
	defaultIndex	= 0
	defaultValue	= Nothing
	
	getselectedradioitems :: ![MenuElementHandle`] !(![Id],![(Id,Index,Maybe Id)]) -> (![Id],![(Id,Index,Maybe Id)])
	getselectedradioitems itemHs ids_selects
		= statemapMenuElementHandles` (isEmpty o fst) getradioitemselect ids_selects itemHs
	where
		getradioitemselect :: !MenuElementHandle` !(![Id],![(Id,Index,Maybe Id)]) -> (![Id],![(Id,Index,Maybe Id)])
		
		getradioitemselect (SubMenuHandle` itemH=:{mSubItems`}) ids_selects
			= getselectedradioitems mSubItems` ids_selects
		
		getradioitemselect (RadioMenuHandle` itemH=:{mRadioId`,mRadioItems`,mRadioIndex`}) ids_selects=:(ids,selects)
			# (hadId,id,ids)			= case mRadioId` of
											Nothing	-> (False,undef,ids)
											Just id	-> remove ((==) id) id ids
			| not hadId					= (ids,selects)
			| otherwise					= (ids,snd (creplace (eqfst3id id) (id,mRadioIndex`,selectid) selects))
		where
			selectid					= if (mRadioIndex`==0) Nothing mItemId`
			(MenuItemHandle` {mItemId`})= mRadioItems`!!(mRadioIndex`-1)
		
		getradioitemselect _ ids_selects
			= ids_selects

getSelectedRadioMenuItem :: !Id !MState -> (!Index,!Maybe Id)
getSelectedRadioMenuItem id mstate = hd (getSelectedRadioMenuItems [id] mstate)


getMenuElementSelectStates :: ![Id] !MState -> [(Bool,SelectState)]
getMenuElementSelectStates ids mstate=:{mRep={mItems`}}
	= map snd3thd3 (snd (getmenuitemsselect mItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool	= False
	defaultValue= Able
	
	getmenuitemsselect :: ![MenuElementHandle`] !(![Id],![(Id,Bool,SelectState)]) -> (![Id],![(Id,Bool,SelectState)])
	getmenuitemsselect itemHs ids_selects
		= statemapMenuElementHandles` (isEmpty o fst) getmenuitemselect ids_selects itemHs
	where
		getmenuitemselect :: !MenuElementHandle` !(![Id],![(Id,Bool,SelectState)]) -> (![Id],![(Id,Bool,SelectState)])
		
		getmenuitemselect (SubMenuHandle` itemH=:{mSubMenuId`}) (ids,selects)
			# (hadId,id,ids)	= case mSubMenuId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			  (ids,selects)		= getmenuitemsselect itemH.mSubItems` (ids,selects)
			| not hadId			= (ids,selects)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,if itemH.mSubSelect` Able Unable) selects))
		
		getmenuitemselect (RadioMenuHandle` itemH=:{mRadioId`}) (ids,selects)
			# (hadId,id,ids)	= case mRadioId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			  (ids,selects)		= getmenuitemsselect itemH.mRadioItems` (ids,selects)
			| not hadId			= (ids,selects)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,if itemH.mRadioSelect` Able Unable) selects))
		
		getmenuitemselect (MenuItemHandle` itemH=:{mItemId`}) (ids,selects)
			# (hadId,id,ids)	= case mItemId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			| not hadId			= (ids,selects)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,if itemH.mItemSelect` Able Unable) selects))
		
		getmenuitemselect _ ids_selects
			= ids_selects

getMenuElementSelectState :: !Id !MState -> (Bool,SelectState)
getMenuElementSelectState id mstate = hd (getMenuElementSelectStates [id] mstate)


getMenuElementMarkStates :: ![Id] !MState -> [(Bool,MarkState)]
getMenuElementMarkStates ids mstate=:{mRep={mItems`}}
	= map snd3thd3 (snd (getmenuitemsmark mItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool		= False
	defaultValue	= NoMark
	
	getmenuitemsmark :: ![MenuElementHandle`] !(![Id],![(Id,Bool,MarkState)]) -> (![Id],![(Id,Bool,MarkState)])
	getmenuitemsmark itemHs ids_marks
		= statemapMenuElementHandles` (isEmpty o fst) getmenuitemmark ids_marks itemHs
	where
		getmenuitemmark :: !MenuElementHandle` !(![Id],![(Id,Bool,MarkState)]) -> (![Id],![(Id,Bool,MarkState)])
		
		getmenuitemmark (SubMenuHandle` {mSubItems`}) ids_marks
			= getmenuitemsmark mSubItems` ids_marks
		
		getmenuitemmark (MenuItemHandle` itemH=:{mItemId`}) (ids,marks)
			# (hadId,id,ids)	= case mItemId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			| not hadId			= (ids,marks)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,if itemH.mItemMark` Mark NoMark) marks))
		
		getmenuitemmark _ ids_marks
			= ids_marks

getMenuElementMarkState :: !Id !MState -> (Bool,MarkState)
getMenuElementMarkState id mstate = hd (getMenuElementMarkStates [id] mstate)


getMenuElementTitles :: ![Id] !MState -> [(Bool,Maybe String)]
getMenuElementTitles ids mstate=:{mRep={mItems`}}
	= map snd3thd3 (snd (getmenuitemstitle mItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool		= False
	defaultValue	= Nothing
	
	getmenuitemstitle :: ![MenuElementHandle`] !(![Id],![(Id,Bool,Maybe String)]) -> ([Id],![(Id,Bool,Maybe String)])
	getmenuitemstitle itemHs ids_titles
		= statemapMenuElementHandles` (isEmpty o fst) getmenuitemtitle ids_titles itemHs
	where
		getmenuitemtitle :: !MenuElementHandle` !(![Id],![(Id,Bool,Maybe String)]) -> (![Id],![(Id,Bool,Maybe String)])
		
		getmenuitemtitle (SubMenuHandle` itemH=:{mSubMenuId`}) (ids,titles)
			# (hadId,id,ids)	= case mSubMenuId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			  (ids,titles)		= getmenuitemstitle itemH.mSubItems` (ids,titles)
			| not hadId			= (ids,titles)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,Just itemH.mSubTitle`) titles))
		
		getmenuitemtitle (RadioMenuHandle` itemH=:{mRadioId`}) (ids,titles)
			# (hadId,id,ids)	= case mRadioId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			  (ids,titles)		= getmenuitemstitle itemH.mRadioItems` (ids,titles)
			| not hadId			= (ids,titles)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,Nothing) titles))
		
		getmenuitemtitle (MenuItemHandle` itemH=:{mItemId`}) (ids,titles)
			# (hadId,id,ids)	= case mItemId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			| not hadId			= (ids,titles)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,Just itemH.mItemTitle`) titles))
		
		getmenuitemtitle _ ids_titles
			= ids_titles

getMenuElementTitle :: !Id !MState -> (Bool,Maybe String)
getMenuElementTitle id mstate = hd (getMenuElementTitles [id] mstate)


getMenuElementShortKeys :: ![Id] !MState -> [(Bool,Maybe Char)]
getMenuElementShortKeys ids mstate=:{mRep={mItems`}}
	= map snd3thd3 (snd (getmenuitemsshortkey mItems` (ids,map (\id->(id,defaultBool,defaultValue)) ids)))
where
	defaultBool		= False
	defaultValue	= Nothing
	
	getmenuitemsshortkey :: ![MenuElementHandle`] !(![Id],![(Id,Bool,Maybe Char)]) -> (![Id],![(Id,Bool,Maybe Char)])
	getmenuitemsshortkey itemHs ids_keys
		= statemapMenuElementHandles` (isEmpty o fst) getmenuitemshortkey ids_keys itemHs
	where
		getmenuitemshortkey :: !MenuElementHandle` !(![Id],![(Id,Bool,Maybe Char)]) -> (![Id],![(Id,Bool,Maybe Char)])
		
		getmenuitemshortkey (SubMenuHandle` {mSubItems`}) ids_keys
			= getmenuitemsshortkey mSubItems` ids_keys
		
		getmenuitemshortkey (MenuItemHandle` {mItemId`,mItemKey`}) (ids,keys)
			# (hadId,id,ids)	= case mItemId` of
									Nothing	-> (False,undef,ids)
									Just id	-> remove ((==) id) id ids
			| not hadId			= (ids,keys)
			| otherwise			= (ids,snd (creplace (eqfst3id id) (id,True,mItemKey`) keys))
		
		getmenuitemshortkey _ ids_keys
			= ids_keys

getMenuElementShortKey :: !Id !MState -> (Bool,Maybe Char)
getMenuElementShortKey id mstate = hd (getMenuElementShortKeys [id] mstate)
