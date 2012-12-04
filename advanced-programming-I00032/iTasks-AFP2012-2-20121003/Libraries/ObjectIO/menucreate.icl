implementation module menucreate


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	StdMenuElementClass, StdPSt
import	commondef, devicesystemstate, iostate, menuaccess, menudefaccess, menuhandle, sdisize
import	osmenu


menucreateFatalError :: String String -> .x
menucreateFatalError rule error
	= fatalError rule "menucreate" error


/*	Creating menus:
		Because in a SDI process menus might be added to the process window, the ViewFrame of the
		process window can change size.
		In that case, the layout of the controls should be recalculated, and the window updated.
	OpenMenu` assumes that the Id argument has been verified and that the MenuDevice exists.
*/
openMenu` :: !Id .ls !.(Menu m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | MenuElements m
openMenu` menuId ls (Menu title items atts) pState=:{io=ioState}
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar	// This condition should never hold
		= menucreateFatalError "openMenu (Menu)" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (idtable,ioState)			= ioStGetIdTable ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= menucreateFatalError "openMenu (Menu)" "MenuDevice not found"
	# mHs						= menuSystemStateGetMenuHandles mDevice
	# (menus,mHs)				= menuHandlesGetMenus mHs
	  (exists,menus)			= ucontains (isMenuWithThisId menuId) menus
	| exists					// This condition should never hold
		= menucreateFatalError "openMenu (Menu)" "inconsistency detected between IdTable and ReceiverTable"
	# (nrmenus,menus)			= ulength menus
	# index						= getMenuIndexAtt (snd (cselect isMenuIndex (MenuIndex nrmenus) atts))
	  (hasMenuWindowMenu,menus)	= ucontains (isMenuWithThisId windowMenuId) menus
	  index`					= setBetween index 0 (max 0 (if hasMenuWindowMenu (nrmenus-1) nrmenus))
	# (rt,ioState)				= ioStGetReceiverTable ioState
	# (ioid,ioState)			= ioStGetIOId ioState
	# (sdiSize1,sdiPtr,ioState)	= getSDIWindowSize ioState
	# pState					= {pState & io=ioState}
	  (mKeys,mHs)				= menuHandlesGetKeys mHs
	# (ok,mH,mKeys,rt,idtable,osMenuBar,pState)
								= createMenu index` ioid menuId (Menu title items atts) mKeys rt idtable osMenuBar pState
 	  mHs						= menuHandlesSetKeys mKeys mHs
 	# ioState					= ioStSetReceiverTable rt pState.io
 	# ioState					= ioStSetIdTable idtable ioState
 	# osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo			// DvA
	# ioState					= ioStSetOSDInfo osdInfo ioState				// DvA
	| not ok
		# mHs					= menuHandlesSetMenus menus mHs
		# ioState				= ioStSetDevice (MenuSystemState mHs) ioState
		= (ErrorIdsInUse,{pState & io=ioState})
	| otherwise
		= (NoError,pState2)
	with
		(before,after)			= splitAt index` menus
		msH						= MenuLSHandle {mlsState=ls1,mlsHandle=mH}
		mHs1					= {mHs & mMenus=before++[msH:after]}
		ioState1				= appIOToolbox (osDrawMenuBar osMenuBar) ioState
		ioState2				= ioStSetDevice (MenuSystemState mHs1) ioState1
		ioState3				= checkSDISize sdiPtr sdiSize1 ioState2
		pState1					= {pState & io=ioState3}
		(ls1,pState2)			= menuInit (ls,pState1)
where
	menuInit					= getMenuInitFun (snd (cselect isMenuInit (MenuInit id) atts))
	
	checkSDISize :: !OSWindowPtr !Size !(IOSt .l) -> IOSt .l
	checkSDISize sdiPtr sdiSize1 ioState
		# (sdiSize2,_,ioState)	= getSDIWindowSize ioState
		| sdiSize1==sdiSize2	= ioState
		| otherwise				= resizeSDIWindow sdiPtr sdiSize1 sdiSize2 ioState
	
	createMenu :: !Int !SystemId !Id !(Menu m .ls (PSt .l)) ![Char] !*ReceiverTable !*IdTable !OSMenuBar !(PSt .l)
						 -> (!Bool,MenuHandle .ls (PSt .l), ![Char],!*ReceiverTable,!*IdTable,!OSMenuBar, !PSt .l)
						 |  MenuElements m
	createMenu index ioId menuId (Menu title items atts) keys rt it osMenuBar pState
		# (ms,pState)				= menuElementToHandles items pState
		  itemHs					= map menuElementStateToMenuElementHandle ms
		  (ok,itemHs,rt,it)			= menuIdsAreConsistent ioId menuId itemHs rt it
		| not ok
			= (False,undef,keys,rt,it,osMenuBar,pState)
		| otherwise
			# (tb,ioState)			= getIOToolbox pState.io
			# (menu,mH,osMenuBar,tb)= NewMenuHandle title atts index menuId osMenuBar tb
			# (_,itemHs,keys,tb)	= createMenuElements osMenuBar menu 1 itemHs keys tb
			  mH					= {mH & mItems=itemHs}
			# ioState				= setIOToolbox tb ioState
			# pState				= {pState & io=ioState}
			  (_,it)				= addIdToIdTable menuId {idpIOId=ioId,idpDevice=MenuDevice,idpId=menuId} it
			= (True,mH,keys,rt,it,osMenuBar,pState)

isMenuWithThisId :: !Id !*(MenuStateHandle .pst) -> (!Bool,!*MenuStateHandle .pst)
isMenuWithThisId id msH
	# (id`,msH)	= menuStateHandleGetMenuId msH
	= (id==id`,msH)

/*	Creating pop up menus.
	It is assumed that MenuHandles contains no pop up menu in mMenus and that mPopUpId contains an Id.
*/
createPopUpMenu :: !SystemId .ls !.(PopUpMenu m .ls (PSt .l)) !(MenuHandles (PSt .l)) !*ReceiverTable !*IdTable !OSMenuBar !(PSt .l)
													 -> (!Bool,!MenuHandles (PSt .l), !*ReceiverTable,!*IdTable,!OSMenuBar, !PSt .l)
													 |  PopUpMenuElements m
createPopUpMenu ioId ls (PopUpMenu items) mHs=:{mMenus, mKeys=keys, mPopUpId} rt it osMenuBar pState
	# (ms,pState)			= popUpMenuElementToHandles items pState
	  itemHs				= map menuElementStateToMenuElementHandle ms
	  menuId				= fromJust mPopUpId
	  (ok,itemHs,rt,it)		= menuIdsAreConsistent ioId menuId itemHs rt it
	| not ok
		= (False,mHs,rt,it,osMenuBar,pState)
	| otherwise
		# (tb,ioState)		= getIOToolbox pState.io
		# (menu,tb)			= osCreatePopUpMenu tb
		# (_,itemHs,keys,tb)= createMenuElements osMenuBar menu 1 itemHs keys tb
		  itemHs			= map validatePopUpMenuFunction itemHs
		  mlsH				= {	mHandle		= menu
							  , mMenuId		= menuId
							  ,	mOSMenuNr	= 0
							  ,	mTitle		= ""
							  ,	mSelect		= True
							  ,	mItems		= itemHs
							  }
		  msH				= MenuLSHandle {mlsState=ls,mlsHandle=mlsH}
		  mHs				= {mHs & mMenus=[msH:mMenus], mKeys=keys, mPopUpId=Nothing}
		# ioState			= setIOToolbox tb ioState
		# pState			= {pState & io=ioState}
		= (True,mHs,rt,it,osMenuBar,pState)
where
/*	validatePopUpMenuFunction takes care that all Menu(Mods)Function arguments of the elements
	apply closePopUpMenu after their own action.
*/
	validatePopUpMenuFunction :: !(MenuElementHandle .ls (PSt .l)) -> MenuElementHandle .ls (PSt .l)
	validatePopUpMenuFunction (MenuItemHandle itemH=:{mItemAtts})
		= MenuItemHandle {itemH & mItemAtts=map validateMenuFunction mItemAtts}
	where
		validateMenuFunction :: !(MenuAttribute *(.ls,PSt .l)) -> MenuAttribute *(.ls,PSt .l)
		validateMenuFunction (MenuFunction f)
			= MenuFunction (f` f)
		where
			f` :: (IdFun *(.ls,PSt .l)) !*(.ls,PSt .l) -> *(.ls,PSt .l)
			f` f (ls,pst)
				# (ls,pst)	= f (ls,pst)
				= (ls,closePopUpMenu pst)
		validateMenuFunction (MenuModsFunction f)
			= MenuModsFunction (f` f)
		where
			f` :: (ModifiersFunction *(.ls,PSt .l)) !Modifiers !*(.ls,PSt .l) -> *(.ls,PSt .l)
			f` f modifiers (ls,pst)
				# (ls,pst)	= f modifiers (ls,pst)
				= (ls,closePopUpMenu pst)
		validateMenuFunction att
			= att
	validatePopUpMenuFunction (MenuReceiverHandle _)
		= menucreateFatalError "validatePopUpMenuFunction" "Receiver(2) should not be an element of PopUpMenus"
	validatePopUpMenuFunction (SubMenuHandle submH=:{mSubItems})
//		= menucreateFatalError "validatePopUpMenuFunction" "SubMenu should not be an element of PopUpMenus"
		= SubMenuHandle {submH & mSubItems = map validatePopUpMenuFunction mSubItems}
	validatePopUpMenuFunction (RadioMenuHandle radioH=:{mRadioItems})
		= RadioMenuHandle {radioH & mRadioItems=map validatePopUpMenuFunction mRadioItems}
	validatePopUpMenuFunction (MenuSeparatorHandle separatorH)
		= MenuSeparatorHandle separatorH
	validatePopUpMenuFunction (MenuListLSHandle itemHs)
		= MenuListLSHandle (map validatePopUpMenuFunction itemHs)
	validatePopUpMenuFunction (MenuExtendLSHandle mExH=:{mExtendItems})
		= MenuExtendLSHandle {mExH & mExtendItems=map validatePopUpMenuFunction mExtendItems}
	validatePopUpMenuFunction (MenuChangeLSHandle mChH=:{mChangeItems})
		= MenuChangeLSHandle {mChH & mChangeItems=map validatePopUpMenuFunction mChangeItems}
	
/*	closePopUpMenu takes care that the internal administration of the menus is restored again to
	no open pop up menu. It is assumed that all resources have been freed.
*/
	closePopUpMenu :: !(PSt .l) -> PSt .l
	closePopUpMenu pState=:{io}
		# (found,mDevice,ioState)	= ioStGetDevice MenuDevice io
		| not found
			= menucreateFatalError "closePopUpMenu" "could not retrieve MenuSystemState from IOSt"
		| otherwise
			# mHs					= menuSystemStateGetMenuHandles mDevice
			  mHs					= closepopupmenu mHs
			# ioState				= ioStSetDevice (MenuSystemState mHs) ioState
			= {pState & io=ioState}


/*	Creating menu elements: retrieving toolbox handles and ids for elements, and building the menu gui.
*/
createMenuElements :: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
									 -> (!Int,![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
createMenuElements osmenubar menu iNr itemHs keys tb
	# (empty,itemHs)			= uisEmpty itemHs
	| empty
		= (iNr,itemHs,keys,tb)
	| otherwise
		# (itemH,itemHs)		= hdtl itemHs
		# (iNr,itemH, keys,tb)	= createMenuElement  osmenubar menu iNr itemH keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,[itemH:itemHs],keys,tb)
where
	createMenuElement :: !OSMenuBar !OSMenu !Int !(MenuElementHandle .ls .pst) ![Char] !*OSToolbox
										-> (!Int, !MenuElementHandle .ls .pst, ![Char],!*OSToolbox)
	createMenuElement osmenubar menu iNr (SubMenuHandle subH) keys tb
		# (subH,tb)				= NewSubMenuHandle subH iNr menu tb
		# (_,itemHs,keys,tb)	= createMenuElements osmenubar subH.mSubHandle 1 subH.mSubItems keys tb
		  subH					= {subH & mSubItems=itemHs}
		= (iNr+1,SubMenuHandle subH,keys,tb)
	createMenuElement osmenubar menu iNr (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,RadioMenuHandle {itemH & mRadioItems=itemHs},keys,tb)
	createMenuElement osmenubar menu iNr (MenuItemHandle itemH) keys tb
		# (itemH,keys)			= checkshortcutkey itemH keys
		# (osMenuItem,itemH,tb)	= insertMenu osmenubar menu iNr itemH tb
		  itemH					= {itemH & mOSMenuItem=osMenuItem}
		= (iNr+1,MenuItemHandle itemH,keys,tb)
	createMenuElement osmenubar menu iNr (MenuSeparatorHandle itemH) keys tb
		# (osMenuSeparator,_,tb)= osAppendMenuSeparator iNr menu tb
		  itemH					= {itemH & mOSMenuSeparator=osMenuSeparator}
		= (iNr+1,MenuSeparatorHandle itemH,keys,tb)
	createMenuElement _ _ iNr itemH=:(MenuReceiverHandle _) keys tb
		= (iNr,itemH,keys,tb)
	createMenuElement osmenubar menu iNr (MenuListLSHandle itemHs) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuListLSHandle itemHs,keys,tb)
	createMenuElement osmenubar menu iNr (MenuExtendLSHandle exH=:{mExtendItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuExtendLSHandle {exH & mExtendItems=itemHs},keys,tb)
	createMenuElement osmenubar menu iNr (MenuChangeLSHandle chH=:{mChangeItems=itemHs}) keys tb
		# (iNr,itemHs,keys,tb)	= createMenuElements osmenubar menu iNr itemHs keys tb
		= (iNr,MenuChangeLSHandle {chH & mChangeItems=itemHs},keys,tb)
	createMenuElement _ _ _ _ _ _
		= menucreateFatalError "createMenuElements" "unmatched MenuElementHandle"


/*	Extend an existing menu with new menu elements.
*/
extendMenu :: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
															    -> (![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
extendMenu osmenubar menu iNr itemHs items keys tb
	# (empty,itemHs)		= uisEmpty itemHs
	| empty
		= (items,keys,tb)
	| otherwise
		# (itemH,itemHs)	= hdtl itemHs
		# (items,keys,tb)	= extendMenu  osmenubar menu iNr itemHs items keys tb
		# (itemH,keys,tb)	= extendMenu` osmenubar menu iNr itemH        keys tb
		= ([itemH:items],keys,tb)
where
	extendMenu` :: !OSMenuBar !OSMenu !Int !(MenuElementHandle .ls .pst) ![Char] !*OSToolbox
										-> (!MenuElementHandle .ls .pst, ![Char],!*OSToolbox)
	extendMenu` osmenubar menu iNr (SubMenuHandle subH) keys tb
		# (subH,tb)						= NewSubMenuHandle subH iNr menu tb
		# (_,itemHs,keys,tb)			= createMenuElements osmenubar subH.mSubHandle 1 subH.mSubItems keys tb
		  subH							= {subH & mSubItems=itemHs}
		= (SubMenuHandle subH,keys,tb)
	extendMenu` osmenubar menu iNr (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys tb
		# (_,itemHs,keys,tb)			= createMenuElements osmenubar menu iNr itemHs keys tb
		= (RadioMenuHandle {itemH & mRadioItems=itemHs},keys,tb)
	extendMenu` osmenubar menu iNr (MenuItemHandle itemH) keys tb
		# (itemH,keys)					= checkshortcutkey itemH keys
		# (osMenuItem,itemH,tb)			= insertMenu osmenubar menu iNr itemH tb
		= (MenuItemHandle {itemH & mOSMenuItem=osMenuItem},keys,tb)
	extendMenu` osmenubar menu iNr (MenuSeparatorHandle itemH) keys tb
		# (osMenuItem,_,tb)				= osAppendMenuSeparator iNr menu tb
		= (MenuSeparatorHandle {itemH & mOSMenuSeparator=osMenuItem},keys,tb)
	extendMenu` _ _ _ itemH=:(MenuReceiverHandle _) keys tb
		= (itemH,keys,tb)
	extendMenu` osmenubar menu iNr (MenuListLSHandle itemHs) keys tb
		# (itemHs,keys,tb)				= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuListLSHandle itemHs,keys,tb)
	extendMenu` osmenubar menu iNr (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) keys tb
		# (itemHs,keys,tb)				= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},keys,tb)
	extendMenu` osmenubar menu iNr (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) keys tb
		# (itemHs,keys,tb)	= extendMenu osmenubar menu iNr itemHs [] keys tb
		= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},keys,tb)
	extendMenu` _ _ _ _ _ _
		= menucreateFatalError "extendMenu" "unmatched MenuElementHandle"


insertMenu :: !OSMenuBar !OSMenu !Int !*(MenuItemHandle .ls .pst) !*OSToolbox -> (!OSMenuItem,!*MenuItemHandle .ls .pst,!*OSToolbox)
insertMenu osmenubar menu iNr itemH=:{mItemKey,mItemTitle,mItemSelect,mItemMark,mItemAtts} tb
	# (osMenuItem,_,tb)	= osAppendMenuItem osmenubar iNr menu mItemTitle mItemSelect mItemMark shortcut tb
	= (osMenuItem,itemH,tb)
where
	shortcut			= case mItemKey of
							(Just key)	-> key
							_			-> '\0'

checkshortcutkey :: !(MenuItemHandle .ls .pst) ![Char] -> (!MenuItemHandle .ls .pst,![Char])
checkshortcutkey mItemH=:{mItemKey} cs
	| isNothing mItemKey	= ( mItemH,cs)
	| isMember c cs			= ({mItemH & mItemKey=Nothing},cs)
	| otherwise				= ( mItemH,[c:cs])
where
	c						= fromJust mItemKey


//	Creation and manipulation of Menu(Element)Handles:


SystemAble			:== True
SystemUnable		:== False

//	Initialisation and Allocation:

NewMenuHandle :: !String ![MenuAttribute *(.ls,.pst)] !Int !Id !OSMenuBar !*OSToolbox -> (!OSMenu,!MenuHandle .ls .pst,!OSMenuBar,!*OSToolbox)
NewMenuHandle title atts index menuId menuBar tb
	# (ok,osMenuNr,tb)	= osNewMenuNr tb		// PA: generation of internal menu numbers added
	| not ok
		= menucreateFatalError "NewMenuHandle" "To many Menus created for one interactive process"
	# (menu,menuBar,tb)	= osMenuInsert index osMenuNr title menuBar tb
	  select			= getMenuSelectStateAtt (snd (cselect isMenuSelectState (MenuSelectState Able) atts))
	  mH				= {	mHandle		= menu
						  , mMenuId		= menuId
						  ,	mOSMenuNr	= osMenuNr
						  ,	mTitle		= title
						  ,	mSelect		= enabled select
						  ,	mItems		= []
						  }
	| enabled select
		= (menu,mH,menuBar,osEnableMenu  index menuBar tb)
	| otherwise
		= (menu,mH,menuBar,osDisableMenu index menuBar tb)

//	PA: New version of creating a SubMenu:
NewSubMenuHandle :: !(SubMenuHandle .ls .pst) !Int !OSMenu !*OSToolbox -> (!SubMenuHandle .ls .pst,!*OSToolbox)
NewSubMenuHandle mH=:{mSubTitle,mSubSelect} index menu tb
	# (ok,osMenuNr,tb)	= osNewSubMenuNr tb
	| not ok
		= menucreateFatalError "NewSubMenuHandle" "To many SubMenus created for one interactive process"
	# (osH,_,tb)		= osSubMenuInsert index osMenuNr mSubTitle menu tb
	# mH				= {mH & mSubHandle=osH,mSubOSMenuNr=osMenuNr}
	| mSubSelect
		= (mH,osEnableMenuItem  menu osH index tb)
	| otherwise
		= (mH,osDisableMenuItem menu osH index tb)

closepopupmenu :: !(MenuHandles .pst) -> MenuHandles .pst
closepopupmenu mHs=:{mMenus,mPopUpId}
	| isJust mPopUpId
		= mHs
	| otherwise
		# (msH,msHs)	= hdtl mMenus
		  (id,_)		= menuStateHandleGetMenuId msH
		= {mHs & mMenus=msHs,mPopUpId=Just id}

disposeMenuItemHandle :: !OSMenu !Int !(MenuItemHandle .ls .pst) !(![Char],!*IdTable,!*OSToolbox)
								   -> (!MenuItemHandle .ls .pst, !(![Char],!*IdTable,!*OSToolbox))
disposeMenuItemHandle menu iNr itemH=:{mItemKey,mItemId,mOSMenuItem} (keys,it,tb)
	# tb		= snd (osMenuRemoveItem mOSMenuItem iNr menu tb)
	| isJust mItemId
		# it	= snd (removeIdFromIdTable (fromJust mItemId) it)
		= (itemH,(keys`,it,tb))
	| otherwise
		= (itemH,(keys`,it,tb))
where
	keys`		= if (isJust mItemKey) [fromJust mItemKey:keys] keys

// PA: deze functie doet niks!! Alternatief toegevoegd om daadwerkelijk SubMenu's te verwijderen.
disposeSubMenuHandles :: !(MenuElementHandle .ls .pst) !(!OSMenu,!*OSToolbox)
					  -> (!MenuElementHandle .ls .pst, !(!OSMenu,!*OSToolbox))
disposeSubMenuHandles itemH (parentH,tb)
	# (itemH,(parentH,_,tb))	= disposeSubMenuHandles` itemH (parentH,1,tb)
	= (itemH,(parentH,tb))
where
	disposeSubMenuHandles` :: !(MenuElementHandle .ls .pst) !(!OSMenu,!Int,!*OSToolbox)
						   -> (!MenuElementHandle .ls .pst, !(!OSMenu,!Int,!*OSToolbox))
	disposeSubMenuHandles` (SubMenuHandle subH=:{mSubHandle=mSubH,mSubOSMenuNr=mSubNr,mSubItems=itemHs}) (parentH,iNr,tb)	// PA: alternative added to really dispose submenus
		# (itemHs,(mSubH,_,tb))	= stateMap disposeSubMenuHandles` itemHs (mSubH,1,tb)
		# (parentH,tb)			= osSubMenuRemove mSubH parentH mSubNr iNr tb
		= (SubMenuHandle {subH & mSubHandle=mSubH,mSubItems=itemHs},(parentH,iNr+1,tb))
	disposeSubMenuHandles` (MenuListLSHandle mListItems) parentH_tb
		# (itemHs,parentH_tb)	= stateMap disposeSubMenuHandles` mListItems parentH_tb
		= (MenuListLSHandle itemHs,parentH_tb)
	disposeSubMenuHandles` (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) parentH_tb
		# (itemHs,parentH_tb)	= stateMap disposeSubMenuHandles` itemHs parentH_tb
		= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},parentH_tb)
	disposeSubMenuHandles` (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) parentH_tb
		# (itemHs,parentH_tb)	= stateMap disposeSubMenuHandles` itemHs parentH_tb
		= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},parentH_tb)
	disposeSubMenuHandles` itemH=:(MenuItemHandle _) (parentH,iNr,tb)
		= (itemH,(parentH,iNr+1,tb))
	disposeSubMenuHandles` itemH=:(RadioMenuHandle radioH=:{mRadioItems=itemHs}) (parentH,iNr,tb)
		# (nrRadio,itemHs)		= ulength itemHs
		= (RadioMenuHandle {radioH & mRadioItems=itemHs},(parentH,iNr+nrRadio,tb))
	disposeSubMenuHandles` itemH st
		= (itemH,st)

disposeShortcutkeys :: !OSWindowPtr !(MenuElementHandle .ls .pst) !(![Char],!*OSToolbox)
								 -> (!MenuElementHandle .ls .pst, !(![Char],!*OSToolbox))
disposeShortcutkeys framePtr (MenuItemHandle itemH=:{mItemKey,mOSMenuItem}) (keys,tb)
//	PA: this should still occur. osRemoveMenuShortKey has a dummy implementation on Mac, but administration should be OK.
	| isJust mItemKey
		= (MenuItemHandle itemH,(thd3 (remove ((==) key) key keys),osRemoveMenuShortKey framePtr mOSMenuItem tb))
	with
		key = fromJust mItemKey
	| otherwise
		= (MenuItemHandle itemH,(keys,tb))
disposeShortcutkeys framePtr (SubMenuHandle itemH=:{mSubItems=itemHs}) keys_tb
	# (itemHs,keys_tb)	= stateMap (disposeShortcutkeys framePtr) itemHs keys_tb
	= (SubMenuHandle {itemH & mSubItems=itemHs},keys_tb)
disposeShortcutkeys framePtr (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys_tb
	# (itemHs,keys_tb)	= stateMap (disposeShortcutkeys framePtr) itemHs keys_tb
	= (RadioMenuHandle {itemH & mRadioItems=itemHs},keys_tb)
disposeShortcutkeys framePtr (MenuListLSHandle itemHs) keys_tb
	# (itemHs,keys_tb)	= stateMap (disposeShortcutkeys framePtr) itemHs keys_tb
	= (MenuListLSHandle itemHs,keys_tb)
disposeShortcutkeys framePtr (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) keys_tb
	# (itemHs,keys_tb)	= stateMap (disposeShortcutkeys framePtr) itemHs keys_tb
	= (MenuExtendLSHandle {mExH & mExtendItems=itemHs},keys_tb)
disposeShortcutkeys framePtr (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) keys_tb
	# (itemHs,keys_tb)	= stateMap (disposeShortcutkeys framePtr) itemHs keys_tb
	= (MenuChangeLSHandle {mChH & mChangeItems=itemHs},keys_tb)
disposeShortcutkeys _ itemH keys_tb
	= (itemH,keys_tb)

disposeMenuIds :: !SystemId !(MenuElementHandle .ls .pst) !(!*ReceiverTable,!*IdTable)
						 -> (!MenuElementHandle .ls .pst, !(!*ReceiverTable,!*IdTable))
disposeMenuIds pid (MenuItemHandle itemH=:{mItemId}) (rt,it)
	| isNothing mItemId
		= (MenuItemHandle itemH,(rt,it))
	| otherwise
		= (MenuItemHandle itemH,(rt,snd (removeIdFromIdTable (fromJust mItemId) it)))
disposeMenuIds pid (MenuReceiverHandle itemH=:{mReceiverHandle={rId}}) (rt,it)
	= (MenuReceiverHandle itemH,(snd (removeReceiverFromReceiverTable rId rt),snd (removeIdFromIdTable rId it)))
disposeMenuIds pid (SubMenuHandle itemH=:{mSubItems=itemHs}) ts
	# (itemHs,ts)		= stateMap (disposeMenuIds pid) itemHs ts
	= (SubMenuHandle {itemH & mSubItems=itemHs},ts)
disposeMenuIds pid (RadioMenuHandle itemH=:{mRadioId,mRadioItems=itemHs}) ts
	# (itemHs,(rt,it))	= stateMap (disposeMenuIds pid) itemHs ts
	# radioH			= RadioMenuHandle {itemH & mRadioItems=itemHs}
	| isNothing mRadioId
		= (radioH,(rt,it))
	| otherwise
		= (radioH,(rt,snd (removeIdFromIdTable (fromJust mRadioId) it)))
disposeMenuIds pid (MenuSeparatorHandle itemH=:{mSepId}) (rt,it)
	| isNothing mSepId
		= (MenuSeparatorHandle itemH,(rt,it))
	| otherwise
		= (MenuSeparatorHandle itemH,(rt,snd (removeIdFromIdTable (fromJust mSepId) it)))
disposeMenuIds pid (MenuListLSHandle itemHs) ts
	# (itemHs,ts)	= stateMap (disposeMenuIds pid) itemHs ts
	= (MenuListLSHandle itemHs,ts)
disposeMenuIds pid (MenuExtendLSHandle itemH=:{mExtendItems=itemHs}) ts
	# (itemHs,ts)	= stateMap (disposeMenuIds pid) itemHs ts
	= (MenuExtendLSHandle {itemH & mExtendItems=itemHs},ts)
disposeMenuIds pid (MenuChangeLSHandle itemH=:{mChangeItems=itemHs}) ts
	# (itemHs,ts)	= stateMap (disposeMenuIds pid) itemHs ts
	= (MenuChangeLSHandle {itemH & mChangeItems=itemHs},ts)

disposeMenuHandles :: !Bool !(MenuHandles .pst) !(!OSMenuBar,!*OSToolbox)
						 -> (!MenuHandles .pst, !(!OSMenuBar,!*OSToolbox))
disposeMenuHandles _ menus=:{mMenus} (osMenuBar,tb)
	# (mHs,(osMenuBar,tb))	= stateMap dispose mMenus (osMenuBar,tb)
	= ({menus & mMenus=mHs},(osMenuBar,tb))
where
	dispose :: !(MenuStateHandle .pst) !(!OSMenuBar,!*OSToolbox) -> (!MenuStateHandle .pst,(!OSMenuBar,!*OSToolbox))
	dispose msH (osMenuBar,tb)
		# (menuH,msH)	= menuStateHandleGetHandle msH
		= (msH,osMenuRemove menuH osMenuBar tb)
