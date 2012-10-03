implementation module menuevent


import	StdBool, StdList, StdMisc
import	clCrossCall_12
from	clCCall_12			import winMakeCString, :: CSTR, toModifiers
from	osmenu				import osMenuItemCheck, :: OSTrackPopUpMenu{..}, :: OSTrackPopUpMenuResult(..)
import	commondef, deviceevents, iostate
from	menuaccess			import menuStateHandleGetHandle, menuStateHandleGetMenuId
from	processstack		import topShowProcessShowState
from	StdProcessAttribute	import getProcessToolbarAtt, isProcessToolbar
from	StdPSt				import accPIO


menueventFatalError :: String String -> .x
menueventFatalError function error
	= fatalError function "menuevent" error


/*	menuEvent filters the scheduler events that can be handled by this menu device.
	For the time being no timer menu elements are added, so these events are ignored.
	menuEvent assumes that it is not applied to an empty IOSt and that its device is
	present.
*/
menuEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
menuEvent schedulerEvent pState
	# (hasMenuDevice,pState)	= accPIO (ioStHasDevice MenuDevice) pState
	| not hasMenuDevice			// This condition should never hold
		= menueventFatalError "menuEvent" "MenuDevice.dEvent applied while MenuDevice not present in IOSt"
	| otherwise
		= menuEvent schedulerEvent pState
where
	menuEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
	menuEvent schedulerEvent=:(ScheduleOSEvent osEvent=:{ccMsg} _) pState=:{io=ioState}
		| isToolbarOSEvent ccMsg
			# (osdInfo,ioState)				= ioStGetOSDInfo ioState
			# (myEvent,replyToOS,deviceEvent,ioState)
											= filterToolbarEvent osdInfo osEvent ioState
			# pState						= {pState & io=ioState}
			  schedulerEvent				= if (isJust replyToOS) (ScheduleOSEvent osEvent (fromJust replyToOS)) schedulerEvent
			= (myEvent,deviceEvent,schedulerEvent,pState)
		| isMenuOSEvent ccMsg
			# (processStack,ioState)		= ioStGetProcessStack ioState
			  (found,systemId)				= topShowProcessShowState processStack
			# (ioId,ioState)				= ioStGetIOId ioState
			# (tb,ioState)					= getIOToolbox ioState
			# (found,mDevice,ioState)		= ioStGetDevice MenuDevice ioState
			# menus							= menuSystemStateGetMenuHandles mDevice
			# (myEvent,replyToOS,deviceEvent,menus,tb)
			  								= filterOSEvent osEvent (found && systemId==ioId) menus tb
			# ioState						= ioStSetDevice (MenuSystemState menus) ioState
			# ioState						= setIOToolbox tb ioState
			# pState						= {pState & io=ioState}
			  schedulerEvent				= if (isJust replyToOS) (ScheduleOSEvent osEvent (fromJust replyToOS)) schedulerEvent
			= (myEvent,deviceEvent,schedulerEvent,pState)
		| otherwise
			= (False,Nothing,schedulerEvent,pState)
	where
		isMenuOSEvent :: !Int -> Bool
		isMenuOSEvent CcWmCOMMAND				= True
		isMenuOSEvent _							= False
		
		isToolbarOSEvent :: !Int -> Bool
		isToolbarOSEvent CcWmBUTTONCLICKED		= True
		isToolbarOSEvent CcWmGETTOOLBARTIPTEXT	= True
		isToolbarOSEvent _						= False
	
	menuEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState=:{io=ioState}
		# (ioId,ioState)				= ioStGetIOId ioState
		| ioId<>recLoc.rlIOId || recLoc.rlDevice<>MenuDevice
			= (False,Nothing,schedulerEvent,{pState & io=ioState})
		| otherwise
			# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
			  menus						= menuSystemStateGetMenuHandles mDevice
			  (found,menus)				= hasMenuHandlesMenu recLoc.rlParentId menus
			  deviceEvent				= if found (Just (ReceiverEvent msgEvent)) Nothing
			# ioState					= ioStSetDevice (MenuSystemState menus) ioState
			= (found,deviceEvent,schedulerEvent,{pState & io=ioState})
	where
		recLoc							= getMsgEventRecLoc msgEvent
		
		hasMenuHandlesMenu :: !Id !*(MenuHandles .pst) -> (!Bool,!*MenuHandles .pst)
		hasMenuHandlesMenu menuId mHs=:{mMenus}
			# (found,mMenus)= ucontains (eqMenuId menuId) mMenus
			= (found,{mHs & mMenus=mMenus})
		where
			eqMenuId :: !Id !*(MenuStateHandle .pst) -> *(!Bool,!*MenuStateHandle .pst)
			eqMenuId theId msH
				# (mId,msH)	= menuStateHandleGetMenuId msH
				= (theId==mId,msH)
	
	menuEvent schedulerEvent pState
		= (False,Nothing,schedulerEvent,pState)


/*	filterToolbarEvent filters the OSEvents that can be handled by this menu device.
*/
filterToolbarEvent :: !OSDInfo !OSEvent !(IOSt .l) -> (!Bool,!Maybe [Int],!Maybe DeviceEvent,!IOSt .l)

/*	CcWmBUTTONCLICKED is a menu event in case of a toolbar selection. 
*/
filterToolbarEvent osdInfo {ccMsg=CcWmBUTTONCLICKED,p2=toolbarPtr,p4=toolbarIndex} ioState
	| isToolbarEvent osdInfo toolbarPtr
		= (True,Nothing,Just (ToolbarSelection {tbsItemNr=toolbarIndex}),ioState)
	| otherwise
		= (False,Nothing,Nothing,ioState)

/*	CcWmGETTOOLBARTIPTEXT does not continue platform independent event handling, but returns the 
	String associated with the requested toolbar item.
*/
filterToolbarEvent osdInfo {ccMsg=CcWmGETTOOLBARTIPTEXT,p1=toolbarPtr,p2=toolbarIndex} ioState
	| isToolbarEvent osdInfo toolbarPtr
		# (atts,ioState)		= ioStGetProcessAttributes ioState
		  (found,att)			= cselect isProcessToolbar undef atts
		| not found
			= (True,Nothing,Nothing,ioState)
		# maybe_tip				= gettooltip toolbarIndex (getProcessToolbarAtt att)
		| isNothing maybe_tip
			= (True,Nothing,Nothing,ioState)
		// otherwise
			# (textptr,ioState)	= accIOToolbox (winMakeCString (fromJust maybe_tip)) ioState
			= (True,Just [textptr],Nothing,ioState)
	| otherwise
		= (False,Nothing,Nothing,ioState)
where
	gettooltip :: !Int ![ToolbarItem .pst] -> Maybe String
	gettooltip i [item:items]
		| i==1 && isItem	= tip
		| otherwise			= gettooltip i` items
	where
		(isItem,i`,tip)		= case item of
								ToolbarItem _ tip _	-> (True,i-1,tip)
								ToolbarSeparator	-> (False,i,Nothing)
	gettooltip _ _
		= Nothing

filterToolbarEvent _ _ _
	= menueventFatalError "filterToolbarEvent" "unmatched OSEvent"


/*	filterOSEvent filters the OSEvents that can be handled by this menu device.
		The Bool argument is True iff the parent process is visible and active.
*/
filterOSEvent :: !OSEvent !Bool !(MenuHandles .pst) !*OSToolbox -> (!Bool,!Maybe [Int],!Maybe DeviceEvent,!MenuHandles .pst,!*OSToolbox)

/*	CcWmCOMMAND returns the selected menu item.
	This item is identified by:
	-	the top level menu Id,
	-	a possibly empty list of parent sub menus. This list is given by zero based indices starting from the top level menu.
	-	in the parent (sub) menu, the zero based index of the item.
	Only MenuItemHandle and SubMenuHandle elements increase the index; all other elements don't.
*/
filterOSEvent {ccMsg=CcWmCOMMAND,p1=item,p2=mods} _ menus=:{mEnabled,mMenus=mHs} tb
	| not mEnabled
		= (False,Nothing,Nothing,menus,tb)
	| otherwise
		# (found,deviceEvent,mHs,tb)= getSelectedMenuStateHandlesItem item (toModifiers mods) mHs tb
		= (found,Nothing,deviceEvent,{menus & mMenus=mHs},tb)
where
	getSelectedMenuStateHandlesItem :: !Int !Modifiers !*[*MenuStateHandle .pst] !*OSToolbox
						  -> (!Bool,!Maybe DeviceEvent,!*[*MenuStateHandle .pst],!*OSToolbox)
	getSelectedMenuStateHandlesItem item modifiers msHs tb
		# (empty,msHs)					= uisEmpty msHs
		| empty
			= (False,Nothing,msHs,tb)
		# (msH,msHs)					= hdtl msHs
		# (found,menuEvent,msH,tb)		= getSelectedMenuStateHandleItem item modifiers msH tb
		| found
			= (found,menuEvent,[msH:msHs],tb)
		| otherwise
			# (found,menuEvent,msHs,tb)	= getSelectedMenuStateHandlesItem item modifiers msHs tb
			= (found,menuEvent,[msH:msHs],tb)
filterOSEvent _ _ _ _
	= menueventFatalError "filterOSEvent" "unmatched OSEvent"


/*	popUpMenuEvent returns the proper DeviceEvent for PopUpMenu selections (determined by the OSTrackPopUpMenu result; see osmenu).
*/
popUpMenuEvent :: !OSTrackPopUpMenu !(MenuStateHandle .ps) !*OSToolbox -> (!Maybe DeviceEvent, !MenuStateHandle .ps, !*OSToolbox)
popUpMenuEvent {ospupItem=PopUpTrackedByItemId item,ospupModifiers=mods} msH tb
	# (_,menuEvent,msH,tb)	= getSelectedMenuStateHandleItem item mods msH tb
	= (menuEvent,msH,tb)
popUpMenuEvent _ _ _
	= menueventFatalError "popUpMenuEvent" "PopUpTrackedByIndex not expected"


/*	getSelectedMenuStateHandleItem item mods msH
		determines if there is a menu item identified by item in msh. If so, the corresponding abstract device event is returned.
	This function is used by (filterOSEvent {ccMsg=CcWmCOMMAND}) and popUpMenuEvent.
*/
getSelectedMenuStateHandleItem :: !Int !Modifiers !*(MenuStateHandle .pst) !*OSToolbox
					 -> (!Bool,!Maybe DeviceEvent, !*MenuStateHandle .pst, !*OSToolbox)
getSelectedMenuStateHandleItem item mods msH=:(MenuLSHandle mlsH=:{mlsHandle=mH=:{mSelect,mHandle,mMenuId,mItems}}) tb
	| not mSelect
		= (False,Nothing,msH,tb)
	| otherwise
		# (found,menuEvent,_,_,itemHs,tb)	= getSelectedMenuElementHandlesItem item mHandle mMenuId mods [] 0 mItems tb
		= (found,menuEvent,MenuLSHandle {mlsH & mlsHandle={mH & mItems=itemHs}},tb)
where
	getSelectedMenuElementHandlesItem :: !Int !OSMenu !Id !Modifiers ![Int] !Int !*[*MenuElementHandle .ls .pst] !*OSToolbox
										-> (!Bool,!Maybe DeviceEvent,![Int],!Int,!*[*MenuElementHandle .ls .pst],!*OSToolbox)
	getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
		# (empty,itemHs)							= uisEmpty itemHs
		| empty
			= (False,Nothing,parents,zIndex,itemHs,tb)
		# (itemH,itemHs)							= hdtl itemHs
		# (found,menuEvent,parents,zIndex,itemH,tb)	= getSelectedMenuElementHandle item mH menuId mods parents zIndex itemH tb
		| found
			= (found,menuEvent,parents,zIndex,[itemH:itemHs],tb)
		| otherwise
			# (found,menuEvent,parents,zIndex,itemHs,tb)= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,[itemH:itemHs],tb)
	where
		getSelectedMenuElementHandle :: !Int !OSMenu !Id !Modifiers ![Int] !Int !*(MenuElementHandle .ls .pst) !*OSToolbox
									   -> (!Bool,!Maybe DeviceEvent,![Int],!Int, !*MenuElementHandle .ls .pst, !*OSToolbox)
		
		getSelectedMenuElementHandle item mH menuId mods parents zIndex itemH=:(MenuItemHandle {mOSMenuItem,mItemId}) tb
			| item==mOSMenuItem
				= (True,Just (MenuTraceEvent {mtId=menuId,mtParents=parents,mtItemNr=zIndex,mtModifiers=mods}),parents,zIndex+1,itemH,tb)
			| otherwise
				= (False,Nothing,parents,zIndex+1,itemH,tb)
		
		getSelectedMenuElementHandle item mH menuId mods parents zIndex itemH=:(SubMenuHandle submenuH=:{mSubSelect,mSubHandle,mSubItems}) tb
			| not mSubSelect
				= (False,Nothing,parents,zIndex+1,itemH,tb)
			| otherwise
				# (found,menuEvent,parents1,_,itemHs,tb)
							= getSelectedMenuElementHandlesItem item mSubHandle menuId mods (parents++[zIndex]) 0 mSubItems tb
				  itemH		= SubMenuHandle {submenuH & mSubItems=itemHs}
				  parents	= if found parents1 parents
				= (found,menuEvent,parents,zIndex+1,itemH,tb)
		
	/*	getSelectedMenuElementHandle item mH menuId mods parents zIndex (RadioMenuHandle rH=:{mRadioSelect,mRadioItems=itemHs,mRadioIndex}) tb
			# (nrRadios,itemHs)	= Ulength itemHs
			| not mRadioSelect
				= (False,Nothing,parents,zIndex+nrRadios,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			# (found,menuEvent,parents,zIndex1,itemHs,tb)	= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			| not found
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			# curIndex	= mRadioIndex
			  newIndex	= zIndex1-zIndex
			| curIndex==newIndex
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			| otherwise
				# curH	= getMenuItemOSMenuItem (itemHs!!(curIndex-1))
				  newH	= getMenuItemOSMenuItem (itemHs!!(newIndex-1))
				# tb	= OSMenuItemCheck False mH curH tb
				# tb	= OSMenuItemCheck True  mH newH tb
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=itemHs,mRadioIndex=newIndex},tb)
		where
			getMenuItemOSMenuItem :: !(MenuElementHandle .ls .pst) -> OSMenuItem
			getMenuItemOSMenuItem (MenuItemHandle {mOSMenuItem}) = mOSMenuItem
	*/	
		getSelectedMenuElementHandle item mH menuId mods parents zIndex (RadioMenuHandle rH=:{mRadioSelect,mRadioItems=itemHs,mRadioIndex}) tb
			# (nrRadios,itemHs)	= ulength itemHs
			| not mRadioSelect
				= (False,Nothing,parents,zIndex+nrRadios,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			# (found,menuEvent,parents,zIndex1,itemHs,tb)	= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			| not found
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			# curIndex	= mRadioIndex
			  newIndex	= zIndex1-zIndex
			| curIndex==newIndex
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
			| otherwise
				# (before,[itemH:after])= splitAt (curIndex-1) itemHs
				# (curH,itemH)			= getMenuItemOSMenuItem itemH
				# (before,[itemH:after])= splitAt (newIndex-1) (before ++ [itemH:after])
				# (newH,itemH)			= getMenuItemOSMenuItem itemH
				# tb					= osMenuItemCheck False mH curH 0 0 tb	// 0 0 added: dummy on Windows
				# tb					= osMenuItemCheck True  mH newH 0 0 tb	// 0 0 added: dummy on Windows
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=before ++ [itemH:after],mRadioIndex=newIndex},tb)
		where
			getMenuItemOSMenuItem :: !*(MenuElementHandle .ls .pst) -> (!OSMenuItem,!MenuElementHandle .ls .pst)
			getMenuItemOSMenuItem itemH=:(MenuItemHandle {mOSMenuItem}) = (mOSMenuItem,itemH)
		
		getSelectedMenuElementHandle item mH menuId mods parents zIndex itemH=:(MenuSeparatorHandle _) tb
			= (False,Nothing,parents,zIndex+1,itemH,tb)
		
		getSelectedMenuElementHandle item mH menuId mods parents zIndex (MenuListLSHandle itemHs) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuListLSHandle itemHs,tb)
		
		getSelectedMenuElementHandle item mH menuId mods parents zIndex (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuExtendLSHandle {mExH & mExtendItems=itemHs},tb)
		
		getSelectedMenuElementHandle item mH menuId mods parents itemNr (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem item mH menuId mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuChangeLSHandle {mChH & mChangeItems=itemHs},tb)
		
		getSelectedMenuElementHandle _ _ _ _ parents zIndex itemH tb
			= (False,Nothing,parents,zIndex,itemH,tb)

/*	PA: this function is now defined in clCCall_12.
toModifiers :: !Int -> Modifiers
toModifiers i
	=	{	shiftDown	= shifton
		,	optionDown	= alton
		,	commandDown	= ctrlon
		,	controlDown	= ctrlon
		,	altDown		= alton
		}
where
	shifton	= i bitand SHIFTBIT <> 0
	alton	= i bitand ALTBIT   <> 0
	ctrlon	= i bitand CTRLBIT  <> 0
*/
//	isToolbarEvent checks whether the toolbar equals the OSDInfo toolbar.
isToolbarEvent :: !OSDInfo !OSWindowPtr -> Bool
isToolbarEvent osdInfo toolbarPtr
	| isNothing maybeToolbar	= False
	| otherwise					= (fromJust maybeToolbar).toolbarPtr==toolbarPtr
where
	maybeToolbar				= getOSDInfoOSToolbar osdInfo

/*	PA: not used. Instead use menuHandlesGetMenus/menuHandlesSetMenus.
menuHandlesGetMenuStateHandles :: !(MenuHandles .pst) -> (![MenuStateHandle .pst],!MenuHandles .pst)
menuHandlesGetMenuStateHandles mHs=:{mMenus}
	= (mMenus,{mHs & mMenus=[]})
*/
