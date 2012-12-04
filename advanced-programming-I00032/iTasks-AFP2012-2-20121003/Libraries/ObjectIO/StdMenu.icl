implementation module StdMenu


import	StdBool, StdList, StdTuple
import	osmenu
import	commondef, iostate, menuaccess, menucreate, menudevice, menuevent, menuinternal, menuitems, StdId
from devicesystemstate import windowSystemStateGetWindowHandles
from menudefaccess import menuDefGetMenuId
from devicesystemstate import menuSystemStateGetMenuHandles
from StdPSt import accPIO
from windowaccess import getWindowHandlesActiveModalDialog


stdMenuFatalError :: String String -> .x
stdMenuFatalError function error
	= fatalError function "StdMenu" error


//	General rules to access MenuHandles:

accessMenuHandles :: !Id !((MenuStateHandle (PSt .l)) -> (x,!MenuStateHandle (PSt .l))) !(IOSt .l) -> (!Maybe x,!IOSt .l)
accessMenuHandles id f ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# mHs						= menuSystemStateGetMenuHandles mDevice
	  (result,msHs)				= accessmenuhandles id f mHs.mMenus
	# ioState					= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
	= (result,ioState)
where
	accessmenuhandles :: !Id !((MenuStateHandle .pst) -> (x,!MenuStateHandle .pst)) ![MenuStateHandle .pst] -> (!Maybe x,![MenuStateHandle .pst])
	accessmenuhandles id f [mH:mHs]
		# (menu_id,mH)			= menuStateHandleGetMenuId mH
		| id==menu_id
			# (result,mH)		= f mH
			= (Just result,[mH:mHs])
		| otherwise
			# (opt_result,mHs)	= accessmenuhandles id f mHs
			= (opt_result,[mH:mHs])
	accessmenuhandles _ _ _
		= (Nothing,[])

changeMenuSystemState :: !Bool
						 !(OSMenuBar -> (MenuHandles (PSt .l)) -> *(*OSToolbox -> *(MenuHandles (PSt .l),*OSToolbox)))
						 !(IOSt .l)
						-> IOSt .l
changeMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= stdMenuFatalError "changeMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= ioState
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= menuSystemStateGetMenuHandles mDevice
	# (menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (MenuSystemState menus) ioState
	| otherwise
		# tb					= osDrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (MenuSystemState menus) ioState

accessMenuSystemState :: !Bool
						 !(OSMenuBar -> (MenuHandles (PSt .l)) -> *(*OSToolbox -> *(.x,MenuHandles (PSt .l),*OSToolbox)))
						 !(IOSt .l)
					-> (!Maybe .x,!IOSt .l)
accessMenuSystemState redrawMenus f ioState
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should never hold
		= stdMenuFatalError "accessMenuSystemState" "could not retrieve OSMenuBar from IOSt"
	# osMenuBar					= fromJust maybeOSMenuBar
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	# (tb,ioState)				= getIOToolbox ioState
	  menus						= menuSystemStateGetMenuHandles mDevice
	# (x,menus,tb)				= f osMenuBar menus tb
	| not redrawMenus
		# ioState				= setIOToolbox tb ioState
		= (Just x,ioStSetDevice (MenuSystemState menus) ioState)
	| otherwise
		# tb					= osDrawMenuBar osMenuBar tb
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= setIOToolbox tb ioState
		= (Just x,ioStSetDevice (MenuSystemState menus) ioState)


//	Opening a menu for an interactive process.

class Menus mdef where
	openMenu	:: .ls !.(mdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getMenuType	::      .(mdef .ls .pst)               -> MenuType

instance Menus (Menu m)	| MenuElements m where
	openMenu ls mDef pState
		# pState			= menuFunctions.dOpen pState
		# (isZero,pState)	= accPIO checkZeroMenuBound pState
		| isZero
			= (ErrorViolateDI,pState)
		# (optMenuId,mDef)	= menuDefGetMenuId mDef
		# (optMenuId,pState)= accPIO (validateMenuId optMenuId) pState
		| isNothing optMenuId
			= (ErrorIdsInUse,pState)
		# menuId			= fromJust optMenuId
		| menuId==windowMenuId
			= (ErrorIdsInUse,pState)
		| otherwise
			= openMenu` menuId ls mDef pState
	where
		checkZeroMenuBound :: !(IOSt .l) -> (!Bool,!IOSt .l)
		checkZeroMenuBound ioState
			# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= stdMenuFatalError "openMenu (Menu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= menuSystemStateGetMenuHandles mDevice
			  (bound,mHs)				= (\msHs=:{mNrMenuBound}->(mNrMenuBound,msHs)) mHs
			# ioState					= ioStSetDevice (MenuSystemState mHs) ioState
			= (zeroBound bound,ioState)
	
	getMenuType _ = "Menu"

validateMenuId :: !(Maybe Id) !(IOSt .l) -> (!Maybe Id,!IOSt .l)
validateMenuId Nothing ioState
	# (mId,ioState)				= openId ioState
	= (Just mId,ioState)
validateMenuId (Just id) ioState
	# (idtable,ioState)			= ioStGetIdTable ioState
	# (member,idtable)			= memberIdTable id idtable
	| member					= (Nothing,ioStSetIdTable idtable ioState)
	| otherwise					= (Just id,ioStSetIdTable idtable ioState)

instance Menus (PopUpMenu m) | PopUpMenuElements m where
	openMenu ls mDef pState
		# (osdInfo,pState)			= accPIO ioStGetOSDInfo pState
		| getOSDInfoDocumentInterface osdInfo==NDI
			= (ErrorViolateDI,pState)
		# maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
		| isNothing maybeOSMenuBar	// This condition should never occur
			= stdMenuFatalError "openMenu (PopUpMen)" "OSMenuBar could not be retrieved from OSDInfo"
		# pState					= menuFunctions.dOpen pState
		# (found,mDevice,ioState)	= ioStGetDevice MenuDevice pState.io
		| not found
			= (ErrorUnknownObject,{pState & io=ioState})
		# mHs						= menuSystemStateGetMenuHandles mDevice
		  mHs						= closepopupmenu mHs
		  osMenuBar					= fromJust maybeOSMenuBar
		# (idtable,ioState)			= ioStGetIdTable ioState
		# (rt,ioState)				= ioStGetReceiverTable ioState
		# (ioid,ioState)			= ioStGetIOId ioState
		# (ok,mHs,rt,idtable,osMenuBar,pState)
									= createPopUpMenu ioid ls mDef mHs rt idtable osMenuBar {pState & io=ioState}
	 	  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
	 	# ioState					= ioStSetOSDInfo osdInfo pState.io
	 	# ioState					= ioStSetReceiverTable rt ioState
	 	# ioState					= ioStSetIdTable idtable ioState
		# ioState					= ioStSetDevice (MenuSystemState mHs) ioState
		# pState					= {pState & io=ioState}
		| ok
			= (NoError,handlePopUpMenu pState)
		| otherwise
			= (ErrorIdsInUse,pState)
	where
	//	handlePopUpMenu opens the pop up menu.
/*		handlePopUpMenu :: !(PSt .l) -> (!ErrorReport,!PSt .l)
		handlePopUpMenu pState
			# (osdInfo,ioState)			= ioStGetOSDInfo pState.io
			  framePtr					= case (getOSDInfoOSInfo osdInfo) of
			  								Just info -> info.osFrame
			  								nothing   -> stdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
			# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
			| not found					// This condition should never occur
				= stdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt"
			# mHs						= menuSystemStateGetMenuHandles mDevice
			  (menus,mHs)				= menuHandlesGetMenus mHs
			  (popUpMenu,menus)			= hdtl menus
			  (popUpId,popUpMenu)		= menuStateHandleGetMenuId popUpMenu
			  (mPtr,popUpMenu)			= menuStateHandleGetHandle popUpMenu
//			# ((ok,event,popUpMenu),ioState)	= accIOToolbox (osTrackPopUpMenu mPtr framePtr popUpMenu) ioState
			# ((itemNr,menuMods),ioState)		= accIOToolbox (osTrackPopUpMenu mPtr framePtr) ioState
			# ((ok,event,popUpMenu),ioState)	= case itemNr of
													0 -> ((True,Nothing,popUpMenu),ioState)
													_ -> accIOToolbox (getSelectedMenuStateHandleItem itemNr menuMods popUpMenu) ioState
			| not ok
				# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=menus,mPopUpId=Just popUpId}) ioState
				# pState				= {pState & io=ioState}
				= (OtherError "PopUpMenu tracking error",pState)
			| otherwise
				# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=[popUpMenu:menus]}) ioState
				# pState				= {pState & io=ioState}
				= (NoError,pState)
*/		handlePopUpMenu :: !(PSt .ps) -> PSt .ps
		handlePopUpMenu pState
			# (osdInfo,ioState)				= ioStGetOSDInfo pState.io
			  framePtr						= case (getOSDInfoOSInfo osdInfo) of
				  								Just info -> info.osFrame
				  								nothing   -> stdMenuFatalError "openMenu (PopUpMenu)" "incorrect OSDInfo retrieved"
			# (found,mDevice,ioState)		= ioStGetDevice MenuDevice ioState
			| not found						// This condition should never occur
				= stdMenuFatalError "openMenu (PopUpMenu)" "could not retrieve MenuSystemState from IOSt"
			# (tb,ioState)					= getIOToolbox ioState
			  mHs							= menuSystemStateGetMenuHandles mDevice
			  (menus,mHs)					= menuHandlesGetMenus mHs
			  (popUpMenu,menus)				= hdtl menus
	//		  (popUpId,popUpMenu)			= menuStateHandleGetMenuId popUpMenu		PA: not needed anymore
			  (mPtr,popUpMenu)				= menuStateHandleGetHandle popUpMenu
			# (maybePopUpItem,tb)			= osTrackPopUpMenu mPtr framePtr tb
			| isNothing maybePopUpItem		// No item has been selected
				# ioState					= setIOToolbox tb ioState
				# ioState					= ioStSetDevice (MenuSystemState {mHs & mMenus=[popUpMenu:menus]}) ioState
				# pState					= {pState & io=ioState}
				= pState
			# (maybePopUpEvent,popUpMenu,tb)= popUpMenuEvent (fromJust maybePopUpItem) popUpMenu tb
			# ioState						= setIOToolbox tb ioState
			# ioState						= ioStSetDevice (MenuSystemState {mHs & mMenus=[popUpMenu:menus]}) ioState
			# pState						= {pState & io=ioState}
			| isNothing maybePopUpEvent		// No abstract event is associated with the item
				= pState
			| otherwise						// Evaluate the abstract event
				= snd (menuFunctions.dDoIO (fromJust maybePopUpEvent) pState)
	
	getMenuType _ = "PopUpMenu"


/*	PA: no need to copy/adapt code. Better to reuse menuevent and menudevice. 
//	where (copied from ?menuevent)

getSelectedMenuStateHandleItem :: !Int !Modifiers !(MenuStateHandle .pst) !*OSToolbox
				-> ((!Bool,!Maybe DeviceEvent,!MenuStateHandle .pst), !*OSToolbox)
getSelectedMenuStateHandleItem itemNr mods msH=:(MenuLSHandle mlsH=:{mlsHandle=mH=:{mSelect,mHandle,mMenuId,mItems,mOSMenuNr}}) tb
	| not mSelect
		= ((False,Nothing,msH),tb)
	| otherwise
		# (found,menuEvent,_,_,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mHandle mMenuId mOSMenuNr mods [] 1 mItems tb
		= ((found,menuEvent,MenuLSHandle {mlsH & mlsHandle={mH & mItems=itemHs}}),tb)
where
	getSelectedMenuElementHandlesItem :: !Int !OSMenu !Id !OSMenuNr !Modifiers ![Int] !Int ![MenuElementHandle .ls .pst] !*OSToolbox
								  -> (!Bool,!Maybe DeviceEvent,![Int],!Int,![MenuElementHandle .ls .pst],!*OSToolbox)
	getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
		# (isEmpty,itemHs)	= uisEmpty itemHs
		| isEmpty
			= (False,Nothing,parents,zIndex,itemHs,tb)
		# (itemH,itemHs)							= hdtl itemHs
		# (found,menuEvent,parents,zIndex,itemH,tb)	= getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex itemH tb
		| found
			= (found,menuEvent,parents,zIndex,[itemH:itemHs],tb)
		| otherwise
			# (found,menuEvent,parents,zIndex,itemHs,tb)= getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,[itemH:itemHs],tb)
	where
		getSelectedMenuElementHandle :: !Int !OSMenu !Id !OSMenuNr !Modifiers ![Int] !Int !(MenuElementHandle .ls .pst) !*OSToolbox
								 -> (!Bool,!Maybe DeviceEvent,![Int],!Int, !MenuElementHandle .ls .pst, !*OSToolbox)
		
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex itemH=:(MenuItemHandle {mOSMenuItem,mItemId}) tb
//			# tb = trace_n ("?: z"+++toString zIndex+++" mOSM: "+++toString mOSMenuNr) tb
			| itemNr==zIndex
//				#! tb = trace_n (zIndex,pretty parents) tb
				= (True,Just (MenuTraceEvent {mtId=mMenuId,mtParents=parents,mtItemNr=itemNr-1,mtModifiers= mods}),parents,zIndex+1,itemH,tb)
			| otherwise
				= (False,Nothing,parents,zIndex+1,itemH,tb)
/*				
		getSelectedMenuElenebtHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex itemH=:(MenuSeparatorHandle) tb
			| itemNr == zIndex
				= ()
			| otherwise
				= (False,Nothing,parents,zIndex+1,itemH,tb)
*/					
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex itemH=:(SubMenuHandle submenuH=:{mSubOSMenuNr,mSubSelect,mSubHandle,mSubItems}) tb
//			#! tb = trace_n ("mSubOSMenuNr",mSubOSMenuNr,zIndex) tb
			| not mSubSelect
				= (False,Nothing,parents,zIndex+1,itemH,tb)
			| otherwise
				#! parents1	= parents++[zIndex-1]
//				#! tb = trace_n ("parents1 before ",pretty parents1) tb
				# (found,menuEvent,parents1,_,itemHs,tb)
							= getSelectedMenuElementHandlesItem itemNr mSubHandle mMenuId mSubOSMenuNr mods parents1 1 mSubItems tb
//				#! tb = trace_n ("parents1 after",pretty parents1,found) tb
				# itemH		= SubMenuHandle {submenuH & mSubItems=itemHs}
				  parents	= if found parents1 parents
				= (found,menuEvent,parents,zIndex+1,itemH,tb)
		
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex (RadioMenuHandle rH=:{mRadioSelect,mRadioItems=itemHs,mRadioIndex}) tb
			# (nrRadios,itemHs)	= ulength itemHs
			| not mRadioSelect
				= (False,Nothing,parents,zIndex+nrRadios,RadioMenuHandle {rH & mRadioItems=itemHs},tb)
//					# (found,menuEvent,parents,zIndex1,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mH mMenuId mods parents zIndex itemHs tb// itemNr mH mMenuId mmods parents zIndex itemHs tb
			# (found,menuEvent,parents,zIndex1,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
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
				# tb					= osMenuItemCheck False mH curH curIndex (curIndex+zIndex-1) tb
				# tb					= osMenuItemCheck True  mH newH newIndex (zIndex1-1) tb
				= (found,menuEvent,parents,zIndex1,RadioMenuHandle {rH & mRadioItems=before ++ [itemH:after],mRadioIndex=newIndex},tb)
		where
			getMenuItemOSMenuItem :: !*(MenuElementHandle .ls .pst) -> (!OSMenuItem,!MenuElementHandle .ls .pst)
			getMenuItemOSMenuItem itemH=:(MenuItemHandle {mOSMenuItem}) = (mOSMenuItem,itemH)
		
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex (MenuListLSHandle itemHs) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuListLSHandle itemHs,tb)
		
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex (MenuExtendLSHandle mExH=:{mExtendItems=itemHs}) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuExtendLSHandle {mExH & mExtendItems=itemHs},tb)
		
		getSelectedMenuElementHandle itemNr mH mMenuId mOSMenuNr mods parents zIndex (MenuChangeLSHandle mChH=:{mChangeItems=itemHs}) tb
			# (found,menuEvent,parents,zIndex,itemHs,tb)	= getSelectedMenuElementHandlesItem itemNr mH mMenuId mOSMenuNr mods parents zIndex itemHs tb
			= (found,menuEvent,parents,zIndex,MenuChangeLSHandle {mChH & mChangeItems=itemHs},tb)
		
		getSelectedMenuElementHandle _ _ _ _ _ parents zIndex itemH=:(MenuReceiverHandle _) tb
			= (False,Nothing,parents,zIndex,itemH,tb)

		getSelectedMenuElementHandle _ _ _ _ _ parents zIndex itemH tb
			= (False,Nothing,parents,zIndex+1,itemH,tb)
*/

//	Closing a menu.

closeMenu :: !Id !(IOSt .l) -> IOSt .l
closeMenu id ioState
	| id==windowMenuId	= ioState
	| otherwise			= closemenu id ioState


//	Enabling and Disabling of the MenuSystem:

enableMenuSystem :: !(IOSt .l) -> IOSt .l
enableMenuSystem ioState
	# (isModal,ioState)		= hasModalDialog ioState
	| isModal
		= ioState
	# (di,ioState)			= ioStGetDocumentInterface ioState
	| di==NDI
		= ioState
	| otherwise
		= changeMenuSystemState True (enablemenusystem di) ioState
where
	hasModalDialog :: !(IOSt .l) -> (!Bool,!IOSt .l)
	hasModalDialog ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found
			= (False,ioState)
		| otherwise
			# windows				= windowSystemStateGetWindowHandles wDevice
			  (modalWIDS,windows)	= getWindowHandlesActiveModalDialog windows
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			= (isJust modalWIDS,ioState)
	
	enablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	enablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= ulength mMenus
//			# tb			= enablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			# tb			= enablemenus nrMenus osMenuBar tb
			= ({menus & mMenus=msHs,mEnabled=SystemAble},tb)
	where
		enablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		enablemenus i osmenubar tb
			| i<0			= tb
			| otherwise		= enablemenus (i-1) osMenuBar (osEnableMenu i osMenuBar tb)


disableMenuSystem :: !(IOSt .l) -> IOSt .l
disableMenuSystem ioState
	# (di,ioState)	= ioStGetDocumentInterface ioState
	| di==NDI		= ioState
	| otherwise		= changeMenuSystemState True (disablemenusystem di) ioState
where
	disablemenusystem :: !DocumentInterface !OSMenuBar !(MenuHandles .pst) !*OSToolbox -> (!MenuHandles .pst,!*OSToolbox)
	disablemenusystem di osMenuBar menus=:{mEnabled,mMenus} tb
		| not mEnabled
			= (menus,tb)
		| otherwise
			# (nrMenus,msHs)= ulength mMenus
//			# tb			= disablemenus (if (di==MDI) (nrMenus+1) (nrMenus-1)) osMenuBar tb
			# tb			= disablemenus nrMenus osMenuBar tb
			// DvA: zou MDI (en SDI/NDI?) geval nu niet gewoon nrMenus moeten zijn?
			= ({menus & mMenus=msHs,mEnabled=SystemUnable},tb)
	where
		disablemenus :: !Int !OSMenuBar !*OSToolbox -> *OSToolbox
		disablemenus i osMenuBar tb
			| i<0			= tb
			| otherwise		= disablemenus (i-1) osMenuBar (osDisableMenu i osMenuBar tb)


//	Enabling and Disabling of Menus:

enableMenus :: ![Id] !(IOSt .l) -> IOSt .l
enableMenus ids ioState
	# ids			= filter ((<>) windowMenuId) ids
	| isEmpty ids	= ioState
	| otherwise		= enablemenus ids ioState

disableMenus :: ![Id] !(IOSt .l) -> IOSt .l
disableMenus ids ioState
	# ids			= filter ((<>) windowMenuId) ids
	| isEmpty ids	= ioState
	| otherwise		= disablemenus ids ioState


//	Get the SelectState of a menu: 

getMenuSelectState :: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
getMenuSelectState id ioState
	# (optSelect,ioState)	= accessMenuHandles id menuStateHandleGetSelect ioState
	| isNothing optSelect	= (Nothing,		ioState)
	| fromJust optSelect	= (Just Able,	ioState)
	| otherwise				= (Just Unable,	ioState)


/*	Adding menu elements to (sub/radio)menus:
		Items in a (sub/radio)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front
		Open with a position higher than the number of items adds the new elements to
		the end.
		Open an item on a position adds the item AFTER the item on that position.
*/
openMenuElements :: !Id !Index .ls .(m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | MenuElements m
openMenuElements mId pos ls new pState
	# (it,ioState)					= ioStGetIdTable pState.io
	# (maybeParent,it)				= getIdParent mId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# parent						= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (pid,ioState)					= ioStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	| parent.idpId<>mId
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (found,mDevice,ioState)		= ioStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (osdInfo,ioState)				= ioStGetOSDInfo ioState
	  maybeOSMenuBar				= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar		// This condition should not occur
		= stdMenuFatalError "openMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar					= fromJust maybeOSMenuBar
		# (rt, ioState)				= ioStGetReceiverTable ioState
		# (tb, ioState)				= getIOToolbox ioState
		# pState					= {pState & io=ioState}
		  menus						= menuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
									= addMenusItems (mId,Nothing) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState					= setIOToolbox (osDrawMenuBar osMenuBar tb) pState.io
		  mDevice					= MenuSystemState menus
		  osdInfo					= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState					= ioStSetOSDInfo osdInfo ioState
		# ioState					= ioStSetDevice mDevice ioState
		# ioState					= ioStSetIdTable it ioState
		# ioState					= ioStSetReceiverTable rt ioState
		# pState					= {pState & io=ioState}
		= (error,pState)

openSubMenuElements :: !Id !Index .ls .(m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)	| MenuElements m
openSubMenuElements sId pos ls new pState
	# (it,ioState)				= ioStGetIdTable pState.io
	# (maybeParent,it)			= getIdParent sId it
	| isNothing maybeParent
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# parent					= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (pid,ioState)				= ioStGetIOId ioState
	| parent.idpIOId<>pid
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetIdTable it ioState})
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdInfo
	| isNothing maybeOSMenuBar	// This condition should not occur
		= stdMenuFatalError "openSubMenuElements" "OSMenuBar could not be retrieved from OSDInfo"
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (rt,ioState)			= ioStGetReceiverTable ioState
		# (tb,ioState)			= getIOToolbox ioState
		# pState				= {pState & io=ioState}
		  menus					= menuSystemStateGetMenuHandles mDevice
		# ((error,rt,it),menus,osMenuBar,pState)
								= addMenusItems (parent.idpId,Just sId) (max 0 pos) ls new pid rt it menus osMenuBar pState
		# ioState				= setIOToolbox (osDrawMenuBar osMenuBar tb) pState.io
		  mDevice				= MenuSystemState menus
		  osdInfo				= setOSDInfoOSMenuBar osMenuBar osdInfo
		# ioState				= ioStSetOSDInfo osdInfo ioState
		# ioState				= ioStSetDevice mDevice ioState
		# ioState				= ioStSetIdTable it ioState
		# ioState				= ioStSetReceiverTable rt ioState
		# pState				= {pState & io=ioState}
		= (error,pState)

openRadioMenuItems :: !Id !Index ![MenuRadioItem (PSt .l)] !(IOSt .l) -> (!ErrorReport,!IOSt .l)
openRadioMenuItems rId pos radioItems ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent rId idtable
	| isNothing maybeParent
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= (ErrorUnknownObject,ioStSetIdTable idtable ioState)
	| isEmpty radioItems
		= (NoError,ioStSetIdTable idtable ioState)
	# radioIds				= filterMap (\(_,maybeId,_,_)->(isJust maybeId,fromJust maybeId)) radioItems
	# (ok,idtable)			= okMembersIdTable radioIds idtable
	| not ok
		= (ErrorIdsInUse,ioStSetIdTable idtable ioState)
	| otherwise
		# mId				= parent.idpId
		# (error,ioState)	= accessMenuSystemState True (addMenuRadioItems (mId,rId) (max 0 pos) radioItems) ioState
		# ioState			= ioStSetIdTable (snd (addIdsToIdTable (map (\id->(id,{idpIOId=ioId,idpDevice=MenuDevice,idpId=mId})) radioIds) idtable)) ioState
		  error				= case error of
		  						Nothing  -> ErrorUnknownObject
		  						Just err -> err
		= (error,ioState)


//	Removing menu elements from (sub/radio)menus:

closeMenuElements :: !Id ![Id] !(IOSt .l) -> IOSt .l
closeMenuElements mId ids ioState
	# ids			= filter (\id->not (isSpecialId id)) ids
	| isEmpty ids	= ioState
	| otherwise		= closemenuelements mId ids ioState


//	Removing menu elements from (sub/radio)menus by index (counting from 1):

closeMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeMenuIndexElements mId indices ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent mId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId || parent.idpId<>mId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (mId,Nothing) indices ioState

closeSubMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeSubMenuIndexElements sId indices ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent sId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements False ioId (parent.idpId,Just sId) indices ioState

closeRadioMenuIndexElements :: !Id ![Index] !(IOSt .l) -> IOSt .l
closeRadioMenuIndexElements rId indices ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (maybeParent,idtable)	= getIdParent rId idtable
	# ioState				= ioStSetIdTable idtable ioState
	| isNothing maybeParent
		= ioState
	# parent				= fromJust maybeParent
	| parent.idpDevice<>MenuDevice
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	| parent.idpIOId<>ioId
		= ioState
	| otherwise
		= closemenuindexelements NotRemoveSpecialMenuElements True ioId (parent.idpId,Just rId) indices ioState


//	Determine the Ids and MenuTypes of all menus.

getMenus :: !(IOSt .l) -> (![(Id,MenuType)],!IOSt .l)
getMenus ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= ([],ioState)
	| otherwise
		# mHs					= menuSystemStateGetMenuHandles mDevice
		  (idtypes,msHs)		= accessList getIdType mHs.mMenus
		# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
		= (/*tl*/ idtypes,ioState)		// PA: there is no special first menu
where
	getIdType :: !(MenuStateHandle .pst) -> *((Id,MenuType),!MenuStateHandle .pst)
	getIdType msH
		# (id,msH)				= menuStateHandleGetMenuId msH
		= ((id,"Menu"),msH)


//	Determine the index position of a menu.

getMenuPos :: !Id !(IOSt .l) -> (!Maybe Index,!IOSt .l)
getMenuPos id ioState
	# (found,mDevice,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= (Nothing,ioState)
	| otherwise
		# mHs					= menuSystemStateGetMenuHandles mDevice
		  (optIndex,msHs)		= getmenuindex id 0 mHs.mMenus
		# ioState				= ioStSetDevice (MenuSystemState {mHs & mMenus=msHs}) ioState
		= (optIndex,ioState)
where
	getmenuindex :: !Id !Int ![MenuStateHandle .pst] -> (!Maybe Int,![MenuStateHandle .pst])
	getmenuindex id index [mH:mHs]
		# (menu_id,mH)			= menuStateHandleGetMenuId mH
		| id==menu_id
			= (Just index,[mH:mHs])
		| otherwise
			# (optIndex,mHs)	= getmenuindex id (index+1) mHs
			= (optIndex, [mH:mHs])
	getmenuindex _ _ _
		= (Nothing,[])


//	Set & Get the title of a menu.

setMenuTitle :: !Id !Title !(IOSt .l) -> IOSt .l
setMenuTitle id title ioState
	| id==windowMenuId	= ioState
	| otherwise			= setmenutitle id title ioState

getMenuTitle :: !Id !(IOSt .l) -> (!Maybe Title,!IOSt .l)
getMenuTitle id ioState
	= accessMenuHandles id menuStateHandleGetTitle ioState
