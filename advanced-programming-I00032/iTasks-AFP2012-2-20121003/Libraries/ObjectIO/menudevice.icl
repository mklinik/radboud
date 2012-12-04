implementation module menudevice


import	StdBool, StdEnum, StdList, StdMisc
import	menuevent, osdocumentinterface, osmenu, ostypes
import	commondef, devicefunctions, iostate, menuaccess, menucreate, menudefaccess, receiveraccess, StdId
from	StdProcessAttribute	import getProcessToolbarAtt, isProcessToolbar,
		                           getProcessCloseFun,	 isProcessClose
from	StdPSt				import accPIO


menudeviceFatalError :: String String -> .x
menudeviceFatalError rule error
	= fatalError rule "menudevice" error


menuFunctions :: DeviceFunctions (PSt .l)
menuFunctions
	= {	dDevice	= MenuDevice
	  ,	dShow	= menuShow
	  ,	dHide	= menuHide
	  ,	dEvent	= menuEvent
	  ,	dDoIO	= menuIO
	  ,	dOpen	= menuOpen
	  ,	dClose	= menuClose
	  }

menuShow :: !(PSt .l) -> PSt .l
/* PA: mOSMenuBar information is stored in IOSt:OSDInfo.
menuShow pState=:{io=ioState}
	# (activeIO,ioState)	= ioStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	| otherwise
		# (tb,ioState)		= getIOToolbox ioState
		# (menus,ioState)	= ioStGetDevice MenuDevice ioState
		  mHs				= menuSystemStateGetMenuHandles menus
		# (menuBar, tb)		= osMenuBarSet mHs.mOSMenuBar tb
		  mHs				= {mHs & mOSMenuBar = menuBar}
		# ioState			= setIOToolbox tb ioState
		# ioState			= ioStSetDevice (MenuSystemState mHs) ioState
		= {pState & io=ioState}
*/
menuShow pState=:{io=ioState}
	# (activeIO,ioState)		= ioStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	# (osdinfo,ioState)			= ioStGetOSDInfo ioState
	  maybeMenuBar				= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeMenuBar	// No menu bar present, nothing to do
		= {pState & io=ioState}
	| otherwise
		# osMenuBar				= fromJust maybeMenuBar
		# (osMenuBar,ioState)	= accIOToolbox (osMenuBarSet osMenuBar) ioState
		  osdinfo				= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState				= ioStSetOSDInfo osdinfo ioState
		= {pState & io=ioState}

menuClose :: !(PSt .l) -> PSt .l
menuClose pState=:{io=ioState}
	# (osdinfo,ioState)				= ioStGetOSDInfo ioState
	  maybeOSMenuBar				= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar
		= menudeviceFatalError "MenuFunctions.dClose" "OSMenuBar could not be retrieved from OSDInfo"
	# (found,mDevice,ioState)		= ioStGetDevice MenuDevice ioState
	| not found
		= menudeviceFatalError "MenuFunctions.dClose" "could not retrieve MenuSystemState from IOSt"
	| otherwise
		# osMenuBar					= fromJust maybeOSMenuBar
		# (opt_guishare,ioState)	= ioStGetGUIShare ioState
		  menus						= menuSystemStateGetMenuHandles mDevice
		# (tb,ioState)				= getIOToolbox ioState
		// DvA: what about sub menu's, seem to be forgetting these here...
		// PA: this happens automatically on Windows, but you're right. The function disposeMenuHandles should take of this. Not yet done.
		// DvA: can't we call closemenu from menuinternal instead of this whole function?
		# (menus,(osMenuBar,tb))	= disposeMenuHandles (isJust opt_guishare) menus (osMenuBar,tb)
		# ioState					= setIOToolbox tb ioState
		  osdinfo					= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState					= ioStSetOSDInfo osdinfo ioState
		# (ioid,ioState)			= ioStGetIOId ioState
		# (rt,ioState)				= ioStGetReceiverTable ioState
		# (it,ioState)				= ioStGetIdTable ioState
		# (mHs,_)					= menuHandlesGetMenus menus
		  (_,(rt,it))				= stateMap (disposeIds ioid) mHs (rt,it)
		# ioState					= ioStSetIdTable it ioState
		# ioState					= ioStSetReceiverTable rt ioState
		# ioState					= ioStRemoveDeviceFunctions MenuDevice ioState
		= {pState & io=ioState}
where
	disposeIds :: !SystemId !(MenuStateHandle .pst) !*(!*ReceiverTable,!*IdTable)
						 -> (!MenuStateHandle .pst, !*(!*ReceiverTable,!*IdTable))
	disposeIds ioid (MenuLSHandle menuH=:{mlsHandle=mH=:{mItems}}) ts
		# (itemHs,ts)	= stateMap (disposeMenuIds ioid) mItems ts
		= (MenuLSHandle {menuH & mlsHandle={mH & mItems=itemHs}},ts)

menuHide :: !(PSt .l) -> PSt .l
menuHide pState=:{io=ioState}
	# (activeIO,ioState)	= ioStIsActive ioState
	| not activeIO
		= {pState & io=ioState}
	# (found,menus,ioState)	= ioStGetDevice MenuDevice ioState
	| not found
		= {pState & io=ioState}
	| otherwise
		# mHs				= menuSystemStateGetMenuHandles menus
		# (tb,ioState)		= getIOToolbox ioState
		# tb				= osMenuBarClear tb
		# ioState			= setIOToolbox tb ioState
		# ioState			= ioStSetDevice (MenuSystemState mHs) ioState
		= {pState & io=ioState}


/*	Opening menus:
	Note:	all interactive processes have atleast the AppleMenu to identify the 
			process, and is used for checking whether the process is active
			(see ioStIsActive further down this module).
			If the process is a subprocess, then the ioguishare of its IOSt
			contains the list to the Mac toolbox menu system.
*/
menuOpen :: !(PSt .l) -> PSt .l
menuOpen pState=:{io=ioState}
	# (hasMenu,ioState)		= ioStHasDevice MenuDevice ioState
	| hasMenu
		= {pState & io=ioState}
	| otherwise
		# (di,     ioState)	= ioStGetDocumentInterface ioState
		# (popUpId,ioState)	= getPopUpId di ioState
		  bound				= case di of
								NDI -> Finite 0
								SDI -> Infinite
								MDI -> Infinite
		  mHs				= {	mMenus		= []
							  ,	mKeys		= []
			//PA---			  ,	mOSMenuBar	= osMenuBarNew OSNoWindowPtr OSNoWindowPtr (-1) // PA: menubar will be initialised by the (M/S)DI window creation
							  ,	mEnabled	= SystemAble
							  ,	mNrMenuBound= bound
							  ,	mPopUpId	= popUpId
							  }
		# ioState			= ioStSetDevice (MenuSystemState mHs) ioState
		# ioState			= ioStSetDeviceFunctions menuFunctions ioState
		# ioState			= appIOToolbox osInitialiseMenus ioState
		= {pState & io=ioState}
where
	getPopUpId :: !DocumentInterface !(IOSt .l) -> (!Maybe Id,!IOSt .l)
	getPopUpId NDI ioState
		= (Nothing,ioState)
	getPopUpId _ ioState
		# (id,ioState)	= openId ioState
		= (Just id,ioState)


menuIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
menuIO deviceEvent pState
	# (ok,pState)	= accPIO (ioStHasDevice MenuDevice) pState
	| not ok		// This condition should never occur
		= menudeviceFatalError "MenuFunctions.dDoIO" "could not retrieve MenuSystemState from IOSt"
	| otherwise
		= menuIO deviceEvent pState
where
	menuIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
	
	menuIO receiverEvent=:(ReceiverEvent msgEvent) pState
		= (ReceiverEvent msgEvent1,pState2)
	where
		(_,mDevice,ioState)			= ioStGetDevice MenuDevice pState.io
		menus						= menuSystemStateGetMenuHandles mDevice
		ioState1					= ioStSetDevice (MenuSystemState menus1) ioState
		pState1						= {pState & io=ioState1}
		(msgEvent1,menus1,pState2)	= menuMsgIO msgEvent menus pState1
		
		menuMsgIO :: !MsgEvent !(MenuHandles (PSt .l)) (PSt .l) -> (!MsgEvent,!MenuHandles (PSt .l),PSt .l)
		menuMsgIO msgEvent menus=:{mMenus=mHs} pState
			# (msgEvent,mHs,pState)	= menusMsgIO (getMsgEventRecLoc msgEvent).rlParentId msgEvent mHs pState
			= (msgEvent,{menus & mMenus=mHs},pState)
		where
			menusMsgIO :: !Id !MsgEvent ![MenuStateHandle (PSt .l)] (PSt .l)
						  -> (!MsgEvent,![MenuStateHandle (PSt .l)], PSt .l)
			menusMsgIO menuId msgEvent msHs pState
				# (empty,msHs)				= uisEmpty msHs
				| empty
					= menudeviceFatalError "menuIO (ReceiverEvent _) _" "menu could not be found"
				# (msH,msHs)				= hdtl msHs
				  (id,msH)					= menuStateHandleGetMenuId msH
				| id==menuId
					# (msgEvent,msH,pState)	= menuStateMsgIO msgEvent msH pState
					= (msgEvent,[msH:msHs],pState)
				| otherwise
					# (msgEvent,msHs,pState)= menusMsgIO menuId msgEvent msHs pState
					= (msgEvent,[msH:msHs],pState)
	
	menuIO deviceEvent=:(MenuTraceEvent info) pState
		= (deviceEvent,pState2)
	where
		(_,mDevice,ioState)	= ioStGetDevice MenuDevice pState.io
		menus				= menuSystemStateGetMenuHandles mDevice
		ioState1			= ioStSetDevice (MenuSystemState menus1) ioState
		pState1				= {pState & io=ioState1}
		(menus1,pState2)	= menuTraceIO info menus pState1
		
		menuTraceIO :: !MenuTraceInfo !(MenuHandles (PSt .l)) (PSt .l) -> (!MenuHandles (PSt .l),PSt .l)
		menuTraceIO info=:{mtId} menus=:{mMenus=mHs} pState
			# (mHs,pState)	= menusTraceIO mtId info mHs pState
			= ({menus & mMenus=mHs},pState)
		where
			menusTraceIO :: !Id !MenuTraceInfo ![MenuStateHandle (PSt .l)] (PSt .l) -> (![MenuStateHandle (PSt .l)],PSt .l)
			menusTraceIO menuId info msHs pState
				# (empty,msHs)		= uisEmpty msHs
				| empty
					= menudeviceFatalError "menuIO (MenuTraceEvent _) _" "menu could not be found"
				# (msH,msHs)		= hdtl msHs
				  (id, msH)			= menuStateHandleGetMenuId msH
				| id==menuId
					# (msH,pState)	= menuStateTraceIO info msH pState
					= ([msH:msHs],pState)
				| otherwise
					# (msHs,pState)	= menusTraceIO menuId info msHs pState
					= ([msH:msHs],pState)
	
	menuIO deviceEvent=:(ToolbarSelection {tbsItemNr}) pState
		# (atts,pState)			= accPIO ioStGetProcessAttributes pState
		  (hasToolbarAtt,att)	= cselect isProcessToolbar undef atts
		| not hasToolbarAtt
			= (deviceEvent,pState)
		| otherwise
			# toolbarItems		= getProcessToolbarAtt att
			  f					= gettoolbarfunction tbsItemNr toolbarItems
			= (deviceEvent,f pState)
	where
		gettoolbarfunction :: !Int ![ToolbarItem .pst] -> IdFun .pst
		gettoolbarfunction i [item:items]
			| i==1 && isItem	= f
			| otherwise			= gettoolbarfunction i` items
		where
			(isItem,i`,f)		= case item of
					  				ToolbarItem _ _ f	-> (True,i-1,f)
					  				ToolbarSeparator	-> (False,i,undef)
		gettoolbarfunction _ []
			= menudeviceFatalError "menuIO (ToolbarSelection)" "toolbar index out of range"
	
	menuIO deviceEvent=:ProcessRequestClose pState
		# (atts,pState)		= accPIO ioStGetProcessAttributes pState
		  (hasCloseAtt,att)	= cselect isProcessClose undef atts
		| not hasCloseAtt
			= (deviceEvent,pState)
		| otherwise
			= (deviceEvent,getProcessCloseFun att pState)
	
	menuIO _ _
		= menudeviceFatalError "menuIO" "unexpected DeviceEvent"


/*	Apply the Menu(Mods)Function of a selected menu item.
*/
menuStateTraceIO :: !MenuTraceInfo !(MenuStateHandle (PSt .l)) (PSt .l) -> (!MenuStateHandle (PSt .l),PSt .l)
menuStateTraceIO info=:{mtParents} (MenuLSHandle {mlsState=ls,mlsHandle=mH=:{mItems}}) pState
	# (mItems,(ls,pState)) = subMenusTraceIO info mtParents mItems (ls,pState)
	= (MenuLSHandle {mlsState=ls,mlsHandle={mH & mItems=mItems}},pState)
where
//	subMenusTraceIO finds the final submenu that contains the selected menu item and then applies its Menu(Mods)Function.
	subMenusTraceIO :: !MenuTraceInfo ![Int] ![MenuElementHandle .ls .pst] !*(.ls,.pst)
										 -> (![MenuElementHandle .ls .pst], *(.ls,.pst))
	subMenusTraceIO info [] itemHs (ls,pst)
		# (_,itemHs,(ls,pst))	= menuElementsTraceIO info.mtItemNr info 0 itemHs (ls,pst)
		= (itemHs,(ls,pst))
	subMenusTraceIO info [subIndex:subIndices] itemHs (ls,pst)
		# (_,itemHs,(ls,pst))	= subMenuTraceIO subIndex info subIndices 0 itemHs (ls,pst)
		= (itemHs,(ls,pst))
	where
		subMenuTraceIO :: !Int !MenuTraceInfo ![Int] !Int ![MenuElementHandle .ls .pst] !*(.ls,.pst)
												 -> (!Int,![MenuElementHandle .ls .pst], *(.ls,.pst))
		subMenuTraceIO parentIndex info parentsIndex zIndex [itemH:itemHs] (ls,pst)
			# (zIndex,itemH,(ls,pst))	= subMenuTraceIO` parentIndex info parentsIndex zIndex itemH (ls,pst)
			| parentIndex<zIndex
				= (zIndex,[itemH:itemHs],(ls,pst))
			| otherwise
				# (zIndex,itemHs,(ls,pst))	= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs (ls,pst)
				= (zIndex,[itemH:itemHs],(ls,pst))
		where
			subMenuTraceIO` :: !Int !MenuTraceInfo ![Int] !Int !(MenuElementHandle .ls .pst) !*(.ls,.pst)
													  -> (!Int, !MenuElementHandle .ls .pst,  *(.ls,.pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex itemH=:(SubMenuHandle subH=:{mSubItems}) (ls,pst)
				| parentIndex<>zIndex
					= (zIndex+1,itemH,(ls,pst))
				| otherwise
					# (itemHs,(ls,pst))= subMenusTraceIO info parentsIndex mSubItems (ls,pst)
					= (zIndex+1,SubMenuHandle {subH & mSubItems=itemHs},(ls,pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex (RadioMenuHandle itemH=:{mRadioItems}) (ls,pst)
				# (nrRadios,itemHs)	= ulength mRadioItems
				= (zIndex+nrRadios,RadioMenuHandle {itemH & mRadioItems=itemHs},(ls,pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex itemH=:(MenuItemHandle _) (ls,pst)
				= (zIndex+1,itemH,(ls,pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuListLSHandle itemHs) (ls,pst)
				# (zIndex,itemHs,(ls,pst))		= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs (ls,pst)
				= (zIndex,MenuListLSHandle itemHs,(ls,pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,pst)
				# (zIndex,itemHs,((ls1,ls),pst))= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs ((ls1,ls),pst)
				= (zIndex,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,pst))
			subMenuTraceIO` parentIndex info parentsIndex zIndex (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,pst)
				# (zIndex,itemHs,(ls1,pst))		= subMenuTraceIO parentIndex info parentsIndex zIndex itemHs (ls1,pst)
				= (zIndex,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,pst))
			subMenuTraceIO` _ _ _ zIndex itemH=:(MenuSeparatorHandle _) (ls,pst)
				= (zIndex+1,itemH,(ls,pst))
			subMenuTraceIO` _ _ _ zIndex itemH (ls,pst)
				= (zIndex,itemH,(ls,pst))
		subMenuTraceIO _ _ _ zIndex itemHs (ls,pst)
			= (zIndex,itemHs,(ls,pst))
	
//	menuElementsTraceIO applies the Menu(Mods)Function of the menu item at index itemIndex to the context state.
	menuElementsTraceIO :: !Int !MenuTraceInfo !Int ![MenuElementHandle .ls .pst] !*(.ls,.pst)
										   -> (!Int,![MenuElementHandle .ls .pst], *(.ls,.pst))
	menuElementsTraceIO itemIndex info zIndex [itemH:itemHs] ls_ps
		# (zIndex,itemH,ls_ps)		= menuElementTraceIO itemIndex info zIndex itemH ls_ps
		| itemIndex<zIndex
			= (zIndex,[itemH:itemHs],ls_ps)
		| otherwise
			# (zIndex,itemHs,ls_ps)	= menuElementsTraceIO itemIndex info zIndex itemHs ls_ps
			= (zIndex,[itemH:itemHs],ls_ps)
	where
		menuElementTraceIO :: !Int !MenuTraceInfo !Int !(MenuElementHandle .ls .pst) !*(.ls,.pst)
											  -> (!Int, !MenuElementHandle .ls .pst,  *(.ls,.pst))
		menuElementTraceIO itemIndex info zIndex (MenuItemHandle itemH=:{mItemAtts}) (ls,ps)
			| itemIndex<>zIndex || not hasFun
				= (zIndex+1,MenuItemHandle itemH,  (ls,ps))
			| otherwise
				= (zIndex+1,MenuItemHandle itemH,f (ls,ps))
		where
			(hasFun,fAtt)	= cselect isEitherFun undef mItemAtts
			f				= if (isMenuFunction fAtt)	(getMenuFun fAtt)
														(getMenuModsFun fAtt info.mtModifiers)
			isEitherFun f	= isMenuFunction f || isMenuModsFunction f
		menuElementTraceIO itemIndex info zIndex (RadioMenuHandle radioH=:{mRadioIndex,mRadioItems}) (ls,pst)
			# (nrRadios,itemHs)		= ulength mRadioItems
			| itemIndex>zIndex+nrRadios
				// Selected item is not one of these radio items
				= (zIndex+nrRadios,RadioMenuHandle {radioH & mRadioItems=itemHs},(ls,pst))
			| otherwise
				# (_,itemHs,(ls,pst))	= menuElementsTraceIO itemIndex info zIndex itemHs (ls,pst)
				= (zIndex+nrRadios,RadioMenuHandle {radioH & mRadioItems=itemHs},(ls,pst))		// It is assumed that the mRadioIndex is correctly set by the menu EventFunction
		menuElementTraceIO itemIndex info zIndex itemH=:(SubMenuHandle _) (ls,pst)
			= (zIndex+1,itemH,(ls,pst))
		menuElementTraceIO itemIndex info zIndex (MenuListLSHandle itemHs) (ls,pst)
			# (zIndex,itemHs,(ls,pst)) = menuElementsTraceIO itemIndex info zIndex itemHs (ls,pst)
			= (zIndex,MenuListLSHandle itemHs,(ls,pst))
		menuElementTraceIO itemIndex info zIndex (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,pst)
			# (zIndex,itemHs,((ls1,ls),pst)) = menuElementsTraceIO itemIndex info zIndex itemHs ((ls1,ls),pst)
			= (zIndex,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,pst))
		menuElementTraceIO itemIndex info zIndex (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,pst)
			# (zIndex,itemHs,(ls1,pst)) = menuElementsTraceIO itemIndex info zIndex itemHs (ls1,pst)
			= (zIndex,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,pst))
		menuElementTraceIO _ _ zIndex itemH=:(MenuSeparatorHandle _) (ls,pst)
			= (zIndex+1,itemH,(ls,pst))
		menuElementTraceIO _ _ zIndex itemH (ls,pst)
			= (zIndex,itemH,(ls,pst))
	menuElementsTraceIO _ _ zIndex itemHs (ls,pst)
		= (zIndex,itemHs,(ls,pst))


/*	menuStateMsgIO handles all message events.
*/
menuStateMsgIO :: !MsgEvent !(MenuStateHandle (PSt .l)) (PSt .l) -> (!MsgEvent,!MenuStateHandle (PSt .l),PSt .l)
menuStateMsgIO msgEvent msH=:(MenuLSHandle mlsH=:{mlsState=ls,mlsHandle=mH}) pState
	= (msgEvent1,MenuLSHandle {mlsH & mlsState=ls1,mlsHandle=mH1},pState1)
where
	recLoc							= getMsgEventRecLoc msgEvent
	rId								= recLoc.rlReceiverId
	action							= case msgEvent of
										(QASyncMessage msg)	-> menuQASyncIO rId msg
										( ASyncMessage msg) -> menuASyncIO  rId msg
										(  SyncMessage msg) -> menuSyncIO   rId msg
	(msgEvent1,mH1,(ls1,pState1))	= action mH (ls,pState)

	//	menuQASyncIO queues an asynchronous message in the message queue of the indicated receiver menu element.
	menuQASyncIO :: !Id !QASyncMessage !(MenuHandle .ls .pst) *(.ls,.pst) -> (!MsgEvent,!MenuHandle .ls .pst,*(.ls,.pst))
	menuQASyncIO rId msg mH=:{mItems} (ls,pst)
		= (QASyncMessage msg,{mH & mItems=itemHs},(ls,pst))
	where
		(_,itemHs)	= elementsQASyncIO rId msg.qasmMsg mItems
		
		elementsQASyncIO :: !Id !SemiDynamic ![MenuElementHandle .ls .pst] -> (!Bool,![MenuElementHandle .ls .pst])
		elementsQASyncIO rId msg [itemH:itemHs]
			# (done,itemH)		= elementQASyncIO rId msg itemH
			| done
				= (done,[itemH:itemHs])
			| otherwise
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,[itemH:itemHs])
		where
			elementQASyncIO :: !Id !SemiDynamic !(MenuElementHandle .ls .pst) -> (!Bool,!MenuElementHandle .ls .pst)
			elementQASyncIO rId msg mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH})
				| rId<>rH.rId	= (False,mrH)
				| otherwise		= (True,MenuReceiverHandle {itemH & mReceiverHandle=receiverAddASyncMessage rId msg rH})
			elementQASyncIO rId msg (SubMenuHandle itemH=:{mSubItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,SubMenuHandle {itemH & mSubItems=itemHs})
			elementQASyncIO rId msg (MenuListLSHandle itemHs)
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuListLSHandle itemHs)
			elementQASyncIO rId msg (MenuExtendLSHandle	mExH=:{mExtendItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuExtendLSHandle {mExH & mExtendItems=itemHs})
			elementQASyncIO rId msg (MenuChangeLSHandle	mChH=:{mChangeItems=itemHs})
				# (done,itemHs)	= elementsQASyncIO rId msg itemHs
				= (done,MenuChangeLSHandle {mChH & mChangeItems=itemHs})
			elementQASyncIO rId msg itemH
				= (False,itemH)
		elementsQASyncIO _ _ _
			= (False,[])

	//	menuASyncIO handles the first asynchronous message in the message queue of the indicated receiver menu element.
	menuASyncIO :: !Id !ASyncMessage !(MenuHandle .ls .pst) *(.ls,.pst) -> (!MsgEvent,!MenuHandle .ls .pst,*(.ls,.pst))
	menuASyncIO rId msg mH=:{mItems} ls_ps
		= (ASyncMessage msg,{mH & mItems=itemHs},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsASyncIO rId mItems ls_ps
		
		elementsASyncIO :: !Id ![MenuElementHandle .ls .pst] *(.ls,.pst) -> (!Bool,![MenuElementHandle .ls .pst],*(.ls,.pst))
		elementsASyncIO rId [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementASyncIO rId itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsASyncIO rId itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementASyncIO :: !Id !(MenuElementHandle .ls .pst) *(.ls,.pst) -> (!Bool,!MenuElementHandle .ls .pst,*(.ls,.pst))
			elementASyncIO rId mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH}) (ls,pst)
				| rId<>rH.rId
					= (False,mrH,(ls,pst))
				| otherwise
					# (rH,(ls,pst))	= receiverASyncIO rH (ls,pst)
					= (True,MenuReceiverHandle {itemH & mReceiverHandle=rH},(ls,pst))
			where
				receiverASyncIO :: !(ReceiverHandle .ls .pst) *(.ls,.pst) -> (!ReceiverHandle .ls .pst,*(.ls,.pst))
				receiverASyncIO rH=:{rASMQ=[msg:msgs],rFun} (ls,pst)
					# (ls,_,pst)	= rFun msg (ls,pst)
					= ({rH & rASMQ=msgs},(ls,pst))
				receiverASyncIO _ _
					= menudeviceFatalError "receiverASyncIO" "unexpected empty asynchronous message queue"
			elementASyncIO rId (SubMenuHandle itemH=:{mSubItems=itemHs}) (ls,pst)
				# (done,itemHs,(ls,pst))	= elementsASyncIO rId itemHs (ls,pst)
				= (done,SubMenuHandle {itemH & mSubItems=itemHs},(ls,pst))
			elementASyncIO rId (MenuListLSHandle itemHs) (ls,pst)
				# (done,itemHs,(ls,pst))	= elementsASyncIO rId itemHs (ls,pst)
				= (done,MenuListLSHandle itemHs,(ls,pst))
			elementASyncIO rId (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,pst)
				# (done,itemHs,((ls1,ls),pst))	= elementsASyncIO rId itemHs ((ls1,ls),pst)
				= (done,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,pst))
			elementASyncIO rId (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,pst)
				# (done,itemHs,(ls1,pst))	= elementsASyncIO rId itemHs (ls1,pst)
				= (done,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,pst))
			elementASyncIO _ itemH (ls,pst)
				= (False,itemH,(ls,pst))
		elementsASyncIO _ _ (ls,pst)
			= (False,[],(ls,pst))

	//	menuSyncIO lets the indicated receiver control handle the synchronous message.
	menuSyncIO :: !Id !SyncMessage !(MenuHandle .ls .pst) *(.ls,.pst) -> (!MsgEvent,MenuHandle .ls .pst,*(.ls,.pst))
	menuSyncIO r2Id msg mH=:{mItems} (ls,pst)
		= (SyncMessage msg1,{mH & mItems=itemHs},ls_ps1)
	where
		(_,msg1,itemHs,ls_ps1)	= elementsSyncIO r2Id msg mItems (ls,pst)
		
		elementsSyncIO :: !Id !SyncMessage ![MenuElementHandle .ls .pst] *(.ls,.pst)
					-> (!Bool,!SyncMessage, [MenuElementHandle .ls .pst],*(.ls,.pst))
		elementsSyncIO r2Id msg [itemH:itemHs] ls_ps
			# (done,msg,itemH,ls_ps)		= elementSyncIO r2Id msg itemH ls_ps
			| done
				= (done,msg,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO r2Id msg itemHs ls_ps
				= (done,msg,[itemH:itemHs],ls_ps)
		where
			elementSyncIO :: !Id !SyncMessage !(MenuElementHandle .ls .pst) *(.ls,.pst)
					   -> (!Bool,!SyncMessage,  MenuElementHandle .ls .pst, *(.ls,.pst))
			elementSyncIO rId msg mrH=:(MenuReceiverHandle itemH=:{mReceiverHandle=rH}) ls_ps
				| rId<>rH.rId
					= (False,msg,mrH,ls_ps)
				| otherwise
					# (msg,rH,ls_ps)= receiverSyncIO msg rH ls_ps
					= (True,msg,MenuReceiverHandle {itemH & mReceiverHandle=rH},ls_ps)
			where
				receiverSyncIO :: !SyncMessage !(ReceiverHandle .ls .pst) *(.ls,.pst)
							  -> (!SyncMessage,  ReceiverHandle .ls .pst, *(.ls,.pst))
				receiverSyncIO msg rH ls_ps
					# (response,rH,ls_ps)	= receiverHandleSyncMessage msg rH ls_ps
					= ({msg & smResp=response},rH,ls_ps)
			elementSyncIO rId msg (SubMenuHandle itemH=:{mSubItems=itemHs}) ls_ps
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO rId msg itemHs ls_ps
				= (done,msg,SubMenuHandle {itemH & mSubItems=itemHs},ls_ps)
			elementSyncIO rId msg (MenuListLSHandle itemHs) ls_ps
				# (done,msg,itemHs,ls_ps)	= elementsSyncIO rId msg itemHs ls_ps
				= (done,msg,MenuListLSHandle itemHs,ls_ps)
			elementSyncIO rId msg (MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs}) (ls,ps)
				# (done,msg,itemHs,((ls1,ls),ps))	= elementsSyncIO rId msg itemHs ((ls1,ls),ps)
				= (done,msg,MenuExtendLSHandle {mExtendLS=ls1,mExtendItems=itemHs},(ls,ps))
			elementSyncIO rId msg (MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs}) (ls,ps)
				# (done,msg,itemHs,(ls1,ps))	= elementsSyncIO rId msg itemHs (ls1,ps)
				= (done,msg,MenuChangeLSHandle {mChangeLS=ls1,mChangeItems=itemHs},(ls,ps))
			elementSyncIO rId msg itemH ls_ps
				= (False,msg,itemH,ls_ps)
		elementsSyncIO _ msg _ ls_ps
			= (False,msg,[],ls_ps)


/*	Check if the interactive process is active:
	Note:	this test depends on the fact that every interactive process has an AppleMenu,
			with mac menu ID AppleMenuId. This is taken care of by the creation
			of the interactive process (see menuOpen above).
*/
//import menus

ioStIsActive :: !(IOSt .l) -> (!Bool, !IOSt .l)
ioStIsActive ioState
	# (osdinfo,ioState)	= ioStGetOSDInfo ioState
	= accIOToolbox (osOSDInfoIsActive osdinfo) ioState

/*	activate the interactive process:
*/
activateMenuSystem :: !(IOSt .l) -> IOSt .l
activateMenuSystem ioState
	# ioState					= selectIOSt ioState
	# (osdinfo,ioState)			= ioStGetOSDInfo ioState
	  maybeOSMenuBar			= getOSDInfoOSMenuBar osdinfo
	| isNothing maybeOSMenuBar
		= ioState
	| otherwise
		# osMenuBar				= fromJust maybeOSMenuBar
		# (osMenuBar,ioState)	= accIOToolbox (osMenuBarSet osMenuBar) ioState
		  osdinfo				= setOSDInfoOSMenuBar osMenuBar osdinfo
		# ioState				= ioStSetOSDInfo osdinfo ioState
		= ioState
