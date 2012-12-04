implementation module menuitems


import	StdBool, StdList, StdMisc, StdTuple
import	commondef, menuaccess, menucreate, receivertable, receiverhandle
import	osdocumentinterface, osmenu, ostypes
from	iostate	import getIOToolbox, setIOToolbox, :: IOSt, :: PSt(..)


/*	Adding menu elements to (Sub)Menus:
		Items in a (sub)menu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front.
		Open with a position higher than the number of items adds the new elements to the end.
		Open an item on a position adds the item AFTER the item on that position.
	The (Id,Maybe Id) argument indicates where the elements should be added. 
		In case the Maybe Id argument is Nothing, then the elements should be added to the Menu indicated by the Id component. 
		In case the Maybe Id argument is Just id, then the elements should be added to the SubMenu indicated by id.
*/
addMenusItems :: !(!Id,Maybe Id) !Int .ls` .(m .ls` (PSt .l)) !SystemId !*ReceiverTable !*IdTable !(MenuHandles (PSt .l)) !OSMenuBar !(PSt .l)
													-> (!*(!ErrorReport,!*ReceiverTable,!*IdTable),!MenuHandles (PSt .l), !OSMenuBar, !PSt .l)
													|  MenuElements m
addMenusItems loc pos ls new pid rt it menus=:{mMenus,mKeys} osMenuBar pState
	# (newItemHs,pState)				= menuElementToHandles new pState
	  newItemHs							= map menuElementStateToMenuElementHandle newItemHs
	  (ok,newItemHs,rt,it)				= menuIdsAreConsistent pid (fst loc) newItemHs rt it
	| not ok
		= ((ErrorIdsInUse,rt,it),menus,osMenuBar,pState)
	| otherwise
		# (tb,ioState)					= getIOToolbox pState.io
		# (error,_,_,rt,it,mHs,keys,tb)	= addMenusItems` loc pos pid ls newItemHs rt it mMenus mKeys tb
		# ioState						= setIOToolbox tb ioState
		# pState						= {pState & io=ioState}
		= ((error,rt,it),{menus & mMenus=mHs,mKeys=keys},osMenuBar,pState)
where
	addMenusItems` :: !(!Id,Maybe Id) !Int !SystemId
							.ls` *[MenuElementHandle .ls` (PSt .l)] !*ReceiverTable !*IdTable ![MenuStateHandle (PSt .l)] ![Char] !*OSToolbox
		   -> (!ErrorReport,.ls`,*[MenuElementHandle .ls` (PSt .l)],!*ReceiverTable,!*IdTable,![MenuStateHandle (PSt .l)],![Char],!*OSToolbox)
	addMenusItems` loc pos pid ls new rt it [MenuLSHandle mlsH=:{mlsHandle}:mHs] keys tb
		# (opened,error,ls,new,rt,it,mlsHandle,keys,tb)	= addMenuItems loc pos pid ls new rt it mlsHandle keys tb
		  mH											= MenuLSHandle {mlsH & mlsHandle=mlsHandle}
		| opened
			= (error,ls,new,rt,it,[mH:mHs],keys,tb)
		| otherwise
			# (error,ls,new,rt,it,    mHs, keys,tb)		= addMenusItems` loc pos pid ls new rt it mHs keys tb
			= (error,ls,new,rt,it,[mH:mHs],keys,tb)
	where
		addMenuItems :: !(!Id,Maybe Id) !Int !SystemId
									.ls` *[MenuElementHandle .ls` (PSt .l)] !*ReceiverTable !*IdTable !(MenuHandle .ls (PSt .l)) ![Char] !*OSToolbox
			 -> (!Bool,!ErrorReport,.ls`,*[MenuElementHandle .ls` (PSt .l)],!*ReceiverTable,!*IdTable, !MenuHandle .ls (PSt .l), ![Char],!*OSToolbox)
		addMenuItems (mId,itemId) pos pid ls new rt it mH=:{mHandle,mMenuId,mItems} keys tb
			| mId<>mMenuId
				= (False,NoError,ls,new,rt,it,mH,keys,tb)
			| isJust itemId
				# (_,   error,ls,new,rt,it,items,keys,_,tb)		= addSubMenuItems` mId (fromJust itemId) pos mHandle pid ls new rt it mItems keys 1 tb
				= (True,error,ls,new,rt,it,{mH & mItems=items},keys,tb)
			| otherwise
				# (_,   error,ls,new,rt,it,itemHs,keys,_,_,tb)	= extendMenu` mId mHandle pid ls new rt it mItems keys pos 1 tb
				= (True,error,ls,new,rt,it,{mH & mItems=itemHs},keys,tb)
		where
			addSubMenuItems` :: !Id !Id !Int !OSMenu !SystemId 
													.ls` !*[MenuElementHandle .ls` (PSt .l)] !*ReceiverTable !*IdTable ![MenuElementHandle .ls (PSt .l)] ![Char] !Int !*OSToolbox
							 -> (!Bool,!ErrorReport,.ls`,!*[MenuElementHandle .ls` (PSt .l)],!*ReceiverTable,!*IdTable,![MenuElementHandle .ls (PSt .l)],![Char],!Int,!*OSToolbox)
			addSubMenuItems` menuId itemId pos menu pid ls new rt it [item:items] keys iNr tb
				# (opened,error,ls,new,rt,it,item,keys,iNr,tb)		= addSubMenuItems menuId itemId pos menu pid ls new rt it item keys iNr tb
				| opened
					= (opened,error,ls,new,rt,it,[item:items],keys,iNr,tb)
				| otherwise
					# (opened,error,ls,new,rt,it,items,keys,iNr,tb)	= addSubMenuItems` menuId itemId pos menu pid ls new rt it items keys iNr tb
					= (opened,error,ls,new,rt,it,[item:items],keys,iNr,tb)
			addSubMenuItems` _ _ _ _ _ ls new rt it _ keys iNr tb
				= (False,NoError,ls,new,rt,it,[],keys,iNr,tb)
			
			addSubMenuItems :: !Id !Id !Int !OSMenu !SystemId
													.ls` !*[MenuElementHandle .ls` (PSt .l)] !*ReceiverTable !*IdTable !(MenuElementHandle .ls (PSt .l)) ![Char] !Int !*OSToolbox
							 -> (!Bool,!ErrorReport,.ls`,!*[MenuElementHandle .ls` (PSt .l)],!*ReceiverTable,!*IdTable, !MenuElementHandle .ls (PSt .l), ![Char],!Int,!*OSToolbox)
			addSubMenuItems menuId itemId pos menu pid ls new rt it (SubMenuHandle subH=:{mSubHandle,mSubMenuId,mSubItems}) keys iNr tb
				| isNothing mSubMenuId || itemId<>fromJust mSubMenuId
					# (opened,error,ls,new,rt,it,items,keys,_,tb)	= addSubMenuItems` menuId itemId pos mSubHandle pid ls new rt it mSubItems keys 1 tb
					  subH											= {subH & mSubItems=items}
					= (opened,error,ls,new,rt,it,SubMenuHandle subH,keys,iNr+1,tb)
				| otherwise
					# (_,error,ls,new,rt,it,itemHs,keys,_,_,tb)		= extendMenu` menuId mSubHandle pid ls new rt it mSubItems keys pos 1 tb
					  subH											= {subH & mSubItems=itemHs}
					= (True,error,ls,new,rt,it,SubMenuHandle subH,keys,iNr+1,tb)
			addSubMenuItems _ _ _ _ _ ls new rt it (RadioMenuHandle itemH=:{mRadioItems=itemHs}) keys iNr tb
				# (nrRadioItemHs,itemHs)							= ulength itemHs
				# radioH											= RadioMenuHandle {itemH & mRadioItems=itemHs}
				= (False,NoError,ls,new,rt,it,radioH,keys,iNr+nrRadioItemHs,tb)
			addSubMenuItems menuId itemId pos menu pid ls new rt it (MenuListLSHandle mListItems) keys iNr tb
				# (opened,error,ls,new,rt,it,mListItems,keys,iNr,tb)	= addSubMenuItems` menuId itemId pos menu pid ls new rt it mListItems keys iNr tb
				= (opened,error,ls,new,rt,it,MenuListLSHandle mListItems,keys,iNr,tb)
			addSubMenuItems menuId itemId pos menu pid ls new rt it (MenuExtendLSHandle mH=:{mExtendItems}) keys iNr tb
				# (opened,error,ls,new,rt,it,mExtendItems,keys,iNr,tb)	= addSubMenuItems` menuId itemId pos menu pid ls new rt it mExtendItems keys iNr tb
				= (opened,error,ls,new,rt,it,MenuExtendLSHandle {mH & mExtendItems=mExtendItems},keys,iNr,tb)
			addSubMenuItems menuId itemId pos menu pid ls new rt it (MenuChangeLSHandle mH=:{mChangeItems}) keys iNr tb
				# (opened,error,ls,new,rt,it,mChangeItems,keys,iNr,tb)	= addSubMenuItems` menuId itemId pos menu pid ls new rt it mChangeItems keys iNr tb
				= (opened,error,ls,new,rt,it,MenuChangeLSHandle {mH & mChangeItems=mChangeItems},keys,iNr,tb)
			addSubMenuItems _ _ _ _ _ ls new rt it itemH keys iNr tb
				= (False,NoError,ls,new,rt,it,itemH,keys,iNr+1,tb)
			
			extendMenu` :: !Id !OSMenu !SystemId .ls` !*[MenuElementHandle .ls` (PSt .l)] !*ReceiverTable !*IdTable !*[MenuElementHandle .ls (PSt .l)] [Char] !Int !Int !*OSToolbox
						  -> (!Bool,!ErrorReport,.ls`,!*[MenuElementHandle .ls` (PSt .l)],!*ReceiverTable,!*IdTable,!*[MenuElementHandle .ls (PSt .l)],[Char],!Int,!Int,!*OSToolbox)
			extendMenu` menuId menu pid ls new rt it itemHs keys 0 iNr tb
				# newItemHs			= MenuChangeLSHandle {mChangeLS=ls,mChangeItems=new}
				# (itemHs,keys,tb)	= extendMenu osMenuBar menu (iNr-1) [newItemHs] itemHs keys tb
				= (True,NoError,undef,[],rt,it,itemHs,keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it itemHs=:[] keys pos iNr tb
				# newItemHs			= MenuChangeLSHandle {mChangeLS=ls,mChangeItems=new}
				# (itemHs,keys,tb)	= extendMenu osMenuBar menu (iNr-1) [newItemHs] itemHs keys tb
				= (True,NoError,undef,[],rt,it,itemHs,keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [itemH=:(MenuItemHandle _):itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs keys (pos-1) (iNr+1) tb
				= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [itemH=:(MenuReceiverHandle _):itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs keys pos iNr tb
				= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [itemH=:(SubMenuHandle _):itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs keys (pos-1) (iNr+1) tb
				= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [(RadioMenuHandle itemH=:{mRadioItems=radioItemHs}):itemHs] keys pos iNr tb
				# (nrRadioItems,radioItemHs)							= ulength radioItemHs
				# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs keys (pos-1) (iNr+nrRadioItems) tb
				# radioH												= RadioMenuHandle {itemH & mRadioItems=radioItemHs}
				= (opened,error,ls,new,rt,it,[radioH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [itemH=:(MenuSeparatorHandle _):itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs keys (pos-1) (iNr+1) tb
				= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [MenuListLSHandle itemHs`:itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs`,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs` keys pos iNr tb
				  itemH													= MenuListLSHandle itemHs`
				| opened
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
				| otherwise
					# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)= extendMenu` menuId menu pid ls new rt it itemHs  keys pos iNr tb
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [MenuExtendLSHandle mExH=:{mExtendItems=itemHs`}:itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs`,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs` keys pos iNr tb
				  itemH													= MenuExtendLSHandle {mExH & mExtendItems=itemHs`}
				| opened
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
				| otherwise
					# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)= extendMenu` menuId menu pid ls new rt it itemHs keys pos iNr tb
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
			extendMenu` menuId menu pid ls new rt it [MenuChangeLSHandle mChH=:{mChangeItems=itemHs`}:itemHs] keys pos iNr tb
				# (opened,error,ls,new,rt,it,itemHs`,keys,pos,iNr,tb)	= extendMenu` menuId menu pid ls new rt it itemHs` keys pos iNr tb
				  itemH													= MenuChangeLSHandle {mChH & mChangeItems=itemHs`}
				| opened
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
				| otherwise
					# (opened,error,ls,new,rt,it,itemHs,keys,pos,iNr,tb)= extendMenu` menuId menu pid ls new rt it itemHs  keys pos iNr tb
					= (opened,error,ls,new,rt,it,[itemH:itemHs],keys,pos,iNr,tb)
	addMenusItems` _ _ _ ls new rt it [] keys tb
		= (ErrorUnknownObject,ls,new,rt,it,[],keys,tb)


/*	Adding radio menu items to RadioMenus:
		Items in a RadioMenu are positioned starting from 1 and increasing by 1.
		Open with a position less than 1 adds the new elements in front.
		Open with a position higher than the number of items adds the new elements to the end.
		Open an item on a position adds the item AFTER the item on that position.
	The (Id,Id) argument indicates where the elements should be added. 
		The first Id indicates the Menu, the second Id indicates the RadioMenu.
*/
addMenuRadioItems :: !(!Id,Id) !Int [MenuRadioItem (PSt .l)] !OSMenuBar !(MenuHandles (PSt .l)) !*OSToolbox
													   -> *(!ErrorReport,!MenuHandles (PSt .l), !*OSToolbox)
addMenuRadioItems loc pos new osMenuBar menus=:{mMenus,mKeys} tb
	# (error,mHs,keys,tb)	= addMenusItems` loc pos new mMenus mKeys tb
	= (error,{menus & mMenus=mHs,mKeys=keys}, tb)
where
	addMenusItems` :: !(!Id,Id) !Int [MenuRadioItem .ps] ![MenuStateHandle .ps] ![Char] !*OSToolbox
										-> (!ErrorReport,![MenuStateHandle .ps],![Char],!*OSToolbox)
	addMenusItems` loc pos new [MenuLSHandle mlsH=:{mlsHandle}:mHs] keys tb
		# (opened,error,mlsHandle,keys,tb)	= addMenuItems loc pos new mlsHandle keys tb
		  mH								= MenuLSHandle {mlsH & mlsHandle=mlsHandle}
		| opened
			= (error,[mH:mHs],keys,tb)
		| otherwise
			# (error,mHs,keys,tb)			= addMenusItems` loc pos new mHs keys tb
			= (error,[mH:mHs],keys,tb)
	where
		addMenuItems :: !(!Id,Id) !Int [MenuRadioItem .ps] !(MenuHandle .ls .ps) ![Char] !*OSToolbox
									-> (!Bool,!ErrorReport, !MenuHandle .ls .ps, ![Char],!*OSToolbox)
		addMenuItems (mId,itemId) pos new mH=:{mHandle,mMenuId,mItems} keys tb
			| mId<>mMenuId
				= (False,NoError,mH,keys,tb)
			| otherwise
				# (_,error,itemHs,keys,_,tb)	= addSubMenuItems` itemId pos mHandle new mItems keys 1 tb
				  mH							= {mH & mItems=itemHs}
				= (True,error,mH,keys,tb)
		where
			addSubMenuItems` :: !Id !Int !OSMenu [MenuRadioItem .ps] !*[MenuElementHandle .ls .ps] ![Char] !Int !*OSToolbox
											  -> (!Bool,!ErrorReport,!*[MenuElementHandle .ls .ps],![Char],!Int,!*OSToolbox)
			addSubMenuItems` itemId pos menu new [itemH:itemHs] keys iNr tb
				# (opened,error,itemH,sIds,iNr,tb)		= addSubMenuItems itemId pos menu new itemH keys iNr tb
				| opened
					= (opened,error,[itemH:itemHs],keys,iNr,tb)
				| otherwise
					# (opened,error,itemHs,keys,iNr,tb)	= addSubMenuItems` itemId pos menu new itemHs sIds iNr tb
					= (opened,error,[itemH:itemHs],keys,iNr,tb)
			addSubMenuItems` _ _ _ _ _ keys iNr tb
				= (False,ErrorUnknownObject,[],keys,iNr,tb)
			
			addSubMenuItems :: !Id !Int !OSMenu [MenuRadioItem .ps] !*(MenuElementHandle .ls .ps) ![Char] !Int !*OSToolbox
											 -> (!Bool,!ErrorReport, !*MenuElementHandle .ls .ps, ![Char],!Int,!*OSToolbox)
			addSubMenuItems itemId pos menu new (SubMenuHandle subH=:{mSubHandle,mSubItems}) keys iNr tb
				# (opened,error,itemHs,keys,_,tb)	= addSubMenuItems` itemId pos mSubHandle new mSubItems keys 1 tb
				= (opened,error,SubMenuHandle {subH & mSubItems=itemHs},keys,iNr+1,tb)
			addSubMenuItems itemId pos menu new itemH=:(RadioMenuHandle radioH=:{mRadioId,mRadioIndex,mRadioItems}) keys iNr tb
				# (nrItems,mRadioItems)		= ulength mRadioItems
				| isNothing mRadioId || itemId<>fromJust mRadioId
					# itemH					= RadioMenuHandle {radioH & mRadioItems=mRadioItems}
					= (False,NoError,itemH,keys,iNr+nrItems,tb)
				# newItemHs					= map (\(a,b,c,f)->radioMenuItemToMenuElementHandle (a,b,c,noLS f)) new
				# (nrNewItems,newItemHs)	= ulength newItemHs
				  pos						= setBetween pos 0 nrItems
				  index						= if (pos<mRadioIndex) (mRadioIndex+nrNewItems) (max 1 mRadioIndex)
				# (itemHs,keys,tb)			= extendMenu` iNr pos menu newItemHs mRadioItems keys tb
				| nrItems<>0
					= (True,NoError,RadioMenuHandle {radioH & mRadioIndex=index,mRadioItems=itemHs},keys,iNr+nrItems+nrNewItems,tb)
				| otherwise
					# (before,[itemH:after])= splitAt (index-1) itemHs
					# (itemHandle,itemH)	= (\(MenuItemHandle itemH=:{mOSMenuItem})->(mOSMenuItem,MenuItemHandle itemH)) itemH
					# itemHs				= before ++ [itemH:after]
					# tb					= osMenuItemCheck True menu itemHandle index iNr tb
					= (True,NoError,RadioMenuHandle {radioH & mRadioIndex=1,    mRadioItems=itemHs},keys,iNr+nrItems+nrNewItems,tb)
			addSubMenuItems itemId pos menu new (MenuListLSHandle mListItems) keys iNr tb
				# (opened,error,mListItems,keys,iNr,tb)	= addSubMenuItems` itemId pos menu new mListItems keys iNr tb
				= (opened,error,MenuListLSHandle mListItems,keys,iNr,tb)
			addSubMenuItems itemId pos menu new (MenuExtendLSHandle mH=:{mExtendItems}) keys iNr tb
				# (opened,error,mExtendItems,keys,iNr,tb)	= addSubMenuItems` itemId pos menu new mExtendItems keys iNr tb
				= (opened,error,MenuExtendLSHandle {mH & mExtendItems=mExtendItems},keys,iNr,tb)
			addSubMenuItems itemId pos menu new (MenuChangeLSHandle mH=:{mChangeItems}) keys iNr tb
				# (opened,error,mChangeItems,keys,iNr,tb)	= addSubMenuItems` itemId pos menu new mChangeItems keys iNr tb
				= (opened,error,MenuChangeLSHandle {mH & mChangeItems=mChangeItems},keys,iNr,tb)
			addSubMenuItems _ _ _ _ itemH=:(MenuReceiverHandle _) keys iNr tb
				= (False,NoError,itemH,keys,iNr,tb)
			addSubMenuItems _ _ _ _ itemH keys iNr tb
				= (False,NoError,itemH,keys,iNr+1,tb)
			
			extendMenu` :: !Int !Int !OSMenu !*[MenuElementHandle .ls .pst]
						!*[MenuElementHandle .ls .pst] ![Char] !*OSToolbox
					-> (!*[MenuElementHandle .ls .pst],![Char],!*OSToolbox)
			extendMenu` iNr 0 menu new items keys tb
				= extendMenu osMenuBar menu (iNr-1) new items keys tb
			extendMenu` iNr position menu new [item:items] keys tb
				# (items,keys,tb) = extendMenu` (iNr+1) (position-1) menu new items keys tb
				= ([item:items],keys,tb)
			extendMenu` iNr position menu new items keys tb
				= extendMenu osMenuBar menu (iNr-1) new items keys tb
	addMenusItems` _ _ _ [] keys tb
		= (ErrorUnknownObject,[],keys,tb)


//	Removing menu elements from (sub/radio)menus:

removeMenusItems :: !OSDInfo !Id ![Id] !SystemId !OSMenuBar !*(!*ReceiverTable,!*IdTable) !(MenuHandles .pst) !*OSToolbox
														-> (!*(!*ReceiverTable,!*IdTable), !MenuHandles .pst, !*OSToolbox)
removeMenusItems osdInfo mId ids pid _ (rt,it) menus=:{mMenus,mKeys} tb
	# (rt,it,mHs,keys,tb)	= removeMenusItems` framePtr mId ids pid rt it mMenus mKeys tb
	= ((rt,it),{menus & mMenus=mHs,mKeys=keys},tb)
where
	framePtr	= case (getOSDInfoOSInfo osdInfo) of
					Just info -> info.osFrame
					_         -> OSNoWindowPtr
	
	removeMenusItems` :: !OSWindowPtr !Id ![Id] !SystemId !*ReceiverTable !*IdTable ![MenuStateHandle .ps] ![Char] !*OSToolbox
													  -> (!*ReceiverTable,!*IdTable,![MenuStateHandle .ps],![Char],!*OSToolbox)
	removeMenusItems` _ _ [] _ rt it mHs keys tb
		= (rt,it,mHs,keys,tb)
	removeMenusItems` framePtr mId ids pid rt it [MenuLSHandle mlsH=:{mlsHandle=mH=:{mMenuId}}:mHs] keys tb
		| mId<>mMenuId
			# (rt,it,mHs,keys,tb)	= removeMenusItems` framePtr mId ids pid rt it mHs keys tb
			= (rt,it,[MenuLSHandle mlsH:mHs],keys,tb)
		| otherwise
			# (rt,it,_,mH,keys,tb)	= removeMenuItems framePtr pid rt it ids mH keys tb
			= (rt,it,[MenuLSHandle {mlsH & mlsHandle=mH}:mHs],keys,tb)
	where
		removeMenuItems :: !OSWindowPtr !SystemId !*ReceiverTable !*IdTable ![Id] !(MenuHandle .ls .ps) ![Char] !*OSToolbox
											  -> (!*ReceiverTable,!*IdTable,![Id], !MenuHandle .ls .ps, ![Char],!*OSToolbox)
		removeMenuItems framePtr pid rt it ids mH=:{mHandle,mItems} keys tb
			# (_,rt,it,ids,mItems,keys,tb) = removeFromMenu` framePtr mHandle pid 1 rt it ids mItems keys tb
			= (  rt,it,ids,{mH & mItems=mItems},keys,tb)
		where
			removeFromMenu` :: !OSWindowPtr !OSMenu !SystemId !Int !*ReceiverTable !*IdTable ![Id] !*[MenuElementHandle .ls .ps] ![Char] !*OSToolbox
														  -> (!Int,!*ReceiverTable,!*IdTable,![Id],!*[MenuElementHandle .ls .ps],![Char],!*OSToolbox)
			removeFromMenu` _ _ _ iNr rt it ids [] keys tb
				= (iNr,rt,it,ids,[],keys,tb)
			removeFromMenu` framePtr menu pid iNr rt it ids [item:items] keys tb
				| isEmpty ids
					= (iNr,rt,it,ids,[item:items],keys,tb)
				# (removed,iNr,rt,it,ids,item, keys,tb)	= removeFromMenu  framePtr menu pid iNr rt it ids item  keys tb
				# (        iNr,rt,it,ids,items,keys,tb)	= removeFromMenu` framePtr menu pid iNr rt it ids items keys tb
				| removed
					= (iNr,rt,it,ids,      items, keys,tb)
				| otherwise
					= (iNr,rt,it,ids,[item:items],keys,tb)
			
			removeFromMenu :: !OSWindowPtr !OSMenu !SystemId !Int !*ReceiverTable !*IdTable ![Id] !*(MenuElementHandle .ls .ps) ![Char] !*OSToolbox
												   -> (!Bool,!Int,!*ReceiverTable,!*IdTable,![Id], !*MenuElementHandle .ls .ps, ![Char],!*OSToolbox)
			removeFromMenu framePtr menu pid iNr rt it ids (MenuItemHandle itemH=:{mItemId,mItemKey,mOSMenuItem}) keys tb
				# (containsItem,ids)			= if (isNothing mItemId) (False,ids) (removeCheck (fromJust mItemId) ids)
				| not containsItem
					= (containsItem,iNr+1,rt,it,ids,MenuItemHandle itemH,keys,tb)
				| otherwise
					# (itemH,(keys,it,tb))		= disposeMenuItemHandle menu iNr itemH (keys,it,tb)
					= (containsItem,iNr,rt,it,ids,MenuItemHandle itemH,keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids itemH=:(SubMenuHandle subH=:{mSubHandle,mSubMenuId,mSubOSMenuNr,mSubItems}) keys tb
				# (containsItem,ids)			= if (isNothing mSubMenuId) (False,ids) (removeCheck (fromJust mSubMenuId) ids)
				# (_,rt,it,ids,itemHs,keys,tb)	= removeFromMenu` framePtr mSubHandle pid 1 rt it ids mSubItems keys tb
				| not containsItem
					= (containsItem,iNr+1,rt,it,ids,SubMenuHandle {subH & mSubItems=itemHs},keys,tb)
				| otherwise
					# (itemHs,(rt,it))			= stateMap (disposeMenuIds pid) itemHs (rt,it)
					# (itemHs,(keys,tb))		= stateMap (disposeShortcutkeys framePtr) itemHs (keys,tb)
					# (itemHs,(_,tb))			= stateMap disposeSubMenuHandles itemHs (menu,tb)
//					# (_,tb)					= osSubMenuRemove mSubHandle menu tb
					# (_,tb)					= osSubMenuRemove mSubHandle menu mSubOSMenuNr iNr tb
					= (containsItem,iNr,rt,it,ids,SubMenuHandle {subH & mSubItems=itemHs},keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids (RadioMenuHandle radioH=:{mRadioId,mRadioIndex,mRadioItems}) keys tb
				# (containsItem,ids)			= if (isNothing mRadioId)
													(False,ids)
													(removeCheck (fromJust mRadioId) ids)
				  items							= confirmRadioMenuIndex mRadioIndex mRadioItems
				# (_,rt,it,ids,items,keys,tb)	= removeFromMenu` framePtr menu pid iNr rt it ids items keys tb
				| containsItem
					# (_,(keys,it,tb))			= stateMap (\(MenuItemHandle itemH)->disposeMenuItemHandle menu iNr itemH) items (keys,it,tb)
					# h							= RadioMenuHandle {radioH & mRadioItems=[]}
					= (containsItem,iNr,rt,it,ids,h,keys,tb)
				| otherwise
					# (index,items,tb)			= checkNewRadioMenuIndex menu iNr items tb
					  (nrNewItems,items)		= ulength items
					  h							= RadioMenuHandle {radioH & mRadioItems=items,mRadioIndex=index}
				= (containsItem,iNr+nrNewItems,rt,it,ids,h,keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids h=:(MenuSeparatorHandle {mSepId,mOSMenuSeparator}) keys tb
				# (containsItem,ids)			= if (isNothing mSepId) (False,ids) (removeCheck (fromJust mSepId) ids)
				| not containsItem
					= (containsItem,iNr+1,rt,it,ids,h,keys,tb)
				# (_,tb)						= osMenuRemoveItem mOSMenuSeparator iNr menu tb
				| otherwise
					# (_,it)					= removeIdFromIdTable (fromJust mSepId) it
					= (containsItem,iNr,rt,it,ids,h,keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids h=:(MenuReceiverHandle {mReceiverHandle=rH=:{rId}}) keys tb
				# (containsItem,ids)			= removeCheck rId ids
				| not containsItem
					= (containsItem,iNr,rt,it,ids,h,keys,tb)
				| otherwise
					# (_,rt)					= removeReceiverFromReceiverTable rId rt
					  (_,it)					= removeIdFromIdTable rId it
					= (containsItem,iNr,rt,it,ids,h,keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids (MenuListLSHandle items) keys tb
				# (iNr,rt,it,ids,items,keys,tb)	= removeFromMenu` framePtr menu pid iNr rt it ids items keys tb
				= (False,iNr,rt,it,ids,MenuListLSHandle items,keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids (MenuExtendLSHandle mH=:{mExtendItems=items}) keys tb
				# (iNr,rt,it,ids,items,keys,tb)	= removeFromMenu` framePtr menu pid iNr rt it ids items keys tb
				= (False,iNr,rt,it,ids,MenuExtendLSHandle {mH & mExtendItems=items},keys,tb)
			
			removeFromMenu framePtr menu pid iNr rt it ids (MenuChangeLSHandle mH=:{mChangeItems=items}) keys tb
				# (iNr,rt,it,ids,items,keys,tb)	= removeFromMenu` framePtr menu pid iNr rt it ids items keys tb
				= (False,iNr,rt,it,ids,MenuChangeLSHandle {mH & mChangeItems=items},keys,tb)

	removeMenusItems` _ _ _ _ rt it [] keys tb
		= (rt,it,[],keys,tb)


/*	Removing menu elements from (sub/radio)menus by index (counting from 1):
	The second Boolean argument indicates whether the elements to be removed should be removed
		from RadioMenus (True) or (Sub)Menus (False).  
	The (Id,Maybe Id) argument indicates where the elements should be removed. 
		In case the Maybe Id argument is Nothing, then the elements should be removed from 
		the Menu indicated by the Id component. 
		In case the Maybe Id argument is Just id, then the elements should be removed from
		either a SubMenu or a RadioMenu identified by the Id component.
*/
removeMenusIndexItems :: !OSDInfo !Bool !Bool !(!Id,!Maybe Id) ![Int] !SystemId !OSMenuBar
							!*(!*ReceiverTable,!*IdTable) !(MenuHandles .pst) !*OSToolbox
						-> (!*(!*ReceiverTable,!*IdTable), !MenuHandles .pst, !*OSToolbox)
removeMenusIndexItems osdInfo alsoSpecials fromRadioMenu loc indices pid _ (rt,it) menus=:{mMenus,mKeys} tb
	# (rt,it,mHs,keys,tb)	= removeMenusIndexItems` framePtr alsoSpecials fromRadioMenu loc indices pid rt it mMenus mKeys tb
	= ((rt,it),{menus & mMenus=mHs,mKeys=keys},tb)
where
	framePtr	= case (getOSDInfoOSInfo osdInfo) of
					Just info -> info.osFrame
					_         -> OSNoWindowPtr
	
	removeMenusIndexItems` :: !OSWindowPtr !Bool !Bool !(!Id,!Maybe Id) ![Int] !SystemId
								!*ReceiverTable !*IdTable ![MenuStateHandle .ps] ![Char] !*OSToolbox
							-> (!*ReceiverTable,!*IdTable,![MenuStateHandle .ps],![Char],!*OSToolbox)
	removeMenusIndexItems` framePtr alsoSpecials fromRadioMenu loc=:(mId,itemId) indices pid rt it [MenuLSHandle mlsH=:{mlsHandle=mH=:{mMenuId}}:mHs] keys tb
		| mId<>mMenuId
			# (rt,it,mHs,keys,tb)	= removeMenusIndexItems` framePtr alsoSpecials fromRadioMenu loc indices pid rt it mHs keys tb
			= (rt,it,[MenuLSHandle mlsH:mHs],keys,tb)
		| otherwise
			# (_,rt,it,mH,keys,tb)	= removeMenuIndexItems framePtr alsoSpecials fromRadioMenu itemId indices pid rt it mH keys tb
			= (  rt,it,[MenuLSHandle {mlsH & mlsHandle=mH}:mHs],keys,tb)
	removeMenusIndexItems` _ _ _ _ _ _ rt it [] keys tb
		= (rt,it,[],keys,tb)
	
	removeMenuIndexItems :: !OSWindowPtr !Bool !Bool !(Maybe Id) ![Int] !SystemId !*ReceiverTable !*IdTable !(MenuHandle .ls .ps) ![Char] !*OSToolbox
																	   -> (!Bool, !*ReceiverTable,!*IdTable, !MenuHandle .ls .ps, ![Char],!*OSToolbox)
	removeMenuIndexItems framePtr alsoSpecials fromRadioMenu itemId indices pid rt it mH=:{mHandle,mItems} keys tb
		| isNothing itemId
			# (_,_,rt,it,mItems,keys,tb)	= removeItems` framePtr alsoSpecials mHandle pid 1 indices rt it mItems keys tb
			= (True,rt,it,{mH & mItems=mItems},keys,tb)
		| otherwise
			# (done,_,rt,it,mItems,keys,tb)	= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid (fromJust itemId) indices mHandle 
												1 rt it mItems keys tb
			= (done,rt,it,{mH & mItems=mItems},keys,tb)
	where
		removeIndexsFromMenu` :: !OSWindowPtr !Bool !Bool !SystemId !Id ![Int] !OSMenu
								 !Int !*ReceiverTable !*IdTable !*[MenuElementHandle .ls .ps] ![Char] !*OSToolbox
					   -> (!Bool,!Int,!*ReceiverTable,!*IdTable,!*[MenuElementHandle .ls .ps],![Char],!*OSToolbox)
		removeIndexsFromMenu` _ _ _ _ _ _ _ iNr rt it [] keys tb
			= (False,iNr,rt,it,[],keys,tb)
		removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it [item:items] keys tb
			# (done,iNr,rt,it,item,keys,tb)		= removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it item keys tb
			| done
				= (done,iNr,rt,it,[item:items],keys,tb)
			| otherwise
				# (done,iNr,rt,it,items,keys,tb)= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it items keys tb
				= (done,iNr,rt,it,[item:items],keys,tb)
		
		removeIndexsFromMenu :: !OSWindowPtr !Bool !Bool !SystemId !Id ![Int] !OSMenu
								!Int !*ReceiverTable !*IdTable !*(MenuElementHandle .ls .ps) ![Char] !*OSToolbox
					  -> (!Bool,!Int,!*ReceiverTable,!*IdTable, !*MenuElementHandle .ls .ps, ![Char],!*OSToolbox)
		removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it
												  (SubMenuHandle subH=:{mSubHandle,mSubMenuId,mSubItems}) keys tb
			| isJust mSubMenuId && itemId==fromJust mSubMenuId && not fromRadioMenu
				# (_,_,rt,it,items,keys,tb)		= removeItems` framePtr alsoSpecials mSubHandle pid 1 indices rt it mSubItems keys tb
				= (True,iNr+1,rt,it,SubMenuHandle {subH & mSubItems=items},keys,tb)
			| otherwise
				# (done,_,rt,it,items,keys,tb)	= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices mSubHandle 
													1 rt it mSubItems keys tb
				= (done,iNr+1,rt,it,SubMenuHandle {subH & mSubItems=items},keys,tb)
		removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it
												  (RadioMenuHandle radioH=:{mRadioId,mRadioIndex,mRadioItems}) keys tb
			| isNothing mRadioId || itemId<>fromJust mRadioId || not fromRadioMenu
				= (False,iNr,rt,it,RadioMenuHandle radioH,keys,tb)
			| otherwise
				# iNrIndices					= map (\index->index+iNr-1) indices
				  items							= confirmRadioMenuIndex mRadioIndex mRadioItems
				# (_,_,rt,it,items,keys,tb)		= removeItems` framePtr alsoSpecials menu pid iNr iNrIndices rt it items keys tb
				# (index,items,tb)				= checkNewRadioMenuIndex menu iNr items tb
				= (True,iNr,rt,it,RadioMenuHandle {radioH & mRadioItems=items,mRadioIndex=index},keys,tb)
		removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuListLSHandle mListItems) keys tb
			# (removed,iNr,rt,it,mListItems,keys,tb)	= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mListItems keys tb
			= (removed,iNr,rt,it,MenuListLSHandle mListItems,keys,tb)
		removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuExtendLSHandle mH=:{mExtendItems}) keys tb
			# (removed,iNr,rt,it,mExtendItems,keys,tb)	= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mExtendItems keys tb
			= (removed,iNr,rt,it,MenuExtendLSHandle {mH & mExtendItems=mExtendItems},keys,tb)
		removeIndexsFromMenu framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it (MenuChangeLSHandle mH=:{mChangeItems}) keys tb
			# (removed,iNr,rt,it,mChangeItems,keys,tb)	= removeIndexsFromMenu` framePtr alsoSpecials fromRadioMenu pid itemId indices menu iNr rt it mChangeItems keys tb
			= (removed,iNr,rt,it,MenuChangeLSHandle {mH & mChangeItems=mChangeItems},keys,tb)
		removeIndexsFromMenu _ _ _ _ _ _ _ iNr rt it h sIds tb
			= (False,iNr+1,rt,it,h,sIds,tb)
		
		removeItems` :: !OSWindowPtr !Bool !OSMenu !SystemId !Int ![Int] !*ReceiverTable !*IdTable !*[MenuElementHandle .ls .ps] ![Char] !*OSToolbox
														 -> (!Int,![Int],!*ReceiverTable,!*IdTable,!*[MenuElementHandle .ls .ps],![Char],!*OSToolbox)
		removeItems` _ _ _ _ iNr indices rt it [] keys tb
			= (iNr,indices,rt,it,[],keys,tb)
		removeItems` framePtr alsoSpecials menu pid iNr indices rt it [item:items] keys tb
			| isEmpty indices
				= (iNr,indices,rt,it,[item:items],keys,tb)
			# (removed,iNr,indices,rt,it,item, keys,tb)	= removeItems  framePtr alsoSpecials menu pid iNr indices rt it item  keys tb
			# (        iNr,indices,rt,it,items,keys,tb)	= removeItems` framePtr alsoSpecials menu pid iNr indices rt it items keys tb
			| removed
				= (iNr,indices,rt,it,      items, keys,tb)
			| otherwise
				= (iNr,indices,rt,it,[item:items],keys,tb)
		
		removeItems :: !OSWindowPtr !Bool !OSMenu !SystemId !Int ![Int] !*ReceiverTable !*IdTable !(MenuElementHandle .ls .ps) ![Char] !*OSToolbox
												  -> (!Bool,!Int,![Int],!*ReceiverTable,!*IdTable, !MenuElementHandle .ls .ps, ![Char],!*OSToolbox)
		removeItems framePtr alsoSpecials menu pid iNr indices rt it itemH=:(SubMenuHandle subH=:{mSubHandle,mSubMenuId,mSubOSMenuNr,mSubItems}) keys tb
			# (containsItem,indices)= removeCheck iNr indices
			| not containsItem || (specialId && not alsoSpecials)
				= (False,iNr+1,indices,rt,it,itemH,keys,tb)
			| otherwise
				# (itemHs,(rt,it))	= stateMap (disposeMenuIds pid) mSubItems (rt,it)
				# (itemHs,(keys,tb))= stateMap (disposeShortcutkeys framePtr) itemHs (keys,tb)
				# (itemHs,(_,tb))	= stateMap	disposeSubMenuHandles itemHs (menu,tb)
//				# (_,tb)			= osSubMenuRemove mSubHandle menu tb
				# (_,tb)			= osSubMenuRemove mSubHandle menu mSubOSMenuNr iNr tb
				# indices			= map dec indices
				= (containsItem,iNr,indices,rt,it,SubMenuHandle {subH & mSubItems=itemHs},keys,tb)
		where
			specialId				= isJust mSubMenuId && isSpecialId (fromJust mSubMenuId)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuItemHandle mH=:{mItemId,mItemKey,mOSMenuItem}) keys tb
			# (containsItem,indices)= removeCheck iNr indices
			| not containsItem || (specialId && not alsoSpecials)
				= (False,iNr+1,indices,rt,it,MenuItemHandle mH,keys,tb)
			| otherwise
				# (mH,(keys,it,tb))	= disposeMenuItemHandle menu iNr mH (keys,it,tb)
				  indices			= map dec indices
				= (containsItem,iNr,indices,rt,it,MenuItemHandle mH,keys,tb)
		where
			specialId				= isJust mItemId && isSpecialId (fromJust mItemId)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it (RadioMenuHandle radioH=:{mRadioId,mRadioItems}) keys tb
			# (containsItem,indices)= removeCheck iNr indices
			| not containsItem || (specialId && not alsoSpecials)
				# (nrItems,items)	= ulength mRadioItems
				  itemH				= RadioMenuHandle {radioH & mRadioItems=items}
				  indices			= map ((+) (nrItems-1)) indices
				= (False,iNr+nrItems,indices,rt,it,itemH,keys,tb)
			| otherwise
				# (_,(keys,it,tb))	= stateMap (\(MenuItemHandle itemH)->disposeMenuItemHandle menu iNr itemH) mRadioItems (keys,it,tb)
				  itemH				= RadioMenuHandle {radioH & mRadioItems=[]}
				  indices			= map dec indices
				= (containsItem,iNr,indices,rt,it,itemH,keys,tb)
		where
			specialId				= isJust mRadioId && isSpecialId (fromJust mRadioId)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it h=:(MenuSeparatorHandle {mSepId,mOSMenuSeparator}) keys tb
			# (containsItem,indices)= removeCheck iNr indices
			| not containsItem || (specialId && not alsoSpecials)
				= (False,iNr+1,indices,rt,it,h,keys,tb)
			| otherwise
				# (_,tb)			= osMenuRemoveItem mOSMenuSeparator iNr menu tb
				  (_,it)			= removeIdFromIdTable (fromJust mSepId) it
				  indices			= map dec indices
				= (containsItem,iNr,indices,rt,it,h,keys,tb)
		where
			specialId				= isJust mSepId && isSpecialId (fromJust mSepId)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it itemH=:(MenuReceiverHandle recH=:{mReceiverHandle={rId}}) keys tb
			# (containsItem,indices)= removeCheck iNr indices
			| not containsItem
				= (False,iNr,indices,rt,it,itemH,keys,tb)
			| otherwise
				# (_,rt)			= removeReceiverFromReceiverTable rId rt
				  (_,it)			= removeIdFromIdTable rId it
				= (containsItem,iNr,indices,rt,it,itemH,keys,tb)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuListLSHandle mListItems) keys tb
			# (iNr,indices,rt,it,mListItems,keys,tb) = removeItems` framePtr alsoSpecials menu pid iNr indices rt it mListItems keys tb
			= (False,iNr,indices,rt,it,MenuListLSHandle mListItems,keys,tb)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuExtendLSHandle mH=:{mExtendItems}) keys tb
			# (iNr,indices,rt,it,items,keys,tb)	= removeItems` framePtr alsoSpecials menu pid iNr indices rt it mExtendItems keys tb
			= (False,iNr,indices,rt,it,MenuExtendLSHandle {mH & mExtendItems=items},keys,tb)
		
		removeItems framePtr alsoSpecials menu pid iNr indices rt it (MenuChangeLSHandle mH=:{mChangeItems}) keys tb
			# (iNr,indices,rt,it,items,keys,tb)	= removeItems` framePtr alsoSpecials menu pid iNr indices rt it mChangeItems keys tb
			= (False,iNr,indices,rt,it,MenuChangeLSHandle {mH & mChangeItems=items},keys,tb)


/*	confirmRadioMenuIndex ensures that only the menu item at the index position in the list has a True mItemMark field (counted from 1).
*/
confirmRadioMenuIndex :: !Int !*[MenuElementHandle .ls .ps] -> *[MenuElementHandle .ls .ps]
confirmRadioMenuIndex i [MenuItemHandle itemH:itemHs]
	# itemHs	= confirmRadioMenuIndex (i-1) itemHs
	= [MenuItemHandle {itemH & mItemMark=i==1}:itemHs]
confirmRadioMenuIndex _ []
	= []

/*	checkNewRadioMenuIndex yields the Index of the first checked menu item in the list.
	If the list is empty, then 0 is returned and no menu item is checked.
	If there is no checked menu item then 1 is returned, and the first menu item (if
	present) in the list is checked.
*/
checkNewRadioMenuIndex :: !OSMenu !Int !*[MenuElementHandle .ls .ps] !*OSToolbox -> (!Int,!*[MenuElementHandle .ls .ps],!*OSToolbox)
checkNewRadioMenuIndex menu iNr itemHs tb
	# (empty,itemHs)		= uisEmpty itemHs
	| empty
		= (0,itemHs,tb)
	# (found,index,itemHs)	= getNewRadioMenuIndex 1 itemHs
	| found
		= (index,itemHs,tb)
	| otherwise
		# (itemHs,tb)		= checkFirstRadioItem menu iNr itemHs tb
		= (1,itemHs,tb)
where
	getNewRadioMenuIndex :: !Int !*[MenuElementHandle .ls .ps] -> (!Bool,!Int,!*[MenuElementHandle .ls .ps])
	getNewRadioMenuIndex index [itemH=:(MenuItemHandle {mItemMark}):itemHs]
		| mItemMark
			= (mItemMark,index,[itemH:itemHs])
		| otherwise
			# (found,index,itemHs)	= getNewRadioMenuIndex (index+1) itemHs
			= (found,index,[itemH:itemHs])
	getNewRadioMenuIndex index []
		= (False,index,[])
	
	checkFirstRadioItem :: !OSMenu !Int !*[MenuElementHandle .ls .ps] !*OSToolbox -> (!*[MenuElementHandle .ls .ps],!*OSToolbox)
	checkFirstRadioItem menu iNr [MenuItemHandle itemH=:{mOSMenuItem}:itemHs] tb
		= ([MenuItemHandle {itemH & mItemMark=True}:itemHs],osMenuItemCheck True menu mOSMenuItem iNr iNr tb)
	checkFirstRadioItem _ _ [] tb
		= ([],tb)
