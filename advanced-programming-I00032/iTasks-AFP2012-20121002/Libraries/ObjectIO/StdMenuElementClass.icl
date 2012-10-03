implementation module StdMenuElementClass


import	StdBool, StdList, StdMisc, StdTuple
import	StdMenuDef, StdPSt
import	commondef, menuaccess, menudefaccess
import	osmenu


class MenuElements m where
	menuElementToHandles :: !.(m .ls (PSt .l)) !(PSt .l) -> (![MenuElementState .ls (PSt .l)],!PSt .l)
	getMenuElementType   ::  .(m .ls .pst)               -> MenuElementType


/*	Translating menu elements into the internal representation.
	Fields which values can not be determined now are filled with dummy values.
	These are the following:
	-	SubMenuHandle
		-	mSubHandle			(the handle to the sub menu)
		-	mSubOSMenuNr		(the internal system id of the sub menu)
	-	MenuItemHandle
		-	mOSMenuItem			(the handle to the item)
	-	MenuSeparatorHandle
		-	mOSMenuSeparator	(the handle to the item)
	The remaining attributes are copied.
*/
instance MenuElements (AddLS m)	| MenuElements m where
	menuElementToHandles {addLS,addDef} pState
		# (ms,pState)	= menuElementToHandles addDef pState
		= (	[menuElementHandleToMenuElementState 
				(MenuExtendLSHandle {	mExtendLS	= addLS
									,	mExtendItems= map menuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements (NewLS m)	| MenuElements m where
	menuElementToHandles {newLS,newDef} pState
		# (ms,pState)	= menuElementToHandles newDef pState
		= (	[menuElementHandleToMenuElementState
				(MenuChangeLSHandle {	mChangeLS	= newLS
									,	mChangeItems= map menuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements (ListLS m)	| MenuElements m where
	menuElementToHandles (ListLS mDefs) pState
		# (mss,pState)	= stateMap menuElementToHandles mDefs pState
		= (	[menuElementHandleToMenuElementState
			  (MenuListLSHandle (map menuElementStateToMenuElementHandle (flatten mss)))
			]
		  ,	pState
		  )
	getMenuElementType _
		= ""

instance MenuElements NilLS where
	menuElementToHandles NilLS pState
		= ([menuElementHandleToMenuElementState (MenuListLSHandle [])],pState)
	getMenuElementType _
		= ""

instance MenuElements ((:+:) m1 m2)	| MenuElements m1 & MenuElements m2 where
	menuElementToHandles (m1:+:m2) pState
		# (ms1,pState)	= menuElementToHandles m1 pState
		# (ms2,pState)	= menuElementToHandles m2 pState
		= (ms1 ++ ms2,pState)
	getMenuElementType _
		= ""

instance MenuElements (SubMenu m)	| MenuElements m where
	menuElementToHandles (SubMenu title items atts) pState
		# (ms,pState)		= menuElementToHandles items pState
		  (selectAtt,atts)	= validateSelectState atts
		  (idAtt,    atts)	= validateId          atts
		= (	[menuElementHandleToMenuElementState
			  (SubMenuHandle {	mSubHandle	= OSNoMenu
							 ,	mSubMenuId	= idAtt
							 ,	mSubOSMenuNr= 0
							 ,	mSubItems	= map menuElementStateToMenuElementHandle ms
							 ,	mSubTitle	= title
							 ,	mSubSelect	= enabled selectAtt
							 ,	mSubAtts	= atts
							 }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "SubMenu"

instance MenuElements RadioMenu where
	menuElementToHandles (RadioMenu items index atts) pState
		# nrRadios			= length items
		  validIndex		= if (nrRadios==0) 0 (setBetween index 1 nrRadios)
		  itemHs			= validateRadioMenuIndex validIndex (map radioMenuItemToMenuElementHandle items)
		  (selectAtt,atts)	= validateSelectState atts
		  (idAtt,    atts)	= validateId          atts
		= (	[menuElementHandleToMenuElementState
			  (RadioMenuHandle {	mRadioId	= idAtt
							   ,	mRadioIndex	= validIndex
							   ,	mRadioItems	= itemHs
							   ,	mRadioSelect= enabled selectAtt
							   ,	mRadioAtts	= atts
							   }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "RadioMenu"

instance MenuElements MenuItem where
	menuElementToHandles (MenuItem title atts) pState
		# (selectAtt,atts)	= validateSelectState atts
		  (markAtt,  atts)	= validateMarkState   atts
		  (keyAtt,   atts)	= validateShortKey    atts
		  (idAtt,    atts)	= validateId          atts
		= (	[menuElementHandleToMenuElementState
			  (MenuItemHandle {	mItemId		= idAtt
							  ,	mItemKey	= keyAtt
							  ,	mItemTitle	= title
							  ,	mItemSelect	= enabled selectAtt
							  ,	mItemMark	= marked  markAtt
							  ,	mItemAtts	= atts
							  ,	mOSMenuItem	= OSNoMenuItem
							  }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "MenuItem"

instance MenuElements MenuSeparator where
	menuElementToHandles (MenuSeparator atts) pState
		# (idAtt,_)		= validateId atts
		= (	[menuElementHandleToMenuElementState 
			  (MenuSeparatorHandle { mSepId			 = idAtt
								   , mOSMenuSeparator= OSNoMenuSeparator
								   }
			  )
			]
		  ,	pState
		  )
	getMenuElementType _
		= "MenuSeparator"


//	Obtain the SelectState attribute from the attribute list:
validateSelectState :: ![MenuAttribute .ps] -> (!SelectState,![MenuAttribute .ps])
validateSelectState atts
	# (found,selectAtt,atts)= remove isMenuSelectState undef atts
	| found					= (getMenuSelectStateAtt selectAtt,atts)
	| otherwise				= (Able,atts)

//	Obtain the MarkState attribute from the attribute list:
validateMarkState :: ![MenuAttribute .ps] -> (!MarkState,![MenuAttribute .ps])
validateMarkState atts
	# (found,markAtt,atts)	= remove isMenuMarkState undef atts
	| found					= (getMenuMarkStateAtt markAtt,atts)
	| otherwise				= (NoMark,atts)

//	Obtain the Id attribute from the attribute list:
validateId :: ![MenuAttribute .ps] -> (!Maybe Id,![MenuAttribute .ps])
validateId atts
	# (found,idAtt,atts)	= remove isMenuId undef atts
	| found					= (Just (getMenuIdAtt idAtt),atts)
	| otherwise				= (Nothing,atts)

//	Obtain the ShortKey attribute from the attribute list:
validateShortKey :: ![MenuAttribute .ps] -> (!Maybe Char,![MenuAttribute .ps])
validateShortKey atts
	# (hasKey,keyAtt,atts)	= remove isMenuShortKey undef atts
	| hasKey				= (Just (getMenuShortKeyAtt keyAtt),atts)
	| otherwise				= (Nothing,atts)

//	validateRadioMenuIndex ensures that only the element at the valid index position of the RadioMenu
//	has a check mark and all others don't.
validateRadioMenuIndex :: !Int ![MenuElementHandle .ls .ps] -> [MenuElementHandle .ls .ps]
validateRadioMenuIndex index itemHs
	= fst (stateMap (\(MenuItemHandle itemH) i->(MenuItemHandle {itemH & mItemMark=i==index},i+1)) itemHs 1)


/*	Menu elements for PopUpMenus:
*/
class PopUpMenuElements m where
	popUpMenuElementToHandles	:: !.(m .ls (PSt .l)) !(PSt .l)
				 -> (![MenuElementState .ls (PSt .l)], !PSt .l)
	getPopUpMenuElementType		::  .(m .ls .pst)
				 -> MenuElementType

instance PopUpMenuElements (AddLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles {addLS,addDef} pState
		# (ms,pState)	= popUpMenuElementToHandles addDef pState
		= (	[menuElementHandleToMenuElementState 
				(MenuExtendLSHandle {	mExtendLS	= addLS
									,	mExtendItems= map menuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements (NewLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles {newLS,newDef} pState
		# (ms,pState)	= popUpMenuElementToHandles newDef pState
		= (	[menuElementHandleToMenuElementState
				(MenuChangeLSHandle {	mChangeLS	= newLS
									,	mChangeItems= map menuElementStateToMenuElementHandle ms
									}
				)
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements (ListLS m) | PopUpMenuElements m where
	popUpMenuElementToHandles (ListLS mDefs) pState
		# (mss,pState)	= stateMap popUpMenuElementToHandles mDefs pState
		= (	[menuElementHandleToMenuElementState
			  (MenuListLSHandle (map menuElementStateToMenuElementHandle (flatten mss)))
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements NilLS where
	popUpMenuElementToHandles NilLS pState
		= ([menuElementHandleToMenuElementState (MenuListLSHandle [])],pState)
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements ((:+:) m1 m2) | PopUpMenuElements m1 & PopUpMenuElements m2 where
	popUpMenuElementToHandles (m1:+:m2) pState
		# (ms1,pState)	= popUpMenuElementToHandles m1 pState
		# (ms2,pState)	= popUpMenuElementToHandles m2 pState
		= (ms1 ++ ms2,pState)
	getPopUpMenuElementType _
		= ""

instance PopUpMenuElements RadioMenu where
	popUpMenuElementToHandles  radioMenu pState
		= menuElementToHandles radioMenu pState
	getPopUpMenuElementType    radioMenu
		= getMenuElementType   radioMenu

instance PopUpMenuElements MenuItem where
	popUpMenuElementToHandles  menuItem pState
		= menuElementToHandles menuItem pState
	getPopUpMenuElementType    menuItem
		= getMenuElementType   menuItem

instance PopUpMenuElements MenuSeparator where
	popUpMenuElementToHandles  menuSeparator pState
		= menuElementToHandles menuSeparator pState
	getPopUpMenuElementType    menuSeparator
		= getMenuElementType   menuSeparator

//-- DvA --

instance PopUpMenuElements (SubMenu	m) | PopUpMenuElements m where
	popUpMenuElementToHandles (SubMenu title items atts) pState
		# (ms,pState)		= popUpMenuElementToHandles items pState
		  (selectAtt,atts)	= validateSelectState atts
		  (idAtt,    atts)	= validateId          atts
		= (	[menuElementHandleToMenuElementState
			  (SubMenuHandle {	mSubHandle	= OSNoMenu
							 ,	mSubMenuId	= idAtt
							 ,	mSubOSMenuNr= 0
							 ,	mSubItems	= map menuElementStateToMenuElementHandle ms
							 ,	mSubTitle	= title
							 ,	mSubSelect	= enabled selectAtt
							 ,	mSubAtts	= atts
							 }
			  )
			]
		  ,	pState
		  )
	getPopUpMenuElementType _
		= "SubMenu"
