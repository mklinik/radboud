implementation module mstate


import	StdBool, StdClass, StdInt
import	menuhandle
from commondef import fatalError


mstateFatalError :: String String -> .x
mstateFatalError rule error
	= fatalError rule "mstate" error


/*	The MenuHandle` data type.
	This type is a subtype of the MenuHandle data type. The MenuHandle` data type 
	takes the projection of those fields of the (MenuHandle ls ps) data type that 
	do not depend on the type variables {ls,ps}.
*/

::	MenuHandle`
	=	{	mHandle`	:: !OSMenu						// The handle to the menu as created by the OS
		,	mMenuId`	:: !Id							// The menu id
		,	mOSMenuNr`	:: !OSMenuNr					// The OSMenuNr
		,	mTitle`		:: !String						// The title of the menu
		,	mSelect`	:: !Bool						// The MenuSelect==Able (by default True)
		,	mItems`		:: ![MenuElementHandle`]		// The menu elements of this menu
		}
::	MenuElementHandle`
	=	MenuItemHandle`			!MenuItemHandle`
	|	MenuReceiverHandle`		!MenuReceiverHandle`
	|	SubMenuHandle`			!SubMenuHandle`
	|	RadioMenuHandle`		!RadioMenuHandle`
	|	MenuSeparatorHandle`	!MenuSeparatorHandle`
	|	MenuRecursiveHandle`	![MenuElementHandle`] !MenuRecursiveKind
::	MenuRecursiveKind
	=	IsMenuListLSHandle
	|	IsMenuExtendLSHandle
	|	IsMenuChangeLSHandle
::	MenuItemHandle`
	=	{	mItemId`		:: !Maybe Id
		,	mItemKey`		:: !Maybe Char
		,	mItemTitle`		:: !Title
		,	mItemSelect`	:: !Bool
		,	mItemMark`		:: !Bool
		,	mItemAtts`		:: ![MenuAttribute`]
		,	mOSMenuItem`	:: !OSMenuItem
		}
::	MenuReceiverHandle`
	=	{	mReceiverId`	:: !Id
		,	mReceiverSelect`:: !Bool
		}
::	SubMenuHandle`
	=	{	mSubHandle`		:: !OSMenu
		,	mSubMenuId`		:: !Maybe Id
		,	mSubOSMenuNr`	:: !OSSubMenuNr
		,	mSubItems`		:: ![MenuElementHandle`]
		,	mSubTitle`		:: !Title
		,	mSubSelect`		:: !Bool
		,	mSubAtts`		:: ![MenuAttribute`]
		}
::	RadioMenuHandle`
	=	{	mRadioId`		:: !Maybe Id
		,	mRadioIndex`	:: !Int						// If mRadioItems==[] 0, otherwise 1..#mRadioItems
		,	mRadioItems`	:: ![MenuElementHandle`]
		,	mRadioSelect`	:: !Bool
		,	mRadioAtts`		:: ![MenuAttribute`]
		}
::	MenuSeparatorHandle`
	=	{	mSepId`			:: !Maybe Id
		}
::	MenuAttribute`										//	Default:
	=	MenuId`				Id							//	no Id
	|	MenuSelectState`	SelectState					//	menu(item) Able
	|	MenuShortKey`		Char						//	no ShortKey
	|	MenuMarkState`		MarkState					//	NoMark


getMenuHandle` :: !(MenuHandle .ls .ps) -> (!MenuHandle`,!MenuHandle .ls .ps)
getMenuHandle` mH=:{mHandle,mMenuId,mOSMenuNr,mTitle,mSelect,mItems=items}
	# (items`,items)	= getMenuElementHandles items
	= (	{mHandle`=mHandle,mMenuId`=mMenuId,mOSMenuNr`=mOSMenuNr,mTitle`=mTitle,mSelect`=mSelect,mItems`=items`}
	  ,	{mH & mItems=items}
	  )
where
	getMenuElementHandles :: ![MenuElementHandle .ls .ps] -> (![MenuElementHandle`],![MenuElementHandle .ls .ps])
	getMenuElementHandles [itemH:itemHs]
		#! (itemH`, itemH)	= getMenuElementHandle  itemH
		#! (itemHs`,itemHs)	= getMenuElementHandles itemHs
		=  ([itemH`:itemHs`],[itemH:itemHs])
	where
		getMenuElementHandle :: !(MenuElementHandle .ls .ps) -> (!MenuElementHandle`,!MenuElementHandle .ls .ps)
		getMenuElementHandle (MenuItemHandle itemH=:{mItemId,mItemKey,mItemTitle,mItemSelect,mItemMark,mItemAtts=atts,mOSMenuItem})
			# (atts`,atts)		= getMenuAttributes atts
			= ( MenuItemHandle`
				{	mItemId`	= mItemId
				,	mItemKey`	= mItemKey
				,	mItemTitle`	= mItemTitle
				,	mItemSelect`= mItemSelect
				,	mItemMark`	= mItemMark
				,	mItemAtts`	= atts`
				,	mOSMenuItem`= mOSMenuItem
				}
			  , MenuItemHandle 
				{itemH & mItemAtts=atts}
			  )
		getMenuElementHandle (MenuReceiverHandle recH=:{mReceiverHandle={rId,rSelect}})
			= ( MenuReceiverHandle`
					{	mReceiverId`	= rId
					,	mReceiverSelect`= rSelect==Able
					}
			  , MenuReceiverHandle recH
			  )
		getMenuElementHandle (SubMenuHandle subH=:{mSubHandle,mSubMenuId,mSubOSMenuNr,mSubItems=items,mSubTitle,mSubSelect,mSubAtts=atts})
			# (items`,items)		= getMenuElementHandles items
			  (atts`, atts)			= getMenuAttributes atts
			= ( SubMenuHandle`
				{	mSubHandle`		= mSubHandle
				,	mSubMenuId`		= mSubMenuId
				,	mSubOSMenuNr`	= mSubOSMenuNr
				,	mSubItems`		= items`
				,	mSubTitle`		= mSubTitle
				,	mSubSelect`		= mSubSelect
				,	mSubAtts`		= atts`
				}
			  , SubMenuHandle
				{subH & mSubItems=items,mSubAtts=atts}
			  )
		getMenuElementHandle (RadioMenuHandle radioH=:{mRadioId,mRadioIndex,mRadioItems=items,mRadioSelect,mRadioAtts=atts})
			# (items`,items)		= getMenuElementHandles items
			  (atts`, atts)			= getMenuAttributes atts
			= ( RadioMenuHandle`
				{	mRadioId`		= mRadioId
				,	mRadioIndex`	= mRadioIndex
				,	mRadioItems`	= items`
				,	mRadioSelect`	= mRadioSelect
				,	mRadioAtts`		= atts`
				}
			  ,	RadioMenuHandle
				{radioH & mRadioItems=items,mRadioAtts=atts}
			  )
		getMenuElementHandle (MenuSeparatorHandle sepH=:{mSepId})
			= (MenuSeparatorHandle` {mSepId`=mSepId},MenuSeparatorHandle sepH)
		getMenuElementHandle (MenuListLSHandle items)
			# (items`,items)		= getMenuElementHandles items
			= (MenuRecursiveHandle` items` IsMenuListLSHandle,MenuListLSHandle items)
		getMenuElementHandle (MenuExtendLSHandle exH=:{mExtendItems=items})
			# (items`,items)		= getMenuElementHandles items
			= (MenuRecursiveHandle` items` IsMenuExtendLSHandle,MenuExtendLSHandle {exH & mExtendItems=items})
		getMenuElementHandle (MenuChangeLSHandle chH=:{mChangeItems=items})
			# (items`,items)		= getMenuElementHandles items
			= (MenuRecursiveHandle` items` IsMenuChangeLSHandle,MenuChangeLSHandle {chH & mChangeItems=items})
		
		getMenuAttributes :: ![MenuAttribute .ps] -> (![MenuAttribute`],![MenuAttribute .ps])
		getMenuAttributes [att:atts]
			#! (ok,att`,att)	= getMenuAttribute  att
			#! (atts`, atts)	= getMenuAttributes atts
			| ok				= ([att`:atts`],[att:atts])
			| otherwise			= (      atts`, [att:atts])
		where
			getMenuAttribute :: !(MenuAttribute .ps) -> (!Bool,!MenuAttribute`,!MenuAttribute .ps)
			getMenuAttribute att=:(MenuId			id)		= (True,MenuId`			 id,	 att)
			getMenuAttribute att=:(MenuSelectState	select)	= (True,MenuSelectState` select, att)
			getMenuAttribute att=:(MenuShortKey		keycode)= (True,MenuShortKey`	 keycode,att)
			getMenuAttribute att=:(MenuMarkState	mark)	= (True,MenuMarkState`	 mark,	 att)
			getMenuAttribute att							= (False,MenuShortKey`	 '0',	 att)
		getMenuAttributes _
			= ([],[])
	getMenuElementHandles _
		= ([],[])


setMenuHandle` :: !MenuHandle` !(MenuHandle .ls .ps) -> MenuHandle .ls .ps
setMenuHandle` mH`=:{mHandle`,mTitle`,mSelect`,mItems`} mH=:{mHandle,mItems}
	| mHandle`<>mHandle	= mstateFatalError "setMenuHandle`" "mHandle` field <> mHandle field"
	| otherwise			= {mH & mItems=setMenuElementHandles mItems` mItems}
where
	setMenuElementHandles :: ![MenuElementHandle`] ![MenuElementHandle .ls .ps] -> [MenuElementHandle .ls .ps]
	setMenuElementHandles [itemH`:itemHs`] [itemH:itemHs]
		#! itemH		= setElementHandle itemH` itemH
		#! itemHs		= setMenuElementHandles itemHs` itemHs
		= [itemH:itemHs]
	where
		setElementHandle :: !MenuElementHandle` !(MenuElementHandle .ls .ps) -> MenuElementHandle .ls .ps
		
		setElementHandle (MenuItemHandle` itemH`=:{mItemAtts`,mItemTitle`,mItemSelect`,mItemMark`})
						 (MenuItemHandle  itemH =:{mItemAtts})
			# atts	= setMenuAttributes mItemAtts` mItemAtts
			= MenuItemHandle {itemH & mItemAtts=atts,mItemTitle=mItemTitle`,mItemSelect=mItemSelect`,mItemMark=mItemMark`}
		
		setElementHandle (MenuReceiverHandle` recH`=:{mReceiverSelect`})
						 (MenuReceiverHandle  recH =:{mReceiverHandle=rH})
			= MenuReceiverHandle {recH & mReceiverHandle={rH & rSelect=if mReceiverSelect` Able Unable}}
		
		setElementHandle (SubMenuHandle` subH`=:{mSubItems`,mSubAtts`,mSubTitle`,mSubSelect`})
						 (SubMenuHandle  subH =:{mSubItems, mSubAtts})
			# atts	= setMenuAttributes mSubAtts` mSubAtts
			  items	= setMenuElementHandles mSubItems` mSubItems
			= SubMenuHandle {subH & mSubItems=items,mSubAtts=atts,mSubTitle=mSubTitle`,mSubSelect=mSubSelect`}
		
		setElementHandle (RadioMenuHandle` radioH`=:{mRadioItems`,mRadioAtts`,mRadioSelect`,mRadioIndex`})
						 (RadioMenuHandle  radioH =:{mRadioItems, mRadioAtts})
			# atts	= setMenuAttributes mRadioAtts` mRadioAtts
			  items	= setMenuElementHandles mRadioItems` mRadioItems
			= RadioMenuHandle {radioH & mRadioIndex=mRadioIndex`,mRadioItems=items,mRadioSelect=mRadioSelect`,mRadioAtts=atts}
		
		setElementHandle (MenuSeparatorHandle` _) sepH=:(MenuSeparatorHandle _)
			= sepH
		
		setElementHandle (MenuRecursiveHandle` itemHs` IsMenuListLSHandle) (MenuListLSHandle itemHs)
			= MenuListLSHandle (setMenuElementHandles itemHs` itemHs)
		
		setElementHandle (MenuRecursiveHandle` itemHs` IsMenuExtendLSHandle) (MenuExtendLSHandle exH=:{mExtendItems})
			= MenuExtendLSHandle {exH & mExtendItems=setMenuElementHandles itemHs` mExtendItems}
		
		setElementHandle (MenuRecursiveHandle` itemHs` IsMenuChangeLSHandle) (MenuChangeLSHandle chH=:{mChangeItems})
			= MenuChangeLSHandle {chH & mChangeItems=setMenuElementHandles itemHs` mChangeItems}
		
		setElementHandle _ _
			= mstateFatalError "setMenuHandle`" "MenuElementHandles do not match pairwise"
	setMenuElementHandles [] []
		= []
	setMenuElementHandles _ _
		= mstateFatalError "setMenuHandle`" "incompatible number of MenuElementHandles"
	
	setMenuAttributes :: ![MenuAttribute`] ![MenuAttribute .ps] -> [MenuAttribute .ps]
	setMenuAttributes [att`:atts`] atts
		| isConstantAttribute att`		= setMenuAttributes atts` atts
		| otherwise						= setMenuAttributes atts` (setAttribute att` atts)
	where
		isConstantAttribute :: !MenuAttribute` -> Bool
		isConstantAttribute (MenuId` _)	= True
		isConstantAttribute _			= False
		
		setAttribute :: !MenuAttribute` ![MenuAttribute .ps] -> [MenuAttribute .ps]
		setAttribute att` [att:atts]
			| match att` att	= [att1:atts]
			| otherwise
				#! atts			= setAttribute att` atts
				=  [att:atts]
		where
			att1	= case att` of
						(MenuSelectState`	select)	-> MenuSelectState	select
						(MenuShortKey`		keycode)-> MenuShortKey		keycode
						(MenuMarkState`		mark)	-> MenuMarkState	mark
						_							-> mstateFatalError "setMenuHandle" "illegal MenuAttribute` encountered"
			
			match (MenuSelectState` _) (MenuSelectState _)	= True
			match (MenuShortKey`	_) (MenuShortKey	_)	= True
			match (MenuMarkState`	_) (MenuMarkState	_)	= True
			match _						_					= False
		setAttribute att` _
			= mstateFatalError "setMenuHandle" "non-matching MenuAttribute encountered"
	setMenuAttributes _ atts
		= atts
