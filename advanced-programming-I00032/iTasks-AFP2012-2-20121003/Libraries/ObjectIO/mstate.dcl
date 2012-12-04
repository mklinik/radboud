definition module mstate


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	menuhandle


/*	The MenuHandle` data type.
	This type is a subtype of the MenuHandle data type. The MenuHandle` data type 
	takes the projection of those fields of the (MenuHandle ls ps) data type that 
	do not depend on the type variables {ls,ps}.
*/

::	MenuHandle`
	=	{	mHandle`		:: !OSMenu					// The handle to the menu as created by the OS
		,	mMenuId`		:: !Id						// The menu id
		,	mOSMenuNr`		:: !OSMenuNr				// The OSMenuNr
		,	mTitle`			:: !String					// The title of the menu
		,	mSelect`		:: !Bool					// The MenuSelect==Able (by default True)
		,	mItems`			:: ![MenuElementHandle`]	// The menu elements of this menu
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


getMenuHandle` ::				!(MenuHandle .ls .ps) -> (!MenuHandle`,!MenuHandle .ls .ps)
setMenuHandle` :: !MenuHandle`	!(MenuHandle .ls .ps) ->				MenuHandle .ls .ps
