implementation module menuhandle


import StdMenuDef
import commondef, receiverhandle
import osmenu


::	*MenuElementState		ls pst							// The internal implementation of a menu element
	:==	MenuElementHandle	ls pst							// is a MenuElementHandle

::	*MenuHandles pst
	=	{	mMenus			:: !*[*MenuStateHandle pst]		// The menus and their elements of a process
		,	mKeys			:: ![Char]						// All shortcut keys of the menus
//PA---	,	mOSMenuBar		:: !OSMenuBar					// The handle to the toolbox menu bar
		,	mEnabled		:: !Bool						// Flag: the whole menusystem is enabled
		,	mNrMenuBound	:: !Bound						// The maximum number of menus that are allowed to be opened
		,	mPopUpId		:: !Maybe Id					// The Id of the PopUpMenu (Nothing if open; (Just id) if available)
		}
::	*MenuStateHandle pst
	=	E. .ls: MenuLSHandle !(MenuLSHandle ls pst)			// A menu with local state
::	*MenuLSHandle ls pst
	=	{	mlsState		:: ls							// The local state of this menu
		,	mlsHandle		:: !MenuHandle ls pst			// The menu implementation
		}
::	*MenuHandle ls pst
	=	{	mHandle			:: !OSMenu						// The handle to the menu as created by the OS
		,	mMenuId			:: !Id							// The menu id
		,	mOSMenuNr		:: !OSMenuNr					// The OSMenuNr
		,	mTitle			:: !String						// The title of the menu
		,	mSelect			:: !Bool						// The MenuSelect==Able (by default True)
		,	mItems			:: !*[MenuElementHandle ls pst]	// The menu elements of this menu
		}
::	*MenuElementHandle ls pst
	=	MenuItemHandle		!*(MenuItemHandle      ls pst)
	|	MenuReceiverHandle	!*(MenuReceiverHandle  ls pst)
	|	SubMenuHandle		!*(SubMenuHandle       ls pst)
	|	RadioMenuHandle		!*(RadioMenuHandle     ls pst)
	|	MenuSeparatorHandle	!*(MenuSeparatorHandle ls pst)
	|	MenuListLSHandle	!*[MenuElementHandle   ls pst]
	|	MenuExtendLSHandle	!*(MenuExtendLSHandle  ls pst)
	|	MenuChangeLSHandle	!*(MenuChangeLSHandle  ls pst)
::	*MenuItemHandle ls pst
	=	{	mItemId			:: !Maybe Id
		,	mItemKey		:: !Maybe Char
		,	mItemTitle		:: !Title
		,	mItemSelect		:: !Bool
		,	mItemMark		:: !Bool
		,	mItemAtts		:: ![MenuAttribute *(ls,pst)]
		,	mOSMenuItem		:: !OSMenuItem
		}
::	*MenuReceiverHandle ls pst
	=	{	mReceiverHandle	:: !ReceiverHandle ls pst
		,	mReceiverAtts	:: ![MenuAttribute *(ls,pst)]
		}
::	*SubMenuHandle ls pst
	=	{	mSubHandle		:: !OSMenu
		,	mSubMenuId		:: !Maybe Id
		,	mSubOSMenuNr	:: !OSSubMenuNr
		,	mSubItems		:: !*[*MenuElementHandle ls pst]
		,	mSubTitle		:: !Title
		,	mSubSelect		:: !Bool
		,	mSubAtts		:: ![MenuAttribute *(ls,pst)]
		}
::	*RadioMenuHandle ls pst
	=	{	mRadioId		:: !Maybe Id
		,	mRadioIndex		:: !Int
		,	mRadioItems		:: !*[*MenuElementHandle ls pst]
		,	mRadioSelect	:: !Bool
		,	mRadioAtts		:: ![MenuAttribute *(ls,pst)]
		}
::	*MenuSeparatorHandle ls pst
	=	{	mSepId			:: !Maybe Id
		,	mOSMenuSeparator:: !OSMenuSeparator
		}
::	*MenuExtendLSHandle ls pst
	=	E. .ls1:
		{	mExtendLS		:: ls1
		,	mExtendItems	:: !*[*MenuElementHandle *(ls1,ls) pst]
		}
::	*MenuChangeLSHandle ls pst
	=	E. .ls1:
		{	mChangeLS		:: ls1
		,	mChangeItems	:: !*[*MenuElementHandle ls1 pst]
		}


//	Conversion functions from MenuElementState to MenuElementHandle, and vice versa:
menuElementHandleToMenuElementState	:: !*(MenuElementHandle .ls .pst) -> *MenuElementState  .ls .pst
menuElementHandleToMenuElementState mH = mH

menuElementStateToMenuElementHandle	:: !*(MenuElementState  .ls .pst) -> *MenuElementHandle .ls .pst
menuElementStateToMenuElementHandle mH = mH
