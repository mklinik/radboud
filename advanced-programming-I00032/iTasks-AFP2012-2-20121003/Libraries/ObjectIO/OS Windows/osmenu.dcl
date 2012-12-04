definition module osmenu


//	Clean Object I/O library, version 1.2


from	StdMaybe			import :: Maybe
from	StdIOCommon			import :: Modifiers
from	menuCrossCall_12	import :: HMENU, :: HITEM
import	osdocumentinterface, ostoolbox, ostypes


//	Types for menus and menu elements:
::	OSMenu			:== HMENU
::	OSMenuItem		:== HITEM
::	OSMenuSeparator	:== HITEM

//	Dummy values:
OSNoMenu			:== 0
OSNoMenuItem		:== 0
OSNoMenuSeparator	:== 0


/*	Initialisation:
*/
osInitialiseMenus	:: !*OSToolbox -> *OSToolbox


/*	Enabling and disabling of menus and menu elements:
	os(Dis/En)ableMenu index menubar
		(dis/en)ables the top-level menu at the zero based index position of the menubar.
	os(Dis/En)ableMenuItem parentMenu menuitem index
		(dis/en)ables the menuitem that is part of the parentMenu.
	os(Dis/En)ableSubMenu parentMenu submenu index
		(dis/en)ables the submenu that is part of the parentMenu.
*/
osDisableMenu		:: !Int !OSMenuBar			!*OSToolbox -> *OSToolbox
osEnableMenu		:: !Int !OSMenuBar			!*OSToolbox -> *OSToolbox
osDisableMenuItem	:: !OSMenu !OSMenuItem !Int	!*OSToolbox -> *OSToolbox
osEnableMenuItem	:: !OSMenu !OSMenuItem !Int	!*OSToolbox -> *OSToolbox
osDisableSubMenu	:: !OSMenu !OSMenuItem !Int !*OSToolbox -> *OSToolbox
osEnableSubMenu		:: !OSMenu !OSMenuItem !Int !*OSToolbox -> *OSToolbox


/*	Changing and updating the menubar:
	osDrawMenuBar
		redraws the menubar. This must be done after every change of the menubar (adding/removing).
	osMenuBarClear
		clears the menubar.
	osMenuBarSet
		dunno??
	osMenuInsert index menuNr title menubar 
		creates and inserts a new top-level menu at the indicated zero based index position.
		The new menu has the given title and the menuNr as retrieved by OSNewMenuNr (below).
	osSubMenuInsert index menuNr title parentMenu 
		creates and inserts a new submenu at the indicated zero based index position.
		The new submenu has the given title and the menuNr as retrieved by OSNewSubMenuNr (below).
	osMenuRemove menu menubar
		removes the indicated menu both 'logically' and 'physically' from the menubar.
	osSubMenuRemove submenu parentMenu ... submenuid submenuindex
		removes the submenu both 'logically' and 'physically' from the parentMenu.
	osRemoveMenuShortKey framePtr item
		removes the shortcut key of the item.
*/
osDrawMenuBar		:: !OSMenuBar							!*OSToolbox -> *OSToolbox
osMenuBarClear		::										!*OSToolbox -> *OSToolbox
osMenuBarSet		:: !OSMenuBar							!*OSToolbox -> (!OSMenuBar, !*OSToolbox)
osMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenuBar	!*OSToolbox -> (!OSMenu,!OSMenuBar,	!*OSToolbox)
osSubMenuInsert		:: !Int !OSMenuNr !{#Char} !OSMenu		!*OSToolbox -> (!OSMenu,!OSMenu,	!*OSToolbox)
osMenuRemove		:: !OSMenu !OSMenuBar					!*OSToolbox -> (!OSMenuBar,			!*OSToolbox)
osSubMenuRemove		:: !OSMenu !OSMenu !Int	!Int			!*OSToolbox -> (!OSMenu,			!*OSToolbox)
osRemoveMenuShortKey:: !OSWindowPtr !OSMenuItem				!*OSToolbox -> *OSToolbox


/*	PopUpMenu functions:
	osCreatePopUpMenu creates a pop up menu.
	osTrackPopUpMenu  shows the pop up menu and handles user selection: 
		the Int       result is the menu item that has been selected (0 if none); 
		the Modifiers result are the modifiers that have been pressed at selection.
*/
::	OSTrackPopUpMenu									// The result of tracking an item in a PopUpMenu:
	=	{	ospupItem		:: !OSTrackPopUpMenuResult	//	the item that has been selected
		,	ospupModifiers	:: !Modifiers				//	the modifiers that have been pressed at selection
		}
::	OSTrackPopUpMenuResult								// The item of a pop up menu that has been selected is indicated by:
	=	PopUpTrackedByIndex	 !Int !Int					//	the parent menu id and the item's index position (used on Mac)
	|	PopUpTrackedByItemId !Int						//	its identification                               (used on Windows)

osCreatePopUpMenu	:: !*OSToolbox -> (!OSMenu,!*OSToolbox)
osTrackPopUpMenu	:: !OSMenu !OSWindowPtr !*OSToolbox -> (!Maybe OSTrackPopUpMenu,!*OSToolbox)


/*	Changing (sub)menus and menu elements:
	osAppendMenuItem osmenubar index menu title able mark key
		adds a new menuitem to the given menu at the indicated zero based index position.
		The menuitem has the given title, selectstate, markstate, and shortcut key.
		The menu is element of the given osmenubar. 
	osAppendMenuSeparator index menu
		adds a new menuseparator to the given menu at the indicated zero based index position.
	osChangeMenuTitle menubar menu title
		sets the new title of the indicated top-level menu in the menubar.
	osChangeMenuItemTitle parentMenu menuitem index title
		sets the new title of the indicated menuitem/submenu contained in the parentMenu.
	osMenuItemCheck check parentMenu menuitem prevIndex newIndex
		marks the item iff check of the indicated menuitem contained in the parentMenu.
	osMenuRemoveItem menuitem index parentMenu
		removes the menuitem 'logically' from the indicated parentMenu. The menuitem is not destroyed. CHECK APPLICATIONS!!
*/
osAppendMenuItem		:: !OSMenuBar !Int !OSMenu !{#Char} !Bool !Bool !Char	!*OSToolbox -> (!OSMenuItem,	 !OSMenu,!*OSToolbox)
osAppendMenuSeparator	:: !Int       !OSMenu									!*OSToolbox -> (!OSMenuSeparator,!OSMenu,!*OSToolbox)
osChangeMenuTitle		:: !OSMenuBar !OSMenu                  !{#Char}			!*OSToolbox -> *OSToolbox
osChangeMenuItemTitle	::            !OSMenu !OSMenuItem !Int !{#Char}			!*OSToolbox -> *OSToolbox
osMenuItemCheck			:: !Bool      !OSMenu !OSMenuItem !Int !Int				!*OSToolbox -> *OSToolbox
osMenuRemoveItem		:: !OSMenuItem !Int !OSMenu								!*OSToolbox -> (!OSMenu,!*OSToolbox)


/*	Validation of (sub)menu (element) attributes:
*/
osValidateMenuItemTitle :: !(Maybe Char) !{#Char} -> {#Char}


/*	Two functions that generate free OS ids for menus and sub menus.
	If the functions fail, then the Bool result is False, and the Int result is 0. 
	Do not continue to create the (sub)menu.
*/
::	OSMenuNr	:== Int
::	OSSubMenuNr	:== Int

osNewMenuNr		:: !*OSToolbox -> (!Bool,!OSMenuNr,   !*OSToolbox)
osNewSubMenuNr	:: !*OSToolbox -> (!Bool,!OSSubMenuNr,!*OSToolbox)
