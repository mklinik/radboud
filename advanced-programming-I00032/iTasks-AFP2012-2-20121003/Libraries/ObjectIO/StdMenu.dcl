definition module StdMenu


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdMenu defines functions on menus.
//	********************************************************************************


import	StdMenuElementClass
from	iostate import :: IOSt, :: PSt


//	Operations on unknown Ids are ignored.

class Menus mdef where
	openMenu	:: .ls !.(mdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l)
	getMenuType	::      .(mdef .ls .pst)               -> MenuType
/*	Open the given menu definition for this interactive process. 
	openMenu may not be permitted to open a menu depending on its DocumentInterface 
		(see the comments at the shareProcesses instances in module StdProcess).
	In case a menu with the same Id is already open then nothing happens. In case 
		the menu has the WindowMenuId Id then nothing happens. In case the menu does
		not have an Id, it will obtain an Id which is fresh with respect to the 
		current set of menus. The Id can be reused after closing this menu. In case 
		menu elements are opened with duplicate Ids, the menu will not be opened.
	In case the menu definition does not have a MenuIndex attribute (see StdMenuDef)
		it will be opened behind the last menu. In case the menu definition has a 
		MenuIndex attribute it will be placed behind the menu indicated by the 
		integer index. 
		The index of a menu starts from one for the first present menu. If the index
		is negative or zero, then the new menu is added before the first menu. If 
		the index exceeds the number of menus, then the new menu is added behind the
		last menu.
*/

instance Menus (Menu      m) | MenuElements      m
instance Menus (PopUpMenu m) | PopUpMenuElements m
/*	PopUpMenus can only be opened in a SDI or MDI process. If the parent process is 
		a NDI process, then no PopUpMenu is opened and ErrorViolateDI is returned. 
	The elements of a PopUpMenu are the same as for standard menus except for
		SubMenus. For elements the same restrictions hold as for standard menus.
	The PopUpMenu will be closed as soon as the user has dismissed it either by 
		selecting an item or clicking outside the menu. 
*/


closeMenu :: !Id !(IOSt .l) -> IOSt .l
/*	closeMenu closes the indicated Menu and all of its elements.
	The WindowMenu can not be closed by closeMenu (in case the Id argument equals 
	WindowMenuId).
*/


openMenuElements	:: !Id !Index .ls .(m .ls (PSt .l))      !(PSt .l)
											-> (!ErrorReport, !PSt .l)
											|  MenuElements m
openSubMenuElements	:: !Id !Index .ls .(m .ls (PSt .l))      !(PSt .l)
											-> (!ErrorReport, !PSt .l)
											|  MenuElements m
openRadioMenuItems	:: !Id !Index ![MenuRadioItem (PSt .l)] !(IOSt .l)
											-> (!ErrorReport,!IOSt .l)
/*	Add menu elements to the indicated Menu, SubMenu, or RadioMenu.
	openRadioMenuItems checks the first item in the list if the RadioMenu was empty.
	Menu elements are added after the item with the specified index. The index of a 
		menu element starts from one for the first menu element in the indicated 
		menu. 
		If the index is negative or zero, then the new menu elements are added
		before the first menu element of the indicated menu. 
		If the index exceeds the number of menu elements in the indicated menu, then
		the new menu elements are added behind the last menu element of the 
		indicated menu. 
	No menu elements are added if the indicated menu does not exist. In this case
		ErrorUnknownObject is returned.
	open(Sub)MenuElements have no effect in case menu elements with duplicate Ids 
	are opened. In this case ErrorIdsInUse is returned.
*/


closeMenuElements :: !Id ![Id] !(IOSt .l) -> IOSt .l
/*	closeMenuElements
		closes menu elements of the Menu identified by the first Id argument by 
		their Ids. The elements of (Sub/Radio)Menus will be removed first.  
*/


closeMenuIndexElements		:: !Id ![Index] !(IOSt .l) -> IOSt .l
closeSubMenuIndexElements	:: !Id ![Index] !(IOSt .l) -> IOSt .l
closeRadioMenuIndexElements	:: !Id ![Index] !(IOSt .l) -> IOSt .l
/*	Close menu elements of the indicated Menu, SubMenu, or RadioMenu by their Index 
	position.
	Analogous to openMenuElements and openRadioMenuItems indices range from one to 
		the number of menu elements in a menu. Invalid indices (less than one or 
		larger than the number of menu elements of the menu) are ignored.
	If the currently checked element of a RadioMenu is closed, the first remaining 
		element of that RadioMenu will be checked. 
	Closing a (Sub/Radio)Menu closes the indicated (Sub/Radio)Menu and all of its 
	elements.
*/


enableMenuSystem	:: !(IOSt .l) -> IOSt .l
disableMenuSystem	:: !(IOSt .l) -> IOSt .l
/*	Enable/disable the menu system of this interactive process. When the menu system
	is re-enabled the previously selectable menus and elements will become 
	selectable again. 
	Enable/disable operations on the menu(element)s of a disabled menu system take 
	effect when the menu system is re-enabled. 
	enableMenuSystem has no effect in case the interactive process has a (number of)
	modal dialogue(s).
*/


enableMenus			:: ![Id] !(IOSt .l) -> IOSt .l
disableMenus		:: ![Id] !(IOSt .l) -> IOSt .l
/*	Enable/disable individual menus. 
	The WindowMenu can not be enabled/disabled.
	Disabling a menu overrules the SelectStates of its elements, which become 
	unselectable.
	Enabling a disabled menu re-establishes the SelectStates of its elements.
	Enable/disable operations on the elements of a disabled menu take effect when 
	the menu is re-enabled.
*/


getMenuSelectState	:: !Id !(IOSt .l) -> (!Maybe SelectState,!IOSt .l)
/*	getMenuSelectState yields the current SelectState of the indicated menu. In case
	the menu does not exist, Nothing is returned.
*/


getMenus			:: !(IOSt .l) -> (![(Id,MenuType)],!IOSt .l)
/*	getMenus yields the Ids and MenuTypes of the current set of menus of this 
	interactive process.
*/


getMenuPos			:: !Id !(IOSt .l) -> (!Maybe Index,!IOSt .l)
/*	getMenuPos yields the index position of the indicated menu in the current list 
	of menus.
	In case the menu does not exist, Nothing is returned.
*/


setMenuTitle		:: !Id !Title !(IOSt .l) -> IOSt .l
getMenuTitle		:: !Id        !(IOSt .l) -> (!Maybe Title,!IOSt .l)
/*	setMenuTitle sets the title of the indicated menu. 
		In case the menu does not exist or refers to the WindowMenu, nothing 
		happens.
	getMenuTitle retrieves the current title of the indicated menu. 
		In case the menu does not exist, Nothing is returned.
*/
