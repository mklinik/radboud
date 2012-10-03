definition module menucreate

//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************

import	StdMenuElementClass
import	menuhandle
import	ostoolbox, ostypes
from	iostate		import :: PSt

/*	Creating menus:
	In case the Boolean result is False nothing has been created due to duplicate Ids.
*/
openMenu`		:: !Id       .ls !.(Menu      m .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | MenuElements m
createPopUpMenu :: !SystemId .ls !.(PopUpMenu m .ls (PSt .l)) !(MenuHandles (PSt .l)) !*ReceiverTable !*IdTable !OSMenuBar !(PSt .l)
													 -> (!Bool,!MenuHandles (PSt .l), !*ReceiverTable,!*IdTable,!OSMenuBar, !PSt .l)
													 |  PopUpMenuElements m

//	Build a new menu (createMenuElements), and extend an existing menu (extendMenu):

createMenuElements	:: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
									  -> (!Int,![MenuElementHandle .ls .pst],![Char],!*OSToolbox)
extendMenu			:: !OSMenuBar !OSMenu !Int ![MenuElementHandle .ls .pst] ![MenuElementHandle .ls .pst] ![Char] !*OSToolbox
																		 -> (![MenuElementHandle .ls .pst],![Char],!*OSToolbox)

SystemAble				:== True
SystemUnable			:== False

closepopupmenu			:: !(MenuHandles .pst) -> MenuHandles .pst
disposeMenuItemHandle	:: !OSMenu !Int	!(MenuItemHandle    .ls .pst) !(![Char],!*IdTable,!*OSToolbox)
									 -> (!MenuItemHandle    .ls .pst, !(![Char],!*IdTable,!*OSToolbox))
disposeSubMenuHandles	:: !(MenuElementHandle .ls .pst) !(!OSMenu,!*OSToolbox)
						-> (!MenuElementHandle .ls .pst, !(!OSMenu,!*OSToolbox))
disposeMenuHandles		:: !Bool		!(MenuHandles			.pst) !(!OSMenuBar,       !*OSToolbox)
									 -> (!MenuHandles			.pst, !(!OSMenuBar,       !*OSToolbox))
disposeShortcutkeys		:: !OSWindowPtr !(MenuElementHandle .ls .pst) !(![Char],          !*OSToolbox)
									 -> (!MenuElementHandle .ls .pst, !(![Char],          !*OSToolbox))
disposeMenuIds			:: !SystemId	!(MenuElementHandle .ls .pst) !(!*ReceiverTable,  !*IdTable)
									 -> (!MenuElementHandle .ls .pst, !(!*ReceiverTable,  !*IdTable))
