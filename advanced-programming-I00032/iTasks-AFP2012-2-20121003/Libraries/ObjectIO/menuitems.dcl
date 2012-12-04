definition module menuitems


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


import	StdMenuElementClass
import	menuhandle
import	osdocumentinterface, ostoolbox
from	id					import :: IdTable
from	iostate				import :: PSt
from	receivertable		import :: ReceiverTable
from	systemid			import :: SystemId


//	Adding and removing menu elements to and from existing menus.

addMenusItems		:: !(!Id,Maybe Id) !Int .ls` .(m .ls` (PSt .l)) !SystemId
										!*ReceiverTable !*IdTable !(MenuHandles (PSt .l)) !OSMenuBar !(PSt .l)
					-> (!*(!ErrorReport,!*ReceiverTable,!*IdTable),!MenuHandles (PSt .l), !OSMenuBar, !PSt .l)
					|  MenuElements m
/*	addMenusItems (id,Just subid) _ _ items ...
		adds items to the SubMenu identified by subid which is contained in the Menu identified by id.
	addMenusItems (id,Nothing)    _ _ items ...
		adds items to the Menu identified by id.
*/

addMenuRadioItems :: !(!Id,Id) !Int [MenuRadioItem (PSt .l)] !OSMenuBar !(MenuHandles (PSt .l)) !*OSToolbox
													   -> *(!ErrorReport,!MenuHandles (PSt .l), !*OSToolbox)
/*	addMenuRadioItems (id,radioid) _ items ...
		adds items to the RadioMenu identified by radioid which is contained in the Menu identified by id.
*/


//	Adding and removing menu elements to and from existing menus.

removeMenusItems		:: !OSDInfo !Id ![Id] !SystemId !OSMenuBar
							!*(!*ReceiverTable,!*IdTable) !(MenuHandles .pst) !*OSToolbox
						-> (!*(!*ReceiverTable,!*IdTable), !MenuHandles .pst, !*OSToolbox)
/*	removeMenusItems ...
*/

removeMenusIndexItems	:: !OSDInfo !Bool !Bool !(!Id,!Maybe Id) ![Int] !SystemId !OSMenuBar
							!*(!*ReceiverTable,!*IdTable) !(MenuHandles .pst) !*OSToolbox
						-> (!*(!*ReceiverTable,!*IdTable), !MenuHandles .pst, !*OSToolbox)
/*	removeMenusIndexItems ...
*/
