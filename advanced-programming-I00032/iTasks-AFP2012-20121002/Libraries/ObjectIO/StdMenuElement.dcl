definition module StdMenuElement


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdMenuElement specifies all functions on menu elements.
//	********************************************************************************


import	StdMenuDef
from	iostate import :: IOSt


/*	Functions that change the state of menu elements.
	Only those Id arguments that refer to menu elements within the same interactive 
	process are used to change the corresponding menu elements.
*/

enableMenuElements		:: ![Id] !(IOSt .l) -> IOSt .l
disableMenuElements		:: ![Id] !(IOSt .l) -> IOSt .l
/*	(en/dis)ableMenuElements set the SelectState of the indicated menu elements.
	Disabling a (Sub/Radio)Menu overrules the SelectStates of its elements, which 
		become unselectable.
	Enabling a disabled (Sub/Radio)Menu re-establishes the SelectStates of its 
		elements.
	(En/Dis)able operations on the elements of a disabled (Sub/Radio)Menu take 
		effect when the (Sub/Radio)Menu is re-enabled.
*/

setMenuElementTitles	:: ![(Id,Title)] !(IOSt .l) -> IOSt .l
/*	setMenuElementTitles sets the titles of the indicated menu elements.
*/

markMenuItems			:: ![Id] !(IOSt .l) -> IOSt .l
unmarkMenuItems			:: ![Id] !(IOSt .l) -> IOSt .l
/*	(un)markMenuItems sets the MarkState of the indicated MenuItems. 
*/

selectRadioMenuItem		:: !Id !Id    !(IOSt .l) -> IOSt .l
selectRadioMenuIndexItem:: !Id !Index !(IOSt .l) -> IOSt .l
/*	selectRadioMenu(Index)Item
		selects the indicated MenuRadioItem of a RadioMenu, causing the mark of the 
		previously marked MenuRadioItem to disappear. 
	selectRadioMenuItem 
		indicates the MenuRadioItem by the Id of its parent RadioMenu and its Id.
	selectRadioMenuIndexItem 
		indicates the MenuRadioItem by the Id of its parent RadioMenu and its index 
		position (counted from 1).
*/


/*	Access functions on MState. To read the state of a menu element, a MState is 
	required which can be obtained by the getMenu function. The MState value 
	represents the state of a menu at that particular moment.
*/

::	MState

getMenu			:: !Id !(IOSt .l) -> (!Maybe MState, !IOSt .l)
getParentMenu	:: !Id !(IOSt .l) -> (!Maybe MState, !IOSt .l)
/*	getMenu returns a read-only MState for the indicated menu.
		In case the indicated menu does not exist Nothing is returned.
	getParentMenu returns a read-only MState for the indicated menu element.
		In case the Id does not correspond with a menu element, then Nothing
		is returned.
*/

getMenuElementTypes         ::     !MState -> [(MenuElementType,Maybe Id)]
getCompoundMenuElementTypes :: !Id !MState -> [(MenuElementType,Maybe Id)]
/*	getMenuElementTypes
		yields the list of MenuElementTypes of all menu elements of this menu. 
	getCompoundMenuElementTypes
		yields the list of MenuElementTypes of all menu elements of this 
		(Sub/Radio)Menu.
	Both functions return (Just id) if the element has a MenuId attribute, and 
	Nothing otherwise. 
	Ids are not collected recursively through (Sub/Radio)Menus.
*/

/*	Functions that return the current state of menu elements.
	For each access there is one singular and one plural version. In case of the
	plural version the result list is of equal length as the argument Id list. Each 
	result list element corresponds in order with the argument Id list. 
	In both versions the first Boolean result is False in case of invalid Ids (if so
	dummy values are returned - see comment).
	Important: menu elements with no MenuId attribute, or illegal ids, can not be 
	found in the MState!
*/
getSelectedRadioMenuItems	:: ![Id]  !MState -> [(!Index,!Maybe Id)]
getSelectedRadioMenuItem	:: ! Id   !MState ->  (!Index,!Maybe Id)
/*	getSelectedRadioMenuItem(s)
		returns the Index and Id, if any, of the currently selected MenuRadioItem of
		the indicated RadioMenu. 
	If the RadioMenu does not exist or is empty, the Index is zero and the Id is 
	Nothing.
*/

getMenuElementSelectStates	:: ![Id] !MState -> [(Bool,SelectState)]
getMenuElementSelectState	:: ! Id  !MState ->  (Bool,SelectState)
/*	getMenuElementSelectState(s) yields the SelectStates of the indicated elements.
	If the element does not exist Able is returned.
*/
 
getMenuElementMarkStates	:: ![Id] !MState -> [(Bool,MarkState)]
getMenuElementMarkState		:: ! Id  !MState ->  (Bool,MarkState)
/*	getMenuElementMarkState(s) yields the MarkState of the indicated elements. 
	If the element does not exist NoMark is returned.
*/

getMenuElementTitles		:: ![Id] !MState -> [(Bool,Maybe String)]
getMenuElementTitle			:: ! Id  !MState ->  (Bool,Maybe String)
/*	getMenuElementTitle(s) yields (Just title) of the indicated (SubMenu/MenuItem),
	Nothing otherwise.
*/

getMenuElementShortKeys		:: ![Id] !MState -> [(Bool,Maybe Char)]
getMenuElementShortKey		:: ! Id  !MState ->  (Bool,Maybe Char)
/*	getMenuElementShortKey(s) yields (Just key) of the indicated MenuItem, Nothing 
	otherwise.
*/
