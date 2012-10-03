definition module StdMenuAttribute


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdMenuAttribute specifies which MenuAttributes are valid for each of the
//	standard menus and menu elements.
//	Basic comparison operations and retrieval functions are also included.
//	********************************************************************************


import StdMenuDef


/*	The following functions specify the valid attributes for each standard menu
	(element).
*/

isValidMenuAttribute :: !(MenuAttribute .st) -> Bool
/*	Menu			(y = valid, . = invalid)
	MenuFunction	. | MenuInit			y | MenuSelectState	y |
	MenuId			y | MenuMarkState		. | MenuShortKey	. |
	MenuIndex		y | MenuModsFunction	. |
*/

isValidSubMenuAttribute :: !(MenuAttribute .st) -> Bool
/*	SubMenu			(y = valid, . = invalid)
	MenuFunction	. | MenuInit			. | MenuSelectState	y |
	MenuId			y | MenuMarkState		. | MenuShortKey	. |
	MenuIndex		. | MenuModsFunction	. |
*/

isValidRadioMenuAttribute :: !(MenuAttribute .st) -> Bool
/*	RadioMenu		(y = valid, . = invalid)
	MenuFunction	. | MenuInit			. | MenuSelectState	y |
	MenuId			y | MenuMarkState		. | MenuShortKey	. |
	MenuIndex		. | MenuModsFunction	. |
*/

isValidMenuItemAttribute :: !(MenuAttribute .st) -> Bool
/*	MenuItem		(y = valid, . = invalid)
	MenuFunction	y | MenuInit			. | MenuSelectState	y |
	MenuId			y | MenuMarkState		y | MenuShortKey	y |
	MenuIndex		. | MenuModsFunction	y |
*/

isValidMenuSeparatorAttribute :: !(MenuAttribute .st) -> Bool
/*	MenuSeparator	(y = valid, . = invalid)
	MenuFunction	. | MenuInit			. | MenuSelectState	. |
	MenuId			y | MenuMarkState		. | MenuShortKey	. |
	MenuIndex		. | MenuModsFunction	. |
*/


/*	The following functions return True only iff the attribute equals the 
	indicated name.
*/
isMenuFunction			:: !(MenuAttribute .st) -> Bool
isMenuId				:: !(MenuAttribute .st)	-> Bool
isMenuIndex				:: !(MenuAttribute .st) -> Bool
isMenuInit				:: !(MenuAttribute .st) -> Bool
isMenuMarkState			:: !(MenuAttribute .st)	-> Bool
isMenuModsFunction		:: !(MenuAttribute .st)	-> Bool
isMenuSelectState		:: !(MenuAttribute .st)	-> Bool
isMenuShortKey			:: !(MenuAttribute .st)	-> Bool


/*	The following functions return the attribute value if appropriate. 
	THESE ARE PARTIAL FUNCTIONS! They are only defined on the corresponding
	attribute.
*/
getMenuFun				:: !(MenuAttribute .st) -> IdFun .st
getMenuIdAtt			:: !(MenuAttribute .st)	-> Id
getMenuIndexAtt			:: !(MenuAttribute .st) -> Index
getMenuInitFun			:: !(MenuAttribute .st) -> IdFun .st
getMenuMarkStateAtt		:: !(MenuAttribute .st)	-> MarkState
getMenuModsFun			:: !(MenuAttribute .st)	-> ModifiersFunction .st
getMenuSelectStateAtt	:: !(MenuAttribute .st)	-> SelectState
getMenuShortKeyAtt		:: !(MenuAttribute .st)	-> Char
