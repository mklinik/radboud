implementation module StdMenuAttribute


import StdMenuDef


isValidMenuAttribute :: !(MenuAttribute .st) -> Bool
isValidMenuAttribute (MenuId          _)	= True
isValidMenuAttribute (MenuIndex       _)	= True
isValidMenuAttribute (MenuInit        _)	= True
isValidMenuAttribute (MenuSelectState _)	= True
isValidMenuAttribute _						= False

isValidSubMenuAttribute :: !(MenuAttribute .st) -> Bool
isValidSubMenuAttribute (MenuId          _)	= True
isValidSubMenuAttribute (MenuSelectState _)	= True
isValidSubMenuAttribute _					= False

isValidRadioMenuAttribute :: !(MenuAttribute .st) -> Bool
isValidRadioMenuAttribute (MenuId          _)	= True
isValidRadioMenuAttribute (MenuSelectState _)	= True
isValidRadioMenuAttribute _						= False

isValidMenuItemAttribute :: !(MenuAttribute .st) -> Bool
isValidMenuItemAttribute (MenuIndex _)	= False
isValidMenuItemAttribute (MenuInit  _)	= False
isValidMenuItemAttribute _				= True

isValidMenuSeparatorAttribute :: !(MenuAttribute .st) -> Bool
isValidMenuSeparatorAttribute (MenuId _)	= True
isValidMenuSeparatorAttribute _				= False


isMenuFunction		:: !(MenuAttribute .st) -> Bool
isMenuFunction		(MenuFunction _)		= True
isMenuFunction		_						= False

isMenuId			:: !(MenuAttribute .st)	-> Bool
isMenuId			(MenuId _)				= True
isMenuId			_						= False

isMenuIndex			:: !(MenuAttribute .st) -> Bool
isMenuIndex			(MenuIndex _)			= True
isMenuIndex			_						= False

isMenuInit			:: !(MenuAttribute .st) -> Bool
isMenuInit			(MenuInit _)			= True
isMenuInit			_						= False

isMenuMarkState		:: !(MenuAttribute .st)	-> Bool
isMenuMarkState		(MenuMarkState _)		= True
isMenuMarkState		_						= False

isMenuModsFunction	:: !(MenuAttribute .st)	-> Bool
isMenuModsFunction	(MenuModsFunction _)	= True
isMenuModsFunction	_						= False

isMenuSelectState	:: !(MenuAttribute .st)	-> Bool
isMenuSelectState	(MenuSelectState _)		= True
isMenuSelectState	_						= False

isMenuShortKey		:: !(MenuAttribute .st)	-> Bool
isMenuShortKey		(MenuShortKey _)		= True
isMenuShortKey		_						= False


getMenuFun :: !(MenuAttribute .st) -> IdFun .st
getMenuFun (MenuFunction f) = f

getMenuIdAtt :: !(MenuAttribute .st) -> Id
getMenuIdAtt (MenuId id) = id

getMenuIndexAtt :: !(MenuAttribute .st) -> Index
getMenuIndexAtt (MenuIndex index) = index

getMenuInitFun :: !(MenuAttribute .st) -> IdFun .st
getMenuInitFun (MenuInit f) = f

getMenuMarkStateAtt :: !(MenuAttribute .st) -> MarkState
getMenuMarkStateAtt (MenuMarkState mark) = mark

getMenuModsFun :: !(MenuAttribute .st) -> ModifiersFunction .st
getMenuModsFun (MenuModsFunction f) = f

getMenuSelectStateAtt :: !(MenuAttribute .st) -> SelectState
getMenuSelectStateAtt (MenuSelectState select) = select

getMenuShortKeyAtt :: !(MenuAttribute .st) -> Char
getMenuShortKeyAtt (MenuShortKey key) = key
