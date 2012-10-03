implementation module StdMenuDef


import	StdIOCommon, StdMaybe


/*	Menus:				*/

::	Menu      m ls pst = Menu Title (m ls pst) [MenuAttribute *(ls,pst)]
::	PopUpMenu m ls pst = PopUpMenu  (m ls pst)


/*	Menu elements:		*/

::	MenuItem      ls pst = MenuItem Title           [MenuAttribute *(ls,pst)]
::	MenuSeparator ls pst = MenuSeparator            [MenuAttribute *(ls,pst)]
::	RadioMenu     ls pst = RadioMenu                [MenuRadioItem *(ls,pst)]  
                                              Index [MenuAttribute *(ls,pst)]
::	SubMenu     m ls pst = SubMenu Title (m ls pst) [MenuAttribute *(ls,pst)]

::	MenuRadioItem st  :== (Title,Maybe Id,Maybe Char,IdFun st)

::	MenuAttribute			st						// Default:
 //	Attributes for Menus and MenuElements:
	=	MenuId				Id						// no Id
	|	MenuSelectState		SelectState				// menu(item) Able
 //	Attributes only for Menus:
	|	MenuIndex			Int						// at the end of the current menu list
	|	MenuInit			(IdFun st)				// no actions after opening menu
 //	Attributes ignored by (Sub)Menus:
	|	MenuFunction		(IdFun				st)	// \x->x
	|	MenuMarkState		MarkState				// NoMark
	|	MenuModsFunction	(ModifiersFunction	st)	// MenuFunction
	|	MenuShortKey		Char					// no ShortKey

::	MenuType		:==	String
::	MenuElementType	:==	String
