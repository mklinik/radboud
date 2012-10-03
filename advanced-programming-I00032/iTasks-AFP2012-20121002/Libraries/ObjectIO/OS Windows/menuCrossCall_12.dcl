definition module menuCrossCall_12


import	StdString
from	StdIOCommon	import :: Modifiers
from	ostoolbox	import :: OSToolbox
from	ostypes		import :: HWND


::	HITEM	:==  Int
::	HMENU	:==  Int


winCreatePopupMenuHandle::										!*OSToolbox -> (!HMENU, !*OSToolbox)
winTrackPopupMenu		:: !HMENU !HWND							!*OSToolbox -> (!Int,!Modifiers,!*OSToolbox)
winInsertMenu			:: !String !Bool !HMENU !HMENU !Int		!*OSToolbox -> *OSToolbox
winInsertMenuItem       :: !String !Bool !Bool !HMENU !Int		!*OSToolbox -> (!HITEM, !*OSToolbox)
winInsertSeparator		:: !HMENU !Int							!*OSToolbox -> *OSToolbox
winChangeMenuItemCheck  :: !HMENU !HITEM !Bool					!*OSToolbox -> *OSToolbox
winModifyMenu			:: !String !HMENU !HMENU				!*OSToolbox -> *OSToolbox
winModifyMenuItem       :: !String !HITEM !HMENU				!*OSToolbox -> *OSToolbox
winDestroyMenu			:: !HMENU								!*OSToolbox -> *OSToolbox
winDeleteMenu			:: !HMENU !HITEM						!*OSToolbox -> *OSToolbox
winRemoveMenuItem       :: !HMENU !HITEM						!*OSToolbox -> *OSToolbox
winChangeItemAbility    :: !HMENU !HITEM !Bool					!*OSToolbox -> *OSToolbox
winChangeMenuAbility	:: !HMENU !Int   !Bool					!*OSToolbox -> *OSToolbox
winDrawMenuBar			:: !HWND !HWND							!*OSToolbox -> *OSToolbox
winAddMenuShortKey		:: !HWND !Int !Char						!*OSToolbox -> *OSToolbox
winRemoveMenuShortKey	:: !HWND !Int							!*OSToolbox -> *OSToolbox
