implementation module menuCrossCall_12


import	StdInt, StdMisc, StdTuple
from	StdIOCommon	import :: Modifiers
import	clCrossCall_12
from	clCCall_12	import winMakeCString, winReleaseCString, :: CSTR, toModifiers
from	ostypes		import :: HWND


::	HITEM	:==  Int
::	HMENU	:==  Int


winCreatePopupMenuHandle :: !*OSToolbox -> (!HMENU,!*OSToolbox)		// PA: check if this can be a C call
winCreatePopupMenuHandle tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "winCreatePopupMenuHandle") (Rq0Cci CcRqCREATEPOPMENU) tb
	  menu		= case rcci.ccMsg of
					CcRETURN1	-> rcci.p1
					CcWASQUIT	-> 0
					otherwise	-> abort "[winCreatePopupMenuHandle] expected CcRETURN1 value."
	= (menu,tb)

winTrackPopupMenu :: !HMENU !HWND !*OSToolbox -> (!Int,!Modifiers,!*OSToolbox)
winTrackPopupMenu menu framePtr tb
	# (rcci,tb)			= issueCleanRequest2 (errorCallback2 "winTrackPopupMenu") (Rq2Cci CcRqTRACKPOPMENU menu framePtr) tb
	  (menuItemID,mods)	= case rcci.ccMsg of
							CcRETURN2	-> (rcci.p1,rcci.p2)
							CcWASQUIT	-> (0, 0)
							otherwise	-> abort "[winTrackPopupMenu] expected CcRETURN2 value."
	= (menuItemID,toModifiers mods,tb)

winInsertMenu :: !String !Bool !HMENU !HMENU !Int !*OSToolbox -> *OSToolbox
winInsertMenu text state submenu menu pos tb
	# (textptr,tb)	= winMakeCString text tb
	# (_,tb)		= issueCleanRequest2 (errorCallback2 "winInsertMenu") (Rq5Cci CcRqINSERTMENU (toInt state) menu textptr submenu pos) tb
	= winReleaseCString textptr tb

winInsertMenuItem :: !String !Bool !Bool !HMENU !Int !*OSToolbox -> (!HITEM,!*OSToolbox)
winInsertMenuItem text ablestate markstate menu pos tb
	# (textptr,tb)	= winMakeCString text tb
	  insertCci		= Rq5Cci CcRqINSERTMENUITEM (toInt ablestate) menu textptr (toInt markstate) pos
	# (rcci,tb)		= issueCleanRequest2 (errorCallback2 "winInsertMenuItem") insertCci tb
	  hitem			= case rcci.ccMsg of
						CcRETURN1	-> rcci.p1
						CcWASQUIT	-> 0
						other		-> abort "[winInsertMenuItem] expected CcRETURN1 value."
	# tb			= winReleaseCString textptr tb
	= (hitem,tb)

winInsertSeparator :: !HMENU !Int !*OSToolbox -> *OSToolbox
winInsertSeparator menu pos tb
	= snd (issueCleanRequest2 (errorCallback2 "winInsertSeparator") (Rq2Cci CcRqINSERTSEPARATOR menu pos) tb)

winChangeMenuItemCheck :: !HMENU !HITEM !Bool !*OSToolbox -> *OSToolbox
winChangeMenuItemCheck menu hitem state tb
	= snd (issueCleanRequest2 (errorCallback2 "winChangeMenuItemCheck") (Rq3Cci CcRqCHECKMENUITEM menu hitem (toInt state)) tb)

winModifyMenu :: !String !HMENU !HMENU !*OSToolbox -> *OSToolbox
winModifyMenu text submenu menu tb
	# (textptr,tb)	= winMakeCString text tb
	# (_,tb)		= issueCleanRequest2 (errorCallback2 "winModifyMenu") (Rq3Cci CcRqMODIFYMENU submenu menu textptr) tb
	= winReleaseCString textptr tb

winModifyMenuItem :: !String !HITEM !HMENU !*OSToolbox -> *OSToolbox
winModifyMenuItem text hitem menu tb
	# (textptr,tb)	= winMakeCString text tb
	# (_,tb)		= issueCleanRequest2 (errorCallback2 "winModifyMenuItem") (Rq3Cci CcRqMODIFYMENUITEM hitem menu textptr) tb
	= winReleaseCString textptr tb

winDestroyMenu :: !HMENU !*OSToolbox -> *OSToolbox
winDestroyMenu menu tb
	= snd (issueCleanRequest2 (errorCallback2 "winDestroyMenu") (Rq1Cci CcRqDESTROYMENU menu) tb)

winDeleteMenu :: !HMENU !HITEM !*OSToolbox -> *OSToolbox
winDeleteMenu menu hitem tb
	= snd (issueCleanRequest2 (errorCallback2 "winDeleteMenu") (Rq2Cci CcRqDELETEMENU menu hitem) tb)

winRemoveMenuItem :: !HMENU !HITEM !*OSToolbox -> *OSToolbox
winRemoveMenuItem menu hitem tb
	= snd (issueCleanRequest2 (errorCallback2 "winRemoveMenuItem") (Rq2Cci CcRqREMOVEMENUITEM menu hitem) tb)

winChangeItemAbility :: !HMENU !HITEM !Bool !*OSToolbox -> *OSToolbox
winChangeItemAbility parent hitem onoff tb
	= snd (issueCleanRequest2 (errorCallback2 "winChangeItemAbility") (Rq3Cci CcRqITEMENABLE parent hitem (toInt onoff)) tb)

winChangeMenuAbility :: !HMENU !Int !Bool !*OSToolbox -> *OSToolbox
winChangeMenuAbility parent zIndex onoff tb
	= snd (issueCleanRequest2 (errorCallback2 "winChangeMenuAbility") (Rq3Cci CcRqMENUENABLE parent zIndex (toInt onoff)) tb)

winDrawMenuBar :: !HWND !HWND !*OSToolbox -> *OSToolbox
winDrawMenuBar framePtr clientPtr tb
	= snd (issueCleanRequest2 (errorCallback2 "winDrawMenuBar") (Rq2Cci CcRqDRAWMBAR framePtr clientPtr) tb)

winAddMenuShortKey :: !HWND !Int !Char !*OSToolbox -> *OSToolbox
winAddMenuShortKey framePtr cmd key tb
	= snd (issueCleanRequest2 (errorCallback2 "winAddMenuShortKey") (Rq3Cci CcRqADDMENUSHORTKEY framePtr cmd (toInt key)) tb)

winRemoveMenuShortKey :: !HWND !Int !*OSToolbox -> *OSToolbox
winRemoveMenuShortKey framePtr cmd tb
	= snd (issueCleanRequest2 (errorCallback2 "winRemoveMenuShortKey") (Rq2Cci CcRqREMOVEMENUSHORTKEY framePtr cmd) tb)
