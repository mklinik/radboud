implementation module windowCrossCall_12


import	StdMisc, StdTuple
import	clCrossCall_12
from	ostypes			import :: HWND
from	clCCall_12		import winMakeCString, winGetCStringAndFree, winReleaseCString, :: CSTR
from	pictCCall_12	import :: HDC
from	rgnCCall_12		import :: HRGN


//	Cursor shape constants:
CURSHIDDEN			:== 6
CURSARROW			:== 5
CURSFATCROSS		:== 4
CURSCROSS			:== 3
CURSIBEAM			:== 2
CURSBUSY			:== 1

//	Constants for handling scrollbars.
SB_HORZ				:== 0
SB_VERT				:== 1
SB_CTL				:== 2
SB_BOTH				:== 3

SB_LINEUP			:== 0
SB_LINELEFT			:== 0
SB_LINEDOWN			:== 1
SB_LINERIGHT		:== 1
SB_PAGEUP			:== 2
SB_PAGELEFT			:== 2
SB_PAGEDOWN			:== 3
SB_PAGERIGHT		:== 3
SB_THUMBPOSITION	:== 4
SB_THUMBTRACK		:== 5
SB_TOP				:== 6
SB_LEFT				:== 6
SB_BOTTOM			:== 7
SB_RIGHT			:== 7
SB_ENDSCROLL		:== 8

//	PA: constants for handling window styles.
WS_OVERLAPPED		:== 0x00000000
WS_POPUP			:== 0x80000000
WS_CHILD			:== 0x40000000
WS_MINIMIZE			:== 0x20000000
WS_VISIBLE			:== 0x10000000
WS_DISABLED			:== 0x08000000
WS_CLIPSIBLINGS		:== 0x04000000
WS_CLIPCHILDREN		:== 0x02000000
WS_MAXIMIZE			:== 0x01000000
WS_CAPTION			:== 0x00C00000		/* WS_BORDER | WS_DLGFRAME  */
WS_BORDER			:== 0x00800000
WS_DLGFRAME			:== 0x00400000
WS_VSCROLL			:== 0x00200000
WS_HSCROLL			:== 0x00100000
WS_SYSMENU			:== 0x00080000
WS_THICKFRAME		:== 0x00040000
WS_GROUP			:== 0x00020000
WS_TABSTOP			:== 0x00010000

WS_MINIMIZEBOX		:== 0x00020000
WS_MAXIMIZEBOX		:== 0x00010000

WS_TILED			:== WS_OVERLAPPED
WS_ICONIC			:== WS_MINIMIZE
WS_SIZEBOX			:== WS_THICKFRAME
//	PA: end of addition.

//	PA: constants for stacking windows.
HWND_TOP			:==	0
HWND_BOTTOM			:==	1
HWND_TOPMOST		:== -1
HWND_NOTOPMOST		:==	-2
//	PA: end of addition.

//	PA: flag values for passing information about edit controls from Clean to OS.
EDITISMULTILINE		:==	1			/* PA: flag value: edit control is multi-line. */
EDITISKEYSENSITIVE	:==	2			/* PA: flag value: edit control sends keyboard events to Clean. */
//	PA: end of addition.

//	PA: values for telling Windows if a (custom)button control is OK, CANCEL, or normal. 
ISNORMALBUTTON		:==	0			/* The button is a normal button.   */
ISOKBUTTON			:==	1			/* The button is the OK button.     */
ISCANCELBUTTON		:==	2			/* The button is the CANCEL button. */
//	PA: end of addition


winSetWindowCursor :: !HWND !Int !*OSToolbox -> *OSToolbox
winSetWindowCursor hwnd cursorcode tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetWindowCursor") (Rq2Cci CcRqCHANGEWINDOWCURSOR hwnd cursorcode) tb)

winObscureCursor :: !*OSToolbox -> *OSToolbox
winObscureCursor tb
	= snd (issueCleanRequest2 (errorCallback2 "winObscureCursor") (Rq0Cci CcRqOBSCURECURSOR) tb)

winSetWindowTitle :: !HWND !String !*OSToolbox -> *OSToolbox
winSetWindowTitle hwnd title tb
	# (textptr,tb)	= winMakeCString title tb
	# (_,tb)		= issueCleanRequest2 (errorCallback2 "SetWindowTitle") (Rq2Cci CcRqSETWINDOWTITLE hwnd textptr) tb
	= winReleaseCString textptr tb

winGetWindowText :: !HWND !*OSToolbox -> (!String, !*OSToolbox)
winGetWindowText hwnd tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "winGetWindowText") (Rq1Cci CcRqGETWINDOWTEXT hwnd) tb
	# (text,tb)	= case rcci.ccMsg of
					CcRETURN1	-> winGetCStringAndFree rcci.p1 tb
					CcWASQUIT	-> ("",tb)
					other		-> abort "[winGetWindowText] expected CcRETURN1 value."
	= (text,tb)

/* PA: the following four functions are now implemented as C-calls.
winInvalidateWindow :: !HWND !*OSToolbox -> *OSToolbox
winInvalidateWindow hwnd tb
	= snd (issueCleanRequest2 (errorCallback2 "winInvalidateWindow") (Rq1Cci CcRqINVALIDATEWINDOW hwnd) tb)

winInvalidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winInvalidateRect hwnd (left,top, right,bottom) tb
	= snd (issueCleanRequest2 (errorCallback2 "InvalidateRect") (Rq5Cci CcRqINVALIDATERECT hwnd left top right bottom) tb)

winValidateRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winValidateRect hwnd (left,top, right,bottom) tb
	= snd (issueCleanRequest2 (errorCallback2 "ValidateRect") (Rq5Cci CcRqVALIDATERECT hwnd left top right bottom) tb)

winValidateRgn :: !HWND !HRGN !*OSToolbox -> *OSToolbox
winValidateRgn hwnd rgn tb
	= snd (issueCleanRequest2 (errorCallback2 "ValidateRgn") (Rq2Cci CcRqVALIDATERGN hwnd rgn) tb)
*/

winUpdateWindowRect :: !HWND !(!Int,!Int,!Int,!Int) !*OSToolbox -> *OSToolbox
winUpdateWindowRect hwnd (left,top,right,bottom) tb
	= snd (issueCleanRequest2 (errorCallback2 "winUpdateWindowRect") (Rq5Cci CcRqUPDATEWINDOWRECT hwnd left top right bottom) tb)

winSetSelectStateWindow :: !HWND !(!Bool,!Bool) !Bool !Bool !*OSToolbox -> *OSToolbox
winSetSelectStateWindow hwnd (hasHScroll,hasVScroll) toAble modalContext tb
	# selectCci	= Rq5Cci CcRqSETSELECTWINDOW hwnd (toInt hasHScroll) (toInt hasVScroll) (toInt toAble) (toInt modalContext)
	= snd (issueCleanRequest2 (errorCallback2 "winSetSelectStateWindow") selectCci tb)

winBeginPaint :: !HWND !*OSToolbox -> (!HDC,!*OSToolbox) 
winBeginPaint hwnd tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "BeginPaint") (Rq1Cci CcRqBEGINPAINT hwnd) tb
	  hdc		= case rcci.ccMsg of
					CcRETURN1	-> rcci.p1
					CcWASQUIT	-> 0 
					other		-> abort "[winBeginPaint] expected CcRETURN1 value."
	= (hdc,tb)

winEndPaint :: !HWND !(!HDC, !*OSToolbox) -> *OSToolbox
winEndPaint hwnd (hdc,tb)
	= snd (issueCleanRequest2 (errorCallback2 "EndPaint") (Rq2Cci CcRqENDPAINT hwnd hdc) tb)

winFakePaint :: !HWND !*OSToolbox -> *OSToolbox
winFakePaint hwnd tb
	= snd (issueCleanRequest2 (errorCallback2 "FakePaint") (Rq1Cci CcRqFAKEPAINT hwnd) tb)

winGetClientSize :: !HWND !*OSToolbox -> (!(!Int,!Int), !*OSToolbox)
winGetClientSize hwnd tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "winGetClientSize") (Rq1Cci CcRqGETCLIENTSIZE hwnd) tb
	  size		= case rcci.ccMsg of
					CcRETURN2	-> (rcci.p1,rcci.p2)
					CcWASQUIT	-> (0,0) 
					other		-> abort "[winGetClientSize] expected CcRETURN2 value."
	= (size,tb)

winGetWindowSize :: !HWND !*OSToolbox -> (!(!Int,!Int), !*OSToolbox)
winGetWindowSize hwnd tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "winGetWindowSize") (Rq1Cci CcRqGETWINDOWSIZE hwnd) tb
	  size		= case rcci.ccMsg of
	  				CcRETURN2	-> (rcci.p1,rcci.p2)
	  				CcWASQUIT	-> (0,0)
	  				other		-> abort "[winGetWindowSize] expected CcRETURN2 value."
	= (size,tb)

winSetClientSize :: !HWND !(!Int,!Int) !*OSToolbox -> *OSToolbox
winSetClientSize hwnd (w,h) tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetClientSize") (Rq3Cci CcRqSETCLIENTSIZE hwnd w h) tb)

winSetWindowSize :: !HWND !(!Int,!Int) !Bool !*OSToolbox -> *OSToolbox
winSetWindowSize hwnd (w,h) update tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetWindowSize") (Rq4Cci CcRqSETWINDOWSIZE hwnd w h (toInt update)) tb)

winGetWindowPos :: !HWND !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
winGetWindowPos hwnd tb
	# (rcci,tb)	= issueCleanRequest2 (errorCallback2 "winGetWindowPos") (Rq1Cci CcRqGETWINDOWPOS hwnd) tb
	  pos		= case rcci.ccMsg of
					CcRETURN2	-> (rcci.p1,rcci.p2)
					CcWASQUIT	-> (0,0) 
					other		-> abort "[winGetWindowPos] expected CcRETURN2 value."
	= (pos,tb)

winSetWindowPos :: !HWND !(!Int,!Int) !Bool !Bool !*OSToolbox -> *OSToolbox
winSetWindowPos hwnd (x,y) update inclScrollbars tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetWindowPos") (Rq5Cci CcRqSETWINDOWPOS hwnd x y (toInt update) (toInt inclScrollbars)) tb)

winSetScrollRange :: !HWND !Int !Int !Int !Bool !*OSToolbox -> *OSToolbox
winSetScrollRange scrollHWND iBar min max redraw tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetScrollRange") (Rq5Cci CcRqSETSCROLLRANGE scrollHWND iBar min max (toInt redraw)) tb)
	
winSetScrollPos :: !HWND !Int !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
winSetScrollPos scrollHWND iBar thumb maxx maxy extent tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetScrollPos") (Rq6Cci CcRqSETSCROLLPOS scrollHWND iBar thumb maxx maxy extent) tb)

winSetScrollThumbSize :: !HWND !Int !Int !Int !Int !Int !*OSToolbox -> *OSToolbox
winSetScrollThumbSize scrollHWND iBar size maxx maxy extent tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetScrollThumbSize") (Rq6Cci CcRqSETSCROLLSIZE scrollHWND iBar size maxx maxy extent) tb)

winSetEditSelection :: !HWND !Int !Int !*OSToolbox -> *OSToolbox
winSetEditSelection editHWND first last tb
	= snd (issueCleanRequest2 (errorCallback2 "winSetEditSelection") (Rq3Cci CcRqSETEDITSELECTION editHWND first last) tb)

winShowControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
winShowControl hwnd bool tb
	= snd (issueCleanRequest2 (errorCallback2 "winShowControl") (Rq2Cci CcRqSHOWCONTROL hwnd (toInt bool)) tb)

winEnableControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
winEnableControl hwnd bool tb
	= snd (issueCleanRequest2 (errorCallback2 "winEnableControl") (Rq2Cci CcRqENABLECONTROL hwnd (toInt bool)) tb)

winEnablePopupItem :: !HWND !Int !Bool !*OSToolbox -> *OSToolbox
winEnablePopupItem hwnd pos bool tb
	= snd (issueCleanRequest2 (errorCallback2 "winEnablePopupItem") (Rq3Cci CcRqENABLEPOPUPITEM hwnd pos (toInt bool)) tb)

winCheckControl :: !HWND !Bool !*OSToolbox -> *OSToolbox
winCheckControl hwnd bool tb
	= snd (issueCleanRequest2 (errorCallback2 "winCheckControl") (Rq2Cci CcRqSETITEMCHECK hwnd (toInt bool)) tb)

winSelectPopupItem :: !HWND !Int !*OSToolbox -> *OSToolbox
winSelectPopupItem hwnd pos tb
	= snd (issueCleanRequest2 (errorCallback2 "winSelectPopupItem") (Rq2Cci CcRqSELECTPOPUPITEM hwnd pos) tb)
