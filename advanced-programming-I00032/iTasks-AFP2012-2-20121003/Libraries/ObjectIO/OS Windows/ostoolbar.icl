implementation module ostoolbar

import	StdMisc, StdTuple
from	osbitmap		import :: OSBitmap, osGetBitmapHandle
from	ostypes			import :: HWND, :: OSWindowPtr, OSNoWindowPtr
from	pictCCall_12	import winCreateBitmap
import	clCrossCall_12, windowCCall_12


::	OSToolbar
	=	{	toolbarPtr		:: !OSToolbarHandle		// The toolbar of the frame window (zero if no toolbar)
		,	toolbarHeight	:: !Int					// The height of the toolbar       (zero if no toolbar)
		}
::	OSToolbarHandle
	:==	OSWindowPtr

OSdefaultToolbarHeight :== 16	// The default height of the toolbar

/*	osCreateToolbar wPtr height
		creates a toolbar in the argument window with the given size of the bitmap images.
		The return Int is the actual height of the toolbar. 
*/
osCreateToolbar :: !Bool !OSWindowPtr !(!Int,!Int) !*OSToolbox -> (!(!OSToolbarHandle,!Int),!*OSToolbox)
osCreateToolbar forMDI hwnd (w,h) tb
	# (rcci,tb)		= issueCleanRequest2 (errorCallback2 "osCreateToolbar") (Rq3Cci (if forMDI CcRqCREATEMDITOOLBAR CcRqCREATESDITOOLBAR) hwnd w h) tb
	  tbPtr_Height	= case rcci.ccMsg of
						CcRETURN2	-> (rcci.p1,rcci.p2)
						CcWASQUIT	-> (OSNoWindowPtr,0)
						other		-> abort "[osCreateToolbar] expected CcRETURN1 value."
	= (tbPtr_Height,tb)

osCreateBitmapToolbarItem :: !OSToolbarHandle !OSBitmap !Int !*OSToolbox -> *OSToolbox
osCreateBitmapToolbarItem tbPtr osBitmap index tb
	= snd (issueCleanRequest2 (errorCallback2 "osCreateBitmapToolbarItem") (Rq3Cci CcRqCREATETOOLBARITEM tbPtr hbmp index) tb)
where
	hbmp	= osGetBitmapHandle  osBitmap

osCreateToolbarSeparator :: !OSToolbarHandle !*OSToolbox -> *OSToolbox
osCreateToolbarSeparator tbPtr tb
	= snd (issueCleanRequest2 (errorCallback2 "osCreateToolbarSeparator") (Rq1Cci CcRqCREATETOOLBARSEPARATOR tbPtr) tb)
