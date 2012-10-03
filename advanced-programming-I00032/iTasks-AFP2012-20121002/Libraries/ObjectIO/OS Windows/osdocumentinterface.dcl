definition module osdocumentinterface

//	Clean object I/O library, version 1.2

import	StdIOCommon
from	menuCrossCall_12	import :: HMENU
import	ostoolbar, ostoolbox, ostypes

::	OSDInfo
	=	OSMDInfo !OSMDInfo
	|	OSSDInfo !OSSDInfo
	|	OSNoInfo
::	OSMDInfo
	=	{	osmdOSInfo		:: !OSInfo			// The general document interface infrastructure
		,	osmdWindowMenu	:: !HMENU			// The Window menu in the MDI menu bar
		}
::	OSSDInfo
	=	{	ossdOSInfo		:: !OSInfo			// The general document interface infrastructure
		}
::	OSInfo
	=	{	osFrame			:: !HWND			// The frame window of the (M/S)DI frame window
		,	osToolbar		:: !Maybe OSToolbar	// The toolbar of the (M/S)DI frame window (Nothing if no toolbar)
		,	osClient		:: !HWND			// The client window of the (M/S)DI frame window
		,	osMenuBar		:: !HMENU			// The menu bar of the (M/S)DI frame window
		}
::	OSMenuBar
	=	{	menuBar			:: !HMENU
		,	menuWindow		:: !HWND
		,	menuClient		:: !HWND
		}

/*	Before using osOpenMDI, osOpenSDI, or osCloseOSDInfo evaluate osInitialiseDI.
*/
osInitialiseDI :: !*OSToolbox -> *OSToolbox

/*	emptyOSDInfo creates a OSDInfo with dummy values for the argument document interface.
*/
emptyOSDInfo :: !DocumentInterface -> OSDInfo

/*	getOSDInfoDocumentInterface returns the DocumentInterface of the argument OSDInfo.
*/
getOSDInfoDocumentInterface :: !OSDInfo -> DocumentInterface

/*	getOSDInfoOSMenuBar returns the OSMenuBar info from the argument OSDInfo.
	setOSDInfoOSMenuBar sets the OSMenuBar info in the OSDInfo.
*/
getOSDInfoOSMenuBar ::            !OSDInfo -> Maybe OSMenuBar
setOSDInfoOSMenuBar :: !OSMenuBar !OSDInfo -> OSDInfo

/*	getOSDInfoOSInfo returns the OSInfo from the argument OSDInfo if present.
	setOSDInfoOSInfo sets the OSInfo in the OSDInfo.
*/
getOSDInfoOSInfo ::         !OSDInfo -> Maybe OSInfo
setOSDInfoOSInfo :: !OSInfo !OSDInfo -> OSDInfo

/*	osOpenMDI  creates  the infrastructure of a MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
	osOpenSDI  creates the infrastructure of a SDI process.
		The Bool argument indicates whether the process accepts file open events.
	osOpenNDI  creates the infrastructure of a NDI process.
	osCloseOSDInfo destroys the infrastructure.
*/
osOpenMDI     :: !Bool !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
osOpenSDI     ::       !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
osOpenNDI     ::             !*OSToolbox -> (!OSDInfo,!*OSToolbox)
osCloseOSDInfo:: !OSDInfo    !*OSToolbox -> *OSToolbox

/*	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
*/
getOSDInfoOSToolbar :: !OSDInfo -> Maybe OSToolbar

/*	osOSDInfoIsActive tests if the given OSDInfo represents the interactive process with the
	active menu system. (Always True on Windows; use menu bar on Mac.)
*/
osOSDInfoIsActive :: !OSDInfo !*OSToolbox -> (!Bool, !*OSToolbox)

/*	getOSDInfoOffset returns the offset vector (dx,dy) that points to the left-top corner of the client area
	of the corresponding infrastructure.
*/
getOSDInfoOffset :: !OSDInfo !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
