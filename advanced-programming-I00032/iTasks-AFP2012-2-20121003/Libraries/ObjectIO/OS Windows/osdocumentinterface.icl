implementation module osdocumentinterface


import	StdInt, StdMaybe, StdOverloaded, StdString, StdTuple
import	clCrossCall_12, ostoolbar, ossystem, ostypes, windowCrossCall_12
from	commondef	import fatalError
from	StdIOCommon	import :: DocumentInterface(..)
import	code from "cCrossCallxDI_121."


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


osdocumentinterfaceFatalError :: String String -> .x
osdocumentinterfaceFatalError function error
	= fatalError function "osdocumentinterface" error

osInitialiseDI :: !*OSToolbox -> *OSToolbox
osInitialiseDI _
	= code
	{
		.inline InstallCrossCallxDI
			ccall InstallCrossCallxDI "I-I"
		.end
	}

/*	emptyOSDInfo creates a OSDInfo with dummy values for the argument document interface.
*/
emptyOSDInfo :: !DocumentInterface -> OSDInfo
emptyOSDInfo di
	= case di of
		MDI -> OSMDInfo {osmdOSInfo=emptyOSInfo,osmdWindowMenu=(-1)}
		SDI -> OSSDInfo {ossdOSInfo=emptyOSInfo}
		NDI -> OSNoInfo
where
	emptyOSInfo = {osFrame=(-1),osToolbar=Nothing,osClient=(-1),osMenuBar=(-1)}


/*	getOSDInfoDocumentInterface returns the DocumentInterface of the argument OSDInfo.
*/
getOSDInfoDocumentInterface :: !OSDInfo -> DocumentInterface
getOSDInfoDocumentInterface (OSMDInfo _)	= MDI
getOSDInfoDocumentInterface (OSSDInfo _)	= SDI
getOSDInfoDocumentInterface OSNoInfo		= NDI


/*	getOSDInfoOSMenuBar returns the OSMenuBar info from the argument OSDInfo.
	setOSDInfoOSMenuBar sets the OSMenuBar info in the OSDInfo.
*/
getOSDInfoOSMenuBar :: !OSDInfo -> Maybe OSMenuBar
getOSDInfoOSMenuBar osdInfo
	= case osdInfo of
		OSMDInfo {osmdOSInfo} -> get osmdOSInfo
		OSSDInfo {ossdOSInfo} -> get ossdOSInfo
		osnoinfo              -> Nothing
where
	get {osFrame,osClient,osMenuBar} = Just {menuBar=osMenuBar,menuWindow=osFrame,menuClient=osClient}

setOSDInfoOSMenuBar :: !OSMenuBar !OSDInfo -> OSDInfo
setOSDInfoOSMenuBar {menuBar,menuWindow,menuClient} osdInfo
	= case osdInfo of
		OSMDInfo mdi=:{osmdOSInfo=info} -> OSMDInfo {mdi & osmdOSInfo=set info}
		OSSDInfo sdi=:{ossdOSInfo=info} -> OSSDInfo {sdi & ossdOSInfo=set info}
		osnoinfo                        -> osnoinfo
where
	set info = {info & osMenuBar=menuBar,osFrame=menuWindow,osClient=menuClient}


/*	getOSDInfoOSInfo returns the OSInfo from the argument OSDInfo if present.
	setOSDInfoOSInfo sets the OSInfo in the OSDInfo.
*/
getOSDInfoOSInfo :: !OSDInfo -> Maybe OSInfo
getOSDInfoOSInfo (OSMDInfo {osmdOSInfo}) = Just osmdOSInfo
getOSDInfoOSInfo (OSSDInfo {ossdOSInfo}) = Just ossdOSInfo
getOSDInfoOSInfo osnoinfo                = Nothing

setOSDInfoOSInfo :: !OSInfo !OSDInfo -> OSDInfo
setOSDInfoOSInfo osinfo (OSMDInfo osm) = OSMDInfo {osm & osmdOSInfo=osinfo}
setOSDInfoOSInfo osinfo (OSSDInfo oss) = OSSDInfo {oss & ossdOSInfo=osinfo}
setOSDInfoOSInfo _       osnoinfo      = osnoinfo


/*	osOpenMDI creates the infrastructure of an MDI process.
		If the first Bool argument is True, then the frame window is shown, otherwise it is hidden.
		The second Bool indicates whether the process accepts file open events.
*/
osOpenMDI :: !Bool !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
osOpenMDI show acceptFileOpen tb
	# createCci			= Rq2Cci CcRqCREATEMDIFRAMEWINDOW (toInt show) (toInt acceptFileOpen)
	# (returncci,tb)	= issueCleanRequest2 osCreateMDIWindowCallback createCci tb
	  (framePtr,clientPtr,menuBar,windowMenu)
		  				= case returncci.ccMsg of
			  				CcRETURN4	-> (returncci.p1,returncci.p2,returncci.p3,returncci.p4)
			  				CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr,OSNoWindowPtr,OSNoWindowPtr)
			  				msg			-> osdocumentinterfaceFatalError "OSopenMDI" ("CcRETURN4 expected instead of "+++toString msg)
	# osmdinfo			= {	osmdOSInfo		= {	osFrame		= framePtr
											  ,	osToolbar	= Nothing
											  ,	osClient	= clientPtr
											  ,	osMenuBar	= menuBar
											  }
						  ,	osmdWindowMenu	= windowMenu
						  }
	= (OSMDInfo osmdinfo,tb)
where
	osCreateMDIWindowCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osCreateMDIWindowCallback {ccMsg=CcWmDEACTIVATE} tb
		= (return0Cci,tb)
	osCreateMDIWindowCallback {ccMsg=CcWmACTIVATE} tb
		= (return0Cci,tb)
	osCreateMDIWindowCallback {ccMsg=CcWmKILLFOCUS} tb		/* PA: added. Shouldn't ControlDeactivate be delayed? */
		= (return0Cci,tb)
	osCreateMDIWindowCallback {ccMsg} tb
		= osdocumentinterfaceFatalError "osCreateMDIWindowCallback" ("received message nr:"+++toString ccMsg)

osOpenSDI :: !Bool !*OSToolbox -> (!OSDInfo,!*OSToolbox)
osOpenSDI acceptFileOpen tb
	# createCci			= Rq1Cci CcRqCREATESDIFRAMEWINDOW (toInt acceptFileOpen)
	# (returncci,tb)	= issueCleanRequest2 osCreateSDIWindowCallback createCci tb
	  (framePtr,menuBar)= case returncci.ccMsg of
	  						CcRETURN2	-> (returncci.p1,returncci.p2)
	  						CcWASQUIT	-> (OSNoWindowPtr,OSNoWindowPtr)
	  						msg			-> osdocumentinterfaceFatalError "OSopenSDI" ("CcRETURN2 expected instead of "+++toString msg)
	# ossdinfo			= {	ossdOSInfo = {osFrame=framePtr,osToolbar=Nothing,osClient=OSNoWindowPtr,osMenuBar=menuBar} }
	= (OSSDInfo ossdinfo,tb)
where
	osCreateSDIWindowCallback :: !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
	osCreateSDIWindowCallback {ccMsg=CcWmDEACTIVATE} tb
		= (return0Cci,tb)
	osCreateSDIWindowCallback {ccMsg=CcWmACTIVATE} tb
		= (return0Cci,tb)
	osCreateSDIWindowCallback {ccMsg=CcWmKILLFOCUS} tb		/* PA: added. Shouldn't ControlDeactivate be delayed? */
		= (return0Cci,tb)
	osCreateSDIWindowCallback {ccMsg} tb
		= osdocumentinterfaceFatalError "osCreateSDIWindowCallback" ("received message nr:"+++toString ccMsg)

osOpenNDI :: !*OSToolbox -> (!OSDInfo,!*OSToolbox)			// PA: added. Dummy on Windows.
osOpenNDI tb
	= (OSNoInfo,tb)

osCloseOSDInfo :: !OSDInfo !*OSToolbox -> *OSToolbox
osCloseOSDInfo (OSMDInfo {osmdOSInfo={osFrame}}) tb
	= snd (issueCleanRequest2 (osDestroyProcessWindowCallback "osCloseMDI") (Rq1Cci CcRqDESTROYWINDOW osFrame) tb)
osCloseOSDInfo (OSSDInfo {ossdOSInfo={osFrame}}) tb
	= snd (issueCleanRequest2 (osDestroyProcessWindowCallback "osCloseSDI") (Rq1Cci CcRqDESTROYWINDOW osFrame) tb)
osCloseOSDInfo _ tb
	= tb

osDestroyProcessWindowCallback :: String !CrossCallInfo !*OSToolbox -> (!CrossCallInfo,!*OSToolbox)
osDestroyProcessWindowCallback _ {ccMsg=CcWmDEACTIVATE} tb
	= (return0Cci,tb)
osDestroyProcessWindowCallback _ {ccMsg=CcWmACTIVATE} tb
	= (return0Cci,tb)
osDestroyProcessWindowCallback _ {ccMsg=CcWmKEYBOARD} tb
	= (return0Cci,tb)
osDestroyProcessWindowCallback _ {ccMsg=CcWmPAINT,p1=hwnd} tb
	= (return0Cci,winFakePaint hwnd tb)
osDestroyProcessWindowCallback function {ccMsg} tb
	= osdocumentinterfaceFatalError function ("received message nr:"+++toString ccMsg)

//	getOSDInfoOSToolbar retrieves the OSToolbar, if any.
getOSDInfoOSToolbar :: !OSDInfo -> Maybe OSToolbar
getOSDInfoOSToolbar (OSMDInfo {osmdOSInfo={osToolbar}})	= osToolbar
getOSDInfoOSToolbar (OSSDInfo {ossdOSInfo={osToolbar}})	= osToolbar
getOSDInfoOSToolbar _									= Nothing

/*	osOSDInfoIsActive tests if the given OSDInfo represents the interactive process with the
	active menu system. (Always True on Windows; use menu bar on Mac.)
*/
osOSDInfoIsActive :: !OSDInfo !*OSToolbox -> (!Bool, !*OSToolbox)
osOSDInfoIsActive osdinfo tb = (True,tb)

/*	getOSDInfoOffset returns the offset vector (dx,dy) that points to the left-top corner of the client area
	of the corresponding infrastructure.
*/
getOSDInfoOffset :: !OSDInfo !*OSToolbox -> (!(!Int,!Int),!*OSToolbox)
getOSDInfoOffset OSNoInfo tb
	= ((0,0),tb)
getOSDInfoOffset osdInfo tb
	# ((dw,dh),tb)	= osStripOuterSize isMDI True tb
	# (outerRect,tb)= osGetProcessWindowDimensions osdInfo tb	// Outer Rect of (M/S)DI frame in screen coordinates
	= ((outerRect.rleft,outerRect.rtop + dh + menuBarH + toolBarH),tb)
where
	(isMDI,osInfo)	= case osdInfo of
						OSMDInfo {osmdOSInfo}	= (True, osmdOSInfo)
						OSSDInfo {ossdOSInfo}	= (False,ossdOSInfo)
						otherwise				= osdocumentinterfaceFatalError "getOSDInfoHeight" "illegally applied to OSNoInfo argument"
	toolBarH		= case osInfo.osToolbar of
						Just {toolbarHeight}	= toolbarHeight
						otherwise				= 0
	menuBarH		= 19		// PA: this should be derived platform independently!
