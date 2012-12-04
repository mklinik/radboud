implementation module toolbar


import StdBool, StdInt, StdMisc
import osbitmap, osdocumentinterface, ostoolbar, ostypes
import commondef, iostate, sdisize, StdProcessAttribute


toolbarFatalError :: String String -> .x
toolbarFatalError function error
	= fatalError function "toolbar" error


openToolbar :: !(IOSt .l) -> IOSt .l
openToolbar ioState
	# (osdInfo,ioState)			= ioStGetOSDInfo ioState
	  di						= getOSDInfoDocumentInterface osdInfo
	| di==NDI
		= ioState
	# (atts,ioState)			= ioStGetProcessAttributes ioState
	  (hasToolbarAtt,toolbarAtt)= cselect isProcessToolbar undef atts
	| not hasToolbarAtt
		= ioState
	# toolbar					= getProcessToolbarAtt toolbarAtt
	| di==SDI
		= openSDIToolbar toolbar osdInfo ioState
	| otherwise
		= openMDIToolbar toolbar osdInfo ioState
where
	openSDIToolbar :: ![ToolbarItem (PSt .l)] !OSDInfo !(IOSt .l) -> IOSt .l
	openSDIToolbar items osdInfo/*(OSSDInfo sdinfo=:{ossdOSInfo=info=:{osFrame,osToolbar}})*/ ioState
		| isJust osToolbar
			= toolbarFatalError "openSDIToolbar" "toolbar already present"
		# (oldSize,_,ioState)	= getSDIWindowSize ioState
		# (tb,ioState)			= getIOToolbox ioState
		# ((tbPtr,tbHeight),tb)	= osCreateToolbar False osFrame (toTuple reqSize) tb
		| tbPtr==OSNoWindowPtr
			= toolbarFatalError "openSDIToolbar" "toolbar could not be created"
		| otherwise
			# (_,tb)			= stateMap2 (openToolbarItem tbPtr) items (1,tb)
			  ostoolbar			= {toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			  osinfo			= {osinfo & osToolbar=Just ostoolbar}
			# ioState			= setIOToolbox tb ioState
			# ioState			= ioStSetOSDInfo (setOSDInfoOSInfo osinfo osdInfo) ioState
			# ioState			= resizeSDIWindow osFrame oldSize {oldSize & h=oldSize.h-tbHeight} ioState
			= ioState
	where
		reqSize					= getBitmapsSize items
		osinfo					= case (getOSDInfoOSInfo osdInfo) of
									Just info -> info
									nothing   -> toolbarFatalError "openSDIToolbar" "could not retrieve OSInfo from OSDInfo"
		{osFrame,osToolbar}		= osinfo
	
	openMDIToolbar :: ![ToolbarItem (PSt .l)] !OSDInfo !(IOSt .l) -> IOSt .l
	openMDIToolbar items osdInfo ioState
		| isJust osToolbar
			= toolbarFatalError "openMDIToolbar" "toolbar already present"
		# (tb,ioState)			= getIOToolbox ioState
		# ((tbPtr,tbHeight),tb)	= osCreateToolbar True osFrame (toTuple reqSize) tb
		| tbPtr==OSNoWindowPtr
			= toolbarFatalError "openMDIToolbar" "toolbar could not be created"
		| otherwise
			# (_,tb)			= stateMap2 (openToolbarItem tbPtr) items (1,tb)
			  ostoolbar			= {toolbarPtr=tbPtr,toolbarHeight=tbHeight}
			  osinfo			= {osinfo & osToolbar=Just ostoolbar}
			# ioState			= setIOToolbox tb ioState
			# ioState			= ioStSetOSDInfo (setOSDInfoOSInfo osinfo osdInfo) ioState
			= ioState
	where
		reqSize					= getBitmapsSize items
		osinfo					= case (getOSDInfoOSInfo osdInfo) of
									Just info -> info
									nothing   -> toolbarFatalError "openMDIToolbar" "could not retrieve OSInfo from OSDInfo"
		{osFrame,osToolbar}		= osinfo

	openToolbarItem	:: !OSToolbarHandle !(ToolbarItem (PSt .l)) !(!Int,!*OSToolbox) -> (!Int,!*OSToolbox)
	openToolbarItem tbPtr (ToolbarItem bitmap tooltip _) (index,tb)
		= (index+1,osCreateBitmapToolbarItem tbPtr (fromBitmap bitmap) index tb)
	openToolbarItem tbPtr ToolbarSeparator (index,tb)
		= (index,osCreateToolbarSeparator tbPtr tb)

getBitmapsSize :: ![ToolbarItem .pst] -> Size
getBitmapsSize items
	= stateMap2 maxBitmapSize items {w=OSdefaultToolbarHeight,h=OSdefaultToolbarHeight}
where
	maxBitmapSize :: !(ToolbarItem .pst) !Size -> Size
	maxBitmapSize item size
		= {w=max itemsize.w size.w,h=max itemsize.h size.h}
	where
		itemsize	= case item of
						ToolbarItem bitmap _ _	-> getBitmapSize bitmap
						_						-> zero
