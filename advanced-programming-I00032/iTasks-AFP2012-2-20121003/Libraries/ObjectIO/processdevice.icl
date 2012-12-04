implementation module processdevice

import StdBool, StdFunc, StdMisc
import StdPSt
import commondef, devicefunctions, menudevice, menuwindowmenu, processevent, StdProcessAttribute, toolbar


processdeviceFatalError :: String String -> .x
processdeviceFatalError rule error
	= fatalError rule "processdevice" error


processFunctions :: DeviceFunctions (PSt .l)
processFunctions
	= {	dDevice	= ProcessDevice
	  ,	dShow	= id//processShow
	  ,	dHide	= id//processHide
	  ,	dEvent	= processEvent
	  ,	dDoIO	= processIO
	  ,	dOpen	= processOpen
	  ,	dClose	= processClose
	  }

processOpen :: !(PSt .l) -> PSt .l
processOpen pState=:{io=ioState}
	# (hasProcess,ioState)			= ioStHasDevice ProcessDevice ioState
	| hasProcess
		= {pState & io=ioState}
	| otherwise
		# ioState					= appIOToolbox osInitialiseDI ioState
		# ioState					= ioStSetDeviceFunctions processFunctions ioState
		# (osdinfo,ioState)			= ioStGetOSDInfo ioState
		# ioState					= createOSDInfo osdinfo ioState
		# pState					= {pState & io=ioState}
//	PA: openWindowMenu will ensure that menu device has been created.
//		# pState					= menuFunctions.dOpen pState	// DvA: ensure process menubar exists
		# pState					= menuFunctions.dShow pState	// DvA: ensure process menubar visible
		# pState					= case (getOSDInfoDocumentInterface osdinfo) of	// DvA: for an MDI process open the Window menu
										MDI		-> openWindowMenu pState
										_		-> pState
		= pState
where
	createOSDInfo :: !OSDInfo !(IOSt .l) -> IOSt .l
	createOSDInfo emptyOSDInfo ioState
		| di==NDI
			# (tb,ioState)	= getIOToolbox ioState
			# (osdinfo,tb)	= osOpenNDI tb
			# ioState		= ioStSetOSDInfo osdinfo ioState
			# ioState		= setIOToolbox tb ioState
			= ioState
		# (atts,ioState)	= ioStGetProcessAttributes ioState
		  acceptOpenFiles	= contains isProcessOpenFiles atts
		# (tb,ioState)		= getIOToolbox ioState
		| di==MDI
			# hasToolbarAtt	= contains isProcessToolbar   atts
			# (osdinfo,tb)	= osOpenMDI (not hasToolbarAtt) acceptOpenFiles tb
			# ioState		= setIOToolbox tb ioState
			# ioState		= ioStSetOSDInfo osdinfo ioState
			# ioState		= openToolbar ioState
			= ioState
		| di==SDI
			# (osdinfo,tb)	= osOpenSDI acceptOpenFiles tb
			# ioState		= setIOToolbox tb ioState
			# ioState		= ioStSetOSDInfo osdinfo ioState
			# ioState		= openToolbar ioState
			= ioState
	where
		di					= getOSDInfoDocumentInterface emptyOSDInfo

processClose :: !(PSt .l) -> PSt .l
processClose pState=:{io=ioState}
	# (_,_,ioState)	= ioStGetDevice ProcessDevice ioState
	# ioState		= ioStRemoveDeviceFunctions ProcessDevice ioState
	= {pState & io=ioState}

processIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)

processIO deviceEvent=:ProcessRequestClose pState
	# (atts,pState)		= accPIO ioStGetProcessAttributes pState
	  (hasCloseAtt,att)	= cselect isProcessClose undef atts
	| not hasCloseAtt
		= (deviceEvent,pState)
	| otherwise
		= (deviceEvent,getProcessCloseFun att pState)

processIO deviceEvent=:(ProcessRequestOpenFiles openFilesInfo) pState
	# (atts,pState)			= accPIO ioStGetProcessAttributes pState
	  (hasFilesOpenAtt,att)	= cselect isProcessOpenFiles undef atts
	| not hasFilesOpenAtt
		= (deviceEvent,pState)
	| otherwise
		= (deviceEvent,getProcessOpenFilesFun att openFilesInfo pState)

processIO deviceEvent=:(ProcessRequestClipboardChanged) pState
	# (atts,pState)			= accPIO ioStGetProcessAttributes pState
	  (hasClipChangeAtt,att)= cselect isProcessClipboardChanged undef atts
	| not hasClipChangeAtt
		= (deviceEvent,pState)
	| otherwise
		= (deviceEvent,getProcessClipboardChangedFun att pState)

processIO _ _
	= processdeviceFatalError "processIO" "unexpected DeviceEvent"
