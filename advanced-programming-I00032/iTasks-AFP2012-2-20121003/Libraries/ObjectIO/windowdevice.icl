implementation module windowdevice


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	osevent, ospicture, osrgn, oswindow
import	commondef, controldraw, controllayout, controlrelayout, controlresize, iostate, processstack, receiveraccess, receiverid, scheduler
import	StdControlAttribute, StdWindowAttribute, windowaccess, windowclipstate, windowdispose, windowdraw, windowevent, windowupdate
from	keyfocus import setNoFocusItem, setNewFocusItem
from	StdPSt   import accPIO


windowdeviceFatalError :: String String -> .x
windowdeviceFatalError function error
	= fatalError function "windowdevice" error


windowFunctions :: DeviceFunctions (PSt .l)
windowFunctions
	= {	dDevice	= WindowDevice
	  ,	dShow	= id //windowShow not yet implemented
	  ,	dHide	= id //windowHide not yet implemented
	  ,	dEvent	= windowEvent
	  ,	dDoIO	= windowIO
	  ,	dOpen	= windowOpen
	  ,	dClose	= windowClose
	  }


/*	windowOpen initialises the window device for this interactive process.
*/
windowOpen :: !(PSt .l) -> PSt .l
windowOpen pState=:{io=ioState}
	# (hasWindow,ioState)			= ioStHasDevice WindowDevice ioState
	| hasWindow
		= {pState & io=ioState}
	| otherwise
		# (ioInterface,ioState)		= ioStGetDocumentInterface ioState
		  (rgnH,ioState)			= accIOToolbox osnewrgn ioState
		  windows					= {	whsWindows		= []
//	/*
									  ,	whsCursorInfo	= {	cInfoChanged	= True
														  ,	cLocalRgn		= rgnH
														  ,	cMouseWasInRgn	= False
														  ,	cLocalShape		= StandardCursor
														  ,	cGlobalSet		= False
														  ,	cGlobalShape	= StandardCursor
														  }
//	*/
									  ,	whsNrWindowBound= case ioInterface of
								  							NDI	-> Finite 0
								  							SDI	-> Finite 1
									  						MDI	-> Infinite
									  ,	whsModal		= False
									  ,	whsFinalModalLS	= []
									  }
		# ioState					= ioStSetDevice (WindowSystemState windows) ioState
		# ioState					= ioStSetDeviceFunctions windowFunctions ioState
		= {pState & io=ioState}


/*	windowClose closes all windows associated with this interactive process. 
	Bindings of (Receiver/Timer)s are undone.
	System resources are released.
	Note that the window device is not removed from the IOSt because there still might be
	a modal dialog which final state has to be retrieved. 
*/
windowClose :: !(PSt .l) -> PSt .l
windowClose pState=:{io=ioState}
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= {pState & io=ioState}
	| otherwise
		# (osdinfo,ioState)		= ioStGetOSDInfo ioState
		  windows				= windowSystemStateGetWindowHandles wDevice
		# (inputTrack,ioState)	= ioStGetInputTrack ioState
		# (tb,ioState)			= getIOToolbox ioState
		  (wsHs,windows)		= (\windows=:{whsWindows}->(whsWindows,{windows & whsWindows=[]})) windows
		# pState				= {pState & io=ioStSetDevice (WindowSystemState windows) ioState}
		# (disposeInfo,(osdinfo,inputTrack,pState,tb))
								= stateMap disposeWindowStateHandle` wsHs (osdinfo,inputTrack,pState,tb)
		# ioState				= setIOToolbox tb pState.io
		# ioState				= ioStSetOSDInfo osdinfo ioState
		# ioState				= ioStSetInputTrack inputTrack ioState
		  (freeRIdss,freeIdss,_,finalLSs)
		  						= unzip4 disposeInfo
		  freeRIds				= flatten freeRIdss
		  freeIds				= flatten freeIdss
		  finalLSs				= flatten finalLSs
		# ioState				= unbindRIds freeRIds ioState
		# (idtable,ioState)		= ioStGetIdTable ioState
		  (_,idtable)			= removeIdsFromIdTable (freeRIds++freeIds) idtable
		# ioState				= ioStSetIdTable idtable ioState
//		  windows				= (\windows=:{whsFinalModalLS=finalLS}->{windows & whsFinalModalLS=finalLS++finalLSs}) windows
//		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
	//	# ioState				= ioStRemoveDeviceFunction WindowDevice ioState		PA: it is not clear whether this should be done
		# ioState				= setFinalLS finalLSs ioState
		# pState				= {pState & io=ioState}
		= pState
where
	disposeWindowStateHandle` :: !*(WindowStateHandle (PSt .l)) !*(!OSDInfo,!Maybe InputTrack,PSt .l,!*OSToolbox)
			   -> (!(![Id],![Id],![DelayActivationInfo],![FinalModalLS]),!*(!OSDInfo,!Maybe InputTrack,PSt .l,!*OSToolbox))
	disposeWindowStateHandle` wsH (osdinfo,inputTrack,state,tb)
		# ((freeRIds,freeIds,delayInfo,finalLS,osdinfo,inputTrack),(_,state),tb) = disposeWindowStateHandle osdinfo inputTrack handleOSEvent (wsH,state) tb
		= ((freeRIds,freeIds,delayInfo,finalLS),(osdinfo,inputTrack,state,tb))
	
	handleOSEvent :: !OSEvent !(PSt .l) -> (![Int],!PSt .l)
	handleOSEvent osEvent pState = accContext (handleContextOSEvent osEvent) pState
	
	setFinalLS :: ![FinalModalLS] !(IOSt .l) -> IOSt .l
	setFinalLS finalLSs ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found
			= windowdeviceFatalError "windowFunctions.dClose" "setFinalLS could not retrieve WindowSystemState from IOSt"
		| otherwise
			# windows				= windowSystemStateGetWindowHandles wDevice
			# windows				= {windows & whsFinalModalLS=windows.whsFinalModalLS++finalLSs}
			= ioStSetDevice (WindowSystemState windows) ioState


/*	windowIO handles the DeviceEvents that have been filtered by windowEvent.
*/
windowIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
windowIO deviceEvent pState
	# (hasDevice,pState)	= accPIO (ioStHasDevice WindowDevice) pState
	| not hasDevice
		= windowdeviceFatalError "windowFunctions.dDoIO" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= windowIO deviceEvent pState
where
	windowIO :: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
	windowIO receiverEvent=:(ReceiverEvent msgEvent) pState
		# (_,wDevice,ioState)		= ioStGetDevice WindowDevice pState.io
		  windows					= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ReceiverEvent _) _" "window could not be found"
		| otherwise
			= (ReceiverEvent msgEvent1,pState2)
		with
			windows1				= setWindowHandlesWindow wsH1 windows
			ioState1				= ioStSetDevice (WindowSystemState windows1) ioState
			pState1					= {pState & io=ioState1}
			(msgEvent1,wsH1,pState2)= windowStateMsgIO msgEvent wsH pState1
	where
		recLoc						= getMsgEventRecLoc msgEvent
		wId							= recLoc.rlParentId
	
	windowIO deviceEvent=:(CompoundScrollAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.csaWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (CompoundScrollAction _) _" "window could not be found"
		| otherwise
			# (wMetrics,ioState)= ioStGetOSWindowMetrics ioState
			# (wsH,ioState)		= accIOToolbox (windowStateCompoundScrollActionIO wMetrics info wsH) ioState
			  windows			= setWindowHandlesWindow wsH windows
			# ioState			= ioStSetDevice (WindowSystemState windows) ioState
			# pState			= {pState & io=ioState}
			= (deviceEvent,pState)
	
	windowIO deviceEvent=:(ControlGetKeyFocus info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.ckfWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlGetKeyFocus _) _" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateControlKeyFocusActionIO True info wsH pState1
	
	windowIO deviceEvent=:(ControlKeyboardAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.ckWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlKeyboardAction _) _" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateControlKeyboardActionIO info wsH pState1
	
	windowIO deviceEvent=:(ControlLooseKeyFocus info) pState
		# (_,wDevice,ioState)			= ioStGetDevice WindowDevice pState.io
		  windows						= windowSystemStateGetWindowHandles wDevice
		  wids							= info.ckfWIDS
		  (found,wsH,windows)			= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlLooseKeyFocus _) _" "window could not be found"
		# (oldInputTrack,ioState)		= ioStGetInputTrack ioState
		  (newInputTrack,lostMouse,lostKey)
		  								= case oldInputTrack of
		  									Just it=:{itWindow,itControl,itKind}
		  											-> if (itWindow==wids.wPtr && itControl==info.ckfItemNr)
		  													(	Nothing
		  													,	if itKind.itkMouse    [createOSLooseMouseEvent itWindow itControl] []
		  													,	if itKind.itkKeyboard [createOSLooseKeyEvent   itWindow itControl] []
		  													)
		  													(oldInputTrack,[],[])
		  									nothing	-> (nothing,[],[])
		# ioState						= ioStSetInputTrack newInputTrack ioState
		  lostInputEvents				= lostMouse ++ lostKey
		| isEmpty lostInputEvents		// no input was being tracked: simply evaluate control deactivate function
			= (deviceEvent,pState2)
		with
			windows1					= setWindowHandlesWindow wsH1 windows
			ioState1					= ioStSetDevice (WindowSystemState windows1) ioState
			pState1						= {pState & io=ioState1}
			(wsH1,pState2)				= windowStateControlKeyFocusActionIO False info wsH pState1
		| otherwise						// handle control deactivate function AFTER lost input events
			# (osDelayEvents,ioState)	= accIOToolbox (strictSeqList (lostInputEvents ++ [createOSDeactivateControlEvent wids.wPtr info.ckfItemPtr])) ioState
			# (osEvents,ioState)		= ioStGetEvents ioState
//			# ioState					= ioStSetEvents (osAppendEvents osDelayEvents osEvents) ioState
			# ioState					= ioStSetEvents (osInsertEvents osDelayEvents osEvents) ioState
			# windows					= setWindowHandlesWindow wsH windows
			# ioState					= ioStSetDevice (WindowSystemState windows) ioState
			# pState					= {pState & io=ioState}
			= (deviceEvent,pState)
	
	windowIO deviceEvent=:(ControlMouseAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.cmWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlMouseAction _) _" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateControlMouseActionIO info wsH pState1
	
	windowIO deviceEvent=:(ControlSelection info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.csWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlSelection _) _" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateControlSelectionIO info wsH pState1
	
	windowIO deviceEvent=:(ControlSliderAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.cslWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (ControlSliderAction _) _" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateControlSliderActionIO info wsH pState1
	
	windowIO deviceEvent=:(WindowActivation wids) pState
		# (processStack,ioState)= ioStGetProcessStack pState.io
		# (ioId,ioState)		= ioStGetIOId ioState
		  processStack			= selectProcessShowState ioId processStack
		# ioState				= ioStSetProcessStack processStack ioState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		  windows				= windowSystemStateGetWindowHandles wDevice
		  wid					= toWID wids.wId
		  (found,wsH,windows)	= getWindowHandlesWindow wid windows
		| not found
			= windowdeviceFatalError "windowIO (WindowActivation _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			(_,_,windows1)		= removeWindowHandlesWindow wid windows			// Remove the placeholder from windows
			windows2			= addWindowHandlesActiveWindow wsH2 windows1	// Place the change window in front of all (modal/modeless) windows
			ioState1			= ioStSetDevice (WindowSystemState windows2) ioState
			pState1				= {pState & io=ioState1}
			wsH1				= {wsH & wshIds={wids & wActive=True}}			// PA: Set active flag
			(wsH2,pState2)		= windowStateActivationIO wsH1 pState1
	
	windowIO deviceEvent=:(WindowDeactivation wids) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowDeactivation _)" "window could not be found"
//		# wsH					= {wsH & wshIds={wids & wActive=False}}	// PA: clear isActive flag
		# (inputTrack,ioState)	= ioStGetInputTrack ioState
		# ioState				= ioStSetInputTrack Nothing ioState		// clear input track information
		  (lostMouse,lostKey)	= case inputTrack of					// check for (Mouse/Key)Lost events
	  								Nothing	-> ([],[])
	  								Just it=:{itWindow,itControl,itKind}
	  										-> (if itKind.itkMouse    [createOSLooseMouseEvent itWindow (if (itControl==0) itWindow itControl)] []
	  										   ,if itKind.itkKeyboard [createOSLooseKeyEvent   itWindow (if (itControl==0) itWindow itControl)] []
	  										   )
		# lostInputEvents		= lostMouse ++ lostKey
		| isEmpty lostInputEvents										// no input was being tracked: simply evaluate deactivate function
			# wsH					= {wsH & wshIds={wids & wActive=False}}	// DvA: moved here!
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateDeactivationIO wsH pState1
		| otherwise														// handle deactivate function AFTER lost input events, but BEFORE other delayed events
			# (osDelayEvents,ioState)
								= accIOToolbox (strictSeqList (lostInputEvents ++ [createOSDeactivateWindowEvent wids.wPtr])) ioState
			# (osEvents,ioState)= ioStGetEvents ioState
			# ioState			= ioStSetEvents (osInsertEvents osDelayEvents osEvents) ioState
			# windows			= setWindowHandlesWindow wsH windows
			# ioState			= ioStSetDevice (WindowSystemState windows) ioState
			# pState			= {pState & io=ioState}
			= (deviceEvent,pState)
	
	windowIO deviceEvent=:(WindowInitialise wids) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowInitialise _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateInitialiseIO wsH pState1
	
	windowIO deviceEvent=:(WindowKeyboardAction action) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID action.wkWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowKeyboardAction _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateWindowKeyboardActionIO action wsH pState1
	
	windowIO deviceEvent=:(WindowMouseAction action) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID action.wmWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowMouseAction _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateWindowMouseActionIO action wsH pState1
	
	windowIO deviceEvent=:(WindowCANCEL wids) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowCANCEL _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateCANCELIO wsH pState1
	
	windowIO deviceEvent=:(WindowOK wids) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowOK _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateOKIO wsH pState1
	
	windowIO deviceEvent=:(WindowRequestClose wids) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wids.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowRequestClose _)" "window could not be found"
		| otherwise
			= (deviceEvent,pState2)
		with
			windows1			= setWindowHandlesWindow wsH1 windows
			ioState1			= ioStSetDevice (WindowSystemState windows1) ioState
			pState1				= {pState & io=ioState1}
			(wsH1,pState2)		= windowStateRequestCloseIO wsH pState1
	
	windowIO deviceEvent=:(WindowScrollAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.wsaWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowScrollAction _)" "window could not be found"
		| otherwise
			# (wMetrics,ioState)= ioStGetOSWindowMetrics ioState
			# (wsH,ioState)		= accIOToolbox (windowStateScrollActionIO wMetrics info wsH) ioState
			  windows			= setWindowHandlesWindow wsH windows
			# ioState			= ioStSetDevice (WindowSystemState windows) ioState
			# pState			= {pState & io=ioState}
			= (deviceEvent,pState)
	
	windowIO deviceEvent=:(WindowSizeAction info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  wId					= info.wsWIDS.wId
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowSizeAction _)" "window could not be found"
		| otherwise
			# (activeWIDS,windows)
								= getWindowHandlesActiveWindow windows
			# (wMetrics,ioState)= ioStGetOSWindowMetrics ioState
			# (tb,ioState)		= getIOToolbox ioState
			# (wsH,tb)			= windowStateSizeAction wMetrics (isJust activeWIDS && (fromJust activeWIDS).wId==wId) info wsH tb
			# ioState			= setIOToolbox tb ioState
			  windows			= setWindowHandlesWindow wsH windows
			# ioState			= ioStSetDevice (WindowSystemState windows) ioState
			# pState			= {pState & io=ioState}
			= (deviceEvent,pState)
	
	windowIO deviceEvent=:(WindowUpdate info) pState
		# (_,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
		  windows				= windowSystemStateGetWindowHandles wDevice
		  (found,wsH,windows)	= getWindowHandlesWindow (toWID info.updWIDS.wId) windows
		| not found
			= windowdeviceFatalError "windowIO (WindowUpdate _)" "window could not be found"
		| otherwise
			# (wMetrics,ioState)= ioStGetOSWindowMetrics ioState
			# (wsH,ioState)		= accIOToolbox (windowStateUpdateIO wMetrics info wsH) ioState
			  windows			= setWindowHandlesWindow wsH windows
			# ioState			= ioStSetDevice (WindowSystemState windows) ioState
			# pState			= {pState & io=ioState}
			= (deviceEvent,pState)
	where
		windowStateUpdateIO :: !OSWindowMetrics !UpdateInfo !(WindowStateHandle .pst) !*OSToolbox
														 -> (!WindowStateHandle .pst, !*OSToolbox)
		windowStateUpdateIO wMetrics info wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
			# (wH,tb)			= updatewindow wMetrics info wH tb
			  wsH				= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
			= (wsH,tb)
		windowStateUpdateIO _ _ _ _
			= windowdeviceFatalError "windowIO (WindowUpdate _) _" "unexpected placeholder argument"
	
	windowIO _ _
		= windowdeviceFatalError "windowIO" "unexpected DeviceEvent"


/*	windowStateMsgIO handles all message events.
*/
windowStateMsgIO :: !MsgEvent !(WindowStateHandle (PSt .l)) (PSt .l)
				-> (!MsgEvent, !WindowStateHandle (PSt .l),  PSt .l)
windowStateMsgIO msgEvent wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= (msgEvent1,{wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	recLoc							= getMsgEventRecLoc msgEvent
	rId								= recLoc.rlReceiverId
	action							= case msgEvent of
										(QASyncMessage msg)	-> windowControlQASyncIO rId msg
										( ASyncMessage msg) -> windowControlASyncIO  rId msg
										(  SyncMessage msg) -> windowControlSyncIO   rId msg
	(msgEvent1,wH1,(ls1,pState1))	= action wH (ls,pState)

	//	windowControlQASyncIO queues an asynchronous message in the message queue of the indicated receiver control.
	windowControlQASyncIO :: !Id !QASyncMessage !(WindowHandle .ls .pst) *(.ls,.pst)
								   -> (!MsgEvent, WindowHandle .ls .pst, *(.ls,.pst))
	windowControlQASyncIO rId msg wH=:{whItems} (ls,pst)
		= (QASyncMessage msg,{wH & whItems=itemHs},(ls,pst))
	where
		(_,itemHs)	= elementsControlQASyncIO rId msg.qasmMsg whItems
		
		elementsControlQASyncIO :: !Id !SemiDynamic ![WElementHandle .ls .pst] -> (!Bool,[WElementHandle .ls .pst])
		elementsControlQASyncIO rId msg [itemH:itemHs]
			# (done,itemH)		= elementControlQASyncIO rId msg itemH
			| done
				= (done,[itemH:itemHs])
			| otherwise
				# (done,itemHs)	= elementsControlQASyncIO rId msg itemHs
				= (done,[itemH:itemHs])
		where
			elementControlQASyncIO :: !Id !SemiDynamic !(WElementHandle .ls .pst) -> (!Bool,WElementHandle .ls .pst)
			elementControlQASyncIO rId msg (WItemHandle itemH=:{wItemId,wItems,wItemInfo,wItemKind})
				| not (identifyMaybeId rId wItemId)
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH)
					// otherwise
						# (done,itemHs)	= elementsControlQASyncIO rId msg wItems
						= (done,WItemHandle {itemH & wItems=itemHs})
				| wItemKind<>IsOtherControl "Receiver" && wItemKind<>IsOtherControl "Receiver2"
					= (True,WItemHandle itemH)
				| otherwise
					# rH	= getWItemReceiverInfo wItemInfo
					# rH	= receiverAddASyncMessage rId msg rH
					# itemH	= {itemH & wItemInfo=ReceiverInfo rH}
					= (True,WItemHandle itemH)
			
			elementControlQASyncIO rId msg (WListLSHandle itemHs)
				# (done,itemHs)			= elementsControlQASyncIO rId msg itemHs
				= (done,WListLSHandle itemHs)
			
			elementControlQASyncIO rId msg (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (done,itemHs)	= elementsControlQASyncIO rId msg itemHs
				= (done,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			elementControlQASyncIO rId msg (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (done,itemHs)		= elementsControlQASyncIO rId msg itemHs
				= (done,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		elementsControlQASyncIO _ _ []
			= (False,[])

	//	windowControlASyncIO handles the first asynchronous message in the message queue of the indicated receiver control.
	windowControlASyncIO :: !Id !ASyncMessage !(WindowHandle .ls .pst) *(.ls,.pst)
								 -> (!MsgEvent, WindowHandle .ls .pst, *(.ls,.pst))
	windowControlASyncIO rId msg wH=:{whItems} (ls,pst)
		= (ASyncMessage msg,{wH & whItems=itemHs},(ls1,pst1))
	where
		(_,itemHs,ls_ps1)	= elementsControlASyncIO rId whItems (ls,pst)
		(ls1,pst1)			= ls_ps1
		
		elementsControlASyncIO :: !Id ![WElementHandle .ls .pst] *(.ls,.pst)
							-> (!Bool, [WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlASyncIO rId [itemH:itemHs] (ls,pst)
			# (done,itemH,(ls,pst))		= elementControlASyncIO rId itemH (ls,pst)
			| done
				= (done,[itemH:itemHs],(ls,pst))
			| otherwise
				# (done,itemHs,(ls,pst))= elementsControlASyncIO rId itemHs (ls,pst)
				= (done,[itemH:itemHs],(ls,pst))
		where
			elementControlASyncIO :: !Id !(WElementHandle .ls .pst) *(.ls,.pst)
								-> (!Bool, WElementHandle .ls .pst, *(.ls,.pst))
			elementControlASyncIO rId (WItemHandle itemH=:{wItemId,wItems,wItemInfo,wItemKind}) (ls,pst)
				| not (identifyMaybeId rId wItemId)
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,(ls,pst))
					// otherwise
						# (done,itemHs,(ls,pst))	= elementsControlASyncIO rId wItems (ls,pst)
						= (done,WItemHandle {itemH & wItems=itemHs},(ls,pst))
				| wItemKind<>IsOtherControl "Receiver" && wItemKind<>IsOtherControl "Receiver2"
					= (True,WItemHandle itemH,(ls,pst))
				| otherwise
					# rH			= getWItemReceiverInfo wItemInfo
					# (rH,(ls,pst))	= receiverASyncIO rH (ls,pst)
					# itemH			= {itemH & wItemInfo=ReceiverInfo rH}
					= (True,WItemHandle itemH,(ls,pst))
			where
				receiverASyncIO :: !(ReceiverHandle .ls .pst) *(.ls,.pst)
								-> ( ReceiverHandle .ls .pst, *(.ls,.pst))
				receiverASyncIO rH=:{rASMQ=[msg:msgs],rFun} (ls,pst)
					# (ls,_,pst)	= rFun msg (ls,pst)
					= ({rH & rASMQ=msgs},(ls,pst))
				receiverASyncIO _ _
					= windowdeviceFatalError "receiverASyncIO" "unexpected empty asynchronous message queue"
			
			elementControlASyncIO rId (WListLSHandle itemHs) (ls,pst)
				# (done,itemHs,(ls,pst))		= elementsControlASyncIO rId itemHs (ls,pst)
				= (done,WListLSHandle itemHs,(ls,pst))
			
			elementControlASyncIO rId (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,pst)
				# (done,itemHs,((extLS,ls),pst))= elementsControlASyncIO rId itemHs ((extLS,ls),pst)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,pst))
			
			elementControlASyncIO rId (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,pst)
				# (done,itemHs,(chLS,pst))		= elementsControlASyncIO rId itemHs (chLS,pst)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,pst))
		
		elementsControlASyncIO _ [] (ls,pst)
			= (False,[],(ls,pst))

	//	windowControlSyncIO lets the indicated receiver control handle the synchronous message.
	windowControlSyncIO :: !Id !SyncMessage !(WindowHandle .ls .pst) *(.ls,.pst)
								-> (!MsgEvent,WindowHandle .ls .pst, *(.ls,.pst))
	windowControlSyncIO r2Id msg wH=:{whItems} ls_ps
		= (SyncMessage msg1,{wH & whItems=itemHs},ls_ps1)
	where
		(_,msg1,itemHs,ls_ps1)	= elementsControlSyncIO r2Id msg whItems ls_ps
		
		elementsControlSyncIO :: !Id !SyncMessage ![WElementHandle .ls .pst] *(.ls,.pst)
						   -> (!Bool,!SyncMessage, [WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlSyncIO r2Id msg [itemH:itemHs] ls_ps
			# (done,msg,itemH,ls_ps)		= elementControlSyncIO r2Id msg itemH ls_ps
			| done
				= (done,msg,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,msg,itemHs,ls_ps)	= elementsControlSyncIO r2Id msg itemHs ls_ps
				= (done,msg,[itemH:itemHs],ls_ps)
		where
			elementControlSyncIO :: !Id !SyncMessage !(WElementHandle .ls .pst) *(.ls,.pst)
							  -> (!Bool,!SyncMessage,  WElementHandle .ls .pst, *(.ls,.pst))
			elementControlSyncIO r2Id msg (WItemHandle itemH=:{wItemId,wItemKind,wItems,wItemInfo}) ls_ps
				| not (identifyMaybeId r2Id wItemId)
					| not (isRecursiveControl wItemKind)
						= (False,msg,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,msg,itemHs,ls_ps)	= elementsControlSyncIO r2Id msg wItems ls_ps
						= (done,msg,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| wItemKind<>IsOtherControl "Receiver" && wItemKind<>IsOtherControl "Receiver2"
					= (True,msg,WItemHandle itemH,ls_ps)
				| otherwise
					# rH			= getWItemReceiverInfo wItemInfo
					# (msg,rH,ls_ps)= receiverSyncIO msg rH ls_ps
					# itemH			= {itemH & wItemInfo=ReceiverInfo rH}
					= (True,msg,WItemHandle itemH,ls_ps)
			where
				receiverSyncIO :: !SyncMessage !(ReceiverHandle .ls .pst) *(.ls,.pst)
							  -> (!SyncMessage,  ReceiverHandle .ls .pst, *(.ls,.pst))
				receiverSyncIO msg rH ls_ps
					# (response,rH,ls_ps)	= receiverHandleSyncMessage msg rH ls_ps
					= ({msg & smResp=response},rH,ls_ps)
			
			elementControlSyncIO r2Id msg (WListLSHandle itemHs) ls_ps
				# (done,msg,itemHs,ls_ps)			= elementsControlSyncIO r2Id msg itemHs ls_ps
				= (done,msg,WListLSHandle itemHs,ls_ps)
			
			elementControlSyncIO r2Id msg (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,pst)
				# (done,msg,itemHs,((extLS,ls),pst))= elementsControlSyncIO r2Id msg itemHs ((extLS,ls),pst)
				= (done,msg,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,pst))
			
			elementControlSyncIO r2Id msg (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,pst)
				# (done,msg,itemHs,(chLS,pst))		= elementsControlSyncIO r2Id msg itemHs (chLS,pst)
				= (done,msg,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,pst))
		
		elementsControlSyncIO _ msg _ ls_ps
			= (False,msg,[],ls_ps)
windowStateMsgIO  _ _ _
	= windowdeviceFatalError "windowStateMsgIO" "unexpected window placeholder"


/*	windowStateCompoundScrollActionIO handles the mouse actions of CompoundControl scrollbars.
*/
windowStateCompoundScrollActionIO :: !OSWindowMetrics !CompoundScrollActionInfo !(WindowStateHandle .pst) !*OSToolbox
																			 -> (!WindowStateHandle .pst, !*OSToolbox)
windowStateCompoundScrollActionIO wMetrics info=:{csaWIDS={wPtr}}
								  wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whKind,whWindowInfo,whItems,whSize,whAtts,whSelect,whShow,whDefaultId}}} 
								  tb
	# (whItems`,whItems,tb)		= getWElementHandles` wPtr whItems tb
	# (done,originChanged,itemHs,tb)
								= calcNewCompoundOrigin wMetrics info zero whItems tb
	| not done
		= windowdeviceFatalError "windowStateCompoundScrollActionIO" "could not locate CompoundControl"
	| not originChanged
		= ({wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}},tb)
	| otherwise
		# (_,newItems,tb)		= layoutControls wMetrics hMargins vMargins spaces contentSize minSize [(domain,origin)] itemHs tb
		  wH					= {wH & whItems=newItems}
		# (wH,tb)				= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,newItems,tb)	= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (wFrame,zero,zero,whItems`) (wFrame,zero,zero,wH.whItems) tb
		# (wH,tb)				= updatewindowbackgrounds wMetrics updRgn info.csaWIDS {wH & whItems=newItems} tb
		# (wH,tb)				= drawcompoundlook wMetrics whSelect wFrame info.csaItemNr wPtr wH tb	// PA: this might be redundant now because of updatewindowbackgrounds
//		# tb					= OSvalidateWindowRect wPtr (sizeToRect whSize) tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo
	(origin,domainRect,hasHScroll,hasVScroll)
								= if (whKind==IsWindow)
									(windowInfo.windowOrigin,windowInfo.windowDomain,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
									(zero,sizeToRect whSize,False,False)
	domain						= rectToRectangle domainRect
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	wFrame						= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
	contentSize					= rectSize wFrame
	(defMinW,defMinH)			= osMinWindowSize
	minSize						= {w=defMinW,h=defMinH}
	hMargins					= getWindowHMargins   whKind wMetrics whAtts
	vMargins					= getWindowVMargins   whKind wMetrics whAtts
	spaces						= getWindowItemSpaces whKind wMetrics whAtts
	
	drawcompoundlook :: !OSWindowMetrics !Bool !OSRect !Int !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
	drawcompoundlook wMetrics ableContext clipRect itemNr wPtr wH=:{whItems} tb
		# (_,itemHs,tb)	= drawcompoundlook` wMetrics ableContext zero clipRect itemNr wPtr whItems tb
		= ({wH & whItems=itemHs},tb)
	where
		drawcompoundlook` :: !OSWindowMetrics !Bool !Point2 !OSRect !Int !OSWindowPtr ![WElementHandle .ls .pst] !*OSToolbox
		                                                                    -> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
		drawcompoundlook` wMetrics ableContext parentPos clipRect itemNr wPtr [itemH:itemHs] tb
			# (done,itemH,tb)		= drawWElementLook wMetrics ableContext parentPos clipRect itemNr wPtr itemH tb
			| done
				= (done,[itemH:itemHs],tb)
			| otherwise
				# (done,itemHs,tb)	= drawcompoundlook` wMetrics ableContext parentPos clipRect itemNr wPtr itemHs tb
				= (done,[itemH:itemHs],tb)
		where
			drawWElementLook :: !OSWindowMetrics !Bool !Point2 !OSRect !Int !OSWindowPtr !(WElementHandle .ls .pst) !*OSToolbox
			                                                                    -> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
			drawWElementLook wMetrics ableContext parentPos clipRect itemNr wPtr (WItemHandle itemH=:{wItemKind,wItemSelect,wItemNr,wItemInfo,wItemPos,wItemSize,wItems}) tb
				| info.csaItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,tb)
					| wItemKind==IsLayoutControl
						# (done,itemHs,tb)	= drawcompoundlook` wMetrics isAble absolutePos (intersectRects clipRect itemRect) itemNr wPtr wItems tb
						  itemH				= {itemH & wItems=itemHs}
						= (done,WItemHandle itemH,tb)
					// otherwise
						# (done,itemHs,tb)	= drawcompoundlook` wMetrics isAble absolutePos clipRect1 itemNr wPtr wItems tb
						  itemH				= {itemH & wItems=itemHs}
						= (done,WItemHandle itemH,tb)
				| wItemKind<>IsCompoundControl
					= windowdeviceFatalError "drawWElementLook (windowStateCompoundScrollActionIO)" "argument control is not a CompoundControl"
				| otherwise
					# (itemH,tb)			= drawCompoundLook wMetrics isAble wPtr parentPos clipRect1 itemH tb
				//	# tb					= OSvalidateWindowRect itemH.wItemPtr clipRect1 tb//(sizeToRect wItemSize) tb	// PA: validation of (SizeToRect wItemSize) is to much
					= (True,WItemHandle itemH,tb)
			where
				absolutePos					= movePoint wItemPos parentPos
				isAble						= ableContext && wItemSelect
				itemInfo					= getWItemCompoundInfo wItemInfo
				domainRect					= itemInfo.compoundDomain
				hasScrolls					= (isJust itemInfo.compoundHScroll,isJust itemInfo.compoundVScroll)
				visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
				itemRect					= posSizeToRect absolutePos wItemSize
				contentRect					= osGetCompoundContentRect wMetrics visScrolls itemRect 
				clipRect1					= intersectRects clipRect contentRect
			
			drawWElementLook wMetrics ableContext parentPos clipRect itemNr wPtr (WListLSHandle itemHs) tb
				# (done,itemHs,tb)	= drawcompoundlook` wMetrics ableContext parentPos clipRect itemNr wPtr itemHs tb
				= (done,WListLSHandle itemHs,tb)
			
			drawWElementLook wMetrics ableContext parentPos clipRect itemNr wPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
				# (done,itemHs,tb)	= drawcompoundlook` wMetrics ableContext parentPos clipRect itemNr wPtr itemHs tb
				= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
			
			drawWElementLook wMetrics ableContext parentPos clipRect itemNr wPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
				# (done,itemHs,tb)	= drawcompoundlook` wMetrics ableContext parentPos clipRect itemNr wPtr itemHs tb
				= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)
		
		drawcompoundlook` _ _ _ _ _ _ [] tb
			= (False,[],tb)
	
	calcNewCompoundOrigin :: !OSWindowMetrics !CompoundScrollActionInfo !Point2 ![WElementHandle .ls .pst] !*OSToolbox
	                                                            -> (!Bool,!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	calcNewCompoundOrigin wMetrics info parentPos [itemH:itemHs] tb
		# (done,changed,itemH,tb)		= calcNewWElementOrigin wMetrics info parentPos itemH tb
		| done
			= (done,changed,[itemH:itemHs],tb)
		| otherwise
			# (done,changed,itemHs,tb)	= calcNewCompoundOrigin wMetrics info parentPos itemHs tb
			= (done,changed,[itemH:itemHs],tb)
	where
		calcNewWElementOrigin :: !OSWindowMetrics !CompoundScrollActionInfo !Point2 !(WElementHandle .ls .pst) !*OSToolbox
		                                                             -> (!Bool,!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		calcNewWElementOrigin wMetrics info parentPos (WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemAtts,wItems,wItemPos,wItemSize=compoundSize,wItemInfo}) tb
			| info.csaItemNr<>wItemNr
				| not (isRecursiveControl wItemKind)
					= (False,False,WItemHandle itemH,tb)
				// otherwise
					# (done,changed,itemHs,tb)	= calcNewCompoundOrigin wMetrics info absolutePos wItems tb
					  itemH						= {itemH & wItems=itemHs}
					= (done,changed,WItemHandle itemH,tb)
			| wItemKind<>IsCompoundControl		// This alternative should never occur
				= windowdeviceFatalError "windowStateCompoundScrollActionIO" "CompoundScrollAction does not correspond with CompoundControl"
			| newThumb==oldThumb
				= (True,False,WItemHandle itemH,tb)
			| otherwise
				# tb							= osSetCompoundSliderThumb wMetrics wPtr wItemPtr itemPtr scrollRect isHorizontal newOSThumb (toTuple compoundSize) True tb
				# itemH							= {itemH & wItemInfo=CompoundInfo {compoundInfo & compoundOrigin=newOrigin}
														 , wItemAtts=replaceOrAppend isControlViewSize (ControlViewSize {w=w,h=h}) wItemAtts
												  }
				= (True,True,WItemHandle itemH,tb)
		where
			absolutePos							= movePoint wItemPos parentPos
			itemPtr								= info.csaItemPtr
			compoundInfo						= getWItemCompoundInfo wItemInfo
			(domainRect,origin,hScroll,vScroll)	= (compoundInfo.compoundDomain,compoundInfo.compoundOrigin,compoundInfo.compoundHScroll,compoundInfo.compoundVScroll)
			(hScrollPtr,vScrollPtr)				= (mscrollptr compoundInfo.compoundHScroll,mscrollptr compoundInfo.compoundVScroll)
			mscrollptr							= mapMaybe (\{scrollItemPtr}->scrollItemPtr)
			visScrolls							= osScrollbarsAreVisible wMetrics domainRect (toTuple compoundSize) (isJust hScroll,isJust vScroll)
			{w,h}								= rectSize (osGetCompoundContentRect wMetrics visScrolls (sizeToRect compoundSize))
			hScrollRect							= osGetCompoundHScrollRect wMetrics visScrolls (posSizeToRect absolutePos compoundSize)
			vScrollRect							= osGetCompoundVScrollRect wMetrics visScrolls (posSizeToRect absolutePos compoundSize)
			scrollRect							= if isHorizontal hScrollRect vScrollRect
			isHorizontal						= info.csaDirection==Horizontal
			scrollInfo							= fromJust (if isHorizontal hScroll vScroll)
			scrollFun							= scrollInfo.scrollFunction
			viewFrame							= posSizeToRectangle origin {w=w,h=h}
			(min`,oldThumb,max`,viewSize)		= if isHorizontal
													(domainRect.rleft,origin.x,domainRect.rright, w)
													(domainRect.rtop, origin.y,domainRect.rbottom,h)
			sliderState							= {sliderMin=min`,sliderThumb=oldThumb,sliderMax=max min` (max`-viewSize)}
			newThumb`							= scrollFun viewFrame sliderState info.csaSliderMove
			newThumb							= setBetween newThumb` min` (max min` (max`-viewSize))
			(_,newOSThumb,_,_)					= toOSscrollbarRange (min`,newThumb,max`) viewSize
			newOrigin							= if isHorizontal {origin & x=newThumb} {origin & y=newThumb}
		
		calcNewWElementOrigin wMetrics info parentPos (WListLSHandle itemHs) tb
			# (done,changed,itemHs,tb)	= calcNewCompoundOrigin wMetrics info parentPos itemHs tb
			= (done,changed,WListLSHandle itemHs,tb)
		
		calcNewWElementOrigin wMetrics info parentPos (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,changed,itemHs,tb)	= calcNewCompoundOrigin wMetrics info parentPos itemHs tb
			= (done,changed,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		calcNewWElementOrigin wMetrics info parentPos (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,changed,itemHs,tb)	= calcNewCompoundOrigin wMetrics info parentPos itemHs tb
			= (done,changed,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)
	
	calcNewCompoundOrigin _ _ _ _ tb
		= (False,False,[],tb)
windowStateCompoundScrollActionIO _ _ _ _
	= windowdeviceFatalError "windowStateCompoundScrollActionIO" "unexpected window placeholder"


/*	windowStateControlKeyFocusActionIO handles the keyboard focus actions of (Compound/Custom/Edit/PopUp)Controls.
	The Bool argument indicates whether the control has obtained key focus (True) or lost key focus (False).
*/
windowStateControlKeyFocusActionIO :: !Bool !ControlKeyFocusInfo !(WindowStateHandle (PSt .l)) !(PSt .l)
															  -> (!WindowStateHandle (PSt .l), PSt .l)
windowStateControlKeyFocusActionIO activated info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowControlKeyFocusActionIO activated info wH (ls,pState)
	
	windowControlKeyFocusActionIO :: !Bool !ControlKeyFocusInfo !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst,*(.ls,.pst))
	windowControlKeyFocusActionIO activated info wH=:{whItems,whKeyFocus} ls_ps
		| activated
			= ({wH & whItems=itemHs,whKeyFocus=setNewFocusItem info.ckfItemNr whKeyFocus},ls_ps1)
		| otherwise
			= ({wH & whItems=itemHs,whKeyFocus=setNoFocusItem whKeyFocus},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsControlKeyFocusActionIO activated info whItems ls_ps
		
		elementsControlKeyFocusActionIO :: !Bool !ControlKeyFocusInfo ![WElementHandle .ls .pst] *(.ls,.pst)
															-> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlKeyFocusActionIO activated info [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementControlKeyFocusActionIO activated info itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsControlKeyFocusActionIO activated info itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementControlKeyFocusActionIO :: !Bool !ControlKeyFocusInfo !(WElementHandle .ls .pst) *(.ls,.pst)
																-> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
			elementControlKeyFocusActionIO activated info (WItemHandle itemH=:{wItemNr,wItemKind,wItems}) ls_ps
				| info.ckfItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,itemHs,ls_ps)	= elementsControlKeyFocusActionIO activated info wItems ls_ps
						= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| otherwise
					# (itemH,ls_ps)	= itemControlKeyFocusActionIO activated itemH ls_ps
					= (True,WItemHandle itemH,ls_ps)
			where
				itemControlKeyFocusActionIO :: !Bool !(WItemHandle .ls .pst) *(.ls,.pst) -> (!WItemHandle .ls .pst,*(.ls,.pst))
				itemControlKeyFocusActionIO activated itemH=:{wItemAtts} ls_pst
					| found			= (itemH,getAtt att ls_pst)
					| otherwise		= (itemH,ls_pst)
				where
					(reqAtt,getAtt)	= if activated
										(isControlActivate,  getControlActivateFun  )
										(isControlDeactivate,getControlDeactivateFun)
					(found,att)		= cselect reqAtt undef wItemAtts
			
			elementControlKeyFocusActionIO activated info (WListLSHandle itemHs) ls_ps
				# (done,itemHs,ls_ps)			= elementsControlKeyFocusActionIO activated info itemHs ls_ps
				= (done,WListLSHandle itemHs,ls_ps)
			
			elementControlKeyFocusActionIO activated info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,ps)
				# (done,itemHs,((extLS,ls),ps))	= elementsControlKeyFocusActionIO activated info itemHs ((extLS,ls),ps)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,ps))
			
			elementControlKeyFocusActionIO activated info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,ps)
				# (done,itemHs,(chLS,ps))		= elementsControlKeyFocusActionIO activated info itemHs (chLS,ps)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,ps))
		
		elementsControlKeyFocusActionIO _ _ _ ls_ps
			= (False,[],ls_ps)
windowStateControlKeyFocusActionIO _ _ _ _
	= windowdeviceFatalError "windowStateControlKeyFocusActionIO" "unexpected window placeholder"


/*	windowStateControlKeyboardActionIO handles the keyboard actions of (PopUp/Custom)Controls and CompoundControls (not yet).
*/
windowStateControlKeyboardActionIO :: !ControlKeyboardActionInfo !(WindowStateHandle (PSt .l)) !(PSt .l)
															  -> (!WindowStateHandle (PSt .l),PSt .l)
windowStateControlKeyboardActionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowControlKeyboardActionIO info wH (ls,pState)
	
	windowControlKeyboardActionIO :: !ControlKeyboardActionInfo !(WindowHandle .ls .pst) *(.ls,.pst)
															 -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowControlKeyboardActionIO info wH=:{whItems} ls_ps
		= ({wH & whItems=itemHs},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsControlKeyboardActionIO info whItems ls_ps
		
		elementsControlKeyboardActionIO :: !ControlKeyboardActionInfo ![WElementHandle .ls .pst] *(.ls,.pst)
															-> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlKeyboardActionIO info [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementControlKeyboardActionIO info itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsControlKeyboardActionIO info itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementControlKeyboardActionIO :: !ControlKeyboardActionInfo !(WElementHandle .ls .pst) *(.ls,.pst)
																-> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
			elementControlKeyboardActionIO info (WItemHandle itemH=:{wItemNr,wItemKind,wItems}) ls_ps
				| info.ckItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,itemHs,ls_ps)	= elementsControlKeyboardActionIO info wItems ls_ps
						= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| otherwise
					# (itemH,ls_ps)	= itemControlKeyboardActionIO info itemH ls_ps
					= (True, WItemHandle itemH,ls_ps)
			where
				itemControlKeyboardActionIO :: !ControlKeyboardActionInfo !(WItemHandle .ls .pst) *(.ls,.pst)
																	   -> (!WItemHandle .ls .pst, *(.ls,.pst))
				itemControlKeyboardActionIO {ckKeyboardState} itemH=:{wItemAtts} ls_ps
					= (itemH,f ckKeyboardState ls_ps)
				where
					(_,_,f)	= getControlKeyboardAtt (snd (cselect isControlKeyboard undef wItemAtts))
			
			elementControlKeyboardActionIO info (WListLSHandle itemHs) ls_ps
				# (done,itemHs,ls_ps)			= elementsControlKeyboardActionIO info itemHs ls_ps
				= (done,WListLSHandle itemHs,ls_ps)
			
			elementControlKeyboardActionIO info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,ps)
				# (done,itemHs,((extLS,ls),ps))	= elementsControlKeyboardActionIO info itemHs ((extLS,ls),ps)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,ps))
			
			elementControlKeyboardActionIO info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,ps)
				# (done,itemHs,(chLS,ps))		= elementsControlKeyboardActionIO info itemHs (chLS,ps)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,ps))
		
		elementsControlKeyboardActionIO _ _ ls_ps
			= (False,[],ls_ps)
windowStateControlKeyboardActionIO _ _ _
	= windowdeviceFatalError "windowStateControlKeyboardActionIO" "unexpected window placeholder"


/*	windowStateControlMouseActionIO handles the mouse actions of CustomControls and CompoundControls (not yet).
*/
windowStateControlMouseActionIO :: !ControlMouseActionInfo !(WindowStateHandle (PSt .l)) (PSt .l)
														-> (!WindowStateHandle (PSt .l),PSt .l)
windowStateControlMouseActionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowControlMouseActionIO info wH (ls,pState)
	
	windowControlMouseActionIO :: !ControlMouseActionInfo !(WindowHandle .ls .pst) *(.ls,.pst)
													   -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowControlMouseActionIO info wH=:{whItems} ls_ps
		= ({wH & whItems=itemHs},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsControlMouseActionIO info whItems ls_ps
		
		elementsControlMouseActionIO :: !ControlMouseActionInfo ![WElementHandle .ls .pst] *(.ls,.pst)
													  -> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlMouseActionIO info [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementControlMouseActionIO info itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsControlMouseActionIO info itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementControlMouseActionIO :: !ControlMouseActionInfo !(WElementHandle .ls .pst) *(.ls,.pst)
														  -> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
			elementControlMouseActionIO info (WItemHandle itemH=:{wItemNr,wItemKind,wItems}) ls_ps
				| info.cmItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,itemHs,ls_ps)	= elementsControlMouseActionIO info wItems ls_ps
						= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| otherwise
					# (itemH,ls_ps)	= itemControlMouseActionIO info itemH ls_ps
					= (True, WItemHandle itemH,ls_ps)
			where
				itemControlMouseActionIO :: !ControlMouseActionInfo !(WItemHandle .ls .pst) *(.ls,.pst)
																 -> (!WItemHandle .ls .pst, *(.ls,.pst))
				itemControlMouseActionIO {cmMouseState} itemH=:{wItemAtts} ls_ps
					= (itemH,f cmMouseState ls_ps)
				where
					(_,_,f)	= getControlMouseAtt (snd (cselect isControlMouse undef wItemAtts))
			
			elementControlMouseActionIO info (WListLSHandle itemHs) ls_ps
				# (done,itemHs,ls_ps)			= elementsControlMouseActionIO info itemHs ls_ps
				= (done,WListLSHandle itemHs,ls_ps)
			
			elementControlMouseActionIO info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,ps)
				# (done,itemHs,((extLS,ls),ps))	= elementsControlMouseActionIO info itemHs ((extLS,ls),ps)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,ps))
			
			elementControlMouseActionIO info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,ps)
				# (done,itemHs,(chLS,ps))		= elementsControlMouseActionIO info itemHs (chLS,ps)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,ps))
		
		elementsControlMouseActionIO _ _ ls_ps
			= (False,[],ls_ps)
windowStateControlMouseActionIO _ _ _
	= windowdeviceFatalError "windowStateControlMouseActionIO" "unexpected window placeholder"


/*	windowStateControlSelectionIO handles the selection of the control.
*/
windowStateControlSelectionIO :: !ControlSelectInfo !(WindowStateHandle (PSt .l)) (PSt .l)
												 -> (!WindowStateHandle (PSt .l),PSt .l)
windowStateControlSelectionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowControlSelectionIO info wH (ls,pState)
	
	windowControlSelectionIO :: !ControlSelectInfo !(WindowHandle .ls .pst) *(.ls,.pst)
												-> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowControlSelectionIO info wH=:{whItems} ls_ps
		# (_,itemHs,ls_ps)	= elementsControlSelectionIO info whItems ls_ps
		= ({wH & whItems=itemHs},ls_ps)
	where
		elementsControlSelectionIO :: !ControlSelectInfo ![WElementHandle .ls .pst] *(.ls,.pst)
											   -> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlSelectionIO info [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementControlSelectionIO info itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsControlSelectionIO info itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementControlSelectionIO :: !ControlSelectInfo !(WElementHandle .ls .pst) *(.ls,.pst)
												   -> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
			elementControlSelectionIO info (WItemHandle itemH=:{wItemNr,wItemKind,wItems}) ls_ps
				| info.csItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,itemHs,ls_ps)	= elementsControlSelectionIO info wItems ls_ps
						= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| otherwise
					# (itemH,ls_ps)	= itemControlSelectionIO info itemH ls_ps
					= (True,WItemHandle itemH,ls_ps)
			where
				itemControlSelectionIO :: !ControlSelectInfo !(WItemHandle .ls .pst) *(.ls,.pst)
														  -> (!WItemHandle .ls .pst, *(.ls,.pst))
				itemControlSelectionIO info itemH=:{wItemKind=IsRadioControl,wItemInfo} (ls,pst)
					# radioInfo		= RadioInfo {radioInfo & radioIndex=index}
					  itemH			= {itemH & wItemInfo=radioInfo}
					= (itemH,f (ls,pst))
				where
					itemPtr			= info.csItemPtr
					radioInfo		= getWItemRadioInfo wItemInfo
					error			= windowdeviceFatalError "windowIO _ (ControlSelection _) _" "RadioControlItem could not be found"
					(index,radio)	= selectedAtIndex (\{radioItemPtr}->radioItemPtr==itemPtr) error radioInfo.radioItems
					f				= thd3 radio.radioItem
				itemControlSelectionIO info itemH=:{wItemKind=IsCheckControl,wItemInfo} (ls,pst)
					# checkInfo		= CheckInfo {checkInfo & checkItems=checks}
					  itemH			= {itemH & wItemInfo=checkInfo}
					= (itemH,f (ls,pst))
				where
					itemPtr			= info.csItemPtr
					checkInfo		= getWItemCheckInfo wItemInfo
					error			= windowdeviceFatalError "windowIO _ (ControlSelection _) _" "CheckControlItem could not be found"
					(_,f,checks)	= access (isCheckItem itemPtr) error checkInfo.checkItems
					
					isCheckItem :: !OSWindowPtr !(CheckItemInfo *(.ls,.pst)) -> *(!*(!Bool,!IdFun *(.ls,.pst)),!CheckItemInfo *(.ls,.pst))
					isCheckItem itemPtr check=:{checkItemPtr,checkItem=(title,width,mark,f)}
						| itemPtr==checkItemPtr
							= ((True,f),{check & checkItem=(title,width,~mark,f)})
						| otherwise
							= ((False,id),check)
				itemControlSelectionIO info itemH=:{wItemKind=IsPopUpControl,wItemInfo} (ls,pst)
					# popUpInfo		= PopUpInfo {popUpInfo & popUpInfoIndex=index}
					  itemH			= {itemH & wItemInfo=popUpInfo}
					= (itemH,f (ls,pst))
				where
					popUpInfo		= getWItemPopUpInfo wItemInfo
					index			= info.csMoreData
					f				= if (index < 0)
										id		// DvA: fix for Editable Popups on Macintosh where edit item has own menu entry...
										(snd (popUpInfo.popUpInfoItems!!(index-1)))
				itemControlSelectionIO info itemH=:{wItemKind,wItemAtts} (ls,pst)
					| wItemKind==IsButtonControl || wItemKind==IsCustomButtonControl
						| hasAtt
							= (itemH,f (ls,pst))
						// otherwise
							= (itemH,  (ls,pst))
					| otherwise
						= (itemH,(ls,pst))
				where
					(hasAtt,fAtt)	= cselect (\att->isControlFunction att || isControlModsFunction att) undef wItemAtts
					f				= case fAtt of
										(ControlFunction     f) -> f
										(ControlModsFunction f) -> f info.csModifiers
										wrongAttribute          -> windowdeviceFatalError "windowStateControlSelectionIO" "argument is not a function attribute"
			
			elementControlSelectionIO info (WListLSHandle itemHs) (ls,pst)
				# (done,itemHs,(ls,pst))		= elementsControlSelectionIO info itemHs (ls,pst)
				= (done,WListLSHandle itemHs,(ls,pst))
			
			elementControlSelectionIO info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,pst)
				# (done,itemHs,((extLS,ls),pst))= elementsControlSelectionIO info itemHs ((extLS,ls),pst)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,pst))
			
			elementControlSelectionIO info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,pst)
				# (done,itemHs,(chLS,pst))		= elementsControlSelectionIO info itemHs (chLS,pst)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,pst))
		
		elementsControlSelectionIO _ _ (ls,pst)
			= (False,[],(ls,pst))
windowStateControlSelectionIO _ _ _
	= windowdeviceFatalError "windowStateControlSelectionIO" "unexpected window placeholder"


/*	windowStateControlSliderActionIO handles the slider of windows/dialogs.
*/
windowStateControlSliderActionIO :: !ControlSliderInfo !(WindowStateHandle (PSt .l)) (PSt .l)
													-> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateControlSliderActionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowControlSliderAction info wH (ls,pState)
	
	windowControlSliderAction :: !ControlSliderInfo !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst,*(.ls,.pst))
	windowControlSliderAction info wH=:{whItems} ls_ps
		= ({wH & whItems=itemHs},ls_ps1)
	where
		(_,itemHs,ls_ps1)	= elementsControlSliderActionIO info whItems ls_ps
		
		elementsControlSliderActionIO :: !ControlSliderInfo ![WElementHandle .ls .pst] *(.ls,.pst)
												  -> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
		elementsControlSliderActionIO info [itemH:itemHs] ls_ps
			# (done,itemH,ls_ps)		= elementControlSliderActionIO info itemH ls_ps
			| done
				= (done,[itemH:itemHs],ls_ps)
			| otherwise
				# (done,itemHs,ls_ps)	= elementsControlSliderActionIO info itemHs ls_ps
				= (done,[itemH:itemHs],ls_ps)
		where
			elementControlSliderActionIO :: !ControlSliderInfo !(WElementHandle .ls .pst) *(.ls,.pst)
													  -> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
			elementControlSliderActionIO info (WItemHandle itemH=:{wItemNr,wItemKind,wItems}) ls_ps
				| info.cslItemNr<>wItemNr
					| not (isRecursiveControl wItemKind)
						= (False,WItemHandle itemH,ls_ps)
					// otherwise
						# (done,itemHs,ls_ps)	= elementsControlSliderActionIO info wItems ls_ps
						= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
				| otherwise
					# (itemH,ls_ps)	= itemControlSliderActionIO info itemH ls_ps
					= (True,WItemHandle itemH,ls_ps)
			where
				itemControlSliderActionIO :: !ControlSliderInfo !(WItemHandle .ls .pst) *(.ls,.pst)
															 -> (!WItemHandle .ls .pst, *(.ls,.pst))
				itemControlSliderActionIO info itemH=:{wItemKind=IsSliderControl,wItemInfo} ls_ps
					= (itemH,f info.cslSliderMove ls_ps)
				where
					f	= (getWItemSliderInfo wItemInfo).sliderInfoAction
				itemControlSliderActionIO _ itemH ls_ps
					= (itemH,ls_ps)
			
			elementControlSliderActionIO info (WListLSHandle itemHs) ls_ps
				# (done,itemHs,ls_ps)			= elementsControlSliderActionIO info itemHs ls_ps
				= (done,WListLSHandle itemHs,ls_ps)
			
			elementControlSliderActionIO info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,pst)
				# (done,itemHs,((extLS,ls),pst))= elementsControlSliderActionIO info itemHs ((extLS,ls),pst)
				= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,pst))
			
			elementControlSliderActionIO info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,pst)
				# (done,itemHs,(chLS,pst))		= elementsControlSliderActionIO info itemHs (chLS,pst)
				= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,pst))
		
		elementsControlSliderActionIO _ _ ls_ps
			= (False,[],ls_ps)
windowStateControlSliderActionIO _ _ _
	= windowdeviceFatalError "windowStateControlSliderActionIO" "unexpected window placeholder"


/*	windowStateActivationIO handles the activation of the window/dialog.
*/
windowStateActivationIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
						-> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateActivationIO wsH=:{wshIds=wids,wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshIds={wids & wActive=True},wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowActivationIO wH (ls,pState)
	
	windowActivationIO :: !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowActivationIO wH=:{whAtts} ls_pst
		| hasAtt			= (wH,f ls_pst)
		| otherwise			= (wH,  ls_pst)
	where
		(hasAtt,activateAtt)= cselect isWindowActivate undef whAtts
		f					= getWindowActivateFun activateAtt
windowStateActivationIO _ _
	= windowdeviceFatalError "windowStateActivationIO" "unexpected window placeholder"


/*	windowStateDeactivationIO handles the deactivation of the window/dialog.
*/
windowStateDeactivationIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
						  -> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateDeactivationIO wsH=:{wshIds=wids,wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshIds={wids & wActive=False},wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowDeactivationIO wH (ls,pState)
	
	windowDeactivationIO :: !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowDeactivationIO wH=:{whAtts} ls_pst
		| hasAtt				= (wH,f ls_pst)
		| otherwise				= (wH,  ls_pst)
	where
		(hasAtt,deactivateAtt)	= cselect isWindowDeactivate undef whAtts
		f						= getWindowDeactivateFun deactivateAtt
windowStateDeactivationIO _ _
	= windowdeviceFatalError "windowStateDeactivationIO" "unexpected window placeholder"


/*	windowStateInitialiseIO handles the initialisation of the window/dialog.
*/
windowStateInitialiseIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
						-> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateInitialiseIO wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowInitialiseIO wH (ls,pState)
	
	windowInitialiseIO :: !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowInitialiseIO wH=:{whAtts} ls_pst
		| hasAtt				= (wH1,f ls_pst)
		| otherwise				= (wH1,  ls_pst)
	where
		(hasAtt,initAtt,atts)	= remove isWindowInit undef whAtts
		wH1						= {wH & whAtts=atts}
		f						= getWindowInitFun initAtt
windowStateInitialiseIO _ _
	= windowdeviceFatalError "windowStateInitialiseIO" "unexpected window placeholder"


/*	windowStateWindowKeyboardActionIO handles the keyboard for the window.
*/
windowStateWindowKeyboardActionIO :: !WindowKeyboardActionInfo !(WindowStateHandle (PSt .l)) (PSt .l)
															-> (!WindowStateHandle (PSt .l),PSt .l)
windowStateWindowKeyboardActionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowKeyboardActionIO info.wkKeyboardState wH (ls,pState)
	
	windowKeyboardActionIO :: !KeyboardState !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst,*(.ls,.pst))
	windowKeyboardActionIO key wH=:{whAtts} ls_ps
		= (wH,f key ls_ps)
	where
		(_,_,f) = getWindowKeyboardAtt (snd (cselect isWindowKeyboard undef whAtts))
windowStateWindowKeyboardActionIO _ _ _
	= windowdeviceFatalError "windowStateWindowKeyboardActionIO" "unexpected window placeholder"


/*	windowStateWindowMouseActionIO handles the mouse for the window.
*/
windowStateWindowMouseActionIO :: !WindowMouseActionInfo !(WindowStateHandle (PSt .l)) (PSt .l)
													  -> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateWindowMouseActionIO info wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))	= windowMouseActionIO info.wmMouseState wH (ls,pState)
	
	windowMouseActionIO :: !MouseState !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowMouseActionIO mouse wH=:{whAtts} ls_pst
		= (wH,f mouse ls_pst)
	where
		(_,_,f) = getWindowMouseAtt (snd (cselect isWindowMouse undef whAtts))
windowStateWindowMouseActionIO _ _ _
	= windowdeviceFatalError "windowStateWindowMouseActionIO" "unexpected window placeholder"


/*	windowButtonActionIO id wH st
		evaluates the button function of the (Custom)ButtonControl associated with id.
	This function is used by windowStateCANCELIO and windowStateOKIO.
*/
windowButtonActionIO :: !Id !(WindowHandle .ls .pst) *(.ls,.pst) -> (!WindowHandle .ls .pst,*(.ls,.pst))
windowButtonActionIO buttonId wH=:{whItems} ls_ps
	= ({wH & whItems=itemHs},ls_ps1)
where
	(_,itemHs,ls_ps1)	= elementsButtonActionIO buttonId whItems ls_ps
	
	elementsButtonActionIO :: !Id ![WElementHandle .ls .pst] *(.ls,.pst)
						-> (!Bool,![WElementHandle .ls .pst],*(.ls,.pst))
	elementsButtonActionIO id [itemH:itemHs] ls_ps
		# (done,itemH,ls_ps)		= elementButtonActionIO id itemH ls_ps
		| done
			= (done,[itemH:itemHs],ls_ps)
		| otherwise
			# (done,itemHs,ls_ps)	= elementsButtonActionIO id itemHs ls_ps
			= (done,[itemH:itemHs],ls_ps)
	where
		elementButtonActionIO :: !Id !(WElementHandle .ls .pst) *(.ls,.pst)
							-> (!Bool,!WElementHandle .ls .pst, *(.ls,.pst))
		elementButtonActionIO id (WItemHandle itemH=:{wItemId,wItemKind,wItems}) ls_ps
			| isNothing wItemId || fromJust wItemId<>id
				| not (isRecursiveControl wItemKind)
					= (False,WItemHandle itemH,ls_ps)
				// otherwise
					# (done,itemHs,ls_ps)	= elementsButtonActionIO id wItems ls_ps
					= (done,WItemHandle {itemH & wItems=itemHs},ls_ps)
			| otherwise
				# (itemH,ls_ps)	= itemButtonActionIO id itemH ls_ps
				= (True,WItemHandle itemH,ls_ps)
		where
			itemButtonActionIO :: !Id !(WItemHandle .ls .pst) *(.ls,.pst)
								   -> (!WItemHandle .ls .pst, *(.ls,.pst))
			itemButtonActionIO id itemH=:{wItemKind,wItemAtts} ls_ps
				| wItemKind<>IsButtonControl && wItemKind<>IsCustomButtonControl
					= windowdeviceFatalError "windowButtonActionIO" "Id argument does not refer to (Custom)ButtonControl"
				| hasFunAtt
					= (itemH,getControlFun fAtt ls_ps)
				| otherwise
					= (itemH,ls_ps)
			where
				(hasFunAtt,fAtt)	= cselect isControlFunction undef wItemAtts
		
		elementButtonActionIO info (WListLSHandle itemHs) ls_ps
			# (done,itemHs,ls_ps)			= elementsButtonActionIO info itemHs ls_ps
			= (done,WListLSHandle itemHs,ls_ps)
		
		elementButtonActionIO info (WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs}) (ls,pst)
			# (done,itemHs,((extLS,ls),pst))= elementsButtonActionIO info itemHs ((extLS,ls),pst)
			= (done,WExtendLSHandle {wExtendLS=extLS,wExtendItems=itemHs},(ls,pst))
		
		elementButtonActionIO info (WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs}) (ls,pst)
			# (done,itemHs,(chLS,pst))		= elementsButtonActionIO info itemHs (chLS,pst)
			= (done,WChangeLSHandle {wChangeLS=chLS,wChangeItems=itemHs},(ls,pst))
	
	elementsButtonActionIO _ _ ls_ps
		= (False,[],ls_ps)


/*	windowStateCANCELIO handles the evaluation of the Cancel button.
*/
windowStateCANCELIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
					-> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateCANCELIO wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH=:{whCancelId=maybeId}}} pState
	| isNothing maybeId
		= windowdeviceFatalError "windowStateCANCELIO" "no Cancel button administrated"
	| otherwise
		= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
	with
		(wH1,(ls1,pState1))	= windowButtonActionIO (fromJust maybeId) wH (ls,pState)
windowStateCANCELIO _ _
	= windowdeviceFatalError "windowStateCANCELIO" "unexpected window placeholder"


/*	windowStateOKIO handles the evaluation of the Ok button.
*/
windowStateOKIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
				-> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateOKIO wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH=:{whDefaultId=maybeId}}} pState
	| isNothing maybeId
		= windowdeviceFatalError "windowStateOKIO" "no Ok button administrated"
	| otherwise
		= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
	with
		(wH1,(ls1,pState1))	= windowButtonActionIO (fromJust maybeId) wH (ls,pState)
windowStateOKIO _ _
	= windowdeviceFatalError "windowStateOKIO" "unexpected window placeholder"


/*	windowStateRequestCloseIO handles the request to close the window/dialog.
*/
windowStateRequestCloseIO :: !(WindowStateHandle (PSt .l)) (PSt .l)
						  -> (!WindowStateHandle (PSt .l),  PSt .l)
windowStateRequestCloseIO wsH=:{wshHandle=Just wlsH=:{wlsState=ls,wlsHandle=wH}} pState
	= ({wsH & wshHandle=Just {wlsH & wlsState=ls1,wlsHandle=wH1}},pState1)
where
	(wH1,(ls1,pState1))		= windowRequestCloseIO wH (ls,pState)
	
	windowRequestCloseIO :: !(WindowHandle .ls .pst) *(.ls,.pst)
						 -> (!WindowHandle .ls .pst, *(.ls,.pst))
	windowRequestCloseIO wH=:{whAtts} ls_ps
		| hasAtt			= (wH,f ls_ps)
		| otherwise			= (wH,  ls_ps)
	where
		(hasAtt,closeAtt)	= cselect isWindowClose undef whAtts
		f					= getWindowCloseFun closeAtt
windowStateRequestCloseIO _ _
	= windowdeviceFatalError "windowStateRequestCloseIO" "unexpected window placeholder"


/*	windowStateScrollActionIO handles the mouse action of window scrollbars.
*/
windowStateScrollActionIO :: !OSWindowMetrics !WindowScrollActionInfo !(WindowStateHandle .pst) !*OSToolbox
																   -> (!WindowStateHandle .pst, !*OSToolbox)
windowStateScrollActionIO wMetrics info wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}} tb
	# (wH,tb)	= windowScrollActionIO wMetrics info wH tb
	= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	windowScrollActionIO :: !OSWindowMetrics !WindowScrollActionInfo !(WindowHandle .ls .pst) !*OSToolbox
																  -> (!WindowHandle .ls .pst, !*OSToolbox)
	windowScrollActionIO wMetrics info=:{wsaWIDS={wPtr}} wH=:{whWindowInfo,whItems=oldItems,whSize,whAtts,whSelect,whShow,whDefaultId} tb
		| newThumb==oldThumb
			= (wH,tb)
		| otherwise
			# (_,newOSThumb,_,_)		= toOSscrollbarRange (min`,newThumb,max`) viewSize
			  newOrigin					= if isHorizontal {oldOrigin & x=newThumb} {oldOrigin & y=newThumb}
			# tb						= osSetWindowSliderThumb wMetrics wPtr isHorizontal newOSThumb hScroll vScroll hRect vRect (toTuple whSize) True tb
			# (oldItems`,oldItems,tb)	= getWElementHandles` wPtr oldItems tb
			# (_,newItems,tb)			= layoutControls wMetrics hMargins vMargins spaces contentSize minSize [(domain,newOrigin)] oldItems tb
			  wH						= {	wH & whWindowInfo	= WindowInfo {windowInfo & windowOrigin=newOrigin}
						  					   , whItems		= newItems
										  }
			# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
			  (whWindowInfo,wH)			= (\wH=:{whWindowInfo}->(whWindowInfo,wH)) wH
			# (isRect,areaRect,tb)		= case whWindowInfo of
			  								WindowInfo {windowClip={clipRgn}} -> osgetrgnbox clipRgn tb
			  								_                                 -> windowdeviceFatalError "windowScrollActionIO" "unexpected whWindowInfo field"
			# (updRgn,newItems,tb)		= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (contentRect,zero,zero,oldItems`) (contentRect,zero,zero,wH.whItems) tb
			# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn info.wsaWIDS {wH & whItems=newItems} tb
			  newFrame					= posSizeToRectangle newOrigin contentSize
			  toMuch					= if isHorizontal
			  								(abs (newOrigin.x-oldOrigin.x)>=w`)
			  								(abs (newOrigin.y-oldOrigin.y)>=h`)
			  (updArea,updAction)		= if (not lookInfo.lookSysUpdate || toMuch || not isRect)
			  								(newFrame,return [])
			  								(calcScrollUpdateArea oldOrigin newOrigin areaRect)
			  updState					= {oldFrame=oldFrame,newFrame=newFrame,updArea=[updArea]}
			# (wH,tb)					= drawwindowlook` wMetrics wPtr updAction updState wH tb
			# tb						= osValidateWindowRect wPtr (sizeToRect whSize) tb
			= (wH,tb)
	where
		windowInfo						= getWindowInfoWindowData whWindowInfo
		(oldOrigin,domainRect,hasHScroll,hasVScroll,lookInfo)
										= (windowInfo.windowOrigin,windowInfo.windowDomain,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll,windowInfo.windowLook)
		hScroll							= if hasHScroll (Just (fromJust windowInfo.windowHScroll).scrollItemPtr) Nothing
		vScroll							= if hasVScroll (Just (fromJust windowInfo.windowVScroll).scrollItemPtr) Nothing
		isHorizontal					= info.wsaDirection==Horizontal
		domain							= rectToRectangle domainRect
		visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
		contentRect						= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
		hRect							= osGetWindowHScrollRect wMetrics visScrolls (sizeToRect whSize)
		vRect							= osGetWindowVScrollRect wMetrics visScrolls (sizeToRect whSize)
		contentSize						= rectSize contentRect
		{w=w`,h=h`}						= contentSize
		oldFrame						= posSizeToRectangle oldOrigin contentSize

		(min`,oldThumb,max`,viewSize)	= if isHorizontal
											(domain.corner1.x,oldOrigin.x,domain.corner2.x,w`)
											(domain.corner1.y,oldOrigin.y,domain.corner2.y,h`)

		sliderState						= {sliderMin=min`,sliderThumb=oldThumb,sliderMax=max`-viewSize}

		scrollInfo						= fromJust (if isHorizontal windowInfo.windowHScroll windowInfo.windowVScroll)
		scrollFun						= scrollInfo.scrollFunction
		newThumb`						= scrollFun oldFrame sliderState info.wsaSliderMove
		newThumb						= setBetween newThumb` min` (max`-viewSize)
		(defMinW,defMinH)				= osMinWindowSize
		minSize							= {w=defMinW,h=defMinH}
		hMargins						= getWindowHMargins   IsWindow wMetrics whAtts
		vMargins						= getWindowVMargins   IsWindow wMetrics whAtts
		spaces							= getWindowItemSpaces IsWindow wMetrics whAtts
		
	/*	calcScrollUpdateArea p1 p2 area calculates the new Rectangle that has to be updated. 
		Assumptions: p1 is the origin before scrolling,
		             p2 is the origin after  scrolling,
		             area is the visible area of the window view frame,
		             scrolling occurs either horizontally or vertically.
	*/
		calcScrollUpdateArea :: !Point2 !Point2 !OSRect -> (!Rectangle,!St *Picture [OSRect])
		calcScrollUpdateArea oldOrigin newOrigin areaRect
			= (updArea,scroll {newOriginAreaRect & rright=rright+1,rbottom=rbottom+1} restArea v)
		where
			newOriginAreaRect			= addVector (toVector newOrigin) areaRect
			{rleft,rtop,rright,rbottom}	= newOriginAreaRect
			newOriginAreaRectangle		= rectToRectangle newOriginAreaRect
			v							= toVector (oldOrigin-newOrigin)
			{vx,vy}						= v
			(updArea,restArea)			= if (vx<0) ({newOriginAreaRectangle & corner1={x=rright+vx,y=rtop}},      {newOriginAreaRect & rright =rright +vx})
										 (if (vx>0) ({newOriginAreaRectangle & corner2={x=rleft+vx, y=rbottom}},   {newOriginAreaRect & rleft  =rleft  +vx})
										 (if (vy<0) ({newOriginAreaRectangle & corner1={x=rleft,    y=rbottom+vy}},{newOriginAreaRect & rbottom=rbottom+vy})
										 (if (vy>0) ({newOriginAreaRectangle & corner2={x=rright,   y=rtop+vy}},   {newOriginAreaRect & rtop   =rtop   +vy})
										            (windowdeviceFatalError "calcScrollUpdateArea (scrolling window)" "assumption violation"))))
			
			scroll :: !OSRect !OSRect !Vector2 !*Picture -> (![OSRect],!*Picture)
			scroll scrollRect restRect v picture
				# (updRect,picture)	= pictscroll scrollRect v picture
				| updRect==zero
					= ([],picture)
				| otherwise
					= ([restRect],picture)

windowStateScrollActionIO _ _ _ _
	= windowdeviceFatalError "windowStateScrollActionIO" "unexpected window placeholder"


/*	windowStateSizeAction handles resizing a window and its controls.
*/
windowStateSizeAction :: !OSWindowMetrics !Bool !WindowSizeActionInfo !(WindowStateHandle .pst) !*OSToolbox
																   -> (!WindowStateHandle .pst, !*OSToolbox)
windowStateSizeAction wMetrics isActive info=:{wsWIDS={wPtr},wsSize,wsUpdateAll} wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whSelect}}} tb
	#  oldVisScrolls	= osScrollbarsAreVisible wMetrics domainRect oldSize` (hasHScroll,hasVScroll)
	   oldContent		= osGetWindowContentRect wMetrics oldVisScrolls (sizeToRect oldSize)
	   oldContentSize	= rectSize oldContent

	   domainSize		= rectSize domainRect
	   newOrigin		= {	x = if (w>=domainSize.w) domainRect.rleft (setBetween oldOrigin.x domainRect.rleft (domainRect.rright -w))
	  					  ,	y = if (h>=domainSize.h) domainRect.rtop  (setBetween oldOrigin.y domainRect.rtop  (domainRect.rbottom-h))
	  					  }
	   newVisScrolls	= osScrollbarsAreVisible wMetrics domainRect newSize` (hasHScroll,hasVScroll)
	   newSizeRect		= sizeToRect wsSize
	   newContent		= osGetWindowContentRect wMetrics newVisScrolls newSizeRect
	   newContentSize	= rectSize newContent
	   newHRect			= osGetWindowHScrollRect wMetrics newVisScrolls newSizeRect
	   newVRect			= osGetWindowVScrollRect wMetrics newVisScrolls newSizeRect
	   newHScroll		= newScroll oldHScroll newHRect
	   newVScroll		= newScroll oldVScroll newVRect
	#! winfo			= { windowInfo & windowOrigin  = newOrigin
							           , windowHScroll = newHScroll
							           , windowVScroll = newVScroll
						  }
	   newWindowInfo	= WindowInfo winfo
	   resizedAtt		= WindowViewSize newContentSize
	   (replaced,atts)	= creplace isWindowViewSize resizedAtt wH.whAtts
	   resizedAtts		= if replaced atts [resizedAtt:atts]
	#! wH				= {wH & whSize=wsSize,whWindowInfo=newWindowInfo,whAtts=resizedAtts}
	#  osHState			= toOSscrollbarRange (domainRect.rleft,newOrigin.x,domainRect.rright)  newContentSize.w
	#  osVState			= toOSscrollbarRange (domainRect.rtop, newOrigin.y,domainRect.rbottom) newContentSize.h
	#  tb				= setwindowslider hasHScroll wMetrics wPtr True  (newOrigin.x == oldOrigin.x) osHState newHRect newVRect newSize` tb
	#  tb				= setwindowslider hasVScroll wMetrics wPtr False (newOrigin.y == oldOrigin.y) osVState newHRect newVRect newSize` tb
	#  (wH,tb)			= resizeControls wMetrics isActive wsUpdateAll info.wsWIDS oldOrigin oldContentSize newContentSize wH tb
	#! wlsH				= {wlsH & wlsHandle=wH}
	   wsH				= {wsH & wshHandle=Just wlsH}
	=  (wsH,tb)
where
	oldSize				= wH.whSize
	oldSize`			= toTuple oldSize
	newSize`			= toTuple wsSize
	(w,h)				= newSize`

	windowInfo			= getWindowInfoWindowData wH.whWindowInfo
	(oldOrigin,domainRect,hasHScroll,hasVScroll)
						= (windowInfo.windowOrigin,windowInfo.windowDomain,isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
	hScroll				= if hasHScroll (Just (fromJust windowInfo.windowHScroll).scrollItemPtr) Nothing
	vScroll				= if hasVScroll (Just (fromJust windowInfo.windowVScroll).scrollItemPtr) Nothing
	(oldHScroll,oldVScroll)
						= ( windowInfo.windowHScroll, windowInfo.windowVScroll)
	
	newScroll Nothing _ = Nothing
	newScroll (Just scroll_inf=:{scrollItemPtr}) {rleft,rtop,rright,rbottom}
		= Just {scroll_inf & scrollItemPos = {x=rleft,y=rtop},scrollItemSize = {w = rright - rleft, h = rbottom - rtop}}

	newOrigin :: !Point2 !OSRect !Size -> Point2
	newOrigin {x,y} {rleft,rtop,rright,rbottom} {w,h}
		= {x=x`,y=y`}
	where
		x`	= if (x+w>rright)  (max (rright -w) rleft) x
		y`	= if (y+h>rbottom) (max (rbottom-h) rtop ) y
	
	setwindowslider :: !Bool !OSWindowMetrics !OSWindowPtr !Bool !Bool !(!Int,!Int,!Int,!Int) !OSRect !OSRect !(!Int,!Int) !*OSToolbox -> *OSToolbox
	setwindowslider hasScroll wMetrics wPtr isHorizontal thumbChanged (osMin,osThumb,osMax,osSize) hRect vRect maxcoords tb
		| not hasScroll	= tb
		# tb			= osSetWindowSliderPosSize wPtr sPtr (if isHorizontal hRect vRect) tb
		# tb			= osSetWindowSliderThumbSize wMetrics wPtr sPtr isHorizontal osMin osMax osSize maxcoords (if isHorizontal hRect vRect) whSelect thumbChanged tb
		| thumbChanged		= tb
		| otherwise	
			= osSetWindowSliderThumb wMetrics wPtr isHorizontal osThumb hScroll vScroll hRect vRect maxcoords True tb
	where
		sPtr = case isHorizontal of
				True	-> fromJust hScroll
				_		-> fromJust vScroll

windowStateSizeAction _ _ _ _ _
	= windowdeviceFatalError "windowIO _ (WindowSizeAction _) _" "unexpected placeholder argument"
