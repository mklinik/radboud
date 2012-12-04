implementation module windowevent


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	clCCall_12, clCrossCall_12, windowCrossCall_12
from	ostypes				import	OSNoWindowPtr
from	oswindow			import	fromOSscrollbarRange, osScrollbarsAreVisible
import	commondef, controlcreate, deviceevents, iostate, windowaccess
from	StdControlAttribute	import	isControlKeyboard, getControlKeyboardAtt, 
									isControlMouse,    getControlMouseAtt, 
									isControlActivate, isControlDeactivate
from	StdPSt				import	accPIO
from	StdWindowAttribute	import	isWindowKeyboard,  getWindowKeyboardAtt,
									isWindowMouse,     getWindowMouseAtt,
									isWindowCursor,    getWindowCursorAtt
from	windowcreate		import	createModalDialogControls


windoweventFatalError :: String String -> .x
windoweventFatalError function error
	= fatalError function "windowevent" error


/*	windowEvent filters the scheduler events that can be handled by this window device.
	For the time being no timer controls are added, so these events are ignored.
	windowEvent assumes that it is not applied to an empty IOSt.
*/
windowEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
windowEvent schedulerEvent pState
	# (hasDevice,pState)	= accPIO (ioStHasDevice WindowDevice) pState
	| not hasDevice			// This condition should never occur: WindowDevice must have been 'installed'
		= windoweventFatalError "windowFunctions.dEvent" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= windowEvent schedulerEvent pState
where
	windowEvent :: !SchedulerEvent !(PSt .l) -> (!Bool,!Maybe DeviceEvent,!SchedulerEvent,!PSt .l)
	windowEvent schedulerEvent=:(ScheduleOSEvent osEvent _) pState=:{io=ioState}
		| not (isWindowOSEvent osEvent.ccMsg)
			= (False,Nothing,schedulerEvent,pState)
		| otherwise
			# (_,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
			# (wMetrics, ioState)	= ioStGetOSWindowMetrics ioState
			  windows				= windowSystemStateGetWindowHandles wDevice
			  (myEvent,replyToOS,deviceEvent,windows,ioState)
			  						= filterOSEvent wMetrics osEvent windows ioState
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			# pState				= {pState & io=ioState}
			  schedulerEvent		= if (isJust replyToOS) (ScheduleOSEvent osEvent (fromJust replyToOS)) schedulerEvent
			= (myEvent,deviceEvent,schedulerEvent,pState)
	where
		isWindowOSEvent :: !Int -> Bool
		isWindowOSEvent CcWmACTIVATE		= True
		isWindowOSEvent CcWmBUTTONCLICKED	= True
		isWindowOSEvent CcWmCLOSE			= True
		isWindowOSEvent CcWmCOMBOSELECT		= True
		isWindowOSEvent CcWmDEACTIVATE		= True
		isWindowOSEvent CcWmDRAWCONTROL		= True
		isWindowOSEvent CcWmIDLEDIALOG		= True
		isWindowOSEvent CcWmINITDIALOG		= True
		isWindowOSEvent CcWmKEYBOARD		= True
		isWindowOSEvent CcWmKILLFOCUS		= True
		isWindowOSEvent CcWmLOSTKEY			= True
		isWindowOSEvent CcWmLOSTMOUSE		= True
		isWindowOSEvent CcWmMOUSE			= True
		isWindowOSEvent CcWmMOUSEWHEEL		= True
		isWindowOSEvent CcWmPAINT			= True
		isWindowOSEvent CcWmSCROLLBARACTION	= True
		isWindowOSEvent CcWmSETFOCUS		= True
		isWindowOSEvent CcWmSIZE			= True
		isWindowOSEvent CcWmSPECIALBUTTON	= True
		isWindowOSEvent _					= False
	
	windowEvent schedulerEvent=:(ScheduleMsgEvent msgEvent) pState=:{io=ioState}
		# (ioId,ioState)		= ioStGetIOId ioState
		| ioId<>recLoc.rlIOId || recLoc.rlDevice<>WindowDevice
			= (False,Nothing,schedulerEvent,{pState & io=ioState})
		| otherwise
			# (_,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
			  windows				= windowSystemStateGetWindowHandles wDevice
			  (found,windows)		= hasWindowHandlesWindow (toWID recLoc.rlParentId) windows
			  deviceEvent			= if found (Just (ReceiverEvent msgEvent)) Nothing
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			# pState				= {pState & io=ioState}
			= (found,deviceEvent,schedulerEvent,pState)
	where
		recLoc						= getMsgEventRecLoc msgEvent
	
	windowEvent schedulerEvent pState
		= (False,Nothing,schedulerEvent,pState)


/*	filterOSEvent filters the OSEvents that can be handled by this window device.
*/
filterOSEvent :: !OSWindowMetrics !OSEvent !(WindowHandles (PSt .l)) !(IOSt .l)
  -> (!Bool,!Maybe [Int],!Maybe DeviceEvent,!WindowHandles (PSt .l),  !IOSt .l)

filterOSEvent _ {ccMsg=CcWmBUTTONCLICKED,p1=wPtr,p2=cPtr,p3=mods,p4=toolbarIndex} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,  wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getControlsItemNr cPtr wsH
		  controlSelectInfo	= if (itemNr==0)	// itemNrs are always > 0
	  							Nothing
	  							(Just (ControlSelection {csWIDS		= wids
														,csItemNr	= itemNr
														,csItemPtr	= cPtr
														,csMoreData	= 0
														,csModifiers= toModifiers mods
														})
								)
		= (True,Nothing,controlSelectInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getControlsItemNr :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Int,!WindowStateHandle .pst)
	getControlsItemNr cPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
		# (_,itemNr,itemHs)	= getControlsItemNr cPtr whItems
		= (itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
	where
		getControlsItemNr :: !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Int,![WElementHandle .ls .pst])
		getControlsItemNr cPtr [itemH:itemHs]
			# (found,itemNr,itemH)				= getControlItemNr cPtr itemH
			| found
				= (found,itemNr,[itemH:itemHs])
			| otherwise
				# (found,itemNr,itemHs)			= getControlsItemNr cPtr itemHs
				= (found,itemNr,[itemH:itemHs])
		where
			getControlItemNr :: !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Int,!WElementHandle .ls .pst)
			getControlItemNr cPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemInfo,wItemKind,wItemSelect,wItemShow,wItems})
				| cPtr==wItemPtr			= (True,itemNr,WItemHandle itemH)
				| wItemKind==IsRadioControl	= (contains (\{radioItemPtr}->radioItemPtr==cPtr) (getWItemRadioInfo wItemInfo).radioItems,itemNr,WItemHandle itemH)
				| wItemKind==IsCheckControl	= (contains (\{checkItemPtr}->checkItemPtr==cPtr) (getWItemCheckInfo wItemInfo).checkItems,itemNr,WItemHandle itemH)
				| wItemSelect && wItemShow
					# (found,itemNr,itemHs)	= getControlsItemNr cPtr wItems
					= (found,itemNr,WItemHandle {itemH & wItems=itemHs})
				| otherwise
					= (False,0,WItemHandle itemH)
			where
				itemNr						= if wItemSelect wItemNr 0
			
			getControlItemNr cPtr (WListLSHandle itemHs)
				# (found,itemNr,itemHs)		= getControlsItemNr cPtr itemHs
				= (found,itemNr,WListLSHandle itemHs)
			
			getControlItemNr cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,itemNr,itemHs)		= getControlsItemNr cPtr itemHs
				= (found,itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			getControlItemNr cPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,itemNr,itemHs)		= getControlsItemNr cPtr itemHs
				= (found,itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		getControlsItemNr _ []
			= (False,0,[])
	
	getControlsItemNr _ _
		= windoweventFatalError "getControlsItemNr" "window placeholder not expected"

filterOSEvent _ {ccMsg=CcWmCOMBOSELECT,p1=wPtr,p2=cPtr,p3=index} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,  wsH)		= getWindowStateHandleWIDS wsH
		  (itemNr,wsH)		= getPopUpControlItemNr cPtr wsH
		  controlSelectInfo	= if (itemNr==0)	// itemNrs are always > 0
								Nothing
								(Just (ControlSelection {csWIDS		= wids
														,csItemNr	= itemNr
														,csItemPtr	= cPtr
														,csMoreData	= index+1
														,csModifiers= NoModifiers
														})
								)
		= (True,Nothing,controlSelectInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getPopUpControlItemNr :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Int,!WindowStateHandle .pst)
	getPopUpControlItemNr cPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
		# (_,itemNr,itemHs)				= getPopUpControlsItemNr cPtr whItems
		= (itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
	where
		getPopUpControlsItemNr :: !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Int,![WElementHandle .ls .pst])
		getPopUpControlsItemNr cPtr [itemH:itemHs]
			# (found,itemNr,itemH)		= getPopUpControlItemNr cPtr itemH
			| found
				= (found,itemNr,[itemH:itemHs])
			| otherwise
				# (found,itemNr,itemHs)	= getPopUpControlsItemNr cPtr itemHs
				= (found,itemNr,[itemH:itemHs])
		where
			getPopUpControlItemNr :: !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Int,!WElementHandle .ls .pst)
			getPopUpControlItemNr cPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemSelect,wItemShow,wItems})
				| cPtr==wItemPtr
					= (True,if (wItemKind==IsPopUpControl && wItemSelect && wItemShow) wItemNr 0,WItemHandle itemH)
				| wItemShow
					# (found,itemNr,itemHs)	= getPopUpControlsItemNr cPtr wItems
					= (found,itemNr,WItemHandle {itemH & wItems=itemHs})
				| otherwise
					= (False,0,WItemHandle itemH)
			
			getPopUpControlItemNr cPtr (WListLSHandle itemHs)
				# (found,itemNr,itemHs)	= getPopUpControlsItemNr cPtr itemHs
				= (found,itemNr,WListLSHandle itemHs)
			
			getPopUpControlItemNr cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,itemNr,itemHs)	= getPopUpControlsItemNr cPtr itemHs
				= (found,itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			getPopUpControlItemNr cPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,itemNr,itemHs)	= getPopUpControlsItemNr cPtr itemHs
				= (found,itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		getPopUpControlsItemNr _ []
			= (False,0,[])
	
	getPopUpControlItemNr _ _
		= windoweventFatalError "getPopUpControlItemNr" "window placeholder not expected"

filterOSEvent _ {ccMsg=CcWmDRAWCONTROL,p1=wPtr,p2=cPtr,p3=gc} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (controls,wsH)	= getUpdateControls cPtr wsH
		  updateInfo		= if (isEmpty controls)
								Nothing
								(Just (WindowUpdate {updWIDS=wids,updWindowArea=zero,updControls=controls,updGContext=Just gc}))
		= (True,Nothing,updateInfo,setWindowHandlesWindow wsH windows,ioState)
where
	getUpdateControls :: !OSWindowPtr !(WindowStateHandle .pst) -> (![ControlUpdateInfo],!WindowStateHandle .pst)
	getUpdateControls cPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems,whSize}}}
		# (_,controls,itemHs)				= getUpdateControls cPtr zero (sizeToRect whSize) whItems
		= (controls,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
	where
		getUpdateControls :: !OSWindowPtr !Point2 !OSRect ![WElementHandle .ls .pst] -> (!Bool,![ControlUpdateInfo],![WElementHandle .ls .pst])
		getUpdateControls cPtr parentPos clipRect [itemH:itemHs]
			# (found,controls,itemH)		= getUpdateControl cPtr parentPos clipRect itemH
			| found
				= (found,controls,[itemH:itemHs])
			| otherwise
				# (found,controls,itemHs)	= getUpdateControls cPtr parentPos clipRect itemHs
				= (found,controls,[itemH:itemHs])
		where
			getUpdateControl :: !OSWindowPtr !Point2 !OSRect !(WElementHandle .ls .pst) -> (!Bool,![ControlUpdateInfo],!WElementHandle .ls .pst)
			getUpdateControl cPtr parentPos clipRect (WItemHandle itemH=:{wItemPtr,wItemNr,wItemShow,wItemPos,wItemSize,wItems})
				| cPtr==wItemPtr
					= (True,[{cuItemNr=wItemNr,cuItemPtr=wItemPtr,cuArea=clipRect1}],WItemHandle itemH)
				| wItemShow
					# (found,controls,itemHs)	= getUpdateControls cPtr absolutePos clipRect1 wItems
					= (found,controls,WItemHandle {itemH & wItems=itemHs})
				| otherwise
					= (False,[],WItemHandle itemH)
			where
				absolutePos						= movePoint wItemPos parentPos
				clipRect1						= intersectRects clipRect (posSizeToRect absolutePos wItemSize)
			
			getUpdateControl cPtr parentPos clipRect (WListLSHandle itemHs)
				# (found,controls,itemHs)		= getUpdateControls cPtr parentPos clipRect itemHs
				= (found,controls,WListLSHandle itemHs)
			
			getUpdateControl cPtr parentPos clipRect (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,controls,itemHs)		= getUpdateControls cPtr parentPos clipRect itemHs
				= (found,controls,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			getUpdateControl cPtr parentPos clipRect (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,controls,itemHs)		= getUpdateControls cPtr parentPos clipRect itemHs
				= (found,controls,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		getUpdateControls _ _ _ []
			= (False,[],[])
	
	getUpdateControls _ _
		= windoweventFatalError "getUpdateControls" "placeholder not expected"

/*	PA: CcWmIDLEDIALOG is sent after a modal dialogue and its controls have been created.
		At that moment the initialisation action can be evaluated. This is done by the 
		WindowInitialise device event. 
*/
filterOSEvent _ {ccMsg=CcWmIDLEDIALOG,p1=wPtr} windows ioState
	# (maybeWIDS,windows)		= getWindowHandlesActiveModalDialog windows
	| isNothing maybeWIDS
		= (False,Nothing,Nothing,windows,ioState)
	# wids						= fromJust maybeWIDS
	| wPtr<>wids.wPtr
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		= (True,Nothing,Just (WindowInitialise (fromJust maybeWIDS)),windows,ioState)

/*	PA:	CcWmINITDIALOG is generated for modal and modeless dialogs. It should create all the controls of the
		dialog, and return the desired position, size, and focus control of the dialog.
	PA: THE FOLLOWING STATEMENT IS NOT TRUE; FUNCTIONALITY MOVED TO CcWmIDLEDIALOG.
		In addition, the return DeviceEvent should be WindowInitialise to have the initialisation
		function evaluated.
*/
/*	PA: previous version. Now code is shared in windowcreate and windowevent.
filterOSEvent wMetrics {ccMsg=CcWmINITDIALOG,p1=wPtr} windows ioState
	# (maybeWIDS,windows)		= getWindowHandlesActiveWindow windows
	| isNothing maybeWIDS
		= (False,Nothing,Nothing,windows,ioState)
	# wids						= fromJust maybeWIDS
	| wids.wPtr<>0
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (_,wsH,windows)		= removeWindowHandlesWindow (toWID 0) windows
		  wids					= {wids & wPtr=wPtr}
		  wsH					= (\wsH->{wsH & wshIds=wids}) wsH
		# (tb,ioState)			= getIOToolbox ioState
		# (returnOS,wsH,tb)		= createDialogControls wMetrics wsH tb
		# ioState				= setIOToolbox tb ioState
		  windows				= addWindowHandlesActiveWindow wsH windows
		= (True,Just returnOS,Nothing,windows,ioState)
where
	createDialogControls :: !OSWindowMetrics !(WindowStateHandle .pst) !*OSToolbox
								  -> (![Int], !WindowStateHandle .pst, !*OSToolbox)
	createDialogControls wMetrics wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=itemHs,whSize={w,h}}}} tb
		# (itemHs,tb)			= createControls wMetrics whDefaultId whCancelId True wPtr itemHs tb
		# (itemPtr,wH)			= getInitActiveControl {wH & whItems=itemHs}
		  r5cci					= [-1,-1,w,h,if (itemPtr==OSNoWindowPtr) 0 itemPtr]
		= (r5cci,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		whDefaultId				= wH.whDefaultId
		whCancelId				= wH.whCancelId
	createDialogControls _ _ _
		= windoweventFatalError "createDialogControls" "placeholder not expected"
*/
filterOSEvent wMetrics {ccMsg=CcWmINITDIALOG,p1=wPtr} windows ioState
	# (maybeWIDS,windows)	= getWindowHandlesActiveWindow windows
	| isNothing maybeWIDS
		= (False,Nothing,Nothing,windows,ioState)
	# wids					= fromJust maybeWIDS
	| wids.wPtr<>0
		= (False,Nothing,Nothing,windows,ioState)
	# (tb,ioState)			= getIOToolbox ioState
	# (itemPtr,windows,tb)	= createModalDialogControls wMetrics wPtr windows tb
	# ioState				= setIOToolbox tb ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found				// This alternative can't be reached, because createModalDialogControls has added this handle
		= windoweventFatalError "filterOSEvent (CcWmINITDIALOG)" "could not retrieve window"
	| otherwise
		# ({w,h},wsH)		= getWindowStateHandleSize wsH
		  windows			= setWindowHandlesWindow wsH windows
		= (True,Just [-1,-1,w,h,if (itemPtr==OSNoWindowPtr) 0 itemPtr],Nothing,windows,ioState)

filterOSEvent _ {ccMsg=CcWmSCROLLBARACTION,p1=wPtr,p2=cPtr,p3=iBar,p4=action,p5=osThumb} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  (sliderEvent,wsH)	= getSlidersEvent wids iBar osThumb cPtr wsH
		= (True,Nothing,Just sliderEvent,setWindowHandlesWindow wsH windows,ioState)
where
	getSlidersEvent :: !WIDS !Int !Int !OSWindowPtr !(WindowStateHandle .pst) -> (!DeviceEvent,!WindowStateHandle .pst)
	getSlidersEvent wids iBar osThumb itemPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whWindowInfo,whItems,whSize={w,h}}}}
		| wids.wPtr==itemPtr
			= (WindowScrollAction info,wsH)
		with
			info				= {	wsaWIDS			= wids
								  ,	wsaSliderMove	= move min max view osThumb
								  ,	wsaDirection	= if isHorizontal Horizontal Vertical
								  }
			windowInfo			= getWindowInfoWindowData whWindowInfo
			domainRect			= windowInfo.windowDomain
			isHorizontal		= iBar==SB_HORZ
			(min,max,view)		= if isHorizontal
									(domainRect.rleft,domainRect.rright, w)
									(domainRect.rtop, domainRect.rbottom,h)
		# (found,sliderEvent,itemHs)= getSlidersEvent wids iBar osThumb itemPtr whItems
		| found
			= (sliderEvent,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
		| otherwise
			= windoweventFatalError "getSlidersEvent" "SliderControl could not be located"
	where
		getSlidersEvent :: !WIDS !Int !Int !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!DeviceEvent,![WElementHandle .ls .pst])
		getSlidersEvent wids iBar osThumb itemPtr [itemH:itemHs]
			# (found,sliderEvent,itemH)		= getSliderEvent wids iBar osThumb itemPtr itemH
			| found
				= (found,sliderEvent,[itemH:itemHs])
			| otherwise
				# (found,sliderEvent,itemHs)= getSlidersEvent wids iBar osThumb itemPtr itemHs
				= (found,sliderEvent,[itemH:itemHs])
		where
			getSliderEvent :: !WIDS !Int !Int !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!DeviceEvent,!WElementHandle .ls .pst)
			getSliderEvent wids iBar osThumb itemPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemShow,wItems,wItemInfo,wItemSize})
				| itemPtr<>wItemPtr
					| wItemShow
						# (found,sliderEvent,itemHs)	= getSlidersEvent wids iBar osThumb itemPtr wItems
						= (found,sliderEvent,WItemHandle {itemH & wItems=itemHs})
					// otherwise
						= (False,ControlSliderAction dummySlidersEvent,WItemHandle itemH)
				| wItemKind==IsCompoundControl
					= (True,CompoundScrollAction info,WItemHandle itemH)
				with
					info			= {	csaWIDS			= wids
									  ,	csaItemNr		= wItemNr
									  ,	csaItemPtr		= itemPtr
									  ,	csaSliderMove	= move min max view osThumb
									  ,	csaDirection	= if isHorizontal Horizontal Vertical
									  }
					compoundInfo	= getWItemCompoundInfo wItemInfo
					domainRect		= compoundInfo.compoundDomain
					isHorizontal	= iBar==SB_HORZ
					(min,max,view)	= if isHorizontal
										(domainRect.rleft,domainRect.rright, wItemSize.w)
										(domainRect.rtop, domainRect.rbottom,wItemSize.h)
				| otherwise
					= (True,ControlSliderAction info,WItemHandle itemH)
				with
					info			= {	cslWIDS			= wids
									  ,	cslItemNr		= wItemNr
									  ,	cslItemPtr		= itemPtr
									  ,	cslSliderMove	= move sliderState.sliderMin sliderState.sliderMax 0 osThumb
									  }
					sliderInfo		= getWItemSliderInfo wItemInfo
					sliderState		= sliderInfo.sliderInfoState
			
			getSliderEvent wids iBar osThumb itemPtr (WListLSHandle itemHs)
				# (found,sliderEvent,itemHs)	= getSlidersEvent wids iBar osThumb itemPtr itemHs
				= (found,sliderEvent,WListLSHandle itemHs)
			
			getSliderEvent wids iBar osThumb itemPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,sliderEvent,itemHs)	= getSlidersEvent wids iBar osThumb itemPtr itemHs
				= (found,sliderEvent,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			getSliderEvent wids iBar osThumb itemPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,sliderEvent,itemHs)	= getSlidersEvent wids iBar osThumb itemPtr itemHs
				= (found,sliderEvent,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		getSlidersEvent _ _ _ _ []
			= (False,ControlSliderAction dummySlidersEvent,[])
		
		dummySlidersEvent	= { cslWIDS=wids,cslItemNr=0,cslItemPtr=0,cslSliderMove=SliderIncSmall }
	
	getSlidersEvent _ _ _ _ _
		= windoweventFatalError "getSlidersEvent" "placeholder not expected"
	
	move :: !Int !Int !Int !Int -> SliderMove
	move min max view osThumb
		= case action of
			SB_LINEUP		-> SliderDecSmall
			SB_LINEDOWN		-> SliderIncSmall
			SB_PAGEUP		-> SliderDecLarge
			SB_PAGEDOWN		-> SliderIncLarge
			SB_THUMBPOSITION-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)
			SB_THUMBTRACK	-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)
			SB_TOP			-> SliderThumb min
			SB_BOTTOM		-> SliderThumb (max-view)
			SB_ENDSCROLL	-> SliderThumb (fromOSscrollbarRange (min,max) osThumb)

//	PA: new alternative to handle mouse wheel events.
filterOSEvent _ {ccMsg=CcWmMOUSEWHEEL,p1=wPtr,p2=cPtr,p3=dir} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)			= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  (sliderEvent,wsH)	= getScrollEvent` wids cPtr dir wsH
		= (True,Nothing,sliderEvent,setWindowHandlesWindow wsH windows,ioState)
where
	getScrollEvent` :: !WIDS !Int !Int !(WindowStateHandle .pst) -> (!Maybe DeviceEvent,!WindowStateHandle .pst)
	getScrollEvent` wids itemPtr dir wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whWindowInfo,whItems,whSize={w,h}}}}
		| wids.wPtr==itemPtr
			# windowInfo	= getWindowInfoWindowData whWindowInfo
			| isNothing windowInfo.windowVScroll		// There is no vertical scroll bar
				= (Nothing,wsH)
			| otherwise
				= (Just (WindowScrollAction info),wsH)	// There is a vertical scroll bar
		with
			info			= {	wsaWIDS			= wids
							  ,	wsaSliderMove	= if (dir==0) SliderDecSmall SliderIncSmall
							  ,	wsaDirection	= Vertical
							  }
		# (found,sliderEvent,itemHs)= getScrollEvent wids dir itemPtr whItems
		| found
			= (sliderEvent,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
		| otherwise
			= windoweventFatalError "getScrollEvent" "SliderControl could not be located"
	where
		getScrollEvent :: !WIDS !Int !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Maybe DeviceEvent,![WElementHandle .ls .pst])
		getScrollEvent wids dir itemPtr [itemH:itemHs]
			# (found,sliderEvent,itemH)		= getSliderEvent wids dir itemPtr itemH
			| found
				= (found,sliderEvent,[itemH:itemHs])
			| otherwise
				# (found,sliderEvent,itemHs)= getScrollEvent wids dir itemPtr itemHs
				= (found,sliderEvent,[itemH:itemHs])
		where
			getSliderEvent :: !WIDS !Int !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Maybe DeviceEvent,!WElementHandle .ls .pst)
			getSliderEvent wids dir itemPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemShow,wItems,wItemInfo,wItemSize})
				| itemPtr<>wItemPtr
					| wItemShow
						# (found,sliderEvent,itemHs)	= getScrollEvent wids dir itemPtr wItems
						= (found,sliderEvent,WItemHandle {itemH & wItems=itemHs})
					| otherwise
						= (False,Nothing,WItemHandle itemH)
				| wItemKind==IsCompoundControl
					# compoundInfo	= getWItemCompoundInfo wItemInfo
					| isNothing compoundInfo.compoundVScroll	// There is no vertical scroll bar
						= (True,Nothing,WItemHandle itemH)
					| otherwise
						= (True,Just (CompoundScrollAction info),WItemHandle itemH)
				with
					info			= {	csaWIDS			= wids
									  ,	csaItemNr		= wItemNr
									  ,	csaItemPtr		= itemPtr
									  ,	csaSliderMove	= if (dir==0) SliderDecSmall SliderIncSmall
									  ,	csaDirection	= Vertical
									  }
				| otherwise
					= (True,Just (ControlSliderAction info),WItemHandle itemH)
				with
					info			= {	cslWIDS			= wids
									  ,	cslItemNr		= wItemNr
									  ,	cslItemPtr		= itemPtr
									  ,	cslSliderMove	= if (dir==0) SliderDecSmall SliderIncSmall
									  }
			
			getSliderEvent wids dir itemPtr (WListLSHandle itemHs)
				# (found,sliderEvent,itemHs)	= getScrollEvent wids dir itemPtr itemHs
				= (found,sliderEvent,WListLSHandle itemHs)
			
			getSliderEvent wids dir itemPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,sliderEvent,itemHs)	= getScrollEvent wids dir itemPtr itemHs
				= (found,sliderEvent,WExtendLSHandle {wExH & wExtendItems=itemHs})
			
			getSliderEvent wids dir itemPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,sliderEvent,itemHs)	= getScrollEvent wids dir itemPtr itemHs
				= (found,sliderEvent,WChangeLSHandle {wChH & wChangeItems=itemHs})
		
		getScrollEvent _ _ _ []
			= (False,Nothing,[])
	
	getScrollEvent` _ _ _ _
		= windoweventFatalError "getScrollEvent`" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmACTIVATE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (active,wsH)				= getWindowStateHandleActive wsH
	| active				// The window is already active, skip
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
//		  (activeModal,windows)	= getWindowHandlesActiveModalDialog windows
//		= (True,Nothing,if (isJust activeModal) (Just (WindowInitialise wids)) (Just (WindowActivation wids)),windows,ioState)	// PA: WindowInitialise? Why? Doesn't smell well
		= (True,Nothing,Just (WindowActivation wids),windows,ioState)	// DvA: always activate/deactivate windows

filterOSEvent _ {ccMsg=CcWmCLOSE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (WindowRequestClose wids),windows,ioState)

filterOSEvent _ {ccMsg=CcWmDEACTIVATE,p1=wPtr} windows ioState
	# (found,wsH,windows)		= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
//	PA: in my version this test was not present.
	# (active,wsH)				= getWindowStateHandleActive wsH
	| not active				// The window is already inactive, skip
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
//  ...PA
	| otherwise
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
//		  (activeModal,windows)	= getWindowHandlesActiveModalDialog windows
//		= (True,Nothing,if (isJust activeModal) Nothing (Just (WindowDeactivation wids)),windows,ioState)
		= (True,Nothing,Just (WindowDeactivation wids),windows,ioState)	// DvA: always activate/deactivate windows

filterOSEvent _ {ccMsg=CcWmKEYBOARD,p1=wPtr,p2=cPtr,p3=keycode,p4=state,p5=mods} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr					// The keyboard action takes place in the window
		# (inputTrack,ioState)		= ioStGetInputTrack ioState
		  (ok,key,wsH,inputTrack)	= okWindowKeyboardState keycode state mods wsH inputTrack
		# ioState					= ioStSetInputTrack inputTrack ioState
		  deviceEvent				= if ok (Just (WindowKeyboardAction {wkWIDS=wids,wkKeyboardState=key})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowKeyboardState :: !Int !Int !Int !(WindowStateHandle .pst) !(Maybe InputTrack)
						 -> (!Bool,KeyboardState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okWindowKeyboardState keycode state mods wsH=:{wshHandle=Just {wlsHandle={whKind,whWindowInfo,whAtts}}} inputTrack
			| whKind==IsDialog
				= (False,undef,wsH,inputTrack)
			| trackingKeyboard wPtr 0 inputTrack								// Window is already handle Key(Repeat/Up)
				| isDownKey														// Ignore all key down events
					= (False,undef,wsH,inputTrack)
				| pressState==KeyUp
					= (okKeyboardAtt,keystate,wsH,untrackKeyboard inputTrack)	// Clear keyboard tracking
				// otherwise
					= (okKeyboardAtt,keystate,wsH,inputTrack)
			| isDownKey
				= (okKeyboardAtt,keystate,wsH,trackKeyboard wPtr 0 inputTrack)	// Key down sets input track
			| otherwise
				= (False,undef,wsH,inputTrack)
		where
			keystate				= keyState keycode state mods
			pressState				= getKeyboardStateKeyState keystate
			isDownKey				= pressState==KeyDown False
			(filter,selectState,_)	= getWindowKeyboardAtt (snd (cselect isWindowKeyboard (WindowKeyboard (const False) Unable undef) whAtts))
			okKeyboardAtt			= filter keystate && selectState==Able
		okWindowKeyboardState _ _ _ _ _
			= windoweventFatalError "okWindowKeyboardState" "placeholder not expected"
	| otherwise				// The keyboard action takes place in a control
		# (inputTrack,ioState)			= ioStGetInputTrack ioState
		  (ok,itemNr,key,wsH,inputTrack)= okControlItemsNrKeyboardState wPtr cPtr keycode state mods wsH inputTrack
		# ioState						= ioStSetInputTrack inputTrack ioState
		  info							= {	ckWIDS			= wids
										  ,	ckItemNr		= itemNr
										  ,	ckItemPtr		= cPtr
										  ,	ckKeyboardState	= key
										  }
		  deviceEvent					= if ok (Just (ControlKeyboardAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemsNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Int !Int !Int !(WindowStateHandle .pst) !(Maybe InputTrack)
													  -> (!Bool,!Int,KeyboardState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okControlItemsNrKeyboardState wPtr itemPtr keycode state mods wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}} inputTrack
			# (_,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr True keycode state mods whItems inputTrack
			= (ok,itemNr,itemPos,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}},inputTrack)
		where
			okControlsItemNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int ![WElementHandle .ls .pst] !(Maybe InputTrack)
														 -> (!Bool,!Bool,!Int,KeyboardState,![WElementHandle .ls .pst],! Maybe InputTrack)
			okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods [itemH:itemHs] inputTrack
				# (found,ok,itemNr,itemPos,itemH,inputTrack)	= okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemH inputTrack
				| found
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
				| otherwise
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
			where
				okControlItemNrKeyboardState :: !OSWindowPtr !OSWindowPtr !Bool !Int !Int !Int !(WElementHandle .ls .pst) !(Maybe InputTrack)
															 -> (!Bool,!Bool,!Int,KeyboardState,!WElementHandle .ls .pst, ! Maybe InputTrack)
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods 
											(WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemSelect,wItemShow,wItemAtts}) 
											inputTrack
					| itemPtr<>wItemPtr
						| not wItemShow
							= (False,False,0,undef,WItemHandle itemH,inputTrack)
						// otherwise
							# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble1 keycode state mods itemH.wItems inputTrack
							= (found,ok,itemNr,itemPos,WItemHandle {itemH & wItems=itemHs},inputTrack)
					| trackingKeyboard wPtr itemPtr inputTrack		// Control is already handling Key(Repeat/Up)
						| isDownKey									// Ignore all key down events
							= (True,False,0,undef,WItemHandle itemH,inputTrack)
						| pressState==KeyUp							// Clear keyboard tracking
							= (True,okKeyboardAtt,wItemNr,keystate,WItemHandle itemH,untrackKeyboard inputTrack)
						// otherwise
							= (True,okKeyboardAtt,wItemNr,keystate,WItemHandle itemH,inputTrack)
					| isDownKey										// Key down sets input track
						= (True,okKeyboardAtt,wItemNr,keystate,WItemHandle itemH,trackKeyboard wPtr itemPtr inputTrack)
					| otherwise
						= (True,False,0,undef,WItemHandle itemH,inputTrack)
				where
					contextAble1			= contextAble && wItemSelect
  					noKeyboardAtt			= ControlKeyboard (const False) Unable undef
					(filter,selectState,_)	= getControlKeyboardAtt (snd (cselect isControlKeyboard noKeyboardAtt wItemAtts))
					okKeyboardAtt			= contextAble1 && enabled selectState && filter keystate
					keystate				= keyState keycode state mods
					pressState				= getKeyboardStateKeyState keystate
					isDownKey				= pressState==KeyDown False
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WListLSHandle itemHs) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WListLSHandle itemHs,inputTrack)
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WExtendLSHandle wExH=:{wExtendItems=itemHs}) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WExtendLSHandle {wExH & wExtendItems=itemHs},inputTrack)
				
				okControlItemNrKeyboardState wPtr itemPtr contextAble keycode state mods (WChangeLSHandle wChH=:{wChangeItems=itemHs}) inputTrack
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrKeyboardState wPtr itemPtr contextAble keycode state mods itemHs inputTrack
					= (found,ok,itemNr,itemPos,WChangeLSHandle {wChH & wChangeItems=itemHs},inputTrack)
			
			okControlsItemNrKeyboardState _ _ _ _ _ _ itemH inputTrack
				= (False,False,0,undef,itemH,inputTrack)
		
		okControlItemsNrKeyboardState _ _ _ _ _ _ _
			= windoweventFatalError "okControlItemsNrKeyboardState" "window placeholder not expected"
where
	keyState :: !Int !Int !Int -> KeyboardState
	keyState keycode state mods
		| isSpecial			= SpecialKey special ks modifiers
		| otherwise			= CharKey (toChar keycode) ks
	where
		modifiers			= toModifiers mods
		ks					= case state of
								KEYDOWN		-> KeyDown False
								KEYREPEAT	-> KeyDown True
								KEYUP		-> KeyUp
		(isSpecial,special)	= case keycode of
								WinBackSpKey-> (True,backSpaceKey)
								WinBeginKey	-> (True,beginKey)
								WinDelKey	-> (True,deleteKey)
								WinDownKey	-> (True,downKey)
								WinEndKey	-> (True,endKey)
								WinEscapeKey-> (True,escapeKey)
								WinHelpKey	-> (True,helpKey)
								WinLeftKey	-> (True,leftKey)
								WinPgDownKey-> (True,pgDownKey)
								WinPgUpKey	-> (True,pgUpKey)
								WinReturnKey-> (True,enterKey)
								WinRightKey	-> (True,rightKey)
								WinUpKey	-> (True,upKey)
								WinF1Key	-> (True,f1Key)
								WinF2Key	-> (True,f2Key)
								WinF3Key	-> (True,f3Key)
								WinF4Key	-> (True,f4Key)
								WinF5Key	-> (True,f5Key)
								WinF6Key	-> (True,f6Key)
								WinF7Key	-> (True,f7Key)
								WinF8Key	-> (True,f8Key)
								WinF9Key	-> (True,f9Key)
								WinF10Key	-> (True,f10Key)
								WinF11Key	-> (True,f11Key)
								WinF12Key	-> (True,f12Key)
								_			-> (False,undef)

filterOSEvent _ {ccMsg=CcWmKILLFOCUS,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wids,wsH)			= getWindowStateHandleWIDS wsH
	  (found,itemNr,wsH)	= getControlKeyFocusItemNr False cPtr wsH
	  windows				= setWindowHandlesWindow wsH windows
	| not found
		= (True,Nothing,Nothing,windows,ioState)
	| otherwise
		= (True,Nothing,Just (ControlLooseKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr}),windows,ioState)

filterOSEvent _ {ccMsg=CcWmLOSTKEY,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The window lost the keyboard input
		# (ok,wsH)					= okWindowKeyLost wsH
		  deviceEvent				= if ok (Just (WindowKeyboardAction {wkWIDS=wids,wkKeyboardState=KeyLost})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowKeyLost :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
		okWindowKeyLost wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
			| whKind==IsDialog
				= (False,wsH)
			| otherwise
				= (okKeyAtt,wsH)
		where
			(filter,selectState,_)	= getWindowKeyboardAtt (snd (cselect isWindowKeyboard (WindowKeyboard (const False) Unable undef) whAtts))
			okKeyAtt				= filter KeyLost && selectState==Able
		okWindowKeyLost _
			= windoweventFatalError "okWindowKeyLost" "placeholder not expected"
	| otherwise		// One of the window controls lost the keyboard input
		# (ok,itemNr,wsH)			= okControlItemNrsKeyLost cPtr wsH
		  info						= {	ckWIDS			= wids
									  ,	ckItemNr		= itemNr
									  ,	ckItemPtr		= cPtr
									  ,	ckKeyboardState	= KeyLost
									  }
		  deviceEvent				= if (ok && itemNr>0) (Just (ControlKeyboardAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemNrsKeyLost :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Bool,!Int,!WindowStateHandle .pst)
		okControlItemNrsKeyLost itemPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
			# (_,ok,itemNr,itemHs)	= okControlsItemNrKeyLost True itemPtr whItems
			= (ok,itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
		where
			okControlsItemNrKeyLost :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Bool,!Int,![WElementHandle .ls .pst])
			okControlsItemNrKeyLost contextAble itemPtr [itemH:itemHs]
				# (found,ok,itemNr,itemH)		= okControlItemNrKeyLost contextAble itemPtr itemH
				| found
					= (found,ok,itemNr,[itemH:itemHs])
				| otherwise
					# (found,ok,itemNr,itemHs)	= okControlsItemNrKeyLost contextAble itemPtr itemHs
					= (found,ok,itemNr,[itemH:itemHs])
			where
				okControlItemNrKeyLost :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Bool,!Int,!WElementHandle .ls .pst)
				okControlItemNrKeyLost contextAble itemPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemSelect,wItemShow,wItemAtts,wItems})
					| itemPtr<>wItemPtr
						| wItemShow
							# (found,okKey,itemNr,itemHs)	= okControlsItemNrKeyLost contextAble1 itemPtr wItems
							= (found,okKey,itemNr,WItemHandle {itemH & wItems=itemHs})
						// otherwise
							= (False,False,0,WItemHandle itemH)
					| otherwise
						= (True,okKeyAtt,wItemNr,WItemHandle itemH)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlKeyboardAtt (snd (cselect isControlKeyboard (ControlKeyboard (const False) Unable undef) wItemAtts))
					okKeyAtt	= contextAble1 && enabled selectState && filter KeyLost
									
				okControlItemNrKeyLost contextAble itemPtr (WListLSHandle itemHs)
					# (found,okKey,itemNr,itemHs)	= okControlsItemNrKeyLost contextAble itemPtr itemHs
					= (found,okKey,itemNr,WListLSHandle itemHs)
				
				okControlItemNrKeyLost contextAble itemPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
					# (found,okKey,itemNr,itemHs)	= okControlsItemNrKeyLost contextAble itemPtr itemHs
					= (found,okKey,itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
				
				okControlItemNrKeyLost contextAble itemPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
					# (found,okKey,itemNr,itemHs)	= okControlsItemNrKeyLost contextAble itemPtr itemHs
					= (found,okKey,itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
			
			okControlsItemNrKeyLost _ _ []
				= (False,False,0,[])
		
		okControlItemNrsKeyLost _ _
			= windoweventFatalError "okControlItemNrsKeyLost" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmLOSTMOUSE,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The window lost the mouse input
		# (ok,wsH)					= okWindowMouseLost wsH
		  deviceEvent				= if ok (Just (WindowMouseAction {wmWIDS=wids,wmMouseState=MouseLost})) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowMouseLost :: !(WindowStateHandle .pst) -> (!Bool,!WindowStateHandle .pst)
		okWindowMouseLost wsH=:{wshHandle=Just {wlsHandle={whKind,whAtts}}}
			| whKind==IsDialog
				= (False,wsH)
			| otherwise
				= (okMouseAtt,wsH)
		where
			(filter,selectState,_)	= getWindowMouseAtt (snd (cselect isWindowMouse (WindowMouse (const False) Unable undef) whAtts))
			okMouseAtt				= filter MouseLost && selectState==Able
		okWindowMouseLost _
			= windoweventFatalError "okWindowMouseLost" "placeholder not expected"
	| otherwise		// One of the window controls lost the mouse input
		# (ok,itemNr,wsH)			= okControlItemNrsMouseLost cPtr wsH
		  info						= {	cmWIDS			= wids
									  ,	cmItemNr		= itemNr
									  ,	cmItemPtr		= cPtr
									  ,	cmMouseState	= MouseLost
									  }
		  deviceEvent				= if (ok && itemNr>0) (Just (ControlMouseAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemNrsMouseLost :: !OSWindowPtr !(WindowStateHandle .pst) -> (!Bool,!Int,!WindowStateHandle .pst)
		okControlItemNrsMouseLost itemPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
			# (_,ok,itemNr,itemHs)	= okControlsItemNrMouseLost True itemPtr whItems
			= (ok,itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
		where
			okControlsItemNrMouseLost :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Bool,!Int,![WElementHandle .ls .pst])
			okControlsItemNrMouseLost contextAble itemPtr [itemH:itemHs]
				# (found,ok,itemNr,itemH)		= okControlItemNrMouseLost contextAble itemPtr itemH
				| found
					= (found,ok,itemNr,[itemH:itemHs])
				| otherwise
					# (found,ok,itemNr,itemHs)	= okControlsItemNrMouseLost contextAble itemPtr itemHs
					= (found,ok,itemNr,[itemH:itemHs])
			where
				okControlItemNrMouseLost :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Bool,!Int,!WElementHandle .ls .pst)
				okControlItemNrMouseLost contextAble itemPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemSelect,wItemShow,wItemAtts,wItems})
					| itemPtr<>wItemPtr
						| wItemShow
							# (found,ok,itemNr,itemHs)	= okControlsItemNrMouseLost contextAble1 itemPtr wItems
							= (found,ok,itemNr,WItemHandle {itemH & wItems=itemHs})
						// otherwise
							= (False,False,0,WItemHandle itemH)
					| otherwise
						= (True,okMouseAtt,wItemNr,WItemHandle itemH)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlMouseAtt (snd (cselect isControlMouse (ControlMouse (const False) Unable undef) wItemAtts))
					okMouseAtt	= contextAble1 && enabled selectState && filter MouseLost
									
				okControlItemNrMouseLost contextAble itemPtr (WListLSHandle itemHs)
					# (found,ok,itemNr,itemHs)	= okControlsItemNrMouseLost contextAble itemPtr itemHs
					= (found,ok,itemNr,WListLSHandle itemHs)
				
				okControlItemNrMouseLost contextAble itemPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
					# (found,ok,itemNr,itemHs)	= okControlsItemNrMouseLost contextAble itemPtr itemHs
					= (found,ok,itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
				
				okControlItemNrMouseLost contextAble itemPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
					# (found,ok,itemNr,itemHs)	= okControlsItemNrMouseLost contextAble itemPtr itemHs
					= (found,ok,itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
			
			okControlsItemNrMouseLost _ _ []
				= (False,False,0,[])
		
		okControlItemNrsMouseLost _ _
			= windoweventFatalError "okControlItemNrsMouseLost" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmMOUSE,p1=wPtr,p2=cPtr,p3=action,p4=x,p5=y,p6=mods} windows ioState
	# (found,wsH,windows)			= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (able,wsH)					= getWindowStateHandleSelect wsH
	| not able
		= (True,Nothing,Nothing,setWindowHandlesWindow wsH windows,ioState)
	# (wids,wsH)					= getWindowStateHandleWIDS wsH
	| wPtr==cPtr	// The mouse action takes place in the window
		# (inputTrack,ioState)		= ioStGetInputTrack ioState
		  (ok,mouse,wsH,inputTrack)	= okWindowMouseState action {x=x,y=y} wsH inputTrack
		  deviceEvent				= if ok (Just (WindowMouseAction {wmWIDS=wids,wmMouseState=mouse})) Nothing
		# ioState					= ioStSetInputTrack inputTrack ioState
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okWindowMouseState :: !Int !Point2 !(WindowStateHandle .pst) !(Maybe InputTrack)
					  -> (!Bool,MouseState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okWindowMouseState action eventPos wsH=:{wshHandle=Just {wlsHandle={whKind,whWindowInfo,whAtts}}} inputTrack
			| whKind==IsDialog
				= (False,undef,wsH,inputTrack)
			| trackingMouse wPtr 0 inputTrack					// Window is already handling Mouse(Drag/Up)
				| isDownButton || buttonstate==ButtonStillUp	// Ignore all mouse down and mouse move events
					= (False,undef,wsH,inputTrack)
				| buttonstate==ButtonUp							// Clear mouse tracking
					= (okMouseAtt,mousestate,wsH,untrackMouse inputTrack)
				// otherwise
					= (okMouseAtt,mousestate,wsH,inputTrack)
			| isDownButton										// Mouse down event sets input track
				= (okMouseAtt,mousestate,wsH,trackMouse wPtr 0 inputTrack)
			| isMember buttonstate [ButtonStillDown,ButtonUp]	// Ignore all mouse drag and up events when not tracking
				= (False,undef,wsH,inputTrack)
			| otherwise
				= (okMouseAtt,mousestate,wsH,inputTrack)
		where
			origin					= (getWindowInfoWindowData whWindowInfo).windowOrigin
			mousestate				= mouseState action (eventPos+origin)
			buttonstate				= getMouseStateButtonState mousestate
			isDownButton			= isMember buttonstate [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
			(filter,selectState,_)	= getWindowMouseAtt (snd (cselect isWindowMouse (WindowMouse (const False) Unable undef) whAtts))
			okMouseAtt				= filter mousestate && selectState==Able
		okWindowMouseState _ _ _ _
			= windoweventFatalError "okWindowMouseState" "placeholder not expected"
	| otherwise				// The mouse action takes place in a control
		# (inputTrack,ioState)		= ioStGetInputTrack ioState
		  (ok,itemNr,mouse,wsH,inputTrack)
		  							= okControlItemsNrMouseState wPtr cPtr action {x=x,y=y} wsH inputTrack
		# ioState					= ioStSetInputTrack inputTrack ioState
		  info						= {	cmWIDS			= wids
									  ,	cmItemNr		= itemNr
									  ,	cmItemPtr		= cPtr
									  ,	cmMouseState	= mouse
									  }
		  deviceEvent				= if ok (Just (ControlMouseAction info)) Nothing
		= (True,Nothing,deviceEvent,setWindowHandlesWindow wsH windows,ioState)
	with
		okControlItemsNrMouseState :: !OSWindowPtr !OSWindowPtr !Int !Point2 !(WindowStateHandle .pst) !(Maybe InputTrack)
												   -> (!Bool,!Int,MouseState,!WindowStateHandle .pst, ! Maybe InputTrack)
		okControlItemsNrMouseState wPtr itemPtr action eventPos wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}} inputTrack
			# (_,ok,itemNr,itemPos,itemHs,inputTrack)
									= okControlsItemNrMouseState True wPtr itemPtr action eventPos whItems inputTrack
			= (ok,itemNr,itemPos,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}},inputTrack)
		where
			okControlsItemNrMouseState :: !Bool !OSWindowPtr !OSWindowPtr !Int !Point2 ![WElementHandle .ls .pst] !(Maybe InputTrack)
													   -> (!Bool,!Bool,!Int,MouseState,![WElementHandle .ls .pst], !Maybe InputTrack)
			okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos [itemH:itemHs] inputTrack
				# (found,ok,itemNr,itemPos,itemH,inputTrack)
										= okControlItemNrMouseState contextAble wPtr itemPtr action eventPos itemH   inputTrack
				| found
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
				| otherwise
					# (found,ok,itemNr,itemPos,itemHs,inputTrack)	= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
					= (found,ok,itemNr,itemPos,[itemH:itemHs],inputTrack)
			where
				okControlItemNrMouseState :: !Bool !OSWindowPtr !OSWindowPtr !Int !Point2 !(WElementHandle .ls .pst) !(Maybe InputTrack)
														   -> (!Bool,!Bool,!Int,MouseState,!WElementHandle .ls .pst,  !Maybe InputTrack)
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos 
										  (WItemHandle itemH=:{wItemPtr,wItemSelect,wItemKind,wItemNr,wItemShow,wItemAtts,wItems,wItemInfo})
										  inputTrack
					| itemPtr<>wItemPtr
						| wItemShow
							# (found,ok,itemNr,mousestate,itemHs,inputTrack)	= okControlsItemNrMouseState contextAble1 wPtr itemPtr action eventPos wItems inputTrack
							= (found,ok,itemNr,mousestate,WItemHandle {itemH & wItems=itemHs},inputTrack)
						// otherwise
							= (False,False,0,undef,WItemHandle itemH,inputTrack)
					| trackingMouse wPtr itemPtr inputTrack				// Control is already handling Mouse(Drag/Up)
						| isDownButton || buttonstate==ButtonStillUp	// Ignore all mouse down and mouse move events
							= (True,False,0,undef,WItemHandle itemH,inputTrack)
						| buttonstate==ButtonUp							// Clear mouse tracking
							= (True,okMouseAtt,wItemNr,mousestate,WItemHandle itemH,untrackMouse inputTrack)
						// otherwise
							= (True,okMouseAtt,wItemNr,mousestate,WItemHandle itemH,inputTrack)
					| isDownButton										// Mouse down event sets input track
						= (True,okMouseAtt,wItemNr,mousestate,WItemHandle itemH,trackMouse wPtr itemPtr inputTrack)
					| isMember buttonstate [ButtonStillDown,ButtonUp]	// Ignore all mouse drag and up events when not tracking
						= (True,False,0,undef,WItemHandle itemH,inputTrack)
					| otherwise
						= (True,okMouseAtt,wItemNr,mousestate,WItemHandle itemH,inputTrack)
				where
					contextAble1= contextAble && wItemSelect
					(filter,selectState,_)
								= getControlMouseAtt (snd (cselect isControlMouse (ControlMouse (const False) Unable undef) wItemAtts))
					okMouseAtt	= contextAble1 && enabled selectState && filter mousestate
					mousestate	= mouseState action (origin+eventPos)
					buttonstate	= getMouseStateButtonState mousestate
					isDownButton= isMember buttonstate [ButtonDown,ButtonDoubleDown,ButtonTripleDown]
					origin		= case wItemKind of
									IsCustomButtonControl	-> zero
									IsCustomControl			-> zero
									IsCompoundControl		-> (getWItemCompoundInfo wItemInfo).compoundOrigin
									_						-> windoweventFatalError "okControlItemsNrMouseState" "mouse event generated for unexpected control"
									
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WListLSHandle itemHs) inputTrack
					# (found,ok,itemNr,mousestate,itemHs,inputTrack)	= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
					= (found,ok,itemNr,mousestate,WListLSHandle itemHs,inputTrack)
				
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WExtendLSHandle wExH=:{wExtendItems=itemHs}) inputTrack
					# (found,ok,itemNr,mousestate,itemHs,inputTrack)	= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
					= (found,ok,itemNr,mousestate,WExtendLSHandle {wExH & wExtendItems=itemHs},inputTrack)
				
				okControlItemNrMouseState contextAble wPtr itemPtr action eventPos (WChangeLSHandle wChH=:{wChangeItems=itemHs}) inputTrack
					# (found,ok,itemNr,mousestate,itemHs,inputTrack)	= okControlsItemNrMouseState contextAble wPtr itemPtr action eventPos itemHs inputTrack
					= (found,ok,itemNr,mousestate,WChangeLSHandle {wChH & wChangeItems=itemHs},inputTrack)
			
			okControlsItemNrMouseState _ _ _ _ _ [] inputTrack
				= (False,False,0,undef,[],inputTrack)
		
		okControlItemsNrMouseState _ _ _ _ _ _
			= windoweventFatalError "okControlItemsNrMouseState" "placeholder not expected"
where
	modifiers				= toModifiers mods
	nrDown					= case action of
								BUTTONDOWN			-> 1
								BUTTONDOUBLEDOWN	-> 2
							 	_					-> 3
	mouseState action pos	= case action of
								BUTTONSTILLUP		-> MouseMove pos modifiers
								BUTTONUP			-> MouseUp   pos modifiers
								BUTTONSTILLDOWN		-> MouseDrag pos modifiers
								_					-> MouseDown pos modifiers nrDown

filterOSEvent _ {ccMsg=CcWmSETFOCUS,p1=wPtr,p2=cPtr} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wids,wsH)			= getWindowStateHandleWIDS wsH
	  (found,itemNr,wsH)	= getControlKeyFocusItemNr True cPtr wsH
	  windows				= setWindowHandlesWindow wsH windows
	| not found
		= (True,Nothing,Nothing,windows,ioState)
	| otherwise
		= (True,Nothing,Just (ControlGetKeyFocus {ckfWIDS=wids,ckfItemNr=itemNr,ckfItemPtr=cPtr}),windows,ioState)

filterOSEvent wMetrics {ccMsg=CcWmSIZE,p1=wPtr,p2=w,p3=h,p4=usersizing} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	# (wKind,wsH)			= getWindowStateHandleWindowKind wsH
	| wKind==IsDialog		// This alternative should never occur
		= windoweventFatalError "filterOSEvent" "WindowSizeAction event generated for Dialog"
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		# (tb,ioState)		= getIOToolbox ioState
		  (info,wsH,tb)		= getWindowStateHandleSize wids w h (usersizing<>0) wsH tb
		# ioState			= setIOToolbox tb ioState
		  windows			= setWindowHandlesWindow wsH windows
		= (True,Nothing,Just (WindowSizeAction info),windows,ioState)
where
	getWindowStateHandleSize :: !WIDS !Int !Int !Bool !(WindowStateHandle .pst) !*OSToolbox
							 -> (!WindowSizeActionInfo,!WindowStateHandle .pst, !*OSToolbox)
	getWindowStateHandleSize wids newW newH usersizing wsH=:{wshHandle=Just {wlsHandle=wH=:{whSize,whWindowInfo}}} tb
		= ({wsWIDS=wids,wsSize={w=newW`,h=newH`},wsUpdateAll=not usersizing},wsH,tb)
	where
		windowInfo				= getWindowInfoWindowData whWindowInfo
		domainRect				= windowInfo.windowDomain
		hasScrolls				= (isJust windowInfo.windowHScroll,isJust windowInfo.windowVScroll)
		(visHScroll,visVScroll)	= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
		newW`					= if visVScroll (newW+wMetrics.osmVSliderWidth)  newW	// Correct newW in case of visible vertical   scrollbar
		newH`					= if visHScroll (newH+wMetrics.osmHSliderHeight) newH	// Correct newH in case of visible horizontal scrollbar
	getWindowStateHandleSize _ _ _ _ _ _
		= windoweventFatalError "getWindowStateHandleSize" "placeholder not expected"

filterOSEvent _ {ccMsg=CcWmSPECIALBUTTON,p1=wPtr,p2=okOrCancel} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS      wsH
		  (okId,wsH)		= getWindowStateHandleDefaultId wsH
		  (cancelId,wsH)	= getWindowStateHandleCancelId  wsH
		  okOrCancelEvent	= if (okOrCancel==ISOKBUTTON)     (if (isJust okId)     (Just (WindowOK     wids)) Nothing)
		  					 (if (okOrCancel==ISCANCELBUTTON) (if (isJust cancelId) (Just (WindowCANCEL wids)) Nothing)
		  					 								  (windoweventFatalError "filterOSEvent (CcWmSPECIALBUTTON)" "incorrect argument"))
		= (True,Nothing,okOrCancelEvent,setWindowHandlesWindow wsH windows,ioState)

/*	The CcWmPAINT message is generated to update the indicated rectangle of the argument window.
*/
filterOSEvent _ {ccMsg=CcWmPAINT,p1=wPtr,p2=left,p3=top,p4=right,p5=bottom,p6=gc} windows ioState
	# (found,wsH,windows)	= getWindowHandlesWindow (toWID wPtr) windows
	| not found
		= (False,Nothing,Nothing,windows,ioState)
	| otherwise
		# (wids,wsH)		= getWindowStateHandleWIDS wsH
		  windows			= setWindowHandlesWindow wsH windows
		  updRect			= fromTuple4 (left,top,right,bottom)
		  updateInfo		= {updWIDS=wids,updWindowArea=updRect,updControls=[],updGContext=if (gc==0) Nothing (Just gc)}
		= (True,Nothing,Just (WindowUpdate updateInfo),windows,ioState)

filterOSEvent _ _ _ _
	= windoweventFatalError "filterOSEvent" "unmatched OSEvent"


/*	PA: moved to clCCall_12:
toModifiers :: !Int -> Modifiers
toModifiers i
	=	{	shiftDown	= shifton
		,	optionDown	= alton
		,	commandDown	= ctrlon
		,	controlDown	= ctrlon
		,	altDown		= alton
		}
where
	shifton	= i bitand SHIFTBIT <> 0
	alton	= i bitand ALTBIT   <> 0
	ctrlon	= i bitand CTRLBIT  <> 0
*/

getControlKeyFocusItemNr :: !Bool !OSWindowPtr !(WindowStateHandle .pst) -> (!Bool,!Int,!WindowStateHandle .pst)
getControlKeyFocusItemNr activated cPtr wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr wH.whItems
	= (found,itemNr,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
where
	getControlsKeyFocusItemNr` :: !Bool !OSWindowPtr ![WElementHandle .ls .pst] -> (!Bool,!Int,![WElementHandle .ls .pst])
	getControlsKeyFocusItemNr` activated cPtr []
		= (False,0,[])
	getControlsKeyFocusItemNr` activated cPtr [itemH:itemHs]
		# (found,itemNr,itemH)	= getControlKeyFocusItemNr` activated cPtr itemH
		| found
			= (found,itemNr,[itemH:itemHs])
		| otherwise
			# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (found,itemNr,[itemH:itemHs])
	where
		getControlKeyFocusItemNr` :: !Bool !OSWindowPtr !(WElementHandle .ls .pst) -> (!Bool,!Int,!WElementHandle .ls .pst)
		getControlKeyFocusItemNr` activated cPtr (WItemHandle itemH=:{wItemPtr,wItemNr,wItemKind,wItemSelect,wItemAtts,wItems})
			| cPtr==wItemPtr
				| not (isMember wItemKind [IsCompoundControl,IsCustomControl,IsEditControl,IsPopUpControl])
					= (True,0,WItemHandle itemH)
			/*	PA: deze tests zijn verwijderd
				| not wItemSelect
					= (0,WItemHandle itemH)
				| contains reqAttribute wItemAtts
					= (wItemNr,WItemHandle itemH)
				// otherwise
					= (0,WItemHandle itemH)
			*/
				// otherwise
					= (True,wItemNr,WItemHandle itemH)
			| not (isRecursiveControl wItemKind)
				= (False,0,WItemHandle itemH)
			| otherwise
				# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr wItems
				= (found,itemNr,WItemHandle {itemH & wItems=itemHs})
	/*	where
			reqAttribute	= if activated isControlActivate isControlDeactivate	// PA: wordt niet meer gebruikt
	*/	
		getControlKeyFocusItemNr` activated cPtr (WListLSHandle itemHs)
			# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (found,itemNr,WListLSHandle itemHs)
		
		getControlKeyFocusItemNr` activated cPtr (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (found,itemNr,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		getControlKeyFocusItemNr` activated cPtr (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,itemNr,itemHs)	= getControlsKeyFocusItemNr` activated cPtr itemHs
			= (found,itemNr,WChangeLSHandle {wChH & wChangeItems=itemHs})
getControlKeyFocusItemNr _ _ _
	= windoweventFatalError "getControlKeyFocusItemNr" "window placeholder not expected"


//	Access operations on InputTrack:

trackingMouse :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Bool
trackingMouse wPtr cPtr (Just {itWindow,itControl,itKind={itkMouse}})
	= wPtr==itWindow && cPtr==itControl && itkMouse
trackingMouse _ _ _
	= False

trackingKeyboard :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Bool
trackingKeyboard wPtr cPtr (Just {itWindow,itControl,itKind={itkKeyboard}})
	= wPtr==itWindow && cPtr==itControl && itkKeyboard
trackingKeyboard _ _ _
	= False

trackMouse :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Maybe InputTrack
trackMouse wPtr cPtr (Just it=:{itWindow,itControl,itKind=itk})
	| wPtr<>itWindow || cPtr<>itControl
		= windoweventFatalError "trackMouse" "incorrect window/control parameters"
	| otherwise
		= Just {it & itKind={itk & itkMouse=True}}
trackMouse wPtr cPtr nothing
//	= Just {itWindow=wPtr,itControl=cPtr,itKind={itkMouse=True,itkKeyboard=False}}
	= Just { itWindow  = wPtr
	       , itControl = cPtr
	       , itKind    = { itkMouse    = True
	                     , itkKeyboard = False
	                     , itkChar     = 0			// PA: assuming the fields itkChar and itkSlider are not used on Windows platform
	                     , itkSlider   = Nothing	// dito
	                     }
	       }

untrackMouse :: !(Maybe InputTrack) -> Maybe InputTrack
untrackMouse (Just it=:{itKind=itk})
	| itk.itkKeyboard
		= Just {it & itKind={itk & itkMouse=False}}
	| otherwise
		= Nothing
untrackMouse nothing
	= nothing

untrackKeyboard :: !(Maybe InputTrack) -> Maybe InputTrack
untrackKeyboard (Just it=:{itKind=itk})
	| itk.itkMouse
		= Just {it & itKind={itk & itkKeyboard=False}}
	| otherwise
		= Nothing
untrackKeyboard nothing
	= nothing

trackKeyboard :: !OSWindowPtr !OSWindowPtr !(Maybe InputTrack) -> Maybe InputTrack
trackKeyboard wPtr cPtr (Just it=:{itWindow,itControl,itKind=itk})
	| wPtr<>itWindow || cPtr<>itControl
		= windoweventFatalError "trackKeyboard" "incorrect window/control parameters"
	| otherwise
		= Just {it & itKind={itk & itkKeyboard=True}}
trackKeyboard wPtr cPtr nothing
//	= Just {itWindow=wPtr,itControl=cPtr,itKind={itkMouse=False,itkKeyboard=True}}
	= Just { itWindow  = wPtr
	       , itControl = cPtr
	       , itKind    = { itkMouse    = False
	                     , itkKeyboard = True
	                     , itkChar     = 0			// PA: assuming the fields itkChar and itkSlider are not used on Windows platform
	                     , itkSlider   = Nothing	// dito
	                     }
	       }
