implementation module windowcreate


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	osevent, ostoolbox, ostypes, oswindow
from	StdMenu				import enableMenuSystem, disableMenuSystem
from	StdPSt				import accPIO, appPIO
from	StdWindowAttribute	import isWindowInit, getWindowInitFun, isWindowClose, isWindowCursor, getWindowCursorAtt
import	commondef, controlpos, iostate, scheduler, windowaccess
from	controlcreate		import createControls
from	windowclipstate		import validateWindowClipState
from	windowdevice		import windowFunctions
from	windowupdate		import updatewindow
from windowvalidate		import validateWindow
import	menuwindowmenu


windowcreateFatalError :: String String -> .x
windowcreateFatalError function error
	= fatalError function "windowcreate" error

/*	Open a modal dialogue.
*/
openmodalwindow :: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!Maybe .ls,!PSt .l)
openmodalwindow wId {wlsState,wlsHandle} pState=:{io=ioState}
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found					// This condition should never occur: WindowDevice must have been 'installed'
		= windowcreateFatalError "openmodalwindow" "could not retrieve WindowSystemState from IOSt"
	# windows					= windowSystemStateGetWindowHandles wDevice
	# (tb,ioState)				= getIOToolbox ioState
	# tb						= osInitialiseWindows tb					// initialise windows toolbox
	# (osdinfo,ioState)			= ioStGetOSDInfo ioState
	# (wMetrics,ioState)		= ioStGetOSWindowMetrics ioState
	# (_,_,size,_,wlsHandle,windows,tb)
								= validateWindow wMetrics osdinfo wlsHandle windows tb
	  (title,closable,wlsHandle)= (\wlsH=:{whTitle,whAtts}->(whTitle,contains isWindowClose whAtts,wlsH)) wlsHandle
	# ioState					= ioStSetOSDInfo osdinfo ioState
	  wlsH						= {wlsState=wlsState,wlsHandle=wlsHandle}
	  wIds						= {wId=wId,wPtr=0,wActive=True}				// wPtr=0 is assumed by system
	  wsH						= {wshIds=wIds,wshHandle=Just wlsH}
	  (modalWIDS,windows)		= getWindowHandlesActiveModalDialog windows
	  windows					= addWindowHandlesActiveWindow wsH windows	// frontmost position is assumed by system
	  (disableMenus,enableMenus)= if (osModalDialogHandlesMenuSelectState || isJust modalWIDS)
								     (id,id)
								     (appPIO disableMenuSystem,appPIO enableMenuSystem)
	# (ioId,ioState)			= ioStGetIOId ioState
	# ioState					= ioStSetIOIsModal (Just ioId) ioState
	# ioState					= setIOToolbox tb ioState
	# ioState					= ioStSetDevice (WindowSystemState windows) ioState
	# (inputTrack,ioState)		= ioStGetInputTrack ioState
	# ioState					= ioStSetInputTrack Nothing ioState			// clear input track information
	# pState					= {pState & io=ioState}
	# pState					= disableMenus pState
	# (noError,pState)			= osCreateModalDialog wMetrics closable title osdinfo (mapMaybe (\{wPtr}->wPtr) modalWIDS) (toTuple size)
									(modalDialogControls wMetrics)			// ignored iff osModalDialogHandlesControlCreation
									(modalInitIO wId)						// ignored iff osModalDialogHandlesWindowInit
									(if osModalDialogHandlesEvents
										(OSModalEventCallback getOSEvent setOSEvent handleOSEvent)
										(OSModalEventLoop (modalEventLoop wId))
									)
									(accPIO getIOToolbox,\tb -> appPIO (setIOToolbox tb))
									pState
	# pState					= enableMenus pState
	  errorReport				= if noError NoError (OtherError "could not create modal dialog")
	  (delayMouse,delayKey)		= case inputTrack of						// after handling modal dialog, generate proper (Mouse/Key)Lost events
	  								Nothing	-> ([],[])
	  								Just it=:{itWindow,itControl,itKind}
	  										-> (if itKind.itkMouse    [createOSLooseMouseEvent itWindow (if (itControl==0) itWindow itControl)] []
	  										   ,if itKind.itkKeyboard [createOSLooseKeyEvent   itWindow (if (itControl==0) itWindow itControl)] []
	  										   )
	# (osDelayEvents,ioState)	= accIOToolbox (strictSeqList (delayMouse++delayKey)) pState.io
	# (osEvents,ioState)		= ioStGetEvents ioState
	# ioState					= ioStSetEvents (osAppendEvents osDelayEvents osEvents) ioState
	# (finalLS,moreLS,ioState)	= getFinalModalDialogLS noError (toWID wId) ioState
	| isJust modalWIDS			// there still are modal dialogs
		|| moreLS				// DvA: or unretrieved final states...
		= (errorReport,finalLS,{pState & io=ioState})
	# (closed,ioState)			= ioStClosed ioState
	| closed					// process has been requested to close; remove WindowDevice, so process can removed (scheduler)
		# (_,_,ioState)			= ioStGetDevice WindowDevice ioState
		= (errorReport,finalLS,{pState & io=ioState})
	| otherwise
		= (errorReport,finalLS,{pState & io=ioState})
where
	handleOSEvent :: !OSEvent !(PSt .l) -> (![Int],!PSt .l)
	handleOSEvent osEvent pState = accContext (handleContextOSEvent osEvent) pState
	
	getOSEvent :: !(PSt .l) -> (!OSEvents,!PSt .l)
	getOSEvent pState = accPIO ioStGetEvents pState
	
	setOSEvent :: !(!OSEvents,!PSt .l) -> PSt .l
	setOSEvent (osEvents,pState) = appPIO (ioStSetEvents osEvents) pState
	
/*	modalDialogControls creates the controls of the given modal dialog.
	PA: Note that the initially active control is not set.
*/	modalDialogControls :: !OSWindowMetrics !OSWindowPtr !(PSt .l) -> PSt .l
	modalDialogControls wMetrics wPtr pState=:{io=ioState}
		# (tb,ioState)				= getIOToolbox ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found					// This condition should never occur: WindowDevice must have been 'installed'
			= windowcreateFatalError "openmodalwindow" "could not retrieve WindowSystemState from IOSt"
		| otherwise
			# windows				= windowSystemStateGetWindowHandles wDevice
			# (itemPtr,windows,tb)	= createModalDialogControls wMetrics wPtr windows tb
			# ioState				= setIOToolbox tb ioState
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			= {pState & io=ioState}

/*	modalInitIO takes care that the WindowInit attribute is evaluated before the event loop is entered.
*/	modalInitIO :: !Id !OSWindowPtr !(PSt .l) -> PSt .l
	modalInitIO modalWindowId wPtr pState
		= snd (doIO (WindowInitialise wids) pState)
	where
		wids	= { wId=modalWindowId, wPtr=wPtr, wActive=True }
		doIO	= windowFunctions.dDoIO

/*	modalEventLoop handles events for the modal dialog (only in case osCreateModalDialog does not do this).
*/	modalEventLoop :: !Id !(PSt .l) -> PSt .l
	modalEventLoop modalWindowId pState
		= appContext (fst o (flip (chandleEvents modalDialogExists) OSNewToolbox)) pState
	where
	/*	modalDialogExists context
			returns True iff the modal dialog exists (and therefore also its parent process).
	*/
		modalDialogExists :: !Context -> (!Bool,!Context)
		modalDialogExists context=:{cProcesses,cModalProcess=Just myId}
			| not found				= (False,          context1)
			| otherwise				= (fromJust closed,context1)
		where
			((found,closed),groups1)= gLocals modalWindowId myId cProcesses
			context1				= {context & cProcesses=groups1}
			
			gLocals :: !Id !SystemId !*CProcesses -> (!Result Bool,!*CProcesses)
			gLocals modalWindowId id locals
				= accessLocals (checkIOStQuitted` modalWindowId id) locals
			where
				checkIOStQuitted` modalWindowId id localIO=:{localIOSt}
					= (r,{localIO & localIOSt=ioState})
				where
					(r,ioState)	= checkIOStQuitted modalWindowId id localIOSt
					
					checkIOStQuitted :: !Id !SystemId !(IOSt .l) -> (!Result Bool,!IOSt .l)
					checkIOStQuitted modalWindowId ioid ioState
						# (ioid`,ioState)		= ioStGetIOId ioState
						| not (ioid == ioid`)
							= ((False,Nothing),ioState)
						# (closed,ioState)	= ioStClosed ioState
						| closed
							= ((True, Just False),ioState)
						# (_,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
						# windows				= windowSystemStateGetWindowHandles wDevice
						# (found,wsH,windows)	= getWindowHandlesWindow (toWID modalWindowId) windows
						| not found
							# ioState			= ioStSetDevice (WindowSystemState windows) ioState
							= ((True, Just False),ioState)
						# (mode,wsH)			= getWindowStateHandleWindowMode wsH
						# windows				= setWindowHandlesWindow wsH windows
						# ioState				= ioStSetDevice (WindowSystemState windows) ioState
						| mode<>Modal
							= ((True, Just False),ioState)
						| otherwise
							= ((True, Just True),ioState)
		
		modalDialogExists context
			= (False,context)
	
/*	getFinalModalDialogLS retrieves the final local state of the modal dialog. This value has been stored in the window handles.
	This MUST have been done by disposeWindow (windowdispose). 
*/
	getFinalModalDialogLS :: !Bool !WID !(IOSt .l) -> (!Maybe .ls,!Bool,!IOSt .l)
	getFinalModalDialogLS noError wid ioState
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found
			= windowcreateFatalError "getFinalModalDialogLS" "could not retrieve WindowSystemState from IOSt"
		# windows					= windowSystemStateGetWindowHandles wDevice
		  (final,more,windows)		= getFinalLS wid windows
		# ioState					= ioStSetDevice (WindowSystemState windows) ioState
		| not noError
			= (Nothing,more,ioState)
		| otherwise
			= case final of
				Nothing    -> windowcreateFatalError "getFinalModalDialogLS" "final local modal dialog state not found"
				Just final -> (getFinalModalLS wid final,more,ioState)
	where
		getFinalLS :: !WID !(WindowHandles .pst) -> (!Maybe FinalModalLS,!Bool,!WindowHandles .pst)
		getFinalLS wid windows=:{whsFinalModalLS}
			# (removed,finalLS,finalLSs)	= uremove (\fmLS=:{fmWIDS}->(identifyWIDS wid fmWIDS,fmLS)) undef whsFinalModalLS
			  (empty,finalLSs)				= uisEmpty finalLSs
			  windows						= {windows & whsFinalModalLS=finalLSs}
			  more							= not empty
			| not removed					= (Nothing,more,windows)
			| otherwise						= (Just finalLS,more,windows)


/*	createModalDialogControls wPtr ioState
		Replaces the OSWindowPtr of the modal dialog that is identified by a zero OSWindowPtr from the IOSt.
		If such a modal dialog could not be found, then a runtime error is generated.
		Then it takes care that the controls of the indicated modal dialog are created.
	NOTE: this function is also used in windowevent.icl
*/
createModalDialogControls :: !OSWindowMetrics !OSWindowPtr !*(WindowHandles .pst) !*OSToolbox
										  -> (!OSWindowPtr, !*WindowHandles .pst, !*OSToolbox)
createModalDialogControls wMetrics wPtr windows tb
	# (maybeWIDS,windows)		= getWindowHandlesActiveWindow windows
	| isNothing maybeWIDS
		= windowcreateFatalError "createModalDialogControls" "could not retrieve active window from IOSt"
	# wids						= fromJust maybeWIDS
	| wids.wPtr<>0
		= windowcreateFatalError "createModalDialogControls" "could not retrieve modal dialog from IOSt"
	| otherwise
		# (_,wsH,windows)		= removeWindowHandlesWindow (toWID 0) windows
		  wids					= {wids & wPtr=wPtr}
		  wsH					= (\wsH->{wsH & wshIds=wids}) wsH
		# (itemPtr,wsH,tb)		= createDialogControls wMetrics wsH tb
		= (itemPtr,addWindowHandlesActiveWindow wsH windows,tb)
where
	createDialogControls :: !OSWindowMetrics !(WindowStateHandle .pst) !*OSToolbox
							-> (!OSWindowPtr, !WindowStateHandle .pst, !*OSToolbox)
	createDialogControls wMetrics wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=itemHs}}} tb
		# (itemHs,tb)			= createControls wMetrics whDefaultId whCancelId True wPtr itemHs tb
		# (itemPtr,wH)			= getInitActiveControl {wH & whItems=itemHs}
		= (itemPtr,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		whDefaultId				= wH.whDefaultId
		whCancelId				= wH.whCancelId
	createDialogControls _ _ _
		= windowcreateFatalError "createDialogControls" "placeholder not expected"


/*	Open a modeless window/dialogue.
*/
openwindow :: !Id !(WindowLSHandle .ls (PSt .l)) !(PSt .l) -> PSt .l
openwindow wId {wlsState,wlsHandle} pState=:{io=ioState}
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found					// This condition should never occur: WindowDevice must have 'installed'
		= windowcreateFatalError "openwindow" "could not retrieve WindowSystemState from IOSt"
	| otherwise
		= pState2
	with
		windows					= windowSystemStateGetWindowHandles wDevice
		(delayinfo,wPtr,index,wlsHandle1,windows1,ioState2)
								= openAnyWindow wId wlsHandle windows ioState
		(windowInit,wlsHandle2)	= getWindowHandleInit wlsHandle1
		wlsH					= {wlsState=ls1,wlsHandle=wlsHandle2}
		wIds					= {wId=wId,wPtr=wPtr,wActive=False}
		wsH						= {wshIds=wIds,wshHandle=Just wlsH}
		windows2				= addWindowHandlesWindow index wsH windows1
		ioState3				= ioStSetDevice (WindowSystemState windows2) ioState2
		ioState4				= bufferDelayedEvents delayinfo ioState3
		pState1					= {pState & io=ioState4}
		(ls1,pState2)			= windowInit (wlsState,pState1)	// DvA: netter om hier delayed WindowInitialise voor te genereren?
																// PA:  nu is het conform andere initialisatie functies. Nog eens over nadenken.
		
		getWindowHandleInit :: !(WindowHandle .ls .pst) -> (!IdFun *(.ls,.pst),!WindowHandle .ls .pst)
		getWindowHandleInit wH=:{whAtts}
			= (getWindowInitFun (snd (cselect isWindowInit (WindowInit id) whAtts)),wH)
		
	/*	openAnyWindow creates a window.
			After validating the window and its controls, the window and its controls are created.
			The return OSWindowPtr is the OSWindowPtr of the newly created window.
			The return Index is the proper insert position in the WindowHandles list.
	*/
		openAnyWindow :: !Id !(WindowHandle .ls (PSt .l)) !(WindowHandles (PSt .l)) !(IOSt .l)
			-> (![DelayActivationInfo],!OSWindowPtr,!Index,!WindowHandle .ls (PSt .l),!WindowHandles (PSt .l),!IOSt .l)
		openAnyWindow wId wH windows ioState
			# (tb,ioState)			= getIOToolbox ioState
			# tb					= osInitialiseWindows tb					// initialise windows toolbox
			# (osdinfo,ioState)		= ioStGetOSDInfo ioState
			# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
			# (index,pos,size,originv,wH,windows,tb)
									= validateWindow wMetrics osdinfo wH windows tb
			  (behindPtr,windows)	= getStackBehindWindow index windows
			# isMDI					= getOSDInfoDocumentInterface osdinfo == MDI
			# wH					= {wH & whAtts = validateWindowActivateForWindowMenu` wId isMDI wH.whAtts}
			# (delayinfo,wPtr,osdinfo,wH,tb)
									= createAnyWindow wMetrics behindPtr wId pos size originv osdinfo wH tb
			# (wH,tb)				= validateWindowClipState wMetrics True wPtr wH tb
			# ioState				= ioStSetOSDInfo osdinfo ioState
			# ioState				= setIOToolbox (osInvalidateWindow wPtr tb) ioState
			= (delayinfo,wPtr,index,wH,windows,ioState)

createAnyWindow :: !OSWindowMetrics !OSWindowPtr !Id !Point2 !Size !Vector2 !OSDInfo !(WindowHandle .ls (PSt .l)) !*OSToolbox
								    -> (![DelayActivationInfo],!OSWindowPtr,!OSDInfo, !WindowHandle .ls (PSt .l), !*OSToolbox)
createAnyWindow wMetrics behindPtr wId {x,y} {w,h} originv osdinfo wH=:{whMode,whKind,whTitle,whWindowInfo,whAtts} tb
	| whKind==IsWindow
		# (delay_info,wPtr,hPtr,vPtr,osdinfo,wH,tb)
									= osCreateWindow wMetrics isResizable hInfo vInfo minSize (toTuple maxSize)
													 isClosable whTitle pos size getInitActiveControl (createWindowControls wMetrics)
													 (updateWindowControl wMetrics wId size)
													 osdinfo behindPtr wH tb
		  windowInfo				= {	windowInfo	& windowHScroll	= setScrollInfoPtr hScroll hPtr
				  									, windowVScroll	= setScrollInfoPtr vScroll vPtr
						  			  }
		  wH						= {wH & whWindowInfo=WindowInfo windowInfo}
		# (wH,tb)					= movewindowviewframe wMetrics originv {wPtr=wPtr,wId=wId,wActive=False} wH tb	// PA: check WIDS value
		# (delay_info`,tb)			= osShowWindow wPtr False tb
		# tb						= osSetWindowCursor wPtr (getWindowCursorAtt cursorAtt) tb
		= (delay_info++delay_info`,wPtr,osdinfo,wH,tb)
		with
			isResizable				= True
			windowInfo				= getWindowInfoWindowData whWindowInfo
			viewDomain				= windowInfo.windowDomain
			viewOrigin				= windowInfo.windowOrigin
			hScroll					= windowInfo.windowHScroll
			vScroll					= windowInfo.windowVScroll
			visScrolls				= osScrollbarsAreVisible wMetrics viewDomain (w,h) (isJust hScroll,isJust vScroll)
			{rright=w`,rbottom=h`}	= osGetWindowContentRect wMetrics visScrolls (sizeToRect {w=w,h=h})
			hRect					= osGetWindowHScrollRect wMetrics visScrolls (sizeToRect {w=w,h=h})
			vRect					= osGetWindowVScrollRect wMetrics visScrolls (sizeToRect {w=w,h=h})
			hInfo					= toScrollbarInfo hScroll hRect (viewDomain.rleft,viewOrigin.x,viewDomain.rright, w`)
			vInfo					= toScrollbarInfo vScroll vRect (viewDomain.rtop, viewOrigin.y,viewDomain.rbottom,h`)
			minSize					= osMinWindowSize
			maxSize					= rectSize viewDomain
			(_,cursorAtt)			= cselect isWindowCursor (WindowCursor StandardCursor) whAtts
			
			toScrollbarInfo :: !(Maybe ScrollInfo) OSRect (Int,Int,Int,Int) -> ScrollbarInfo
			toScrollbarInfo Nothing _ scrollState
				= {cbiHasScroll=False,cbiPos=undef,cbiSize=undef,cbiState=undef}
			toScrollbarInfo (Just {scrollItemPos,scrollItemSize}) rect (min,origin,max,size)
				= {cbiHasScroll=True,cbiPos=toTuple scrollItemPos`,cbiSize=toTuple scrollItemSize`,cbiState=osScrollState}
			where
				osScrollState		= toOSscrollbarRange (min,origin,max) size
				scrollItemPos`		= {x=rect.rleft, y=rect.rtop}
				scrollItemSize`		= rectSize rect
			
			setScrollInfoPtr :: !(Maybe ScrollInfo) !OSWindowPtr -> Maybe ScrollInfo
			setScrollInfoPtr (Just info) scrollPtr	= Just {info & scrollItemPtr=scrollPtr}
			setScrollInfoPtr nothing _				= nothing
	| whKind==IsDialog
		# (delay_info,wPtr,wH,tb)	= osCreateDialog isModal isClosable whTitle pos size behindPtr
										getInitActiveControl (createWindowControls wMetrics)
										(updateWindowControl wMetrics wId size)
										osdinfo wH tb
		= (delay_info,wPtr,osdinfo,wH,tb)
	with
		isModal		= whMode==Modal
where
	isClosable		= contains isWindowClose whAtts
	pos				= (x,y)
	size			= (w,h)
	
	// createWindowControls creates the controls.
	createWindowControls :: !OSWindowMetrics !OSWindowPtr !(WindowHandle .ls (PSt .l)) !*OSToolbox -> (!WindowHandle .ls (PSt .l),!*OSToolbox)
	createWindowControls wMetrics wPtr wH=:{whDefaultId,whCancelId,whSelect,whItems=itemHs} tb
		# (itemHs,tb)	= createControls wMetrics whDefaultId whCancelId whSelect wPtr itemHs tb
		= ({wH & whItems=itemHs},tb)
	
	// updateWindowControl updates customised controls.
	updateWindowControl :: !OSWindowMetrics !Id !(!Int,!Int) !OSWindowPtr !OSWindowPtr !OSPictContext !(WindowHandle .ls (PSt .l)) !*OSToolbox
																								   -> (!WindowHandle .ls (PSt .l), !*OSToolbox)
	updateWindowControl wMetrics wId (w,h) wPtr cPtr osPict wH=:{whItems=itemHs} tb
		#! (_,controls,itemHs)	= getUpdateControls cPtr zero (sizeToRect {w=w,h=h}) itemHs
		#! wH					= {wH & whItems=itemHs}
		# updateInfo			= {	updWIDS			= {wPtr=wPtr,wId=wId,wActive=False}	// PA: check WIDS value
								  ,	updWindowArea	= zero
								  ,	updControls		= controls
								  ,	updGContext		= Just osPict
								  }
		= updatewindow wMetrics updateInfo wH tb
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
			getUpdateControl cPtr parentPos clipRect (WItemHandle itemH=:{wItemPtr,wItemNr,wItemPos,wItemSize,wItems})
				| cPtr==wItemPtr
					= (True, [{cuItemNr=wItemNr,cuItemPtr=wItemPtr,cuArea=clipRect1}],WItemHandle itemH)
				| otherwise
					# (found,controls,itemHs)	= getUpdateControls cPtr absolutePos clipRect1 wItems
					= (found,controls,WItemHandle {itemH & wItems=itemHs})
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

getStackBehindWindow :: !Index !(WindowHandles .pst) -> (!OSWindowPtr,!WindowHandles .pst)
getStackBehindWindow 0 wsHs
	= (OSNoWindowPtr,wsHs)
getStackBehindWindow index wsHs=:{whsWindows}
	# (before,[wsH:after])	= splitAt (index-1) whsWindows
	# ({wPtr},wsH)			= getWindowStateHandleWIDS wsH
	= (wPtr,{wsHs & whsWindows=before++[wsH:after]})


/*	bufferDelayedEvents buffers the events in the OSEvents environment.
*/
bufferDelayedEvents :: ![DelayActivationInfo] !(IOSt .l) -> IOSt .l
bufferDelayedEvents delayinfo ioState
	# (osEvents,ioState)	= ioStGetEvents ioState
	# (delayEvents,ioState)	= accIOToolbox (strictSeqList (map toOSEvent delayinfo)) ioState
	  osEvents				= osAppendEvents delayEvents osEvents
	= ioStSetEvents osEvents ioState
where
	toOSEvent :: !DelayActivationInfo !*OSToolbox -> (!OSEvent,!*OSToolbox)
	toOSEvent (DelayActivatedWindow wPtr) tb
		= createOSActivateWindowEvent wPtr tb
	toOSEvent (DelayDeactivatedWindow wPtr) tb
		= createOSDeactivateWindowEvent wPtr tb
	toOSEvent (DelayActivatedControl wPtr cPtr) tb
		= createOSActivateControlEvent wPtr cPtr tb
	toOSEvent (DelayDeactivatedControl wPtr cPtr) tb
		= createOSDeactivateControlEvent wPtr cPtr tb


/*	WindowBound-checks for normal windows.
*/
checkZeroWindowBound :: !(IOSt .l) -> (!Bool,!IOSt .l)
checkZeroWindowBound ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (False,ioState)
	| otherwise
		# wHs					= windowSystemStateGetWindowHandles wDevice
		  (isZero,wHs)			= checkZeroWindowHandlesBound wHs
		# ioState				= ioStSetDevice (WindowSystemState wHs) ioState
		= (isZero,ioState)

decreaseWindowBound :: !(IOSt .l) -> IOSt .l
decreaseWindowBound ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	| otherwise
		# wHs					= windowSystemStateGetWindowHandles wDevice
		  wHs					= decreaseWindowHandlesBound wHs
		# ioState				= ioStSetDevice (WindowSystemState wHs) ioState
		= ioState
