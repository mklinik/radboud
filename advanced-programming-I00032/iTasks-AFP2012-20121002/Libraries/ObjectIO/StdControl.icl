implementation module StdControl


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, controlaccess, controlinternal, controlvalidate, id, iostate, StdControlClass, windowaccess, windowcontrols, wstate
import	ostoolbox, oswindow
from	controllayout	import calcControlsSize
from	receiverid		import unbindRIds
from	StdPSt			import appPIO
from	windowclipstate	import invalidateWindowClipState`, forceValidWindowClipState`
from	windowupdate	import updatewindow
from	wstateaccess	import iswindowitemspace`, getwindowitemspace`,
								iswindowhmargin`,  getwindowhmargin`,
								iswindowvmargin`,  getwindowvmargin`


StdControlFatalError :: String String -> .x
StdControlFatalError function error
	= fatalError function "StdControl" error

/*	The function isOkControlId can be used to filter out the proper IdParent records.
*/
isOkControlId :: !SystemId !(.x,!Maybe IdParent) -> (!Bool,(.x,Id))
isOkControlId ioId (x,Just {idpIOId,idpDevice,idpId})
	= (ioId==idpIOId && idpDevice==WindowDevice,(x,idpId))
isOkControlId _ _
	= (False,undef)

// PA: two locally used functions that retrieve the parent Id(s).
ioStGetIdParent :: !Id !(IOSt .l) -> (!Maybe IdParent,!IOSt .l)
ioStGetIdParent id ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (idparent,idtable)	= getIdParent id idtable
	# ioState				= ioStSetIdTable idtable ioState
	= (idparent,ioState)

ioStGetIdParents:: ![Id] !(IOSt .l) -> (![Maybe IdParent],!IOSt .l)
ioStGetIdParents ids ioState
	# (idtable,ioState)		= ioStGetIdTable ioState
	# (idparents,idtable)	= getIdParents ids idtable
	# ioState				= ioStSetIdTable idtable ioState
	= (idparents,ioState)


/*	gatherWindowIds collects all first Ids (ControlId) that belong to the same second Id (WindowId).
	gatherWindowIds` does the same, except that not only ControlIds are collected, but also their data item.
*/
gatherWindowIds :: ![(Id,Id)] -> [([Id],Id)]
gatherWindowIds [(cId,wId):ids]
	= [([cId:cIds],wId):cIds_wIds]
where
	(cIds,ids`)	= gatherControlsIds wId ids
	cIds_wIds	= gatherWindowIds ids`
	
	gatherControlsIds :: !Id ![(Id,Id)] -> ([Id],[(Id,Id)])
	gatherControlsIds wId [(cId,wId`):ids]
		| wId==wId`	= ([cId:cIds],ids`)
		| otherwise	= (cIds,[(cId,wId`):ids`])
	where
		(cIds,ids`)	= gatherControlsIds wId ids
	gatherControlsIds _ _
		= ([],[])
gatherWindowIds []
	= []

gatherWindowIds` :: ![((Id,.x),Id)] -> [([(Id,.x)],Id)]
gatherWindowIds` [((cId,x),wId):ids]
	= [([(cId,x):cIds],wId):cIds_wIds]
where
	(cIds,ids`)	= gatherControlsIds wId ids
	cIds_wIds	= gatherWindowIds` ids`
	
	gatherControlsIds :: !Id ![((Id,.x),Id)] -> ([(Id,.x)],[((Id,.x),Id)])
	gatherControlsIds wId [((cId,x),wId`):ids]
		| wId==wId`	= ([(cId,x):cIds],ids`)
		| otherwise	= (cIds,[((cId,x),wId`):ids`])
	where
		(cIds,ids`)	= gatherControlsIds wId ids
	gatherControlsIds _ _
		= ([],[])
gatherWindowIds` []
	= []


//	The WState window representation record:

::	WState
	=	{	wIds	:: !WIDS
		,	wRep	:: !WindowHandle`
		,	wTb		:: !.OSToolbox
		,	wMetrics:: !OSWindowMetrics
		}

::	WState2
	=	{	wIds2	:: !WIDS
		,	wRep2	:: !WindowHandle2
		,	wTb2		:: !.OSToolbox
		,	wMetrics2:: !OSWindowMetrics
		}

getWindow :: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
getWindow windowId ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	# windows					= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not found
		= (Nothing,ioStSetDevice (WindowSystemState windows) ioState)
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		= (Just {wIds=wids,wRep=wsH`,wTb=OSNewToolbox,wMetrics=wMetrics},ioState)

getParentWindow :: !Id !(IOSt .l) -> (!Maybe WState, !IOSt .l)
getParentWindow controlId ioState
	# (maybeParent,ioState)		= ioStGetIdParent controlId ioState
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent					= fromJust maybeParent
	# (ioId,ioState)			= ioStGetIOId ioState
	| ioId==parent.idpIOId && parent.idpDevice==WindowDevice
		= getWindow parent.idpId ioState
	| otherwise
		= (Nothing,ioState)

setWindow :: !Id !(IdFun *WState) !(IOSt .l) -> IOSt .l
setWindow windowId f ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not found
		= ioStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# {wRep=wsH`,wTb=tb}	= f {wIds=wids,wRep=wsH`,wTb=tb,wMetrics=wMetrics}
		  wsH					= insertWindowHandle` wsH` wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		= ioState

setWindow2 :: !Id !(IdFun *WState2) !(IOSt .l) -> IOSt .l
setWindow2 windowId f ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,windows)		= getWindowHandlesWindow (toWID windowId) windows
	| not found
		= ioStSetDevice (WindowSystemState windows) ioState
	| otherwise
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle2 wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# {wRep2=wsH`,wTb2=tb}	= f {wIds2=wids,wRep2=wsH`,wTb2=tb,wMetrics2=wMetrics}
		  wsH					= insertWindowHandle2 wsH` wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		= ioState


/*	controlSize calculates the size of the given control.
*/
controlSize :: !.(cdef .ls (PSt .l)) !Bool !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(Maybe (Int,Int)) !(PSt .l)
			-> (!Size,!PSt .l) | Controls cdef
controlSize cdef isWindow hMargins vMargins itemSpaces pState
	# (cs,pState)		= controlToHandles cdef pState
	  itemHs			= map controlStateToWElementHandle cs
	# (tb,ioState)		= getIOToolbox pState.io
	# (wMetrics,ioState)= ioStGetOSWindowMetrics ioState
	  hMargins			= case hMargins of
		  					(Just (left,right))	-> (max 0 left,max 0 right)
		  					_					-> if isWindow (0,0) (wMetrics.osmHorMargin,wMetrics.osmHorMargin)
	  vMargins			= case vMargins of
		  					(Just (top,bottom))	-> (max 0 top,max 0 bottom)
		  					_					-> if isWindow (0,0) (wMetrics.osmVerMargin,wMetrics.osmVerMargin)
	  itemSpaces		= case itemSpaces of
		  					(Just (hor,vert))	-> (max 0 hor,max 0 vert)
		  					_					-> (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
	  domain			= {viewDomainRange & corner1=zero}
//	# (derSize,_,tb)	= layoutControls wMetrics hMargins vMargins itemSpaces zero zero [(domain,zero)] itemHs tb
	# (derSize,tb)		= calcControlsSize wMetrics hMargins vMargins itemSpaces zero zero [(domain,zero)] itemHs tb
	# ioState			= setIOToolbox tb ioState
	# pState			= {pState & io=ioState}
	= (derSize,pState)


/*	openControls adds controls to the indicated window.
*/
openControls :: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls cdef
openControls wId ls newControls pState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice pState.io
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# wHs						= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetDevice (WindowSystemState wHs) ioState})
    // Mike //
    # (wKind,wsH)				= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    	= (OtherError "WrongObject",{pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState})
    ///
	# (cs,pState)				= controlToHandles newControls {pState & io=ioState}
	# newItemHs					= map controlStateToWElementHandle cs
	  (currentIds,wsH)			= getWindowStateHandleIds wsH
	  (disjoint,newItemHs)		= disjointControlIds currentIds newItemHs
	| not disjoint
		= (ErrorIdsInUse,appPIO (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs))) pState)
	# (rt,ioState)				= ioStGetReceiverTable pState.io
	# (it,ioState)				= ioStGetIdTable ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	  (ok,newItemHs,rt,it)		= controlIdsAreConsistent ioId wId newItemHs rt it
	# ioState					= ioStSetIdTable it ioState
	# ioState					= ioStSetReceiverTable rt ioState
	| not ok
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (ErrorIdsInUse,pState)
	| otherwise
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
// DvA...		
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# wPtr					= wids.wPtr
		# (wsH,ioState)			= accIOToolbox (accGrafport wPtr (opencontrols wMetrics ls newItemHs wsH)) ioState
// ...DvA
//		# (wsH,ioState)			= accIOToolbox (opencontrols wMetrics ls newItemHs wsH) ioState
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (NoError,pState)

/*	getWindowStateHandleIds returns all Ids of the controls in this window.
	This function is used by open(Compound)Controls.
*/
getWindowStateHandleIds :: !(WindowStateHandle .pst) -> (![Id],!WindowStateHandle .pst)
getWindowStateHandleIds wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems}}}
	# (ids,itemHs)	= getWElementControlIds whItems
	= (ids,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=itemHs}}})
getWindowStateHandleIds _
	= StdControlFatalError "getWindowStateHandleIds" "unexpected window placeholder argument"

/*	getParentWindowId controlId returns the Id of the parent window/dialog if this
	exists and belongs to the same interactive process. 
	This function is used by openCompoundControls, openPopUpControlItems, closePopUpControlItems.
*/
getParentWindowId :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
getParentWindowId controlId ioState
	# (maybeParent,ioState)	= ioStGetIdParent controlId ioState
	| isNothing maybeParent
		= (Nothing,ioState)
	# parent				= fromJust maybeParent
	| parent.idpDevice<>WindowDevice
		= (Nothing,ioState)
	# (pid,ioState)			= ioStGetIOId ioState
	| parent.idpIOId<>pid
		= (Nothing,ioState)
	| otherwise
		= (Just parent.idpId,ioState)

/*	openCompoundControls adds controls to the indicated CompoundControl of the indicated window.
	THIS FUNCTION IS DEPRECATED. USE openRecursiveControls INSTEAD WITH EXACTLY THE SAME ARGUMENTS.
*/
openCompoundControls :: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls cdef
openCompoundControls cId ls newControls pState=:{io=ioState}
	# (maybeId,ioState)			= getParentWindowId cId ioState
	| isNothing maybeId
		= (ErrorUnknownObject,{pState & io=ioState})
	# wId						= fromJust maybeId
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# wHs						= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetDevice (WindowSystemState wHs) ioState})
    // Mike //
    # (wKind,wsH)				= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
		= (OtherError "WrongObject",{pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState})
    ///
	# (cs,pState)				= controlToHandles newControls {pState & io=ioState}
	# newItemHs					= map controlStateToWElementHandle cs
	  (currentIds,wsH)			= getWindowStateHandleIds wsH
	  (disjoint,newItemHs)		= disjointControlIds currentIds newItemHs
	| not disjoint
		= (ErrorIdsInUse,appPIO (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs))) pState)
	# (rt,ioState)				= ioStGetReceiverTable pState.io
	# (it,ioState)				= ioStGetIdTable ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	  (ok,newItemHs,rt,it)		= controlIdsAreConsistent ioId wId newItemHs rt it
	# ioState					= ioStSetIdTable it ioState
	# ioState					= ioStSetReceiverTable rt ioState
	| not ok
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (ErrorIdsInUse,pState)
	| otherwise
		# (osdInfo, ioState)	= ioStGetOSDInfo ioState
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (ok,wsH,tb)			= opencompoundcontrols osdInfo wMetrics cId ls newItemHs wsH tb
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (if ok NoError ErrorUnknownObject,pState)

/*	openRecursiveControls adds controls to the indicated LayoutControl.
*/
openRecursiveControls :: !Id .ls .(cdef .ls (PSt .l)) !(PSt .l) -> (!ErrorReport,!PSt .l) | Controls cdef
openRecursiveControls cId ls newControls pState=:{io=ioState}
	# (maybeId,ioState)			= getParentWindowId cId ioState
	| isNothing maybeId
		= (ErrorUnknownObject,{pState & io=ioState})
	# wId						= fromJust maybeId
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (ErrorUnknownObject,{pState & io=ioState})
	# wHs						= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (ErrorUnknownObject,{pState & io=ioStSetDevice (WindowSystemState wHs) ioState})
    // Mike //
    # (wKind,wsH)				= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
		= (OtherError "WrongObject",{pState & io=ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState})
    ///
	# (cs,pState)				= controlToHandles newControls {pState & io=ioState}
	# newItemHs					= map controlStateToWElementHandle cs
	  (currentIds,wsH)			= getWindowStateHandleIds wsH
	  (disjoint,newItemHs)		= disjointControlIds currentIds newItemHs
	| not disjoint
		= (ErrorIdsInUse,appPIO (ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs))) pState)
	# (rt,ioState)				= ioStGetReceiverTable pState.io
	# (it,ioState)				= ioStGetIdTable ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	  (ok,newItemHs,rt,it)		= controlIdsAreConsistent ioId wId newItemHs rt it
	# ioState					= ioStSetIdTable it ioState
	# ioState					= ioStSetReceiverTable rt ioState
	| not ok
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (ErrorIdsInUse,pState)
	| otherwise
		# (osdInfo, ioState)	= ioStGetOSDInfo ioState
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (ok,wsH,tb)			= openrecursivecontrols osdInfo wMetrics cId ls newItemHs wsH tb
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
		# pState				= {pState & io=ioState}
		= (if ok NoError ErrorUnknownObject,pState)

/*	openPopUpControlItems opens items to the PopUpControl of the indicated window/dialogue.
*/
openPopUpControlItems :: !Id !Index ![PopUpControlItem (PSt .l)] !(IOSt .l) -> IOSt .l
openPopUpControlItems popUpId index items ioState
	| isEmpty items
		= ioState
	# (maybeId,ioState)			= getParentWindowId popUpId ioState
	| isNothing maybeId
		= ioState
	# wId						= fromJust maybeId
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs						= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= ioStSetDevice (WindowSystemState wHs) ioState
    // Mike //
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind==IsGameWindow
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
    ///
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH,tb)				= openpopupcontrolitems popUpId index items wsH tb
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
where
	openpopupcontrolitems :: !Id !Index ![PopUpControlItem (PSt .l)] !(WindowStateHandle (PSt .l)) !*OSToolbox
																	 -> (!WindowStateHandle (PSt .l), !*OSToolbox)
	openpopupcontrolitems popUpId index items wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)		= openpopupitems popUpId index items wPtr wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	openpopupcontrolitems _ _ _ _ _
		= StdControlFatalError "openPopUpControlItems" "unexpected window placeholder argument"


/*	closeControls closes the controls in the indicated window.
*/
closeControls :: !Id [Id] !Bool !(IOSt .l) -> IOSt .l
closeControls wId ids relayout ioState
	# (found,wDevice,ioState)					= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs										= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)							= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= ioStSetDevice (WindowSystemState wHs) ioState
    // Mike //
    # (wKind,wsH)								= getWindowStateHandleWindowKind wsH
    | wKind==IsGameWindow
    	= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
    ///
	| otherwise
		# (wMetrics,ioState)					= ioStGetOSWindowMetrics ioState
		# (tb,ioState)							= getIOToolbox ioState
		# (freeRIds,freeIds,disposeFun,wsH,tb)	= closecontrols wMetrics ids relayout wsH tb
		# ioState								= setIOToolbox tb ioState
		# ioState								= unbindRIds freeRIds ioState
		# (idtable,ioState)						= ioStGetIdTable ioState
		  (_,idtable)							= removeIdsFromIdTable (freeRIds++freeIds) idtable
		# ioState								= ioStSetIdTable idtable ioState
		# (f,ioState)							= ioStGetInitIO ioState
		# ioState								= ioStSetInitIO ((appPIO (appIOToolbox disposeFun)) o f) ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState


/*	closeAllControls closes all controls from the indicated window.
*/
closeAllControls :: !Id !(IOSt .l) -> IOSt .l
closeAllControls wId ioState
	# (found,wDevice,ioState)					= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs										= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)							= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= ioStSetDevice (WindowSystemState wHs) ioState
	| otherwise
		# (tb,ioState)							= getIOToolbox ioState
		# (freeRIds,freeIds,disposeFun,wsH,tb)	= closeallcontrols wsH tb
		# ioState								= setIOToolbox tb ioState
		# ioState								= unbindRIds freeRIds ioState
		# (idtable,ioState)						= ioStGetIdTable ioState
		  (_,idtable)							= removeIdsFromIdTable (freeRIds++freeIds) idtable
		# ioState								= ioStSetIdTable idtable ioState
		# (f,ioState)							= ioStGetInitIO ioState
		# ioState								= ioStSetInitIO ((appPIO (appIOToolbox disposeFun)) o f) ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState


/*	closePopUpControlItems closes items from the indicated PopUpControl in the indicated window/dialogue.
*/
closePopUpControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
closePopUpControlItems popUpId indexs ioState
	| isEmpty indexs
		= ioState
	# (maybeId,ioState)			= getParentWindowId popUpId ioState
	| isNothing maybeId
		= ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# wHs						= windowSystemStateGetWindowHandles wDevice
	  (found,wsH,wHs)			= getWindowHandlesWindow (toWID (fromJust maybeId)) wHs
	| not found
		= ioStSetDevice (WindowSystemState wHs) ioState
	| otherwise
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH,tb)				= closepopupcontrolitems popUpId indexs wsH tb
		# ioState				= setIOToolbox tb ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState
where
	closepopupcontrolitems :: !Id ![Index] !(WindowStateHandle (PSt .l)) !*OSToolbox
										-> (!WindowStateHandle (PSt .l), !*OSToolbox)
	closepopupcontrolitems popUpId indexs wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
		# (wH,tb)		= closepopupitems popUpId indexs wPtr wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	closepopupcontrolitems _ _ _ _
		= StdControlFatalError "closepopupcontrolitems" "unexpected window placeholder argument"


/*	setControlPos changes the position of the indicated control.
*/
setControlPos :: !Id ![(Id,ItemPos)] !(IOSt .l) -> (!Bool,!IOSt .l)
setControlPos wId newPoss ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (False,ioState)
	# wHs						= windowSystemStateGetWindowHandles wDevice
	# (found,wsH,wHs)			= getWindowHandlesWindow (toWID wId) wHs
	| not found
		= (False,ioStSetDevice (WindowSystemState wHs) ioState)
	// Mike //
	# (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind==IsGameWindow
		= (False,ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH wHs)) ioState)
	///
	| otherwise
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (ok,wsH,tb)			= setcontrolpositions wMetrics newPoss wsH tb
		# ioState				= setIOToolbox tb ioState
		  wHs					= setWindowHandlesWindow wsH wHs
		= (ok,ioStSetDevice (WindowSystemState wHs) ioState)


//	Show/Hide controls.

showControls :: ![Id] !(IOSt .l) -> IOSt .l
showControls ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  cIds_wIds				= filterMap (isOkControlId ioId) (zip2 ids idparents)
	  cIds_wIds				= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds		= ioState
	| otherwise				= strictSeq [setWindow wId (setControlsShowState` True cIds) \\ (cIds,wId)<-cIds_wIds] ioState

showControl :: !Id !(IOSt .l) -> IOSt .l
showControl id ioState = showControls [id] ioState

hideControls :: ![Id] !(IOSt .l) -> IOSt .l
hideControls ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  cIds_wIds				= filterMap (isOkControlId ioId) (zip2 ids idparents)
	  cIds_wIds				= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds		= ioState
	| otherwise				= strictSeq [setWindow wId (setControlsShowState` False cIds) \\ (cIds,wId)<-cIds_wIds] ioState

hideControl :: !Id !(IOSt .l) -> IOSt .l
hideControl id ioState = hideControls [id] ioState

setControlsShowState` :: !Bool ![Id] !*WState -> *WState
setControlsShowState` show ids wState=:{wIds,wRep,wTb,wMetrics}
	# (wH,tb)	= setcontrolsshowstate ids show wMetrics wIds wRep wTb
//	  wH		= invalidateWindowClipState` wH
	# (wH,tb)	= forceValidWindowClipState` wMetrics True wIds.wPtr wH tb
	= {wState & wRep=wH,wTb=tb}


/*	Enabling/Disabling of controls.
*/
enableControls :: ![Id] !(IOSt .l) -> IOSt .l
enableControls ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  cIds_wIds				= filterMap (isOkControlId ioId) (zip2 ids idparents)
	  cIds_wIds				= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds		= ioState
	| otherwise				= strictSeq [setWindow wId (enableControls` cIds) \\ (cIds,wId)<-cIds_wIds] ioState
where
	enableControls` :: ![Id] !*WState -> *WState
	enableControls` ids wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= enablecontrols ids False wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

enableControl :: !Id !(IOSt .l) -> IOSt .l
enableControl id ioState = enableControls [id] ioState

disableControls :: ![Id] !(IOSt .l) -> IOSt .l
disableControls ids ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents ids ioState
	  cIds_wIds				= filterMap (isOkControlId ioId) (zip2 ids idparents)
	  cIds_wIds				= gatherWindowIds cIds_wIds
	| isEmpty cIds_wIds		= ioState
	| otherwise				= strictSeq [setWindow wId (disableControls` cIds) \\ (cIds,wId)<-cIds_wIds] ioState
where
	disableControls` :: ![Id] !*WState -> *WState
	disableControls` ids wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)			= disablecontrols ids False wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

disableControl :: !Id !(IOSt .l) -> IOSt .l
disableControl id ioState = disableControls [id] ioState


//	Marking/Unmarking of check controls.

markCheckControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
markCheckControlItems cId indexs ioState
	= setControlsMarkState Mark cId indexs ioState

unmarkCheckControlItems :: !Id ![Index] !(IOSt .l) -> IOSt .l
unmarkCheckControlItems cId indexs ioState
	= setControlsMarkState NoMark cId indexs ioState

setControlsMarkState :: !MarkState !Id ![Index] !(IOSt .l) -> IOSt .l
setControlsMarkState mark cId indexs ioState
	| isEmpty indexs
		= ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlsMarkState` mark cId indexs) ioState
where
	setControlsMarkState` :: !MarkState !Id ![Index] !*WState -> *WState
	setControlsMarkState` mark id indexs wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= setcontrolsmarkstate id mark indexs wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Selecting/Unselecting a radio control.

selectRadioControlItem :: !Id !Index !(IOSt .l) -> IOSt .l
selectRadioControlItem cId index ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (selectRadioControlItem` cId index) ioState
where
	selectRadioControlItem` :: !Id !Index !*WState -> *WState
	selectRadioControlItem` id index wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)			= selectradiocontrol id index wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Select a pop up menu item.

selectPopUpControlItem :: !Id !Index !(IOSt .l) -> IOSt .l
selectPopUpControlItem cId index ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (selectPopUpControlItem` cId index) ioState
where
	selectPopUpControlItem` :: !Id !Index !*WState -> *WState
	selectPopUpControlItem` id index wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)		= selectpopupitem id index wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Move the orientation of a CompoundControl.

moveControlViewFrame :: !Id Vector2 !(IOSt .l) -> IOSt .l
moveControlViewFrame cId v ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (moveControlViewFrame` cId v) ioState
where
	moveControlViewFrame` :: !Id !Vector2 !*WState -> *WState
	moveControlViewFrame` id v wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= movecontrolviewframe id v wMetrics wIds wRep wTb
	//	  wH		= invalidateWindowClipState` wH		PA: seems to me that this is a bit exagerated
		= {wState & wRep=wH,wTb=tb}


//	Set a new view domain of a CompoundControl.

setControlViewDomain :: !Id ViewDomain !(IOSt .l) -> IOSt .l
setControlViewDomain cId newDomain ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlViewDomain` cId newDomain) ioState
where
	setControlViewDomain` :: !Id !ViewDomain !*WState -> *WState
	setControlViewDomain` id newDomain wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= setcontrolviewdomain id newDomain wMetrics wIds wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Set the ScrollFunction of a CompoundControl.

setControlScrollFunction :: !Id Direction ScrollFunction !(IOSt .l) -> IOSt .l
setControlScrollFunction cId direction scrollFun ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlScrollFunction` cId direction scrollFun) ioState
where
	setControlScrollFunction` :: !Id !Direction !ScrollFunction !*WState -> *WState
	setControlScrollFunction` id direction scrollFun wState=:{wRep}
		# wH			= setcontrolscrollfun id direction scrollFun wRep
		= {wState & wRep=wH}


//	Set the complete size of a (Compound/Custom(Button)/Layout)Control.

setControlOuterSize :: !Id Size Bool !(IOSt .l) -> IOSt .l
setControlOuterSize cId newSize relayout ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlOuterSize` cId newSize relayout) ioState
where
	setControlOuterSize` :: !Id !Size !Bool !*WState -> *WState
	setControlOuterSize` id newSize relayout wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= setcontroloutersize id newSize relayout wMetrics wIds wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Set the width of a (Button/Edit/PopUp/Text)Control.

setControlWidth :: !Id ControlWidth Bool !(IOSt .l) -> IOSt .l
setControlWidth cId newWidth relayout ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setControlWidth` cId newWidth relayout) ioState
where
	setControlWidth` :: !Id !ControlWidth !Bool !*WState -> *WState
	setControlWidth` id newWidth relayout wState=:{wIds,wRep,wTb,wMetrics}
		# (wH,tb)	= setcontrolwidth id newWidth relayout wMetrics wIds wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Change the text of (Text/Edit/Button)Control.

setControlTexts :: ![(Id,String)] !(IOSt .l) -> IOSt .l
setControlTexts cid_texts ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	# (idparents,ioState)		= ioStGetIdParents cids ioState
	  cid_texts_wIds			= filterMap (isOkControlId ioId) (zip2 cid_texts idparents)
	  cid_texts_wIds			= gatherWindowIds` cid_texts_wIds
	| isEmpty cid_texts_wIds
		= ioState
	| otherwise
		= strictSeq [setWindow2 wId (setControlTexts` cid_texts) \\ (cid_texts,wId)<-cid_texts_wIds] ioState
where
	(cids,_)					= unzip cid_texts
	
	setControlTexts` :: ![(Id,String)] !*WState2 -> *WState2
	setControlTexts` texts wState=:{wIds2={wPtr},wRep2,wTb2,wMetrics2}
		# (wH,tb)	= setcontroltexts texts wMetrics2 wPtr wRep2 wTb2
		= {wState & wRep2=wH,wTb2=tb}

setControlText :: !Id !String !(IOSt .l) -> IOSt .l
setControlText id text ioState = setControlTexts [(id,text)] ioState


//	Set the cursor position of an EditControl.

setEditControlCursor :: !Id !Int !(IOSt .l) -> IOSt .l
setEditControlCursor cId pos ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setEditControlCursor` cId pos) ioState
where
	setEditControlCursor` :: !Id !Int !*WState -> *WState
	setEditControlCursor` id pos wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= seteditcontrolcursor id pos wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


//	Set the selection of an EditControl.

setEditControlSelection :: !Id !Int !Int !(IOSt .l) -> IOSt .l
setEditControlSelection cId begin end ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (maybeParent,ioState)	= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	| otherwise
		= setWindow (fromJust maybeParent).idpId (setEditControlSelection` cId begin` end`) ioState
where
	(begin`,end`)	= if (begin<=end) (begin,end) (0,-1)
	
	setEditControlSelection` :: !Id !Int !Int !*WState -> *WState
	setEditControlSelection` id begin end wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= seteditcontrolselection id begin end wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}


/*	Change the Look of the corresponding (Custom(Button)/Compound)Controls and redraw
	only if the first Boolean is True.
*/
setControlLooks :: ![(Id,Bool,(Bool,Look))] !(IOSt .l) -> IOSt .l
setControlLooks cid_looks ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	  cid_looks					= [(cid,(redraw,look)) \\ (cid,redraw,look)<-cid_looks]
	  (cids,_)					= unzip cid_looks
	# (idparents,ioState)		= ioStGetIdParents cids ioState
	  cid_looks_wIds			= filterMap (isOkControlId ioId) (zip2 cid_looks idparents)
	  cid_looks_wIds			= gatherWindowIds` cid_looks_wIds
	| isEmpty cid_looks_wIds
		= ioState
	| otherwise
		= strictSeq [	setWindow wId (setControlLooks` [(cid,redraw,look) \\ (cid,(redraw,look))<-cid_looks])
					\\	(cid_looks,wId)<-cid_looks_wIds
					]	ioState
where
	setControlLooks` :: ![(Id,Bool,(Bool,Look))] !*WState -> *WState
	setControlLooks` looks wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= setcontrolslook looks wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

setControlLook :: !Id !Bool (Bool,Look) !(IOSt .l) -> IOSt .l
setControlLook id redraw newlook ioState = setControlLooks [(id,redraw,newlook)] ioState


//	Change the SliderState and redraw the settings of the SliderControls.

setSliderStates :: ![(Id,IdFun SliderState)] !(IOSt .l) -> IOSt .l
setSliderStates cid_fs ioState
	# (ioId,ioState)		= ioStGetIOId ioState
	# (idparents,ioState)	= ioStGetIdParents cids ioState
	  cid_funs_wIds			= filterMap (isOkControlId ioId) (zip2 cid_fs idparents)
	  cid_funs_wIds			= gatherWindowIds` cid_funs_wIds
	| isEmpty cid_funs_wIds
		= ioState
	| otherwise
		= strictSeq [setWindow wId (setSliderStates` cid_funs) \\ (cid_funs,wId)<-cid_funs_wIds] ioState
where
	(cids,_)				= unzip cid_fs
	
	setSliderStates` :: ![(Id,IdFun SliderState)] !*WState -> *WState
	setSliderStates` id_fs wState=:{wIds={wPtr},wRep,wTb,wMetrics}
		# (wH,tb)	= setslidercontrolstates id_fs wMetrics wPtr wRep wTb
		= {wState & wRep=wH,wTb=tb}

setSliderState :: !Id (IdFun SliderState) !(IOSt .l) -> IOSt .l
setSliderState id fun ioState = setSliderStates [(id,fun)] ioState


//	Change the thumb value of the SliderState of a SliderControl. 

setSliderThumbs :: ![(Id,Int)] !(IOSt .l) -> IOSt .l
setSliderThumbs cid_thumbs ioState
	= setSliderStates (map (\(cid,thumb)->(cid,\state->{state & sliderThumb=thumb})) cid_thumbs) ioState

setSliderThumb :: !Id Int !(IOSt .l) -> IOSt .l
setSliderThumb id thumb ioState = setSliderThumbs [(id,thumb)] ioState


//	Draw in a (Custom(Button)/Compound)Control.

appControlPicture :: !Id !.(IdFun *Picture) !(IOSt .l) -> IOSt .l
appControlPicture cId drawfun ioState
	= snd (accControlPicture cId (\p->(undef,drawfun p)) ioState)

accControlPicture :: !Id !.(St *Picture .x) !(IOSt .l) -> (!Maybe .x,!IOSt .l)
accControlPicture cId drawfun ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	# (maybeParent,ioState)		= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= (Nothing,ioState)
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= (Nothing,ioState)
	| otherwise
		# windows				= windowSystemStateGetWindowHandles wDevice
		  wId					= (fromJust maybeParent).idpId
		  (_,wsH,windows)		= getWindowHandlesWindow (toWID wId) windows
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (tb,ioState)			= getIOToolbox ioState
		# (wsH`,wsH,tb)			= retrieveWindowHandle` wsH tb
		# (wids,wsH)			= getWindowStateHandleWIDS wsH
		# (maybe_result,wsH`,tb)= drawincontrol cId drawfun wMetrics wids.wPtr wsH` tb
		  wsH					= insertWindowHandle` wsH` wsH
		  windows				= setWindowHandlesWindow wsH windows
		# ioState				= setIOToolbox tb ioState
		# ioState				= ioStSetDevice (WindowSystemState windows) ioState
		= (maybe_result,ioState)

//	Update a selection of a (Compound/Custom(Button))Control:
updateControl :: !Id !(Maybe ViewFrame) !(IOSt .l) -> IOSt .l
updateControl cId maybeViewFrame ioState
	# (ioId,ioState)			= ioStGetIOId ioState
	# (maybeParent,ioState)		= ioStGetIdParent cId ioState
	| not (fst (isOkControlId ioId (cId,maybeParent)))
		= ioState
	# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
	| not found
		= ioState
	# windows					= windowSystemStateGetWindowHandles wDevice
	  wId						= (fromJust maybeParent).idpId
	  (_,wsH,windows)			= getWindowHandlesWindow (toWID wId) windows
	  (wKind,wsH)				= getWindowStateHandleWindowKind wsH
	| wKind<>IsWindow
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
	| otherwise
		# (wMetrics,ioState)	= ioStGetOSWindowMetrics ioState
		# (wsH,ioState)			= accIOToolbox (updateControlBackground wMetrics wKind cId maybeViewFrame wsH) ioState
		= ioStSetDevice (WindowSystemState (setWindowHandlesWindow wsH windows)) ioState
where
	updateControlBackground :: !OSWindowMetrics !WindowKind !Id !(Maybe ViewFrame) !(WindowStateHandle .pst) !*OSToolbox
																				-> (!WindowStateHandle .pst, !*OSToolbox)
	updateControlBackground wMetrics wKind cId maybeViewFrame wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whSize=whSize,whItems=itemHs}}} tb
		# (_,updInfo,itemHs)			= getWElementHandlesUpdateInfo wMetrics cId contentRect zero itemHs
		  wH							= {wH & whItems=itemHs}
		# tb							= strictSeq [osInvalidateWindowRect updInfo.updWIDS.wPtr cuArea \\ {cuArea} <- updInfo.updControls] tb // DvA
		# (wH,tb)						= updatewindow wMetrics updInfo wH tb
		= ({wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	where
		info							= getWindowInfoWindowData wH.whWindowInfo
		(domainRect,hasScrolls)			= case wKind of
											IsWindow -> (info.windowDomain,(isJust info.windowHScroll,isJust info.windowVScroll))
											_        -> (sizeToRect whSize,(False,False))
		visScrolls						= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
		contentRect						= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
		
		getWElementHandlesUpdateInfo :: !OSWindowMetrics !Id !OSRect !Point2 ![WElementHandle .ls .pst] -> (!Bool,UpdateInfo,![WElementHandle .ls .pst])
		getWElementHandlesUpdateInfo _ _ _ _ []
			= (False,undef,[])
		getWElementHandlesUpdateInfo wMetrics cId clipRect parentPos [itemH:itemHs]
			# (found,updInfo,itemH)		= getWElementHandleUpdateInfo wMetrics cId clipRect parentPos itemH
			| found
				= (found,updInfo,[itemH:itemHs])
			| otherwise
				# (found,updInfo,itemHs)= getWElementHandlesUpdateInfo wMetrics cId clipRect parentPos itemHs
				= (found,updInfo,[itemH:itemHs])
		where
			getWElementHandleUpdateInfo :: !OSWindowMetrics !Id !OSRect !Point2 !(WElementHandle .ls .pst) -> (!Bool,UpdateInfo,!WElementHandle .ls .pst)
			getWElementHandleUpdateInfo wMetrics cId clipRect parentPos (WItemHandle itemH=:{wItemId,wItemNr,wItemPtr,wItemKind,wItemPos,wItemSize,wItems,wItemInfo})
				| isNothing wItemId || cId<>fromJust wItemId
					| not (isRecursiveControl wItemKind)
						= (False,undef,WItemHandle itemH)
					// otherwise
						# (found,updInfo,itemHs)	= getWElementHandlesUpdateInfo wMetrics cId visRect absolutePos wItems
						= (found,updInfo,WItemHandle {itemH & wItems=itemHs})
				| isMember wItemKind [IsCompoundControl,IsCustomControl,IsCustomButtonControl]
					= (True,updInfo,WItemHandle itemH)
				| otherwise
					= (False,undef,WItemHandle itemH)
			where
				absolutePos				= movePoint wItemPos parentPos
				itemRect				= posSizeToRect absolutePos wItemSize
				compoundInfo			= getWItemCompoundInfo wItemInfo
				origin					= if (wItemKind==IsCompoundControl)
											compoundInfo.compoundOrigin
											zero
				domain					= compoundInfo.compoundDomain
				hasScrolls				= (isJust compoundInfo.compoundHScroll,isJust compoundInfo.compoundVScroll)
				visScrolls				= osScrollbarsAreVisible wMetrics domain (toTuple wItemSize) hasScrolls
				contentRect				= if (wItemKind==IsCompoundControl)
											(osGetCompoundContentRect wMetrics visScrolls itemRect)
											itemRect
				visRect					= intersectRects contentRect clipRect
				updArea							= case maybeViewFrame of
													Nothing		-> visRect
													Just rect	-> intersectRects (rectangleToRect (addVector (toVector absolutePos)
																								   (subVector (toVector origin) rect)
																								   )
																				  ) visRect
				updInfo							= {	updWIDS			= wshIds
												  ,	updWindowArea	= zero
												  ,	updControls		= [	{	cuItemNr	= wItemNr
																		,	cuItemPtr	= wItemPtr
																		,	cuArea		= updArea
																		}]
												  ,	updGContext		= Nothing
												  }
			getWElementHandleUpdateInfo wMetrics cId clipRect parentPos (WListLSHandle itemHs)
				# (found,updInfo,itemHs)= getWElementHandlesUpdateInfo wMetrics cId clipRect parentPos itemHs
				= (found,updInfo,WListLSHandle itemHs)
			getWElementHandleUpdateInfo wMetrics cId clipRect parentPos (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (found,updInfo,itemHs)= getWElementHandlesUpdateInfo wMetrics cId clipRect parentPos itemHs
				= (found,updInfo,WExtendLSHandle {wExH & wExtendItems=itemHs})
			getWElementHandleUpdateInfo wMetrics cId clipRect parentPos (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (found,updInfo,itemHs)= getWElementHandlesUpdateInfo wMetrics cId clipRect parentPos itemHs
				= (found,updInfo,WChangeLSHandle {wChH & wChangeItems=itemHs})
	updateControlBackground _ _ _ _ _ _
		= StdControlFatalError "updateControl" "unexpected window placeholder argument"


//	Access operations on WState:

getWStateControls :: !WState -> [WElementHandle`]
getWStateControls {wRep={whItems`}}
	= whItems`

getControlTypes :: !WState -> [(ControlType,Maybe Id)]
getControlTypes wstate
	= getcontrolstypes (getWStateControls wstate)

getCompoundTypes :: !Id !WState -> [(ControlType,Maybe Id)]
getCompoundTypes id wstate
	= getcompoundstypes id (getWStateControls wstate)


// snd3thd3	:: !(.a,.b,.c) -> (.b,.c)								// (t2,t3) of (t1,t2,t3)
snd3thd3 tuple :== (t2,t3) where (_,t2,t3) = tuple

getControlLayouts :: ![Id] !WState -> [(Bool,(Maybe ItemPos,Vector2))]
getControlLayouts ids wstate
	= map snd3thd3 (snd (getcontrolslayouts zero (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= (Nothing,zero)

getControlLayout :: !Id !WState -> (Bool,(Maybe ItemPos,Vector2))
getControlLayout id wstate = hd (getControlLayouts [id] wstate)

getControlViewSizes :: ![Id] !WState -> [(Bool,Size)]
getControlViewSizes ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsviewsizes wMetrics (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= zero

getControlViewSize :: !Id !WState -> (Bool,Size)
getControlViewSize id wstate = hd (getControlViewSizes [id] wstate)

getControlOuterSizes :: ![Id] !WState -> [(Bool,Size)]
getControlOuterSizes ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsoutersizes wMetrics (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= zero

getControlOuterSize :: !Id !WState -> (Bool,Size)
getControlOuterSize id wstate = hd (getControlOuterSizes [id] wstate)

getControlSelectStates :: ![Id] !WState -> [(Bool,SelectState)]
getControlSelectStates ids wstate
	= map snd3thd3 (snd (getcontrolsselects (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Able

getControlSelectState :: !Id !WState -> (Bool,SelectState)
getControlSelectState id wstate = hd (getControlSelectStates [id] wstate)

getControlShowStates :: ![Id] !WState -> [(Bool,Bool)]
getControlShowStates ids wstate
	= map snd3thd3 (snd (getcontrolsshowstates (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= False

getControlShowState :: !Id !WState -> (Bool,Bool)
getControlShowState id wstate = hd (getControlShowStates [id] wstate)

getControlTexts :: ![Id] !WState -> [(Bool,Maybe String)]
getControlTexts ids wstate
	= map snd3thd3 (snd (getcontrolstexts (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlText :: !Id !WState -> (Bool,Maybe String)
getControlText id wstate = hd (getControlTexts [id] wstate)

getControlNrLines :: ![Id] !WState -> [(Bool,Maybe NrLines)]
getControlNrLines ids wstate
	= map snd3thd3 (snd (getcontrolsnrlines (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlNrLine :: !Id !WState -> (Bool,Maybe NrLines)
getControlNrLine id wstate = hd (getControlNrLines [id] wstate)

getControlLooks :: ![Id] !WState -> [(Bool,Maybe (Bool,Look))]
getControlLooks ids wstate
	= map snd3thd3 (snd (getcontrolslooks (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlLook :: !Id !WState -> (Bool,Maybe (Bool,Look))
getControlLook id wstate = hd (getControlLooks [id] wstate)

getControlMinimumSizes :: ![Id] !WState -> [(Bool,Maybe Size)]
getControlMinimumSizes ids wstate
	= map snd3thd3 (snd (getcontrolsminsizes (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlMinimumSize :: !Id !WState -> (Bool,Maybe Size)
getControlMinimumSize id wstate = hd (getControlMinimumSizes [id] wstate)

getControlResizes :: ![Id] !WState -> [(Bool,Maybe ControlResizeFunction)]
getControlResizes ids wstate
	= map snd3thd3 (snd (getcontrolsresizes (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlResize :: !Id !WState -> (Bool,Maybe ControlResizeFunction)
getControlResize id wstate = hd (getControlResizes [id] wstate)

getRadioControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getRadioControlItems ids wstate
	= map snd3thd3 (snd (getradioitems (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getRadioControlItem :: !Id !WState -> (Bool,Maybe [String])
getRadioControlItem id wstate = hd (getRadioControlItems [id] wstate)

getRadioControlSelections :: ![Id] !WState -> [(Bool,Maybe Index)]
getRadioControlSelections ids wstate
	= map snd3thd3 (snd (getradiocontrolsmarks (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getRadioControlSelection :: !Id !WState -> (Bool,Maybe Index)
getRadioControlSelection id wstate = hd (getRadioControlSelections [id] wstate)

getCheckControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getCheckControlItems ids wstate
	= map snd3thd3 (snd (getcheckitems (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getCheckControlItem :: !Id !WState -> (Bool,Maybe [String])
getCheckControlItem id wstate = hd (getCheckControlItems [id] wstate)

getCheckControlSelections :: ![Id] !WState -> [(Bool,Maybe [Index])]
getCheckControlSelections ids wstate
	= map snd3thd3 (snd (getcheckcontrolsmarks (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getCheckControlSelection :: !Id !WState -> (Bool,Maybe [Index])
getCheckControlSelection id wstate = hd (getCheckControlSelections [id] wstate)

getPopUpControlItems :: ![Id] !WState -> [(Bool,Maybe [String])]
getPopUpControlItems ids wstate
	= map snd3thd3 (snd (getpopupitems (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getPopUpControlItem :: !Id !WState -> (Bool,Maybe [String])
getPopUpControlItem id wstate = hd (getPopUpControlItems [id] wstate)

getPopUpControlSelections :: ![Id] !WState -> [(Bool,Maybe Index)]
getPopUpControlSelections ids wstate
	= map snd3thd3 (snd (getselectedpopupitems (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getPopUpControlSelection :: !Id !WState -> (Bool,Maybe Index)
getPopUpControlSelection id wstate = hd (getPopUpControlSelections [id] wstate)

getSliderDirections :: ![Id] !WState -> [(Bool,Maybe Direction)]
getSliderDirections ids wstate
	= map snd3thd3 (snd (getslidersdirections (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getSliderDirection :: !Id !WState -> (Bool,Maybe Direction)
getSliderDirection id wstate = hd (getSliderDirections [id] wstate)

getSliderStates :: ![Id] !WState -> [(Bool,Maybe SliderState)]
getSliderStates ids wstate
	= map snd3thd3 (snd (getslidersstates (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getSliderState :: !Id !WState -> (Bool,Maybe SliderState)
getSliderState id wstate = hd (getSliderStates [id] wstate)

getControlViewFrames :: ![Id] !WState -> [(Bool,Maybe ViewFrame)]
getControlViewFrames ids wstate=:{wMetrics}
	= map snd3thd3 (snd (getcontrolsframes wMetrics (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlViewFrame :: !Id !WState -> (Bool,Maybe ViewFrame)
getControlViewFrame id wstate = hd (getControlViewFrames [id] wstate)

getControlViewDomains :: ![Id] !WState -> [(Bool,Maybe ViewDomain)]
getControlViewDomains ids wstate
	= map snd3thd3 (snd (getcontrolsdomains (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlViewDomain :: !Id !WState -> (Bool,Maybe ViewDomain)
getControlViewDomain id wstate = hd (getControlViewDomains [id] wstate)

getControlScrollFunctions :: ![Id] !WState -> [(Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))]
getControlScrollFunctions ids wstate
	= map snd3thd3 (snd (getscrollfunctions (getWStateControls wstate) (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing

getControlScrollFunction :: !Id !WState -> (Bool,Maybe ((Direction,Maybe ScrollFunction),(Direction,Maybe ScrollFunction)))
getControlScrollFunction id wstate = hd (getControlScrollFunctions [id] wstate)

getControlItemSpaces :: ![Id] !WState -> [(Bool,Maybe (Int,Int))]
getControlItemSpaces ids {wRep={whItems`,whAtts`},wMetrics={osmHorItemSpace,osmVerItemSpace}}
	= map snd3thd3 (snd (getcontrolsspaces spaces whItems` (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing
	spaces		= getwindowitemspace` (snd (cselect iswindowitemspace` (WindowItemSpace` osmHorItemSpace osmVerItemSpace) whAtts`))

getControlItemSpace :: !Id !WState -> (Bool,Maybe (Int,Int))
getControlItemSpace id wstate = hd (getControlItemSpaces [id] wstate)

getControlMargins :: ![Id] !WState -> [(Bool,Maybe ((Int,Int),(Int,Int)))]
getControlMargins ids {wRep={whKind`,whItems`,whAtts`},wMetrics={osmHorMargin,osmVerMargin}}
	= map snd3thd3 (snd (getcontrolsmargins (hMargins,vMargins) whItems` (ids,[(id,defaultBool,defaultValue) \\ id<-ids])))
where
	defaultBool	= False
	defaultValue= Nothing
	(hMargin,vMargin)
				= if (whKind`==IsDialog) (osmHorMargin,osmVerMargin) (0,0)
	hMargins	= getwindowhmargin` (snd (cselect iswindowhmargin` (WindowHMargin` hMargin hMargin) whAtts`))
	vMargins	= getwindowvmargin` (snd (cselect iswindowvmargin` (WindowVMargin` vMargin vMargin) whAtts`))

getControlMargin :: !Id !WState -> (Bool,Maybe ((Int,Int),(Int,Int)))
getControlMargin id wstate = hd (getControlMargins [id] wstate)
