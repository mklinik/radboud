implementation module windowcontrols


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, controlcreate, windowclipstate
from StdControlAttribute import isControlPos
from controllayout import layoutControls
from controlrelayout import relayoutControls
from	windowaccess		import identifyMaybeId, genWElementItemNrs, getWindowHMargins, getWindowVMargins, getWindowItemSpaces
from windowdispose import disposeWItemHandle
from windowdraw import drawwindowlook
from windowupdate import updatewindowbackgrounds
import	osdocumentinterface, oswindow


windowcontrolsFatalError :: String String -> .x
windowcontrolsFatalError function error
	= fatalError function "windowcontrols" error


/*	opencontrols adds the given controls to the window. 
	It is assumed that the new controls do not conflict with the current controls.
*/
opencontrols :: !OSWindowMetrics .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox -> (!WindowStateHandle .pst,!*OSToolbox)
opencontrols wMetrics ls newItems 
			 wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems,whSize,whAtts,whDefaultId,whCancelId,whSelect,whItemNrs,whKind,whWindowInfo}}} 
			 tb
	# (nrCurItems,curItems)		= ulength curItems
	  (itemNrs,newItems)		= genWElementItemNrs whItemNrs newItems
	  newItems					= [WChangeLSHandle {wChangeLS=ls,wChangeItems=newItems}]
	  allItems					= curItems++newItems
	  visScrolls				= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	  {rright=curw,rbottom=curh}= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)
	  hMargins					= getWindowHMargins   whKind wMetrics whAtts
	  vMargins					= getWindowVMargins   whKind wMetrics whAtts
	  spaces					= getWindowItemSpaces whKind wMetrics whAtts
	  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
	# (_,allItems,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] allItems tb
	  (curItems,newItems)		= split nrCurItems allItems
	# (newItems,tb)				= createControls wMetrics whDefaultId whCancelId whSelect wPtr newItems tb
	  allItems					= curItems++newItems
	  wH						= {wH & whItemNrs=itemNrs,whItems=allItems}
	  wH						= invalidateWindowClipState wH
	  wsH						= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
	= (wsH,tb)
where
	wPtr						= wshIds.wPtr
	(origin,hasHScroll,hasVScroll,domainRect)
								= case whWindowInfo of
									WindowInfo info	-> (info.windowOrigin,isJust info.windowHScroll,isJust info.windowVScroll,info.windowDomain)
									other			-> (zero,             False,                    False,                    sizeToRect whSize)
	domain						= rectToRectangle domainRect
opencontrols _ _ _ _ _
	= windowcontrolsFatalError "opencontrols" "unexpected window placeholder argument"


/*	opencompoundcontrols adds the given controls to the compound control of the given window. 
	It is assumed that the new controls do not conflict with the current controls.
*/
opencompoundcontrols :: !OSDInfo !OSWindowMetrics !Id .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																			-> (!Bool,!WindowStateHandle .pst, !*OSToolbox)
opencompoundcontrols osdInfo wMetrics compoundId ls newItems 
					 wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems,whAtts,whDefaultId,whCancelId,whSelect,whShow,whItemNrs,whKind,whSize,whWindowInfo}}} 
					 tb
	# (found,nrSkip,_,_,itemNrs,oldItemHs)
									= addControlsToCompound compoundId ls newItems whItemNrs whItems
	| not found
		= (False,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=oldItemHs}}},tb)
	| otherwise
		# (curw,curh)				= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  curSize					= {w=curw,h=curh}
		  wFrame					= sizeToRect curSize
		  hMargins					= getWindowHMargins   whKind wMetrics whAtts
		  vMargins					= getWindowVMargins   whKind wMetrics whAtts
		  spaces					= getWindowItemSpaces whKind wMetrics whAtts
		  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (oldItemHs`,oldItemHs,tb)	= getWElementHandles` wPtr oldItemHs tb
		# (derSize,newItemHs,tb)	= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs tb
	//	# tb						= checkNewWindowSize curSize derSize wPtr osdInfo tb	// PA: curSize might be bigger than domain, then you shouldn't resize!
		# (newItemHs,tb)			= createCompoundControls wMetrics compoundId nrSkip whDefaultId whCancelId whSelect wPtr newItemHs tb
		  wH						= {wH & whItemNrs=itemNrs,whItems=newItemHs}
		# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,newItemHs,tb)		= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (wFrame,zero,zero,oldItemHs`) (wFrame,zero,zero,wH.whItems) tb
		# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wshIds {wH & whItems=newItemHs} tb
		= (True,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	wPtr							= wshIds.wPtr
	domain							= rectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
									= case whWindowInfo of
										WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
										other			-> (zero,             sizeToRect whSize,False,False)
	(visHScroll,visVScroll)			= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	
	addControlsToCompound :: !Id .ls` ![WElementHandle .ls` .pst] [Int] ![WElementHandle .ls .pst]
				  -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int],![WElementHandle .ls .pst])
	addControlsToCompound _ ls newItems itemNrs []
		= (False,0,ls,newItems,itemNrs,[])
	addControlsToCompound compoundId ls newItems itemNrs [itemH:itemHs]
		# (found,nrSkip,ls,newItems,itemNrs,itemH)		= addControlsToCompound` compoundId ls newItems itemNrs itemH
		| found
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
		| otherwise
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
	where
		addControlsToCompound` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WElementHandle .ls .pst)
					   -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WElementHandle .ls .pst)
		addControlsToCompound` compoundId ls newItems itemNrs (WItemHandle itemH)
			# (found,nrSkip,ls,newItems,itemNrs,itemH) = addControlsToCompound`` compoundId ls newItems itemNrs itemH
			= (found,nrSkip,ls,newItems,itemNrs,WItemHandle itemH)
		where
			addControlsToCompound`` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WItemHandle .ls .pst)
							-> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WItemHandle .ls .pst)
			addControlsToCompound`` compoundId ls newItems itemNrs itemH=:{wItemKind,wItemId}
				| not (isRecursiveControl wItemKind)
					= (False,0,ls,newItems,itemNrs,itemH)
				| wItemKind==IsLayoutControl
					# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemH.wItems
					  itemH										= {itemH & wItems=itemHs}
					= (found,nrSkip,ls,newItems,itemNrs,itemH)
				| not (identifyMaybeId compoundId wItemId)
					# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemH.wItems
					| found
						= (found,nrSkip,ls,newItems,itemNrs,invalidateCompoundClipState {itemH & wItems=itemHs})
					// otherwise
						= (found,nrSkip,ls,newItems,itemNrs,{itemH & wItems=itemHs})
				| otherwise
					# (nrSkip,curItems)	= ulength itemH.wItems
					  (itemNrs,newItems)= genWElementItemNrs itemNrs newItems
					  newItems			= [WChangeLSHandle {wChangeLS=ls,wChangeItems=newItems}]
					  itemH				= {itemH & wItems=curItems++newItems}
					  itemH				= invalidateCompoundClipState itemH
					= (True,nrSkip,undef,[],itemNrs,itemH)
		
		addControlsToCompound` compoundId ls newItems itemNrs (WListLSHandle itemHs)
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WListLSHandle itemHs)
		
		addControlsToCompound` compoundId ls newItems itemNrs (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		addControlsToCompound` compoundId ls newItems itemNrs (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToCompound compoundId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WChangeLSHandle {wChH & wChangeItems=itemHs})
opencompoundcontrols _ _ _ _ _ _ _
	= windowcontrolsFatalError "opencompoundcontrols" "unexpected window placeholder argument"


/*	openrecursivecontrols adds the given controls to the (Compound/Layout)Control of the given window. 
	It is assumed that the new controls do not conflict with the current controls.
*/
openrecursivecontrols :: !OSDInfo !OSWindowMetrics !Id .ls ![WElementHandle .ls .pst] !(WindowStateHandle .pst) !*OSToolbox
																			 -> (!Bool,!WindowStateHandle .pst, !*OSToolbox)
openrecursivecontrols osdInfo wMetrics controlId ls newItems 
					  wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems,whAtts,whDefaultId,whCancelId,whSelect,whShow,whItemNrs,whKind,whSize,whWindowInfo}}} 
					  tb
	# (found,nrSkip,_,_,itemNrs,oldItemHs)
									= addControlsToRecursiveControl controlId ls newItems whItemNrs whItems
	| not found
		= (False,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=oldItemHs}}},tb)
	| otherwise
		# (curw,curh)				= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  curSize					= {w=curw,h=curh}
		  wFrame					= sizeToRect curSize
		  hMargins					= getWindowHMargins   whKind wMetrics whAtts
		  vMargins					= getWindowVMargins   whKind wMetrics whAtts
		  spaces					= getWindowItemSpaces whKind wMetrics whAtts
		  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (oldItemHs`,oldItemHs,tb)	= getWElementHandles` wPtr oldItemHs tb
		# (derSize,newItemHs,tb)	= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs tb
	//	# tb						= checkNewWindowSize curSize derSize wPtr osdInfo tb	// PA: curSize might be bigger than domain, then you shouldn't resize!
		# (newItemHs,tb)			= createRecursiveControls wMetrics controlId nrSkip whDefaultId whCancelId whSelect wPtr newItemHs tb
		  wH						= {wH & whItemNrs=itemNrs,whItems=newItemHs}
		# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,newItemHs,tb)		= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (wFrame,zero,zero,oldItemHs`) (wFrame,zero,zero,wH.whItems) tb
		# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wshIds {wH & whItems=newItemHs} tb
		= (True,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	wPtr							= wshIds.wPtr
	domain							= rectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
									= case whWindowInfo of
										WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
										other			-> (zero,             sizeToRect whSize,False,False)
	(visHScroll,visVScroll)			= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	
	addControlsToRecursiveControl :: !Id .ls` ![WElementHandle .ls` .pst] [Int] ![WElementHandle .ls .pst]
						  -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int],![WElementHandle .ls .pst])
	addControlsToRecursiveControl _ ls newItems itemNrs []
		= (False,0,ls,newItems,itemNrs,[])
	addControlsToRecursiveControl controlId ls newItems itemNrs [itemH:itemHs]
		# (found,nrSkip,ls,newItems,itemNrs,itemH)		= addControlsToRecursiveControl` controlId ls newItems itemNrs itemH
		| found
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
		| otherwise
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToRecursiveControl controlId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,[itemH:itemHs])
	where
		addControlsToRecursiveControl` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WElementHandle .ls .pst)
							   -> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WElementHandle .ls .pst)
		addControlsToRecursiveControl` controlId ls newItems itemNrs (WItemHandle itemH)
			# (found,nrSkip,ls,newItems,itemNrs,itemH) = addControlsToRecursiveControl`` controlId ls newItems itemNrs itemH
			= (found,nrSkip,ls,newItems,itemNrs,WItemHandle itemH)
		where
			addControlsToRecursiveControl`` :: !Id .ls` ![WElementHandle .ls` .pst] [Int] !(WItemHandle .ls .pst)
									-> (!Bool,!Int,.ls`,![WElementHandle .ls` .pst],[Int], !WItemHandle .ls .pst)
			addControlsToRecursiveControl`` controlId ls newItems itemNrs itemH=:{wItemKind,wItemId}
				| not (isRecursiveControl wItemKind)
					= (False,0,ls,newItems,itemNrs,itemH)
				| not (identifyMaybeId controlId wItemId)
					# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToRecursiveControl controlId ls newItems itemNrs itemH.wItems
					# itemH										= {itemH & wItems=itemHs}
					| found && wItemKind==IsCompoundControl
						= (found,nrSkip,ls,newItems,itemNrs,invalidateCompoundClipState itemH)
					| otherwise
						= (found,nrSkip,ls,newItems,itemNrs,itemH)
				| otherwise
					# (nrSkip, curItems)						= ulength itemH.wItems
					  (itemNrs,newItems)						= genWElementItemNrs itemNrs newItems
					  newItems									= [WChangeLSHandle {wChangeLS=ls,wChangeItems=newItems}]
					  itemH										= {itemH & wItems=curItems++newItems}
					| wItemKind==IsCompoundControl
						= (True,nrSkip,undef,[],itemNrs,invalidateCompoundClipState itemH)
					| otherwise
						= (True,nrSkip,undef,[],itemNrs,itemH)
		
		addControlsToRecursiveControl` controlId ls newItems itemNrs (WListLSHandle itemHs)
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToRecursiveControl controlId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WListLSHandle itemHs)
		
		addControlsToRecursiveControl` controlId ls newItems itemNrs (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToRecursiveControl controlId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		addControlsToRecursiveControl` controlId ls newItems itemNrs (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (found,nrSkip,ls,newItems,itemNrs,itemHs)	= addControlsToRecursiveControl controlId ls newItems itemNrs itemHs
			= (found,nrSkip,ls,newItems,itemNrs,WChangeLSHandle {wChH & wChangeItems=itemHs})
openrecursivecontrols _ _ _ _ _ _ _
	= windowcontrolsFatalError "openrecursivecontrols" "unexpected window placeholder argument"


/*	closecontrols closes the indicated controls and returns their R(2)Ids (first result [Id]) and
	Ids (second result [Id]) if appropriate.
	When closecontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
*/
closecontrols :: !OSWindowMetrics ![Id] !Bool !(WindowStateHandle .pst) !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst,!*OSToolbox)
closecontrols wMetrics closeIds relayout 
			  wsH=:{wshIds=wshIds=:{wPtr},wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems,whItemNrs,whAtts,whKind,whSize,whSelect,whShow,whDefaultId,whWindowInfo}}} 
			  tb
	# (freeRIds,freeIds,disposeFun,_,itemNrs,oldItemHs,tb)
									= closeWElementHandles wPtr zero closeIds whItemNrs curItems tb
	| not relayout
		# wH						= {wH & whItemNrs=itemNrs,whItems=oldItemHs}
		  wH						= invalidateWindowClipState wH
		= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
	| otherwise
		# (curw,curh)				= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  wFrame					= sizeToRect {w=curw,h=curh}
		  hMargins					= getWindowHMargins   whKind wMetrics whAtts
		  vMargins					= getWindowVMargins   whKind wMetrics whAtts
		  spaces					= getWindowItemSpaces whKind wMetrics whAtts
		  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (oldItemHs`,oldItemHs,tb)	= getWElementHandles` wPtr oldItemHs tb
		# (_,newItemHs,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] oldItemHs tb
		  wH						= {wH & whItemNrs=itemNrs, whItems=newItemHs}
		# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
		# (updRgn,newItemHs,tb)		= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (wFrame,zero,zero,oldItemHs`) (wFrame,zero,zero,wH.whItems) tb
		# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wshIds {wH & whItems=newItemHs} tb
		= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	domain							= rectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
									= case whWindowInfo of
										WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
										other			-> (zero,             sizeToRect whSize,False,False)
	(visHScroll,visVScroll)			= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)
	
	closeWElementHandles :: !OSWindowPtr !Point2 ![Id]  [Int] ![WElementHandle .ls .pst] !*OSToolbox
	           -> (![Id],![Id],!IdFun *OSToolbox,![Id],![Int],![WElementHandle .ls .pst],!*OSToolbox)
	closeWElementHandles _ _ ids itemNrs [] tb
		= ([],[],id,ids,itemNrs,[],tb)
	closeWElementHandles parentPtr parentPos ids itemNrs [itemH:itemHs] tb
		| isEmpty ids
			= ([],[],id,ids,itemNrs,[itemH:itemHs],tb)
		# (close,freeRIds1,freeIds1,f1,ids,itemNrs,itemH, tb)	= closeWElementHandle  parentPtr parentPos ids itemNrs itemH  tb
		# (      freeRIds2,freeIds2,f2,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr parentPos ids itemNrs itemHs tb
		  freeRIds												= freeRIds1++freeRIds2
		  freeIds												= freeIds1 ++freeIds2
		  f														= f2 o f1
		| close
			= (freeRIds,freeIds,f,ids,itemNrs,       itemHs, tb)
		| otherwise
			= (freeRIds,freeIds,f,ids,itemNrs,[itemH:itemHs],tb)
	where
		closeWElementHandle :: !OSWindowPtr !Point2 ![Id]  [Int] !(WElementHandle .ls .pst) !*OSToolbox
		    -> (!Bool,![Id],![Id],!IdFun *OSToolbox,![Id],![Int], !WElementHandle .ls .pst, !*OSToolbox)
		closeWElementHandle parentPtr parentPos ids itemNrs (WItemHandle itemH) tb
			# (keep,freeRIds,freeIds,f,ids,itemNrs,itemH,tb)= closeWItemHandle parentPtr parentPos ids itemNrs itemH tb
			= (keep,freeRIds,freeIds,f,ids,itemNrs,WItemHandle itemH,tb)
		where
			closeWItemHandle :: !OSWindowPtr !Point2 ![Id]  [Int] !(WItemHandle .ls .pst) !*OSToolbox
			 -> (!Bool,![Id],![Id],!IdFun *OSToolbox,![Id],![Int], !WItemHandle .ls .pst, !*OSToolbox)
			
			closeWItemHandle parentPtr parentPos ids itemNrs itemH=:{wItemKind,wItemId,wItemPos,wItemSize,wItemNr,wItems} tb
				# (close,ids)										= case wItemId of
																		(Just id)	-> removeCheck id ids
																		_			-> (False,ids)
				| isRecursiveControl wItemKind
					# (freeRIds,freeIds,f1,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr absolutePos ids itemNrs wItems tb
					  itemH											= {itemH & wItems=itemHs}
					| not close
						| wItemKind==IsCompoundControl
							= (close,freeRIds,freeIds,f1,ids,itemNrs,invalidateCompoundClipState itemH,tb)
						// otherwise
							= (close,freeRIds,freeIds,f1,ids,itemNrs,itemH,tb)
					// otherwise
						# (freeRIds1,freeIds1,f2,itemH,tb)			= disposeWItemHandle parentPtr parentPos itemH tb
						# tb										= osInvalidateWindowRect parentPtr itemRect tb
						= (close,freeRIds1++freeRIds,freeIds1++freeIds,f2 o f1,ids,itemNrs,itemH,tb)
				| not close
					= (close,[],[],id,ids,itemNrs,itemH,tb)
				| otherwise
					# (freeRIds,freeIds,f,itemH,tb)					= disposeWItemHandle parentPtr parentPos itemH tb
					# tb											= osInvalidateWindowRect parentPtr itemRect tb
					= (close,freeRIds,freeIds,f,ids,[wItemNr:itemNrs],itemH,tb)
			where
				absolutePos											= movePoint wItemPos parentPos
				itemRect											= posSizeToRect absolutePos wItemSize
		
		closeWElementHandle parentPtr parentPos ids itemNrs (WListLSHandle itemHs) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr parentPos ids itemNrs itemHs tb
			# (empty,itemHs)								= uisEmpty itemHs
			= (empty,freeRIds,freeIds,f,ids,itemNrs,WListLSHandle itemHs,tb)
		
		closeWElementHandle parentPtr parentPos ids itemNrs (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr parentPos ids itemNrs itemHs tb
			# (empty,itemHs)								= uisEmpty itemHs
			= (empty,freeRIds,freeIds,f,ids,itemNrs,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		closeWElementHandle parentPtr parentPos ids itemNrs (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (freeRIds,freeIds,f,ids,itemNrs,itemHs,tb)	= closeWElementHandles parentPtr parentPos ids itemNrs itemHs tb
			# (empty,itemHs)								= uisEmpty itemHs
			= (empty,freeRIds,freeIds,f,ids,itemNrs,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)
closecontrols _ _ _ _ _
	= windowcontrolsFatalError "closecontrols" "unexpected window placeholder argument"


/*	closeallcontrols closes all controls and returns their R(2)Ids (first result [Id]) and Ids (second result [Id]).
	When closeallcontrols returns, the indicated controls will have been hidden. To actually dispose of them,
	the return (IdFun *OSToolbox) function should be applied.
*/
closeallcontrols :: !(WindowStateHandle .pst) !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,!WindowStateHandle .pst,!*OSToolbox)
closeallcontrols wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=curItems,whItemNrs}}} tb
	# (freeRIds,freeIds,disposeFun,itemNrs,tb)	= closeWElementHandles wPtr zero curItems whItemNrs tb
	  wH										= {wH & whItemNrs=itemNrs,whItems=[]}
	  wH										= invalidateWindowClipState wH
	= (freeRIds,freeIds,disposeFun,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
where
	closeWElementHandles :: !OSWindowPtr !Point2 ![WElementHandle .ls .pst] ![Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
	closeWElementHandles _ _ [] itemNrs tb
		= ([],[],id,itemNrs,tb)
	closeWElementHandles parentPtr parentPos [itemH:itemHs] itemNrs tb
		# (freeRIds1,freeIds1,f1,itemNrs,tb)= closeWElementHandle  parentPtr parentPos itemH  itemNrs tb
		# (freeRIds2,freeIds2,f2,itemNrs,tb)= closeWElementHandles parentPtr parentPos itemHs itemNrs tb
		= (freeRIds1++freeRIds2,freeIds1++freeIds2,f2 o f1,itemNrs,tb)
	where
		closeWElementHandle :: !OSWindowPtr !Point2 !(WElementHandle .ls .pst) ![Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
		closeWElementHandle parentPtr parentPos (WItemHandle itemH) itemNrs tb
			= closeWItemHandle parentPtr parentPos itemH itemNrs tb
		where
			closeWItemHandle :: !OSWindowPtr !Point2 !(WItemHandle .ls .pst) ![Int] !*OSToolbox -> (![Id],![Id],!IdFun *OSToolbox,![Int],!*OSToolbox)
			closeWItemHandle parentPtr parentPos itemH=:{wItemKind,wItemNr,wItems,wItemPos,wItemSize} itemNrs tb
				| isRecursiveControl wItemKind
					# (freeRIds1,freeIds1,f1,itemNrs,tb)= closeWElementHandles parentPtr absolutePos wItems itemNrs tb
					# (freeRIds2,freeIds2,f2,_,tb)		= disposeWItemHandle parentPtr parentPos {itemH & wItems=[]} tb		// PA: itemH --> {itemH & wItems=[]}
					# tb								= osInvalidateWindowRect parentPtr itemRect tb
					= (freeRIds2++freeRIds1,freeIds2++freeIds1,f2 o f1,itemNrs,tb)
				| otherwise
					# (freeRIds,freeIds,f,_,tb)			= disposeWItemHandle parentPtr parentPos itemH tb
					# tb								= osInvalidateWindowRect parentPtr itemRect tb
					= (freeRIds,freeIds,f,[wItemNr:itemNrs],tb)
			where
				absolutePos								= movePoint wItemPos parentPos
				itemRect								= posSizeToRect absolutePos wItemSize
		
		closeWElementHandle parentPtr parentPos (WListLSHandle itemHs) itemNrs tb
			= closeWElementHandles parentPtr parentPos itemHs itemNrs tb
		
		closeWElementHandle parentPtr parentPos (WExtendLSHandle {wExtendItems=itemHs}) itemNrs tb
			= closeWElementHandles parentPtr parentPos itemHs itemNrs tb
		
		closeWElementHandle parentPtr parentPos (WChangeLSHandle {wChangeItems=itemHs}) itemNrs tb
			= closeWElementHandles parentPtr parentPos itemHs itemNrs tb
closeallcontrols _ _
	= windowcontrolsFatalError "closeallcontrols" "unexpected window placeholder argument"


/*	setcontrolpositions changes the position of the indicated controls.
	It is assumed that the argument WindowStateHandle is either a Window or a Dialog. 
*/
setcontrolpositions :: !OSWindowMetrics ![(Id,ItemPos)] !(WindowStateHandle .pst) !*OSToolbox -> (!Bool,!WindowStateHandle .pst,!*OSToolbox)
setcontrolpositions wMetrics newPoss 
					wsH=:{wshIds,wshHandle=Just wlsH=:{wlsHandle=wH=:{whItems=oldItems,whAtts,whKind,whSize,whSelect,whShow,whDefaultId,whWindowInfo}}} 
					tb
	# (valid,oldItems)				= validateNewItemPoss newPoss oldItems
	| not valid
		= (False,{wsH & wshHandle=Just {wlsH & wlsHandle={wH & whItems=oldItems}}},tb)
	| otherwise
		# (curw,curh)				= (whSize.w-(if visVScroll wMetrics.osmVSliderWidth 0),whSize.h-(if visHScroll wMetrics.osmHSliderHeight 0))
		  wFrame					= sizeToRect {w=curw,h=curh}
		  hMargins					= getWindowHMargins   whKind wMetrics whAtts
		  vMargins					= getWindowVMargins   whKind wMetrics whAtts
		  spaces					= getWindowItemSpaces whKind wMetrics whAtts
		  reqSize					= {w=curw-fst hMargins-snd hMargins,h=curh-fst vMargins-snd vMargins}
		# (oldItems`,oldItems,tb)	= getWElementHandles` wPtr oldItems tb
		  (_,newItems)				= setNewItemPoss newPoss oldItems
		# (_,newItems,tb)			= layoutControls wMetrics hMargins vMargins spaces reqSize zero [(domain,origin)] newItems tb
		  wH						= {wH & whItems=newItems}
		# (wH,tb)					= forceValidWindowClipState wMetrics True wPtr wH tb
		  viewFrame					= posSizeToRectangle origin {w=curw,h=curh}
		  updState					= rectangleToUpdateState viewFrame
		  drawbackground			= if (whKind==IsDialog) (\x y->(x,y)) (drawwindowlook wMetrics wPtr id updState)
		# (wH,tb)					= drawbackground wH tb	// DvA: was switched off because of clipping concerns
		# (updRgn,newItems,tb)		= relayoutControls wMetrics wPtr whDefaultId False whSelect whShow (wFrame,zero,zero,oldItems`) (wFrame,zero,zero,wH.whItems) tb
		# (wH,tb)					= updatewindowbackgrounds wMetrics updRgn wshIds {wH & whItems=newItems} tb
		# tb						= osValidateWindowRect wPtr (sizeToRect whSize) tb
		  wsH						= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
		= (True,wsH,tb)
where
	wPtr							= wshIds.wPtr
	domain							= rectToRectangle domainRect
	(origin,domainRect,hasHScroll,hasVScroll)
									= case whWindowInfo of
										WindowInfo info	-> (info.windowOrigin,info.windowDomain,isJust info.windowHScroll,isJust info.windowVScroll)
										other			-> (zero,             sizeToRect whSize,False,False)
	(visHScroll,visVScroll)			= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) (hasHScroll,hasVScroll)

	validateNewItemPoss :: ![(Id,ItemPos)] ![WElementHandle .ls .pst] -> (!Bool,![WElementHandle .ls .pst])
	validateNewItemPoss idPoss itemHs
		# (ids,itemHs)	= controlsExist (getids idPoss) itemHs
		= (isEmpty ids,itemHs)
	where
		getids :: ![(Id,ItemPos)] -> [Id]
		getids [(controlId,(itemLoc,_)):idPoss]
			= case itemLoc of
				LeftOf	id	-> [id:ids]
				RightTo	id	-> [id:ids]
				Above	id	-> [id:ids]
				Below	id	-> [id:ids]
				_			-> ids
		where
			ids	= [controlId:getids idPoss]
		getids _
			= []
		
		controlsExist :: ![Id] ![WElementHandle .ls .pst] -> (![Id],![WElementHandle .ls .pst])
		controlsExist ids []
			= (ids,[])
		controlsExist ids [itemH:itemHs]
			| isEmpty ids
				= (ids,[itemH:itemHs])
			| otherwise
				# (ids,itemH)	= controlsExist` ids itemH
				# (ids,itemHs)	= controlsExist  ids itemHs
				= (ids,[itemH:itemHs])
		where
			controlsExist` :: ![Id] !(WElementHandle .ls .pst) -> (![Id],!WElementHandle .ls .pst)
			controlsExist` ids (WItemHandle itemH=:{wItemId,wItems})
				# ids			= if (isJust wItemId) (removeMember (fromJust wItemId) ids) ids
				# (ids,itemHs)	= controlsExist ids wItems
				= (ids,WItemHandle {itemH & wItems=itemHs})
			controlsExist` ids (WListLSHandle itemHs)
				# (ids,itemHs)	= controlsExist ids itemHs
				= (ids,WListLSHandle itemHs)
			controlsExist` ids (WExtendLSHandle wExH=:{wExtendItems=itemHs})
				# (ids,itemHs)	= controlsExist ids itemHs
				= (ids,WExtendLSHandle {wExH & wExtendItems=itemHs})
			controlsExist` ids (WChangeLSHandle wChH=:{wChangeItems=itemHs})
				# (ids,itemHs)	= controlsExist ids itemHs
				= (ids,WChangeLSHandle {wChH & wChangeItems=itemHs})
	
	setNewItemPoss :: ![(Id,ItemPos)] ![WElementHandle .ls .pst] -> (![(Id,ItemPos)],![WElementHandle .ls .pst])
	setNewItemPoss idPoss []
		= (idPoss,[])
	setNewItemPoss idPoss [itemH:itemHs]
		| isEmpty idPoss
			= (idPoss,[itemH:itemHs])
		| otherwise
			# (idPoss,itemH)	= setNewItemPos` idPoss itemH
			  (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,[itemH:itemHs])
	where
		setNewItemPos` :: ![(Id,ItemPos)] !(WElementHandle .ls .pst) -> (![(Id,ItemPos)],!WElementHandle .ls .pst)
		setNewItemPos` idPoss (WItemHandle itemH)
			# (idPoss,itemH)	= setNewItemPos`` idPoss itemH
			= (idPoss,WItemHandle itemH)
		where
			setNewItemPos`` :: ![(Id,ItemPos)] !(WItemHandle .ls .pst) -> (![(Id,ItemPos)],!WItemHandle .ls .pst)
			setNewItemPos`` idPoss itemH=:{wItemId,wItemKind,wItemAtts,wItems}
				| isNothing wItemId
					| isRecursiveControl wItemKind
						# (idPoss,itemHs)	= setNewItemPoss idPoss wItems
						= (idPoss,{itemH & wItems=itemHs})
					// otherwise
						= (idPoss,itemH)
				# (found,idPos,idPoss)	= remove ((==) itemId o fst) undef idPoss
				  atts					= if found (snd (creplace isControlPos (ControlPos (snd idPos)) wItemAtts)) wItemAtts
				| isRecursiveControl wItemKind
					# (idPoss,itemHs)	= setNewItemPoss idPoss wItems
					| found && wItemKind==IsCompoundControl
						= (idPoss,invalidateCompoundClipState {itemH & wItems=itemHs,wItemAtts=atts})
					// otherwise
						= (idPoss,{itemH & wItems=itemHs,wItemAtts=atts})
				| otherwise
					= (idPoss,{itemH & wItemAtts=atts})
			where
				itemId					= fromJust wItemId
		
		setNewItemPos` idPoss (WListLSHandle itemHs)
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WListLSHandle itemHs)
		
		setNewItemPos` idPoss (WExtendLSHandle wExH=:{wExtendItems=itemHs})
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WExtendLSHandle {wExH & wExtendItems=itemHs})
		
		setNewItemPos` idPoss (WChangeLSHandle wChH=:{wChangeItems=itemHs})
			# (idPoss,itemHs)	= setNewItemPoss idPoss itemHs
			= (idPoss,WChangeLSHandle {wChH & wChangeItems=itemHs})

setcontrolpositions _ _ _ _
	= windowcontrolsFatalError "setcontrolpositions" "unexpected window placeholder argument"
