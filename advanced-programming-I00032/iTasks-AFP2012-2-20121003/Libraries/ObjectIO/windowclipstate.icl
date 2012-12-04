implementation module windowclipstate


import	StdBool, StdList, StdMisc
import	osrgn, oswindow
import	commondef, wstate
from windowaccess	import getWItemRadioInfo,  getWItemCheckInfo,  getWItemCompoundInfo, getWindowInfoWindowData
from	wstateaccess	import getWItemRadioInfo`, getWItemCheckInfo`, getWItemCompoundInfo`

/*	createClipState wMetrics allClipStates validate wPtr clipRect defId isVisible items
		calculates the ClipState that corresponds with items.
		If the Boolean argument is True, also the invalid ClipStates are recalculated of CompoundControls
			that are inside the window frame. 
*/
createClipState :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool ![WElementHandle .ls .pst] !*OSToolbox
                                                                                -> (!ClipState,![WElementHandle .ls .pst],!*OSToolbox)
createClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs tb
	# (clipRgn,tb)			= osnewrectrgn clipRect tb
	# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
	= ({clipRgn=clipRgn,clipOk=True},itemHs,tb)
where
	createWElementsClipState :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool ![WElementHandle .ls .pst] !OSRgnHandle !*OSToolbox
	                                                                                                    -> (![WElementHandle .ls .pst],!OSRgnHandle,!*OSToolbox)
	createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible [itemH:itemHs] clipRgn tb
		# (itemH, clipRgn,tb)	= createWElementClipState  wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemH  clipRgn tb
		# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
		= ([itemH:itemHs],clipRgn,tb)
	where
		createWElementClipState :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool !(WElementHandle .ls .pst) !OSRgnHandle !*OSToolbox
		                                                                                                    -> (!WElementHandle .ls .pst, !OSRgnHandle,!*OSToolbox)
		createWElementClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WItemHandle itemH=:{wItemShow,wItemKind,wItemPos,wItemSize}) clipRgn tb
			| not itemVisible || disjointRects clipRect (posSizeToRect absolutePos wItemSize)
				| not allClipStates || not (isRecursiveControl wItemKind)	// PA:<>IsCompoundControl
					= (WItemHandle itemH,clipRgn,tb)
				| wItemKind==IsLayoutControl								// PA: this alternative added
					# clipRect				= posSizeToRect absolutePos wItemSize
					# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr absolutePos clipRect defId False itemH.wItems clipRgn tb
					# itemH					= {itemH & wItems=itemHs}
					= (WItemHandle itemH,clipRgn,tb)
				| validate
					# (itemH,tb)			= validateCompoundClipState wMetrics allClipStates wPtr parentPos defId itemVisible itemH tb
					= (WItemHandle itemH,clipRgn,tb)
				// otherwise
					# (itemH,tb)			= forceValidCompoundClipState wMetrics allClipStates wPtr parentPos defId itemVisible itemH tb
					= (WItemHandle itemH,clipRgn,tb)
			| otherwise
				# (itemH,clipRgn,tb)		= createWItemClipState wMetrics allClipStates validate wPtr parentPos clipRect defId itemH clipRgn tb
				= (WItemHandle itemH,clipRgn,tb)
		where
			absolutePos						= movePoint wItemPos parentPos
			itemVisible						= isVisible && wItemShow
			
			createWItemClipState :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !(WItemHandle .ls .pst) !OSRgnHandle !*OSToolbox
			                                                                                           -> (!WItemHandle .ls .pst, !OSRgnHandle,!*OSToolbox)
			createWItemClipState _ _ _ wPtr parentPos clipRect _ itemH=:{wItemKind=IsRadioControl,wItemInfo,wItemPos} clipRgn tb
				# (clipRgn,tb)		= stateMap2 (createRadioClipState wPtr absolutePos clipRect) (getWItemRadioInfo wItemInfo).radioItems (clipRgn,tb)
				= (itemH,clipRgn,tb)
			where
				absolutePos			= movePoint wItemPos parentPos
				
				createRadioClipState :: !OSWindowPtr !Point2 !OSRect !(RadioItemInfo .pst) !(!OSRgnHandle,!*OSToolbox) -> (!OSRgnHandle,!*OSToolbox)
				createRadioClipState wPtr parentPos clipRect {radioItemPos,radioItemSize} (clipRgn,tb)
					# (radioRgn,tb)	= osClipRadioControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple radioItemSize) tb
					# (diffRgn, tb)	= osdiffrgn clipRgn radioRgn tb
					# tb			= osdisposergn clipRgn tb
					# tb			= osdisposergn radioRgn tb
					= (diffRgn,tb)
				where
					absolutePos		= movePoint radioItemPos parentPos
			
			createWItemClipState _ _ _ wPtr parentPos clipRect defId itemH=:{wItemKind=IsCheckControl,wItemInfo,wItemPos} clipRgn tb
				# (clipRgn,tb)	= stateMap2 (createCheckClipState wPtr absolutePos clipRect) (getWItemCheckInfo wItemInfo).checkItems (clipRgn,tb)
				= (itemH,clipRgn,tb)
			where
				absolutePos			= movePoint wItemPos parentPos
				
				createCheckClipState :: !OSWindowPtr !Point2 !OSRect !(CheckItemInfo .pst) !(!OSRgnHandle,!*OSToolbox) -> (!OSRgnHandle,!*OSToolbox)
				createCheckClipState wPtr parentPos clipRect {checkItemPos,checkItemSize} (clipRgn,tb)
					# (checkRgn,tb)	= osClipCheckControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple checkItemSize) tb
					# (diffRgn, tb)	= osdiffrgn clipRgn checkRgn tb
					# tb			= osdisposergn clipRgn tb
					# tb			= osdisposergn checkRgn tb
					= (diffRgn,tb)
				where
					absolutePos		= movePoint checkItemPos parentPos
			
			createWItemClipState wMetrics allClipStates validate wPtr parentPos clipRect defId itemH=:{wItemKind=IsCompoundControl,wItems,wItemPos,wItemSize} clipRgn tb
				# (rectRgn,tb)			= osClipCompoundControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple wItemSize) tb
				# (diffRgn,tb)			= osdiffrgn clipRgn rectRgn tb
				# tb					= osdisposergn clipRgn tb
				# tb					= osdisposergn rectRgn tb
				| allClipStates
					| validate
						# (itemH,tb)	= validateCompoundClipState wMetrics allClipStates wPtr parentPos defId True itemH tb
						= (itemH,diffRgn,tb)
					// otherwise
						# (itemH,tb)	= forceValidCompoundClipState wMetrics allClipStates wPtr parentPos defId True itemH tb
						= (itemH,diffRgn,tb)
				| otherwise
					= (itemH,diffRgn,tb)
			where
				absolutePos				= movePoint wItemPos parentPos
			
			createWItemClipState wMetrics allClipStates validate wPtr parentPos clipRect defId itemH=:{wItemKind=IsLayoutControl,wItems,wItemPos,wItemSize} clipRgn tb
				# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr absolutePos clipRect1 defId True wItems clipRgn tb
				= ({itemH & wItems=itemHs},clipRgn,tb)
			where
				absolutePos				= movePoint wItemPos parentPos
				clipRect1				= intersectRects (posSizeToRect absolutePos wItemSize) clipRect
			
			createWItemClipState _ _ _ wPtr parentPos clipRect defId itemH=:{wItemKind,wItemPos,wItemSize} clipRgn tb
				| okItem
					# (itemRgn,tb)		= clipItem wPtr (0,0) clipRect (toTuple absolutePos) (toTuple wItemSize) tb
					# (diffRgn,tb)		= osdiffrgn clipRgn itemRgn tb
					# tb				= osdisposergn clipRgn tb
					# tb				= osdisposergn itemRgn tb
					= (itemH,diffRgn,tb)
			where
				absolutePos				= movePoint wItemPos parentPos
				(okItem,clipItem)		= case wItemKind of
											IsPopUpControl			-> (True,osClipPopUpControl)
											IsSliderControl			-> (True,osClipSliderControl)
											IsTextControl			-> (True,osClipTextControl)
											IsEditControl			-> (True,osClipEditControl)
											IsButtonControl			-> (True,osClipButtonControl)
											IsCustomButtonControl	-> (True,osClipCustomButtonControl)
											IsCustomControl			-> (True,osClipCustomControl)
											_						-> (False,undef)
				
			createWItemClipState _ _ _ _ _ _ _ itemH clipRgn tb
				= (itemH,clipRgn,tb)
		
		createWElementClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WListLSHandle itemHs) clipRgn tb
			# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
			= (WListLSHandle itemHs,clipRgn,tb)
		
		createWElementClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WExtendLSHandle wExH=:{wExtendItems=itemHs}) clipRgn tb
			# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
			= (WExtendLSHandle {wExH & wExtendItems=itemHs},clipRgn,tb)
		
		createWElementClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WChangeLSHandle wChH=:{wChangeItems=itemHs}) clipRgn tb
			# (itemHs,clipRgn,tb)	= createWElementsClipState wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
			= (WChangeLSHandle {wChH & wChangeItems=itemHs},clipRgn,tb)
	
	createWElementsClipState _ _ _ _ _ _ _ _ [] clipRgn tb
		= ([],clipRgn,tb)


createClipState` :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool ![WElementHandle`] !*OSToolbox
                                                                                 -> (!ClipState,![WElementHandle`],!*OSToolbox)
createClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs tb
	# (clipRgn,tb)			= osnewrectrgn clipRect tb
	# (itemHs,clipRgn,tb)	= createWElementsClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
	= ({clipRgn=clipRgn,clipOk=True},itemHs,tb)
where
	createWElementsClipState` :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool ![WElementHandle`] !OSRgnHandle !*OSToolbox
	                                                                                                     -> (![WElementHandle`],!OSRgnHandle,!*OSToolbox)
	createWElementsClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible [itemH:itemHs] clipRgn tb
		# (itemH, clipRgn,tb)	= createWElementClipState`  wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemH  clipRgn tb
		# (itemHs,clipRgn,tb)	= createWElementsClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
		= ([itemH:itemHs],clipRgn,tb)
	where
		createWElementClipState` :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !Bool !WElementHandle` !OSRgnHandle !*OSToolbox
		                                                                                                    -> (!WElementHandle`,!OSRgnHandle,!*OSToolbox)
		createWElementClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WItemHandle` itemH=:{wItemShow`,wItemKind`,wItemPos`,wItemSize`}) clipRgn tb
			| not itemVisible || disjointRects clipRect (posSizeToRect absolutePos wItemSize`)
				| not allClipStates || not (isRecursiveControl wItemKind`)	// PA:<>IsCompoundControl
					= (WItemHandle` itemH,clipRgn,tb)
				| wItemKind`==IsLayoutControl								// PA: this alternative added
					# clipRect				= posSizeToRect absolutePos wItemSize`
					# (itemHs,clipRgn,tb)	= createWElementsClipState` wMetrics allClipStates validate wPtr absolutePos clipRect defId False itemH.wItems` clipRgn tb
					# itemH					= {itemH & wItems`=itemHs}
					= (WItemHandle` itemH,clipRgn,tb)
				| validate
					# (itemH,tb)			= validateCompoundClipState` wMetrics allClipStates wPtr parentPos defId itemVisible itemH tb
					= (WItemHandle` itemH,clipRgn,tb)
				// otherwise
					# (itemH,tb)			= forceValidCompoundClipState` wMetrics allClipStates wPtr parentPos defId itemVisible itemH tb
					= (WItemHandle` itemH,clipRgn,tb)
			| otherwise
				# (itemH,clipRgn,tb)		= createWItemClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId itemH clipRgn tb
				= (WItemHandle` itemH,clipRgn,tb)
		where
			absolutePos						= movePoint wItemPos` parentPos
			itemVisible						= isVisible && wItemShow`
			
			createWItemClipState` :: !OSWindowMetrics !Bool !Bool !OSWindowPtr !Point2 !OSRect !(Maybe Id) !WItemHandle` !OSRgnHandle !*OSToolbox
			                                                                                           -> (!WItemHandle`,!OSRgnHandle,!*OSToolbox)
			createWItemClipState` _ _ _ wPtr parentPos clipRect _ itemH=:{wItemKind`=IsRadioControl,wItemInfo`,wItemPos`} clipRgn tb
				# (clipRgn,tb)		= stateMap2 (createRadioClipState` wPtr absolutePos clipRect) (getWItemRadioInfo` wItemInfo`).radioItems` (clipRgn,tb)
				= (itemH,clipRgn,tb)
			where
				absolutePos			= movePoint wItemPos` parentPos
				
				createRadioClipState` :: !OSWindowPtr !Point2 !OSRect !RadioItemInfo` !(!OSRgnHandle,!*OSToolbox) -> (!OSRgnHandle,!*OSToolbox)
				createRadioClipState` wPtr parentPos clipRect {radioItemPos`,radioItemSize`} (clipRgn,tb)
					# (radioRgn,tb)	= osClipRadioControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple radioItemSize`) tb
					# (diffRgn, tb)	= osdiffrgn clipRgn radioRgn tb
					# tb			= osdisposergn clipRgn tb
					# tb			= osdisposergn radioRgn tb
					= (diffRgn,tb)
				where
					absolutePos		= movePoint radioItemPos` parentPos
			
			createWItemClipState` _ _ _ wPtr parentPos clipRect defId itemH=:{wItemKind`=IsCheckControl,wItemInfo`,wItemPos`} clipRgn tb
				# (clipRgn,tb)		= stateMap2 (createCheckClipState` wPtr absolutePos clipRect) (getWItemCheckInfo` wItemInfo`).checkItems` (clipRgn,tb)
				= (itemH,clipRgn,tb)
			where
				absolutePos			= movePoint wItemPos` parentPos
				
				createCheckClipState` :: !OSWindowPtr !Point2 !OSRect !CheckItemInfo` !(!OSRgnHandle,!*OSToolbox) -> (!OSRgnHandle,!*OSToolbox)
				createCheckClipState` wPtr parentPos clipRect {checkItemPos`,checkItemSize`} (clipRgn,tb)
					# (checkRgn,tb)	= osClipCheckControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple checkItemSize`) tb
					# (diffRgn, tb)	= osdiffrgn clipRgn checkRgn tb
					# tb			= osdisposergn clipRgn tb
					# tb			= osdisposergn checkRgn tb
					= (diffRgn,tb)
				where
					absolutePos		= movePoint checkItemPos` parentPos
			
			createWItemClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId itemH=:{wItemKind`=IsCompoundControl,wItems`,wItemPos`,wItemSize`} clipRgn tb
				# (rectRgn,tb)			= osClipCompoundControl wPtr (0,0) clipRect (toTuple absolutePos) (toTuple wItemSize`) tb
				# (diffRgn,tb)			= osdiffrgn clipRgn rectRgn tb
				# tb					= osdisposergn clipRgn tb
				# tb					= osdisposergn rectRgn tb
				| allClipStates
					| validate
						# (itemH,tb)	= validateCompoundClipState` wMetrics allClipStates wPtr parentPos defId True itemH tb
						= (itemH,diffRgn,tb)
					// otherwise
						# (itemH,tb)	= forceValidCompoundClipState` wMetrics allClipStates wPtr parentPos defId True itemH tb
						= (itemH,diffRgn,tb)
				| otherwise
					= (itemH,diffRgn,tb)
			where
				absolutePos				= movePoint wItemPos` parentPos
			
			createWItemClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId itemH=:{wItemKind`=IsLayoutControl,wItems`,wItemPos`,wItemSize`} clipRgn tb
				# (itemHs,clipRgn,tb)	= createWElementsClipState` wMetrics allClipStates validate wPtr absolutePos clipRect1 defId True wItems` clipRgn tb
				= ({itemH & wItems`=itemHs},clipRgn,tb)
			where
				absolutePos				= movePoint wItemPos` parentPos
				clipRect1				= intersectRects (posSizeToRect absolutePos wItemSize`) clipRect
			
			createWItemClipState` _ _ _ wPtr parentPos clipRect defId itemH=:{wItemKind`,wItemPos`,wItemSize`} clipRgn tb
				| okItem
					# (itemRgn,tb)		= clipItem wPtr (0,0) clipRect (toTuple absolutePos) (toTuple wItemSize`) tb
					# (diffRgn,tb)		= osdiffrgn clipRgn itemRgn tb
					# tb				= osdisposergn clipRgn tb
					# tb				= osdisposergn itemRgn tb
					= (itemH,diffRgn,tb)
			where
				absolutePos				= movePoint wItemPos` parentPos
				(okItem,clipItem)		= case wItemKind` of
											IsPopUpControl			-> (True,osClipPopUpControl)
											IsSliderControl			-> (True,osClipSliderControl)
											IsTextControl			-> (True,osClipTextControl)
											IsEditControl			-> (True,osClipEditControl)
											IsButtonControl			-> (True,osClipButtonControl)
											IsCustomButtonControl	-> (True,osClipCustomButtonControl)
											IsCustomControl			-> (True,osClipCustomControl)
											_						-> (False,undef)
				
			createWItemClipState` _ _ _ _ _ _ _ itemH clipRgn tb
				= (itemH,clipRgn,tb)
		
		createWElementClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible (WRecursiveHandle` itemHs wKind) clipRgn tb
			# (itemHs,clipRgn,tb)	= createWElementsClipState` wMetrics allClipStates validate wPtr parentPos clipRect defId isVisible itemHs clipRgn tb
			= (WRecursiveHandle` itemHs wKind,clipRgn,tb)
	
	createWElementsClipState` _ _ _ _ _ _ _ _ [] clipRgn tb
		= ([],clipRgn,tb)

disposeClipState:: !ClipState !*OSToolbox -> *OSToolbox
disposeClipState {clipRgn} tb
	| clipRgn==0	= tb
	| otherwise		= osdisposergn clipRgn tb


/*	validateAllClipStates(`) metrics validate wPtr parentPos defaultId isVisible items
		checks for all items if the ClipState of CompoundControls is valid.
		If validate holds, then the ClipState is checked, otherwise the ClipState is disposed and recalculated(!!).
		The parentPos argument must be the exact location of the parent GUI element of items.
*/
validateAllClipStates :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool ![WElementHandle .ls .pst] !*OSToolbox -> (![WElementHandle .ls .pst],!*OSToolbox)
validateAllClipStates wMetrics validate wPtr parentPos defaultId isVisible itemHs tb
	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
where
	validateclipstate :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !(WElementHandle .ls.pst) !*OSToolbox -> (!WElementHandle .ls .pst,!*OSToolbox)
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WItemHandle itemH=:{wItemKind,wItemShow,wItems,wItemPos}) tb
		| wItemKind<>IsCompoundControl
			| isRecursiveControl wItemKind	// PA: added for LayoutControls
				# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr absolutePos defaultId itemVisible) wItems tb
				= (WItemHandle {itemH & wItems=itemHs},tb)
			// otherwise
				= (WItemHandle itemH,tb)
		| validate
			# (itemH,tb)	= validateCompoundClipState wMetrics True wPtr parentPos defaultId itemVisible itemH tb
			= (WItemHandle itemH,tb)
		| otherwise
			# (itemH,tb)	= forceValidCompoundClipState wMetrics True wPtr parentPos defaultId itemVisible itemH tb
			= (WItemHandle itemH,tb)
	where
		absolutePos			= movePoint wItemPos parentPos
		itemVisible			= isVisible && wItemShow
	
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WListLSHandle itemHs) tb
		# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
		= (WListLSHandle itemHs,tb)
	
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
		# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
		= (WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
	
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
		# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
		= (WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


validateAllClipStates` :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool ![WElementHandle`] !*OSToolbox -> (![WElementHandle`],!*OSToolbox)
validateAllClipStates` wMetrics validate wPtr parentPos defaultId isVisible itemHs tb
	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
where
	validateclipstate :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !WElementHandle` !*OSToolbox -> (!WElementHandle`,!*OSToolbox)
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WItemHandle` itemH=:{wItemKind`,wItemShow`,wItems`,wItemPos`}) tb
		| wItemKind`<>IsCompoundControl
			| isRecursiveControl wItemKind`	// PA: added for LayoutControls
				# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr absolutePos defaultId itemVisible) wItems` tb
				= (WItemHandle` {itemH & wItems`=itemHs},tb)
			// otherwise
				= (WItemHandle` itemH,tb)
		| validate
			# (itemH,tb)	= validateCompoundClipState` wMetrics True wPtr parentPos defaultId itemVisible itemH tb
			= (WItemHandle` itemH,tb)
		| otherwise
			# (itemH,tb)	= forceValidCompoundClipState` wMetrics True wPtr parentPos defaultId itemVisible itemH tb
			= (WItemHandle` itemH,tb)
	where
		absolutePos			= movePoint wItemPos` parentPos
		itemVisible			= isVisible && wItemShow`
	
	validateclipstate wMetrics validate wPtr parentPos defaultId isVisible (WRecursiveHandle` itemHs wKind) tb
		# (itemHs,tb)	= stateMap (validateclipstate wMetrics validate wPtr parentPos defaultId isVisible) itemHs tb
		= (WRecursiveHandle` itemHs wKind,tb)


validateWindowClipState :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
validateWindowClipState wMetrics allClipStates wPtr wH=:{whKind,whWindowInfo,whItems,whSize,whDefaultId,whShow} tb
	| whKind==IsGameWindow
		= (wH,tb)
	| whKind==IsDialog
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates wMetrics True wPtr zero whDefaultId whShow whItems tb
			= ({wH & whItems=itemHs},tb)
	| clipState.clipOk
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates wMetrics True wPtr zero whDefaultId whShow whItems tb
			= ({wH & whItems=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState wMetrics allClipStates True wPtr zero contentRect whDefaultId whShow whItems tb
		  windowInfo			= {windowInfo & windowClip=clipState}
		= ({wH & whItems=itemHs,whWindowInfo=WindowInfo windowInfo},tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo
	clipState					= windowInfo.windowClip
	domainRect					= windowInfo.windowDomain
	hasScrolls					= (isJust windowInfo.windowHScroll, isJust windowInfo.windowVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	contentRect					= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)

validateWindowClipState` :: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
validateWindowClipState` wMetrics allClipStates wPtr wH=:{whKind`,whWindowInfo`,whItems`,whSize`,whDefaultId`,whShow`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	| whKind`==IsDialog
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates` wMetrics True wPtr zero whDefaultId` whShow` whItems` tb
			= ({wH & whItems`=itemHs},tb)
	| clipState.clipOk
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates` wMetrics True wPtr zero whDefaultId` whShow` whItems` tb
			= ({wH & whItems`=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState` wMetrics allClipStates True wPtr zero contentRect whDefaultId` whShow` whItems` tb
		  windowInfo			= {windowInfo & windowClip=clipState}
		= ({wH & whItems`=itemHs,whWindowInfo`=WindowInfo windowInfo},tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo`
	clipState					= windowInfo.windowClip
	domainRect					= windowInfo.windowDomain
	hasScrolls					= (isJust windowInfo.windowHScroll, isJust windowInfo.windowVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize`) hasScrolls
	contentRect					= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize`)

forceValidWindowClipState :: !OSWindowMetrics !Bool !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
forceValidWindowClipState wMetrics allClipStates wPtr wH=:{whKind,whWindowInfo,whItems,whSize,whDefaultId,whShow} tb
	| whKind==IsGameWindow
		= (wH,tb)
	| whKind==IsDialog
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates wMetrics False wPtr zero whDefaultId whShow whItems tb
			= ({wH & whItems=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState wMetrics allClipStates False wPtr zero contentRect whDefaultId whShow whItems tb
		  windowInfo			= {windowInfo & windowClip=clipState}
		= ({wH & whItems=itemHs,whWindowInfo=WindowInfo windowInfo},tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo
	clipState					= windowInfo.windowClip
	domainRect					= windowInfo.windowDomain
	hasScrolls					= (isJust windowInfo.windowHScroll, isJust windowInfo.windowVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize) hasScrolls
	contentRect					= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize)

forceValidWindowClipState` :: !OSWindowMetrics !Bool !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
forceValidWindowClipState` wMetrics allClipStates wPtr wH=:{whKind`,whWindowInfo`,whItems`,whSize`,whDefaultId`,whShow`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	| whKind`==IsDialog
		| not allClipStates
			= (wH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates` wMetrics False wPtr zero whDefaultId` whShow` whItems` tb
			= ({wH & whItems`=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState` wMetrics allClipStates False wPtr zero contentRect whDefaultId` whShow` whItems` tb
		  windowInfo			= {windowInfo & windowClip=clipState}
		= ({wH & whItems`=itemHs,whWindowInfo`=WindowInfo windowInfo},tb)
where
	windowInfo					= getWindowInfoWindowData whWindowInfo`
	clipState					= windowInfo.windowClip
	domainRect					= windowInfo.windowDomain
	hasScrolls					= (isJust windowInfo.windowHScroll, isJust windowInfo.windowVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple whSize`) hasScrolls
	contentRect					= osGetWindowContentRect wMetrics visScrolls (sizeToRect whSize`)


invalidateWindowClipState :: !(WindowHandle .ls .pst) -> WindowHandle .ls .pst
invalidateWindowClipState wH=:{whKind,whWindowInfo}
	| whKind==IsWindow
		# windowInfo	= getWindowInfoWindowData whWindowInfo
		  clipState		= windowInfo.windowClip
		= {wH & whWindowInfo=WindowInfo {windowInfo & windowClip={clipState & clipOk=False}}}
	| otherwise
		= wH

invalidateWindowClipState` :: !WindowHandle` -> WindowHandle`
invalidateWindowClipState` wH=:{whKind`,whWindowInfo`}
	| whKind`==IsWindow
		# windowInfo	= getWindowInfoWindowData whWindowInfo`
		  clipState		= windowInfo.windowClip
		= {wH & whWindowInfo`=WindowInfo {windowInfo & windowClip={clipState & clipOk=False}}}
	| otherwise
		= wH


validateCompoundClipState :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
validateCompoundClipState wMetrics allClipStates wPtr parentPos defId isVisible itemH=:{wItemShow,wItemPos,wItemSize,wItemInfo,wItems} tb
	| clipState.clipOk
		| not allClipStates
			= (itemH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates wMetrics True wPtr absolutePos defId itemVisible wItems tb
			= ({itemH & wItems=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState wMetrics allClipStates True wPtr absolutePos contentRect defId itemVisible wItems tb
		  compoundInfo			= {compoundInfo & compoundLookInfo={compoundLook & compoundClip=clipState}}
		= ({itemH & wItemInfo=CompoundInfo compoundInfo,wItems=itemHs},tb)
where
	absolutePos					= movePoint wItemPos parentPos
	itemVisible					= isVisible && wItemShow
	compoundInfo				= getWItemCompoundInfo wItemInfo
	compoundLook				= compoundInfo.compoundLookInfo
	clipState					= compoundLook.compoundClip
	domainRect					= compoundInfo.compoundDomain
	hasScrolls					= (isJust compoundInfo.compoundHScroll, isJust compoundInfo.compoundVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
	contentRect					= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect absolutePos wItemSize)

validateCompoundClipState` :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
validateCompoundClipState` wMetrics allClipStates wPtr parentPos defId isVisible itemH=:{wItemShow`, wItemPos`,wItemSize`,wItemInfo`,wItems`} tb
	| clipState.clipOk
		| not allClipStates
			= (itemH,tb)
		// otherwise
			# (itemHs,tb)		= validateAllClipStates` wMetrics True wPtr absolutePos defId itemVisible wItems` tb
			= ({itemH & wItems`=itemHs},tb)
	| otherwise
		# tb					= disposeClipState clipState tb
		# (clipState,itemHs,tb)	= createClipState` wMetrics allClipStates True wPtr absolutePos contentRect defId itemVisible wItems` tb
		  compoundInfo			= {compoundInfo & compoundLookInfo={compoundLook & compoundClip=clipState}}
		= ({itemH & wItems`=itemHs,wItemInfo`=CompoundInfo` compoundInfo},tb)
where
	absolutePos					= movePoint wItemPos` parentPos
	itemVisible					= isVisible && wItemShow`
	compoundInfo				= getWItemCompoundInfo` wItemInfo`
	compoundLook				= compoundInfo.compoundLookInfo
	clipState					= compoundLook.compoundClip
	domainRect					= compoundInfo.compoundDomain
	hasScrolls					= (isJust compoundInfo.compoundHScroll, isJust compoundInfo.compoundVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize`) hasScrolls
	contentRect					= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect absolutePos wItemSize`)

forceValidCompoundClipState :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle .ls .pst,!*OSToolbox)
forceValidCompoundClipState wMetrics allClipStates wPtr parentPos defId isVisible itemH=:{wItemShow,wItemPos,wItemSize,wItemInfo,wItems} tb
	# tb						= disposeClipState clipState tb
	# (clipState,itemHs,tb)		= createClipState wMetrics allClipStates False wPtr absolutePos contentRect defId itemVisible wItems tb
	  compoundInfo				= {compoundInfo & compoundLookInfo={compoundLook & compoundClip=clipState}}
	= ({itemH & wItemInfo=CompoundInfo compoundInfo,wItems=itemHs},tb)
where
	absolutePos					= movePoint wItemPos parentPos
	itemVisible					= isVisible && wItemShow
	compoundInfo				= getWItemCompoundInfo wItemInfo
	compoundLook				= compoundInfo.compoundLookInfo
	clipState					= compoundLook.compoundClip
	domainRect					= compoundInfo.compoundDomain
	hasScrolls					= (isJust compoundInfo.compoundHScroll, isJust compoundInfo.compoundVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize) hasScrolls
	contentRect					= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect absolutePos wItemSize)

forceValidCompoundClipState` :: !OSWindowMetrics !Bool !OSWindowPtr !Point2 !(Maybe Id) !Bool !WItemHandle` !*OSToolbox -> (!WItemHandle`,!*OSToolbox)
forceValidCompoundClipState` wMetrics allClipStates wPtr parentPos defId isVisible itemH=:{wItemShow`,wItemPos`,wItemSize`,wItemInfo`,wItems`} tb
	# tb						= disposeClipState clipState tb
	# (clipState,itemHs,tb)		= createClipState` wMetrics allClipStates False wPtr absolutePos contentRect defId itemVisible wItems` tb
	  compoundInfo				= {compoundInfo & compoundLookInfo={compoundLook & compoundClip=clipState}}
	= ({itemH & wItems`=itemHs,wItemInfo`=CompoundInfo` compoundInfo},tb)
where
	absolutePos					= movePoint wItemPos` parentPos
	itemVisible					= isVisible && wItemShow`
	compoundInfo				= getWItemCompoundInfo` wItemInfo`
	compoundLook				= compoundInfo.compoundLookInfo
	clipState					= compoundLook.compoundClip
	domainRect					= compoundInfo.compoundDomain
	hasScrolls					= (isJust compoundInfo.compoundHScroll, isJust compoundInfo.compoundVScroll)
	visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple wItemSize`) hasScrolls
	contentRect					= osGetCompoundContentRect wMetrics visScrolls (posSizeToRect absolutePos wItemSize`)

invalidateCompoundClipState :: !(WItemHandle .ls .pst) -> WItemHandle .ls .pst
invalidateCompoundClipState itemH=:{wItemInfo}
	# compoundInfo	= getWItemCompoundInfo wItemInfo
	  compoundLook	= compoundInfo.compoundLookInfo
	  clipState		= compoundLook.compoundClip
//	= {itemH & wItemInfo=CompoundInfo {compoundInfo & compoundLookInfo={compoundLook & compoundClip={clipState & clipOk=False}}}}
	= {itemH & wItemInfo=CompoundInfo {compoundInfo & compoundLookInfo.compoundClip.clipOk=False}}

invalidateCompoundClipState` :: !WItemHandle` -> WItemHandle`
invalidateCompoundClipState` itemH=:{wItemInfo`}
	# compoundInfo	= getWItemCompoundInfo` wItemInfo`
	  compoundLook	= compoundInfo.compoundLookInfo
	  clipState		= compoundLook.compoundClip
//	= {itemH & wItemInfo`=CompoundInfo` {compoundInfo & compoundLookInfo={compoundLook & compoundClip={clipState & clipOk=False}}}}
	= {itemH & wItemInfo`=CompoundInfo` {compoundInfo & compoundLookInfo.compoundClip.clipOk=False}}
