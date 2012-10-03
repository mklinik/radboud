implementation module controlinternal


import	StdBool, StdEnum, StdList, StdMisc, StdTuple
import	ospicture, osrgn, ostooltip, oswindow, ossystem
import	commondef, controlresize, wstateaccess
from	StdControlAttribute	import isControlTip, getControlTipAtt, isControlKeyboard
from	controldraw			import drawCustomButtonLook`, drawCustomLook`, drawCompoundLook`,
									drawInCustomButton`,  drawInCustom`,   drawInCompound`
from	controllayout		import layoutControls`
from	controlrelayout		import relayoutControls`
from	controlvalidate		import validateControlTitle, validateSliderState
from	windowaccess		import identifyMaybeId, getWItemPopUpInfo, getWindowInfoWindowData
from	windowclipstate		import validateCompoundClipState`, forceValidCompoundClipState`, invalidateCompoundClipState`
from	windowupdate		import updatewindowbackgrounds`
from	windowvalidate		import validateViewDomain


//	General occurrence tests on Id.

maybeRemoveCheck :: !(Maybe x) [x] -> (!Bool,![x])	| Eq x
maybeRemoveCheck (Just id) ids
	= removeCheck id ids
maybeRemoveCheck nothing ids
	= (False,ids)

removeOnIdOfPair :: !(Maybe Id) ![(Id,x)] -> (!Bool,(Id,x),![(Id,x)])
removeOnIdOfPair (Just id) id_args
	= remove (\(id`,_)->id`==id) undef id_args
removeOnIdOfPair nothing id_args
	= (False,undef,id_args)

removeOnIdOfTriple :: !(Maybe Id) ![(Id,x,y)] -> (!Bool,(Id,x,y),![(Id,x,y)])
removeOnIdOfTriple (Just id) id_args
	= remove (\(id`,_,_)->id`==id) undef id_args
removeOnIdOfTriple nothing id_args
	= (False,undef,id_args)


/*	getContentRect returns the content rect of the window. 
*/
getContentRect :: !OSWindowMetrics !WindowInfo !Size -> OSRect
getContentRect wMetrics (WindowInfo wInfo) size
	= osGetWindowContentRect wMetrics visScrolls (sizeToRect size)
where
	domainRect	= wInfo.windowDomain
	hasScrolls	= (isJust wInfo.windowHScroll,isJust wInfo.windowVScroll)
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple size) hasScrolls
getContentRect _ (GameWindowInfo gwInfo) _
	= sizeToRect gwInfo.gamewindowSize
getContentRect _ NoWindowInfo size
	= sizeToRect size

/*	Calculate the intersection of the given OSRect with the content of a CompoundControl.
	The Point2 argument must be the absolute position of the compound control.
	The Size   argument must be the exact size of the compound control.
*/
intersectRectContent :: !OSWindowMetrics !OSRect !CompoundInfo !Point2 !Size -> OSRect
intersectRectContent wMetrics clipRect info itemPos itemSize
	= intersectRects clipRect contentRect
where
	hasScrolls	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
	domainRect	= info.compoundDomain
	itemRect	= posSizeToRect itemPos itemSize
	visScrolls	= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
	contentRect	= osGetCompoundContentRect wMetrics visScrolls itemRect


/*	Enable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be enabled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
*/
enablecontrols :: ![Id] !Bool !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
enablecontrols ids overrule wMetrics wPtr wH=:{whItems`,whShow`,whSelect`,whSize`,whDefaultId`} tb
	# (itemHs,(_,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr whDefaultId` overrule whSelect` whShow` clipRect zero) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics wH.whWindowInfo` whSize`
	
	enableWItemHandle` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !OSRect !Point2 !WItemHandle` !(![Id],!*OSToolbox)
	                                                                                              -> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect _ itemH=:{wItemKind`} (ids,tb)
		| systemControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				= ({itemH & wItemSelect`=True},(ids,systemOSAction contextSelect tb))
			| overrule
				= (itemH,(ids,systemOSAction (contextSelect && itemSelect) tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr		= itemH.wItemPtr`
		itemSelect	= itemH.wItemSelect`
		radioItems	= (getWItemRadioInfo` itemH.wItemInfo`).radioItems`
		checkItems	= (getWItemCheckInfo` itemH.wItemInfo`).checkItems`
		(systemControl,systemOSAction)
					= case wItemKind` of
						IsRadioControl	-> (True,\able->stateMap2 (\{radioItemPtr`}->osSetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->stateMap2 (\{checkItemPtr`}->osSetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
  						IsPopUpControl	-> (True,osSetPopUpControlSelect  wPtr itemPtr clipRect)
  						IsSliderControl	-> (True,osSetSliderControlSelect wPtr itemPtr clipRect)
  						IsTextControl	-> (True,osSetTextControlSelect   wPtr itemPtr clipRect)
  						IsEditControl	-> (True,osSetEditControlSelect   wPtr itemPtr clipRect)
  						IsButtonControl	-> (True,osSetButtonControlSelect wPtr itemPtr clipRect)
  						_				-> (False,undef)
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`} (ids,tb)
		| customControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				# itemH		= {itemH & wItemSelect`=True}
				# tb		= customOSAction contextSelect tb
				# (itemH,tb)= customDraw contextSelect wPtr parentPos clipRect itemH tb
				= (itemH,(ids,tb))
			| overrule
				# select	= contextSelect && itemSelect
				# (itemH,tb)= customDraw select wPtr parentPos clipRect itemH tb
				= (itemH,(ids,customOSAction select tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr				= itemH.wItemPtr`
		itemSelect			= itemH.wItemSelect`
		(customControl,customDraw,customOSAction)
							= case wItemKind` of
		  						IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
		  						IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlSelect       wPtr itemPtr clipRect)
		  						_						-> (False,undef,undef)
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr parentPos defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect wPtr parentPos clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId True contextSelect contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo scrollPtrs contextSelect tb
			  itemH				= {itemH & wItemSelect`=True,wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr parentPos defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr parentPos clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo scrollPtrs contextSelect1 tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| otherwise
			# (itemHs,(ids,tb))	= setAllWElements (enableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemPtr					= itemH.wItemPtr`
		itemSelect				= itemH.wItemSelect`
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		scrollInfo				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hPtr | fst scrollInfo	= (fromJust info.compoundHScroll).scrollItemPtr
								= OSNoWindowPtr
		vPtr | snd scrollInfo	= (fromJust info.compoundVScroll).scrollItemPtr
								= OSNoWindowPtr
		scrollPtrs				= (hPtr,vPtr)
		itemSize				= itemH.wItemSize`
		contextSelect1			= contextSelect && itemSelect
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemSize
	
	enableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextSelect1		= if found contextSelect (contextSelect && itemSelect)
		  (itemHs,(ids,tb))		= setAllWElements (enableWItemHandle` wMetrics wPtr defId (overrule || found) contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItemSelect`=found || itemSelect,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemSelect				= itemH.wItemSelect`
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
	
	enableWItemHandle` _ _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found					= ({itemH & wItemSelect`=True},(ids,tb))
		| otherwise				= (itemH,(ids,tb))


/*	Disable the controls and provide proper feedback.
	The [Id] argument contains the Ids of the controls that should be (dis/en)abled.
	The Boolean argument controls the new SelectState. 
		If the Boolean argument is False, then SelectState is the new SelectState of the indicated controls.
		If the Boolean argument is True,  then SelectState is the new SelectState of the indicated controls 
										  and  all other controls. 
*/
disablecontrols :: ![Id] !Bool !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
disablecontrols ids overrule wMetrics wPtr wH=:{whItems`,whShow`,whSelect`,whSize`,whDefaultId`} tb
	# (itemHs,(_,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr whDefaultId` overrule whSelect` whShow` clipRect zero) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics wH.whWindowInfo` whSize`
	
	disableWItemHandle` :: !OSWindowMetrics !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !OSRect !Point2 !WItemHandle` !(![Id],!*OSToolbox)
	                                                                                               -> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect _ itemH=:{wItemKind`} (ids,tb)
		| systemControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				= ({itemH & wItemSelect`=False},(ids,systemOSAction False tb))
			| overrule
				= (itemH,(ids,systemOSAction (contextSelect && itemSelect) tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr		= itemH.wItemPtr`
		itemSelect	= itemH.wItemSelect`
		radioItems	= (getWItemRadioInfo` itemH.wItemInfo`).radioItems`
		checkItems	= (getWItemCheckInfo` itemH.wItemInfo`).checkItems`
		(systemControl,systemOSAction)
					= case wItemKind` of
						IsRadioControl	-> (True,\able->stateMap2 (\{radioItemPtr`}->osSetRadioControlSelect wPtr radioItemPtr` clipRect able) radioItems)
						IsCheckControl	-> (True,\able->stateMap2 (\{checkItemPtr`}->osSetCheckControlSelect wPtr checkItemPtr` clipRect able) checkItems)
						IsPopUpControl	-> (True,osSetPopUpControlSelect  wPtr itemPtr clipRect)
						IsSliderControl	-> (True,osSetSliderControlSelect wPtr itemPtr clipRect)
						IsTextControl	-> (True,osSetTextControlSelect   wPtr itemPtr clipRect)
						IsEditControl	-> (True,osSetEditControlSelect   wPtr itemPtr clipRect)
						IsButtonControl	-> (True,osSetButtonControlSelect wPtr itemPtr clipRect)
						_				-> (False,undef)
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`} (ids,tb)
		| customControl
			# (found,ids)	= maybeRemoveCheck itemH.wItemId` ids
			| found
				# itemH		= {itemH & wItemSelect`=False}
				# tb		= customOSAction False tb
				# (itemH,tb)= customDraw False wPtr parentPos clipRect itemH tb
				= (itemH,(ids,tb))
			| overrule
				# select	= contextSelect && itemSelect
				# (itemH,tb)= customDraw select wPtr parentPos clipRect itemH tb
				= (itemH,(ids,customOSAction select tb))
			// otherwise
				= (itemH,(ids,tb))
	where
		itemPtr				= itemH.wItemPtr`
		itemSelect			= itemH.wItemSelect`
		(customControl,customDraw,customOSAction)
							= case wItemKind` of
								IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlSelect wPtr itemPtr clipRect)
								IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlSelect       wPtr itemPtr clipRect)
								_						-> (False,undef,undef)
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found
			# itemH				= {itemH & wItemSelect`=False}
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr parentPos defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics False wPtr parentPos clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId True False contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo scrollPtrs False tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| overrule
			# (itemH,tb)		= validateCompoundClipState` wMetrics False wPtr parentPos defId contextShow itemH tb
			# (itemH,tb)		= drawCompoundLook` wMetrics contextSelect1 wPtr parentPos clipRect1 itemH tb
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			# tb				= osSetCompoundSelect wPtr itemPtr clipRect scrollInfo scrollPtrs contextSelect1 tb
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
		| otherwise
			# (itemHs,(ids,tb))	= setAllWElements (disableWItemHandle` wMetrics wPtr defId overrule contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
			  itemH				= {itemH & wItems`=itemHs}
			= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemPtr					= itemH.wItemPtr`
		itemSelect				= itemH.wItemSelect`
		itemSize				= itemH.wItemSize`
		contextSelect1			= contextSelect && itemSelect
		contextShow1			= contextShow && itemH.wItemShow`
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		scrollInfo				= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hPtr	| fst scrollInfo= (fromJust info.compoundHScroll).scrollItemPtr
								= OSNoWindowPtr
		vPtr	| snd scrollInfo= (fromJust info.compoundVScroll).scrollItemPtr
								= OSNoWindowPtr
		scrollPtrs				= (hPtr,vPtr)
		clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemSize
	
	disableWItemHandle` wMetrics wPtr defId overrule contextSelect contextShow clipRect parentPos itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextSelect1		= if found False (contextSelect && itemSelect)
		# (itemHs,(ids,tb))		= setAllWElements (disableWItemHandle` wMetrics wPtr defId (found || overrule) contextSelect1 contextShow1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItemSelect`=if found False itemSelect,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemSelect				= itemH.wItemSelect`
		contextShow1			= contextShow && itemH.wItemShow`
		clipRect1				= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
	
	disableWItemHandle` _ _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| found					= ({itemH & wItemSelect`=False},(ids,tb))
		| otherwise				= (itemH,(ids,tb))


/*	Set the show state of the controls and provide proper feedback.
*/
setcontrolsshowstate :: ![Id] !Bool !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsshowstate ids itemsShow wMetrics wids=:{wPtr} wH=:{whItems`,whSelect`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))	= setWElements (setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect zero) whItems` (ids,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics whWindowInfo` whSize`
	overrule			= False
	contextShow			= True	// DvA: moet dit niet itemsShow zijn? PA: nee, want itemsShow bepaalt de nieuwe show-state van elementen; contextShow bepaalt show-state window
	contextSelect		= if whSelect` Able Unable
	
	setWItemShowStates :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !Bool !SelectState !OSRect !Point2 !WItemHandle` !(![Id],!*OSToolbox)
	                                                                                               -> (!WItemHandle`,!(![Id],!*OSToolbox))
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect _ itemH=:{wItemKind`=IsRadioControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			  info				= getWItemRadioInfo` itemH.wItemInfo`
			# tb				= stateMap2 (setradio osShow clipRect) info.radioItems` tb
			= (itemH,(ids,tb))
	where
		setradio :: !Bool !OSRect !RadioItemInfo` !*OSToolbox -> *OSToolbox
		setradio osShow clipRect {radioItemPtr`,radioItemSize`} tb
			= osSetRadioControlShow wPtr radioItemPtr` clipRect osShow tb
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect _ itemH=:{wItemKind`=IsCheckControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			  info				= getWItemCheckInfo` itemH.wItemInfo`
			# tb				= stateMap2 (setcheck osShow clipRect) info.checkItems` tb
			= (itemH,(ids,tb))
	where
		setcheck :: !Bool !OSRect !CheckItemInfo` !*OSToolbox -> *OSToolbox
		setcheck osShow clipRect {checkItemPtr`,checkItemSize`} tb
			= osSetCheckControlShow wPtr checkItemPtr` clipRect osShow tb
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect parentPos itemH=:{wItemKind`=IsTextControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			  info				= getWItemTextInfo` itemH.wItemInfo`
			  text				= info.textInfoText
			  itemRect			= posSizeToRect absolutePos itemH.wItemSize`
			# tb				= osSetTextControlShow wPtr itemH.wItemPtr` clipRect itemRect osShow text tb
			= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos

	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect _ itemH=:{wItemKind`} (ids,tb)
		| osControl
			# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
			| not found && not overrule
				= (itemH,(ids,tb))
			// otherwise
				# osShow		= if overrule contextShow (contextShow && itemsShow)
				  itemH			= if found {itemH & wItemShow`=itemsShow} itemH
				# tb			= osAction wPtr itemH.wItemPtr` clipRect osShow tb
				= (itemH,(ids,tb))
	where
		(osControl,osAction)	= case wItemKind` of
									IsPopUpControl  -> (True,osSetPopUpControlShow)
									IsSliderControl -> (True,osSetSliderControlShow)
									IsEditControl   -> (True,osSetEditControlShow)
									IsButtonControl -> (True,osSetButtonControlShow)
									_               -> (False,undef)
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect parentPos itemH=:{wItemKind`} (ids,tb)
		| isCustom
			# (found,ids)		= maybeRemoveCheck itemH.wItemId` ids
			| not found && not overrule
				= (itemH,(ids,tb))
			// otherwise
				# osShow		= if overrule contextShow (contextShow && itemsShow)
				  itemH			= if found {itemH & wItemShow`=itemsShow} itemH
				  customDraw	= if osShow customDraw (\_ _ _ _ itemH tb -> (itemH,tb))
				  itemRect		= posSizeToRect absolutePos itemH.wItemSize`
				# tb			= osAction wPtr itemH.wItemPtr` itemRect clipRect osShow tb
				# (itemH,tb)	= customDraw itemH.wItemSelect` wPtr parentPos clipRect itemH tb
				= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		(isCustom,customDraw,osAction)
				  				= case wItemKind` of
				  					IsCustomButtonControl	-> (True,drawCustomButtonLook`,osSetCustomButtonControlShow)
				  					IsCustomControl			-> (True,drawCustomLook`,      osSetCustomControlShow)
				  					_						-> (False,undef,undef)
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect parentPos itemH=:{wItemKind`=IsCompoundControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextShow1			= contextShow && (if found itemsShow itemShow)
		  overrule1				= overrule || found && itemShow<>itemsShow
		# (itemHs,(ids,tb))		= setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItems`=itemHs}
		| not found && not overrule
			= (itemH,(ids,tb))
		| otherwise
			# itemH				= if found {itemH & wItemShow`=itemsShow} itemH
			# itemRect			= posSizeToRect absolutePos itemSize
			# osShow			= if overrule contextShow (contextShow && itemsShow)
			  customDraw		= if osShow customDraw (\_ _ _ _ itemH tb -> (itemH,tb))
			# tb				= osSetCompoundShow wPtr itemH.wItemPtr` itemRect clipRect osShow tb
			# (itemH,tb)		= customDraw itemH.wItemSelect` wPtr parentPos clipRect itemH tb
			= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemShow				= itemH.wItemShow`
		customDraw				= drawCompoundLook` wMetrics
		contextSelect1			= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		itemSize				= itemH.wItemSize`
		clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemSize
	
	setWItemShowStates wMetrics wPtr overrule itemsShow contextShow contextSelect clipRect parentPos itemH=:{wItemKind`=IsLayoutControl} (ids,tb)
		# (found,ids)			= maybeRemoveCheck itemH.wItemId` ids
		  contextShow1			= contextShow && (if found itemsShow itemShow)
		  overrule1				= overrule || found && itemShow<>itemsShow
		# (itemHs,(ids,tb))		= setAllWElements (setWItemShowStates wMetrics wPtr overrule1 itemsShow contextShow1 contextSelect1 clipRect1 absolutePos) itemH.wItems` (ids,tb)
		  itemH					= {itemH & wItemShow`=if found itemsShow itemShow,wItems`=itemHs}
		= (itemH,(ids,tb))
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		itemShow				= itemH.wItemShow`
		contextSelect1			= if (enabled contextSelect) (if itemH.wItemSelect` Able Unable) contextSelect
		clipRect1				= clipRect//intersectRects clipRect (posSizeToRect itemH.wItemPos` itemH.wItemSize`)
	
	setWItemShowStates _ _ _ _ _ _ _ _ itemH=:{wItemKind`=IsOtherControl _} (ids,tb)
		# (_,ids)				= maybeRemoveCheck itemH.wItemId` ids
		= (itemH,(ids,tb))


//	Set the MarkState of the controls and provide proper feedback.

setcontrolsmarkstate :: !Id !MarkState ![Index] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolsmarkstate id mark indexs wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)					= setWElement (setWItemMarks wMetrics wPtr mark zero clipRect indexs) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect						= getContentRect wMetrics whWindowInfo` whSize`
	
	setWItemMarks :: !OSWindowMetrics !OSWindowPtr !MarkState !Point2 !OSRect ![Index] !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setWItemMarks wMetrics wPtr mark _ clipRect indexs id itemH=:{wItemKind`=IsCheckControl} tb
		| identifyMaybeId id itemH.wItemId`
			# info					= getWItemCheckInfo` itemH.wItemInfo`
			  checkItems			= info.checkItems`
			  nrCheckItems			= length checkItems
			  indexs				= filter (\index->isBetween index 1 nrCheckItems) indexs
			| isEmpty indexs
				= (True,itemH,tb)
			// otherwise
				# (checkItems,tb)	= stateMap2 (setCheckMark wPtr clipRect mark) indexs (checkItems,tb)
				  itemH				= {itemH & wItemInfo`=CheckInfo` {info & checkItems`=checkItems}}
				= (True,itemH,tb)
		| otherwise
			= (False,itemH,tb)
	where
		setCheckMark :: !OSWindowPtr !OSRect !MarkState !Index !(![CheckItemInfo`],!*OSToolbox) -> (![CheckItemInfo`],!*OSToolbox)
		setCheckMark wPtr clipRect mark index (checkItems,tb)
			# (before,[item:after])	= split (index-1) checkItems
			  (title,width,_)		= item.checkItem`
			  checkItems			= before++[{item & checkItem`=(title,width,mark)}:after]
			= (checkItems,osSetCheckControl wPtr item.checkItemPtr` clipRect (marked mark) tb)
	
	setWItemMarks wMetrics wPtr mark parentPos clipRect indexs id itemH=:{wItemKind`} tb
		| wItemKind`==IsCompoundControl
			# (found,itemHs,tb)		= setWElement (setWItemMarks wMetrics wPtr mark absolutePos clipRect1 indexs) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
		with
			info					= getWItemCompoundInfo` itemH.wItemInfo`
			clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemSize
		| wItemKind`==IsLayoutControl
			# (found,itemHs,tb)		= setWElement (setWItemMarks wMetrics wPtr mark absolutePos clipRect1 indexs) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
		with
			clipRect1				= intersectRects clipRect (posSizeToRect absolutePos itemSize)
		| otherwise
			= (False,itemH,tb)
	where
		absolutePos					= movePoint itemH.wItemPos` parentPos
		itemSize					= itemH.wItemSize`

//	Set the text of the controls and provide proper feedback.

setcontroltexts :: ![(Id,String)] !OSWindowMetrics !OSWindowPtr !WindowHandle2 !*OSToolbox -> (!WindowHandle2,!*OSToolbox)
setcontroltexts id_texts wMetrics wPtr wH`=:{whItems2,whSize2,whWindowInfo2} tb
	# clipRect = getContentRect wMetrics whWindowInfo2 whSize2
	# (itemHs,(_,tb)) = setWElementsControlText whItems2 True clipRect zero (id_texts,tb)
	= ({wH` & whItems2=itemHs},tb)
where
	setWElementsControlText :: [WElementHandle2] Bool OSRect Point2 *([(Id,{#Char})],*Int) -> *(![WElementHandle2],!*(![(Id,{#Char})],!*Int))
	setWElementsControlText [itemH:itemHs] shownContext clipRect absolutePos (args=:[_:_],s)
		# (itemH, args_s)	= setWElementsControlText` itemH  shownContext clipRect absolutePos (args,s)
		# (itemHs,args_s)	= setWElementsControlText  itemHs shownContext clipRect absolutePos args_s
		= ([itemH:itemHs],args_s)
	setWElementsControlText itemHs shownContext clipRect absolutePos (args,s)
		= (itemHs,(args,s))

	setWElementsControlText` :: WElementHandle2 Bool OSRect Point2 *([(Id,{#Char})],*Int) -> *(!WElementHandle2,!*(![(Id,{#Char})],!*Int))
	setWElementsControlText` weh=:(WItemHandle2 itemH) shownContext clipRect absolutePos args_s
		= setControlText wMetrics wPtr shownContext clipRect absolutePos weh args_s
	setWElementsControlText` (WRecursiveHandle2 itemHs dRecKind) shownContext clipRect absolutePos args_s
		# (itemHs,args_s)	= setWElementsControlText itemHs shownContext clipRect absolutePos args_s
		= (WRecursiveHandle2 itemHs dRecKind,args_s)

	setControlText :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect !Point2 !WElementHandle2 !(![(Id,String)],!*OSToolbox)
	                                                                  -> (!WElementHandle2,!(![(Id,String)],!*OSToolbox))
	
	setControlText wMetrics wPtr shownContext clipRect parentPos weh=:(WItemHandle2 itemH=:{wItemKind2=IsEditControl}) (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId2 id_texts
		| not found
			= (weh,(id_texts,tb))
		| otherwise
			# shownContext1			= shownContext && itemH.wItemShow2
			  (_,text)				= id_text
			  editInfo				= getWItemEditInfo` itemH.wItemInfo2
			  itemH					= {itemH & wItemInfo2=EditInfo` {editInfo & editInfoText=text}}
			  itemRect				= posSizeToRect absolutePos itemH.wItemSize2
			# tb					= osSetEditControlText wPtr itemH.wItemPtr2 clipRect itemRect shownContext1 text tb
			= (WItemHandle2 itemH,(id_texts,tb))
	where
		absolutePos					= movePoint itemH.wItemPos2 parentPos
	
	setControlText wMetrics wPtr shownContext clipRect parentPos weh=:(WItemHandle2 itemH=:{wItemKind2=IsTextControl}) (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId2 id_texts
		| not found
			= (weh,(id_texts,tb))
		| otherwise
			# (_,text)				= id_text
			  shownContext1			= shownContext && itemH.wItemShow2
			#! textInfo				= getWItemTextInfo` itemH.wItemInfo2
			# itemH					= {itemH & wItemInfo2=TextInfo` {textInfo & textInfoText=text}}
			  itemRect				= posSizeToRect absolutePos itemH.wItemSize2
			# tb					= osSetTextControlText wPtr itemH.wItemPtr2 clipRect itemRect shownContext1 text tb
			= (WItemHandle2 itemH,(id_texts,tb))
	where
		absolutePos					= movePoint itemH.wItemPos2 parentPos

	setControlText wMetrics wPtr _ clipRect _ weh=:(WItemHandle2 itemH=:{wItemKind2=IsButtonControl}) (id_texts,tb)
		# (found,id_text,id_texts)	= removeOnIdOfPair itemH.wItemId2 id_texts
		| not found
			= (weh,(id_texts,tb))
		| otherwise
			# (_,text)				= id_text
			  buttonInfo			= getWItemButtonInfo` itemH.wItemInfo2
			  itemH					= {itemH & wItemInfo2=ButtonInfo` {buttonInfo & buttonInfoText=text}}
			# tb					= osSetButtonControlText wPtr itemH.wItemPtr2 clipRect (validateControlTitle text) tb
			= (WItemHandle2 itemH,(id_texts,tb))

	setControlText wMetrics wPtr shownContext clipRect parentPos (WItemHandle2 itemH=:{wItemKind2=IsCompoundControl}) s
		# (itemHs,s)				= setWElementsControlText itemH.wItems2 shownContext1 clipRect1 absolutePos s
		= (WItemHandle2 {itemH & wItems2=itemHs},s)
	where
		absolutePos					= movePoint itemH.wItemPos2 parentPos
		info						= getWItemCompoundInfo` itemH.wItemInfo2
		clipRect1					= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize2
		shownContext1				= shownContext && itemH.wItemShow2
	
	setControlText wMetrics wPtr shownContext clipRect parentPos (WItemHandle2 itemH=:{wItemKind2=IsLayoutControl}) s
		# (itemHs,s)	= setWElementsControlText itemH.wItems2 shownContext1 clipRect1 absolutePos s
		= (WItemHandle2 {itemH & wItems2=itemHs},s)
	where
		absolutePos					= movePoint itemH.wItemPos2 parentPos
		clipRect1					= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize2)
		shownContext1				= shownContext && itemH.wItemShow2

	setControlText _ _ _ _ _ itemH s
		= (itemH,s)

//	Set the cursor position of an EditControl, and handle proper feedback.

seteditcontrolcursor :: !Id !Int !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
seteditcontrolcursor id pos wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (setEditCursor wMetrics wPtr True clipRect zero pos) id whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setEditCursor :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect !Point2 !Int !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setEditCursor wMetrics wPtr shownContext clipRect parentPos pos id itemH=:{wItemKind`=IsEditControl} tb
		| not (identifyMaybeId id itemH.wItemId`)
			= (False,itemH,tb)
		| otherwise
			# itemRect		= posSizeToRect absolutePos itemH.wItemSize`
			# tb			= osSetEditControlCursor wPtr itemH.wItemPtr` clipRect itemRect pos tb
			= (True,itemH,tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
	
	setEditCursor wMetrics wPtr shownContext clipRect parentPos pos id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 absolutePos pos) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize`
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditCursor wMetrics wPtr shownContext clipRect parentPos pos id itemH=:{wItemKind`=IsLayoutControl} tb
		# (found,itemHs,tb)	= setWElement (setEditCursor wMetrics wPtr shownContext1 clipRect1 absolutePos pos) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		clipRect1			= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditCursor _ _ _ _ _ _ _ itemH tb
		= (False,itemH,tb)


//	Set the text selection of an EditControl, and handle proper feedback.

seteditcontrolselection :: !Id !Int !Int !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
seteditcontrolselection id begin end wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (setEditSelection wMetrics wPtr True clipRect zero begin end) id whItems` tb
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setEditSelection :: !OSWindowMetrics !OSWindowPtr !Bool !OSRect !Point2 !Int !Int !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	setEditSelection wMetrics wPtr shownContext clipRect parentPos begin end id itemH=:{wItemKind`=IsEditControl} tb
		| not (identifyMaybeId id itemH.wItemId`)
			= (False,itemH,tb)
		| otherwise
			# itemRect		= posSizeToRect absolutePos itemH.wItemSize`
			# tb			= osSetEditControlSelection wPtr itemH.wItemPtr` clipRect itemRect begin end tb
			= (True,itemH,tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
	
	setEditSelection wMetrics wPtr shownContext clipRect parentPos begin end id itemH=:{wItemKind`=IsCompoundControl} tb
		# (found,itemHs,tb)	= setWElement (setEditSelection wMetrics wPtr shownContext1 clipRect1 absolutePos begin end) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize`
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditSelection wMetrics wPtr shownContext clipRect parentPos begin end id itemH=:{wItemKind`=IsLayoutControl} tb
		# (found,itemHs,tb)	= setWElement (setEditSelection wMetrics wPtr shownContext1 clipRect1 absolutePos begin end) id itemH.wItems` tb
		= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		clipRect1			= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
		shownContext1		= shownContext && itemH.wItemShow`
	
	setEditSelection _ _ _ _ _ _ _ _ itemH tb
		= (False,itemH,tb)


//	Set the look of a control, and handle proper feedback.

setcontrolslook :: ![(Id,Bool,(Bool,Look))] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolslook looks wMetrics wPtr wH=:{whItems`,whSelect`,whDefaultId`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))	= setWElements (setWItemLook wMetrics wPtr whSelect` True resizeable whDefaultId` clipRect zero) whItems` (looks,tb)
	= ({wH & whItems`=itemHs},tb)
where
	clipRect			= getContentRect wMetrics whWindowInfo` whSize`
	resizeable			= True
	
	setWItemLook :: !OSWindowMetrics !OSWindowPtr !Bool !Bool !Bool (Maybe Id) !OSRect !Point2 !WItemHandle` !(![(Id,Bool,(Bool,Look))],!*OSToolbox)
	                                                                                       -> (!WItemHandle`,!(![(Id,Bool,(Bool,Look))],!*OSToolbox))
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect parentPos itemH=:{wItemId`,wItemKind`=IsCompoundControl,wItemSize`} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		# (itemHs,looks_tb)			= setWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1 absolutePos) itemH.wItems` (looks,tb)
		  itemH						= {itemH & wItems`=itemHs}
		| not found
			= (itemH,looks_tb)
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & compoundLookInfo={lookInfo & compoundLook = {	lookFun			= cLook
		  																				  ,	lookPen			= pen
		  																				  ,	lookSysUpdate	= sysLook
		  																				  }}}
		  itemH						= {itemH & wItemInfo`=CompoundInfo` info}
		| not redraw || not shownContext1
			= (itemH,looks_tb)
		| otherwise
			# (looks,tb)			= looks_tb
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr parentPos defId shownContext itemH tb
			# (itemH,tb)			= drawCompoundLook` wMetrics ableContext1 wPtr parentPos clipRect1 itemH tb
			= (itemH,(looks,tb))
	where
		absolutePos					= movePoint itemH.wItemPos` parentPos
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		compoundLookInfo			= info.compoundLookInfo
		lookInfo					= compoundLookInfo
		pen							= compoundLookInfo.compoundLook.lookPen
		visScrolls					= osScrollbarsAreVisible wMetrics info.compoundDomain (toTuple wItemSize`) hasScrolls
		itemRect					= posSizeToRect absolutePos wItemSize`
		contentRect					= osGetCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= intersectRects clipRect contentRect
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect parentPos itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		| not found
			= (itemH,(looks,tb))
		# (_,redraw,(sysLook,cLook))= look
		  info						= {info & cButtonInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		  itemH						= {itemH & wItemInfo`=CustomButtonInfo` info}
		| not redraw || not shownContext1
			= (itemH,(looks,tb))
		| otherwise
			# (itemH,tb)			= drawCustomButtonLook` ableContext1 wPtr parentPos clipRect itemH tb
			= (itemH,(looks,tb))
	where
		info						= getWItemCustomButtonInfo` itemH.wItemInfo`
		itemLook					= info.cButtonInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect parentPos itemH=:{wItemId`,wItemKind`=IsCustomControl} (looks,tb)
		# (found,look,looks)		= removeOnIdOfTriple wItemId` looks
		| not found
			= (itemH,(looks,tb))
		# (_,redraw,(sysLook,cLook))= look
		# info						= {info & customInfoLook={itemLook & lookFun=cLook,lookSysUpdate=sysLook}}
		# itemH						= {itemH & wItemInfo`=CustomInfo` info}
		| not redraw || not shownContext1
			= (itemH,(looks,tb))
		| otherwise
			# (itemH,tb)			= drawCustomLook` ableContext1 wPtr parentPos clipRect itemH tb
			= (itemH,(looks,tb))
	where
		info						= getWItemCustomInfo` itemH.wItemInfo`
		itemLook					= info.customInfoLook
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook wMetrics wPtr ableContext shownContext resizeable defId clipRect parentPos itemH=:{wItemId`,wItemKind`=IsLayoutControl} (looks,tb)
		# (_,_,looks)				= removeOnIdOfTriple wItemId` looks
		# (itemHs,looks_tb)			= setWElements (setWItemLook wMetrics wPtr ableContext1 shownContext1 resizeable defId clipRect1 absolutePos) itemH.wItems` (looks,tb)
		  itemH						= {itemH & wItems`=itemHs}
		= (itemH,looks_tb)
	where
		absolutePos					= movePoint itemH.wItemPos` parentPos
		clipRect1					= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
		ableContext1				= ableContext  && itemH.wItemSelect`
		shownContext1				= shownContext && itemH.wItemShow`
	
	setWItemLook _ _ _ _ _ _ _ _ itemH=:{wItemId`} (looks,tb)
		# (_,_,looks)				= removeOnIdOfTriple wItemId` looks
		= (itemH,(looks,tb))


//	Draw in a customised control.

drawincontrol :: !Id !.(St *Picture .x) !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> *(!Maybe .x,!WindowHandle`,!*OSToolbox)
drawincontrol controlId drawfun wMetrics wPtr wH=:{whItems`,whDefaultId`,whShow`,whSize`,whWindowInfo`} tb
	# (_,itemHs,(x,_,tb))	= setWElement (drawInWItem wMetrics wPtr resizeable whDefaultId` whShow` clipRect zero) controlId whItems` (Nothing,drawfun,tb)
	= (x,{wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	resizeable				= True
	
	drawInWItem :: !OSWindowMetrics !OSWindowPtr Bool (Maybe Id) !Bool !OSRect !Point2 !Id !WItemHandle` !*(!v:(Maybe u:x),w:(St *Picture u:x),!*OSToolbox)
	                                                                             -> (!Bool,!WItemHandle`,!*(!v:(Maybe u:x),w:(St *Picture u:x),!*OSToolbox)), [v<=u]
	
	drawInWItem wMetrics wPtr resizeable defId contextShow clipRect parentPos id itemH=:{wItemId`,wItemPtr`,wItemKind`=IsCompoundControl} x_tb
		| not (identifyMaybeId id wItemId`)
			# (found,itemHs,x_tb)	= setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1 absolutePos) id itemH.wItems` x_tb
			= (found,{itemH & wItems`=itemHs},x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (itemH,tb)			= validateCompoundClipState` wMetrics False wPtr parentPos defId contextShow itemH tb
			# (x,itemH,tb)			= drawInCompound` drawfun wPtr parentPos clipRect1 itemH tb
			= (True,itemH,(Just x,undef,tb))
	where
		itemShow					= contextShow && itemH.wItemShow`
		info						= getWItemCompoundInfo` itemH.wItemInfo`
		domainRect					= info.compoundDomain
		hasScrolls					= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		absolutePos					= movePoint itemPos parentPos
		itemPos						= itemH.wItemPos`
		itemSize					= itemH.wItemSize`
		itemRect					= posSizeToRect absolutePos itemSize
		visScrolls					= osScrollbarsAreVisible wMetrics domainRect (toTuple itemSize) hasScrolls
		contentRect					= osGetCompoundContentRect wMetrics visScrolls itemRect
		clipRect1					= intersectRects clipRect contentRect
	
	drawInWItem _ wPtr _ _ _ clipRect parentPos id itemH=:{wItemId`,wItemKind`=IsCustomButtonControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustomButton` drawfun wPtr parentPos clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem _ wPtr _ _ _ clipRect parentPos id itemH=:{wItemId`,wItemKind`=IsCustomControl} x_tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,x_tb)
		| otherwise
			# (_,drawfun,tb)		= x_tb
			# (x,itemH,tb)			= drawInCustom` drawfun wPtr parentPos clipRect itemH tb
			= (True,itemH,(Just x,undef,tb))
	
	drawInWItem wMetrics wPtr resizeable defId contextShow clipRect parentPos id itemH=:{wItemId`,wItemKind`=IsLayoutControl} x_tb
		| identifyMaybeId id wItemId`
			= (True,itemH,x_tb)
		| otherwise
			# (found,itemHs,x_tb)	= setWElement (drawInWItem wMetrics wPtr resizeable defId itemShow clipRect1 absolutePos) id itemH.wItems` x_tb
			= (found,{itemH & wItems`=itemHs},x_tb)
	where
		absolutePos					= movePoint itemH.wItemPos` parentPos
		itemShow					= contextShow && itemH.wItemShow`
		clipRect1					= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
	
	drawInWItem _ _ _ _ _ _ _ id itemH=:{wItemId`} x_tb
		= (identifyMaybeId id wItemId`,itemH,x_tb)


//	Change the state of the slider and handle proper feedback.

setslidercontrolstates :: ![(Id,IdFun SliderState)] !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setslidercontrolstates id_fs wMetrics wPtr wH`=:{whItems`,whSize`,whWindowInfo`} tb
	# (itemHs,(_,tb))		= setWElements (setSliderState wMetrics wPtr clipRect zero) whItems` (id_fs,tb)
	= ({wH` & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	setSliderState :: !OSWindowMetrics !OSWindowPtr !OSRect !Point2 !WItemHandle` !(![(Id,IdFun SliderState)],!*OSToolbox)
	                                                            -> (!WItemHandle`,!(![(Id,IdFun SliderState)],!*OSToolbox))
	
	setSliderState wMetrics wPtr clipRect _ itemH=:{wItemId`,wItemKind`=IsSliderControl,wItemPtr`} (id_fs,tb)
		# (found,id_f,id_fs)= removeOnIdOfPair wItemId` id_fs
		| not found
			= (itemH,(id_fs,tb))
		| otherwise
			# info			= getWItemSliderInfo` itemH.wItemInfo`
			  oldState		= info.sliderInfoState`
			  (_,f)			= id_f
			  newState		= validateSliderState (f oldState)
			  itemH			= {itemH & wItemInfo`=SliderInfo` {info & sliderInfoState`=newState}}
			  thumbState	= toOSscrollbarRange (newState.sliderMin,newState.sliderThumb,newState.sliderMax) 0
			# tb			= osSetSliderControlThumb wPtr wItemPtr` clipRect (not (isEmptyRect clipRect)) thumbState tb
			= (itemH,(id_fs,tb))
	
	setSliderState wMetrics wPtr clipRect parentPos itemH=:{wItemKind`=IsCompoundControl} (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		# (itemHs,id_fs_tb)	= setWElements (setSliderState wMetrics wPtr clipRect1 absolutePos) itemH.wItems` (id_fs,tb)
		= ({itemH & wItems`=itemHs},id_fs_tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		info				= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1			= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize`
	
	setSliderState wMetrics wPtr clipRect parentPos itemH=:{wItemKind`=IsLayoutControl} (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		# (itemHs,id_fs_tb)	= setWElements (setSliderState wMetrics wPtr clipRect1 absolutePos) itemH.wItems` (id_fs,tb)
		= ({itemH & wItems`=itemHs},id_fs_tb)
	where
		absolutePos			= movePoint itemH.wItemPos` parentPos
		clipRect1			= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
	
	setSliderState _ _ _ _ itemH (id_fs,tb)
		# (_,_,id_fs)		= removeOnIdOfPair itemH.wItemId` id_fs
		= (itemH,(id_fs,tb))


//	Selecting a RadioControl item.

selectradiocontrol :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectradiocontrol id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemRadioControl wMetrics wPtr clipRect zero index) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemRadioControl :: !OSWindowMetrics !OSWindowPtr !OSRect !Point2 !Index !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemRadioControl wMetrics wPtr clipRect _ index id itemH=:{wItemId`,wItemKind`=IsRadioControl} tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info					= getWItemRadioInfo` itemH.wItemInfo`
		  cur					= info.radioIndex`
		  items					= info.radioItems`
		  nrItems				= length items
		  index					= setBetween index 1 nrItems
		  info					= {info & radioIndex`=index}
		  itemH					= {itemH & wItemInfo`=RadioInfo` info}
		| index==cur
			= (True,itemH,tb)
		| otherwise
			# tb				= osSetRadioControl wPtr (items!!(cur-1)).radioItemPtr` (items!!(index-1)).radioItemPtr` clipRect tb
			= (True,itemH,tb)
	
	selectWItemRadioControl wMetrics wPtr clipRect parentPos index id itemH=:{wItemKind`=IsCompoundControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (done,itemHs,tb)	= setWElement (selectWItemRadioControl wMetrics wPtr clipRect1 absolutePos index) id itemH.wItems` tb
			= (done,{itemH & wItems`=itemHs},tb)
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize`
	
	selectWItemRadioControl wMetrics wPtr clipRect parentPos index id itemH=:{wItemKind`=IsLayoutControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (done,itemHs,tb)	= setWElement (selectWItemRadioControl wMetrics wPtr clipRect1 absolutePos index) id itemH.wItems` tb
			= (done,{itemH & wItems`=itemHs},tb)
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		clipRect1				= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
	
	selectWItemRadioControl _ _ _ _ _ id itemH tb
		= (identifyMaybeId id itemH.wItemId`,itemH,tb)


//	Select a PopUpControl item.
	
selectpopupitem :: !Id !Index !OSWindowMetrics !OSWindowPtr !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
selectpopupitem id index wMetrics wPtr wH=:{whItems`,whSize`,whWindowInfo`} tb
	# (_,itemHs,tb)			= setWElement (selectWItemPopUp wMetrics wPtr True index clipRect zero) id whItems` tb
	= ({wH & whItems`=itemHs},tb)
where
	clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	
	selectWItemPopUp :: !OSWindowMetrics !OSWindowPtr !Bool !Index !OSRect !Point2 !Id !WItemHandle` !*OSToolbox -> (!Bool,!WItemHandle`,!*OSToolbox)
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect parentPos id itemH=:{wItemId`,wItemKind`=IsPopUpControl} tb
		| not (identifyMaybeId id wItemId`)
			= (False,itemH,tb)
		# info					= getWItemPopUpInfo` itemH.wItemInfo`
		  popUps				= info.popUpInfoItems`
		  curindex				= info.popUpInfoIndex`
		  editinfo				= info.popUpInfoEdit`
		  nrPopUps				= length popUps
		  index					= setBetween index 1 nrPopUps
		| curindex==index
			= (True,itemH,tb)
		| otherwise
			# shownContext1		= shownContext && itemH.wItemShow`
			  info				= {info & popUpInfoIndex`=index}
			  itemH				= {itemH & wItemInfo`=PopUpInfo` info}
			  itemRect			= posSizeToRect absolutePos itemH.wItemSize`
			  editPtr			= mapMaybe (\{popUpEditPtr}->popUpEditPtr) editinfo
			# tb				= osSetPopUpControl wPtr itemH.wItemPtr` editPtr clipRect itemRect curindex index (popUps!!(index-1)) shownContext1 tb
			= (True,itemH,tb)
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect parentPos id itemH=:{wItemKind`=IsCompoundControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (found,itemHs,tb)	= setWElement (selectWItemPopUp wMetrics wPtr shownContext1 index clipRect1 absolutePos) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		clipRect1				= intersectRectContent wMetrics clipRect info absolutePos itemH.wItemSize`
		shownContext1			= shownContext && itemH.wItemShow`
	
	selectWItemPopUp wMetrics wPtr shownContext index clipRect parentPos id itemH=:{wItemKind`=IsLayoutControl} tb
		| identifyMaybeId id itemH.wItemId`
			= (True,itemH,tb)
		| otherwise
			# (found,itemHs,tb)	= setWElement (selectWItemPopUp wMetrics wPtr shownContext1 index clipRect1 absolutePos) id itemH.wItems` tb
			= (found,{itemH & wItems`=itemHs},tb)
	where
		absolutePos				= movePoint itemH.wItemPos` parentPos
		clipRect1				= intersectRects clipRect (posSizeToRect absolutePos itemH.wItemSize`)
		shownContext1			= shownContext && itemH.wItemShow`
	
	selectWItemPopUp _ _ _ _ _ _ id itemH tb
		= (identifyMaybeId id itemH.wItemId`,itemH,tb)


//	Add new items to a PopUpControl. 

openpopupitems :: !Id !Index ![PopUpControlItem .pst] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox
																-> (!WindowHandle .ls .pst, !*OSToolbox)
openpopupitems id index newItems wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id whItems tb
	= ({wH & whItems=itemHs},tb)
where
	openWElementsPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id ![WElementHandle .ls .pst] !*OSToolbox
																	  -> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	openWElementsPopUpItems _ _ _ _ [] tb
		= (False,[],tb)
	openWElementsPopUpItems wPtr index newItems id [itemH:itemHs] tb
		# (done,itemH,tb)		= openWElementPopUpItems wPtr index newItems id itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		openWElementPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id !(WElementHandle .ls .pst) !*OSToolbox
																		  -> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		openWElementPopUpItems wPtr id index newItems (WItemHandle itemH) tb
			# (done,itemH,tb)	= openWItemPopUpItems wPtr id index newItems itemH tb
			= (done,WItemHandle itemH,tb)
		where
			openWItemPopUpItems :: !OSWindowPtr !Index ![PopUpControlItem .pst] !Id !(WItemHandle .ls .pst) !*OSToolbox
																		   -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			openWItemPopUpItems wPtr index items id itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= osCreateEmptyPopUpControl wPtr /*popUpPtr*/ (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# maybeEditPtr		= if isEditable (Just editPtr) (Nothing)
				# tb				= osCreatePopUpControlItems newPopUpPtr maybeEditPtr ableContext (map fst newItems) newIndex tb
				# tb				= osDestroyPopUpControl popUpPtr popUpEdit tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)	= cselect isControlTip undef itemH.wItemAtts
				isEditable		= contains isControlKeyboard itemH.wItemAtts
				ableContext		= itemH.wItemSelect
				popUpPtr		= itemH.wItemPtr
				popUpSize		= itemH.wItemSize
				popUpPos		= itemH.wItemPos
				popUpInfo		= getWItemPopUpInfo itemH.wItemInfo
				popUpEdit		= mapMaybe (\{popUpEditPtr}->popUpEditPtr) popUpInfo.popUpInfoEdit
				curEditInfo		= fromJust popUpInfo.popUpInfoEdit
				curIndex		= popUpInfo.popUpInfoIndex
				curItems		= popUpInfo.popUpInfoItems
				newItems		= before++[(title,noLS f)\\(title,f)<-items]++after
				newIndex		= if (curIndex<=index) curIndex (curIndex+index)
				(before,after)	= split index curItems
				
			openWItemPopUpItems wPtr index newItems id itemH=:{wItemId,wItems} tb
				| identifyMaybeId id wItemId
					= (True,itemH,tb)
				| otherwise
					# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id wItems tb
					= (done,{itemH & wItems=itemHs},tb)
		
		openWElementPopUpItems wPtr index newItems id (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		openWElementPopUpItems wPtr index newItems id (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		openWElementPopUpItems wPtr index newItems id (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= openWElementsPopUpItems wPtr index newItems id itemHs tb
			= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


//	Remove items from a PopUpControl. 

closepopupitems :: !Id ![Index] !OSWindowPtr !(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle .ls .pst,!*OSToolbox)
closepopupitems id indexs wPtr wH=:{whItems} tb
	# (_,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id whItems tb
	= ({wH & whItems=itemHs},tb)
where
	closeWElementsPopUpItems :: !OSWindowPtr ![Index] !Id ![WElementHandle .ls .pst] !*OSToolbox
												-> (!Bool,![WElementHandle .ls .pst],!*OSToolbox)
	closeWElementsPopUpItems _ _ _ [] tb
		= (False,[],tb)
	closeWElementsPopUpItems wPtr indexs id [itemH:itemHs] tb
		# (done,itemH,tb)		= closeWElementPopUpItems wPtr indexs id itemH tb
		| done
			= (done,[itemH:itemHs],tb)
		| otherwise
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,[itemH:itemHs],tb)
	where
		closeWElementPopUpItems :: !OSWindowPtr ![Index] !Id !(WElementHandle .ls .pst) !*OSToolbox
													-> (!Bool,!WElementHandle .ls .pst, !*OSToolbox)
		closeWElementPopUpItems wPtr indexs id (WItemHandle itemH) tb
			# (done,itemH,tb)	= closeWItemPopUpItems wPtr indexs id itemH tb
			= (done,WItemHandle itemH,tb)
		where
			closeWItemPopUpItems :: !OSWindowPtr ![Index] !Id !(WItemHandle .ls .pst) !*OSToolbox
													 -> (!Bool,!WItemHandle .ls .pst, !*OSToolbox)
			
			closeWItemPopUpItems wPtr indexs id itemH=:{wItemKind=IsPopUpControl} tb
				| not (identifyMaybeId id itemH.wItemId)
					= (False,itemH,tb)
				# (newPopUpPtr,editPtr,tb)
									= osCreateEmptyPopUpControl wPtr /*popUpPtr*/ (0,0) itemH.wItemShow ableContext 
																(toTuple popUpPos) (toTuple popUpSize) (length newItems) isEditable tb
				# maybeEditPtr		= if isEditable (Just editPtr) Nothing
				# tb				= osCreatePopUpControlItems newPopUpPtr maybeEditPtr ableContext (map fst newItems) newIndex tb
				# tb				= osDestroyPopUpControl popUpPtr maybeEditPtr tb
				  newPopUpInfo		= {	popUpInfoItems = newItems
									  ,	popUpInfoIndex = newIndex
									  ,	popUpInfoEdit  = if isEditable (Just {curEditInfo & popUpEditPtr=editPtr}) Nothing
									  }
				  itemH				= {itemH & wItemInfo=PopUpInfo newPopUpInfo,wItemPtr=newPopUpPtr}
				| not hasTip
					= (True,itemH,tb)
				| otherwise
					= (True,itemH,osAddControlToolTip wPtr newPopUpPtr (getControlTipAtt tipAtt) tb)
			where
				(hasTip,tipAtt)		= cselect isControlTip undef itemH.wItemAtts
				isEditable			= contains isControlKeyboard itemH.wItemAtts
				ableContext			= itemH.wItemSelect
				popUpPtr			= itemH.wItemPtr
				popUpSize			= itemH.wItemSize
				popUpPos			= itemH.wItemPos
				popUpInfo			= getWItemPopUpInfo itemH.wItemInfo
				curEditInfo			= fromJust popUpInfo.popUpInfoEdit
				curIndex			= popUpInfo.popUpInfoIndex
				curItems			= popUpInfo.popUpInfoItems
				newItems			= map snd (filter (\(i,_)->not (isMember i indexs)) (zip2 [1..] curItems))
				nrNewItems			= length newItems
				newIndex			= if (isMember curIndex indexs) 1 (min nrNewItems curIndex)
				
			closeWItemPopUpItems wPtr indexs id itemH=:{wItemId,wItems} tb
				| identifyMaybeId id wItemId
					= (True,itemH,tb)
				| otherwise
					# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id wItems tb
					= (done,{itemH & wItems=itemHs},tb)
		
		closeWElementPopUpItems wPtr indexs id (WListLSHandle itemHs) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WListLSHandle itemHs,tb)
		
		closeWElementPopUpItems wPtr indexs id (WExtendLSHandle wExH=:{wExtendItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WExtendLSHandle {wExH & wExtendItems=itemHs},tb)
		
		closeWElementPopUpItems wPtr indexs id (WChangeLSHandle wChH=:{wChangeItems=itemHs}) tb
			# (done,itemHs,tb)	= closeWElementsPopUpItems wPtr indexs id itemHs tb
			= (done,WChangeLSHandle {wChH & wChangeItems=itemHs},tb)


/*	The record MetricsInfo and the functions shiftControls` and setcompoundsliderthumb are used by
	movecontrolviewframe and setcontrolviewdomain.
*/
::	MetricsInfo
	=	{	miOSMetrics		:: !OSWindowMetrics
		,	miHMargins		:: !(!Int,!Int)
		,	miVMargins		:: !(!Int,!Int)
		,	miItemSpaces	:: !(!Int,!Int)
		,	miOrientation	:: ![(ViewDomain,Origin)]
		}

shiftControls` :: !Vector2 ![WElementHandle`] -> [WElementHandle`]
shiftControls` v itemHs = map (shiftControl` v) itemHs
where
	shiftControl` :: !Vector2 !WElementHandle` -> WElementHandle`
	shiftControl` v (WItemHandle` itemH=:{wItemPos`,wItems`})
		= WItemHandle` {itemH & wItemPos`=v+wItemPos`}	// PA: do not descend recursively!, wItems`=shiftControls` v wItems`}
	shiftControl` v (WRecursiveHandle` itemHs kind)
		= WRecursiveHandle` (shiftControls` v itemHs) kind

/*
setsliderthumb :: !Bool OSWindowMetrics OSWindowPtr Bool (Int,Int,Int) Int (Int,Int) !*OSToolbox -> *OSToolbox
setsliderthumb hasScroll wMetrics itemPtr isHScroll scrollValues viewSize maxcoords tb
	| not hasScroll	= tb
	| otherwise		= OSsetCompoundSlider wMetrics itemPtr isHScroll (toOSscrollbarRange scrollValues viewSize) maxcoords tb

//	# tb			= osSetCompoundSliderThumbSize wMetrics compoundPtr isHorizontal size (rright,rbottom) (old==new) tb
	# tb			= osSetCompoundSliderThumbSize compoundPtr scrollPtr scrollMin scrollMax scrollSize scrollRect able (old==new) tb
*/

setcompoundsliderthumb :: !Bool Bool OSWindowMetrics OSWindowPtr OSWindowPtr OSWindowPtr Bool (Int,Int,Int) Int (Int,Int) !(OSRect,OSRect) !*OSToolbox -> *OSToolbox
setcompoundsliderthumb hasScroll able wMetrics windPtr compPtr itemPtr isHScroll scrollValues viewSize maxcoords (hRect,vRect) tb
	| not hasScroll
		= tb
//	| otherwise			= osSetCompoundSlider wMetrics itemPtr isHScroll (toOSscrollbarRange scrollValues viewSize) maxcoords tb
	| otherwise	
		# tb			= osSetCompoundSliderThumbSize wMetrics windPtr compPtr itemPtr min max wid scrollRect isHScroll able False tb
	//	# tb			= appGrafport windPtr (updateCompoundScroll windPtr itemPtr scrollRect) tb		PA: line turned into function of oswindow, osUpdateCompoundScroll
		# tb			= osUpdateCompoundScroll windPtr itemPtr scrollRect tb
		# tb			= osSetCompoundSliderThumb wMetrics windPtr compPtr itemPtr scrollRect isHScroll pos /*(42,42)*/maxcoords True tb
		= tb
where
	(min,pos,max,wid)	= toOSscrollbarRange scrollValues viewSize
	scrollRect
		| isHScroll		= hRect
		| otherwise		= vRect

makeMetricsInfo :: OSWindowMetrics WindowKind Size [WindowAttribute`] WindowInfo -> .MetricsInfo
makeMetricsInfo wMetrics=:{osmHorMargin,osmVerMargin,osmHorItemSpace,osmVerItemSpace} whKind` whSize` whAtts` whWindowInfo`
	# windowInfo = getWindowInfoWindowData whWindowInfo`
	# (domainRect,origin,defHMargin,defVMargin)
							= if (whKind`==IsDialog)
								(sizeToRect whSize`,zero,osmHorMargin,osmVerMargin)
								(windowInfo.windowDomain,windowInfo.windowOrigin,0,0)
	# hMargins = getwindowhmargin` (snd (cselect iswindowhmargin` (WindowHMargin` defHMargin defHMargin) whAtts`))
	# vMargins = getwindowvmargin` (snd (cselect iswindowvmargin` (WindowVMargin` defVMargin defVMargin) whAtts`))
	# orientation				= [(rectToRectangle domainRect,origin)]
	# spaces = getwindowitemspace` (snd (cselect iswindowitemspace` (WindowItemSpace` osmHorItemSpace osmVerItemSpace) whAtts`))
	= {miOSMetrics=wMetrics,miHMargins=hMargins,miVMargins=vMargins,miItemSpaces=spaces,miOrientation=orientation}

/*	Move the ViewFrame of a CompoundControl. (In future version also customised controls.)
*/
movecontrolviewframe :: !Id !Vector2 !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
movecontrolviewframe id v wMetrics wids=:{wPtr} wH=:{whKind`,whItems`,whSize`,whAtts`,whSelect`,whDefaultId`,whWindowInfo`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	# metricsInfo			= makeMetricsInfo wMetrics whKind` whSize` whAtts` whWindowInfo`
	# clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	# (_,itemHs,(updRgn,tb))= setWElement (moveWItemFrame metricsInfo wPtr whDefaultId` False True whSelect` clipRect zero zero v)
								id whItems` (Nothing,tb)
	  wH					= {wH & whItems`=itemHs}
	| isNothing updRgn
		= (wH,tb)
	# updRgn				= fromJust updRgn
	# (empty,tb)			= osisemptyrgn updRgn tb
	| empty
		= (wH,osdisposergn updRgn tb)
	| otherwise
		= updatewindowbackgrounds` wMetrics updRgn wids wH tb
where
	moveWItemFrame :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !OSRect !Point2 !Vector2 !Vector2 !Id
	                         !WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
	               -> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	moveWItemFrame metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} 
	               wPtr defaultId withinCompound shownContext ableContext clipRect parentPos compoundPos v id
				   itemH=:{wItemId`,wItemKind`} updRgn_tb
		| not (isRecursiveControl wItemKind`)
			= (identifyMaybeId id wItemId`,itemH,updRgn_tb)
		| wItemKind`==IsLayoutControl
			| identifyMaybeId id wItemId`
				= (True,itemH,updRgn_tb)
			// otherwise
				# metricsInfo`	= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`}
				  clipRect1		= intersectRects clipRect (posSizeToRect absolutePos itemSize)
				# (done,itemHs,updRgn_tb)
								= setWElement (moveWItemFrame metricsInfo` wPtr defaultId withinCompound shownContext1 ableContext1 clipRect1 absolutePos compoundPos v) 
								              id itemH.wItems` updRgn_tb
				= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(domain,oldOrigin):miOrientation]
			  clipRect1			= intersectRects contentRect clipRect
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,updRgn_tb)
								= setWElement (moveWItemFrame metricsInfo` wPtr defaultId True shownContext1 ableContext1 clipRect1 absolutePos compoundPos1 v) id itemH.wItems` updRgn_tb
			= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| newOrigin==oldOrigin
			= (True,itemH,updRgn_tb)
		# (updRgn,tb)			= updRgn_tb
		# tb					= setcompoundsliderthumb (hasHScroll && newOrigin.x<>oldOrigin.x) ableContext1 miOSMetrics wPtr itemPtr hPtr True
												 (minx,newOrigin.x,maxx) viewx (toTuple itemSize) (hRect,vRect) tb
		# tb					= setcompoundsliderthumb (hasVScroll && newOrigin.y<>oldOrigin.y) ableContext1 miOSMetrics wPtr itemPtr vPtr False
												 (miny,newOrigin.y,maxy) viewy (toTuple itemSize) (hRect,vRect) tb
		  info					= {info & compoundOrigin=newOrigin}
		  clipRect1				= intersectRects contentRect clipRect
		| isEmpty itemH.wItems`
			# itemH				= {itemH & wItemInfo`=CompoundInfo` info}
			# (itemH,tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr parentPos clipRect1 itemH tb
			= (True,itemH,(updRgn,tb))
		| otherwise
			# oldItems`			= itemH.wItems`
			  orientation`		= [(domain,newOrigin):miOrientation]
			# (_,newItems`,tb)	= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
			  newItems`			= shiftControls` itemPos newItems`
			  itemH				= {itemH & wItems`=newItems`,wItemInfo`=CompoundInfo` info}
			# tb				= case updRgn of
									Just rgn -> osdisposergn rgn tb
									nothing  -> tb
			# (itemH, tb)		= forceValidCompoundClipState` miOSMetrics True wPtr parentPos defaultId shownContext itemH tb
			# (updRgn,tb)		= relayoutControls` miOSMetrics itemPtr defaultId True ableContext1 shownContext1 (contentRect,absolutePos,compoundPos,oldItems`) 
								                                                                                  (contentRect,absolutePos,compoundPos,itemH.wItems`) tb
			# (itemH, tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr parentPos clipRect1 itemH tb
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		domainRect				= info.compoundDomain
		domain					= rectToRectangle domainRect
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemAtts				= itemH.wItemAtts`
		absolutePos				= movePoint itemPos parentPos
		compoundPos1			= compoundPos + itemPos
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hPtr	| hasHScroll	= (fromJust info.compoundHScroll).scrollItemPtr
								= OSNoWindowPtr
		vPtr	| hasVScroll	= (fromJust info.compoundVScroll).scrollItemPtr
								= OSNoWindowPtr
		visScrolls				= osScrollbarsAreVisible miOSMetrics domainRect (toTuple itemSize) (hasHScroll,hasVScroll)
		contentRect				= osGetCompoundContentRect miOSMetrics visScrolls (posSizeToRect absolutePos itemSize)
		hRect					= osGetCompoundHScrollRect miOSMetrics visScrolls (posSizeToRect absolutePos itemSize)
		vRect					= osGetCompoundVScrollRect miOSMetrics visScrolls (posSizeToRect absolutePos itemSize)
		contentSize				= rectSize contentRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (cselect iscontrolhmargin`   (ControlHMargin` (fst miHMargins) (snd miHMargins)) itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (cselect iscontrolvmargin`   (ControlVMargin` (fst miVMargins) (snd miVMargins)) itemAtts))
		spaces`					= getcontrolitemspace` (snd (cselect iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))
		(minx,maxx,viewx)		= (domainRect.rleft,domainRect.rright, contentSize.w)
		(miny,maxy,viewy)		= (domainRect.rtop, domainRect.rbottom,contentSize.h)
		newOrigin				= {x=setBetweenCheckBounds (oldOrigin.x+v.vx) minx (maxx-viewx),y=setBetweenCheckBounds (oldOrigin.y+v.vy) miny (maxy-viewy)}


/*	Set the ViewDomain of a CompoundControl. (In future versions also customised controls.)
*/
setcontrolviewdomain :: !Id !ViewDomain !OSWindowMetrics !WIDS !WindowHandle`!*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolviewdomain id newDomain wMetrics wids=:{wPtr} wH=:{whKind`,whItems`,whSize`,whAtts`,whSelect`,whDefaultId`,whWindowInfo`} tb
	| whKind`==IsGameWindow
		= (wH,tb)
	# metricsInfo			= makeMetricsInfo wMetrics whKind` whSize` whAtts` whWindowInfo`
	# clipRect				= getContentRect wMetrics whWindowInfo` whSize`
	# (_,itemHs,(updRgn,tb))= setWElement (setWItemDomain metricsInfo wPtr whDefaultId` False True whSelect` clipRect zero zero (validateViewDomain newDomain))
								id whItems` (Nothing,tb)
	  wH					= {wH & whItems`=itemHs}
	| isNothing updRgn
		= (wH,tb)
	# updRgn				= fromJust updRgn
	# (empty,tb)			= osisemptyrgn updRgn tb
	| empty
		= (wH,osdisposergn updRgn tb)
	| otherwise
		= updatewindowbackgrounds` wMetrics updRgn wids wH tb
where	
	setWItemDomain :: !MetricsInfo !OSWindowPtr !(Maybe Id) !Bool !Bool !Bool !OSRect !Point2 !Vector2 !ViewDomain !Id !WItemHandle` !(!Maybe OSRgnHandle,!*OSToolbox)
	                                                                                                         -> (!Bool,!WItemHandle`,!(!Maybe OSRgnHandle,!*OSToolbox))
	
	setWItemDomain metricsInfo=:{miOSMetrics,miHMargins,miVMargins,miItemSpaces,miOrientation} 
	               wPtr defaultId withinCompound shownContext ableContext clipRect parentPos compoundPos newDomain id
				   itemH=:{wItemId`,wItemKind`} updRgn_tb=:(updRgn,tb)
		| not (isRecursiveControl wItemKind`)
			= (identifyMaybeId id wItemId`,itemH,updRgn_tb)
		| wItemKind`==IsLayoutControl
			| identifyMaybeId id wItemId`
				= (True,itemH,updRgn_tb)
			// otherwise
				# metricsInfo`	= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`}
				  clipRect1		= intersectRects clipRect (posSizeToRect absolutePos itemSize)
				# (done,itemHs,updRgn_tb)
								= setWElement (setWItemDomain metricsInfo` wPtr defaultId withinCompound shownContext1 ableContext1 clipRect1 absolutePos compoundPos newDomain)
										id itemH.wItems` updRgn_tb
				= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| not (identifyMaybeId id itemH.wItemId`)
			# orientation`		= [(oldDomain,oldOrigin):miOrientation]
			  clipRect1			= intersectRects oldContentRect clipRect
			  metricsInfo`		= {metricsInfo & miHMargins=hMargins`,miVMargins=vMargins`,miItemSpaces=spaces`,miOrientation=orientation`}
			# (done,itemHs,updRgn_tb)
								= setWElement (setWItemDomain metricsInfo` wPtr defaultId True shownContext1 ableContext1 clipRect1 absolutePos compoundPos1 newDomain)
									    id itemH.wItems` updRgn_tb
			= (done,{itemH & wItems`=itemHs},updRgn_tb)
		| newDomain==oldDomain
			= (True,itemH,updRgn_tb)
		# (updRgn,tb)			= updRgn_tb
		# (minx,maxx,viewx)		= (newDomainRect.rleft,newDomainRect.rright, newContentSize.w)
		  (miny,maxy,viewy)		= (newDomainRect.rtop, newDomainRect.rbottom,newContentSize.h)
		  newOrigin				= {x=setBetween oldOrigin.x minx (max minx (maxx-viewx)),y=setBetween oldOrigin.y miny (max miny (maxy-viewy))}
		  info					= {info & compoundOrigin=newOrigin,compoundDomain=newDomainRect}
		  compoundInfo`			= CompoundInfo` info
		# tb					= setcompoundsliderthumb hasHScroll ableContext1 miOSMetrics wPtr itemPtr hPtr True  (minx,newOrigin.x,maxx) viewx itemSize` (hRect,vRect) tb
		# tb					= setcompoundsliderthumb hasVScroll ableContext1 miOSMetrics wPtr itemPtr vPtr False (miny,newOrigin.y,maxy) viewy itemSize` (hRect,vRect) tb
		#! no_update_necessary	=  newOrigin==oldOrigin
								&& intersectRects (addVector (toVector absolutePos) newDomainRect) clipRect ==
								   intersectRects (addVector (toVector absolutePos) oldDomainRect) clipRect
								&& newContentSize == rectSize oldContentRect;
		# (update_necessary,tb) = if no_update_necessary
									(osWindowHasUpdateRect wPtr tb)
									(True,tb);
		| not update_necessary
			= (True,{itemH & wItemInfo`= compoundInfo`},(updRgn,tb))
		# oldItems`				= itemH.wItems`
		| isEmpty oldItems`		// CompoundControl has no controls
			# itemH				= {itemH & wItemInfo`= compoundInfo`}
			| shownContext1
				# (itemH,tb)	= drawCompoundLook` miOSMetrics ableContext1 wPtr parentPos (intersectRects newContentRect clipRect) itemH tb
				= (True,itemH,(updRgn,tb))
			// otherwise
				= (True,itemH,(updRgn,tb))
		// CompoundControl has controls
		# orientation`			= [(newDomain,newOrigin):miOrientation]
		# (_,newItems`,tb)		= layoutControls` miOSMetrics hMargins` vMargins` spaces` itemSize itemSize orientation` oldItems` tb
		  newItems`				= shiftControls` itemPos newItems`
		  itemH					= {itemH & wItems`=newItems`,wItemInfo`= compoundInfo`}
		# tb					= case updRgn of
									Just rgn -> osdisposergn rgn tb
									nothing  -> tb
		# (itemH, tb)			= forceValidCompoundClipState` miOSMetrics True wPtr parentPos defaultId shownContext itemH tb
		# itemH					= {itemH & wItemAtts` = replaceControlSizeAtt newContentSize itemH.wItemAtts`}		// PA: update required because controllayout.icl relies on info
		# (updRgn,tb)			= relayoutControls` miOSMetrics itemPtr defaultId True ableContext1 shownContext1 (newContentRect,absolutePos,compoundPos,oldItems`) 
								                                                                                  (newContentRect,absolutePos,compoundPos,itemH.wItems`) tb
		| shownContext1
			# (itemH,tb)		= drawCompoundLook` miOSMetrics ableContext1 wPtr parentPos (intersectRects newContentRect clipRect) itemH tb
			= (True,itemH,(Just updRgn,tb))
		| otherwise
			= (True,itemH,(Just updRgn,tb))
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		oldOrigin				= info.compoundOrigin
		oldDomainRect			= info.compoundDomain
		oldDomain				= rectToRectangle oldDomainRect
		newDomainRect			= rectangleToRect newDomain
		absolutePos				= movePoint itemPos parentPos
		compoundPos1			= compoundPos + itemPos
		itemPtr					= itemH.wItemPtr`
		itemPos					= itemH.wItemPos`
		itemSize				= itemH.wItemSize`
		itemSize`				= toTuple itemSize
		itemAtts				= itemH.wItemAtts`
		itemRect				= posSizeToRect absolutePos itemSize
		(hasHScroll,hasVScroll)	= (isJust info.compoundHScroll,isJust info.compoundVScroll)
		hPtr	| hasHScroll	= (fromJust info.compoundHScroll).scrollItemPtr
								= OSNoWindowPtr
		vPtr	| hasVScroll	= (fromJust info.compoundVScroll).scrollItemPtr
								= OSNoWindowPtr
		oldVisScrolls			= osScrollbarsAreVisible miOSMetrics oldDomainRect itemSize` (hasHScroll,hasVScroll)
		newVisScrolls			= osScrollbarsAreVisible miOSMetrics newDomainRect itemSize` (hasHScroll,hasVScroll)
		oldContentRect			= osGetCompoundContentRect miOSMetrics oldVisScrolls itemRect
		newContentRect			= osGetCompoundContentRect miOSMetrics newVisScrolls itemRect
		hRect					= osGetCompoundHScrollRect miOSMetrics newVisScrolls itemRect
		vRect					= osGetCompoundVScrollRect miOSMetrics newVisScrolls itemRect
		newContentSize			= rectSize newContentRect
		shownContext1			= if shownContext itemH.wItemShow` shownContext
		ableContext1			= ableContext && itemH.wItemSelect`
		hMargins`				= getcontrolhmargin`   (snd (cselect iscontrolhmargin`   (ControlHMargin`   (fst miHMargins)   (snd miHMargins))   itemAtts))
		vMargins`				= getcontrolvmargin`   (snd (cselect iscontrolvmargin`   (ControlVMargin`   (fst miVMargins)   (snd miVMargins))   itemAtts))
		spaces`					= getcontrolitemspace` (snd (cselect iscontrolitemspace` (ControlItemSpace` (fst miItemSpaces) (snd miItemSpaces)) itemAtts))
		
		replaceControlSizeAtt :: !Size ![ControlAttribute`] -> [ControlAttribute`]
		replaceControlSizeAtt size atts
			= replaceOrAppend iscontrolviewsize` (ControlViewSize` size) (filter (not o iscontroloutersize`) atts)


setcontrolscrollfun	:: !Id !Direction ScrollFunction !WindowHandle` -> WindowHandle`
setcontrolscrollfun id direction scrollFun wH`=:{whItems`}
	# (_,itemHs,_)			= setWElement (setCompoundScrollFun direction scrollFun) id whItems` 0
	= {wH` & whItems`=itemHs}
where
	setCompoundScrollFun :: !Direction ScrollFunction !Id !WItemHandle` .s -> (!Bool,!WItemHandle`,.s)
	
	setCompoundScrollFun direction scrollFun id itemH=:{wItemId`,wItemKind`=IsCompoundControl} s
		| not (identifyMaybeId id wItemId`)
			# (found,itemHs,s)	= setWElement (setCompoundScrollFun direction scrollFun) id itemH.wItems` s
			= (found,{itemH & wItems`=itemHs},s)
		| direction==Horizontal && isJust hScroll
			# info				= {info & compoundHScroll=mapMaybe (setScrollFun scrollFun) hScroll}
			= (True,{itemH & wItemInfo`=CompoundInfo` info},s)
		| direction==Vertical && isJust vScroll
			# info				= {info & compoundVScroll=mapMaybe (setScrollFun scrollFun) vScroll}
			= (True,{itemH & wItemInfo`=CompoundInfo` info},s)
		| otherwise
			= (True,itemH,s)
	where
		info					= getWItemCompoundInfo` itemH.wItemInfo`
		hScroll					= info.compoundHScroll
		vScroll					= info.compoundVScroll
		
		setScrollFun :: !ScrollFunction !ScrollInfo -> ScrollInfo
		setScrollFun f scrollInfo
			= {scrollInfo & scrollFunction=f}
	
	setCompoundScrollFun direction scrollFun id itemH=:{wItemId`,wItems`} s
		| identifyMaybeId id wItemId`
			= (True,itemH,s)
		| otherwise
			# (found,itemHs,s)	= setWElement (setCompoundScrollFun direction scrollFun) id wItems` s
			= (found,{itemH & wItems`=itemHs},s)


setcontroloutersize :: !Id !Size !Bool !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontroloutersize id newCSize relayout wMetrics wids wH tb
	= resizeControl wMetrics relayout wids id newCSize wH tb

setcontrolwidth :: !Id !ControlWidth !Bool !OSWindowMetrics !WIDS !WindowHandle` !*OSToolbox -> (!WindowHandle`,!*OSToolbox)
setcontrolwidth id newWidth relayout wMetrics wids wH tb
	= abort "setcontrolwidth not yet implemented."
