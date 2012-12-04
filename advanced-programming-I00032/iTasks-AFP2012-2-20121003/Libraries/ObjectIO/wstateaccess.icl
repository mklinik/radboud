implementation module wstateaccess

import	StdBool, StdInt, StdList, StdTuple
import	commondef, wstate, keyfocus
import	ossystem


//	Higher order access functions on [WElementHandle`]

setWElements :: (WItemHandle` *([arg],.s) -> (WItemHandle`,*([arg],.s))) ![WElementHandle`] !*(![arg],!.s)
																	-> *(![WElementHandle`],!*(![arg],!.s))
setWElements f itemHs (args,s)
	| isEmpty args || isEmpty itemHs
		= (itemHs,(args,s))
	| otherwise
		# (itemH,itemHs)	= hdtl itemHs
		# (itemH, args_s)	= setWElements` f itemH  (args,s)
		# (itemHs,args_s)	= setWElements  f itemHs args_s
		= ([itemH:itemHs],args_s)
where
	setWElements` :: (WItemHandle` *([arg],.s) -> (WItemHandle`,*([arg],.s))) !WElementHandle` !*(![arg],!.s)
																		  -> *(!WElementHandle`,!*(![arg],!.s))
	setWElements` f (WItemHandle` itemH) args_s
		# (itemH,args_s)	= f itemH args_s
		= (WItemHandle` itemH,args_s)
	setWElements` f (WRecursiveHandle` itemHs dRecKind) args_s
		# (itemHs,args_s)	= setWElements f itemHs args_s
		= (WRecursiveHandle` itemHs dRecKind,args_s)

setAllWElements :: (WItemHandle` .s -> *(WItemHandle`,.s)) ![WElementHandle`] !.s
													  -> *(![WElementHandle`],!.s)
setAllWElements f [itemH:itemHs] s
	# (itemH, s)	= setWElement     f itemH  s
	# (itemHs,s)	= setAllWElements f itemHs s
	= ([itemH:itemHs],s)
where
	setWElement :: (WItemHandle` .s -> *(WItemHandle`,.s)) !WElementHandle` !.s -> *(!WElementHandle`,!.s)
	setWElement f (WItemHandle` itemH) s
		# (itemH,s)		= f itemH s
		= (WItemHandle` itemH,s)
	setWElement f (WRecursiveHandle` itemHs wRecKind) s
		# (itemHs,s)	= setAllWElements f itemHs s
		= (WRecursiveHandle` itemHs wRecKind,s)
setAllWElements _ _ s
	= ([],s)

setWElement :: (Id WItemHandle` .s -> *(Bool,WItemHandle`,.s)) !Id ![WElementHandle`] !.s -> *(!Bool,![WElementHandle`],!.s)
setWElement f id itemHs s
	| isEmpty itemHs
		= (False,itemHs,s)
	# (itemH,itemHs)		= hdtl itemHs
	# (done,itemH,s)		= setWElement` f id itemH  s
	| done
		= (done,[itemH:itemHs],s)
	| otherwise
		# (done,itemHs,s)	= setWElement  f id itemHs s
		= (done,[itemH:itemHs],s)
where
	setWElement` :: (Id WItemHandle` .s -> *(Bool,WItemHandle`,.s)) !Id !WElementHandle` !.s -> *(!Bool,!WElementHandle`,!.s)
	setWElement` f id (WItemHandle` itemH) s
		# (done,itemH,s)	= f id itemH s
		= (done,WItemHandle` itemH,s)
	setWElement` f id (WRecursiveHandle` itemHs dRecKind) s
		# (done,itemHs,s)	= setWElement f id itemHs s
		= (done,WRecursiveHandle` itemHs dRecKind,s)

setWItemHandle :: (WItemHandle` .s -> *(Bool,WItemHandle`,.s)) ![WElementHandle`] !.s -> *(!Bool,![WElementHandle`],!.s)
setWItemHandle f itemHs s
	| isEmpty itemHs
		= (False,itemHs,s)
	# (itemH,itemHs)		= hdtl itemHs
	# (done,itemH,s)		= setWItemHandle` f itemH  s
	| done
		= (done,[itemH:itemHs],s)
	| otherwise
		# (done,itemHs,s)	= setWItemHandle  f itemHs s
		= (done,[itemH:itemHs],s)
where
	setWItemHandle` :: (WItemHandle` .s -> *(Bool,WItemHandle`,.s)) !WElementHandle` !.s -> *(!Bool,!WElementHandle`,!.s)
	setWItemHandle` f (WItemHandle` itemH) s
		# (done,itemH,s)	= f itemH s
		= (done,WItemHandle` itemH,s)
	setWItemHandle` f (WRecursiveHandle` itemHs dRecKind) s
		# (done,itemHs,s)	= setWItemHandle f itemHs s
		= (done,WRecursiveHandle` itemHs dRecKind,s)


//	Determine the list of window items that can obtain the keyboard input focus.
/*
getWElementKeyFocusIds` :: !Bool ![WElementHandle`] -> [FocusItem]
getWElementKeyFocusIds` shownContext [itemH:itemHs]
	= getWElementKeyFocusIds`` shownContext itemH++getWElementKeyFocusIds` shownContext itemHs
where
	getWElementKeyFocusIds`` :: !Bool !WElementHandle` -> [FocusItem]
	getWElementKeyFocusIds`` shownContext (WItemHandle` {wItemNr`,wItemKind`,wItemShow`,wItemAtts`,wItems`,wItemInfo`})
		| wItemKind`==IsEditControl	= focus
		| wItemKind`==IsPopUpControl && hasKeyAtt	//isEditable
									= focus
		| keySensitive && hasKeyAtt	= focus
		| otherwise					= getWElementKeyFocusIds` (shownContext && wItemShow`) wItems`
	where
		focus						= [{focusNr=wItemNr`,focusShow=shownContext}]
		hasKeyAtt					= contains iscontrolkeyboard` wItemAtts`
		keySensitive				= wItemKind`==IsCustomControl
//		isEditable					= isJust (getWItemPopUpInfo` wItemInfo`).popUpInfoEdit`
	getWElementKeyFocusIds`` shownContext (WRecursiveHandle` itemHs _)
		= getWElementKeyFocusIds` shownContext itemHs
getWElementKeyFocusIds` _ _
	= []
*/

instance == WRecursiveKind where
	(==) IsWListLSHandle	wRecKind	= case wRecKind of
											IsWListLSHandle		-> True
											_					-> False
	(==) IsWExtendLSHandle	wRecKind	= case wRecKind of
											IsWExtendLSHandle	-> True
											_					-> False
	(==) IsWChangeLSHandle	wRecKind	= case wRecKind of
											IsWChangeLSHandle	-> True
											_					-> False


//	Access to the additional WItemInfo` field of a WItemHandle` (partial functions!).

getWItemRadioInfo` :: !WItemInfo` -> RadioInfo`
getWItemRadioInfo` (RadioInfo` info) = info

getWItemCheckInfo` :: !WItemInfo` -> CheckInfo`
getWItemCheckInfo` (CheckInfo` info) = info

getWItemPopUpInfo` :: !WItemInfo` -> PopUpInfo`
getWItemPopUpInfo` (PopUpInfo` info) = info

getWItemSliderInfo` :: !WItemInfo` -> SliderInfo`
getWItemSliderInfo` (SliderInfo` info) = info

getWItemTextInfo` :: !WItemInfo` -> TextInfo
getWItemTextInfo` (TextInfo` info) = info

getWItemEditInfo` :: !WItemInfo` -> EditInfo
getWItemEditInfo` (EditInfo` info) = info

getWItemButtonInfo` :: !WItemInfo` -> ButtonInfo
getWItemButtonInfo` (ButtonInfo` info) = info

getWItemCustomButtonInfo` :: !WItemInfo` -> CustomButtonInfo
getWItemCustomButtonInfo` (CustomButtonInfo` info) = info

getWItemCustomInfo` :: !WItemInfo` -> CustomInfo
getWItemCustomInfo` (CustomInfo` info) = info

getWItemCompoundInfo` :: !WItemInfo` -> CompoundInfo
getWItemCompoundInfo` (CompoundInfo` info) = info


//	General access rules for (Window/Control)Attribute`:

iswindowitemspace` :: !WindowAttribute` -> Bool
iswindowitemspace` (WindowItemSpace` _ _)			= True
iswindowitemspace` _								= False

iswindowhmargin` :: !WindowAttribute` -> Bool
iswindowhmargin` (WindowHMargin` _ _)				= True
iswindowhmargin` _									= False

iswindowvmargin` :: !WindowAttribute` -> Bool
iswindowvmargin` (WindowVMargin` _ _)				= True
iswindowvmargin` _									= False

getwindowhmargin` :: !WindowAttribute` -> (Int,Int)
getwindowhmargin` (WindowHMargin` l r)				= (l,r)

getwindowvmargin` :: !WindowAttribute` -> (Int,Int)
getwindowvmargin` (WindowVMargin` t b)				= (t,b)

getwindowitemspace` :: !WindowAttribute` -> (Int,Int)
getwindowitemspace` (WindowItemSpace` hspace vspace)= (hspace,vspace)


iscontrolid` :: !ControlAttribute` -> Bool
iscontrolid` (ControlId` _)		= True
iscontrolid` _					= False

iscontrolpos` :: !ControlAttribute` -> Bool
iscontrolpos` (ControlPos` _)	= True
iscontrolpos` _					= False

iscontrolviewsize` :: !ControlAttribute` -> Bool
iscontrolviewsize` (ControlViewSize` _)	= True
iscontrolviewsize` _					= False

iscontroloutersize` :: !ControlAttribute` -> Bool
iscontroloutersize` (ControlOuterSize` _)	= True
iscontroloutersize` _						= False

iscontrolminimumsize` :: !ControlAttribute` -> Bool
iscontrolminimumsize` (ControlMinimumSize` _)	= True
iscontrolminimumsize` _							= False

iscontrolresize` :: !ControlAttribute` -> Bool
iscontrolresize` (ControlResize` _)	= True
iscontrolresize` _					= False

iscontrolselectstate` :: !ControlAttribute` -> Bool
iscontrolselectstate` (ControlSelectState` _)	= True
iscontrolselectstate` _							= False

iscontrolkeyboard` :: !ControlAttribute` -> Bool
iscontrolkeyboard` (ControlKeyboard` _ )	= True
iscontrolkeyboard` _						= False

iscontrolitemspace` :: !ControlAttribute` -> Bool
iscontrolitemspace` (ControlItemSpace` _ _)	= True
iscontrolitemspace` _						= False

iscontrolhmargin` :: !ControlAttribute` -> Bool
iscontrolhmargin` (ControlHMargin` _ _)	= True
iscontrolhmargin` _						= False

iscontrolvmargin` :: !ControlAttribute` -> Bool
iscontrolvmargin` (ControlVMargin` _ _)	= True
iscontrolvmargin` _						= False

iscontrolhscroll` :: !ControlAttribute` -> Bool
iscontrolhscroll` (ControlHScroll` _)	= True
iscontrolhscroll` _						= False

iscontrolvscroll` :: !ControlAttribute` -> Bool
iscontrolvscroll` (ControlVScroll` _)	= True
iscontrolvscroll` _						= False


getcontrolid` :: !ControlAttribute` -> Id
getcontrolid` (ControlId` id)	= id

getcontrolpos` :: !ControlAttribute` -> ItemPos
getcontrolpos` (ControlPos` pos)	= pos

getcontrolviewsize` :: !ControlAttribute` -> Size
getcontrolviewsize` (ControlViewSize` size) = size

getcontroloutersize` :: !ControlAttribute` -> Size
getcontroloutersize` (ControlOuterSize` size) = size

getcontrolminimumsize` :: !ControlAttribute` -> Size
getcontrolminimumsize` (ControlMinimumSize` size) = size

getcontrolresize` :: !ControlAttribute` -> ControlResizeFunction
getcontrolresize` (ControlResize` f)	= f

getcontrolselectstate` :: !ControlAttribute` -> SelectState
getcontrolselectstate` (ControlSelectState` select) = select

getcontrolitemspace` :: !ControlAttribute` -> (Int,Int)
getcontrolitemspace` (ControlItemSpace` hspace vspace)	= (hspace,vspace)

getcontrolhmargin` :: !ControlAttribute` -> (Int,Int)
getcontrolhmargin` (ControlHMargin` l r)	= (l,r)

getcontrolvmargin` :: !ControlAttribute` -> (Int,Int)
getcontrolvmargin` (ControlVMargin` t b)	= (t,b)

getcontrolhscrollfunction` :: !ControlAttribute` -> ScrollFunction
getcontrolhscrollfunction` (ControlHScroll` f)	= f

getcontrolvscrollfunction` :: !ControlAttribute` -> ScrollFunction
getcontrolvscrollfunction` (ControlVScroll` f)	= f


/*	Access operations on the margins and item space attributes of the window attributes.
	getWindow((H/V)Margin/ItemSpace)s type metrics atts
		retrieves the indicated attribute if present from the attribute list. If the attribute
		could not be found, the appropriate default value is returned. 
*/
getWindowHMargins` :: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
getWindowHMargins` type wMetrics atts
	= getwindowhmargin` (snd (cselect iswindowhmargin` (WindowHMargin` defaultLeft defaultRight) atts))
where
	(defaultLeft,defaultRight)	= case type of
									IsDialog -> (wMetrics.osmHorMargin,wMetrics.osmHorMargin)
									other    -> (0,0)

getWindowVMargins` :: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
getWindowVMargins` type wMetrics atts
	= getwindowvmargin` (snd (cselect iswindowvmargin` (WindowVMargin` defaultTop defaultBottom) atts))
where
	(defaultTop,defaultBottom)	= case type of
									IsDialog -> (wMetrics.osmVerMargin,wMetrics.osmVerMargin)
									other    -> (0,0)

getWindowItemSpaces` :: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
getWindowItemSpaces` type wMetrics atts
	= getwindowitemspace` (snd (cselect iswindowitemspace` (WindowItemSpace` defaultHor defaultVer) atts))
where
	(defaultHor,defaultVer)		= case type of
									IsDialog -> (wMetrics.osmHorItemSpace,wMetrics.osmVerItemSpace)
									other    -> (0,0)
