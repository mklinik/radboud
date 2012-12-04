definition module wstateaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access operations on WItemHandle`
//	********************************************************************************


from	ossystem import :: OSWindowMetrics{..}
import	wstate
//from	keyfocus	import :: FocusItem


setWElements				:: (WItemHandle` *([arg],.s) -> (WItemHandle`,*([arg],.s)))    ![WElementHandle`] !*(![arg],!.s)
																					  -> *(![WElementHandle`],!*(![arg],!.s))
setAllWElements				:: (      WItemHandle` .s -> *(      WItemHandle`,.s))        ![WElementHandle`] !.s
																			  -> *(       ![WElementHandle`],!.s)
setWElement					:: (  Id  WItemHandle` .s -> *( Bool,WItemHandle`,.s))   !Id  ![WElementHandle`] !.s
																			  -> *(!Bool, ![WElementHandle`],!.s)
setWItemHandle				:: (      WItemHandle` .s -> *( Bool,WItemHandle`,.s))        ![WElementHandle`] !.s
																			  -> *(!Bool, ![WElementHandle`],!.s)
//getWElementKeyFocusIds`		:: !Bool ![WElementHandle`] -> [FocusItem]


instance == WRecursiveKind


//	Access to the additional WItemInfo` field of a WItemHandle` (partial functions!).
getWItemRadioInfo`			:: !WItemInfo`			-> RadioInfo`
getWItemCheckInfo`			:: !WItemInfo`			-> CheckInfo`
getWItemPopUpInfo`			:: !WItemInfo`			-> PopUpInfo`
getWItemSliderInfo`			:: !WItemInfo`			-> SliderInfo`
getWItemTextInfo`			:: !WItemInfo`			-> TextInfo
getWItemEditInfo`			:: !WItemInfo`			-> EditInfo
getWItemButtonInfo`			:: !WItemInfo`			-> ButtonInfo
getWItemCustomButtonInfo`	:: !WItemInfo`			-> CustomButtonInfo
getWItemCustomInfo`			:: !WItemInfo`			-> CustomInfo
getWItemCompoundInfo`		:: !WItemInfo`			-> CompoundInfo


//	General functions on WindowAttribute`:
iswindowitemspace`			:: !WindowAttribute`	-> Bool
iswindowhmargin`			:: !WindowAttribute`	-> Bool
iswindowvmargin`			:: !WindowAttribute`	-> Bool
getwindowhmargin`			:: !WindowAttribute`	-> (Int,Int)
getwindowvmargin`			:: !WindowAttribute`	-> (Int,Int)
getwindowitemspace`			:: !WindowAttribute`	-> (Int,Int)


//	General functions on ControlAttribute`:
iscontrolid`				:: !ControlAttribute`	-> Bool
iscontrolpos`				:: !ControlAttribute`	-> Bool
iscontrolviewsize`			:: !ControlAttribute`	-> Bool
iscontroloutersize`			:: !ControlAttribute`	-> Bool
iscontrolminimumsize`		:: !ControlAttribute`	-> Bool
iscontrolresize`			:: !ControlAttribute`	-> Bool
iscontrolselectstate`		:: !ControlAttribute`	-> Bool
iscontrolkeyboard`			:: !ControlAttribute`	-> Bool
iscontrolitemspace`			:: !ControlAttribute`	-> Bool
iscontrolhmargin`			:: !ControlAttribute`	-> Bool
iscontrolvmargin`			:: !ControlAttribute`	-> Bool
iscontrolhscroll`			:: !ControlAttribute`	-> Bool
iscontrolvscroll`			:: !ControlAttribute`	-> Bool
getcontrolid`				:: !ControlAttribute`	-> Id
getcontrolpos`				:: !ControlAttribute`	-> ItemPos
getcontrolviewsize`			:: !ControlAttribute`	-> Size
getcontroloutersize`		:: !ControlAttribute`	-> Size
getcontrolminimumsize`		:: !ControlAttribute`	-> Size
getcontrolresize`			:: !ControlAttribute`	-> ControlResizeFunction
getcontrolselectstate`		:: !ControlAttribute`	-> SelectState
getcontrolitemspace`		:: !ControlAttribute`	-> (Int,Int)
getcontrolhmargin`			:: !ControlAttribute`	-> (Int,Int)
getcontrolvmargin`			:: !ControlAttribute`	-> (Int,Int)
getcontrolhscrollfunction`	:: !ControlAttribute`	-> ScrollFunction
getcontrolvscrollfunction`	:: !ControlAttribute`	-> ScrollFunction

/*	Access operations on the margins and item space attributes of the window attributes.
	getWindow((H/V)Margin/ItemSpace)s type metrics atts
		retrieves the indicated attribute if present from the attribute list. If the attribute
		could not be found, the appropriate default value is returned. 
*/
getWindowHMargins`			:: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
getWindowVMargins`			:: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
getWindowItemSpaces`		:: !WindowKind !OSWindowMetrics ![WindowAttribute`] -> (!Int,!Int)
