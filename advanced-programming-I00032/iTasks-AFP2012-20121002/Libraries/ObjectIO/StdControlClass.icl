implementation module StdControlClass


import	StdBool, StdFunc, StdList, StdMisc, StdTuple
import	commondef, iostate, StdControlAttribute, windowhandle
from	controlvalidate	import validateItemPos, validateSliderState
from	StdPSt			import class accScreenPicture(..), instance accScreenPicture IOSt
from	windowvalidate	import validateViewDomain
import	ospicture, ostypes, oswindow


stdControlClassFatalError :: String String -> .x
stdControlClassFatalError function error
	= fatalError function "StdControlClass" error


class Controls cdef where
	controlToHandles:: !.(cdef .ls (PSt .l)) !(PSt .l) -> (![ControlState .ls (PSt .l)],!PSt .l)
	getControlType	::  .(cdef .ls .pst)               -> ControlType


/*	Translating control elements with local state into the internal representation.
	Note that no additional information is generated yet.
	Attributes that can be placed in the relevant record fields 
		wItemId		- ControlId
		wItemShow	- ControlHide
		wItemSelect	- ControlSelectState
		wItemLook	- ControlLook
		wItemInfo	- ControlDomain
	are removed from the attribute list. 
	The remaining attribute list is copied to wItemAtts.
*/

instance Controls (AddLS c) | Controls c where
	controlToHandles {addLS,addDef} pState
		# (cs,pState)	= controlToHandles addDef pState
		= (	[wElementHandleToControlState
				(WExtendLSHandle {	wExtendLS		= addLS
								 ,	wExtendItems	= map controlStateToWElementHandle cs
								 }
				)
			]
		  ,	pState
		  )
	getControlType _
		= ""

instance Controls (NewLS c) | Controls c where
	controlToHandles {newLS,newDef} pState
		# (cs,pState)	= controlToHandles newDef pState
		= (	[wElementHandleToControlState
				(WChangeLSHandle {	wChangeLS		= newLS
								 ,	wChangeItems	= map controlStateToWElementHandle cs
								 }
				)
			]
		  ,	pState
		  )
	getControlType _
		= ""

instance Controls (ListLS c) | Controls c where
	controlToHandles (ListLS cDefs) pState
		# (css,pState)	= stateMap controlToHandles cDefs pState
		= ([wElementHandleToControlState (WListLSHandle (map controlStateToWElementHandle (flatten css)))],pState)
	getControlType _
		= ""

instance Controls NilLS where
	controlToHandles NilLS pState
		= ([wElementHandleToControlState (WListLSHandle [])],pState)
	getControlType _
		= ""

instance Controls ((:+:) c1 c2)	| Controls c1 & Controls c2 where
	controlToHandles (c1:+:c2) pState
		# (cs1,pState)	= controlToHandles c1 pState
		# (cs2,pState)	= controlToHandles c2 pState
		= (cs1++cs2,pState)
	getControlType _
		= ""

instance Controls ButtonControl where
	controlToHandles (ButtonControl textLine atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		# (size,ioState)			= getButtonSize wMetrics textLine /*cWidth*/sizeAtt ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsButtonControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= ButtonInfo {buttonInfoText=textLine}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
	//	cWidth						= getControlWidthAttribute atts
		sizeAtt						= case filter (isControlOuterSize orc isControlWidth orc isControlViewSize) atts of
										[att : _] = Just att
										nothing   = Nothing
		
		getButtonSize :: !OSWindowMetrics String !(Maybe (ControlAttribute *(.ls,PSt .l))) !(IOSt .l) -> (!Size,!IOSt .l)
		getButtonSize wMetrics text Nothing ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetButtonControlSize wMetrics text) ioState
			  wOK					= max (osGetButtonControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getButtonSize wMetrics text (Just (ControlWidth w)) ioState
			= getButtonWidth wMetrics text w ioState
		where
			getButtonWidth :: !OSWindowMetrics String !ControlWidth !(IOSt .l) -> (!Size,!IOSt .l)
			getButtonWidth wMetrics _ (PixelWidth reqW) ioState
				# wOK				= max (osGetButtonControlMinWidth wMetrics) reqW
				# hOK				= osGetButtonControlHeight wMetrics
				= ({w=wOK,h=hOK},ioState)
			getButtonWidth wMetrics _ (TextWidth wtext) ioState
				# (w,ioState)		= getDialogFontTextWidth wtext ioState
				  wOK				= max (osGetButtonControlMinWidth wMetrics) w
				  hOK				= osGetButtonControlHeight wMetrics
				= ({w=wOK,h=hOK},ioState)
			getButtonWidth wMetrics _ (ContentWidth wtext) ioState
				# ((w,hOK),ioState)	= accIOToolbox (osGetButtonControlSize wMetrics wtext) ioState
				  wOK				= max (osGetButtonControlMinWidth wMetrics) w
				= ({w=wOK,h=hOK},ioState)
		getButtonSize wMetrics text (Just size) ioState
			= ({w=wOK,h=hOK},ioState)
		where
			wOK						= max 0 reqW
			hOK						= max 0 reqH
			{w=reqW,h=reqH}			= case size of
										ControlOuterSize size -> size
										ControlViewSize  size -> size
										otherAttribute        -> stdControlClassFatalError "controlToHandles (ButtonControl)" "unexpected ControlAttribute"
	getControlType _
		= "ButtonControl"

instance Controls CheckControl where
	controlToHandles (CheckControl items layout atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		  (nrItems,items)			= ulength items
		# (infoItems,ioState)		= stateMap (checkItemToInfo wMetrics) items ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCheckControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CheckInfo
										{	checkItems = infoItems
										,	checkLayout= validateLayout nrItems layout
										}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		checkItemToInfo :: !OSWindowMetrics !(CheckControlItem *(.ls,PSt .l)) !(IOSt .l)
											-> (!CheckItemInfo *(.ls,PSt .l), ! IOSt .l)
		checkItemToInfo wMetrics (text,Just (PixelWidth reqW),mark,f) ioState
			# wOK				= max (osGetCheckControlItemMinWidth wMetrics) reqW
			# hOK				= osGetCheckControlItemHeight wMetrics
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Just (TextWidth wtext),mark,f) ioState
			# (w,ioState)		= getDialogFontTextWidth wtext ioState
			  wOK				= max (osGetCheckControlItemMinWidth wMetrics) w
			# hOK				= osGetCheckControlItemHeight wMetrics
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Just (ContentWidth wtext),mark,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetCheckControlItemSize wMetrics wtext) ioState
			  wOK				= max (osGetCheckControlItemMinWidth wMetrics) w
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
		checkItemToInfo wMetrics (text,Nothing,mark,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetCheckControlItemSize wMetrics text) ioState
			  wOK				= max (osGetCheckControlItemMinWidth wMetrics) w
			= ({checkItem=(text,wOK,mark,f),checkItemSize={w=wOK,h=hOK},checkItemPos=zero,checkItemPtr=OSNoWindowPtr},ioState)
	getControlType _
		= "CheckControl"

instance Controls (CompoundControl c)	| Controls c where
	controlToHandles (CompoundControl controls atts) pState
		# (cs,pState)	= controlToHandles controls pState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCompoundControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CompoundInfo
										{	compoundDomain	= rectangleToRect domain
										,	compoundOrigin	= origin
										,	compoundHScroll	= if hasHScroll (Just hScrollInfo) Nothing
										,	compoundVScroll	= if hasVScroll (Just vScrollInfo) Nothing
										,	compoundLookInfo= {compoundLook={	lookFun			= lookFun
																			,	lookPen			= pen
																			,	lookSysUpdate	= sysLook
																			}
															  ,compoundClip={clipRgn=0,clipOk=False}
															  }
										}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= map controlStateToWElementHandle cs
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	where
		(hasHScroll,hScrollAtt)		= cselect isControlHScroll undef atts
		(hasVScroll,vScrollAtt)		= cselect isControlVScroll undef atts
		(_,lookAtt)					= cselect isControlLook (ControlLook True stdUnfillUpdAreaLook) atts
		(sysLook,lookFun)			= getControlLookAtt lookAtt
		defaultDomain				= ControlViewDomain {viewDomainRange & corner1=zero}
		(_,domainAtt)				= cselect isControlViewDomain defaultDomain atts
		domain						= validateViewDomain (getControlViewDomainAtt domainAtt)
		(_,originAtt)				= cselect isControlOrigin (ControlOrigin domain.corner1) atts
		origin						= validateOrigin domain (getControlOriginAtt originAtt)
		hScrollInfo					= {	scrollFunction	= getControlHScrollFun hScrollAtt
									  ,	scrollItemPos	= zero
									  ,	scrollItemSize	= zero
									  ,	scrollItemPtr	= OSNoWindowPtr
									  }
		vScrollInfo					= {	scrollFunction	= getControlVScrollFun vScrollAtt
									  ,	scrollItemPos	= zero
									  ,	scrollItemSize	= zero
									  ,	scrollItemPtr	= OSNoWindowPtr
									  }
		pen							= getInitialPen atts
	getControlType _
		= "CompoundControl"

instance Controls CustomButtonControl where
	controlToHandles (CustomButtonControl {w,h} controlLook atts) pState
		# size	= {w=max 0 w,h=max 0 h}
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCustomButtonControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CustomButtonInfo {cButtonInfoLook={lookFun=controlLook,lookPen=getInitialPen atts,lookSysUpdate=True}}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	getControlType _
		= "CustomButtonControl"

instance Controls CustomControl where
	controlToHandles (CustomControl {w,h} controlLook atts) pState
		# size	= {w=max 0 w,h=max 0 h}
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsCustomControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= CustomInfo {customInfoLook={lookFun=controlLook,lookPen=getInitialPen atts,lookSysUpdate=True}}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	getControlType _
		= "CustomControl"

instance Controls EditControl where
	controlToHandles (EditControl textLine cWidth nrLines atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		# (size,ioState)			= getEditSize wMetrics nrLines cWidth ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsEditControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= EditInfo
										{	editInfoText	= textLine
										,	editInfoWidth	= size.w			// PA: this field might have become redundant
										,	editInfoNrLines	= nrLines
										}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		getEditSize :: !OSWindowMetrics Int !ControlWidth !(IOSt .l) -> (!Size,!IOSt .l)
		getEditSize wMetrics nrLines (PixelWidth reqW) ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetEditControlSize wMetrics reqW nrLines) ioState
			# wOK					= max (osGetEditControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getEditSize wMetrics nrLines (TextWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetEditControlSize wMetrics w nrLines) ioState
			  wOK					= max (osGetEditControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getEditSize wMetrics nrLines (ContentWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth (wtext+++"mm") ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetEditControlSize wMetrics w nrLines) ioState
			  wOK					= max (osGetEditControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "EditControl"

instance Controls (LayoutControl c)	| Controls c where
	controlToHandles (LayoutControl controls atts) pState
		# (cs,pState)	= controlToHandles controls pState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsLayoutControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= NoWItemInfo
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= map controlStateToWElementHandle cs
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	pState
		  )
	getControlType _
		= "LayoutControl"

instance Controls PopUpControl where
	controlToHandles (PopUpControl popUpItems index atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		# (size,ioState)			= getPopUpSize wMetrics (map fst popUpItems) cWidth ioState
		# nrItems					= length popUpItems
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsPopUpControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= PopUpInfo 
										{	popUpInfoItems = popUpItems
										,	popUpInfoIndex = validatePopUpIndex nrItems index
										,	popUpInfoEdit  = Nothing
										}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		cWidth						= getControlWidthAttribute atts
		
		getPopUpSize :: !OSWindowMetrics [String] !(Maybe ControlWidth) !(IOSt .l) -> (!Size,!IOSt .l)
		getPopUpSize wMetrics _ (Just (PixelWidth reqW)) ioState
			# wOK					= max (osGetPopUpControlMinWidth wMetrics) reqW
			  hOK					= osGetPopUpControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics _ (Just (TextWidth wtext)) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  wOK					= max (osGetPopUpControlMinWidth wMetrics) w
			  hOK					= osGetPopUpControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics _ (Just (ContentWidth wtext)) ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetPopUpControlSize wMetrics [wtext]) ioState
			  wOK					= max (osGetPopUpControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getPopUpSize wMetrics itemtexts Nothing ioState
			# ((w,hOK),ioState)		= accIOToolbox (osGetPopUpControlSize wMetrics (if (isEmpty itemtexts) ["MMMMMMMMMM"] itemtexts)) ioState
			  wOK					= max (osGetPopUpControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "PopUpControl"

instance Controls RadioControl where
	controlToHandles (RadioControl items layout index atts) pState
		# (wMetrics, ioState)		= ioStGetOSWindowMetrics pState.io
		  (nrItems,items)			= ulength items
		# (infoItems,ioState)		= stateMap (radioItemToInfo wMetrics) items ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsRadioControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= RadioInfo 
										{	radioItems = infoItems
										,	radioLayout= validateLayout nrItems layout
										,	radioIndex = setBetween index 1 nrItems
										}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= zero
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		radioItemToInfo :: !OSWindowMetrics !(RadioControlItem *(.ls,PSt .l)) !(IOSt .l)
											-> (!RadioItemInfo *(.ls,PSt .l), ! IOSt .l)
		radioItemToInfo wMetrics (text,Just (PixelWidth reqW),f) ioState
			# wOK				= max (osGetRadioControlItemMinWidth wMetrics) reqW
			# hOK				= osGetRadioControlItemHeight wMetrics
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Just (TextWidth wtext),f) ioState
			# (w,ioState)		= getDialogFontTextWidth wtext ioState
			  wOK				= max (osGetRadioControlItemMinWidth wMetrics) w
			# hOK				= osGetRadioControlItemHeight wMetrics
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Just (ContentWidth wtext),f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetRadioControlItemSize wMetrics wtext) ioState
			  wOK				= max (osGetRadioControlItemMinWidth wMetrics) w
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
		radioItemToInfo wMetrics (text,Nothing,f) ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetRadioControlItemSize wMetrics text) ioState
			  wOK				= max (osGetRadioControlItemMinWidth wMetrics) w
			= ({radioItem=(text,wOK,f),radioItemSize={w=wOK,h=hOK},radioItemPos=zero,radioItemPtr=OSNoWindowPtr},ioState)
	getControlType _
		= "RadioControl"

instance Controls SliderControl where
	controlToHandles (SliderControl direction cWidth sliderState action atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		# (size,ioState)			= getSliderSize wMetrics isHorizontal cWidth ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsSliderControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= SliderInfo 
				 						{	sliderInfoDir	= direction
				 						,	sliderInfoLength= if isHorizontal size.w size.h		// PA: maybe this field is now redundant
			 							,	sliderInfoState	= validateSliderState sliderState
			 							,	sliderInfoAction= action
			 							}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		isHorizontal				= direction == Horizontal
		
		getSliderSize :: !OSWindowMetrics !Bool !ControlWidth !(IOSt .l) -> (!Size,!IOSt .l)
		getSliderSize wMetrics isHorizontal (PixelWidth reqW) ioState
			# (wOK,hOK)				= osGetSliderControlSize wMetrics isHorizontal reqW
			= ({w=wOK,h=hOK},ioState)
		getSliderSize wMetrics isHorizontal (TextWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  (wOK,hOK)				= osGetSliderControlSize wMetrics isHorizontal w
			= ({w=wOK,h=hOK},ioState)
		getSliderSize wMetrics isHorizontal (ContentWidth wtext) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  (wOK,hOK)				= osGetSliderControlSize wMetrics isHorizontal w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "SliderControl"

instance Controls TextControl where
	controlToHandles (TextControl textLine atts) pState
		# (wMetrics,ioState)		= ioStGetOSWindowMetrics pState.io
		# (size,ioState)			= getTextSize wMetrics textLine cWidth ioState
		= (	[wElementHandleToControlState
				(WItemHandle 
				{	wItemId			= getIdAttribute atts
				,	wItemNr			= 0
				,	wItemKind		= IsTextControl
				,	wItemShow		= not (contains isControlHide atts)
				,	wItemSelect		= getSelectStateAttribute atts
				,	wItemInfo		= TextInfo {textInfoText=textLine}
				,	wItemAtts		= map validateControlPos (filter (not o redundantAttribute) atts)
				,	wItems			= []
				,	wItemVirtual	= False
				,	wItemPos		= zero
				,	wItemSize		= size
				,	wItemPtr		= OSNoWindowPtr
				,	wItemLayoutInfo	= undef
				})
			]
		  ,	{pState & io=ioState}
		  )
	where
		cWidth						= getControlWidthAttribute atts
		
		getTextSize :: !OSWindowMetrics !String !(Maybe ControlWidth) !(IOSt .l) -> (!Size,!IOSt .l)
		getTextSize wMetrics _ (Just (PixelWidth reqW)) ioState
			# wOK					= max (osGetTextControlMinWidth wMetrics) reqW
			  hOK					= osGetTextControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics _ (Just (TextWidth wtext)) ioState
			# (w,ioState)			= getDialogFontTextWidth wtext ioState
			  wOK					= max (osGetTextControlMinWidth wMetrics) w
			  hOK					= osGetTextControlHeight wMetrics
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics _ (Just (ContentWidth wtext)) ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetTextControlSize wMetrics wtext) ioState
			  wOK				= max (osGetTextControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
		getTextSize wMetrics text Nothing ioState
			# ((w,hOK),ioState)	= accIOToolbox (osGetTextControlSize wMetrics text) ioState
			  wOK				= max (osGetTextControlMinWidth wMetrics) w
			= ({w=wOK,h=hOK},ioState)
	getControlType _
		= "TextControl"


//	Additional functions:

getDialogFontTextWidth :: !String !*env -> (!Int,!*env) | accScreenPicture env
getDialogFontTextWidth s env
	= accScreenPicture getTextWidth env
where
	getTextWidth :: !*Picture -> (!Int,!*Picture)
	getTextWidth picture
		# (dialogFont,picture)	= openDialogFont picture
		= getFontStringWidth dialogFont s picture

getIdAttribute :: ![ControlAttribute .st] -> Maybe Id
getIdAttribute atts
	| hasId			= Just (getControlIdAtt idAtt)
	| otherwise		= Nothing
where
	(hasId,idAtt)	= cselect isControlId undef atts

getControlWidthAttribute :: ![ControlAttribute .st] -> Maybe ControlWidth
getControlWidthAttribute atts
	| hasControlWidth			= Just (getControlWidthAtt widthAtt)
	| otherwise					= Nothing
where
	(hasControlWidth,widthAtt)	= cselect isControlWidth undef atts

getSelectStateAttribute :: ![ControlAttribute .st] -> Bool
getSelectStateAttribute atts
	= enabled (getControlSelectStateAtt (snd (cselect isControlSelectState (ControlSelectState Able) atts)))

redundantAttribute :: !(ControlAttribute .st) -> Bool
redundantAttribute (ControlId _)			= True
redundantAttribute ControlHide				= True
redundantAttribute (ControlSelectState _)	= True
redundantAttribute (ControlLook _ _)		= True
redundantAttribute (ControlViewDomain _)	= True
redundantAttribute _						= False

getInitialPen :: ![ControlAttribute .st] -> Pen
getInitialPen atts
	| hasPenAtts
		= stateMap2 setPenAttribute (reverse (getControlPenAtt penAttsAtt)) defaultPen
	| otherwise
		= defaultPen
where
	(hasPenAtts,penAttsAtt)	= cselect isControlPen undef atts

validateLayout :: !Int !RowsOrColumns -> RowsOrColumns
validateLayout nrItems (Rows    n) = Rows    (setBetween n 1 nrItems)
validateLayout nrItems (Columns n) = Columns (setBetween n 1 nrItems)

validatePopUpIndex :: !Int !Index -> Index
validatePopUpIndex nrItems index
	| isBetween index 1 nrItems	= index
	| otherwise					= 1

validateOrigin :: !ViewDomain !Point2 -> Point2
validateOrigin domain origin
	= {	x=setBetween origin.x domain.corner1.x domain.corner2.x
	  ,	y=setBetween origin.y domain.corner1.y domain.corner2.y
	  }

validateControlPos :: !(ControlAttribute .st) -> ControlAttribute .st
validateControlPos (ControlPos itemPos) = ControlPos (validateItemPos itemPos)
validateControlPos att                  = att
