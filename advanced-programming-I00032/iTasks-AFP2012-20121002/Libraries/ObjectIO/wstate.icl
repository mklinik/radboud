implementation module wstate


import	StdInt, StdList, StdTuple, StdFunc
import	oswindow
import	commondef, keyfocus, windowhandle

wstateFatalError :: String String -> .x
wstateFatalError rule error
	= fatalError rule "wstate" error


/*	The WindowHandle` data type.
	This type is a subtype of the WindowHandle data type. The WindowHandle` data type 
	takes the projection of those fields of the (WindowHandle ls pst) data type that 
	do not depend on the type variables {ls,pst}.
*/
::	WindowHandle`
	=	{	whMode`				:: WindowMode					// The window mode (Modal or Modeless)
		,	whKind`				:: WindowKind					// The window kind (Window or Dialog)
		,	whTitle`			:: Title						// The window title
		,	whItemNrs`			:: [Int]						// The list of free system item numbers for all controls
		,	whKeyFocus`			:: KeyFocus						// The item that has the keyboard input focus
		,	whWindowInfo`		:: WindowInfo					// Additional information about the Window (Nothing for Dialogs)
		,	whItems`			:: [WElementHandle`]			// The window controls
		,	whShow`				:: Bool							// The visibility of the window (True iff visible)
		,	whSelect`			:: Bool							// The WindowSelect==Able (by default True)
		,	whAtts`				:: [WindowAttribute`]			// The window attributes
		,	whDefaultId`		:: Maybe Id						// The Id of the optional default button
		,	whCancelId`			:: Maybe Id						// The Id of the optional cancel  button
		,	whSize`				:: Size							// The exact size of the window
		,	whClosing`			:: Bool							// Flag: the window is being closed (True)
		}
::	WElementHandle`
	=	WItemHandle`			WItemHandle`
	|	WRecursiveHandle`		[WElementHandle`] WRecursiveKind
::	WRecursiveKind
	=	IsWListLSHandle
	|	IsWExtendLSHandle
	|	IsWChangeLSHandle
::	WItemHandle`
	=	{	wItemId`			:: Maybe Id						// If the control has a (ControlId id) attribute, then Just id; Nothing
		,	wItemNr`			:: Int							// The internal nr of this control  (generated from whIds)
		,	wItemKind`			:: ControlKind					// The sort of control
		,	wItemShow`			:: Bool							// The visibility of the control (True iff visible)
		,	wItemSelect`		:: Bool							// The ControlSelectState==Able  (by default True)
		,	wItemInfo`			:: WItemInfo`					// Additional information of the control
		,	wItemAtts`			:: [ControlAttribute`]			// The control attributes
		,	wItems`				:: [WElementHandle`]			// In case of	CompoundControl	: its control elements
																//				Otherwise		: []
		,	wItemVirtual`		:: Bool							// The control is virtual (True) and should not be layn out
		,	wItemPos`			:: !Vector2						// The position of the item, relative to its parent (window/dialog/compound/layout)
		,	wItemSize`			:: Size							// The exact size of the item
		,	wItemPtr`			:: OSWindowPtr					// The ptr to the item (OSNoWindowPtr if no handle)
		,	wItemLayoutInfo`	:: LayoutInfo					// Additional information on layout
		}
::	WItemInfo`
	=	ButtonInfo`				ButtonInfo						// In case of	ButtonControl		: the button information
	|	CheckInfo`				CheckInfo`						// In case of	CheckControl		: the check items information
	|	CompoundInfo`			CompoundInfo					// In case of	CompoundControl		: the compound control information
	|	CustomButtonInfo`		CustomButtonInfo				// In case of	CustomButtonControl	: the custom button information
	|	CustomInfo`				CustomInfo						// In case of	CustomControl		: the custom information
	|	EditInfo`				EditInfo						// In case of	EditControl			: the edit text information
	|	PopUpInfo`				PopUpInfo`						// In case of	PopUpControl		: the pop up information
	|	RadioInfo`				RadioInfo`						// In case of	RadioControl		: the radio items information
	|	SliderInfo`				SliderInfo`						// In case of	SliderControl		: the slider information
	|	TextInfo`				TextInfo						// In case of	TextControl			: the text information
	|	NoWItemInfo`											// No additional information
::	RadioInfo`
	=	{	radioItems`			:: [RadioItemInfo`]				// The radio items and their exact position (initially zero)
		,	radioLayout`		:: RowsOrColumns				// The layout of the radio items
		,	radioIndex`			:: Int							// The currently selected radio item (1<=radioIndex<=length radioItems)
		}
::	RadioItemInfo`
	=	{	radioItem`			:: (String,Int)					// The text of the item
		,	radioItemPos`		:: !Vector2						// The exact position of the item
		,	radioItemSize`		:: Size							// The exact size of the item
		,	radioItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	CheckInfo`
	=	{	checkItems`			:: [CheckItemInfo`]				// The check items and their exact position (initially zero)
		,	checkLayout`		:: RowsOrColumns				// The layout of the check items
		}
::	CheckItemInfo`
	=	{	checkItem`			:: (String,Int,MarkState)		// The text and mark of the item
		,	checkItemPos`		:: !Vector2						// The exact position of the item
		,	checkItemSize`		:: Size							// The exact size of the item
		,	checkItemPtr`		:: OSWindowPtr					// The OSWindowPtr of the item
		}
::	PopUpInfo`
	=	{	popUpInfoItems`		:: [String]						// The pop up items
		,	popUpInfoIndex`		:: Index						// The currently selected pop up item (1<=popUpInfoIndex<=length popUpInfoItems)
		,	popUpInfoEdit`		:: Maybe PopUpEditInfo			// If the pop up is editable: the PopUpEditInfo, otherwise Nothing
		}
::	SliderInfo`
	=	{	sliderInfoDir`		:: Direction					// The direction of the slider
		,	sliderInfoLength`	:: Int							// The length (in pixels) of the slider
		,	sliderInfoState`	:: SliderState					// The current slider state
		}
::	WindowAttribute`
	=	WindowActivate`
	|	WindowCancel`		Id
	|	WindowClose`
	| 	WindowCursor`		CursorShape
	|	WindowDeactivate`
	|	WindowHMargin`		Int Int
	|	WindowHScroll`		ScrollFunction
	|	WindowId`			Id
	|	WindowIndex`		Int
	|	WindowInit`
	|	WindowInitActive`	Id
	|	WindowItemSpace`	Int Int
	|	WindowKeyboard`		SelectState
	|	WindowLook`			Bool Look
	|	WindowMouse`		SelectState
	|	WindowOk`			Id
	|	WindowOrigin`		Point2
	|	WindowOuterSize`	Size
	|	WindowPen`			[PenAttribute]
	|	WindowPos`			ItemPos
	|	WindowSelectState`	SelectState
	|	WindowViewDomain`	ViewDomain
	|	WindowViewSize`		Size
	|	WindowVMargin`		Int Int
	|	WindowVScroll`		ScrollFunction
::	ControlAttribute`
	=	ControlActivate`
	|	ControlDeactivate`
	|	ControlFunction`
	|	ControlHide`
	|	ControlHMargin`		Int Int
	|	ControlHScroll`		ScrollFunction
	|	ControlId`			Id
	|	ControlItemSpace`	Int Int
	|	ControlKeyboard`	SelectState
	|	ControlLook`		Bool Look
	|	ControlMinimumSize`	Size
	|	ControlModsFunction`
	|	ControlMouse`		SelectState
	|	ControlOrigin`		Point2
	|	ControlOuterSize`	Size
	|	ControlPen`			[PenAttribute]
	|	ControlPos`			ItemPos
	|	ControlResize`		ControlResizeFunction
	|	ControlSelectState`	SelectState
	|	ControlTip`			String
	|	ControlViewDomain`	ViewDomain
	|	ControlViewSize`	Size
	|	ControlVMargin`		Int Int
	|	ControlVScroll`		ScrollFunction
	|	ControlWidth`		ControlWidth


retrieveWindowHandle` :: !*(WindowStateHandle .pst) !*OSToolbox -> (!WindowHandle`,!*WindowStateHandle .pst,!*OSToolbox)
retrieveWindowHandle` wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
	# (wH`,wH,tb)	= getWindowHandle` wPtr wH tb
	= (wH`,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
retrieveWindowHandle` _ _
	= wstateFatalError "retrieveWindowHandle`" "unexpected window placeholder argument"

insertWindowHandle` :: !WindowHandle` !*(WindowStateHandle .pst) -> *WindowStateHandle .pst
insertWindowHandle` wH` wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	#! wH	= setWindowHandle` wH` wH
	= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
insertWindowHandle` _ _
	= wstateFatalError "insertWindowHandle`" "unexpected window placeholder argument"

getWindowHandle` :: !OSWindowPtr !*(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle`,!*WindowHandle .ls .pst,!*OSToolbox)
getWindowHandle` wPtr wH=:{	whMode
						  ,	whKind
						  ,	whTitle
						  ,	whItemNrs
						  ,	whKeyFocus=kf
						  ,	whWindowInfo
						  ,	whItems=items
						  ,	whShow
						  ,	whSelect
						  ,	whAtts
						  ,	whDefaultId
						  ,	whCancelId
						  ,	whSize
						  ,	whClosing
						  } tb
	#! (items`,items,tb)	= getWElementHandles` wPtr items tb
	#! (kf`,kf)				= copyKeyFocus kf
	= (	{	whMode`			= whMode
		,	whKind`			= whKind
		,	whTitle`		= whTitle
		,	whItemNrs`		= whItemNrs
		,	whKeyFocus`		= kf`
		,	whWindowInfo`	= whWindowInfo
		,	whItems`		= items`
		,	whShow`			= whShow
		,	whSelect`		= whSelect
		,	whAtts`			= map getWAtt whAtts
		,	whDefaultId`	= whDefaultId
		,	whCancelId`		= whCancelId
		,	whSize`			= whSize
		,	whClosing`		= whClosing
		}
	  ,	{wH & whKeyFocus=kf,whItems=items}
	  ,	tb
	  )
where
	getWAtt :: !(WindowAttribute .st) -> WindowAttribute`
	getWAtt (WindowActivate    _)			= WindowActivate`
	getWAtt (WindowCancel      id)			= WindowCancel`      id
	getWAtt (WindowClose       _)			= WindowClose`
	getWAtt (WindowCursor      shape)		= WindowCursor`      shape
	getWAtt (WindowDeactivate  _)			= WindowDeactivate`
	getWAtt (WindowHMargin     l r)			= WindowHMargin`     l r
	getWAtt (WindowHScroll     f)			= WindowHScroll`     f
	getWAtt (WindowId          id)			= WindowId`          id
	getWAtt (WindowIndex       index)		= WindowIndex`       index
	getWAtt (WindowInit        _)			= WindowInit`
	getWAtt (WindowInitActive  id)			= WindowInitActive`  id
	getWAtt (WindowItemSpace   h v)			= WindowItemSpace`   h v
	getWAtt (WindowKeyboard    _ select _)	= WindowKeyboard`    select
	getWAtt (WindowLook        sysLook look)= WindowLook`        sysLook look
	getWAtt (WindowMouse       _ select _)	= WindowMouse`       select
	getWAtt (WindowOk          id)			= WindowOk`          id
	getWAtt (WindowOrigin      origin)		= WindowOrigin`      origin
	getWAtt (WindowOuterSize   size)		= WindowOuterSize`   size
	getWAtt (WindowPen         pen)			= WindowPen`         pen
	getWAtt (WindowPos         pos)			= WindowPos`         pos
	getWAtt (WindowSelectState select)		= WindowSelectState` select
	getWAtt (WindowViewDomain  domain)		= WindowViewDomain`  domain
	getWAtt (WindowViewSize    size)		= WindowViewSize`    size
	getWAtt (WindowVMargin     t b)			= WindowVMargin`     t b
	getWAtt (WindowVScroll     f)			= WindowVScroll`     f

getWElementHandles` :: !OSWindowPtr !*[WElementHandle .ls .pst] !*OSToolbox -> (![WElementHandle`],!*[WElementHandle .ls .pst],!*OSToolbox)
getWElementHandles` wPtr [itemH:itemHs] tb
	#! (itemH`, itemH, tb)	= getWElementHandle`  wPtr itemH  tb
	#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
	= ([itemH`:itemHs`],[itemH:itemHs],tb)
where
	getWElementHandle` :: !OSWindowPtr !*(WElementHandle .ls .pst) !*OSToolbox -> (!WElementHandle`,!*WElementHandle .ls .pst,!*OSToolbox)
	getWElementHandle` wPtr (WItemHandle itemH) tb
		#! (itemH`,itemH,tb)	= getWItemHandle` wPtr itemH tb
		= (WItemHandle` itemH`,WItemHandle itemH,tb)
	getWElementHandle` wPtr (WListLSHandle itemHs) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWListLSHandle,WListLSHandle itemHs,tb)
	getWElementHandle` wPtr (WExtendLSHandle exH=:{wExtendItems=itemHs}) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWExtendLSHandle,WExtendLSHandle {exH & wExtendItems=itemHs},tb)
	getWElementHandle` wPtr (WChangeLSHandle chH=:{wChangeItems=itemHs}) tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr itemHs tb
		= (WRecursiveHandle` itemHs` IsWChangeLSHandle,WChangeLSHandle {chH & wChangeItems=itemHs},tb)
getWElementHandles` _ _ tb
	= ([],[],tb)

getWItemHandle` :: !OSWindowPtr !*(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle`,!*WItemHandle .ls .pst,!*OSToolbox)
getWItemHandle` wPtr itemH=:{	wItemId
							,	wItemNr
							,	wItemKind
							,	wItemShow
							,	wItemSelect
							,	wItemInfo
							,	wItemAtts
							,	wItems
							,	wItemVirtual
							,	wItemPos
							,	wItemSize
							,	wItemPtr
							,	wItemLayoutInfo
							} tb
	#! (itemHs`,itemHs,tb)	= getWElementHandles` wPtr wItems tb
	#! (info`,info,tb)		= getWItemInfo` wPtr wItemPtr wItemInfo tb
	= (	{	wItemId`		= wItemId
		,	wItemNr`		= wItemNr
		,	wItemKind`		= wItemKind
		,	wItemShow`		= wItemShow
		,	wItemSelect`	= wItemSelect
		,	wItemInfo`		= info`
		,	wItemAtts`		= map getWItemAtt` wItemAtts
		,	wItems`			= itemHs`
		,	wItemVirtual`	= wItemVirtual
		,	wItemPos`		= wItemPos
		,	wItemSize`		= wItemSize
		,	wItemPtr`		= wItemPtr
		,	wItemLayoutInfo`= wItemLayoutInfo
		}
	  ,	{itemH & wItems=itemHs,wItemInfo=info}
	  ,	tb
	  )
where
	getWItemAtt` :: !(ControlAttribute .st) -> ControlAttribute`
	getWItemAtt` (ControlActivate     _)			= ControlActivate`
	getWItemAtt` (ControlDeactivate   _)			= ControlDeactivate`
	getWItemAtt` (ControlFunction     _)			= ControlFunction`
	getWItemAtt`  ControlHide						= ControlHide`
	getWItemAtt` (ControlHMargin      l r)			= ControlHMargin`     l r
	getWItemAtt` (ControlHScroll      f)			= ControlHScroll`     f
	getWItemAtt` (ControlId           id)			= ControlId`          id
	getWItemAtt` (ControlItemSpace    h v)			= ControlItemSpace`   h v
	getWItemAtt` (ControlKeyboard     _ select _)	= ControlKeyboard`    select
	getWItemAtt` (ControlLook         sysLook look)	= ControlLook`        sysLook look
	getWItemAtt` (ControlMinimumSize  size)			= ControlMinimumSize` size
	getWItemAtt` (ControlModsFunction _)			= ControlModsFunction`
	getWItemAtt` (ControlMouse        _ select _)	= ControlMouse`       select
	getWItemAtt` (ControlOrigin       origin)		= ControlOrigin`      origin
	getWItemAtt` (ControlOuterSize    size)			= ControlOuterSize`   size
	getWItemAtt` (ControlPen          pen)			= ControlPen`         pen
	getWItemAtt` (ControlPos          pos)			= ControlPos`         pos
	getWItemAtt` (ControlResize       f)			= ControlResize`      f
	getWItemAtt` (ControlSelectState  select)		= ControlSelectState` select
	getWItemAtt` (ControlTip		  tip)			= ControlTip`		  tip
	getWItemAtt` (ControlViewDomain   domain)		= ControlViewDomain`  domain
	getWItemAtt` (ControlViewSize     size)			= ControlViewSize`    size
	getWItemAtt` (ControlVMargin      t b)			= ControlVMargin`     t b
	getWItemAtt` (ControlVScroll      f)			= ControlVScroll`     f
	getWItemAtt` (ControlWidth        width)		= ControlWidth`       width
	
	getWItemInfo` :: !OSWindowPtr !OSWindowPtr !(WItemInfo .ls .pst) !*OSToolbox -> (!WItemInfo`,!WItemInfo .ls .pst,!*OSToolbox)
	getWItemInfo` wPtr itemPtr info=:(RadioInfo {radioItems,radioLayout,radioIndex}) tb
		= (	RadioInfo` { radioItems`  = map getRadioInfo` radioItems
					   , radioLayout` = radioLayout
					   , radioIndex`  = radioIndex
					   }
		  ,	info
		  ,	tb
		  )
	where
		getRadioInfo` :: !(RadioItemInfo .st) -> RadioItemInfo`
		getRadioInfo` {radioItem=(text,width,_),radioItemPos,radioItemSize,radioItemPtr}
			= {	radioItem`     = (text,width)
			  ,	radioItemPos`  = radioItemPos
			  ,	radioItemSize` = radioItemSize
			  ,	radioItemPtr`  = radioItemPtr
			  }
	getWItemInfo` wPtr itemPtr info=:(CheckInfo {checkItems,checkLayout}) tb
		= (	CheckInfo` { checkItems`  = map getCheckInfo` checkItems
					   , checkLayout` = checkLayout
					   }
		  ,	info
		  ,	tb
		  )
	where
		getCheckInfo` :: !(CheckItemInfo .st) -> CheckItemInfo`
		getCheckInfo` {checkItem=(text,width,mark,_),checkItemPos,checkItemSize,checkItemPtr}
			= {	checkItem`     = (text,width,mark)
			  ,	checkItemPos`  = checkItemPos
			  ,	checkItemSize` = checkItemSize
			  ,	checkItemPtr`  = checkItemPtr
			  }
	getWItemInfo` wPtr itemPtr info=:(PopUpInfo {popUpInfoItems,popUpInfoIndex,popUpInfoEdit}) tb
		# (infoEdit,tb)	= getPopUpInfoEdit` popUpInfoEdit tb
		= (	PopUpInfo` { popUpInfoItems` = map fst popUpInfoItems
					   , popUpInfoIndex` = popUpInfoIndex
					   , popUpInfoEdit`  = infoEdit
					   }
		  ,	info
		  ,	tb
		  )
	where
		getPopUpInfoEdit` :: !(Maybe PopUpEditInfo) !*OSToolbox -> (!Maybe PopUpEditInfo,!*OSToolbox)
		getPopUpInfoEdit` Nothing tb
			= (Nothing,tb)
		getPopUpInfoEdit` (Just info=:{popUpEditPtr}) tb
			# (content,tb)	= osGetPopUpControlText wPtr popUpEditPtr tb
			= (Just {info & popUpEditText=content},tb)
	getWItemInfo` wPtr itemPtr info=:(SliderInfo {sliderInfoDir,sliderInfoLength,sliderInfoState}) tb
		= (	SliderInfo` { sliderInfoDir`    = sliderInfoDir
						, sliderInfoLength` = sliderInfoLength
						, sliderInfoState`  = sliderInfoState
						}
		  ,	info
		  ,	tb
		  )
	getWItemInfo` wPtr itemPtr info=:(TextInfo textInfo) tb
		= (TextInfo` textInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(EditInfo editInfo) tb
		# (content,tb)	= osGetEditControlText wPtr itemPtr tb
		#! editInfo		= {editInfo & editInfoText=content}
		= (EditInfo` editInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(ButtonInfo buttonInfo) tb
		= (ButtonInfo` buttonInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CustomButtonInfo customButtonInfo) tb
		= (CustomButtonInfo` customButtonInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CustomInfo customInfo) tb
		= (CustomInfo` customInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(CompoundInfo compoundInfo) tb
		= (CompoundInfo` compoundInfo,info,tb)
	getWItemInfo` wPtr itemPtr info=:(ReceiverInfo _) tb
		= (NoWItemInfo`,info,tb)
	getWItemInfo` wPtr itemPtr info=:NoWItemInfo tb
		= (NoWItemInfo`,info,tb)

setWindowHandle` :: !WindowHandle` !*(WindowHandle .ls .pst) -> *WindowHandle .ls .pst
setWindowHandle` wH`=:{whTitle`,whItemNrs`,whKeyFocus`,whWindowInfo`,whItems`,whShow`,whSelect`,whAtts`,whSize`,whClosing`}
				 wH =:{whItems,whAtts}
	#! itemHs	= setWElementHandles` whItems` whItems
	#! atts		= setWAtts whAtts` whAtts
	= {	wH	& whTitle		= whTitle`
			, whItemNrs		= whItemNrs`
			, whKeyFocus	= case whKeyFocus`.kfItem of		// DvA: fixing space-leak with kfEval
				(Just fItem)	-> kfEval (setNewFocusItem fItem (newFocusItems (setFocusItems whKeyFocus`.kfItems)))
				Nothing			-> kfEval (setNoFocusItem (newFocusItems (setFocusItems whKeyFocus`.kfItems)))
			, whWindowInfo	= whWindowInfo`
			, whItems		= itemHs
			, whShow		= whShow`
			, whSelect		= whSelect`
			, whAtts		= atts
			, whSize		= whSize`
			, whClosing		= whClosing`
	  }
where
	kfEval :: !*KeyFocus -> *KeyFocus
	kfEval {kfItem,kfItems}
		| length kfItems <> 0
			= {kfItem = kfItem, kfItems = kfItems}
			= {kfItem = kfItem, kfItems = []}
	
	setFocusItems :: ![FocusItem] -> *[FocusItem]
	setFocusItems [item:items] = [item:setFocusItems items]
	setFocusItems _            = []
	
	setWAtts :: ![WindowAttribute`] ![WindowAttribute .st] -> [WindowAttribute .st]
	setWAtts [att`:atts`] [att:atts]
		#! att	= setWAtt att` att
		#! atts	= setWAtts atts` atts
		= [att:atts]
	setWAtts [] []
		= []
	setWAtts _ _
		= wstateFatalError "setWindowHandle`" "incompatible number of WindowAttributes"
	
	setWAtt :: !WindowAttribute` !(WindowAttribute .st) -> WindowAttribute .st
	setWAtt  WindowActivate`					att=:(WindowActivate     _)	= att
	setWAtt (WindowCancel`      cancelId)		att=:(WindowCancel       _)	= WindowCancel      cancelId
	setWAtt  WindowClose`						att=:(WindowClose        _)	= att
	setWAtt (WindowCursor`      cursor)			att=:(WindowCursor       _)	= WindowCursor      cursor
	setWAtt  WindowDeactivate`					att=:(WindowDeactivate   _)	= att
	setWAtt (WindowHMargin`     _ _)			att=:(WindowHMargin    _ _)	= att
	setWAtt (WindowHScroll`     scroll)			att=:(WindowHScroll      _)	= WindowHScroll		scroll
	setWAtt (WindowId`          _)				att=:(WindowId           _)	= att
	setWAtt (WindowIndex`       index)			att=:(WindowIndex        _)	= WindowIndex       index
	setWAtt  WindowInit`						att=:(WindowInit         _)	= att
	setWAtt (WindowInitActive`  id)				att=:(WindowInitActive   _)	= WindowInitActive id
	setWAtt (WindowItemSpace`   _ _)			att=:(WindowItemSpace  _ _)	= att
	setWAtt (WindowKeyboard`    select)			att=:(WindowKeyboard s _ f)	= WindowKeyboard s select f
	setWAtt (WindowLook`        sysLook look)	att=:(WindowLook       _ _)	= WindowLook        sysLook look
	setWAtt (WindowMouse`       select)			att=:(WindowMouse    s _ f)	= WindowMouse    s select f
	setWAtt (WindowOk`          okId)			att=:(WindowOk           _)	= WindowOk          okId
	setWAtt (WindowOrigin`      origin)			att=:(WindowOrigin       _)	= WindowOrigin      origin
	setWAtt (WindowOuterSize`   size)			att=:(WindowOuterSize    _)	= WindowOuterSize   size
	setWAtt (WindowPen`         pen)			att=:(WindowPen          _) = WindowPen         pen
	setWAtt (WindowPos`         pos)			att=:(WindowPos          _)	= WindowPos         pos
	setWAtt (WindowSelectState`	select)			att=:(WindowSelectState  _)	= WindowSelectState select
	setWAtt (WindowViewDomain`  domain)			att=:(WindowViewDomain   _)	= WindowViewDomain  domain
	setWAtt (WindowViewSize`    size)			att=:(WindowViewSize     _)	= WindowViewSize    size
	setWAtt (WindowVMargin`     _ _)			att=:(WindowVMargin    _ _)	= att
	setWAtt (WindowVScroll`     scroll)			att=:(WindowVScroll      _)	= WindowVScroll     scroll
	setWAtt _ _
		= wstateFatalError "setWindowHandle`" "WindowAttributes do not match pairwise"
	
setWElementHandles` :: ![WElementHandle`] !*[WElementHandle .ls .pst] -> *[WElementHandle .ls .pst]
setWElementHandles` [itemH`:itemHs`] [itemH:itemHs]
	#! itemH	= setWElement`  itemH`  itemH
	#! itemHs	= setWElementHandles` itemHs` itemHs
	= [itemH:itemHs]
where
	setWElement` :: !WElementHandle` !*(WElementHandle .ls .pst) -> *WElementHandle .ls .pst
	setWElement` (WItemHandle` itemH`) (WItemHandle itemH)
		#! itemH	= setWItemHandle` itemH` itemH
		=  WItemHandle itemH
	setWElement` (WRecursiveHandle` itemHs` IsWListLSHandle) (WListLSHandle itemHs)
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WListLSHandle itemHs
	setWElement` (WRecursiveHandle` itemHs` IsWExtendLSHandle) (WExtendLSHandle exH=:{wExtendItems=itemHs})
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WExtendLSHandle {exH & wExtendItems=itemHs}
	setWElement` (WRecursiveHandle` itemHs` IsWChangeLSHandle) (WChangeLSHandle chH=:{wChangeItems=itemHs})
		#! itemHs	= setWElementHandles` itemHs` itemHs
		=  WChangeLSHandle {chH & wChangeItems=itemHs}
	setWElement` _ _
		= wstateFatalError "setWElementHandles`" "WElementHandles do not match pairwise"
setWElementHandles` [] []
	= []
setWElementHandles` _ _
	= wstateFatalError "setWElementHandles`" "incompatible number of WElementHandles"

setWItemHandle` :: !WItemHandle` !*(WItemHandle .ls .pst) -> *WItemHandle .ls .pst
setWItemHandle` itemH`=:{	wItemNr`
						,	wItemShow`
						,	wItemSelect`
						,	wItemInfo`
						,	wItemAtts`	= atts`
						,	wItems`		= itemHs`
						,	wItemVirtual`
						,	wItemPos`
						,	wItemSize`
						,	wItemLayoutInfo`
						,	wItemKind`
						}
				itemH =:{	wItemInfo	= info
						,	wItemAtts	= atts
						,	wItems		= itemHs
						}
	#! info1	= setWItemInfo` wItemInfo` info
	   atts1	= setWItemAtts` atts` atts
	   itemHs1	= setWElementHandles` itemHs` itemHs
	= {	itemH	& wItemNr			= wItemNr`
				, wItemShow			= wItemShow`
				, wItemSelect		= wItemSelect`
				, wItemInfo			= info1
				, wItemAtts			= atts1
				, wItems			= itemHs1
				, wItemVirtual		= wItemVirtual`
				, wItemPos			= wItemPos`
				, wItemSize			= wItemSize`
				, wItemLayoutInfo	= wItemLayoutInfo`
	  }
where
	setWItemAtts` :: ![ControlAttribute`] ![ControlAttribute .st] -> [ControlAttribute .st]
	setWItemAtts` [att`:atts`] [att:atts]
		#! att	= setWItemAtt`  att`  att
		#! atts	= setWItemAtts` atts` atts
		= [att:atts]
	setWItemAtts` [] []
		= []
	setWItemAtts` _ _
		= wstateFatalError "setWItemHandle`" "incompatible number of ControlAttributes"
	
	setWItemAtt` :: !ControlAttribute` !(ControlAttribute .st) -> ControlAttribute .st
	setWItemAtt`  ControlActivate`						att=:(ControlActivate     _)	= att
	setWItemAtt`  ControlDeactivate`					att=:(ControlDeactivate   _)	= att
	setWItemAtt`  ControlFunction`						att=:(ControlFunction     _)	= att
	setWItemAtt`  ControlHide`							att=: ControlHide				= att
	setWItemAtt` (ControlHMargin`      _ _)				att=:(ControlHMargin    _ _)	= att
	setWItemAtt` (ControlHScroll`      scroll)			att=:(ControlHScroll      _)	= ControlHScroll scroll
	setWItemAtt` (ControlId`           _)				att=:(ControlId           _)	= att
	setWItemAtt` (ControlItemSpace`    _ _)				att=:(ControlItemSpace  _ _)	= att
	setWItemAtt` (ControlKeyboard`     select)			att=:(ControlKeyboard s _ f)	= ControlKeyboard s select f
	setWItemAtt` (ControlLook`         sysLook look)	att=:(ControlLook       _ _)	= ControlLook sysLook look
	setWItemAtt` (ControlMinimumSize`  _)				att=:(ControlMinimumSize  _)	= att
	setWItemAtt`  ControlModsFunction`					att=:(ControlModsFunction _)	= att
	setWItemAtt` (ControlMouse`        select)			att=:(ControlMouse    s _ f)	= ControlMouse s select f
	setWItemAtt` (ControlOrigin`       origin)			att=:(ControlOrigin       _)	= ControlOrigin origin
	setWItemAtt` (ControlOuterSize`    size)			att=:(ControlOuterSize    _)	= ControlOuterSize size
	setWItemAtt` (ControlPen`          pen)				att=:(ControlPen          _)	= ControlPen pen
	setWItemAtt` (ControlPos`          pos)				att=:(ControlPos          _)	= ControlPos  pos
	setWItemAtt` (ControlResize`       f)				att=:(ControlResize       _)	= ControlResize f
	setWItemAtt` (ControlSelectState`  select)			att=:(ControlSelectState  _)	= ControlSelectState select
	setWItemAtt` (ControlTip`		   tip)				att=:(ControlTip		  _)	= ControlTip tip
	setWItemAtt` (ControlViewDomain`   domain)			att=:(ControlViewDomain   _)	= ControlViewDomain domain
	setWItemAtt` (ControlViewSize`     size)			att=:(ControlViewSize     _)	= ControlViewSize size
	setWItemAtt` (ControlVMargin`      _ _)				att=:(ControlVMargin    _ _)	= att
	setWItemAtt` (ControlVScroll`      scroll)			att=:(ControlVScroll      _)	= ControlVScroll scroll
	setWItemAtt` (ControlWidth`        width)			att=:(ControlWidth        _)	= ControlWidth width
	setWItemAtt` att` att
		= wstateFatalError "setWItemHandle`"
			("ControlAttributes of "+++toString wItemKind`+++" do not match pairwise ("+++toString att`+++" vs. "+++toString att+++")")
	
	setWItemInfo` :: !WItemInfo` !(WItemInfo .ls .pst) -> WItemInfo .ls .pst
	setWItemInfo` (RadioInfo` {radioItems`,radioIndex`}) (RadioInfo radio=:{radioItems,radioIndex})
		#! radioItems=setRadioInfos radioItems` radioItems
		= RadioInfo {radio & radioItems=radioItems,radioIndex=radioIndex`}
	where
		setRadioInfos :: ![RadioItemInfo`] ![RadioItemInfo .st] -> [RadioItemInfo .st]
		setRadioInfos [info`:infos`] [info:infos]
			#! info=setRadioInfo info` info
			#! infos=setRadioInfos infos` infos
			= [info:infos]
		where
			setRadioInfo :: !RadioItemInfo` !(RadioItemInfo .st) -> RadioItemInfo .st
			setRadioInfo {radioItem`=(item`,s`),radioItemPos`,radioItemSize`} info=:{radioItem=(_,_,f)}
				= {info & radioItem=(item`,s`,f),radioItemPos=radioItemPos`,radioItemSize=radioItemSize`}
		setRadioInfos [] []
			= []
		setRadioInfos _ _
			= wstateFatalError "setWindowHandle`" "incompatible RadioInfo"
	setWItemInfo` (CheckInfo` {checkItems`}) (CheckInfo check=:{checkItems})
		= CheckInfo {check & checkItems=setCheckInfos checkItems` checkItems}
	where
		setCheckInfos :: ![CheckItemInfo`] ![CheckItemInfo .st] -> [CheckItemInfo .st]
		setCheckInfos [info`:infos`] [info:infos]
			= [setCheckInfo info` info:setCheckInfos infos` infos]
		where
			setCheckInfo :: !CheckItemInfo` !(CheckItemInfo .st) -> CheckItemInfo .st
			setCheckInfo {checkItem`=(text`,s`,mark`),checkItemPos`,checkItemSize`} info=:{checkItem=(_,_,_,f)}
				= {info & checkItem=(text`,s`,mark`,f),checkItemPos=checkItemPos`,checkItemSize=checkItemSize`}
		setCheckInfos [] []
			= []
		setCheckInfos _ _
			= wstateFatalError "setWindowHandle`" "incompatible CheckInfo"
	setWItemInfo` (PopUpInfo` {popUpInfoItems`=texts`,popUpInfoIndex`=i,popUpInfoEdit`}) (PopUpInfo popup=:{popUpInfoItems=items})
		= PopUpInfo {popup & popUpInfoItems=setpopuptexts texts` items,popUpInfoIndex=i,popUpInfoEdit=popUpInfoEdit`}
	where
		setpopuptexts :: ![String] ![PopUpControlItem .st] -> [PopUpControlItem .st]
		setpopuptexts [text:texts] [(_,f):items]
			= [(text,f):setpopuptexts texts items]
		setpopuptexts [] []
			= []
		setpopuptexts _ _
			= wstateFatalError "setWindowHandle`" "incompatible PopUpInfo"
	setWItemInfo` (SliderInfo` {sliderInfoDir`=dir,sliderInfoLength`=length,sliderInfoState`=state}) (SliderInfo slider)
		= SliderInfo {slider & sliderInfoDir=dir,sliderInfoLength=length,sliderInfoState=state}
	setWItemInfo` (TextInfo` info) (TextInfo _)
		= TextInfo info
	setWItemInfo` (EditInfo` info) (EditInfo _)
		= EditInfo info
	setWItemInfo` (ButtonInfo` info) (ButtonInfo _)
		= ButtonInfo info
	setWItemInfo` (CustomButtonInfo` info) (CustomButtonInfo _)
		= CustomButtonInfo info
	setWItemInfo` (CustomInfo` info) (CustomInfo _)
		= CustomInfo info
	setWItemInfo` (CompoundInfo` info) (CompoundInfo _)
		= CompoundInfo info
	setWItemInfo` NoWItemInfo` info
		= info
	setWItemInfo` _ _
		= wstateFatalError "setWindowHandle`" "incompatible WItemInfo"

retrieveWindowHandle2 :: !*(WindowStateHandle .pst) !*OSToolbox -> (!WindowHandle2,!*WindowStateHandle .pst,!*OSToolbox)
retrieveWindowHandle2 wsH=:{wshIds={wPtr},wshHandle=Just wlsH=:{wlsHandle=wH}} tb
	# (wH`,wH,tb)	= getWindowHandle2 wPtr wH tb
	= (wH`,{wsH & wshHandle=Just {wlsH & wlsHandle=wH}},tb)
retrieveWindowHandle2 _ _
	= wstateFatalError "retrieveWindowHandle2" "unexpected window placeholder argument"

getWindowHandle2 :: !OSWindowPtr !*(WindowHandle .ls .pst) !*OSToolbox -> (!WindowHandle2,!*WindowHandle .ls .pst,!*OSToolbox)
getWindowHandle2 wPtr wH=:{	whWindowInfo
						  ,	whItems=items
						  ,	whSize
						  } tb
	#! (items`,items,tb)	= getWElementHandles2 wPtr items tb
	= (	{	whWindowInfo2	= whWindowInfo
		,	whItems2		= items`
		,	whSize2			= whSize
		}
	  ,	{wH & whItems=items}
	  ,	tb
	  )
where	
	getWElementHandles2 :: !OSWindowPtr !*[WElementHandle .ls .pst] !*OSToolbox -> (![WElementHandle2],!*[WElementHandle .ls .pst],!*OSToolbox)
	getWElementHandles2 wPtr [itemH:itemHs] tb
		#! (itemH`, itemH, tb)	= getWElementHandle2  wPtr itemH  tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles2 wPtr itemHs tb
		= ([itemH`:itemHs`],[itemH:itemHs],tb)
	where
		getWElementHandle2 :: !OSWindowPtr !*(WElementHandle .ls .pst) !*OSToolbox -> (!WElementHandle2,!*WElementHandle .ls .pst,!*OSToolbox)
		getWElementHandle2 wPtr (WItemHandle itemH) tb
			#! (itemH`,itemH,tb)	= getWItemHandle2 wPtr itemH tb
			= (WItemHandle2 itemH`,WItemHandle itemH,tb)
		getWElementHandle2 wPtr (WListLSHandle itemHs) tb
			#! (itemHs`,itemHs,tb)	= getWElementHandles2 wPtr itemHs tb
			= (WRecursiveHandle2 itemHs` IsWListLSHandle,WListLSHandle itemHs,tb)
		getWElementHandle2 wPtr (WExtendLSHandle exH=:{wExtendItems=itemHs}) tb
			#! (itemHs`,itemHs,tb)	= getWElementHandles2 wPtr itemHs tb
			= (WRecursiveHandle2 itemHs` IsWExtendLSHandle,WExtendLSHandle {exH & wExtendItems=itemHs},tb)
		getWElementHandle2 wPtr (WChangeLSHandle chH=:{wChangeItems=itemHs}) tb
			#! (itemHs`,itemHs,tb)	= getWElementHandles2 wPtr itemHs tb
			= (WRecursiveHandle2 itemHs` IsWChangeLSHandle,WChangeLSHandle {chH & wChangeItems=itemHs},tb)
	getWElementHandles2 _ _ tb
		= ([],[],tb)
	
	getWItemHandle2 :: !OSWindowPtr !*(WItemHandle .ls .pst) !*OSToolbox -> (!WItemHandle2,!*WItemHandle .ls .pst,!*OSToolbox)
	getWItemHandle2 wPtr itemH=:{	wItemId
								,	wItemKind
								,	wItemShow
								,	wItemInfo
								,	wItems
								,	wItemPos
								,	wItemSize
								,	wItemPtr
								} tb
		#! (itemHs`,itemHs,tb)	= getWElementHandles2 wPtr wItems tb
		#! (info`,info,tb)		= getWItemInfo` wPtr wItemPtr wItemInfo tb
		= (	{	wItemId2		= wItemId
			,	wItemKind2		= wItemKind
			,	wItemShow2		= wItemShow
			,	wItemInfo2		= info`
			,	wItems2			= itemHs`
			,	wItemPos2		= wItemPos
			,	wItemSize2		= wItemSize
			,	wItemPtr2		= wItemPtr
			}
		  ,	{itemH & wItems=itemHs,wItemInfo=info}
		  ,	tb
		  )
	where	
		getWItemInfo` :: !OSWindowPtr !OSWindowPtr !(WItemInfo .ls .pst) !*OSToolbox -> (!WItemInfo`,!WItemInfo .ls .pst,!*OSToolbox)
		getWItemInfo` wPtr itemPtr info=:(RadioInfo {radioItems,radioLayout,radioIndex}) tb
			= (	RadioInfo` { radioItems`  = map getRadioInfo` radioItems
						   , radioLayout` = radioLayout
						   , radioIndex`  = radioIndex
						   }
			  ,	info
			  ,	tb
			  )
		where
			getRadioInfo` :: !(RadioItemInfo .st) -> RadioItemInfo`
			getRadioInfo` {radioItem=(text,width,_),radioItemPos,radioItemSize,radioItemPtr}
				= {	radioItem`     = (text,width)
				  ,	radioItemPos`  = radioItemPos
				  ,	radioItemSize` = radioItemSize
				  ,	radioItemPtr`  = radioItemPtr
				  }
		getWItemInfo` wPtr itemPtr info=:(CheckInfo {checkItems,checkLayout}) tb
			= (	CheckInfo` { checkItems`  = map getCheckInfo` checkItems
						   , checkLayout` = checkLayout
						   }
			  ,	info
			  ,	tb
			  )
		where
			getCheckInfo` :: !(CheckItemInfo .st) -> CheckItemInfo`
			getCheckInfo` {checkItem=(text,width,mark,_),checkItemPos,checkItemSize,checkItemPtr}
				= {	checkItem`     = (text,width,mark)
				  ,	checkItemPos`  = checkItemPos
				  ,	checkItemSize` = checkItemSize
				  ,	checkItemPtr`  = checkItemPtr
				  }
		getWItemInfo` wPtr itemPtr info=:(PopUpInfo {popUpInfoItems,popUpInfoIndex,popUpInfoEdit}) tb
			# (infoEdit,tb)	= getPopUpInfoEdit` popUpInfoEdit tb
			= (	PopUpInfo` { popUpInfoItems` = map fst popUpInfoItems
						   , popUpInfoIndex` = popUpInfoIndex
						   , popUpInfoEdit`  = infoEdit
						   }
			  ,	info
			  ,	tb
			  )
		where
			getPopUpInfoEdit` :: !(Maybe PopUpEditInfo) !*OSToolbox -> (!Maybe PopUpEditInfo,!*OSToolbox)
			getPopUpInfoEdit` Nothing tb
				= (Nothing,tb)
			getPopUpInfoEdit` (Just info=:{popUpEditPtr}) tb
				# (content,tb)	= osGetPopUpControlText wPtr popUpEditPtr tb
				= (Just {info & popUpEditText=content},tb)
		getWItemInfo` wPtr itemPtr info=:(SliderInfo {sliderInfoDir,sliderInfoLength,sliderInfoState}) tb
			= (	SliderInfo` { sliderInfoDir`    = sliderInfoDir
							, sliderInfoLength` = sliderInfoLength
							, sliderInfoState`  = sliderInfoState
							}
			  ,	info
			  ,	tb
			  )
		getWItemInfo` wPtr itemPtr info=:(TextInfo textInfo) tb
			= (TextInfo` textInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(EditInfo editInfo) tb
//			# (content,tb)	= osGetEditControlText wPtr itemPtr tb
//			#! editInfo		= {editInfo & editInfoText=content}
			= (EditInfo` editInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(ButtonInfo buttonInfo) tb
			= (ButtonInfo` buttonInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(CustomButtonInfo customButtonInfo) tb
			= (CustomButtonInfo` customButtonInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(CustomInfo customInfo) tb
			= (CustomInfo` customInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(CompoundInfo compoundInfo) tb
			= (CompoundInfo` compoundInfo,info,tb)
		getWItemInfo` wPtr itemPtr info=:(ReceiverInfo _) tb
			= (NoWItemInfo`,info,tb)
		getWItemInfo` wPtr itemPtr info=:NoWItemInfo tb
			= (NoWItemInfo`,info,tb)

insertWindowHandle2 :: !WindowHandle2 !*(WindowStateHandle .pst) -> *WindowStateHandle .pst
insertWindowHandle2 wH` wsH=:{wshHandle=Just wlsH=:{wlsHandle=wH}}
	#! wH	= setWindowHandle2 wH` wH
	= {wsH & wshHandle=Just {wlsH & wlsHandle=wH}}
insertWindowHandle2 _ _
	= wstateFatalError "insertWindowHandle2" "unexpected window placeholder argument"

setWindowHandle2 :: !WindowHandle2 !*(WindowHandle .ls .pst) -> *WindowHandle .ls .pst
setWindowHandle2 wH`=:{whWindowInfo2,whItems2,whSize2}
				 wH =:{whItems,whAtts}
	#! itemHs	= setWElementHandles2 whItems2 whItems
	= {	wH	& whItems		= itemHs
			, whWindowInfo	= whWindowInfo2
			, whSize		= whSize2
	  }
where
	setWElementHandles2 :: ![WElementHandle2] !*[WElementHandle .ls .pst] -> *[WElementHandle .ls .pst]
	setWElementHandles2 [itemH`:itemHs`] [itemH:itemHs]
		#! itemH	= setWElement2  itemH`  itemH
		#! itemHs	= setWElementHandles2 itemHs` itemHs
		= [itemH:itemHs]
	where
		setWElement2 :: !WElementHandle2 !*(WElementHandle .ls .pst) -> *WElementHandle .ls .pst
		setWElement2 (WItemHandle2 itemH`) (WItemHandle itemH)
			#! itemH	= setWItemHandle2 itemH` itemH
			=  WItemHandle itemH
		setWElement2 (WRecursiveHandle2 itemHs` IsWListLSHandle) (WListLSHandle itemHs)
			#! itemHs	= setWElementHandles2 itemHs` itemHs
			=  WListLSHandle itemHs
		setWElement2 (WRecursiveHandle2 itemHs` IsWExtendLSHandle) (WExtendLSHandle exH=:{wExtendItems=itemHs})
			#! itemHs	= setWElementHandles2 itemHs` itemHs
			=  WExtendLSHandle {exH & wExtendItems=itemHs}
		setWElement2 (WRecursiveHandle2 itemHs` IsWChangeLSHandle) (WChangeLSHandle chH=:{wChangeItems=itemHs})
			#! itemHs	= setWElementHandles2 itemHs` itemHs
			=  WChangeLSHandle {chH & wChangeItems=itemHs}
		setWElement2 _ _
			= wstateFatalError "setWElementHandles2" "WElementHandles do not match pairwise"
	setWElementHandles2 [] []
		= []
	setWElementHandles2 _ _
		= wstateFatalError "setWElementHandles2" "incompatible number of WElementHandles"
	
	setWItemHandle2 :: !WItemHandle2 !*(WItemHandle .ls .pst) -> *WItemHandle .ls .pst
	setWItemHandle2 itemH`=:{	wItemShow2
							,	wItemInfo2
							,	wItems2		= itemHs`
							,	wItemPos2
							,	wItemSize2
							,	wItemKind2
							}
					itemH =:{	wItemInfo	= info
							,	wItemAtts	= atts
							,	wItems		= itemHs
							}
		#! info1	= setWItemInfo` wItemInfo2 info
		   itemHs1	= setWElementHandles2 itemHs` itemHs
		= {	itemH	& wItemShow			= wItemShow2
					, wItemInfo			= info1
					, wItems			= itemHs1
					, wItemPos			= wItemPos2
					, wItemSize			= wItemSize2
		  }
	where	
		setWItemInfo` :: !WItemInfo` !(WItemInfo .ls .pst) -> WItemInfo .ls .pst
		setWItemInfo` (RadioInfo` {radioItems`,radioIndex`}) (RadioInfo radio=:{radioItems,radioIndex})
			#! radioItems=setRadioInfos radioItems` radioItems
			= RadioInfo {radio & radioItems=radioItems,radioIndex=radioIndex`}
		where
			setRadioInfos :: ![RadioItemInfo`] ![RadioItemInfo .st] -> [RadioItemInfo .st]
			setRadioInfos [info`:infos`] [info:infos]
				#! info=setRadioInfo info` info
				#! infos=setRadioInfos infos` infos
				= [info:infos]
			where
				setRadioInfo :: !RadioItemInfo` !(RadioItemInfo .st) -> RadioItemInfo .st
				setRadioInfo {radioItem`=(item`,s`),radioItemPos`,radioItemSize`} info=:{radioItem=(_,_,f)}
					= {info & radioItem=(item`,s`,f),radioItemPos=radioItemPos`,radioItemSize=radioItemSize`}
			setRadioInfos [] []
				= []
			setRadioInfos _ _
				= wstateFatalError "setWindowHandle2" "incompatible RadioInfo"
		setWItemInfo` (CheckInfo` {checkItems`}) (CheckInfo check=:{checkItems})
			= CheckInfo {check & checkItems=setCheckInfos checkItems` checkItems}
		where
			setCheckInfos :: ![CheckItemInfo`] ![CheckItemInfo .st] -> [CheckItemInfo .st]
			setCheckInfos [info`:infos`] [info:infos]
				= [setCheckInfo info` info:setCheckInfos infos` infos]
			where
				setCheckInfo :: !CheckItemInfo` !(CheckItemInfo .st) -> CheckItemInfo .st
				setCheckInfo {checkItem`=(text`,s`,mark`),checkItemPos`,checkItemSize`} info=:{checkItem=(_,_,_,f)}
					= {info & checkItem=(text`,s`,mark`,f),checkItemPos=checkItemPos`,checkItemSize=checkItemSize`}
			setCheckInfos [] []
				= []
			setCheckInfos _ _
				= wstateFatalError "setWindowHandle2" "incompatible CheckInfo"
		setWItemInfo` (PopUpInfo` {popUpInfoItems`=texts`,popUpInfoIndex`=i,popUpInfoEdit`}) (PopUpInfo popup=:{popUpInfoItems=items})
			= PopUpInfo {popup & popUpInfoItems=setpopuptexts texts` items,popUpInfoIndex=i,popUpInfoEdit=popUpInfoEdit`}
		where
			setpopuptexts :: ![String] ![PopUpControlItem .st] -> [PopUpControlItem .st]
			setpopuptexts [text:texts] [(_,f):items]
				= [(text,f):setpopuptexts texts items]
			setpopuptexts [] []
				= []
			setpopuptexts _ _
				= wstateFatalError "setWindowHandle2" "incompatible PopUpInfo"
		setWItemInfo` (SliderInfo` {sliderInfoDir`=dir,sliderInfoLength`=length,sliderInfoState`=state}) (SliderInfo slider)
			= SliderInfo {slider & sliderInfoDir=dir,sliderInfoLength=length,sliderInfoState=state}
		setWItemInfo` (TextInfo` info) (TextInfo _)
			= TextInfo info
		setWItemInfo` (EditInfo` info) (EditInfo _)
			= EditInfo info
		setWItemInfo` (ButtonInfo` info) (ButtonInfo _)
			= ButtonInfo info
		setWItemInfo` (CustomButtonInfo` info) (CustomButtonInfo _)
			= CustomButtonInfo info
		setWItemInfo` (CustomInfo` info) (CustomInfo _)
			= CustomInfo info
		setWItemInfo` (CompoundInfo` info) (CompoundInfo _)
			= CompoundInfo info
		setWItemInfo` NoWItemInfo` info
			= info
		setWItemInfo` _ _
			= wstateFatalError "setWindowHandle2" "incompatible WItemInfo"

instance toString ControlAttribute` where
	toString  ControlActivate`         = "ControlActivate`"
	toString  ControlDeactivate`       = "ControlDeactivate`"
	toString  ControlFunction`		   = "ControlFunction`"
	toString  ControlHide`			   = "ControlHide`"
	toString (ControlHMargin`	  _ _) = "ControlHMargin`"
	toString (ControlHScroll`		_) = "ControlHScroll`"
	toString (ControlId`			_) = "ControlId`"
	toString (ControlItemSpace`	  _ _) = "ControlItemSpace`"
	toString (ControlKeyboard`		_) = "ControlKeyboard`"
	toString (ControlLook`		  _	_) = "ControlLook`"
	toString (ControlMinimumSize`	_) = "ControlMinimumSize`"
	toString  ControlModsFunction`	   = "ControlModsFunction`"
	toString (ControlMouse`			_) = "ControlMouse`"
	toString (ControlOrigin`		_) = "ControlOrigin`"
	toString (ControlOuterSize`     _) = "ControlOuterSize`"
	toString (ControlPen`           _) = "ControlPen`"
	toString (ControlPos`			_) = "ControlPos`"
	toString (ControlResize`		_) = "ControlResize`"
	toString (ControlSelectState`	_) = "ControlSelectState`"
	toString (ControlTip`			_) = "ControlTip`"
	toString (ControlViewDomain`	_) = "ControlViewDomain`"
	toString (ControlViewSize`		_) = "ControlViewSize`"
	toString (ControlVMargin`	  _ _) = "ControlVMargin`"
	toString (ControlVScroll`		_) = "ControlVScroll`"
	toString (ControlWidth`         _) = "ControlWidth`"
instance toString (ControlAttribute .st) where
	toString (ControlActivate		_) = "ControlActivate"
	toString (ControlDeactivate		_) = "ControlDeactivate"
	toString (ControlFunction		_) = "ControlFunction"
	toString  ControlHide			   = "ControlHide"
	toString (ControlHMargin	  _ _) = "ControlHMargin"
	toString (ControlHScroll		_) = "ControlHScroll"
	toString (ControlId				_) = "ControlId"
	toString (ControlItemSpace	  _ _) = "ControlItemSpace"
	toString (ControlKeyboard	_ _	_) = "ControlKeyboard"
	toString (ControlLook		  _	_) = "ControlLook"
	toString (ControlMinimumSize	_) = "ControlMinimumSize"
	toString (ControlModsFunction	_) = "ControlModsFunction"
	toString (ControlMouse		_ _	_) = "ControlMouse"
	toString (ControlOrigin			_) = "ControlOrigin"
	toString (ControlOuterSize		_) = "ControlOuterSize"
	toString (ControlPen			_) = "ControlPen"
	toString (ControlPos			_) = "ControlPos"
	toString (ControlResize			_) = "ControlResize"
	toString (ControlSelectState	_) = "ControlSelectState"
	toString (ControlTip			_) = "ControlTip"
	toString (ControlViewDomain		_) = "ControlViewDomain"
	toString (ControlViewSize		_) = "ControlViewSize"
	toString (ControlVMargin	  _ _) = "ControlVMargin"
	toString (ControlVScroll		_) = "ControlVScroll"
	toString (ControlWidth			_) = "ControlWidth"
