implementation module StdControlAttribute


import StdControlDef


isValidButtonControlAttribute :: !(ControlAttribute .st) -> Bool
isValidButtonControlAttribute (ControlFunction     _)	= True
isValidButtonControlAttribute  ControlHide				= True
isValidButtonControlAttribute (ControlId           _)	= True
isValidButtonControlAttribute (ControlModsFunction _)	= True
isValidButtonControlAttribute (ControlOuterSize    _)	= True
isValidButtonControlAttribute (ControlPos          _)	= True
isValidButtonControlAttribute (ControlResize       _)	= True
isValidButtonControlAttribute (ControlSelectState  _)	= True
isValidButtonControlAttribute (ControlTip          _)	= True
isValidButtonControlAttribute (ControlViewSize     _)	= True
isValidButtonControlAttribute (ControlWidth        _)	= True
isValidButtonControlAttribute _							= False

isValidCheckControlAttribute :: !(ControlAttribute .st) -> Bool
isValidCheckControlAttribute  ControlHide			= True
isValidCheckControlAttribute (ControlId          _)	= True
isValidCheckControlAttribute (ControlPos         _)	= True
isValidCheckControlAttribute (ControlSelectState _)	= True
isValidCheckControlAttribute (ControlTip         _)	= True
isValidCheckControlAttribute _						= False

isValidCompoundControlAttribute :: !(ControlAttribute .st) -> Bool
isValidCompoundControlAttribute (ControlFunction     _)	= False
isValidCompoundControlAttribute (ControlModsFunction _)	= False
isValidCompoundControlAttribute _						= True

isValidCustomButtonControlAttribute :: !(ControlAttribute .st) -> Bool
isValidCustomButtonControlAttribute (ControlFunction     _)	= True
isValidCustomButtonControlAttribute  ControlHide			= True
isValidCustomButtonControlAttribute (ControlId           _)	= True
isValidCustomButtonControlAttribute (ControlMinimumSize  _)	= True
isValidCustomButtonControlAttribute (ControlModsFunction _)	= True
isValidCustomButtonControlAttribute (ControlPen          _)	= True
isValidCustomButtonControlAttribute (ControlPos          _)	= True
isValidCustomButtonControlAttribute (ControlResize       _)	= True
isValidCustomButtonControlAttribute (ControlSelectState  _)	= True
isValidCustomButtonControlAttribute (ControlTip          _)	= True
isValidCustomButtonControlAttribute _						= False

isValidCustomControlAttribute :: !(ControlAttribute .st) -> Bool
isValidCustomControlAttribute (ControlActivate     _)	= True
isValidCustomControlAttribute (ControlDeactivate   _)	= True
isValidCustomControlAttribute  ControlHide				= True
isValidCustomControlAttribute (ControlId           _)	= True
isValidCustomControlAttribute (ControlKeyboard _ _ _)	= True
isValidCustomControlAttribute (ControlMinimumSize  _)	= True
isValidCustomControlAttribute (ControlMouse    _ _ _)	= True
isValidCustomControlAttribute (ControlPen          _)	= True
isValidCustomControlAttribute (ControlPos          _)	= True
isValidCustomControlAttribute (ControlResize       _)	= True
isValidCustomControlAttribute (ControlSelectState  _)	= True
isValidCustomControlAttribute (ControlTip          _)	= True
isValidCustomControlAttribute _							= False

isValidEditControlAttribute :: !(ControlAttribute .st) -> Bool
isValidEditControlAttribute (ControlActivate    _)	= True
isValidEditControlAttribute (ControlDeactivate  _)	= True
isValidEditControlAttribute  ControlHide			= True
isValidEditControlAttribute (ControlId          _)	= True
isValidEditControlAttribute (ControlKeyboard _ _ _)	= True
isValidEditControlAttribute (ControlPos         _)	= True
isValidEditControlAttribute (ControlResize      _)	= True
isValidEditControlAttribute (ControlSelectState _)	= True
isValidEditControlAttribute (ControlTip         _)	= True
isValidEditControlAttribute _						= False

isValidLayoutControlAttribute :: !(ControlAttribute .st) -> Bool
isValidLayoutControlAttribute  ControlHide			= True
isValidLayoutControlAttribute (ControlHMargin _ _)	= True
isValidLayoutControlAttribute (ControlId      _)	= True
isValidLayoutControlAttribute (ControlItemSpace _ _)= True
isValidLayoutControlAttribute (ControlMinimumSize _)= True
isValidLayoutControlAttribute (ControlOuterSize   _)= True
isValidLayoutControlAttribute (ControlPos       _)	= True
isValidLayoutControlAttribute (ControlResize    _)	= True
isValidLayoutControlAttribute (ControlSelectState _)= True
isValidLayoutControlAttribute (ControlViewSize  _)	= True
isValidLayoutControlAttribute (ControlVMargin   _ _)= True
isValidLayoutControlAttribute _						= False

isValidPopUpControlAttribute :: !(ControlAttribute .st) -> Bool
isValidPopUpControlAttribute (ControlActivate    _)	= True
isValidPopUpControlAttribute (ControlDeactivate  _)	= True
isValidPopUpControlAttribute  ControlHide			= True
isValidPopUpControlAttribute (ControlId          _)	= True
isValidPopUpControlAttribute (ControlPos         _)	= True
isValidPopUpControlAttribute (ControlSelectState _)	= True
isValidPopUpControlAttribute (ControlTip         _)	= True
isValidPopUpControlAttribute (ControlWidth		 _)	= True
isValidPopUpControlAttribute _						= False

isValidRadioControlAttribute :: !(ControlAttribute .st) -> Bool
isValidRadioControlAttribute  ControlHide			= True
isValidRadioControlAttribute (ControlId          _)	= True
isValidRadioControlAttribute (ControlPos         _)	= True
isValidRadioControlAttribute (ControlSelectState _)	= True
isValidRadioControlAttribute (ControlTip         _)	= True
isValidRadioControlAttribute _						= False

isValidSliderControlAttribute :: !(ControlAttribute .st) -> Bool
isValidSliderControlAttribute  ControlHide			= True
isValidSliderControlAttribute (ControlId          _)= True
isValidSliderControlAttribute (ControlPos         _)= True
isValidSliderControlAttribute (ControlResize      _)= True
isValidSliderControlAttribute (ControlSelectState _)= True
isValidSliderControlAttribute (ControlTip         _)= True
isValidSliderControlAttribute _						= False

isValidTextControlAttribute :: !(ControlAttribute .st) -> Bool
isValidTextControlAttribute  ControlHide			= True
isValidTextControlAttribute (ControlId          _)	= True
isValidTextControlAttribute (ControlPos         _)	= True
isValidTextControlAttribute (ControlTip         _)	= True
isValidTextControlAttribute (ControlWidth		_)	= True
isValidTextControlAttribute _						= False


isControlActivate		:: !(ControlAttribute .st) -> Bool
isControlActivate		(ControlActivate _)		= True
isControlActivate		_						= False

isControlDeactivate		:: !(ControlAttribute .st) -> Bool
isControlDeactivate		(ControlDeactivate _)	= True
isControlDeactivate		_						= False

isControlFunction		:: !(ControlAttribute .st) -> Bool
isControlFunction		(ControlFunction _)		= True
isControlFunction		_						= False

isControlHide			:: !(ControlAttribute .st) -> Bool
isControlHide			ControlHide				= True
isControlHide			_						= False

isControlHMargin		:: !(ControlAttribute .st) -> Bool
isControlHMargin		(ControlHMargin _ _)	= True
isControlHMargin		_						= False

isControlHScroll		:: !(ControlAttribute .st) -> Bool
isControlHScroll		(ControlHScroll _)		= True
isControlHScroll		_						= False

isControlId				:: !(ControlAttribute .st) -> Bool
isControlId				(ControlId _)			= True
isControlId				_						= False

isControlItemSpace		:: !(ControlAttribute .st) -> Bool
isControlItemSpace		(ControlItemSpace _ _)	= True
isControlItemSpace		_						= False

isControlKeyboard		:: !(ControlAttribute .st) -> Bool
isControlKeyboard		(ControlKeyboard _ _ _)	= True
isControlKeyboard		_						= False

isControlLook			:: !(ControlAttribute .st) -> Bool
isControlLook			(ControlLook _ _)		= True
isControlLook			_						= False

isControlMinimumSize	:: !(ControlAttribute .st) -> Bool
isControlMinimumSize	(ControlMinimumSize _)	= True
isControlMinimumSize	_						= False

isControlModsFunction	:: !(ControlAttribute .st) -> Bool
isControlModsFunction	(ControlModsFunction _)	= True
isControlModsFunction	_						= False

isControlMouse			:: !(ControlAttribute .st) -> Bool
isControlMouse			(ControlMouse _ _ _)	= True
isControlMouse			_						= False

isControlOrigin			:: !(ControlAttribute .st) -> Bool
isControlOrigin			(ControlOrigin _)		= True
isControlOrigin			_						= False

isControlOuterSize		:: !(ControlAttribute .st) -> Bool
isControlOuterSize		(ControlOuterSize _)	= True
isControlOuterSize		_						= False

isControlPen			:: !(ControlAttribute .st) -> Bool
isControlPen			(ControlPen _)			= True
isControlPen			_						= False

isControlPos			:: !(ControlAttribute .st) -> Bool
isControlPos			(ControlPos _)			= True
isControlPos			_						= False

isControlResize			:: !(ControlAttribute .st) -> Bool
isControlResize			(ControlResize _)		= True
isControlResize			_						= False

isControlSelectState	:: !(ControlAttribute .st) -> Bool
isControlSelectState	(ControlSelectState _)	= True
isControlSelectState	_						= False

isControlTip			:: !(ControlAttribute .st) -> Bool
isControlTip			(ControlTip _)			= True
isControlTip			_						= False

isControlViewDomain		:: !(ControlAttribute .st) -> Bool
isControlViewDomain		(ControlViewDomain _)	= True
isControlViewDomain		_						= False

isControlViewSize		:: !(ControlAttribute .st) -> Bool
isControlViewSize		(ControlViewSize _)		= True
isControlViewSize		_						= False

isControlVMargin		:: !(ControlAttribute .st) -> Bool
isControlVMargin		(ControlVMargin _ _)	= True
isControlVMargin		_						= False

isControlVScroll		:: !(ControlAttribute .st) -> Bool
isControlVScroll		(ControlVScroll _)		= True
isControlVScroll		_						= False

isControlWidth			:: !(ControlAttribute .st) -> Bool
isControlWidth			(ControlWidth _)		= True
isControlWidth			_						= False


getControlActivateFun :: !(ControlAttribute .st) -> IdFun .st
getControlActivateFun (ControlActivate f) = f

getControlDeactivateFun :: !(ControlAttribute .st) -> IdFun .st
getControlDeactivateFun (ControlDeactivate f) = f

getControlFun :: !(ControlAttribute .st) -> IdFun .st
getControlFun (ControlFunction f) = f

getControlHMarginAtt :: !(ControlAttribute .st) -> (Int,Int)
getControlHMarginAtt (ControlHMargin left right) = (left,right)

getControlHScrollFun :: !(ControlAttribute .st) -> ScrollFunction
getControlHScrollFun (ControlHScroll f) = f

getControlIdAtt :: !(ControlAttribute .st) -> Id
getControlIdAtt (ControlId id) = id

getControlItemSpaceAtt :: !(ControlAttribute .st) -> (Int,Int)
getControlItemSpaceAtt (ControlItemSpace hspace vspace) = (hspace,vspace)

getControlKeyboardAtt :: !(ControlAttribute .st) -> (KeyboardStateFilter,SelectState,KeyboardFunction .st)
getControlKeyboardAtt (ControlKeyboard filter s f) = (filter,s,f)

getControlLookAtt :: !(ControlAttribute .st) -> (Bool,Look)
getControlLookAtt (ControlLook systemLook f) = (systemLook,f)

getControlMinimumSizeAtt :: !(ControlAttribute .st) -> Size
getControlMinimumSizeAtt (ControlMinimumSize size) = size

getControlModsFun :: !(ControlAttribute .st) -> ModifiersFunction .st
getControlModsFun (ControlModsFunction f) = f

getControlMouseAtt :: !(ControlAttribute .st) -> (MouseStateFilter,SelectState,MouseFunction .st)
getControlMouseAtt (ControlMouse filter s f) = (filter,s,f)

getControlOriginAtt :: !(ControlAttribute .st) -> Point2
getControlOriginAtt (ControlOrigin p) = p

getControlOuterSizeAtt :: !(ControlAttribute .st) -> Size
getControlOuterSizeAtt (ControlOuterSize size) = size

getControlPenAtt :: !(ControlAttribute .st) -> [PenAttribute]
getControlPenAtt (ControlPen atts) = atts

getControlPosAtt :: !(ControlAttribute .st) -> ItemPos
getControlPosAtt (ControlPos itemPos) = itemPos

getControlResizeFun :: !(ControlAttribute .st) -> ControlResizeFunction
getControlResizeFun (ControlResize f) = f

getControlSelectStateAtt :: !(ControlAttribute .st) -> SelectState
getControlSelectStateAtt (ControlSelectState s) = s

getControlTipAtt :: !(ControlAttribute .st) -> String
getControlTipAtt (ControlTip tip) = tip

getControlViewDomainAtt :: !(ControlAttribute .st) -> ViewDomain
getControlViewDomainAtt (ControlViewDomain pd) = pd

getControlViewSizeAtt :: !(ControlAttribute .st) -> Size
getControlViewSizeAtt (ControlViewSize size) = size

getControlVMarginAtt :: !(ControlAttribute .st) -> (Int,Int)
getControlVMarginAtt (ControlVMargin top bottom) = (top,bottom)

getControlVScrollFun :: !(ControlAttribute .st) -> ScrollFunction
getControlVScrollFun (ControlVScroll f) = f

getControlWidthAtt :: !(ControlAttribute .st) -> ControlWidth
getControlWidthAtt (ControlWidth w) = w
