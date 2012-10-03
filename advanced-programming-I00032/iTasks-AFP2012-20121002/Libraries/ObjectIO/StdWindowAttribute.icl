implementation module StdWindowAttribute


import StdBool
import StdWindowDef


isValidWindowAttribute :: !(WindowAttribute .st) -> Bool
isValidWindowAttribute att = isAllWindowsAttribute att || isWindowOnlyAttribute att

isValidDialogAttribute :: !(WindowAttribute .st) -> Bool
isValidDialogAttribute att = isAllWindowsAttribute att || isDialogOnlyAttribute att

// DvA: following also exported by windowdefaccess?! PA: Nee.
isAllWindowsAttribute :: !(WindowAttribute .st) -> Bool
isAllWindowsAttribute (WindowActivate	_)		= True
isAllWindowsAttribute (WindowClose		_)		= True
isAllWindowsAttribute (WindowDeactivate	_)		= True
isAllWindowsAttribute (WindowHMargin   _ _)		= True
isAllWindowsAttribute (WindowId			_)		= True
isAllWindowsAttribute (WindowIndex		_)		= True
isAllWindowsAttribute (WindowInit		_)		= True
isAllWindowsAttribute (WindowInitActive _)		= True
isAllWindowsAttribute (WindowItemSpace _ _)		= True
isAllWindowsAttribute (WindowOuterSize	_)		= True
isAllWindowsAttribute (WindowPos		_)		= True
isAllWindowsAttribute (WindowViewSize	_)		= True
isAllWindowsAttribute (WindowVMargin   _ _)		= True
isAllWindowsAttribute _							= False

isWindowOnlyAttribute :: !(WindowAttribute .st) -> Bool
isWindowOnlyAttribute (WindowCursor		_)		= True
isWindowOnlyAttribute (WindowHScroll	_)		= True
isWindowOnlyAttribute (WindowKeyboard	_ _ _)	= True
isWindowOnlyAttribute (WindowLook		_ _)	= True
isWindowOnlyAttribute (WindowMouse		_ _ _)	= True
isWindowOnlyAttribute (WindowOrigin		_)		= True
isWindowOnlyAttribute (WindowPen        _)		= True
isWindowOnlyAttribute (WindowSelectState _)		= True
isWindowOnlyAttribute (WindowViewDomain	_)		= True
isWindowOnlyAttribute (WindowVScroll	_)		= True
isWindowOnlyAttribute _							= False

isDialogOnlyAttribute :: !(WindowAttribute .st) -> Bool
isDialogOnlyAttribute (WindowCancel		_)		= True
isDialogOnlyAttribute (WindowOk			_)		= True
isDialogOnlyAttribute _							= False


isWindowActivate	:: !(WindowAttribute .st) -> Bool
isWindowActivate	(WindowActivate _)		= True
isWindowActivate	_						= False

isWindowCancel		:: !(WindowAttribute .st) -> Bool
isWindowCancel		(WindowCancel _)		= True
isWindowCancel		_						= False

isWindowClose		:: !(WindowAttribute .st) -> Bool
isWindowClose		(WindowClose _)			= True
isWindowClose		_						= False

isWindowCursor		:: !(WindowAttribute .st) -> Bool
isWindowCursor		(WindowCursor _)		= True
isWindowCursor		_						= False

isWindowDeactivate	:: !(WindowAttribute .st) -> Bool
isWindowDeactivate	(WindowDeactivate _)	= True
isWindowDeactivate	_						= False

isWindowHMargin		:: !(WindowAttribute .st) -> Bool
isWindowHMargin		(WindowHMargin _ _)		= True
isWindowHMargin		_						= False

isWindowHScroll		:: !(WindowAttribute .st) -> Bool
isWindowHScroll		(WindowHScroll _)		= True
isWindowHScroll		_						= False

isWindowId			:: !(WindowAttribute .st) -> Bool
isWindowId			(WindowId _)			= True
isWindowId			_						= False

isWindowIndex		:: !(WindowAttribute .st) -> Bool
isWindowIndex		(WindowIndex _)			= True
isWindowIndex		_						= False

isWindowInit		:: !(WindowAttribute .st) -> Bool
isWindowInit		(WindowInit _)			= True
isWindowInit		_						= False

isWindowInitActive	:: !(WindowAttribute .st) -> Bool
isWindowInitActive	(WindowInitActive _)	= True
isWindowInitActive	_						= False

isWindowItemSpace	:: !(WindowAttribute .st) -> Bool
isWindowItemSpace	(WindowItemSpace _ _)	= True
isWindowItemSpace	_						= False

isWindowKeyboard	:: !(WindowAttribute .st) -> Bool
isWindowKeyboard	(WindowKeyboard _ _ _)	= True
isWindowKeyboard	_						= False

isWindowLook		:: !(WindowAttribute .st) -> Bool
isWindowLook		(WindowLook _ _)		= True
isWindowLook		_						= False

isWindowMouse		:: !(WindowAttribute .st) -> Bool
isWindowMouse		(WindowMouse _ _ _)		= True
isWindowMouse		_						= False

isWindowOk			:: !(WindowAttribute .st) -> Bool
isWindowOk			(WindowOk _)			= True
isWindowOk			_						= False

isWindowOrigin		:: !(WindowAttribute .st) -> Bool
isWindowOrigin		(WindowOrigin _)		= True
isWindowOrigin		_						= False

isWindowOuterSize	:: !(WindowAttribute .st) -> Bool
isWindowOuterSize	(WindowOuterSize _)		= True
isWindowOuterSize	_						= False

isWindowPen			:: !(WindowAttribute .st) -> Bool
isWindowPen			(WindowPen _)			= True
isWindowPen			_						= False

isWindowPos			:: !(WindowAttribute .st) -> Bool
isWindowPos			(WindowPos _)			= True
isWindowPos			_						= False

isWindowSelectState	:: !(WindowAttribute .st) -> Bool
isWindowSelectState	(WindowSelectState _)	= True
isWindowSelectState	_						= False

isWindowViewDomain	:: !(WindowAttribute .st) -> Bool
isWindowViewDomain	(WindowViewDomain _)	= True
isWindowViewDomain	_						= False

isWindowViewSize	:: !(WindowAttribute .st) -> Bool
isWindowViewSize	(WindowViewSize _)		= True
isWindowViewSize	_						= False

isWindowVMargin		:: !(WindowAttribute .st) -> Bool
isWindowVMargin		(WindowVMargin _ _)		= True
isWindowVMargin		_						= False

isWindowVScroll		:: !(WindowAttribute .st) -> Bool
isWindowVScroll		(WindowVScroll _)		= True
isWindowVScroll		_						= False


getWindowActivateFun :: !(WindowAttribute .st) -> IdFun .st
getWindowActivateFun (WindowActivate f) = f

getWindowCancelAtt :: !(WindowAttribute .st) -> Id
getWindowCancelAtt (WindowCancel id) = id

getWindowCloseFun :: !(WindowAttribute .st) -> IdFun .st
getWindowCloseFun (WindowClose f) = f

getWindowCursorAtt :: !(WindowAttribute .st) -> CursorShape
getWindowCursorAtt (WindowCursor cShape) = cShape

getWindowDeactivateFun :: !(WindowAttribute .st) -> IdFun .st
getWindowDeactivateFun (WindowDeactivate f) = f

getWindowHMarginAtt :: !(WindowAttribute .st) -> (Int,Int)
getWindowHMarginAtt (WindowHMargin left right) = (left,right)

getWindowHScrollFun :: !(WindowAttribute .st) -> ScrollFunction
getWindowHScrollFun (WindowHScroll f) = f

getWindowIdAtt :: !(WindowAttribute .st) -> Id
getWindowIdAtt (WindowId id) = id

getWindowIndexAtt :: !(WindowAttribute .st) -> Int
getWindowIndexAtt (WindowIndex index) = index

getWindowInitFun :: !(WindowAttribute .st) -> IdFun .st
getWindowInitFun (WindowInit init) = init

getWindowInitActiveAtt :: !(WindowAttribute .st) -> Id
getWindowInitActiveAtt (WindowInitActive id) = id

getWindowItemSpaceAtt :: !(WindowAttribute .st) -> (Int,Int)
getWindowItemSpaceAtt (WindowItemSpace hspace vspace) = (hspace,vspace)

getWindowKeyboardAtt :: !(WindowAttribute .st) -> (KeyboardStateFilter,SelectState,KeyboardFunction	.st)
getWindowKeyboardAtt (WindowKeyboard filter select keysF) = (filter,select,keysF)

getWindowLookAtt :: !(WindowAttribute .st) -> (Bool,Look)
getWindowLookAtt (WindowLook systemLook f) = (systemLook,f)

getWindowMouseAtt :: !(WindowAttribute .st) -> (MouseStateFilter,SelectState,MouseFunction .st)
getWindowMouseAtt (WindowMouse filter select mouseF) = (filter,select,mouseF)

getWindowOkAtt :: !(WindowAttribute .st) -> Id
getWindowOkAtt (WindowOk id) = id

getWindowOriginAtt :: !(WindowAttribute .st) -> Point2
getWindowOriginAtt (WindowOrigin origin) = origin

getWindowOuterSizeAtt :: !(WindowAttribute .st) -> Size
getWindowOuterSizeAtt (WindowOuterSize size) = size

getWindowPenAtt :: !(WindowAttribute .st) -> [PenAttribute]
getWindowPenAtt (WindowPen pen) = pen

getWindowPosAtt :: !(WindowAttribute .st) -> ItemPos
getWindowPosAtt (WindowPos pos) = pos

getWindowSelectStateAtt :: !(WindowAttribute .st) -> SelectState
getWindowSelectStateAtt (WindowSelectState select) = select

getWindowViewDomainAtt :: !(WindowAttribute .st) -> ViewDomain
getWindowViewDomainAtt (WindowViewDomain d) = d

getWindowViewSizeAtt :: !(WindowAttribute .st) -> Size
getWindowViewSizeAtt (WindowViewSize size) = size

getWindowVMarginAtt :: !(WindowAttribute .st) -> (Int,Int)
getWindowVMarginAtt (WindowVMargin top bottom) = (top,bottom)

getWindowVScrollFun :: !(WindowAttribute .st) -> ScrollFunction
getWindowVScrollFun (WindowVScroll f) = f
