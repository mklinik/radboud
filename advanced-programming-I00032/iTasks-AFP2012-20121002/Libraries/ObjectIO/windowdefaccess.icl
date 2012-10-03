implementation module windowdefaccess


import	StdTuple, StdList
import	commondef, StdWindowAttribute


//	Get & Set the WindowAttributes:

getWindowAttributes :: !(Window c .ls .pst) -> (![WindowAttribute *(.ls,.pst)],!Window c .ls .pst)
getWindowAttributes wdef=:(Window _ _ atts) = (atts,wdef)

setWindowAttributes :: ![WindowAttribute *(.ls,.pst)] !(Window c .ls .pst) -> Window c .ls .pst
setWindowAttributes atts (Window title controls _) = Window title controls atts


//	Get & Set the DialogAttributes:

getDialogAttributes :: !(Dialog c .ls .pst) -> (![WindowAttribute *(.ls,.pst)],!Dialog c .ls .pst)
getDialogAttributes ddef=:(Dialog _ _ atts) = (atts,ddef)

setDialogAttributes :: ![WindowAttribute *(.ls,.pst)] !(Dialog c .ls .pst) -> Dialog c .ls .pst
setDialogAttributes atts (Dialog title controls _) = Dialog title controls atts


//	Frequently used predicates on the WindowAttributes:

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
