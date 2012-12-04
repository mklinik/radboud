implementation module controldefaccess


import	StdBool, StdInt, StdMisc
import	commondef, StdControlAttribute


//	Access rules to ControlAttributes:

controlAttributesHaveThisId :: !Id ![ControlAttribute .st] -> Bool
controlAttributesHaveThisId id atts
	= hasId && id==getControlIdAtt idAtt
where
	(hasId,idAtt)	= cselect isControlId undef atts


sameControlAttribute :: !(ControlAttribute .st) !(ControlAttribute .st) -> Bool
sameControlAttribute (ControlId				_) att = isControlId att
sameControlAttribute (ControlPen			_) att = isControlPen att
sameControlAttribute (ControlPos			_) att = isControlPos att
sameControlAttribute (ControlViewSize		_) att = isControlViewSize att
sameControlAttribute (ControlMinimumSize	_) att = isControlMinimumSize att
sameControlAttribute (ControlResize			_) att = isControlResize att
sameControlAttribute (ControlSelectState	_) att = isControlSelectState att
sameControlAttribute  ControlHide			   att = isControlHide att
sameControlAttribute (ControlFunction		_) att = isControlFunction att
sameControlAttribute (ControlModsFunction	_) att = isControlModsFunction att
sameControlAttribute (ControlActivate		_) att = isControlActivate att
sameControlAttribute (ControlDeactivate		_) att = isControlDeactivate att
sameControlAttribute (ControlMouse		_ _ _) att = isControlMouse att
sameControlAttribute (ControlKeyboard	_ _ _) att = isControlKeyboard att
sameControlAttribute (ControlItemSpace	_	_) att = isControlItemSpace att
sameControlAttribute (ControlHMargin	_	_) att = isControlHMargin att
sameControlAttribute (ControlVMargin	_	_) att = isControlVMargin att
sameControlAttribute (ControlLook		_	_) att = isControlLook att
sameControlAttribute (ControlTip			_) att = isControlTip att
sameControlAttribute (ControlViewDomain		_) att = isControlViewDomain att
sameControlAttribute (ControlOrigin			_) att = isControlOrigin att
sameControlAttribute (ControlHScroll		_) att = isControlHScroll att
sameControlAttribute (ControlVScroll		_) att = isControlVScroll att
