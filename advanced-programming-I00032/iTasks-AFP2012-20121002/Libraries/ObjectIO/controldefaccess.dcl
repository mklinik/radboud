definition module controldefaccess


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Access functions to ControlDefinitions.
//	********************************************************************************


import	StdControlAttribute


controlAttributesHaveThisId	:: !Id ![ControlAttribute .st]	-> Bool

sameControlAttribute		:: !(ControlAttribute .st) !(ControlAttribute .st) -> Bool
