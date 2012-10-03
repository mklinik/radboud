definition module StdPSt


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdPSt defines operations on PSt and IOSt that are not abstract device related.
//	********************************************************************************


from	StdFunc		import :: St
from	StdIOCommon	import :: DocumentInterface, :: IdFun
from	StdPicture	import :: Picture
from	iostate		import :: IOSt, :: PSt{..}


/*	accScreenPicture provides access to an initial Picture as it would be created in
	a window or control.
*/
class accScreenPicture env :: !.(St *Picture .x) !*env -> (!.x,!*env)

instance accScreenPicture World
instance accScreenPicture (IOSt .l)


beep :: !(IOSt .l) -> IOSt .l
/*	beep emits the alert sound.
*/


//	Operations on the DocumentInterface of an interactive process:

getDocumentInterface :: !(IOSt .l) -> (!DocumentInterface, !IOSt .l)
/*	getDocumentInterface returns the DocumentInterface of the interactive process.
*/


//	Operations on the attributes of an interactive process:

setProcessActivate	:: !(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
setProcessDeactivate:: !(IdFun (PSt .l)) !(IOSt .l) -> IOSt .l
/*	These functions set the ProcessActivate and ProcessDeactivate attribute of the 
	interactive process respectively.
*/


//	Coercing PSt component operations to PSt operations.

appListPIO	:: ![.IdFun (IOSt .l)]	!(PSt .l) ->		 PSt .l
appListPLoc	:: ![.IdFun .l]			!(PSt .l) ->		 PSt .l

appPIO		:: !.(IdFun (IOSt .l))	!(PSt .l) ->		 PSt .l
appPLoc		:: !.(IdFun .l)			!(PSt .l) ->		 PSt .l

//	Accessing PSt component operations.

accListPIO	:: ![.St (IOSt .l) .x]	!(PSt .l) -> (![.x],!PSt .l)
accListPLoc	:: ![.St .l        .x]	!(PSt .l) -> (![.x],!PSt .l)

accPIO		:: !.(St (IOSt .l) .x)	!(PSt .l) -> (! .x,	!PSt .l)
accPLoc		:: !.(St .l        .x)	!(PSt .l) -> (! .x,	!PSt .l)
