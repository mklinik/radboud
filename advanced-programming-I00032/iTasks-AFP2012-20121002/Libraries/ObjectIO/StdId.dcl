definition module StdId


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	StdId specifies the generation functions for identification values.
//	********************************************************************************


from	StdMaybe	import :: Maybe
from	id			import :: Id, :: RId, :: R2Id, rIdtoId, r2IdtoId, class ==,
							instance == Id, 
							instance ==	(RId  mess), 
							instance == (R2Id mess resp)
from	iostate		import :: IOSt, :: PSt

class Ids env where
	openId		::      !*env -> (!Id,         !*env)
	openIds		:: !Int !*env -> (![Id],       !*env)
	
	openRId		::		!*env -> (!RId  m,     !*env)
	openRIds	:: !Int	!*env -> (![RId m],    !*env)
	
	openR2Id	:: 		!*env -> (!R2Id  m r,  !*env)
	openR2Ids	:: !Int	!*env -> (![R2Id m r], !*env)
/*	There are three types of identification values:
	-	RId  m:		for uni-directional message passing (see StdReceiver)
	-	R2Id m r:	for bi-directional  message passing (see StdReceiver)
	-	Id:			for all other Object I/O library components
	Of each generation function there are two variants:
	-	to create exactly one identification value.
	-	to create a number of identification values.
			If the integer argument <=0, then an empty list of identification values
			is generated.
*/

instance Ids World
instance Ids (IOSt .l)
instance Ids (PSt  .l)

getParentId :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
/*	getParentId returns the Id of the parent top-level GUI object 
	of the GUI component identified by the argument Id.
	If the GUI component could not be found then Nothing is returned.
*/
