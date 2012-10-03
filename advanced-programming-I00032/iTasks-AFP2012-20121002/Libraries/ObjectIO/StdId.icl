implementation module StdId


import	StdBool, StdInt, StdEnum
import	id, iostate, StdPSt, world


class Ids env where
	openId		::      !*env -> (!Id,			!*env)
	openIds		:: !Int !*env -> (![Id],		!*env)
	
	openRId		::		!*env -> (!RId  m,		!*env)
	openRIds	:: !Int	!*env -> (![RId m],		!*env)
	
	openR2Id	:: 		!*env -> (!R2Id  m r,	!*env)
	openR2Ids	:: !Int	!*env -> (![R2Id m r],	!*env)

instance Ids World where
	openId :: !*World -> (!Id, !*World)
	openId world
		# w	= loadWorld world
		= (toId w,storeWorld (w-1) world)
		
	openIds :: !Int !*World -> (![Id], !*World)
	openIds n world
		# w = loadWorld world
		= ([toId nr \\ nr<-[w-n+1..w]],storeWorld (w-n) world)
	
	
	openRId :: !*World -> (!RId m, !*World)
	openRId world
		# w = loadWorld world
		= (toRId w,storeWorld (w-1) world)
	
	openRIds :: !Int !*World -> (![RId m], !*World)
	openRIds n world
		# w = loadWorld world
		= ([toRId nr \\ nr<-[w-n+1..w]],storeWorld (w-n) world)
	
	
	openR2Id :: !*World -> (!R2Id m r, !*World)
	openR2Id world
		# w = loadWorld world
		= (toR2Id w,storeWorld (w-1) world)
	
	openR2Ids :: !Int !*World -> (![R2Id m r], !*World)
	openR2Ids n world
		# w = loadWorld world
		= ([toR2Id nr \\ nr<-[w-n+1..w]],storeWorld (w-n) world)

instance Ids (IOSt .l) where
	openId :: !*(IOSt .l) -> (!Id, !*IOSt .l)
	openId ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= (toId idseed,ioStSetIdSeed (idseed-1) ioState)
	
	openIds :: !Int !*(IOSt .l) -> (![Id], !*IOSt .l)
	openIds n ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= ([toId nr \\ nr<-[idseed-n+1..idseed]],ioStSetIdSeed (idseed-n) ioState)
	
	
	openRId :: !*(IOSt .l) -> (!RId m, !*IOSt .l)
	openRId ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= (toRId idseed,ioStSetIdSeed (idseed-1) ioState)
	
	openRIds :: !Int !*(IOSt .l) -> (![RId m], !*IOSt .l)
	openRIds n ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= ([toRId nr \\ nr<-[idseed-n+1..idseed]],ioStSetIdSeed (idseed-n) ioState)
	
	
	openR2Id :: !*(IOSt .l) -> (!R2Id m r, !*IOSt .l)
	openR2Id ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= (toR2Id idseed,ioStSetIdSeed (idseed-1) ioState)
	
	openR2Ids :: !Int !*(IOSt .l) -> (![R2Id m r], !*IOSt .l)
	openR2Ids n ioState
		# (idseed,ioState)	= ioStGetIdSeed ioState
		= ([toR2Id nr \\ nr<-[idseed-n+1..idseed]],ioStSetIdSeed (idseed-n) ioState)

instance Ids (PSt .l) where
	openId      pSt			= accPIO  openId       pSt
	openIds	i   pSt=:{io}	= accPIO (openIds i)   pSt
	openRId	    pSt			= accPIO  openRId      pSt
	openRIds i  pSt			= accPIO (openRIds i)  pSt
	openR2Id    pSt			= accPIO  openR2Id     pSt
	openR2Ids i pSt			= accPIO (openR2Ids i) pSt


getParentId :: !Id !(IOSt .l) -> (!Maybe Id,!IOSt .l)
getParentId id ioState
	# (idTable,ioState)		= ioStGetIdTable ioState
	  (maybeParent,idTable)	= getIdParent id idTable
	# ioState				= ioStSetIdTable idTable ioState
	| isNothing maybeParent
		= (Nothing,ioState)
	| otherwise
		# parent		= fromJust maybeParent
		= (Just parent.idpId,ioState)
