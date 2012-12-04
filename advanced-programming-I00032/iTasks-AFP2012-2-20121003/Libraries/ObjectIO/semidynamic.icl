implementation module semidynamic

import	StdBool
import	cast, commondef, id

semidynamicFatalError :: String String -> .x
semidynamicFatalError function error
	= fatalError function "semidynamic" error

::	SemiDynamic					// A SemiDynamic:
	=	E. x:
		{	value	:: x		//	the encapsulated value
		,	id		:: Id		//	the identification key
		}
::	DId m
	:==	Id

openDynamic :: !(DId m) m -> SemiDynamic
openDynamic did x = {value=x,id=did}

matchDynamic :: !(DId m) !SemiDynamic -> Bool
matchDynamic did {id} = did==id

readDynamic :: !(DId m) !SemiDynamic -> Maybe m
readDynamic did {value,id}
	| did==id
		= Just (cast value)
	| otherwise
		= Nothing

getDynamic :: !Id !SemiDynamic -> Maybe m
getDynamic did {value,id}
	| did==id
		= Just (cast value)
	| otherwise
		= Nothing

setDynamic :: !Id m !SemiDynamic -> SemiDynamic
setDynamic did x sd=:{id}
	| did==id
		= {sd & value=x}
	| otherwise
		= semidynamicFatalError "setDynamic" "SemiDynamic did not match argument Id"

rIdtoDId :: !(RId m) -> DId m
rIdtoDId rid = rIdtoId rid

r2IdtoDId :: !(R2Id m r) -> DId m
r2IdtoDId r2id = r2IdtoId r2id

r2IdtoDId` :: !(R2Id m r) -> DId r
r2IdtoDId` r2id = r2IdtoId r2id

dIdtoId :: !(DId m) -> Id
dIdtoId did = did
