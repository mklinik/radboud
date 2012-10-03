definition module semidynamic


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	This module implements a heavily degraded kind of dynamics. In a version of Clean
//	with proper dynamics, this module will disappear.
//	********************************************************************************


import id

::	SemiDynamic
::	DId m

/*	openDynamic  did value
		creates a new semidynamic.
	matchDynamic did semidynamic
		returns True iff did is equal to the (DId m) when the semidynamic was created.
	readDynamic  did semidynamic
		returns the encapsulated value only if (matchDynamic did semidynamic), otherwise
		readDynamic aborts.
	getDynamic   id semidynamic
		returns the encapsulated value only if id corresponds properly with the (DId m)
		value that was used to create the semidynamic. If not, getDynamic aborts.
	setDynamic   id value semidynamic
		replaces the current value in semidynamic by value only if id corresponds properly
		with the (DId m) value that was used to create the semidynamic. If not, setDynamic
		aborts.
*/
openDynamic	:: !(DId m) m -> SemiDynamic
matchDynamic:: !(DId m) !SemiDynamic -> Bool
readDynamic	:: !(DId m) !SemiDynamic -> Maybe m
getDynamic	:: !Id      !SemiDynamic -> Maybe m
setDynamic	:: !Id m    !SemiDynamic -> SemiDynamic

rIdtoDId	:: !(RId  m  ) -> DId m
r2IdtoDId	:: !(R2Id m r) -> DId m
r2IdtoDId`	:: !(R2Id m r) -> DId r
dIdtoId		:: !(DId  m  ) -> Id
