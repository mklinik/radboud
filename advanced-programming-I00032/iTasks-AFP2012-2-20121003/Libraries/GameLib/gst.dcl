definition module gst

//	********************************************************************************
//	Clean Standard Game library, version 1.2.2
//	
//	Author:   Mike Wiering
//	Modified: 7 Sept 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	ostoolbox
from	StdFunc		import :: St
from	StdIOBasic	import :: IdFun

::  *GSt gs

fromGSt :: !*(GSt .gs) -> (.gs,!*OSToolbox)
toGSt   :: .gs !*OSToolbox -> *GSt .gs
appGStTb:: !.(IdFun *OSToolbox)    !*(GSt .gs) ->       *GSt .gs
accGStTb:: !.(St    *OSToolbox .x) !*(GSt .gs) -> (!.x,!*GSt .gs)
appGSt  :: !.(IdFun .gs)           !*(GSt .gs) ->       *GSt .gs
accGSt  :: !.(St    .gs .x)        !*(GSt .gs) -> (.x, !*GSt .gs)
