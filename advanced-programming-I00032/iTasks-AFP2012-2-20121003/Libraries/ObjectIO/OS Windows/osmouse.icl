implementation module osmouse

from	ostoolbox import :: OSToolbox

// RWS ??? returned resolution
osGetDoubleClickTime :: !*OSToolbox -> (!Int, !*OSToolbox)
osGetDoubleClickTime toolbox
	= (0, toolbox)
