implementation module osbeep

from	clCCall_12	import winBeep
from	ostoolbox	import :: OSToolbox

osBeep :: !*OSToolbox -> *OSToolbox
osBeep toolbox
	= winBeep toolbox
