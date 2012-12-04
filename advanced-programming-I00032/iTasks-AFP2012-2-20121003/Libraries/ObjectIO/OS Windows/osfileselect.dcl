definition module osfileselect

//	Clean Object I/O library, version 1.2

import	StdString
from	ostoolbox	import :: OSToolbox
import	osevent

osInitialiseFileSelectors	:: !*OSToolbox -> *OSToolbox
osSelectinputfile			:: !(OSEvent->.s->.s) !.s                 !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
osSelectoutputfile			:: !(OSEvent->.s->.s) !.s !String !String !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
osSelectdirectory			:: !(OSEvent->.s->.s) !.s                 !*OSToolbox -> (!Bool,!String,!.s,!*OSToolbox)
