module hello

//	**************************************************************************************************
//
//	This program is the graphical counterpart of the ubiquitous 'hello world' program.
//
//	The program has been written in Clean 2.0 and uses the Clean Standard Object I/O library 1.2.2
//	
//	**************************************************************************************************

import StdEnv, StdIO

Start :: *World -> *World
Start world
	= startIO NDI Void (snd o openDialog undef hello) [] world
where
	hello	= Dialog "" (TextControl "Hello world!" []) [WindowClose (noLS closeProcess)]
