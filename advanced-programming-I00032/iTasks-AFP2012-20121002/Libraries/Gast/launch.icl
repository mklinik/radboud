implementation module launch

import StdEnv
import /*StdIO,*/ostoolbox, clCCall_12

//Start = launch (applicationpath "hello.exe") 42
/*Start
	# (ok1,exit1,tb)	= launch exe (toGIF "test") 42
	# (ok2,exit2,tb)	= launch exe (toMAP "test") tb
	= (ok1 && ok2, (exit1,exit2),tb)*/
//Start = launch exe arg 42

/*
exe			= "C:\\Program Files\\ATT\\Graphviz\\bin\\dot.exe"
toGIF dot	= "-Tgif -o "   +++ dot +++ ".gif " +++ dot +++ ".dot"
toMAP dot	= "-Tcmapx -o " +++ dot +++ ".map " +++ dot +++ ".dot"

exe			= "D:\\Peter\\Projecten\\SpecificationVerification\\LaunchExternalApplication\\showCmndlineArgs.exe"
arg			= "one two three four"
*/

launch :: !PathAndApplication !CommandlineArgs !*World -> (!Bool,!Int,!*World)
launch command cmndline world
	# (tb,world)			= worldGetToolbox world
	# (commandptr,tb)		= winMakeCString (command+++" "+++cmndline) tb
	# (success,exitcode,tb) = winCallProcess commandptr 0 0 0 0 0 tb
	# tb					= winReleaseCString commandptr tb
	# world					= worldSetToolbox tb world
	= (success,exitcode,world)
