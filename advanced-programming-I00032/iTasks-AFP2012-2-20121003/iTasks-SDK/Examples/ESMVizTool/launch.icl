implementation module launch

import StdString, ostoolbox, clCCall_12

launch :: !PathAndApplication !CommandlineArgs !*World -> (!Bool,!Int,!*World)
launch command cmndline world
	# (tb,world)			= worldGetToolbox world
	# (commandptr,tb)		= winMakeCString (command+++" "+++cmndline) tb
	# (success,exitcode,tb) = winCallProcess commandptr 0 0 0 0 0 tb
	# tb					= winReleaseCString commandptr tb
	# world					= worldSetToolbox tb world
	= (success,exitcode,world)
