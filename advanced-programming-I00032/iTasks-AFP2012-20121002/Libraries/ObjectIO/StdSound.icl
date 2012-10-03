implementation module StdSound


import	StdString
//from	clCCall_12	import WinPlaySound
import	ostoolbox


class playSoundFile env :: !String !*env -> (!Bool,!*env)

instance playSoundFile World where
	playSoundFile :: !String !*World -> (!Bool,!*World)
	playSoundFile soundFileName world
		# (tb,world)	= worldGetToolbox world
//		# (ok,tb)		= WinPlaySound soundFileName tb
	    # ok = False
	    # world			= worldSetToolbox tb world
	    = (ok,world)
