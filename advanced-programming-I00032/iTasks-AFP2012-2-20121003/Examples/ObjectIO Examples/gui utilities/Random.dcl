definition module Random

//	**************************************************************************************************
//
//	General utility for random number generation.
//
//	This module has been written in Clean 1.3.2 and uses the Clean Standard Object I/O library 1.2
//	
//	**************************************************************************************************

import StdTime

::	RandomSeed

nullRandomSeed	:: RandomSeed
//	nullRandomSeed generates a useless RandomSeed (random nullRandomSeed = (0,nullRandomSeed)).

getNewRandomSeed:: !*env	-> (!RandomSeed, !*env)	| TimeEnv env
//	GetNewRandomSeed generates a useful RandomSeed, using the current time.

random			:: !RandomSeed		-> (!Int, !RandomSeed)
//	Given a RandomSeed, Random generates a random number and a new RandomSeed.
