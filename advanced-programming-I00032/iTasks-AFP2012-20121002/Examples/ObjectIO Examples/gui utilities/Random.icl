implementation module Random


import	StdInt, StdClass
import	StdTime


::	RandomSeed	:== Int


nullRandomSeed :: RandomSeed
nullRandomSeed
	=	0

getNewRandomSeed :: !*env -> (!RandomSeed, !*env) | TimeEnv env
getNewRandomSeed env
	#	({hours,minutes,seconds}, env)	= getCurrentTime env
	=	(1+(hours+minutes+seconds) bitand 65535, env)

random :: !RandomSeed -> (!Int,!RandomSeed)
random seed
	=	(newSeed,newSeed)
where
	newSeed		= if (nextSeed>=0) nextSeed (nextSeed+65537)
	nextSeed	= (seed75 bitand 65535)-(seed75>>16)
	seed75		= seed*75
