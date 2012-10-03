implementation module StdTimerDef


import	StdIOCommon


::	Timer t ls pst	= Timer TimerInterval (t ls pst) [TimerAttribute *(ls,pst)]

::	TimerInterval
	:==	Int

::	TimerAttribute	st							// Default:
	=	TimerFunction		(TimerFunction st)	// \_ x->x
	|	TimerId				Id					// no Id
	|	TimerInit			(IdFun st)			// no actions after opening timer
	|	TimerSelectState	SelectState			// timer Able

::	TimerFunction	st	:==	NrOfIntervals -> st -> st
::	NrOfIntervals		:== Int

::	TimerType			:==	String
::	TimerElementType	:==	String
