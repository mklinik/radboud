implementation module StdTimerElementClass


import	StdList
import	StdTimerDef
import	commondef, iostate, timerhandle


class TimerElements t where
	timerElementToHandles :: !.(t .ls (PSt .l)) !(PSt .l) -> (![TimerElementState .ls (PSt .l)],!PSt .l)
	getTimerElementType   ::  .(t .ls .pst)               -> TimerElementType

instance TimerElements (AddLS t) | TimerElements t where
	timerElementToHandles {addLS,addDef} pState
		# (ts,pState)	= timerElementToHandles addDef pState
		= (	[timerElementHandleToTimerElementState 
				(TimerExtendLSHandle { tExtendLS	= addLS
									 , tExtendItems	= map timerElementStateToTimerElementHandle ts
									 }
				)
			]
		  ,	pState
		  )
	getTimerElementType _ = ""

instance TimerElements (NewLS t) | TimerElements t where
	timerElementToHandles {newLS,newDef} pState
		# (ts,pState)	= timerElementToHandles newDef pState
		= (	[timerElementHandleToTimerElementState 
				(TimerChangeLSHandle { tChangeLS	= newLS
									 , tChangeItems	= map timerElementStateToTimerElementHandle ts
									 }
				)
			]
		  ,	pState
		  )
	getTimerElementType _ = ""

instance TimerElements (ListLS t) | TimerElements t where
	timerElementToHandles (ListLS tDefs) pState
		# (tss,pState)	= stateMap timerElementToHandles tDefs pState
		= (	[timerElementHandleToTimerElementState 
				(TimerListLSHandle (map timerElementStateToTimerElementHandle (flatten tss)))
			]
		  ,	pState
		  )
	getTimerElementType _ = ""

instance TimerElements NilLS where
	timerElementToHandles NilLS pState
		= ([timerElementHandleToTimerElementState (TimerListLSHandle [])],pState)
	
	getTimerElementType _ = ""

instance TimerElements ((:+:) t1 t2) | TimerElements t1 & TimerElements t2 where
	timerElementToHandles (t1:+:t2) pState
		# (ts1,pState)	= timerElementToHandles t1 pState
		# (ts2,pState)	= timerElementToHandles t2 pState
		= (ts1 ++ ts2,pState)
	
	getTimerElementType _ = ""
