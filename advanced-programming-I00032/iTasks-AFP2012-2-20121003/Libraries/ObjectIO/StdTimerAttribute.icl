implementation module StdTimerAttribute


import StdTimerDef


/*	The following functions specify the valid attributes for each standard timer.
*/

isValidTimerAttribute :: !(TimerAttribute .st) -> Bool
isValidTimerAttribute _ = True


isTimerFunction		:: !(TimerAttribute .st) -> Bool
isTimerFunction			(TimerFunction _)	= True
isTimerFunction			_					= False

isTimerId			:: !(TimerAttribute .st) -> Bool
isTimerId				(TimerId _)			= True
isTimerId				_					= False

isTimerInit			:: !(TimerAttribute .st) -> Bool
isTimerInit				(TimerInit _)		= True
isTimerInit				_					= False

isTimerSelectState	:: !(TimerAttribute .st) -> Bool
isTimerSelectState		(TimerSelectState _)= True
isTimerSelectState		_					= False


getTimerFun :: !(TimerAttribute .st) -> TimerFunction .st
getTimerFun (TimerFunction f) = f

getTimerIdAtt :: !(TimerAttribute .st) -> Id
getTimerIdAtt (TimerId id) = id

getTimerInitFun :: !(TimerAttribute .st) -> IdFun .st
getTimerInitFun (TimerInit f) = f

getTimerSelectStateAtt :: !(TimerAttribute .st) -> SelectState
getTimerSelectStateAtt (TimerSelectState s) = s
