implementation module timerdefaccess


import	StdBool, StdMisc
import	StdMaybe, StdTimerAttribute
import	commondef


timerDefGetAttributes :: !(Timer t .ls .pst) -> [TimerAttribute *(.ls,.pst)]
timerDefGetAttributes (Timer _ _ atts)
	= atts

timerDefSetAbility	:: !SelectState !(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetAbility select (Timer interval items atts)
	= Timer interval items (setAbility select atts)
where
	setAbility :: !SelectState ![TimerAttribute .pst] -> [TimerAttribute .pst]
	setAbility select atts
		| done		= atts1
		| otherwise	= [att:atts]
	where
		att			= TimerSelectState select
		(done,atts1)= creplace isTimerSelectState att atts

timerDefSetInterval	:: !TimerInterval !(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetInterval interval (Timer _ items atts)
	= Timer interval items atts

timerDefSetFunction	:: !(TimerFunction *(.ls,.pst)) !(Timer t .ls .pst) -> Timer t .ls .pst
timerDefSetFunction f (Timer interval items atts)
	= Timer interval items (setFunction f atts)
where
	setFunction :: !(TimerFunction .pst) ![TimerAttribute .pst] -> [TimerAttribute .pst]
	setFunction f atts
		| done		= atts1
		| otherwise	= [att:atts]
	where
		att			= TimerFunction f
		(done,atts1)= creplace isTimerFunction att atts
