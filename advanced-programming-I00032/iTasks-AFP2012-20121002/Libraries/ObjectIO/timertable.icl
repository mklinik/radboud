implementation module timertable


import StdInt, StdBool, StdClass, StdList, StdMaybe, StdMisc, StdId
import commondef, systemid, device
import StdTimerDef


::	TimerTable
	:==	[TimerTableEntry]						// The currently active timers
::	TimerTableEntry
	=	{	tteInterval	:: !Int					// The TimerInterval of the positive timer
		,	tteElapse	:: !Int					// The elapsed timer interval (may be negative)
		,	tteLoc		:: !TimerLoc			// The location of the positive timer
		}
::	TimerLoc
	=	{	tlIOId		:: !SystemId			// Id of parent process
		,	tlDevice	:: !Device				// Device kind of parent
		,	tlParentId	:: !Id					// Id of parent device instance
		,	tlTimerId	:: !Id					// Id of the timer itself
		}
::	TimerEvent
	=	{	teLoc		:: !TimerLoc			// The timer that should be evaluated
		,	teNrInterval:: !NrOfIntervals		// The nr of timer intervals that have elapsed
		}

initialTimerTable :: *TimerTable				// initialTimerTable yields an empty TimerTable
initialTimerTable
	= []

identifyTimerTableEntry :: !TimerLoc !TimerTableEntry -> Bool
identifyTimerTableEntry loc tte=:{tteLoc} = loc==tteLoc

instance == TimerLoc where
	(==) tt1 tt2 = tt1.tlIOId == tt2.tlIOId
					&&
				   tt1.tlDevice == tt2.tlDevice
				    &&
				   tt1.tlParentId == tt2.tlParentId
				    &&
				   tt1.tlTimerId == tt2.tlTimerId

/*	addTimerToTimerTable adds a new timer entry to the TimerTable.
	The Boolean result is True iff no duplicate timer entry was found, otherwise it is False.
	The TimerInterval argument is set to zero if it less than zero. 
*/
addTimerToTimerTable :: !TimerLoc !TimerInterval !*TimerTable -> (!Bool,!*TimerTable)
addTimerToTimerTable loc interval timers
	= add loc (max 0 interval) timers
where
	add :: !TimerLoc !TimerInterval !*[TimerTableEntry] -> (!Bool,!*[TimerTableEntry])
	add loc interval [tte:ttes]
		| identifyTimerTableEntry loc tte
			= (False,[tte:ttes])
		| otherwise
			# (isnew,ttes)	= add loc interval ttes
			= (isnew,[tte:ttes])
	add loc interval []
		= (True,[{tteInterval=interval,tteElapse=interval,tteLoc=loc}])

/*	removeTimerFromTimerTable removes a timer from the TimerTable.
	The Boolean result is True iff an entry was actually removed, otherwise it is False.
*/
removeTimerFromTimerTable :: !TimerLoc !*TimerTable -> (!Bool,!*TimerTable)
removeTimerFromTimerTable loc timers
	= (found,timers`)
where
	(found,_,timers`) = remove (identifyTimerTableEntry loc) undef timers

/*	setIntervalInTimerTable changes the timerinterval of the given timer in the TimerTable.
	If the timer was not present in the table, then nothing happens (the Boolean result is False).
	If the timer was present, its entry has been changed (the Boolean result is True).
*/
setIntervalInTimerTable :: !TimerLoc !TimerInterval !*TimerTable -> (!Bool,!*TimerTable)
setIntervalInTimerTable loc interval timers
	= set loc (max 0 interval) timers
where
	set :: !TimerLoc !TimerInterval !*[TimerTableEntry] -> (!Bool,!*[TimerTableEntry])
	set loc interval [tte:ttes]
		| identifyTimerTableEntry loc tte
			# tte = if (interval==0) {tte & tteInterval=interval,tteElapse=0} {tte & tteInterval=interval}
			= (True,[tte:ttes])
		| otherwise
			# (found,ttes)	= set loc interval ttes
			= (found,[tte:ttes])
	set _ _ []
		= (False,[])

/*	shiftTimeInTimerTable dt shifts the TimerTable dt (>0) ticks forward in time. 
*/
shiftTimeInTimerTable :: !Int !*TimerTable -> *TimerTable
shiftTimeInTimerTable dt timers
	| dt<=0
		= timers
	| otherwise
		= shiftTimes dt timers
where
	shiftTimes :: !Int !*[TimerTableEntry] -> *[TimerTableEntry]
	shiftTimes dt [tte=:{tteInterval,tteElapse}:ttes]
		| tteInterval==0
			#! ttes	= shiftTimes dt ttes
			= [tte:ttes]
		| otherwise
			#! tte	= {tte & tteElapse=tteElapse-dt}
			#! ttes	= shiftTimes dt ttes
			= [tte:ttes]
	shiftTimes _ []
		= []

/*	getActiveTimerInTimerTable determines the next timer that should be evaluated given the current
	TimerTable. Such a timer is any timer with a negative or zero elapsed time. 
	If such a timer could be found, then getActiveTimerInTimerTable returns its timer location and 
		number of fully elapsed timer intervals. The timer in question is placed behind all further
		timers, creating a round-robin evaluation order.
	If such a timer could not be found, then Nothing is returned. 
*/
getActiveTimerInTimerTable :: !*TimerTable -> (!Maybe TimerEvent,!*TimerTable)
getActiveTimerInTimerTable [tte=:{tteElapse,tteInterval,tteLoc}:ttes]
	| tteElapse<=0
		#! nrTimeInterval	= if (tteInterval==0) 1 ((abs tteElapse)/tteInterval+1)
		#! tEvent			= {teLoc=tteLoc,teNrInterval=nrTimeInterval}
		#! tte`				= {tte & tteElapse=tteElapse+nrTimeInterval*tteInterval}
		=  (Just tEvent,ttes++[tte`])
	| otherwise
		#  (active,ttes)	= getActiveTimerInTimerTable ttes
		=  (active,[tte:ttes])
getActiveTimerInTimerTable _
	= (Nothing,[])

/*	getTimeIntervalFromTimerTable returns the (Just (zerotimer,time)) interval that can be 
	waited for the next timer to become active. The Boolean zerotimer holds iff the time
	returned belongs to a zero timer.
	If there are no timers, then Nothing is returned.
*/
getTimeIntervalFromTimerTable :: !*TimerTable -> (!Maybe (Bool,Int),!*TimerTable)
getTimeIntervalFromTimerTable []
	= (Nothing,[])
getTimeIntervalFromTimerTable timers
	#! (isZero,sleep,timers)	= getSleepTime (2^31-1) timers
	= (Just (isZero,sleep),timers)
where
	getSleepTime :: !Int !*[TimerTableEntry] -> (!Bool,!Int,!*[TimerTableEntry])
	getSleepTime sleep [tte=:{tteElapse,tteInterval}:ttes]
		| tteElapse<=0
			= (tteInterval==0,0,[tte:ttes])
		| otherwise
			# (isZero,sleep,ttes)	= getSleepTime (min sleep tteElapse) ttes
			= (isZero,sleep,[tte:ttes])
	getSleepTime sleep []
		= (False,sleep,[])
