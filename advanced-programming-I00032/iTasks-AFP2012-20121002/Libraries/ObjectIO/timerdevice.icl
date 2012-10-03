implementation module timerdevice


import	StdInt, StdBool, StdFunc, StdEnum, StdList, StdTuple
from StdPSt import accPIO, appPIO
import	commondef, devicefunctions, iostate, receiveraccess, timeraccess, timerdefaccess, timerevent


timerdeviceFatalError :: String String -> .x
timerdeviceFatalError function error
	= fatalError function "timerdevice" error

timerFunctions :: DeviceFunctions (PSt .l)
timerFunctions
	= {	dDevice	= TimerDevice
	  ,	dShow	= id
	  ,	dHide	= id
	  ,	dEvent	= timerEvent
	  ,	dDoIO	= timerIO
	  ,	dOpen	= timerOpen
	  ,	dClose	= timerClose
	  }

timerOpen :: !(PSt .l) -> PSt .l
timerOpen pState=:{io=ioState}
	# (hasTimer,ioState)	= ioStHasDevice TimerDevice ioState
	| hasTimer
		= {pState & io=ioState}
	| otherwise
		# ioState			= ioStSetDevice (TimerSystemState {tTimers=[]}) ioState
		# ioState			= ioStSetDeviceFunctions timerFunctions ioState
		= {pState & io=ioState}

timerClose :: !(PSt .l) -> PSt .l
timerClose pState=:{io=ioState}
	# (found,timers,ioState)= ioStGetDevice TimerDevice ioState
	| not found
		= {pState & io=ioState}
	| otherwise
		# (rt,ioState)		= ioStGetReceiverTable ioState
		# (tt,ioState)		= ioStGetTimerTable ioState
		# (idtable,ioState)	= ioStGetIdTable ioState
		# (pid,ioState)		= ioStGetIOId ioState
		  tHs				= timerSystemStateGetTimerHandles timers
		  (tt,rt,idtable)	= stateMap2 (closeTimerIds pid) tHs.tTimers (tt,rt,idtable)
		# ioState			= ioStSetReceiverTable rt ioState
		# ioState			= ioStSetTimerTable tt ioState
		# ioState			= ioStSetIdTable idtable ioState
		# ioState			= ioStRemoveDeviceFunctions TimerDevice ioState
		= {pState & io=ioState}
where
	closeTimerIds :: !SystemId !(TimerStateHandle .pst) !*(!*TimerTable,!*ReceiverTable,!*IdTable) -> *(!*TimerTable,!*ReceiverTable,!*IdTable)
	closeTimerIds pid (TimerLSHandle {tHandle={tId,tItems}}) tables
		# (tt,rt,it)	= unbindTimerElementIds pid tItems tables
		= (snd (removeTimerFromTimerTable teLoc tt),rt,snd (removeIdFromIdTable tId it))
	where
		teLoc			= {tlIOId=pid,tlDevice=TimerDevice,tlParentId=tId,tlTimerId=tId}

timerIO	:: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
timerIO deviceEvent pState
	# (hasDevice,pState)= accPIO (ioStHasDevice TimerDevice) pState
	| not hasDevice
		= timerdeviceFatalError "timerFunctions.dDoIO" "could not retrieve TimerSystemState from IOSt"
	| otherwise
		= timerIO deviceEvent pState
where
	timerIO	:: !DeviceEvent !(PSt .l) -> (!DeviceEvent,!PSt .l)
	timerIO deviceEvent=:(TimerEvent te=:{teLoc={tlParentId,tlTimerId},teNrInterval}) pState=:{io}
		# (_,timer,ioState)	= ioStGetDevice TimerDevice io
		  timers			= timerSystemStateGetTimerHandles timer
		  pState			= {pState & io=ioState}
		# pState			= letOneTimerDoIO tlParentId tlTimerId teNrInterval timers pState
		= (deviceEvent,pState)
	where
		letOneTimerDoIO :: !Id !Id !NrOfIntervals !(TimerHandles (PSt .l)) !(PSt .l) -> PSt .l
		letOneTimerDoIO parent timer nrOfIntervals timers=:{tTimers=tHs} pState
			= pState2
		where
			(_,tH,tHs1)		= uremove (identifyTimerStateHandle parent) (timerdeviceFatalError "timerIO (TimerEvent _)" "timer could not be found") tHs
			pState1			= appPIO (ioStSetDevice timers1) pState
			(tH1,pState2)	= letTimerDoIO nrOfIntervals tH pState1
			timers1			= TimerSystemState {timers & tTimers=tHs1++[tH1]}
			
/*			Compiling with 'Reuse Unique Nodes' causes a space-leak in this function definition.
			Therefore it is replaced temporarily with the function below.
			letTimerDoIO :: !NrOfIntervals !(TimerStateHandle .ps) !.ps -> (!TimerStateHandle .ps, .ps)
			letTimerDoIO nrOfIntervals (TimerLSHandle tsH=:{tState=ls,tHandle=tH=:{tFun}}) pState
				= (TimerLSHandle {tsH & tState=ls1},pState1)
			where
				(ls1,pState1)	= tFun nrOfIntervals (ls,pState)
*/			
			letTimerDoIO :: !NrOfIntervals !(TimerStateHandle .ps) !.ps -> (!TimerStateHandle .ps, .ps)
			letTimerDoIO nrOfIntervals (TimerLSHandle tsH=:{tState=ls,tHandle=tH=:{tFun}}) pState
				= (TimerLSHandle {tsH & tState=ls1},pState1)
			where
				(ls1,pState1)	= apply tFun nrOfIntervals ls pState
				
				apply :: !(TimerFunction *(.ls,.pst)) !NrOfIntervals .ls !.pst -> (.ls,!.pst)
				apply f d_i ls pst = f d_i (ls,pst)
	
	timerIO deviceEvent=:(ReceiverEvent (QASyncMessage event)) pState
		= (deviceEvent,timerQASync event pState)
	where
		timerQASync :: !QASyncMessage !(PSt .l) -> PSt .l
		timerQASync msg=:{qasmRecLoc={rlReceiverId=rid}} pState
			# (_,timer,ioState)	= ioStGetDevice TimerDevice pState.io
			  timers			= timerSystemStateGetTimerHandles timer
			  tsHs				= handleASyncReceiver rid msg timers.tTimers
			  timers			= {timers & tTimers=tsHs}
			# ioState			= ioStSetDevice (TimerSystemState timers) ioState
			# pState			= {pState & io=ioState}
			= pState
		where
			handleASyncReceiver :: !Id !QASyncMessage ![TimerStateHandle .pst] -> [TimerStateHandle .pst]
			handleASyncReceiver rid msg [TimerLSHandle tlsH=:{tHandle=tH=:{tItems}}:tsHs]
				# (done,tItems)	= qMessage rid msg tItems
				  tsH			= TimerLSHandle {tlsH & tHandle={tH & tItems=tItems}}
				| done			= [tsH:tsHs]
				| otherwise		= [tsH:handleASyncReceiver rid msg tsHs]
			where
				qMessage :: !Id !QASyncMessage ![TimerElementHandle .ls .pst] -> (!Bool,![TimerElementHandle .ls .pst])
				qMessage rid msg [itemH:itemHs]
					# (done,itemH)	= qMessage` rid msg itemH
					| done
						= (done,[itemH:itemHs])
					| otherwise
						# (done,itemHs)	= qMessage rid msg itemHs
						= (done,[itemH:itemHs])
				where
					qMessage` :: !Id !QASyncMessage !(TimerElementHandle .ls .pst) -> (!Bool,!TimerElementHandle .ls .pst)
					qMessage` rid {qasmMsg} (TimerReceiverHandle trH=:{tReceiverHandle=rH})
						| receiverIdentified rid rH	= (True, TimerReceiverHandle {trH & tReceiverHandle=receiverAddASyncMessage rid qasmMsg rH})
						| otherwise					= (False,TimerReceiverHandle trH)
					qMessage` rid msg (TimerListLSHandle itemHs)
						# (done,itemHs) = qMessage rid msg itemHs
						= (done,TimerListLSHandle itemHs)
					qMessage` rid msg (TimerElimLSHandle itemHs)
						# (done,itemHs) = qMessage rid msg itemHs
						= (done,TimerElimLSHandle itemHs)
					qMessage` rid msg (TimerIntroLSHandle tInH=:{tIntroItems})
						# (done,itemHs) = qMessage rid msg tIntroItems
						= (done,TimerIntroLSHandle {tInH & tIntroItems=itemHs})
					qMessage` rid msg (TimerExtendLSHandle tExH=:{tExtendItems})
						# (done,itemHs) = qMessage rid msg tExtendItems
						= (done,TimerExtendLSHandle {tExH & tExtendItems=itemHs})
					qMessage` rid msg (TimerChangeLSHandle tChH=:{tChangeItems})
						# (done,itemHs) = qMessage rid msg tChangeItems
						= (done,TimerChangeLSHandle {tChH & tChangeItems=itemHs})
				qMessage _ _ _
					= (False,[])
			handleASyncReceiver _ _ _
				= []
	timerIO deviceEvent=:(ReceiverEvent (ASyncMessage event)) pState
		= (deviceEvent,timerASync event pState)
	where
		timerASync :: !ASyncMessage !(PSt .l) -> PSt .l
		timerASync {asmRecLoc={rlReceiverId=rid}} pState
			= pState2
		where
			(_,timers,ioState)	= ioStGetDevice TimerDevice pState.io
			tHs					= timerSystemStateGetTimerHandles timers
			(tsHs,pState2)		= aSyncTimerStateHandles rid tHs.tTimers pState1
			tHs1				= {tHs & tTimers=tsHs}
			ioState1			= ioStSetDevice (TimerSystemState tHs1) ioState
			pState1				= {pState & io=ioState1}
			
			aSyncTimerStateHandles :: !Id ![TimerStateHandle .pst] .pst -> *(![TimerStateHandle .pst],.pst)
			aSyncTimerStateHandles rid [TimerLSHandle {tState=ls,tHandle=tH=:{tItems}}:tsHs] ps
				# (done,tItems,(ls,ps))	= aSyncTimerElementHandles rid tItems (ls,ps)
				  tsH					= TimerLSHandle {tState=ls,tHandle={tH & tItems=tItems}}
				| done
					= ([tsH:tsHs],ps)
				| otherwise
					# (tsHs,ps)			= aSyncTimerStateHandles rid tsHs ps
					= ([tsH:tsHs],ps)
			where
				aSyncTimerElementHandles :: !Id ![TimerElementHandle .ls .pst] *(.ls,.pst)
									  -> (!Bool,![TimerElementHandle .ls .pst],*(.ls,.pst))
				aSyncTimerElementHandles rid [itemH:itemHs] ps
					# (done,itemH, ps)	= aSyncTimerElementHandle  rid itemH  ps
					| done
						= (done,[itemH:itemHs],ps)
					| otherwise
						# (done,itemHs,ps)	= aSyncTimerElementHandles rid itemHs ps
						= (done,[itemH:itemHs],ps)
				where
					aSyncTimerElementHandle :: !Id !(TimerElementHandle .ls .pst) *(.ls,.pst)
										  -> (!Bool,!TimerElementHandle .ls .pst, *(.ls,.pst))
					aSyncTimerElementHandle rid (TimerListLSHandle itemHs) ps
						# (done,itemHs,ps) = aSyncTimerElementHandles rid itemHs ps
						= (done,TimerListLSHandle itemHs,ps)
					aSyncTimerElementHandle rid (TimerElimLSHandle itemHs) ps
						# (done,itemHs,ps) = aSyncTimerElementHandles rid itemHs ps
						= (done,TimerElimLSHandle itemHs,ps)
					aSyncTimerElementHandle rid (TimerIntroLSHandle {tIntroLS,tIntroItems}) (ls,ps)
						# (done,itemHs,(introLS,ps)) = aSyncTimerElementHandles rid tIntroItems (tIntroLS,ps)
						= (done,TimerIntroLSHandle {tIntroLS=introLS,tIntroItems=itemHs},(ls,ps))
					aSyncTimerElementHandle rid (TimerExtendLSHandle {tExtendLS,tExtendItems}) (ls,ps)
						# (done,itemHs,((extendLS,ls),ps)) = aSyncTimerElementHandles rid tExtendItems ((tExtendLS,ls),ps)
						= (done,TimerExtendLSHandle {tExtendLS=extendLS,tExtendItems=itemHs},(ls,ps))
					aSyncTimerElementHandle rid (TimerChangeLSHandle {tChangeLS,tChangeItems}) (ls,ps)
						# (done,itemHs,(changeLS,ps)) = aSyncTimerElementHandles rid tChangeItems (tChangeLS,ps)
						= (done,TimerChangeLSHandle {tChangeLS=changeLS,tChangeItems=itemHs},(ls,ps))
					aSyncTimerElementHandle rid itemH=:(TimerReceiverHandle trH=:{tReceiverHandle=rH}) (ls,ps)
						| receiverIdentified rid rH	= (True, TimerReceiverHandle trH1,(ls1,ps1))
						| otherwise					= (False,TimerReceiverHandle trH, (ls, ps ))
					where
						(rH1,(ls1,ps1))				= aSyncReceiverHandle rH (ls,ps)
						trH1						= {trH & tReceiverHandle=rH1}
						
						aSyncReceiverHandle :: !(ReceiverHandle .ls .pst) *(.ls,.pst) -> *(!ReceiverHandle .ls .pst,*(.ls,.pst))
						aSyncReceiverHandle rH=:{rFun,rASMQ=[m:tailQ]} (ls,ps)
							# (ls,_,ps)	= rFun m (ls,ps)
							= ({rH & rASMQ=tailQ},(ls,ps))
						aSyncReceiverHandle _ _
							= timerdeviceFatalError "asyncReceiverHandle" "message queue of target receiver is empty"
				aSyncTimerElementHandles _ _ ps
					= (False,[],ps)
			aSyncTimerStateHandles _ _ ps
				= ([],ps)
	timerIO (ReceiverEvent (SyncMessage event)) pState
		# (event,pState)	= timerSync event pState
		= (ReceiverEvent (SyncMessage event),pState)
	where
		timerSync :: !SyncMessage !(PSt .l) -> (!SyncMessage,!PSt .l)
		timerSync msg=:{smRecLoc={rlReceiverId=rid}} pState
			= (msg1,pState2)
		where
			(_,timers,ioState)	= ioStGetDevice TimerDevice pState.io
			tHs					= timerSystemStateGetTimerHandles timers
			(msg1,tsHs,pState2)	= syncTimerStateHandles rid msg tHs.tTimers pState1
			tHs1				= {tHs & tTimers=tsHs}
			ioState1			= ioStSetDevice (TimerSystemState tHs1) ioState
			pState1				= {pState & io=ioState1}
			
			syncTimerStateHandles :: !Id !SyncMessage ![TimerStateHandle .pst] .pst
									 -> (!SyncMessage,![TimerStateHandle .pst],.pst)
			syncTimerStateHandles rid msg [TimerLSHandle {tState=ls,tHandle=tH=:{tItems}}:tsHs] ps
				# (done,msg,tItems,(ls,ps))	= syncTimerElementHandles rid msg tItems (ls,ps)
				  tsH						= TimerLSHandle {tState=ls,tHandle={tH & tItems=tItems}}
				| done
					= (msg,[tsH:tsHs],ps)
				| otherwise
					# (msg,tsHs,ps)			= syncTimerStateHandles rid msg tsHs ps
					= (msg,[tsH:tsHs],ps)
			where
				syncTimerElementHandles :: !Id !SyncMessage ![TimerElementHandle .ls .pst] (.ls,.pst)
									 -> (!Bool,!SyncMessage,![TimerElementHandle .ls .pst],(.ls,.pst))
				syncTimerElementHandles rid msg [itemH:itemHs] (ls,ps)
					# (done,msg,itemH,(ls,ps))	= syncTimerElementHandle rid msg itemH (ls,ps)
					| done
						= (done,msg,[itemH:itemHs],(ls,ps))
					| otherwise
						# (done,msg,itemHs,(ls,ps))	= syncTimerElementHandles rid msg itemHs (ls,ps)
						= (done,msg,[itemH:itemHs],(ls,ps))
				where
					syncTimerElementHandle :: !Id !SyncMessage !(TimerElementHandle .ls .pst) (.ls,.pst)
										-> (!Bool,!SyncMessage, !TimerElementHandle .ls .pst, (.ls,.pst))
					syncTimerElementHandle rid msg (TimerReceiverHandle trH=:{tReceiverHandle=rH}) (ls,ps)
						| not (receiverIdentified rid rH)
							= (False,msg,TimerReceiverHandle trH,(ls,ps))
						| otherwise
							# (resp,rH,(ls,ps))	= receiverHandleSyncMessage msg rH (ls,ps)
							= (True, {msg & smResp=resp},TimerReceiverHandle {trH & tReceiverHandle=rH},(ls,ps))
					syncTimerElementHandle rid msg (TimerListLSHandle itemHs) (ls,ps)
						# (done,msg,itemHs,(ls,ps))	= syncTimerElementHandles rid msg itemHs (ls,ps)
						= (done,msg,TimerListLSHandle itemHs,(ls,ps))
					syncTimerElementHandle rid msg (TimerElimLSHandle itemHs) (ls,ps)
						# (done,msg,itemHs,(ls,ps))	= syncTimerElementHandles rid msg itemHs (ls,ps)
						= (done,msg,TimerElimLSHandle itemHs,(ls,ps))
					syncTimerElementHandle rid msg (TimerIntroLSHandle {tIntroLS=inLS,tIntroItems}) (ls,ps)
						# (done,msg,itemHs,(inLS,ps))	= syncTimerElementHandles rid msg tIntroItems (inLS,ps)
						= (done,msg,TimerIntroLSHandle {tIntroLS=inLS,tIntroItems=itemHs},(ls,ps))
					syncTimerElementHandle rid msg (TimerExtendLSHandle {tExtendLS,tExtendItems}) (ls,ps)
						# (done,msg,itemHs,((exLS,ls),ps))	= syncTimerElementHandles rid msg tExtendItems ((tExtendLS,ls),ps)
						= (done,msg,TimerExtendLSHandle {tExtendLS=exLS,tExtendItems=itemHs},(ls,ps))
					syncTimerElementHandle rid msg (TimerChangeLSHandle {tChangeLS,tChangeItems}) (ls,ps)
						# (done,msg,itemHs,(chLS,ps))	= syncTimerElementHandles rid msg tChangeItems (tChangeLS,ps)
						= (done,msg,TimerChangeLSHandle {tChangeLS=chLS,tChangeItems=itemHs},(ls,ps))
				syncTimerElementHandles _ msg _ (ls,ps)
					= (False,msg,[],(ls,ps))
			syncTimerStateHandles _ msg _ ps
				= (msg,[],ps)
	timerIO _ _
		= timerdeviceFatalError "timerIO" "device event passed timer event filter without handling"
