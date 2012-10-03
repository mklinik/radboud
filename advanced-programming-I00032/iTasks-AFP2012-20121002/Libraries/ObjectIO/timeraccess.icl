implementation module timeraccess


import	StdBool, StdTuple
import	devicesystemstate, timerhandle, timertable
from commondef import fatalError, stateMap2


timeraccessFatalError :: String String -> .x
timeraccessFatalError function error
	= fatalError function "timeraccess" error


/*	bindTimerElementIds binds all unbound R(2)Ids and Ids that can be located in the list of TimerElementStates.
	The Boolean result is True only if no bound identification was found, otherwise it is False.
*/
bindTimerElementIds :: !SystemId !Id !*[*TimerElementHandle .ls .pst] !*ReceiverTable !*IdTable
						   -> (!Bool,!*[*TimerElementHandle .ls .pst],!*ReceiverTable,!*IdTable)
bindTimerElementIds pid timerid [itemH:itemHs] rt it
	# (ok,itemH,rt,it)		= bindTimerElementIds` pid timerid itemH rt it
	| not ok
		= (ok,[itemH:itemHs],rt,it)
	| otherwise
		# (ok,itemHs,rt,it)	= bindTimerElementIds pid timerid itemHs rt it
		= (ok,[itemH:itemHs],rt,it)
where
	bindTimerElementIds` :: !SystemId !Id !*(TimerElementHandle .ls .pst) !*ReceiverTable !*IdTable
								-> (!Bool, !*TimerElementHandle .ls .pst, !*ReceiverTable,!*IdTable)
	bindTimerElementIds` pid timerid (TimerReceiverHandle itemH=:{tReceiverHandle=trH}) rt it
		# (member,it)		= memberIdTable rid it
		| member
			= (False,TimerReceiverHandle itemH,rt,it)
		# (maybeRTE,rt)		= getReceiverTableEntry rid rt
		| isJust maybeRTE	// This situation should not occur: the IdTable didn't contain the id while the ReceiverTable does.
			= timeraccessFatalError "bindTimerElementIds" "inconsistency detected between IdTable and ReceiverTable"
			//(False,TimerReceiverHandle itemH,rt,it)
		| otherwise
			# rl			= {rlIOId=pid,rlDevice=TimerDevice,rlParentId=timerid,rlReceiverId=rid}
			  rte			= {rteLoc=rl,rteSelectState=trH.rSelect,rteASMCount=0}
			  (_,rt)		= addReceiverToReceiverTable rte rt
			  (_,it)		= addIdToIdTable rid {idpIOId=pid,idpDevice=TimerDevice,idpId=timerid} it
			= (True,TimerReceiverHandle itemH,rt,it)
	where
		rid					= trH.rId
	bindTimerElementIds` pid timerid (TimerListLSHandle itemHs) rt it
		# (ok,itemHs,rt,it) = bindTimerElementIds pid timerid itemHs rt it
		= (ok,TimerListLSHandle itemHs,rt,it)
	bindTimerElementIds` pid timerid (TimerElimLSHandle itemHs) rt it
		# (ok,itemHs,rt,it) = bindTimerElementIds pid timerid itemHs rt it
		= (ok,TimerElimLSHandle itemHs,rt,it)
	bindTimerElementIds` pid timerid (TimerIntroLSHandle tInH=:{tIntroItems}) rt it
		# (ok,itemHs,rt,it) = bindTimerElementIds pid timerid tIntroItems rt it
		= (ok,TimerIntroLSHandle {tInH & tIntroItems=itemHs},rt,it)
	bindTimerElementIds` pid timerid (TimerExtendLSHandle tExH=:{tExtendItems}) rt it
		# (ok,itemHs,rt,it) = bindTimerElementIds pid timerid tExtendItems rt it
		= (ok,TimerExtendLSHandle {tExH & tExtendItems=itemHs},rt,it)
	bindTimerElementIds` pid timerid (TimerChangeLSHandle tChH=:{tChangeItems}) rt it
		# (ok,itemHs,rt,it) = bindTimerElementIds pid timerid tChangeItems rt it
		= (ok,TimerChangeLSHandle {tChH & tChangeItems=itemHs},rt,it)
bindTimerElementIds _ _ [] rt it
	= (True,[],rt,it)


/*	unbindTimerElementIds unbinds all bound R(2)Ids and Ids that can be located in the list of TimerElementStates.
*/
unbindTimerElementIds :: !SystemId !*[*TimerElementHandle .ls .pst] !(!*TimerTable,!*ReceiverTable,!*IdTable)
																  -> (!*TimerTable,!*ReceiverTable,!*IdTable)
unbindTimerElementIds pid itemHs tables
	= stateMap2 unbindTimerElementIds` itemHs tables
where
	unbindTimerElementIds` :: !*(TimerElementHandle .ls .pst) !(!*TimerTable,!*ReceiverTable,!*IdTable)
															-> (!*TimerTable,!*ReceiverTable,!*IdTable)
	unbindTimerElementIds` (TimerReceiverHandle {tReceiverHandle={rId}}) (tt,rt,idtable)
		= (snd (removeTimerFromTimerTable teLoc tt),snd (removeReceiverFromReceiverTable rId rt),snd (removeIdFromIdTable rId idtable))
	where
		teLoc	= {tlIOId=pid,tlDevice=TimerDevice,tlParentId=rId,tlTimerId=rId}
	unbindTimerElementIds` (TimerListLSHandle tListItems) tables
		= stateMap2 unbindTimerElementIds` tListItems tables
	unbindTimerElementIds` (TimerElimLSHandle tElimItems) tables
		= stateMap2 unbindTimerElementIds` tElimItems tables
	unbindTimerElementIds` (TimerIntroLSHandle {tIntroItems}) tables
		= stateMap2 unbindTimerElementIds` tIntroItems tables
	unbindTimerElementIds` (TimerExtendLSHandle {tExtendItems}) tables
		= stateMap2 unbindTimerElementIds` tExtendItems tables
	unbindTimerElementIds` (TimerChangeLSHandle {tChangeItems}) tables
		= stateMap2 unbindTimerElementIds` tChangeItems tables

identifyTimerStateHandle :: !Id !*(TimerStateHandle .pst) -> *(!Bool,!*TimerStateHandle .pst)
identifyTimerStateHandle id tlsH=:(TimerLSHandle {tHandle={tId}})
	= (id==tId,tlsH)
