definition module scheduler


//	********************************************************************************
//	Clean Standard Object I/O library.
//	
//	Process creation, termination, and handling functions.
//	********************************************************************************

import	StdString
import	deviceevents, StdMaybe
import	osevent, ostime, ostoolbox
from	id				import :: Id
from	iostate			import :: PSt, :: CProcess, :: CProcesses, :: RR
from	receivertable	import :: ReceiverTable
from	processstack	import :: ProcessKind, :: ProcessStack, :: ProcessShowState
from	systemid		import :: SystemId
from	StdIOCommon		import :: ProcessAttribute, :: IdFun
from	StdProcessDef	import :: DocumentInterface, :: ProcessInit


::	*Environs
	=	{	envsEvents		:: !*OSEvents
		,	envsWorld		:: !*World
		}
::	*Context	// DvA: can be made abstract if we move modalDialogExists from windowcreate to here...
	=	{	cEnvs			:: !*Environs			// The global environments
		,	cProcessStack	:: !ProcessStack		// The global process stack
		,	cMaxIONr		:: !SystemId			// The global maximum system number
		,	cProcesses		:: !*CProcesses			// All processes
		,	cModalProcess	:: !Maybe SystemId		// The SystemId of the interactive process that has a modal window
		,	cReceiverTable	:: !*ReceiverTable		// The global receiver-process table
		,	cTimerTable		:: !*TimerTable			// The table of all currently active timers
		,	cIdTable		:: !*IdTable			// The table of all bound Ids
		,	cOSTime			:: !OSTime				// The current OSTime
		,	cIdSeed			:: !Int					// The global id generating number (actually the World)
		,	cOSToolbox		:: !*OSToolbox			// The toolbox environment
		}

initContext		:: !*(ProcessInit (PSt .l)) !String !.l !DocumentInterface !ProcessKind !*World -> (!Context,!*OSToolbox)
/*	Generate an initial Context, given the initial actions, state, and (document/process)kind of an interactive
	process, and World.
*/
initContext`	:: !*World -> (!Context,!*OSToolbox)

handleEvents	:: !Context !*OSToolbox -> (!Context,!*OSToolbox)
/*	Consecutively handle events until termination of all interactive processes.
*/

chandleEvents :: !(St Context Bool) !Context !*OSToolbox -> (!Context,!*OSToolbox)
/*	chandleEvents consecutively handles events while condition holds.
*/

handleContextOSEvent :: !OSEvent !Context -> (![Int],!Context)
/*	Handle one OSEvent. Context MUST not be a final context, otherwise a runtime error occurs!
*/

closeContext	:: !Context !*OSToolbox -> *World
/*	Retrieve the final World value from the Context. Context MUST be a final context, otherwise a runtime error occurs!
*/


handleOneEventForDevices :: !SchedulerEvent !(PSt .l) -> (!Bool,!SchedulerEvent,!PSt .l)
/*	Apply the given event to all devices in the current interactive process.
	The Boolean result indicates whether one of the devices actually handled the event.
	The DeviceEvent result may contain a response of type [o], or an error indication.
*/


addVirtualProcess		:: !*(ProcessInit (PSt .l)) String .l !(PSt .l`) -> PSt .l`
/*	addVirtualProcess adds a process to the process administration as a virtual process.
	For virtual processes no initial devices are created. If the initial actions of the
	virtual process attempt to create a device instance this results in a runtime error.
	Virtual processes always share the GUI of their parent process. Termination is
	detected and handled by the system.
*/

ShareGUI			:==	True
NotShareGUI			:==	False

addInteractiveProcess	:: ![ProcessAttribute (PSt .l)] !*(ProcessInit (PSt .l)) String .l 
							!Bool !DocumentInterface !(PSt .l`) -> PSt .l`
/*	addInteractiveProcess adds a process to the process administration as an interactive process.
	For interactive processes initial devices are created. 
	The Boolean argument determines whether the process shares (ShareGUI) or not shares 
	(NotShareGUI) the GUI of the current process. If it does, then the current process
	becomes the parent process of the new process, and the new process its child.
*/


quitProcess			:: !(PSt .l) -> PSt .l
/*	QuitProcess removes all abstract devices that are held in the interactive process
	and takes care that all processes that share its GUI are also terminated recursively.
	The resulting IOSt is a terminal value and will be removed from the process administration.
*/


//	Context-switching operations:

::	SwitchError
	=	SwitchToYourself
	|	SwitchToDoesNotExist
	|	SwitchToReceiverDoesNotExist
	|	SwitchReceiverUnable
	|	SwitchEndsUpInDeadlock

cswitchProcess :: !SystemId !SchedulerEvent !(PSt .l) -> (!Maybe SwitchError, ![SemiDynamic], !PSt .l)
/*	cswitchProcess processId msgEvent pstate switches to PSt identified by processId, lets that process 
	handle the msgEvent, and switches back to pstate.
	Nothing SwitchError is returned if no exceptions were detected. 
	Exceptions are:
	-	SwitchToYourself:				processId is the current PSt. 
	-	SwitchToDoesNotExist:			no current process can be identified by processId. 
	-	SwitchToReceiverDoesNotExist:	process  exists, but receiver doesn't.
	-	SwitchReceiverUnable:			receiver exists, but is not Able.
	-	SwitchEndsUpInDeadlock:			if this process would block for the process with 
										id processId to get unblocked, a deadlock would occur.
	In all exceptional cases the PSt remains unchanged.
	Observe that in case Nothing is returned, the ps PSt component may have changed value.
*/

appContext :: !.(IdFun Context) !(PSt .l) -> PSt .l
accContext :: !.(St Context .x) !(PSt .l) -> (!.x, !PSt .l)
/*	(app/acc)Context applies the Context access function to the switched out process state (which creates a
	Context value), and switches back to the process state. The Context access function must not
	remove the argument process, otherwise a run-time error will occur!
*/


//	Function threading operations:

::	Result r
	:==	(	!Bool			//	object is found
		,	!Maybe r		//	optional access information
		)

accessLocals :: !(St CProcess (Result r)) !*CProcesses -> (!Result r,!*CProcesses)
/*	Let f::(IOSt .l .p) -> (Result r,IOSt .l .p) be an IOSt access function. 
	To thread f through *CProcesses until fst(fst(f io)), define gLocals as follows:
	
		gLocals :: *CProcesses -> (Result r, *CProcesses)
		gLocals locals = accessLocals f` locals
		where	f` localIO	= (r,{localIO & localIOSt=ioState})
				where	(r,ioState)	= f localIO.localIOSt
*/
