definition module processstack


//	********************************************************************************
//	Clean Standard Object I/O library.
//	********************************************************************************


from	StdOverloaded	import class ==
from	systemid		import :: SystemId


::	ProcessShowState
	=	{	psId	:: !SystemId		// the id of the process
		,	psShow	:: !ShowFlag		// flag: True iff the process is visible
		,	psKind	:: !ProcessKind		// the kind of the process
		}
::	ProcessKind		=	InteractiveProcess | VirtualProcess
::	ShowFlag		:==	Bool
::	ProcessStack	:==	[ProcessShowState]

instance == ProcessKind

emptyProcessStack		:: ProcessStack
pushProcessShowState	:: !ProcessShowState	!ProcessStack -> ProcessStack
setProcessShowState		:: !SystemId !Bool		!ProcessStack -> ProcessStack
selectProcessShowState	:: !SystemId			!ProcessStack -> ProcessStack
removeProcessShowState	:: !SystemId			!ProcessStack -> (!ProcessShowState,!ProcessStack)
topShowProcessShowState	::						!ProcessStack -> (!Bool,!SystemId)
