implementation module processstack


import	StdBool, StdOverloaded, StdString
import	commondef, systemid


::	ProcessShowState
	=	{	psId	:: !SystemId		// the id of the process
		,	psShow	:: !ShowFlag		// flag: True iff the process is visible
		,	psKind	:: !ProcessKind		// the kind of the process
		}
::	ProcessKind		=	InteractiveProcess | VirtualProcess
::	ShowFlag		:==	Bool
::	ProcessStack	:==	[ProcessShowState]


processstackFatalError :: String String -> .x
processstackFatalError rule error
	= fatalError rule "processstack" error


instance == ProcessKind where
	(==) InteractiveProcess kind	= case kind of
										InteractiveProcess	-> True
										_					-> False
	(==) VirtualProcess		kind	= case kind of
										VirtualProcess		-> True
										_					-> False
	
emptyProcessStack :: ProcessStack
emptyProcessStack = []

pushProcessShowState :: !ProcessShowState !ProcessStack -> ProcessStack
pushProcessShowState idVisible psStack = [idVisible:psStack]

selectProcessShowState :: !SystemId !ProcessStack -> ProcessStack
selectProcessShowState id psStack
	= [idVisible:psStack`]
where
	(idVisible,psStack`) = removeProcessShowState id psStack

removeProcessShowState :: !SystemId !ProcessStack -> (!ProcessShowState, !ProcessStack)
removeProcessShowState id [idVisible=:{psId}:psStack]
	| id==psId	= (idVisible, psStack)
	| otherwise	= (idVisible`,[idVisible:psStack`])
	with
		(idVisible`,psStack`)	= removeProcessShowState id psStack
removeProcessShowState _ _
	= processstackFatalError "removeProcessShowState" "Unknown SystemId"

setProcessShowState :: !SystemId !Bool !ProcessStack -> ProcessStack
setProcessShowState id show [idVisible=:{psId}:psStack]
	| id==psId	= [{idVisible & psShow=show}:psStack]
	| otherwise	= [idVisible:setProcessShowState id show psStack]
setProcessShowState _ _ _
	= processstackFatalError "setProcessShowState" "Unknown SystemId"

topShowProcessShowState :: !ProcessStack -> (!Bool,!SystemId)
topShowProcessShowState [{psId,psShow}:psStack]
	| psShow	= (psShow,psId)
	| otherwise	= topShowProcessShowState psStack
topShowProcessShowState _
	= (False,nullSystemId)
