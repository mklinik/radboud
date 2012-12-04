definition module tmdialog

from	StdId	import :: Id, class Ids
from	StdPSt	import :: PSt
from	StdIOCommon import :: IdFun
import	tm

HelpFile
	:== "TuringHelp"

::	TmIds
	=	{	windowID		:: Id
		,	tapeWdID		:: Id
		,	fileMenuId		:: Id
		,	saveItemId		:: Id
		,	machineMenuId	:: Id
		,	stepItemId		:: Id
		,	haltItemId		:: Id
		}
::	Tm
	=	{	tmstate			:: !TmState
		,	name			:: !String
		,	delay			:: !Int
		,	saved			:: !Bool
		,	tmids			:: !TmIds
		}


openTmIds		:: !*env -> (!TmIds,!*env)	| Ids env

AlterCell		:: Int						(PSt Tm) -> PSt Tm
AlterTransition	:: Int						(PSt Tm) -> PSt Tm
AlterState		::							(PSt Tm) -> PSt Tm
ReDraw			::							(PSt Tm) -> PSt Tm
Alert			:: String String			(PSt Tm) -> PSt Tm
SaveBeforeClose	:: String (IdFun (PSt Tm))	(PSt Tm) -> PSt Tm
