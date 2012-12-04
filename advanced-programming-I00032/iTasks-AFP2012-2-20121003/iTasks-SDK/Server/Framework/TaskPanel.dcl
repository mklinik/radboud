definition module TaskPanel

import JSON, TUIDefinition, Types, TaskTree
from Task import :: TaskResult
from TUIDiff import :: TUIUpdate

:: TaskPanel
	= TaskDone
	| TaskNotDone
	| TaskRedundant
	| TUIPanel !TUIPanel

:: TUIPanel = 
	{ content		:: !(Maybe TUIDef)
	, updates		:: !(Maybe [TUIUpdate])
	, menu			:: ![TUIDef]
	}

buildTaskPanel 		:: !(TaskResult a)			-> TaskPanel
buildResultPanel 	:: !(TaskResult a)			-> TaskPanel
diffTaskPanels		:: !TaskPanel !TaskPanel	-> TaskPanel
