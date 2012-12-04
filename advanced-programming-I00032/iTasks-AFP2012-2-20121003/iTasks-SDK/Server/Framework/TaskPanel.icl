implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util, TUIDiff

buildTaskPanel :: !(TaskResult a) -> TaskPanel
//buildTaskPanel cont=:(TTContainer menu tree) = case tree of
buildTaskPanel (TaskFinished _)	= TaskDone
/*

	TTFinishedTask _ _ False
		= TaskDone
	TTFinishedTask _ _ True
		= buildResultPanel cont
	_
		= TUIPanel
			{ TUIPanel
			| content 		= Just (buildTaskPanel` tree menu)
			, updates 		= Nothing	
			, menu			= menu
			}
*/
where
	buildTaskPanel` :: !UITree ![TUIDef] -> TUIDef
	buildTaskPanel` tree menu = case tree of
		TTInteractionTask {TaskInfo|title,description,type,isControlTask,localInteraction,interactionLayout=l=:TIInteractionLayoutMerger layout} (editor,buttons,warning)
			= layout	{ TUIInteraction
						| title				= title
						, description		= description
						, editorParts		= editor
						, buttons			= buttons
						, type				= type
						, isControlTask		= isControlTask
						, localInteraction	= localInteraction
						, warning			= warning
						}
		TTParallelTask {TaskInfo|title,description,parallelLayout=l=:TIParallelLayoutMerger layout} containers
			= layout	{ TUIParallel
						| title			= title
						, description	= description
						, items			= map buildParallelElement containers
						}
	where
		buildParallelElement :: !UIParallelTreeContainer -> TUIDef
		buildParallelElement (TTParallelContainer _ type tree) = case type of
			TTWindow _ menu	= buildTaskPanel` tree menu
			TTDialog _		= buildTaskPanel` tree []
			TTInBody		= buildTaskPanel` tree []
									
buildResultPanel :: !(TaskResult a) -> TaskPanel
buildResultPanel (TaskFinished _) = undef
/*
buildResultPanel tree = case tree of 
	TTContainer _ (TTFinishedTask ti result _)
		= (TUIPanel	{ TUIPanel
					| content 		= Just (content ti result)
					, updates 		= Nothing	
					, menu			= []
					})
	_
		= TaskNotDone
*/
where
	content {TaskInfo|title,description,resultLayout=l=:TIResultLayoutMerger layout} result
		= layout	{ TUIResult
					| title			= title
					, description	= description
					, result		= htmlDisplay (toString result)
					}
		
diffTaskPanels :: !TaskPanel !TaskPanel -> TaskPanel
diffTaskPanels (TUIPanel old) (TUIPanel new)
	= TUIPanel {new & content = Nothing, updates = Just (diffTUIDefinitions (fromJust old.TUIPanel.content) (fromJust new.TUIPanel.content))}
diffTaskPanels _ new
	= new
