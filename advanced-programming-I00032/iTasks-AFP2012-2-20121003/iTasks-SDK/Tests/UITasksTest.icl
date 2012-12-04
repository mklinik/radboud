 module UITasksTest
/**
* This module tests the basic tasks in UITasks.dcl
*/
import iTasks, iData
import UITasks, BasicCombinators, CommonCombinators


Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "editTask_test"
		  		, label		= "editTask"
		  		, roles		= []
		  		, mainTask	= editTask_test
		  		}
		  	,	{ name		= "editTaskPred_test"
		  		, label		= "editTaskPred"
		  		, roles		= []
		  		, mainTask	= editTaskPred_test
		  		}
		  	,	{ name		= "displayHtml_test"
		  		, label		= "displayHtml"
		  		, roles		= []
		  		, mainTask	= displayHtml_test
		  		}
		  	,	{ name		= "displayValue_test"
		  		, label		= "displayValue"
		  		, roles		= []
		  		, mainTask	= displayValue_test
		  		}
		  	,	{ name		= "selectWithButtons_test"
		  		, label		= "selectWithButtons"
		  		, roles		= []
		  		, mainTask	= selectWithButtons_test
		  		}
		  	,	{ name		= "selectWithPulldown_test"
		  		, label		= "selectWithPulldown"
		  		, roles		= []
		  		, mainTask	= selectWithPulldown_test
		  		}
		  	,	{ name		= "selectWithRadiogroup_test"
		  		, label		= "selectWithRadiogroup"
		  		, roles		= []
		  		, mainTask	= selectWithRadiogroup_test
		  		}
		  	,	{ name		= "selectWithCheckboxes_test"
		  		, label		= "selectWithCheckboxes"
		  		, roles		= []
		  		, mainTask	= selectWithCheckboxes_test
		  		}
		  	]
		 
editTask_test :: Task Void
editTask_test = editTask_test` >>| return Void
where
	editTask_test` :: Task (Int,String)
	editTask_test` = editTask "I'm done" createDefault

editTaskPred_test :: Task Void
editTaskPred_test = editTaskPred_test` >>| return Void 
where
	editTaskPred_test` :: Task Int
	editTaskPred_test` = editTaskPred createDefault pred
	
	pred :: Int -> (Bool,[HtmlTag])
	pred i = (i > 10, [Text "Enter a number higher than 10"])
	
displayHtml_test :: Task Void
displayHtml_test = displayHtml [H1Tag [] [Text "This is displayed by displayHtml"]]

displayValue_test :: Task Void
displayValue_test = displayValue 42

selectWithButtons_test :: Task Void
selectWithButtons_test
	= selectWithButtons ["A","B","C"] >>= \choice -> displayValue choice

selectWithPulldown_test :: Task Void
selectWithPulldown_test
	= selectWithPulldown ["A","B","C"] 1 >>= \choice -> displayValue choice
	
selectWithRadiogroup_test :: Task Void
selectWithRadiogroup_test
	= selectWithRadiogroup [[Text "A"],[Text "B"],[Text "C"]] 1 >>= \choice -> displayValue choice

selectWithCheckboxes_test :: Task Void
selectWithCheckboxes_test
	= selectWithCheckboxes options >>= \choices -> displayValue choices
where
	options = [([Text "A"],True,(\_ cur -> cur))
			  ,([Text "B"],False,(\_ cur -> cur))
			  ,([Text "Both"],False,(\set cur -> [set \\ _ <- cur]))
			  ] 	