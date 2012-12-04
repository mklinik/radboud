module BasicCombinatorsTest
/**
* This module tests the combinators in BasicCombinators.dcl
*/
import iTasks, iData, StdOverloaded, StdInt, StdClass, StdList
import BasicCombinators, CommonCombinators, PromptingCombinators

Start :: *World -> *World
Start world = startEngine tests world
where
	tests = [ 	{ name		= "returnbind_test"
		  		, label		= "return_V and =>>"
		  		, roles		= []
		  		, mainTask	= returnbind_test
		  		}
		  	,	{ name		= "assignTaskTo_test"
		  		, label		= "assignTaskTo"
		  		, roles		= []
		  		, mainTask	= assignTaskTo_test
		  		}
		  	,	{ name		= "foreverTask_test"
		  		, label		= "foreverTask"
		  		, roles		= []
		  		, mainTask	= foreverTask_test
		  		}
		  	,	{ name		= "loop_tst"
		  		, label		= "loop (&lt;!)"
		  		, roles		= []
		  		, mainTask	= loop_test
		  		}
		  	,	{ name		= "seqTasks_test"
		  		, label		= "seqTasks"
		  		, roles		= []
		  		, mainTask	= seqTasks_test
		  		}
		  	,	{ name		= "selectTasks_test"
		  		, label		= "selectTasks"
		  		, roles		= []
		  		, mainTask	= selectTasks_test
		  		}
		  	,	{ name		= "allTasksCond_test"
		  		, label		= "allTasksCond"
		  		, roles		= []
		  		, mainTask	= allTasksCond_test
		  		}
		  	]

returnbind_test :: Task Void
returnbind_test
	= return_V 42 =>> \value -> displayHtml [Text (toString value)]
	
assignTaskTo_test :: Task Void
assignTaskTo_test
	= assignTaskTo 0 ("Message for root", displayHtml [Text "This message is for root only"])

foreverTask_test :: Task Void
foreverTask_test
	= (foreverTask 
		( editTask "Task 1" 0 =>> \a ->
		  editTask "Task 2" 0 =>> \b ->
		  editTask "Sum" (a + b)
		 )
	  ) #>> return_V Void

loop_test :: Task Void
loop_test
	= ( (editTask "Ok" 0) <! (\x -> x > 10) ) #>> return_V Void
	
seqTasks_test :: Task Void
seqTasks_test
	= seqTasks [(toString i, editTask "Ok" i) \\ i <- [1..3]] =>> \list -> displayValue list
	
selectTasks_test :: Task Void
selectTasks_test
	= selectTasks doEvenTasks seqTasks [(toString i, editTask "Ok" i) \\ i <- [0..2]] #>> return_V Void  
where
	doEvenTasks tasks = return_V [i \\ task <- tasks & i <- [0..] | isEven i]
	
allTasksCond_test :: Task Void
allTasksCond_test
	= allTasksCond "Do all of these:" (TTSplit []) pred [("Subtask " +++ (toString i), editTask "Ok" i) \\ i <- [1..4]] #>> return_V Void
where
	pred list = length list > 0