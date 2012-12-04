definition module Task

/**
* This module provides types for the definition of tasks & changes.
*/

import SystemTypes, HTTP, GenVisualize, iTaskClass, GenRecord
from TaskState			import :: TaskTree
from LayoutCombinators	import :: Layout

derive JSONEncode		Task
derive JSONDecode		Task
derive gUpdate			Task
derive gVerify			Task
derive gVisualizeText	Task
derive gVisualizeEditor	Task
derive gHeaders			Task
derive gGridRows		Task
derive gEq				Task
derive gGetRecordFields	Task
derive gPutRecordFields	Task

// Tasks
:: Task a = Task !(Event TaskRepOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!TaskId !String !JSONNode	//Update something in an interaction: Task id, edit name, value
			| ActionEvent	!TaskId !String				//Progress in a step combinator: Task id, action id
			| FocusEvent	!TaskId						//Update last event time without changing anything: Task id
			| RefreshEvent								//No event, just recalcalutate the entire task instance
			

:: TaskResult a		= ValueResult !(TaskValue a) !TaskInfo !TaskRep !TaskTree							//If all goes well, a task computes its current value, an observable representation and a new task state
					| ExceptionResult !Dynamic !String													//If something went wrong, a task produces an exception value
					| DestroyedResult																	//If a task finalizes and cleaned up it gives this result

:: TaskInfo =
	{ lastEvent			:: TaskTime		//When was the last edit, action or focus event in this task
//	, lastValueChange	:: TaskTime 	//When was the last time this task's value changed
	, expiresIn			:: Maybe Int	//Guideline for the maximum amount of time to wait before automatically refreshing (in milliseconds)	
	}

:: TaskRepOpts	=
	{ useLayout			:: Maybe Layout
	, afterLayout		:: Maybe (UIDef -> UIDef)
	, modLayout			:: Maybe (Layout -> Layout)
	, appFinalLayout	:: Bool
	}
	
:: TaskRep			= TaskRep !UIDef !TaskServiceRep	//Compute both the UI and the raw service representation simultaneously

//Task representation for web service format
:: TaskServiceRep	:== [TaskPart]

//Summary of the composition structure of tasks (used as input for layouting)
:: TaskCompositionType
	= ViewPart
	| SingleTask
	| SequentialComposition
	| ParallelComposition

:: TaskPart			:== (!String, !JSONNode)		//Task id, value

/**
* Creates an execption result
*/
exception :: !e -> TaskResult a | TC, toString e

/**
* Determine the layout function for a rep target
*/
repLayout :: TaskRepOpts -> Layout

/**
* Determine what function to apply after a layout has been done
*/
afterLayout :: TaskRepOpts -> (UIDef -> UIDef)
/**
* Apply the final layout if necessary
*/
finalizeRep :: TaskRepOpts TaskRep -> TaskRep

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a

//* Provides fmap for Task Values
instance Functor TaskValue

