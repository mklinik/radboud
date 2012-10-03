definition module CoreCombinators
/**
* This is the kernel module for the specification of workflows. It contains the core set of iTasks combinators
* with which additional combinators can be defined.
*/
from Time				import :: Timestamp
from LayoutCombinators	import :: SetLayout, :: AfterLayout, :: ModifyLayout, :: Layout
import Task, Shared

import iTaskClass
derive class iTask ParallelTaskType, WorkOnStatus
/**
* Adds a result transformation function to a task.
* The resulting task is still considered a single step in the workflow.
*
* @param Function: The transformation function. It works on maybe's to also map over instable tasks.
* @param Task: The task to which the transformation function is added
*
* @return The transformed task
*/
transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 

/**
* Projects the result of a task in a share when its result changes.
* The resulting task is still considered a single step in the workflow.
*
* @param The projection function
* @param The share onto which the result should be projected
* @param The task that provides the result

* @return The modified task
*/
project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a

/**
* The generic sequential combinator.
* It does a task followed by one out of a given list of continuations.
* Once the transition to the continuation has been made it cannot be reversed.
*
* @param Task: The first step in the sequence
* @param Continuations: A set of continuation definitions from which one is selected
*   -OnValue: inspect the value, step if the predicate matches
*	-OnAction: enable an action if the predicate matches, step if the actions is chosen
*	-OnException: Provides an exception handler for exceptions of type e
*	-OnAllExceptions: Provides an exception handler that catches all exceptions
*
*	@return The combined task
*/
step :: !(Task a) [TaskStep a b] -> Task b | iTask a & iTask b

:: TaskStep a b
	= 		OnValue 			((TaskValue a) -> Bool)	((TaskValue a)	-> Task b)
	|		OnAction	Action	((TaskValue a) -> Bool)	((TaskValue a)	-> Task b)
	| E.e:	OnException									(e				-> Task b) & iTask e
	| 		OnAllExceptions								(String			-> Task b)

/**
* All-in-one swiss-army-knife parallel task creation
*
* @param Description: The (overloaded) task description
* @param Tasks: The list of tasks to run in parallel, each task is given a view on the status of all tasks in the set
* @return The sum of all results
* 
* @gin False
*/
parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
					
/**
* Get the shared state of a task list
*/
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
/**
* Get the properties share of a task list
*/
taskListMeta	:: !(SharedTaskList a) -> ReadOnlyShared [TaskListItem a]

//Task list manipulation 

/**
* Appends a task to a task list
*/
appendTask :: !ParallelTaskType !(ParallelTask a)	!(SharedTaskList a) -> Task TaskId | iTask a
/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId								!(SharedTaskList a)	-> Task Void | iTask a

/**
* State of another process the user works on.
*/
:: WorkOnStatus
	= WOActive		//* the process is active, the current user works on it
	| WOInUse User	//* the process is active, another user is working on it
	| WOFinished	//* the process is finished
	| WOExcepted	//* an uncaught exception was thrown inside of the process
	| WODeleted		//* the process has been deleted

/**
* Work on a detached task.
*
* @param Task identification of the task to work on
* 
* @return The state of the task to work on
* @throws WorkOnException
*/
workOn :: !TaskId -> Task WorkOnStatus

/**
* Execute a task with the identity of the given user
*
* @param The user with which identity the task is to be executed
* @param The task to do
*
* @return The modified task
*/
workAs :: !User !(Task a)						-> Task a | iTask a

/**
* Provide a local read/write shared for a task to work on.
*
* @param The initial value of the shared variable
* @param The task which uses the shared variable
*/
withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b

/**
* Fine tune a task by specifying custom layouts, tweaking generic layouts,
* or add additional titles, hints and descriptions
*/
class tune b :: !b !(Task a) -> Task a
instance tune	SetLayout				//Set layout algorithm
instance tune	AfterLayout				//Apply a modification after a layout has been run
instance tune	ModifyLayout			//Modify the existing layout
