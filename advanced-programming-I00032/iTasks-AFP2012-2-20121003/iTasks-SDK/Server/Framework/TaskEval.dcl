definition module TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from SystemTypes	import :: IWorld, :: TaskListItem, :: User, :: TaskId, :: SessionId 
from Task			import :: Task, :: TaskResult, :: Event, :: TaskRepOpts
from Shared			import :: Shared

import Maybe, JSON_NG, Error
import TaskState, iTaskClass

/**
* Create a new task instance without evaluating it by an existing instance number
*
* @param The instance number
* @param Optionally the session id
* @param The parent instance number
* @param Optionally the worker use
* @param Optionally, the session id
* @param ManagementMeta
* @param ProgressMeta
* @param The IWorld state
*
* @return The created task instance
* @return The IWorld state
*/
createTaskInstance :: !InstanceNo !(Maybe SessionId) !InstanceNo !(Maybe User) !(Task a) !ManagementMeta !ProgressMeta !*IWorld -> (!TaskInstance, !*IWorld) | iTask a

/**
* Create a new session task instance and evaluate its immediately
*
* @param The task to run as session
* @param An event
* @param The IWorld state
*
* @return The result of the targeted main task and the tasknr of the instance or an error
* @return The IWorld state
*/
createSessionInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld) |  iTask a

/**
* Evaluate a session task instance
*
* @param The session id
* @param An event
* @param The IWorld state
* 
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalSessionInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld)

/**
* Create a stored task instance in the task pool (lazily without evaluating it)
* @param The task to store
* @param Management meta data
* @param The user who issued the task
* @param The parent instance that created the instance
* @param The IWorld state
*
* @return The task id of the stored instance
* @return The IWorld state
*/
createPersistentInstance :: !(Task a) !ManagementMeta !User !InstanceNo !*IWorld -> (!TaskId, !*IWorld) | iTask a

//Helper functions that provide access to shares and parallel task lists
localShare		:: !TaskId ->	Shared a			| iTask a
topListShare	::				SharedTaskList a
parListShare	:: !TaskId ->	SharedTaskList a	| iTask a