definition module WorkflowAdmin
/**
* This extension provides workflows for managing the users of an iTask system.
*/
import iTasks

// A workflow specification
:: Workflow	=
	{ path				:: String					//* a unique name of this workflow
	, roles				:: [String]					//* the roles that are allowed to initate this workflow
	, description		:: String					//* a description of the workflow
	, managerProperties	:: ManagementMeta			//* the initial manager properties of the main task
	, task				:: WorkflowTaskContainer	//* the thread of the main task of the workflow
	}						
:: WorkflowTaskContainer
	= E.a:		WorkflowTask		(Task a)		& iTask a
	| E.a b:	ParamWorkflowTask	(a -> (Task b))	& iTask a & iTask b
				
derive gVisualizeText	Workflow, WorkflowTaskContainer
derive gVisualizeEditor	Workflow, WorkflowTaskContainer
derive gHeaders			Workflow, WorkflowTaskContainer
derive gGridRows		Workflow, WorkflowTaskContainer
derive gUpdate			Workflow, WorkflowTaskContainer
derive gVerify			Workflow, WorkflowTaskContainer
derive JSONEncode		Workflow, WorkflowTaskContainer
derive JSONDecode		Workflow, WorkflowTaskContainer
derive gEq				Workflow, WorkflowTaskContainer

// Available workflows
:: WorkflowFolderLabel :== String

workflows				:: Shared [Workflow]
allowedWorkflows		:: ReadOnlyShared [Workflow]
workflowTree			:: ReadOnlyShared (Tree (Either WorkflowFolderLabel Workflow))
allowedWorkflowTree		:: ReadOnlyShared (Tree (Either WorkflowFolderLabel Workflow))
workflowByPath			:: !String -> Shared Workflow

/**
* Wraps any task as a workflow with no access restrictions
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param The task(container) (with or without parameter)
*/
workflow :: String String w -> Workflow | toWorkflow w
/**
*
* Wraps any task as a workflow that is only available to specified roles
*
* @param A label for the workflow. This may contain slashes to group workflows
* @param A description of the workflow
* @param A list of roles. The workflow will be available to users with any of the specified roles
* @param The task(container) (with or without parameter)
*/
restrictedWorkflow :: String String [Role] w -> Workflow | toWorkflow w

class toWorkflow w :: String String [Role] !w -> Workflow

instance toWorkflow (Task a)						| iTask a
instance toWorkflow (WorkflowContainer a)			| iTask a
instance toWorkflow (a -> Task b)					| iTask a & iTask b
instance toWorkflow (ParamWorkflowContainer a b)	| iTask a & iTask b

:: WorkflowContainer a			= Workflow		ManagementMeta (Task a)
:: ParamWorkflowContainer a b	= ParamWorkflow	ManagementMeta (a -> Task b)

/**
* Default workflow management task.
* This task allows users to manage a catalogue of task definitions
* and let's them create instances of these tasks and work on instances.
*/
manageWorkflows :: ![Workflow] ->  Task Void

manageWorklist :: ![Workflow] -> Task Void

/**
* Dynamically adds a workflow to the system.
*
* @param Workflow: The workflow to add
* @return The description of the added workflow
* 
* @gin False
*/
addWorkflows :: ![Workflow] -> Task [Workflow]

isAllowedWorkflow :: !User !Workflow -> Bool

//Service tasks
viewTaskList	:: Task [TaskListItem Void]
viewTask		:: Task WorkOnStatus

//The default external services
externalTaskInterface :: [PublishedTask]