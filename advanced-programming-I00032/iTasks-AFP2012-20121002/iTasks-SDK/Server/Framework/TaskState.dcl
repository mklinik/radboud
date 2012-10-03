definition module TaskState

import SystemTypes

from Task		import :: TaskTime, :: TaskResult, :: TaskRep
from GenUpdate	import :: UpdateMask

derive JSONEncode TIMeta, TIReduct, TIResult, TaskTree
derive JSONDecode TIMeta, TIReduct, TIResult, TaskTree

//Persistent context of active tasks
//Split up version of task instance information
:: TaskInstance :== (!TIMeta,!TIReduct,!TIResult,!TIRep)

:: TIMeta =
	{ instanceNo	:: !InstanceNo		//Unique global identification
	, sessionId		:: !Maybe SessionId	//zero for top-level instances, instance that detached this one otherwise
	, parent		:: !InstanceNo		
	, observers		:: ![InstanceNo]	//List of instances that may be affected by changes in this instance
	, worker		:: !Maybe User		//Identity of the user working on this instance (this determines the value of the currentUser share)
	, progress		:: !ProgressMeta
	, management	:: !ManagementMeta
	}
	
:: TIReduct = 
	{ task			:: !Task JSONNode
	, nextTaskNo	:: !TaskNo
	, nextTaskTime	:: !TaskTime
	, tree			:: !TaskTree						//Internal task tree state
	, shares		:: !Map TaskId JSONNode				//Locally shared data
	, lists			:: !Map TaskId [TaskListEntry]		//Parallel task lists
	}

:: TIResult
	= TIValue !(TaskValue JSONNode) !TaskTime
	| TIException !Dynamic !String
	
:: TIRep :== TaskRep

:: TaskTree
	= TCInit		!TaskId !TaskTime													//Initial state for all tasks
	| TCBasic		!TaskId !TaskTime !JSONNode !Bool 									//Encoded value and stable indicator
	| TCInteract	!TaskId !TaskTime !JSONNode !JSONNode !JSONNode !UpdateMask
	| TCInteract1	!TaskId !TaskTime !JSONNode !UpdateMask
	| TCInteract2	!TaskId !TaskTime !JSONNode !JSONNode !UpdateMask
	| TCProject		!TaskId !JSONNode !TaskTree
	| TCStep		!TaskId !TaskTime !(Either TaskTree (DeferredJSON,Int,TaskTree))
	| TCParallel	!TaskId !TaskTime
	| TCShared		!TaskId !TaskTime !TaskTree
	| TCStable		!TaskId !TaskTime !DeferredJSON
	| TCNop			
	| TCDestroy		!TaskTree															//Marks a task state as garbage that must be destroyed

:: DeferredJSON
	= E. a:	DeferredJSON !a & TC a & JSONEncode{|*|} a
	|		DeferredJSONNode !JSONNode

derive JSONEncode DeferredJSON
derive JSONDecode DeferredJSON
	
:: TaskListEntry	=
	{ entryId			:: !TaskId					//Identification of entries in the list (for easy updating)
	, state				:: !TaskListEntryState		//Tree if embedded, or instance no if detached
	, result			:: !TIResult				//Stored result of last evaluation (for detached tasks this is a cached copy)
	, attributes		:: !Map String String		//Stored attributes of last evaluation
	, createdAt			:: !TaskTime				//Time the entry was added to the set (used by layouts to highlight new items)
	, lastEvent			:: !TaskTime				//Last modified time
	, expiresIn			:: !Maybe Int				//Optional expiration advice (in ms)
	, removed			:: !Bool					//Flag for marking this entry as 'removed', actual removal is done by the controlling parallel combinator
	}

:: TaskListEntryState
	= EmbeddedState !Dynamic !TaskTree 							//The task definition, task tree and last computed attributes
	| DetachedState !InstanceNo !ProgressMeta !ManagementMeta	//A reference to the detached task (management and progress meta are cached copies)
