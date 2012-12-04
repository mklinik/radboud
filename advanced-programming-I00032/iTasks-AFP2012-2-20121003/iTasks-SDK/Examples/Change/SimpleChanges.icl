implementation module SimpleChanges

import iTasks

changeExamples :: [Workflow]
changeExamples =
	[ 	workflow "Examples/Changes/Change priority" "Change the priority of a task"  (try changePrio catch)
	,	workflow "Examples/Changes/Add warning" "Add a warning message to a task"  (try changeWarningTask catch)
	,	workflow "Examples/Changes/Duplicate task" "Duplicate a task" (try duplicateTask catch)
	,	workflow "Examples/Changes/Show result" "Show result when task finishes"  (try informTask catch)
	,	workflow "Examples/Changes/Check task when finished" "Wait until a task is finished"  (try checkTask catch)
	,	workflow "Examples/Changes/Cancel task" "Cancel a task" (try cancelTask catch)
 	,	workflow "Examples/Changes/Reassign task" "Reassing the task to another user"  (try reassignTask catch)
 	,	workflow "Examples/Changes/Restart task" "Restart a task from the beginning"  (try restartTask catch)
  	]
where
	catch :: String -> Task Void
	catch message  = viewInformation ("Error!",message) [] Void

//Simple change which will run once and change the priority of all tasks to high
changePriority :: TaskPriority -> ChangeDyn
changePriority priority =
	dynamic change :: A.a: Change a | iTask a
where
	change :: TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change meta t t0 = (Just {TaskInstanceMeta|meta & managementMeta = {meta.TaskInstanceMeta.managementMeta & priority = priority}},Nothing, Just (changePriority priority))

//Add a big red warning message prompt to the running task
addWarning :: String -> ChangeDyn
addWarning msg = 
	dynamic change  :: A.a: Change a | iTask a
where
	change :: TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change props t t0 = (Nothing, Just (((viewInformation ("Warning!",redText msg) [] Void) ||- t)), Just (addWarning msg))

redText msg = [DivTag [StyleAttr "color: red; font-size: 30px;"] [Text msg]]

//This will duplicate a running task n times
duplicate :: User User String -> ChangeDyn
duplicate me user topics =
	dynamic change me user topics :: A.a: Change a | iTask a
where
	change :: User User String TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change me user topics meta t t0 
		= 	( Just {TaskInstanceMeta|meta & managementMeta = {meta.TaskInstanceMeta.managementMeta & worker = Just me}}
			, Just (me @:
							(anyTask 	[ (Description topics @>> fromMaybe me meta.TaskInstanceMeta.managementMeta.ManagementMeta.worker @: t) 
										, (Description topics @>> user @: t)
										]
							)
							<<@ Description ("Duplicated " +++ topics))
			, Nothing )

//inform will inform a user that some process has ended.
inform :: User String -> ChangeDyn
inform user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> appendTopLevelTask {ManagementMeta|noMeta & worker = Just user} (viewInformation ("Process ended","Process " +++ procName +++ " ended!") [] res) >>| return res), Nothing)

//check will pass the result to the indicated user who can change the result in an editor before it passed.
check :: User String -> ChangeDyn
check user procName =
	dynamic change user :: A.a: Change a | iTask a
where
	change :: User TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user props t t0 = (Nothing, Just (t >>= \res -> assign {noMeta & worker = Just user, priority = HighPriority} (updateInformation ("Verification","Please verify result of " +++ procName) [] res)), Nothing)

//cancel stop the process, and give the indicated user the responsibility to fill in the result
cancel ::  String ProcessId -> ChangeDyn
cancel procName pid  =
	dynamic change  :: A.b: Change b | iTask b
where
	change p  t t0 = (Nothing, Just (		removeTask (index pid) topLevelTasks 
										>>| 		return defaultValue
										), Nothing)

	index (WorkflowProcess pid) = pid
	index _						= -1
	
//reassign the work to someone else
reassign :: User String ProcessId -> ChangeDyn
reassign user procName pid  =
	dynamic change user :: A.b: Change b | iTask b
where
	change :: User TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user meta t t0 
		= (Just {TaskInstanceMeta|meta & managementMeta = {meta.TaskInstanceMeta.managementMeta & worker = Just user}},Nothing, Nothing)

//restart starts the task from scratch and assigns it to the indicated user
restart :: User String -> Dynamic
restart user procName =
	dynamic change user procName :: A.a: Change a | iTask a
where
	change :: User String TaskInstanceMeta (Task a) (Task a) -> (Maybe TaskInstanceMeta, Maybe (Task a), Maybe ChangeDyn) | iTask a
	change user procName props t t0 = (Nothing, Just (assign {noMeta & worker = Just user, priority = HighPriority} (Description procName @>> t0)), Nothing)

changePrio :: Task Void
changePrio
	=				chooseProcess "Of which process do you want to change the priority?"			
	>>= \proc -> 	enterInformation ("New priority","What should the new priority be?") []
	>>= \priority ->applyChangeToProcess proc (changePriority priority) (CLPersistent "priority")

changeWarningTask :: Task Void
changeWarningTask
	=				enterInformation ("Warning","Type in warning you want to show to all:") []
	>>= \warning ->	chooseProcess "Which process do you want to change?"			
	>>= \proc ->	applyChangeToProcess proc (addWarning warning) (CLPersistent "warning")

duplicateTask :: Task Void
duplicateTask
	=				chooseProcess "Which process do you want to duplicate?"
	>>= \procId ->	getProcess procId
	>>= \process ->	chooseUserA "Select the user you want to work on it as well:"
	>>= \user ->	get currentUser
	>>= \me ->		applyChangeToProcess procId (duplicate me user process.TaskInstanceMeta.taskMeta.TaskMeta.title) CLTransient


informTask :: Task Void
informTask
	=				chooseProcess "The result of which process do you want to show?"
	>>= \procId ->	chooseUserA "Select the user you want this result to see:"
	>>= \user ->	getProcess procId
	>>= \process ->applyChangeToProcess procId (inform user process.TaskInstanceMeta.taskMeta.TaskMeta.title) CLTransient


checkTask :: Task Void
checkTask
	=				chooseProcess "The result of which process do you want to be checked?"
	>>= \procId ->	getProcess procId
	>>= \process -> chooseUserA "Select the user which has to check it:"
	>>= \user ->	applyChangeToProcess procId (check user process.TaskInstanceMeta.taskMeta.TaskMeta.title) CLTransient
	
cancelTask :: Task Void
cancelTask
	=				chooseProcess "Select the task you want to cancel:"
	>>= \procId ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (cancel process.TaskInstanceMeta.taskMeta.TaskMeta.title procId) CLTransient

reassignTask :: Task Void
reassignTask
	=				chooseProcess "Select the task you want to reassign to someone else:"
	>>= \procId ->	chooseUserA "Who should continue with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (reassign user process.TaskInstanceMeta.taskMeta.TaskMeta.title procId) CLTransient

restartTask :: Task Void
restartTask
	=				chooseProcess "Select the task you want to restart from scratch:"
	>>= \procId ->	chooseUserA "Who should start with this work?"
	>>= \user ->	getProcess procId
	>>= \process -> applyChangeToProcess procId (restart user process.TaskInstanceMeta.taskMeta.TaskMeta.title) CLTransient

//Utility
chooseUserA :: !question -> Task User | html question
chooseUserA question
	=		enterSharedChoice ("Choose user",question) [] users
		>?*	[ (ActionCancel,	Always	(throw "choosing a user has been cancelled"))
			, (ActionOk,		IfValid	(\user -> return user))
			]							

chooseProcess :: String -> Task ProcessId
chooseProcess question
	=				get currentProcesses
	>>= \procs ->	enterChoice question []
					[	( proc.TaskInstanceMeta.processId
						, proc.TaskInstanceMeta.taskMeta.TaskMeta.title
						, proc.TaskInstanceMeta.managementMeta.ManagementMeta.priority
						, proc.TaskInstanceMeta.managementMeta.ManagementMeta.worker)
						\\ proc <- procs]
	>?*				[ (ActionCancel,	Always	(throw "choosing a process has been cancelled"))
					, (ActionOk,		IfValid	(\(pid,_,_,_) -> return pid))
					]

getProcess :: !ProcessId -> Task TaskInstanceMeta
getProcess i 
	=	get currentProcesses
	>>= \list -> 
		case [m \\ m <- list | m.processId == i] of
			[m] = return m
			_	= throw "Could not find process"