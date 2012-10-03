implementation module CoreCombinators

import StdList, StdTuple, StdMisc, StdBool, StdOrdList
import Task, TaskState, TaskStore, TaskEval, Util, HTTP, GenUpdate, GenEq_NG, Store, SystemTypes, Time, Text, Shared, Func, Tuple, List_NG
import iTaskClass, InteractionTasks, LayoutCombinators, UIDefinition
import ClientOverride

from Map				import qualified get, put, del, newMap, toList, fromList
from StdFunc			import id, const, o, seq
from IWorld				import :: IWorld(..)
from iTasks				import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode
from TaskEval			import localShare, parListShare, topListShare
from CoreTasks			import return
from SharedDataSource	import write, read

derive class iTask ParallelTaskType, WorkOnStatus

noRep = TaskRep (UIControlSequence ('Map'.newMap,[],Vertical)) []

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{currentInstance,nextTaskNo} = (TaskId currentInstance nextTaskNo, {IWorld|iworld & nextTaskNo = nextTaskNo + 1})

transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b | iTask a & iTask b 
transform f (Task evala) = Task eval
where
	eval event repOpts tree iworld = case evala event repOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= (ValueResult (f val) lastEvent rep tree, iworld)	//TODO: guarantee stability
		(ExceptionResult e str, iworld)				= (ExceptionResult e str, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

project	:: ((TaskValue a) r -> Maybe w) (ReadWriteShared r w) !(Task a) -> Task a | iTask a
project projection share (Task evala) = Task eval
where
	eval event repOpts (TCDestroy (TCProject taskId encprev treea)) iworld	//Cleanup duty simply passed to inner task
		= evala event repOpts (TCDestroy treea) iworld

	eval event repOpts state iworld
		# (taskId,prev,statea) = case state of
			(TCInit taskId _)					= (taskId,NoValue,state) 
			(TCProject taskId encprev statea)	= (taskId,fromJust (fromJSON encprev),statea)
			
		# (resa, iworld) 	= evala event repOpts statea iworld
		= case resa of
			ValueResult val ts rep ncxta
				# result = ValueResult val ts rep (TCProject taskId (toJSON val) ncxta)
				| val =!= prev
					= projectOnShare val result iworld
				| otherwise
					= (result,iworld)
			ExceptionResult e str
				= (ExceptionResult e str,iworld)
	
	projectOnShare val result iworld
		# (er, iworld) = read share iworld
		= case er of
			Ok r = case projection val r of
				Just w
					# (ew, iworld) = write w share iworld
					= case ew of
						Ok _	= (result, iworld)
						Error e	= (exception e, iworld)
				Nothing = (result, iworld)
			Error e = (exception e, iworld)

step :: !(Task a) [TaskStep a b] -> Task b | iTask a & iTask b
step (Task evala) conts = Task eval
where
	eval event repOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event repOpts (TCStep taskId ts (Left (TCInit taskIda ts))) iworld

	//Eval left-hand side
	eval event repOpts (TCStep taskId ts (Left treea)) iworld=:{taskTime}
		# (resa, iworld) 	= evala event {repOpts & appFinalLayout = False} treea iworld
		# ts				= case event of
							(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
		# mbcommit			= case event of
			(ActionEvent t action)
				| t == taskId 		= Just action
			_						= Nothing
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbcommit conts of
				Nothing			
					# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
					= Left (ValueResult NoValue info (doStepLayout taskId repOpts rep (Just val)) (TCStep taskId info.TaskInfo.lastEvent (Left ntreea)) )
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskInfo.lastEvent)
			ExceptionResult e str = case searchContException e str conts of
				Nothing			= Left (ExceptionResult e str)
				Just rewrite	= Right (rewrite,Nothing, ts)		//TODO: Figure out how to garbage collect after exceptions
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					Just treea	= snd (evala RefreshEvent {repOpts & appFinalLayout = False} (TCDestroy treea) iworld) //TODO: Check for exceptions during cleanup
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb RefreshEvent {repOpts & appFinalLayout = False} (TCInit taskIdb lastEvent) iworld 
				= case resb of
					ValueResult val info rep nstateb	
						# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
						= (ValueResult val info (doStepLayout taskId repOpts rep Nothing) (TCStep taskId info.TaskInfo.lastEvent (Right (d_json_a,sel,nstateb))),iworld)
					ExceptionResult e str				= (ExceptionResult e str, iworld)
	//Eval right-hand side
	eval event repOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{taskTime}
		# ts				= case event of
							(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb event {repOpts & appFinalLayout = False} treeb iworld 
				= case resb of
					ValueResult val info rep ntreeb
						# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
						= (ValueResult val info (doStepLayout taskId repOpts rep Nothing) (TCStep taskId info.TaskInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e str			= (ExceptionResult e str, iworld)
			Nothing
				= (exception "Corrupt task value in step", iworld) 	
	
	//Cleanup
	eval event repOpts (TCDestroy (TCStep taskId ts (Left treea))) iworld
		= case evala event repOpts (TCDestroy treea) iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e str,iworld)	= (ExceptionResult e str,iworld)
			(ValueResult _ _ _ _,iworld)	= (exception "Destroy failed in step",iworld)
	
	eval event repOpts (TCDestroy (TCStep taskId ts (Right(enca,sel,treeb)))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)	= evalb event repOpts (TCDestroy treeb) iworld
			Nothing				= (exception "Corrupt task value in step", iworld)
			
	//Incorred state
	eval event _ state iworld
		= (exception ("Corrupt task state in step:" +++ (toString (toJSON state))), iworld)

	searchContValue val mbcommit conts = search val mbcommit 0 Nothing conts
	where
		search _ _ _ mbmatch []							= mbmatch									//No matching OnValue steps were found, return the potential match
		search val mbcommit i mbmatch [OnValue pred f:cs]
			| pred val									= Just (i, f val, DeferredJSON val)				//Don't look any further, first matching trigger wins
														= search val mbcommit (i + 1) mbmatch cs	//Keep search
		search val (Just commit) i Nothing [OnAction action pred f:cs]
			| pred val && commit == actionName action	= search val (Just commit) (i + 1) (Just (i, f val, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
														= search val (Just commit) (i + 1) Nothing cs								//Keep searching
		search val mbcommit i mbmatch [_:cs]			= search val mbcommit (i + 1) mbmatch cs									//Keep searching
		
	searchContException dyn str conts = search dyn str 0 Nothing conts
	where
		search _ _ _ catchall []					= catchall														//Return the maybe catchall
		search dyn str i catchall [OnException f:cs] = case (match f dyn) of
			Just (taskb,enca)						= Just (i, taskb, enca)											//We have a match
			_										= search dyn str (i + 1) catchall cs							//Keep searching
		search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
		search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs							//Keep searching
				
		match :: (e -> Task b) Dynamic -> Maybe (Task b, DeferredJSON) | iTask e
		match f (e :: e^)	= Just (f e, DeferredJSON e)
		match _ _			= Nothing 
	
	restoreTaskB sel d_json_a = case conts !! sel of
		(OnValue _ taskbf)			= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnAction _ _ taskbf)		= call_with_DeferredJSON_TaskValue taskbf d_json_a
		(OnException taskbf)		= call_with_DeferredJSON taskbf d_json_a
		(OnAllExceptions taskbf)	= call_with_DeferredJSON taskbf d_json_a
	
	doStepLayout taskId repOpts (TaskRep def parts) mbVal 
		= finalizeRep repOpts (TaskRep ((repLayout repOpts).Layout.step def (maybe [] (stepActions taskId) mbVal)) parts)
	where
		stepActions taskId val = [{UIAction|taskId=toString taskId,action=action,enabled=pred val}\\ OnAction action pred _ <- conts]

	call_with_DeferredJSON_TaskValue :: ((TaskValue a) -> Task .b) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
	call_with_DeferredJSON_TaskValue f_tva_tb d_json_tva=:(DeferredJSON tva)
        = Just (f_tva_tb (cast_to_TaskValue tva))
	
	call_with_DeferredJSON_TaskValue f_tva_tb (DeferredJSONNode json)
		= case fromJSON json of
			Just a ->  Just (f_tva_tb a)
			Nothing -> Nothing
	
	call_with_DeferredJSON :: (a -> Task .b) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
    call_with_DeferredJSON f_tva_tb d_json_tva=:(DeferredJSON tva)
        = Just (f_tva_tb (cast tva))

	call_with_DeferredJSON f_tva_tb (DeferredJSONNode json)
		= case fromJSON json of
			Just a ->  Just (f_tva_tb a)
			Nothing -> Nothing

// Parallel composition
parallel :: !d ![(!ParallelTaskType,!ParallelTask a)] -> Task [(!TaskTime,!TaskValue a)] | descr d & iTask a
parallel desc initTasks = Task eval 
where
	//Create initial task list
	eval event repOpts (TCInit taskId ts) iworld=:{IWorld|localLists}
		//Append the initial tasks to the list 
		# iworld	= foldl append {iworld & localLists = 'Map'.put taskId [] localLists} initTasks
		//Evaluate the parallel
		= eval event repOpts (TCParallel taskId ts) iworld
	where
		append iworld t = snd (appendTaskToList taskId t iworld)

	//Evaluate the task list
	eval event repOpts (TCParallel taskId ts) iworld=:{taskTime}
		//Evaluate all parallel tasks in the list
		= case evalParTasks taskId event iworld of
			(Just res=:(ExceptionResult e str),_,iworld)	= (res,iworld)
			(Just res=:(ValueResult _ _ _ _),_,iworld)		= (exception "parallel evaluation yielded unexpected result",iworld)
			(Nothing,entries,iworld)
				//Filter out removed entries and destroy their state
				# (removed,entries)	= splitWith (\({TaskListEntry|removed},_) -> removed) entries
				= case foldl destroyParTask (Nothing,iworld) (map fst removed) of
					(Just (ExceptionResult e str),iworld)	= (ExceptionResult e str,iworld)	//An exception occurred	
					(Just result,iworld)					= (fixOverloading result initTasks (exception "Destroy failed in step"),iworld)
					(Nothing,iworld=:{localLists})
						//Destruction is ok, build parallel result
						# rep				= parallelRep desc taskId repOpts entries
						# expiry			= parallelExpiry entries
						# values			= map (toValueAndTime o fst) entries 
						# stable			= if (all (isStable o snd) values) Stable Unstable
						# ts				= foldr max 0 [ts:map fst values]
						# ts				= case event of
							(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
							_						= ts
						= (ValueResult (Value values stable) {TaskInfo|lastEvent=ts,expiresIn=expiry} (finalizeRep repOpts rep) (TCParallel taskId ts),{iworld & localLists = 'Map'.put taskId (map fst entries) localLists})
	//Cleanup
	eval event repOpts (TCDestroy (TCParallel taskId ts)) iworld=:{localLists}
		# entries = fromMaybe [] ('Map'.get taskId localLists)
		= case foldl destroyParTask (Nothing,iworld) entries of
			(Nothing,iworld)						= (DestroyedResult,{iworld & localLists = 'Map'.del taskId localLists})			//All destroyed
			(Just (ExceptionResult e str),iworld)	= (ExceptionResult e str,{iworld & localLists = 'Map'.del taskId localLists})	//An exception occurred	
			(Just result,iworld)					= (fixOverloading result initTasks (exception "Destroy failed in step"),iworld)
	//Fallback
	eval _ _ _ iworld
		= (exception "Corrupt task state in parallel", iworld)
	
	evalParTasks :: !TaskId !Event !*IWorld -> (!Maybe (TaskResult [(TaskTime,TaskValue a)]),![(!TaskListEntry,!Maybe TaskRep)],!*IWorld) | iTask a
	evalParTasks taskId event iworld=:{localLists}
		= evalFrom 0 [] (fromMaybe [] ('Map'.get taskId localLists)) iworld
	where
		evalFrom n acc list iworld = case foldl (evalParTask taskId event) (Nothing,acc,iworld) (drop n list) of
			(Just (ExceptionResult e str),acc,iworld)	= (Just (ExceptionResult e str),acc,iworld)
			(Nothing,acc,iworld=:{localLists})			
				# nlist = fromMaybe [] ('Map'.get taskId localLists)
				| length nlist > length list	= evalFrom (length list) acc nlist iworld	//Extra branches were added -> evaluate these as well 
												= (Nothing,acc,iworld)					//Done
			//IMPORTANT: This last rule should never match, but it helps to solve overloading solves overloading
			(Just (ValueResult val info=:{TaskInfo|lastEvent} rep tree),acc,iworld) = (Just (ValueResult (Value [(lastEvent,val)] Unstable) info rep tree),acc,iworld)
	
	evalParTask :: !TaskId !Event !(!Maybe (TaskResult a),![(!TaskListEntry,!Maybe TaskRep)],!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),![(!TaskListEntry,!Maybe TaskRep)],!*IWorld) | iTask a
	//Evaluate embedded tasks
	evalParTask taskId event (Nothing,acc,iworld) {TaskListEntry|entryId,state=EmbeddedState (Task evala :: Task a^) tree, removed=False}
		# (result,iworld) = evala event {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=False} tree iworld
		= case result of
			ExceptionResult _ _
				= (Just result,acc,iworld)	
			ValueResult val ts rep tree
				# (entry,iworld)	= updateListEntryEmbeddedResult taskId entryId result iworld
				= (Nothing, acc++[(entry,Just rep)],iworld)
					
	//Copy the last stored result of detached tasks
	evalParTask taskId event (Nothing,acc,iworld) {TaskListEntry|entryId,state=DetachedState instanceNo _ _,removed=False}
		= case loadTaskInstance instanceNo iworld of
			(Error _, iworld)	= (Nothing,acc,iworld)	//TODO: remove from parallel if it can't be loaded (now it simply keeps the last known result)
			(Ok (meta,_,res), iworld)
				# fixme = []	//TODO: Get the attributes from detached tasks
				# (entry,iworld) = updateListEntryDetachedResult taskId entryId res meta.TIMeta.progress meta.TIMeta.management fixme iworld
				= (Nothing,acc++[(entry,Nothing)],iworld)

	//Do nothing if an exeption occurred or marked as removed
	evalParTask taskId event (result,acc,iworld) entry = (result,acc,iworld) 

	destroyParTask :: (!Maybe (TaskResult a),!*IWorld) !TaskListEntry -> (!Maybe (TaskResult a),!*IWorld) | iTask a
	//Destroy embedded tasks
	destroyParTask (_,iworld) {TaskListEntry|entryId,state=EmbeddedState (Task evala :: Task a^) tree}
		# (result,iworld) = evala RefreshEvent {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=False} (TCDestroy tree) iworld
		= case result of
			DestroyedResult		= (Nothing,iworld)
			_					= (Just result,iworld)
	
	//Destroy detached tasks (Just delete the instance)
	destroyParTask (_,iworld) {TaskListEntry|entryId,state=DetachedState instanceNo _ _}
		= (Nothing,deleteTaskInstance instanceNo iworld)
		
	toValueAndTime :: !TaskListEntry -> (!TaskTime,TaskValue a) | iTask a
	toValueAndTime {TaskListEntry|result=TIValue val _,lastEvent}	= (lastEvent,deserialize val)	
	where
		deserialize (Value json stable) = case fromJSON json of
			Nothing = NoValue
			Just a	= Value a stable
		deserialize NoValue	= NoValue
	toValueAndTime {TaskListEntry|lastEvent}						= (lastEvent,NoValue)
	
	parallelRep :: !d !TaskId !TaskRepOpts ![(!TaskListEntry,!Maybe TaskRep)] -> TaskRep | descr d
	parallelRep desc taskId repOpts entries
		# layout		= repLayout repOpts
		# after			= afterLayout repOpts
		# listId		= toString taskId
		# parts = [(uiDefSetAttribute LAST_EVENT_ATTRIBUTE (toString lastEvent) (uiDefSetAttribute CREATED_AT_ATTRIBUTE (toString createdAt) (uiDefSetAttribute TASK_ATTRIBUTE (toString entryId) def)))
					 \\ ({TaskListEntry|entryId,state=EmbeddedState _ _,result=TIValue val _,createdAt,lastEvent,removed=False},Just (TaskRep def _)) <- entries | not (isStable val)]	
		= TaskRep (after (layout.Layout.parallel (toPrompt desc) parts)) []
	
	parallelExpiry :: [(!TaskListEntry,!Maybe TaskRep)] -> Maybe Int
	parallelExpiry entries = minimum [exp \\ ({TaskListEntry|expiresIn=Just exp},_) <- entries]
	where
		//If we have multiple tasks in parallel, the lowest expiry determines the expiry of the set
		minimum []	= Nothing
		minimum [e]	= Just e
		minimum [e:es] = let (Just mines) = minimum es in Just (if (e < mines) e mines)

	isStable (Value _ Stable) 	= True
	isStable _					= False
	
	//Helper function to help type inferencing a little
	fixOverloading :: (TaskResult a) [(!ParallelTaskType,!ParallelTask a)] !b -> b
	fixOverloading _ _ x = x
							
//SHARED HELPER FUNCTIONS
appendTaskToList :: !TaskId !(!ParallelTaskType,!ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
appendTaskToList taskId=:(TaskId parent _) (parType,parTask) iworld=:{taskTime,currentUser,currentDateTime}
	# (list,iworld) = loadTaskList taskId iworld 
	# (taskIda,state,iworld) = case parType of
		Embedded
			# (taskIda,iworld)	= getNextTaskId iworld
			# task		= parTask (parListShare taskId)
			= (taskIda, EmbeddedState (dynamic task :: Task a^) (TCInit taskIda taskTime),iworld)
		Detached management
			# task									= parTask (parListShare taskId)
			# progress								= {issuedAt=currentDateTime,issuedBy=currentUser,status=Unstable,firstEvent=Nothing,latestEvent=Nothing}
			# (taskIda=:TaskId instanceNo _,iworld)	= createPersistentInstance task management currentUser parent iworld
			= (taskIda,DetachedState instanceNo progress management, iworld)
	# result	= TIValue NoValue taskTime
	# entry		= {entryId = taskIda, state = state, result = result, attributes = 'Map'.newMap, createdAt = taskTime, lastEvent = taskTime, expiresIn = Nothing, removed = False}
	# iworld	= storeTaskList taskId (list ++ [entry]) iworld
	= (taskIda, iworld)		

updateListEntryEmbeddedResult :: !TaskId !TaskId (TaskResult a) !*IWorld -> (!TaskListEntry,!*IWorld) | iTask a
updateListEntryEmbeddedResult listId entryId result iworld
	= updateListEntry listId entryId (\e=:{TaskListEntry|state,lastEvent} ->
		{TaskListEntry|e & state = newTree state result, result = serialize result, attributes = newAttr result, lastEvent = maxTime lastEvent result, expiresIn = expiresIn result}) iworld
where
	serialize (ValueResult val {TaskInfo|lastEvent} _ _) 		= TIValue (fmap toJSON val) lastEvent
	serialize (ExceptionResult e str)							= TIException e str

	newTree (EmbeddedState task _) (ValueResult _ _ _ tree)		= EmbeddedState task tree
	newTree cur _												= cur
	
	newAttr (ValueResult _ _ (TaskRep def _) _)					= uiDefAttributes def
	newAttr _													= 'Map'.newMap
	
	maxTime cur (ValueResult _ {TaskInfo|lastEvent} _ _)		= max cur lastEvent
	maxTime cur _												= cur

	expiresIn (ValueResult _ {TaskInfo|expiresIn} _ _)			= expiresIn
	expiresIn _													= Nothing

updateListEntryDetachedResult :: !TaskId !TaskId TIResult !ProgressMeta !ManagementMeta !TaskMeta !*IWorld -> (!TaskListEntry,!*IWorld)
updateListEntryDetachedResult listId entryId result progress management attributes iworld
	= updateListEntry listId entryId update iworld
where
	update e=:{TaskListEntry|state=DetachedState no _ _}
		= {TaskListEntry| e & state = DetachedState no progress management,result = result, attributes = 'Map'.fromList attributes}
	update e = e

markListEntryRemoved :: !TaskId !TaskId !*IWorld -> *IWorld
markListEntryRemoved listId entryId iworld
	= snd (updateListEntry listId entryId (\e -> {TaskListEntry|e & removed = True}) iworld)

updateListEntry :: !TaskId !TaskId !(TaskListEntry -> TaskListEntry) !*IWorld -> (!TaskListEntry,!*IWorld)
updateListEntry listId entryId f iworld
	# (list,iworld) = loadTaskList listId iworld
	# list			= [if (e.TaskListEntry.entryId == entryId) (f e) e \\ e <- list]	//TODO: MERGE AND OPTIZE WITH ITEM SEARCH
	# [item:_]		= [e \\ e <- list | (e.TaskListEntry.entryId == entryId)]
	# iworld		= storeTaskList listId list iworld
	= (item,iworld)

loadTaskList :: !TaskId !*IWorld -> (![TaskListEntry],!*IWorld)	
loadTaskList taskId=:(TaskId instanceNo taskNo) iworld=:{currentInstance,localLists}
	| instanceNo == currentInstance
		= (fromMaybe [] ('Map'.get taskId localLists),iworld)
	| otherwise
		= case loadTaskReduct instanceNo iworld of
			(Ok {TIReduct|lists},iworld)	= (fromMaybe [] ('Map'.get taskId lists),iworld)
			(_,iworld)						= ([],iworld)

storeTaskList :: !TaskId ![TaskListEntry] !*IWorld -> *IWorld	
storeTaskList taskId=:(TaskId instanceNo taskNo) list iworld=:{currentInstance,localLists}
	| instanceNo == currentInstance
		= {iworld & localLists = 'Map'.put taskId list localLists}
	| otherwise
		= case loadTaskReduct instanceNo iworld of
			(Ok reduct=:{TIReduct|lists},iworld)	= storeTaskReduct instanceNo {TIReduct|reduct & lists = 'Map'.put taskId list lists} iworld
			(_,iworld)								= iworld
			
readListId :: (SharedTaskList a) *IWorld -> (MaybeErrorString (TaskListId a),*IWorld)	| iTask a
readListId slist iworld = case read slist iworld of
	(Ok l,iworld)		= (Ok l.TaskList.listId, iworld)
	(Error e, iworld)	= (Error e, iworld)

//Derived shares
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a]
taskListState tasklist = mapRead (\{TaskList|items} -> [value \\ {TaskListItem|value} <- items]) tasklist

taskListMeta :: !(SharedTaskList a) -> ReadOnlyShared [TaskListItem a]
taskListMeta tasklist = mapRead (\{TaskList|items} -> items) tasklist

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# (taskIda,iworld) = append listId parType parTask iworld
				= (Ok taskIda, iworld)
			(Error e,iworld)
				= (Error (dynamic e,e), iworld)
								
	append :: !(TaskListId a) !ParallelTaskType !(ParallelTask a) !*IWorld -> (!TaskId,!*IWorld) | iTask a
	append TopLevelTaskList parType parTask iworld=:{currentUser}
		# meta						= case parType of Embedded = noMeta; Detached meta = meta;
		# task						= parTask topListShare
		= createPersistentInstance task meta currentUser 0 iworld
	append (ParallelTaskList parId) parType parTask iworld
		= appendTaskToList parId (parType,parTask) iworld

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task Void | iTask a
removeTask entryId slist = mkInstantTask eval
where
	eval taskId iworld=:{taskTime}
		= case readListId slist iworld of
			(Ok listId,iworld)
				# iworld = remove listId entryId iworld
				= (Ok Void, iworld)
			(Error e,iworld)
				= (Error (dynamic e,e), iworld)

	remove :: !(TaskListId a) !TaskId !*IWorld -> *IWorld
	remove TopLevelTaskList (TaskId instanceNo 0) iworld
		= deleteTaskInstance instanceNo iworld
	remove (ParallelTaskList parId) entryId iworld
		= markListEntryRemoved parId entryId iworld
	remove _ _ iworld = iworld

workOn :: !TaskId -> Task WorkOnStatus
workOn (TaskId instanceNo taskNo) = Task eval
where
	eval event repOpts (TCInit taskId ts) iworld=:{currentInstance,currentUser}
		# iworld = setTaskWorker currentUser instanceNo iworld
		# iworld = addTaskInstanceObserver currentInstance instanceNo iworld
		= eval event repOpts (TCBasic taskId ts JSONNull False) iworld
		
	eval event repOpts tree=:(TCBasic taskId ts _ _) iworld=:{currentUser}
		//Load instance
		# (meta,iworld)		= loadTaskMeta instanceNo iworld
		# (result,iworld)	= loadTaskResult instanceNo iworld
		# (rep,iworld)		= loadTaskRep instanceNo iworld
		# layout			= repLayout repOpts
		= case (meta,result,rep) of
			(_,Ok (TIValue (Value _ Stable) _),_)
				= (ValueResult (Value WOFinished Stable) {TaskInfo|lastEvent=ts,expiresIn=Nothing} (finalizeRep repOpts noRep) tree, iworld)
			(_,Ok (TIException _ _),_)
				= (ValueResult (Value WOExcepted Stable) {TaskInfo|lastEvent=ts,expiresIn=Nothing} (finalizeRep repOpts noRep) tree, iworld)
			(Ok meta=:{TIMeta|worker=Just worker},_,Ok (TaskRep def parts))
				| worker == currentUser
					# rep = finalizeRep repOpts (TaskRep (layout.Layout.workOn def meta) parts)
					= (ValueResult (Value WOActive Unstable) {TaskInfo|lastEvent=ts,expiresIn=Nothing} rep tree, iworld)
				| otherwise
					# rep = finalizeRep repOpts (TaskRep (layout.Layout.workOn (inUseDef worker) meta) parts)
					= (ValueResult (Value (WOInUse worker) Unstable) {TaskInfo|lastEvent=ts,expiresIn=Nothing} rep tree, iworld)		
			_
				= (ValueResult (Value WODeleted Stable) {TaskInfo|lastEvent=ts,expiresIn=Nothing} (finalizeRep repOpts noRep) tree, iworld)

	eval event repOpts (TCDestroy (TCBasic taskId _ _ _)) iworld
		//TODO: Remove this workon from the observers
		= (DestroyedResult,iworld)
		
	inUseDef worker
		= UIControlSequence ('Map'.newMap,[(stringDisplay (toString worker +++ " is working on this task"),'Map'.newMap)],Vertical)
/*
* Alters the evaluation functions of a task in such a way
* that before evaluation the currentUser field in iworld is set to
* the given user, and restored afterwards.
*/
workAs :: !User !(Task a) -> Task a | iTask a
workAs user (Task eval) = Task eval`
where
	eval` event repOpts state iworld=:{currentUser}
		# (result,iworld) = eval event repOpts state {iworld & currentUser = user}
		= (result,{iworld & currentUser = currentUser})

withShared :: !b !((Shared b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task eval
where	
	eval event repOpts (TCInit taskId ts) iworld=:{localShares}
		# localShares				= 'Map'.put taskId (toJSON initial) localShares
		# (taskIda,iworld)			= getNextTaskId iworld
		= eval event repOpts (TCShared taskId ts (TCInit taskIda ts)) {iworld & localShares = localShares}
		
	eval event repOpts (TCShared taskId ts treea) iworld=:{taskTime}
		# ts						= case event of
			(FocusEvent focusId)	= if (focusId == taskId) taskTime ts
			_						= ts
		# (Task evala)				= stask (localShare taskId)
		# (resa,iworld)				= evala event repOpts treea iworld
		= case resa of
			ValueResult NoValue info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult NoValue info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ValueResult (Value stable val) info rep ntreea
				# info = {TaskInfo|info & lastEvent = max ts info.TaskInfo.lastEvent}
				= (ValueResult (Value stable val) info rep (TCShared taskId info.TaskInfo.lastEvent ntreea),iworld)
			ExceptionResult e str								= (ExceptionResult e str,iworld)
	
	eval event repOpts (TCDestroy (TCShared taskId ts treea)) iworld //First destroy inner task, then remove shared state
		# (Task evala)					= stask (localShare taskId)
		# (resa,iworld=:{localShares})	= evala event repOpts (TCDestroy treea) iworld
		= (resa,{iworld & localShares = 'Map'.del taskId localShares})
	
	eval _ _ _ iworld
		= (exception "Corrupt task state in withShared", iworld)	
/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a

instance tune SetLayout
where
	tune (SetLayout layout) (Task eval)	= Task eval`
	where
		eval` event repOpts=:{useLayout=Nothing,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just ((fromMaybe id modLayout) layout), modLayout = Nothing} state iworld 
		eval` event repOpts=:{useLayout=Just _,modLayout} state iworld
			= eval event {TaskRepOpts|repOpts & useLayout = Just layout, modLayout = Nothing} state iworld 
	
instance tune AfterLayout
where
	tune (AfterLayout f) (Task eval) = Task eval`
	where
		eval` event repOpts=:{afterLayout=Nothing} state iworld
			= eval event {TaskRepOpts|repOpts & afterLayout = Just f} state iworld 
		eval` event repOpts=:{afterLayout=Just g} state iworld
			= eval event {TaskRepOpts|repOpts & afterLayout = Just (g o f)} state iworld
		
instance tune ModifyLayout
where
	tune (ModifyLayout f) (Task eval)	= Task eval`
	where
		eval` event repOpts=:{modLayout=Nothing} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just f} state iworld 
		eval` event repOpts=:{modLayout=Just g} state iworld
			= eval event {TaskRepOpts|repOpts & modLayout = Just (g o f)} state iworld 	
	