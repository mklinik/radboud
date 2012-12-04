implementation module TaskEval

import StdList, StdBool
import Error
import SystemTypes, IWorld, Shared, Task, TaskState, TaskStore, Util
import LayoutCombinators

from CoreCombinators	import :: ParallelTaskType(..), :: ParallelTask(..)

from Map				import qualified newMap, fromList, toList, get, put
import iTaskClass

createTaskInstance :: !InstanceNo !(Maybe SessionId) !InstanceNo !(Maybe User) !(Task a) !ManagementMeta !ProgressMeta !*IWorld -> (!TaskInstance, !*IWorld) | iTask a
createTaskInstance instanceNo sessionId parent worker task mmeta pmeta iworld=:{taskTime} 
	# meta		= {TIMeta|instanceNo=instanceNo,sessionId=sessionId,parent=parent,worker=worker,observers=[],management=mmeta,progress=pmeta}
	# reduct	= {TIReduct|task=toJSONTask task,nextTaskNo=2,nextTaskTime=1,tree=(TCInit (TaskId instanceNo 0) 1),shares = 'Map'.newMap, lists = 'Map'.newMap}
	# result	= TIValue NoValue taskTime
	# rep		= (TaskRep (UIControlGroup ('Map'.newMap, [(stringDisplay "This task has not been evaluated yet.",'Map'.newMap)],Vertical,[])) [])
	= ((meta,reduct,result,rep),iworld)
where
	toJSONTask (Task eval) = Task eval`
	where
		eval` event repOpts tree iworld = case eval event repOpts tree iworld of
			(ValueResult val ts rep tree,iworld)	= (ValueResult (fmap toJSON val) ts rep tree, iworld)
			(ExceptionResult e str,iworld)			= (ExceptionResult e str,iworld)

createSessionInstance :: !(Task a) !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld) |  iTask a
createSessionInstance task event iworld=:{currentDateTime}
	# (sessionId,iworld)	= newSessionId iworld
	# (instanceId,iworld)	= newInstanceId iworld
	# worker				= AnonymousUser sessionId
	# ((meta,reduct,result,_), iworld)
		= createTaskInstance instanceId (Just sessionId) 0 (Just worker) task noMeta {issuedAt=currentDateTime,issuedBy=worker,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} iworld
	# (mbRes,iworld)		= evalAndStoreInstance True event (meta,reduct,result) iworld
	# iworld				= refreshOutdatedInstances iworld 
	= case loadSessionInstance sessionId iworld of
		(Ok (meta,reduct,result),iworld)
			# (mbRes,iworld)	= evalAndStoreInstance True RefreshEvent (meta,reduct,result) iworld
			= case mbRes of
				Ok result	= (Ok (result, instanceId, sessionId), iworld)
				Error e		= (Error e, iworld)
		(Error e, iworld)
			= (Error e, iworld)

evalSessionInstance :: !SessionId !Event !*IWorld -> (!MaybeErrorString (!TaskResult JSONNode, !InstanceNo, !SessionId), !*IWorld)
evalSessionInstance sessionId event iworld
	//Set session user
	# iworld				= {iworld & currentUser = AnonymousUser sessionId}
	//Update current datetime in iworld
	# iworld				= updateCurrentDateTime iworld
	//Evaluate the instance at which the event is targeted or refresh the session instance
	# iworld = case event of
			RefreshEvent 	= refreshSessionInstance sessionId iworld
			_				= processEvent event iworld
	//Refresh affected tasks
	# iworld				= refreshOutdatedInstances iworld 
	//Evaluate session instance
	# (mbInstance,iworld)	= loadSessionInstance sessionId iworld
	= case mbInstance of
		Error e				= (Error e, iworld)
		Ok (meta,reduct,result)
			# (mbRes,iworld)	= evalAndStoreInstance True RefreshEvent (meta,reduct,result) iworld
			# iworld			= remOutdatedInstance meta.TIMeta.instanceNo iworld
			= case mbRes of
				Ok result		= (Ok (result, meta.TIMeta.instanceNo, sessionId), iworld)
				Error e			= (Error e, iworld)
where
	updateCurrentDateTime :: !*IWorld -> *IWorld
	updateCurrentDateTime iworld=:{IWorld|world}
		# (dt,world) = currentDateTimeWorld world
		= {IWorld|iworld  & currentDateTime = dt, world = world}
	
processEvent :: !Event !*IWorld -> *IWorld
processEvent RefreshEvent iworld = iworld
processEvent event iworld
	= case loadTaskInstance (instanceNo event) iworld of
		(Error _,iworld)	= iworld
		(Ok (meta,reduct,result),iworld)
			//Eval the targeted instance first
			# (_,iworld)	= evalAndStoreInstance False event (meta,reduct,result) iworld
			= iworld
where
	instanceNo (EditEvent (TaskId no _) _ _)	= no
	instanceNo (ActionEvent (TaskId no _) _)	= no
	instanceNo (FocusEvent (TaskId no _))		= no
	instanceNo _								= 0 //Should not happen...
	
createPersistentInstance :: !(Task a) !ManagementMeta !User !InstanceNo !*IWorld -> (!TaskId, !*IWorld) | iTask a
createPersistentInstance task meta issuer parent iworld=:{currentDateTime}
	# (instanceId,iworld)	= newInstanceId iworld
	# (state, iworld)		= createTaskInstance instanceId Nothing parent Nothing task meta {issuedAt=currentDateTime,issuedBy=issuer,status=Unstable,firstEvent=Nothing,latestEvent=Nothing} iworld
	# iworld				= storeTaskInstance state iworld		
	= (TaskId instanceId 0, iworld)

//Evaluate a single task instance
evalAndStoreInstance :: !Bool !Event !(TIMeta,TIReduct,TIResult) !*IWorld -> (!MaybeErrorString (TaskResult JSONNode),!*IWorld)
evalAndStoreInstance _ _ inst=:(meta=:{TIMeta|worker=Nothing},_,_) iworld
	= (Error "Can't evalutate a task instance with no worker set", iworld)
evalAndStoreInstance isSession event (meta=:{TIMeta|instanceNo,parent,worker=Just worker,progress},reduct=:{TIReduct|task=Task eval,nextTaskNo=curNextTaskNo,nextTaskTime,tree,shares,lists},result=:TIValue val _) iworld=:{currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Eval instance
	# repAs						= {TaskRepOpts|useLayout=Nothing,afterLayout=Nothing,modLayout=Nothing,appFinalLayout=isSession}
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# iworld					= {iworld & currentInstance = instanceNo, currentUser = worker, nextTaskNo = curNextTaskNo, taskTime = nextTaskTime, localShares = shares, localLists = lists} 
	//Clear the instance's registrations for share changes & remove from outdated queue
	# iworld					= clearShareRegistrations instanceNo iworld
	# iworld					= remOutdatedInstance instanceNo iworld
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= eval event repAs tree iworld
	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	# (lists,iworld)			= getLocalLists iworld
	//Restore current process id, nextTask id and local shares in iworld
	# iworld					= {iworld & currentInstance = currentInstance, currentUser = currentUser, nextTaskNo = nextTaskNo, taskTime = taskTime, localShares = localShares, localLists = localLists}
	# reduct					= {TIReduct|reduct & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, tree = tasktree result, shares = shares, lists = lists}
	# meta						= {TIMeta|meta & progress = setStatus result progress}
	# inst 						= (meta,reduct,taskres result,taskrep result)
	//Store the instance
	# iworld					= storeTaskInstance inst iworld	
	//If the result has a new value, mark the parent process as outdated
	| parent > 0 && isChanged val result
		# iworld				= addOutdatedInstances [parent] iworld
		= (Ok result, iworld)
	| otherwise
		= (Ok result, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo}	= (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|localLists}	= (localLists,iworld)

	setStatus (ExceptionResult _ _) meta				= {meta & status = Stable}
	setStatus (ValueResult (Value _ Stable) _ _ _) meta	= {meta & status = Stable}
	setStatus _	meta									= {meta & status = Unstable}
	
	isChanged val (ValueResult nval _ _ _)  = val =!= nval
	isChanged val _							= True

	tasktree (ValueResult _ _ _ tree)	= tree
	tasktree (ExceptionResult _ _)		= TCNop
	
	taskres (ValueResult val {TaskInfo|lastEvent} _ _)	= TIValue val lastEvent
	taskres (ExceptionResult e str)						= TIException e str
	
	taskrep	(ValueResult _ _ rep _)		= rep
	taskrep (ExceptionResult _ str)		= TaskRep (UIControlSequence ('Map'.newMap, [(stringDisplay str, 'Map'.newMap)], Vertical)) []

evalAndStoreInstance _ _ (_,_,TIException e msg) iworld
	= (Ok (ExceptionResult e msg), iworld)
evalAndStoreInstance _ _ _ iworld	
	= (Ok (exception "Could not unpack instance state"), iworld)

//Evaluate tasks marked as outdated in the task pool
refreshOutdatedInstances :: !*IWorld -> *IWorld
refreshOutdatedInstances iworld = refresh [] iworld
where
	refresh done iworld = case nextOutdatedInstance iworld of
		(Nothing,iworld)	= iworld
		(Just next,iworld)
			| isMember next done	= iworld
									= refresh [next:done] (refreshInstance next iworld)

//Evaluate a task instance without any events
refreshInstance :: !InstanceNo !*IWorld -> *IWorld
refreshInstance instanceNo iworld
	= case loadTaskInstance instanceNo iworld of
		(Error _,iworld)	= iworld
		(Ok (meta,reduct,result),iworld)
			# (_,iworld)	= evalAndStoreInstance False RefreshEvent (meta,reduct,result) iworld
			= iworld

refreshSessionInstance :: !SessionId !*IWorld -> *IWorld
refreshSessionInstance sessionId iworld
	= case loadSessionInstance sessionId iworld of
		(Error _,iworld)	= iworld
		(Ok (meta,reduct,result),iworld)
			# (_,iworld)	= evalAndStoreInstance True RefreshEvent (meta,reduct,result) iworld
			= iworld
			
localShare :: !TaskId -> Shared a | iTask a
localShare taskId=:(TaskId instanceNo taskNo) = createChangeOnWriteSDS "localShare" shareKey read write
where
	shareKey = toString taskId

	read iworld=:{currentInstance,localShares}
		//Local share
		| instanceNo == currentInstance
			= case 'Map'.get taskId localShares of
				Just encs	
					= case fromJSON encs of
						Just s	= (Ok s, iworld)
						_		= (Error ("Could not decode local shared state " +++ shareKey), iworld)
				_			= (Error ("Could not read local shared state " +++ shareKey), iworld)
		//Share of ancestor
		| otherwise
			= case loadTaskReduct instanceNo iworld of
				(Ok reduct,iworld)
					=  case 'Map'.get taskId reduct.TIReduct.shares of
						Just encs
							= case fromJSON encs of	
								Just s	= (Ok s, iworld)
								_		= (Error ("Could not decode remote shared state " +++ shareKey), iworld)
						_
							= (Error ("Could not read remote shared state " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not read remote shared state " +++ shareKey), iworld)
				
	write value iworld=:{currentInstance,localShares}
		| instanceNo == currentInstance
			= (Ok Void, {iworld & localShares = 'Map'.put taskId (toJSON value) localShares})
		| otherwise
			= case loadTaskReduct instanceNo iworld of
				(Ok reduct,iworld)
					# reduct	= {TIReduct|reduct & shares = 'Map'.put taskId (toJSON value) reduct.TIReduct.shares}
					# iworld	= storeTaskReduct instanceNo reduct iworld
					= (Ok Void, iworld)
				(Error _,iworld)
					= (Error ("Could not write to remote shared state " +++ shareKey), iworld)
		
//Top list share has no items, and is therefore completely polymorphic
topListShare :: SharedTaskList a
topListShare = createReadOnlySDSError read
where
	read iworld
		= (Ok {TaskList|listId = TopLevelTaskList, items = []}, iworld)
		
parListShare :: !TaskId -> SharedTaskList a | iTask a
parListShare taskId=:(TaskId instanceNo taskNo) = createReadOnlySDSError read
where
	shareKey = toString taskId
	read iworld=:{currentInstance,localLists}
		| instanceNo == currentInstance		
			= case 'Map'.get taskId localLists of
				Just entries
					= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
				_	= (Error ("Could not read local task list " +++ shareKey), iworld)
		| otherwise
			= case loadTaskReduct instanceNo iworld of
				(Ok reduct, iworld)
					= case 'Map'.get taskId reduct.TIReduct.lists of					
						Just entries
							= (Ok {TaskList|listId = ParallelTaskList taskId, items = [toItem e\\ e <- entries | not e.TaskListEntry.removed]},iworld)
						_	= (Error ("Could not read remote task list " +++ shareKey), iworld)
				(Error _,iworld)
					= (Error ("Could not load remote task list " +++ shareKey), iworld)
					
	toItem {TaskListEntry|entryId,state,result=TIValue val ts,attributes}
		= 	{taskId			= entryId
			,value			= deserialize val
			,taskMeta		= 'Map'.toList attributes
			,managementMeta = management
			,progressMeta	= progress
			}
	where
		(progress,management) = case state of
			DetachedState _ p m = (Just p,Just m)
			_					= (Nothing,Nothing)
	
	deserialize NoValue	= NoValue
	deserialize (Value json stable) = maybe NoValue (\v -> Value v stable) (fromJSON json)
