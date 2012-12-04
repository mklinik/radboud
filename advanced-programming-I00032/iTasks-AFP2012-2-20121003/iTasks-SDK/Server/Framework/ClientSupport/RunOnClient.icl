implementation module RunOnClient

import StdMisc
import iTasks, Task, Tasklet, TaskState, TaskStore, TaskEval, TUIEncode

:: TaskState :== Maybe (TIMeta,TIReduct,TIResult)

runOnClient :: !(Task m) -> Task m | iTask m
runOnClient task

	# roc_tasklet =
		{ Tasklet 
		| generatorFunc		= roc_generator task
		, resultFunc		= toResult
		, tweakUI			= id
		}

	= mkTask roc_tasklet
				
toResult Nothing
	= NoValue	

toResult (Just (_,_,TIValue NoValue _))
	= NoValue
	
toResult (Just (_,_,TIValue (Value json stab) _))
	= Value (fromJust (fromJSON json)) stab

roc_generator :: !(Task m) !TaskId !*IWorld -> *(!TaskletGUI TaskState, !TaskState, !*IWorld) | iTask m
roc_generator task (TaskId instanceNo taskNo) iworld

	# (res, iworld) = loadTaskInstance instanceNo iworld
	# (ameta, _, _) = fromOk res
	
	# (newInstanceNo, iworld) = newInstanceId iworld
	# ((meta, reduct, taskres, rep), iworld) = 
		createTaskInstance newInstanceNo ameta.sessionId instanceNo ameta.TIMeta.worker task ameta.management ameta.TIMeta.progress iworld
	
	// Initialize embedded task
	# (meta, reduct, taskres, result, iworld) = evalInstance Nothing Nothing False (meta, reduct, taskres) iworld
	
	# mbTUI = case result of
		(ValueResult _ _ (TaskRep (_, mbTUIDef, _, _) _) _) = mbTUIDef
															= Nothing
	
	# gui = TaskletTUI { TaskletTUI
	  			 	   | tui  			= mbTUI
	  			  	   , eventHandler	= Just (newInstanceNo, controllerFunc)
	  			       } 	
	
	= (gui, Just (meta, reduct, taskres), iworld)

// Init
controllerFunc taskId st Nothing Nothing
	= (Nothing, st)

// Commit
controllerFunc taskId st (Just eventName) Nothing
	= controllerFunc` taskId st Nothing (Just (TaskEvent taskId eventName))

// Edit
controllerFunc taskId st (Just eventName) (Just eventValue)
	= controllerFunc` taskId st (Just (TaskEvent taskId (eventName, fromString eventValue))) Nothing

controllerFunc` taskId st mbEditEvent mbCommitEvent
	# iworld = createClientIWorld

	# (meta, reduct, taskres, result, iworld) = 
		evalInstance mbEditEvent mbCommitEvent False (fromJust st) iworld
		
	# mbTUI = case result of
		(ValueResult _ _ (TaskRep (_, mbTUIDef, _, _) _) _) = mbTUIDef
															= Nothing								
	= (mbTUI, Just (meta, reduct, taskres))

createClientIWorld :: *IWorld
createClientIWorld
		= {IWorld
		  |application			= locundef "application"
		  ,build				= locundef "build"
		  ,appDirectory			= locundef "appDirectory"
		  ,sdkDirectory			= locundef "sdkDirectory"
		  ,dataDirectory		= locundef "dataDirectory"
		  ,config				= locundef "config"
		  ,taskTime				= locundef "taskTime"
		  ,timestamp			= locundef "timestamp"
		  ,currentDateTime		= locundef "currentDateTime"
		  ,currentUser			= locundef "currentUser"
		  ,currentInstance		= locundef "currentInstance"
		  ,nextTaskNo			= locundef "nextTaskNo"
		  ,localShares			= locundef "localShares"
		  ,localLists			= locundef "localLists"
		  ,readShares			= locundef "readShares"
		  ,outdated				= locundef "outdated"
		  ,sessions				= locundef "sessions"
		  ,uis					= locundef "uis"	  		  
		  ,world				= locundef "world"
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)

/**
* Evaluate a single task instance by TaskEval.evalAndStoreInstance
*/
evalInstance :: !(Maybe EditEvent) !(Maybe CommitEvent) !RefreshFlag !(TIMeta,TIReduct,TIResult) !*IWorld -> (!TIMeta,!TIReduct,!TIResult,!TaskResult JSONNode,!*IWorld)
evalInstance editEvent commitEvent refresh (meta=:{TIMeta|instanceNo,parent,worker=Just worker,progress},reduct=:{TIReduct|task=Task eval,nextTaskNo=curNextTaskNo,nextTaskTime,tree,shares,lists},result=:TIValue val _) iworld=:{currentUser,currentInstance,nextTaskNo,taskTime,localShares,localLists}
	//Eval instance
	# repAs						= TaskRepOpts Nothing Nothing
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# iworld					= {iworld & currentInstance = instanceNo, currentUser = worker, nextTaskNo = curNextTaskNo, taskTime = nextTaskTime, localShares = shares, localLists = lists} 
	//Apply task's eval function and take updated nextTaskId from iworld
	# (result,iworld)			= eval editEvent commitEvent refresh repAs tree iworld
	# (updNextTaskNo,iworld)	= getNextTaskNo iworld
	# (shares,iworld)			= getLocalShares iworld
	# (lists,iworld)			= getLocalLists iworld
	//Restore current process id, nextTask id and local shares in iworld
	# iworld					= {iworld & currentInstance = currentInstance, currentUser = currentUser, nextTaskNo = nextTaskNo, taskTime = taskTime, localShares = localShares, localLists = localLists}
	# reduct					= {TIReduct|reduct & nextTaskNo = updNextTaskNo, nextTaskTime = nextTaskTime + 1, tree = tasktree result, shares = shares, lists = lists}
	# meta						= {TIMeta|meta & progress = setStatus result progress}
	= (meta, reduct, taskres result, result, iworld)
where
	getNextTaskNo iworld=:{IWorld|nextTaskNo}	= (nextTaskNo,iworld)
	getLocalShares iworld=:{IWorld|localShares}	= (localShares,iworld)
	getLocalLists iworld=:{IWorld|localLists}	= (localLists,iworld)

	setStatus (ExceptionResult _ _) meta				= {meta & status = Stable}
	setStatus (ValueResult (Value _ Stable) _ _ _) meta	= {meta & status = Stable}
	setStatus _	meta									= {meta & status = Unstable}

	tasktree (ValueResult _ _ _ tree)	= tree
	tasktree (ExceptionResult _ _)		= TCNop
	
	taskres (ValueResult val ts _ _)	= TIValue val ts
	taskres (ExceptionResult e str)		= TIException e str

evalInstance _ _ _ (meta,reduct,TIException e msg) iworld
	= (meta, reduct, TIException e msg, ExceptionResult e msg, iworld)

evalInstance _ _ _ _ iworld
	= abort "Could not unpack instance state"
	