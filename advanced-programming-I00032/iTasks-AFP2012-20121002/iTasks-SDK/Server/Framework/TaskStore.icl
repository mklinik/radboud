implementation module TaskStore

import StdEnv, Maybe

import IWorld, TaskState, Task, Store, Util, Text, Time, Random, JSON_NG, UIDefinition, Map
import SharedDataSource
import SerializationGraphCopy //TODO: Make switchable from within iTasks module

//Derives required for storage of TUI definitions
derive JSONEncode TaskRep, TaskCompositionType
derive JSONEncode UIDef, UIAction, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONEncode UIProgressOpts, UISliderOpts, UIGridOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONEncode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts
derive JSONEncode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, ProgressAmount

derive JSONDecode TaskRep, TaskCompositionType
derive JSONDecode UIDef, UIAction, UIControl, UISizeOpts, UIViewOpts, UIEditOpts, UIActionOpts, UIChoiceOpts, UILayoutOpts
derive JSONDecode UIProgressOpts, UISliderOpts, UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UIGridOpts, UIIconOpts, UILabelOpts, UITabOpts, UITaskletOpts, UITreeNode
derive JSONDecode UIMenuButtonOpts, UIButtonOpts, UIContainerOpts, UIPanelOpts, UIFieldSetOpts, UIWindowOpts
derive JSONDecode UISize, UIMinSize, UIDirection, UIHAlign, UIVAlign, UISideSizes, UIMenuItem, ProgressAmount

INCREMENT				:== "increment"
PERSISTENT_INDEX		:== "persistent-index"
OUTDATED_INDEX			:== "outdated-index"
SHARE_REGISTRATIONS		:== "share-registrations"

meta_store t	= toString t +++ "-meta"
reduct_store t	= toString t +++ "-reduct"
result_store t	= toString t +++ "-result"
rep_store t		= toString t +++ "-rep"

newSessionId :: !*IWorld -> (!SessionId,!*IWorld)
newSessionId iworld=:{IWorld|world,timestamp}
	# (Clock c, world)		= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) , {IWorld|iworld & world = world})
	
newInstanceId :: !*IWorld -> (!InstanceNo,!*IWorld)
newInstanceId iworld
	# (mbNewTid,iworld) = loadValue NS_TASK_INSTANCES INCREMENT iworld
	= case mbNewTid of
		Just tid
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT (tid+1) iworld 
			= (tid,iworld)
		Nothing
			# iworld = storeValue NS_TASK_INSTANCES INCREMENT 2 iworld //store the next value (2)
			= (1,iworld) //return the first value (1)		

newDocumentId :: !*IWorld -> (!DocumentId, !*IWorld)
newDocumentId iworld=:{world,timestamp}
	# (Clock c,world)	= clock world
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- genRandInt (toInt timestamp+c)]) ,{iworld & world = world})


storeTaskInstance :: !TaskInstance !*IWorld -> *IWorld
storeTaskInstance (meta=:{TIMeta|instanceNo,sessionId},reduct,result,rep) iworld
	//Store all parts
	# iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) meta iworld
	# iworld = storeValue NS_TASK_INSTANCES (reduct_store instanceNo) reduct iworld
	# iworld = storeValue NS_TASK_INSTANCES (result_store instanceNo) result iworld
	# iworld = storeValue NS_TASK_INSTANCES (rep_store instanceNo) rep iworld
	= case sessionId of
		Just sessionId	= updateSessionInstanceIndex (put sessionId instanceNo) iworld
		Nothing			= updatePersistentInstanceIndex (replace (instanceToTaskListItem meta rep)) iworld
where
	replace item [] = [item]
	replace item [i:is] = if (item.TaskListItem.taskId == i.TaskListItem.taskId) [item:is] [i:replace item is]

	instanceToTaskListItem :: !TIMeta !TIRep -> TaskListItem a
	instanceToTaskListItem {TIMeta|instanceNo,progress,management} (TaskRep def _)
		= {taskId = TaskId instanceNo 0, value = NoValue, taskMeta = toList (uiDefAttributes def), progressMeta = Just progress, managementMeta = Just management}

loadTaskInstance :: !InstanceNo !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadTaskInstance instanceNo iworld
	# (meta,iworld)		= loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	# (reduct,iworld)	= loadValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	# (result,iworld)	= loadValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	= case (meta,reduct,result) of
		(Just meta,Just reduct,Just result)
			= (Ok (meta,reduct,result),iworld)
		_
			= (Error ("Could not load instance state of task " +++ toString instanceNo),iworld)
	
loadSessionInstance	:: !SessionId !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadSessionInstance sessionId iworld=:{sessions}
	= case get sessionId sessions of
		Just topno	= loadTaskInstance topno iworld
		_			= (Error ("Could not load session " +++ sessionId), iworld)

loadTaskMeta :: !InstanceNo !*IWorld -> (!MaybeErrorString TIMeta, !*IWorld)
loadTaskMeta instanceNo iworld
	# (meta,iworld)		= loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	= (maybe (Error ("Could not load meta state of task " +++ toString instanceNo)) Ok meta, iworld)
loadTaskReduct :: !InstanceNo !*IWorld -> (!MaybeErrorString TIReduct, !*IWorld)
loadTaskReduct instanceNo iworld
	# (reduct,iworld)	= loadValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	= (maybe (Error ("Could not load reduct state of task " +++ toString instanceNo)) Ok reduct, iworld)

loadTaskResult :: !InstanceNo !*IWorld -> (!MaybeErrorString TIResult, !*IWorld)
loadTaskResult instanceNo iworld
	# (result,iworld)	= loadValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	= (maybe (Error ("Could not load result state of task " +++ toString instanceNo)) Ok result, iworld)
	
loadTaskRep :: !InstanceNo !*IWorld -> (!MaybeErrorString TIRep, !*IWorld)
loadTaskRep instanceNo iworld
	# (rep,iworld)		= loadValue NS_TASK_INSTANCES (rep_store instanceNo) iworld
	= (maybe (Error ("Could not load ui representation state of task " +++ toString instanceNo)) Ok rep, iworld)

storeTaskMeta :: !InstanceNo !TIMeta !*IWorld -> *IWorld
storeTaskMeta instanceNo meta iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) meta iworld

storeTaskReduct :: !InstanceNo !TIReduct !*IWorld -> *IWorld
storeTaskReduct instanceNo reduct iworld = storeValue NS_TASK_INSTANCES (reduct_store instanceNo) reduct iworld

storeTaskResult :: !InstanceNo !TIResult !*IWorld -> *IWorld
storeTaskResult instanceNo result iworld = storeValue NS_TASK_INSTANCES (result_store instanceNo) result iworld

storeTaskRep :: !InstanceNo !TIRep !*IWorld -> *IWorld
storeTaskRep instanceNo rep iworld = storeValue NS_TASK_INSTANCES (rep_store instanceNo) rep iworld

deleteTaskInstance :: !InstanceNo !*IWorld -> *IWorld
deleteTaskInstance instanceNo iworld
	# iworld = deleteValue NS_TASK_INSTANCES (meta_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (reduct_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (result_store instanceNo) iworld
	# iworld = deleteValue NS_TASK_INSTANCES (rep_store instanceNo) iworld
	# iworld = updatePersistentInstanceIndex (delete instanceNo) iworld
	= iworld
where
	delete id list = [ i \\ i <- list | i.TaskListItem.taskId <> TaskId id 0]

createDocument :: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocument name mime content iworld
	# (documentId, iworld)	= newDocumentId iworld
	# document				= {Document|documentId = documentId, contentUrl = "?download="+++documentId, name = name, mime = mime, size = size content}
	# iworld				= storeBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") content iworld
	# iworld				= storeValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") document iworld	
	= (Ok document,iworld)
	
createDocumentWith :: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith name mime f iworld 
	= createDocument name mime "FIXME" iworld //TODO make it possible to apply the function during creation
	
loadDocumentContent	:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentContent documentId iworld
	= loadBlob NS_DOCUMENT_CONTENT (documentId +++ "-data") iworld

loadDocumentMeta :: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)
loadDocumentMeta documentId iworld
	= loadValue NS_DOCUMENT_CONTENT (documentId +++ "-meta") iworld

documentLocation :: !DocumentId !*IWorld -> (!FilePath,!*IWorld)
documentLocation documentId iworld=:{build,dataDirectory}
	= (storePath dataDirectory build </> NS_DOCUMENT_CONTENT </> (documentId +++ "_data.bin"),iworld)

updateTaskInstanceMeta :: !InstanceNo !(TIMeta -> TIMeta) !*IWorld -> *IWorld
updateTaskInstanceMeta instanceNo f iworld
	= case loadValue NS_TASK_INSTANCES (meta_store instanceNo) iworld of
		(Nothing,iworld)	= iworld
		(Just meta,iworld)	
			# iworld = storeValue NS_TASK_INSTANCES (meta_store instanceNo) (f meta) iworld
			# iworld = addOutdatedInstances [instanceNo] iworld
			= iworld

setTaskWorker :: !User !InstanceNo !*IWorld -> *IWorld
setTaskWorker worker instanceNo iworld
	= updateTaskInstanceMeta instanceNo (set worker) iworld
where
	set worker inst=:{TIMeta|worker=Nothing} = {TIMeta|inst & worker = Just worker}
	set _ inst = inst
	
addTaskInstanceObserver	:: !InstanceNo !InstanceNo !*IWorld -> *IWorld
addTaskInstanceObserver observer instanceNo iworld
	= updateTaskInstanceMeta instanceNo (add observer) iworld
where
	add observer meta=:{TIMeta|observers} = {TIMeta|meta & observers = removeDup (observers ++ [observer])}

addOutdatedInstances :: ![InstanceNo] !*IWorld -> *IWorld
addOutdatedInstances outdated iworld = updateOutdatedInstanceIndex (removeDup o ((++) outdated)) iworld

remOutdatedInstance :: !InstanceNo !*IWorld -> *IWorld
remOutdatedInstance rem iworld = updateOutdatedInstanceIndex (filter ((<>) rem)) iworld

nextOutdatedInstance :: !*IWorld -> (!Maybe InstanceNo,!*IWorld)
nextOutdatedInstance iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES OUTDATED_INDEX iworld
	= case index of	
		Just [next:_]	= (Just next,iworld)
		_				= (Nothing,iworld)

addShareRegistration :: !BasicShareId !InstanceNo !*IWorld -> *IWorld
addShareRegistration shareId instanceNo iworld
	# (mbRegs,iworld) = loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs	= fromMaybe newMap mbRegs
	# sregs	= fromMaybe [] (get shareId regs)
	# regs	= put shareId (removeDup (sregs ++ [instanceNo])) regs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
	
clearShareRegistrations :: !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations instanceNo iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= maybe newMap (fromList o clear instanceNo o toList) mbRegs
	= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
where
	clear :: InstanceNo [(BasicShareId,[InstanceNo])] -> [(BasicShareId,[InstanceNo])]
	clear no regs = [(shareId,removeMember no insts) \\ (shareId,insts) <- regs]

addOutdatedOnShareChange :: !BasicShareId !*IWorld -> *IWorld
addOutdatedOnShareChange shareId iworld
	# (mbRegs,iworld)	= loadValue NS_TASK_INSTANCES SHARE_REGISTRATIONS iworld
	# regs				= fromMaybe newMap mbRegs
	= case get shareId regs of
		Just outdated=:[_:_]
			# iworld			= addOutdatedInstances outdated iworld
			# regs				= del shareId regs
			= storeValue NS_TASK_INSTANCES SHARE_REGISTRATIONS regs iworld
		_	= iworld
		
storeCurUI :: !SessionId !Int !UIDef !*IWorld -> *IWorld
storeCurUI sid version def iworld=:{IWorld|uis} = {IWorld|iworld & uis = put sid (version,def) uis}

loadPrevUI	:: !SessionId !Int !*IWorld -> (!Maybe UIDef, !*IWorld)
loadPrevUI sid version iworld=:{IWorld|uis} 
	= case get sid uis of
		Just (prev,def)	| version == (prev + 1)	= (Just def, iworld)
		_										= (Nothing, iworld)

saveUICache	:: !*IWorld -> *IWorld
saveUICache iworld=:{IWorld|uis}
	= storeValue NS_TASK_INSTANCES "ui-cache" uis iworld

restoreUICache :: !*IWorld -> *IWorld
restoreUICache iworld
	# (mbUis,iworld)	= loadValue NS_TASK_INSTANCES "ui-cache" iworld
	= case mbUis of
		Just uis		= {IWorld|iworld & uis = uis}
		_				= iworld

updateSessionInstanceIndex :: !((Map SessionId InstanceNo)-> (Map SessionId InstanceNo)) !*IWorld -> *IWorld
updateSessionInstanceIndex f iworld=:{sessions}
	= {IWorld|iworld & sessions = f sessions}

updatePersistentInstanceIndex :: !([TaskListItem Void] -> [TaskListItem Void]) !*IWorld -> *IWorld 
updatePersistentInstanceIndex f iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES PERSISTENT_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES PERSISTENT_INDEX (f (fromMaybe [] index)) iworld
	= iworld
	
updateOutdatedInstanceIndex :: ([Int] -> [Int]) !*IWorld -> *IWorld
updateOutdatedInstanceIndex f iworld
	# (index,iworld)	= loadValue NS_TASK_INSTANCES OUTDATED_INDEX iworld
	# iworld			= storeValue NS_TASK_INSTANCES OUTDATED_INDEX (sortBy (>) (removeDup (f (fromMaybe [] index)))) iworld
	= iworld
	
