definition module TaskStore
/**
* This module provides storage of task instances
* It contains two types of task instances:
* Session instances: temporary tasks for each interactive session between a user and the server. 
* Workflow instances: persistent long-running tasks that may be shared between users and exist between sessions.
*/
import Maybe, Error, SystemTypes, Task, TaskState, UIDefinition

from Time				import :: Timestamp
from SharedDataSource	import :: BasicShareId

newSessionId			:: !*IWorld -> (!SessionId,	!*IWorld)
newInstanceId			:: !*IWorld -> (!InstanceNo, !*IWorld)
newDocumentId			:: !*IWorld -> (!DocumentId, !*IWorld)

storeTaskInstance		:: !TaskInstance !*IWorld -> *IWorld

loadTaskInstance		:: !InstanceNo !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)
loadSessionInstance		:: !SessionId !*IWorld -> (!MaybeErrorString (TIMeta,TIReduct,TIResult), !*IWorld)

//Separated load functions
loadTaskMeta			:: !InstanceNo !*IWorld -> (!MaybeErrorString TIMeta, !*IWorld)
loadTaskReduct			:: !InstanceNo !*IWorld -> (!MaybeErrorString TIReduct, !*IWorld)
loadTaskResult			:: !InstanceNo !*IWorld -> (!MaybeErrorString TIResult, !*IWorld)
loadTaskRep				:: !InstanceNo !*IWorld -> (!MaybeErrorString TIRep, !*IWorld)

//Store 
storeTaskMeta			:: !InstanceNo !TIMeta !*IWorld -> *IWorld
storeTaskReduct			:: !InstanceNo !TIReduct !*IWorld -> *IWorld
storeTaskResult			:: !InstanceNo !TIResult !*IWorld -> *IWorld
storeTaskRep			:: !InstanceNo !TIRep !*IWorld -> *IWorld

deleteTaskInstance		:: !InstanceNo !*IWorld -> *IWorld

//Documents
createDocument 			:: !String !String !String !*IWorld -> (!MaybeError FileError Document, !*IWorld)
createDocumentWith		:: !String !String (*File -> *File) !*IWorld -> (!MaybeError FileError Document, !*IWorld)
loadDocumentContent		:: !DocumentId !*IWorld -> (!Maybe String, !*IWorld)
loadDocumentMeta		:: !DocumentId !*IWorld -> (!Maybe Document, !*IWorld)

documentLocation		:: !DocumentId !*IWorld -> (!FilePath,!*IWorld)


//Keep track of which instances depend on other instances
setTaskWorker			:: !User !InstanceNo !*IWorld -> *IWorld
addTaskInstanceObserver	:: !InstanceNo !InstanceNo !*IWorld -> *IWorld

//Keep track of outdated task instances that need to be refreshed
addOutdatedInstances		:: ![InstanceNo] !*IWorld -> *IWorld
remOutdatedInstance			:: !InstanceNo !*IWorld -> *IWorld

nextOutdatedInstance		:: !*IWorld -> (!Maybe InstanceNo,!*IWorld)

addShareRegistration		:: !BasicShareId !InstanceNo !*IWorld -> *IWorld
clearShareRegistrations		:: !InstanceNo !*IWorld -> *IWorld
addOutdatedOnShareChange	:: !BasicShareId !*IWorld -> *IWorld

//Keep last version of session user interfaces around, to be able to send differences to client
storeCurUI				:: !SessionId !Int !UIDef !*IWorld -> *IWorld
loadPrevUI				:: !SessionId !Int !*IWorld -> (!Maybe UIDef, !*IWorld)

//Sync previous user interfaces to disk (Only used with CGI wrapper)
saveUICache				:: !*IWorld -> *IWorld
restoreUICache			:: !*IWorld -> *IWorld

