definition module IWorld

from FilePath			import :: FilePath
from Map				import :: Map
from Maybe				import :: Maybe
from SystemTypes		import :: DateTime, :: User, :: Config, :: InstanceNo, :: TaskNo, :: TaskId, :: TaskListItem, :: ParallelTaskType, :: TaskTime, :: SessionId
from UIDefinition		import :: UIDef, :: UIControl
from Time				import :: Timestamp
from TaskState			import :: TaskListEntry
from JSON_NG			import :: JSONNode
from StdFile			import class FileSystem		
from SharedDataSource	import class registerSDSMsg, class reportSDSChange, class reportSDSChangeFilter

:: *IWorld		=	{ application			:: !String									// The name of the application	
					, build					:: !String									// The date/time identifier of the application's build
					, appDirectory			:: !FilePath								// Location of the application's executable
					, sdkDirectory			:: !FilePath								// Location of the iTasks SDK
					, dataDirectory			:: !FilePath								// Location of the applications data files
					, config				:: !Config									// The server configuration
					, taskTime				:: !TaskTime								// The 'virtual' time for the task. Increments at every event
					, timestamp				:: !Timestamp								// The timestamp of the current request	
					, currentDateTime		:: !DateTime								// The local date & time of the current request
					, currentUser			:: !User									// The current user
					, currentInstance		:: !InstanceNo								// The current evaluated task instance
					, nextTaskNo			:: !TaskNo									// The next task number to assign
					, localShares			:: !Map TaskId JSONNode						// The set of locally shared values
					, localLists			:: !Map TaskId [TaskListEntry]				// The set of local parallel task lists
					, readShares			:: ![String]								// The IDs of shares from which was read
					, outdated				:: !Bool									// Flag that is set when an internal inconsistenty is detected 
					, sessions				:: !Map SessionId InstanceNo				// Index of sessions to instance numbers
					, uis					:: !Map SessionId (!Int,!UIDef)				// Previous ui versions to optimize output sent to clients
					, world					:: !*World									// The outside world
					}

instance FileSystem IWorld

instance registerSDSMsg			InstanceNo	IWorld
instance reportSDSChange					IWorld
instance reportSDSChangeFilter	InstanceNo	IWorld