implementation module Section5

// Examples showing the usage of shared data

// Prdefined shared system variables can be found in: SystemData.dcl

import iTasks
from Section3 import view, repeatUntilApproved

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows5) world

flows5 :: [Workflow]
flows5 
	=   [ workflow "CEFP/Section 5 - Shared Data/1. Date and Time" 					"Shows current date and time"							(view getDateAndTime)
		, workflow "CEFP/Section 5 - Shared Data/2. Continuous Date and Time"		"Keep showing date and time" 							viewDateAndTime
		, workflow "CEFP/Section 5 - Shared Data/3. Administrated users" 			"Shows currently administrated users"					(view getUsers)
		, workflow "CEFP/Section 5 - Shared Data/4. Administrated users details"	"Shows details of all currently administrated users"	(view getUsersDetails)
		, workflow "CEFP/Section 5 - Shared Data/5. Show details of a user"			"Select administrated user and show administration"		selectUserDetails 
		, workflow "CEFP/Section 5 - Shared Data/6. Current Workflows" 				"Which workflows are known here ?"						(view getWorkflows)
		, workflow "CEFP/Section 5 - Shared Data/7. Update To Do List" 				"Edit local copy of To Do list"							(view updateToDoList)
		, workflow "CEFP/Section 5 - Shared Data/8. Update Shared To Do List " 		"Edit To Do list, and share it right away"				(view updateToDoList2)
		, workflow "CEFP/Section 5 - Shared Data/9. View the Shared To Do List" 	"View will be adjusted when updated elsewhere"			viewToDoList 
		, workflow "CEFP/Section 5 - Shared Data/10. Twitter" 						"Follow a Tweet"										joinCEFPtweets  
		, workflow "CEFP/Section 5 - Shared Data/11. Show details of a user, vrs 2"	"Select administrated user and show administration"		selectUserDetails2
		]
		
// Date and Time
getDateAndTime :: Task DateTime
getDateAndTime
    =     		get currentDateTime

viewDateAndTime :: Task (DateTime,Void)  // better switch on automatic refresh
viewDateAndTime
	=			viewSharedInformation "The current date and time is: " [] currentDateTime Void



// Administrated users

getUsers :: Task [User]
getUsers
    =     		get users

// Administrated users details

getUsersDetails :: Task [UserDetails]
getUsersDetails
    =     		get users
      >>= 		getDetails
where
	getDetails []  = return []
	getDetails [user:users]
		=				get (userDetails user)
			>>= \d ->	getDetails users
			>>= \ds ->	return [fromJust d:ds]


selectUserDetails :: Task UserDetails
selectUserDetails
    =     				get users
      >>= \users ->		enterChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	viewInformation ("Details of user " <+++ user) [] (fromJust details)

// Administrated users details

getWorkflows :: Task [Workflow]
getWorkflows	
    =     		get workflows

// getWorkflow would be nice as example ...


// Make my own shared store

:: ToDo =	{ name     :: String
			, deadline :: Maybe Date
			, remark   :: Maybe Note
			, done     :: Bool
			}
derive class iTask ToDo

toDoList :: Shared [ToDo]
toDoList = sharedStore "My To Do List" []

updateToDoList :: Task [ToDo]
updateToDoList
    =     	get toDoList
      >>= 	\oldList -> updateInformation "Your To Do List" [] oldList
      >>=	\newList -> set newList toDoList

updateToDoList2 :: Task ([ToDo], Void)
updateToDoList2
    =     	updateSharedInformation "Your To Do List" []  toDoList Void 

viewToDoList :: Task ([ToDo], Void)
viewToDoList
	=		viewSharedInformation "Your To Do List" []  toDoList Void 

// using interactions on shared data

selectUserDetails2 :: Task UserDetails
selectUserDetails2
    =     				enterSharedChoice "Select a user" [] users
      >>= \user -> 		get (userDetails user)
      >>= \details -> 	viewInformation ("Details of user " <+++ user) [] (fromJust details)

// 

:: Tweet  :== (User,String)

twitterId :: String -> Shared [Tweet]
twitterId name  = sharedStore ("Twitter with " +++ name) []

joinCEFPtweets 
	= 				get currentUser
		>>= \me ->	joinTweets me "CEFP" (twitterId "CEFP")

joinTweets  :: User String (Shared [Tweet]) -> Task Void
joinTweets me name tweets
	=				updateSharedInformation ("Enter tweet for " +++ name) views tweets ""
		>?*			[(ActionQuit,Always (return Void))
					,(ActionOk, IfValid (\(_,message) -> commit (me,message)))
					]
where
	commit :: Tweet -> Task Void
	commit tweet
		=				update (\tweets -> tweets ++ [tweet]) tweets 
			>>| 		joinTweets me name tweets

	views = [ DisplayView (GetShared id)
			, EnterView (SetLocal \(Note reaction) _ _ -> reaction)
			]
			
joinTweets2  :: User String (Shared [Tweet]) -> Task Void
joinTweets2 me name tweets
	=				updateSharedInformation ("Enter tweet for " +++ name) views tweets ""
		>?*			[(ActionQuit,Always (return Void))
					]
where
	views = [ UpdateView 
				( GetCombined (\string tweets -> (Display tweets, Note string))
			    , SetShared (\(_,Note reaction) _ tweets -> tweets ++ [(me,reaction)])
				)
			]
			
joinTweets3  :: User String (Shared [Tweet]) -> Task Void
joinTweets3 me name tweets
	=				updateSharedInformation ("Enter tweet for " +++ name) views tweets ""
		>?*			[(ActionQuit,Always (return Void))
					]
where
	views =  [ DisplayView (GetShared id)
			 , UpdateView (GetLocal \reaction -> Note reaction, SetLocal \(Note reaction) _ _ -> reaction)
			 , UpdateTrigger "Commit" (UpdateCombined (\reaction tweets -> (Just "", Just (tweets ++ [(me,reaction)]))))
			 ]
			 
			 
			