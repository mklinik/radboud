implementation module Section7

// Examples showing the usage of frequently used iTask combinators

import iTasks, Text, StdMisc
from Section3 import view
from Section6 import selectUser, selectUsers
from Section5 import joinTweets, ::Tweet

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows7) world

flows7 :: [Workflow]
flows7 
	=   [ workflow "CEFP/Section 7 - Parallel Tasks I/1. Questionnaire"		"Question N users"					(view questions)
	    , workflow "CEFP/Section 7 - Parallel Tasks I/2. Number guessing"	"First person to guess wins"		guess
		, workflow "CEFP/Section 7 - Parallel Tasks I/3. N Chatters"		"Chat with a selected number of users"		chatting
	    , workflow "CEFP/Section 7 - Parallel Tasks I/4. Naive Chat"		"Naive chat with many users"		naive_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/5. Monitored Chat" 	"Monitored chat with many users"	monitor_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/6. Shared Chat"	 	"Shared chat with two users"		shared_chat
		, workflow "CEFP/Section 7 - Parallel Tasks I/7. Multibind Chat" 	"Multibind chat with many users"	multibind_chat
		]
		
// --- some handy functions

normalTask :: !User -> ManagementMeta
normalTask user = { worker = Just user, role = Nothing, priority = NormalPriority, startAt = Nothing, completeBefore = Nothing, notifyAt = Nothing}

const2 :: a b !c -> c
const2 _ _ x = x

noResult _ _ = Void

forever` :: !(Task a) -> Task a | iTask a 
forever` t = forever t

// A simple application of parallel: all tasks run to completion (generalized variant of exercise 18)
questions :: Task [(User,String)]
questions
	=                  updateInformation "Pose a question" [] "...?"
	  >>= \question -> selectUsers
	  >>= \users    -> parallel "parallel" [] (\_ s -> s) 
	  						[ (Detached (normalTask u), answer u question) 
	  						\\ u <- users
	  						]
where
	answer u question tasks
		=           updateInformation question [] "...!"
		  >>= \a -> update (\answers -> [(u,a):answers]) (taskListState tasks)
		  >>| return Continue

// A simple application of parallel: first task to complete terminates parallel (generalized variant of exercise 19)
guess :: Task String
guess
	= return "variant of exercise 19 to do"

// N users chatting reusing the tweet example from section 5

chatting :: Task Void
chatting 
    =               		enterSharedMultipleChoice "Select chatters" [] users
    	>>= \users     ->	parallel "Chatting" [] (\_ _ -> Void)
								   [  (Detached (normalTask user), chatting user)
								   \\ user <- users
								   ]
where
	chatting user cs = joinTweets user "Chatting together..." (taskListState cs) >>| return Continue								  

	secret :: (TaskList [Tweet]) -> Task Void
	secret chats = chooseAction [(Action "File/Append Chatter",  Void)]

// N users chatting with each other
:: ChatState :== [String]

addLine :: User String ChatState -> ChatState
addLine me line s = s ++ [me +++> ": " +++ line]

naive_chat :: Task ChatState
naive_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let chatters = [me : others]
							in  parallel "Naive chat" initChatState (\_ chat -> chat)
								   [  (Detached (normalTask who), chat who chatters)
								   \\ who <- chatters
								   ]
where
	chat :: User [User] (TaskList ChatState) -> Task ParallelControl
	chat me chatters tasks
		= forever` (             get chatState
		      >>= \xs         -> updateInformation headerEditor [] (Display xs, Note "")
		      >>= \(_,Note n) -> update (addLine me n) chatState 
		  )
		>>| return Stop
	where
		chatState		= taskListState tasks
		headerEditor	= "Chat with " +++ join "," (map toString chatters)

	initChatState :: ChatState
	initChatState = []


//	N users chatting with each other, now with monitor task
monitor_chat :: Task ChatState
monitor_chat
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let chatters = [me : others]
							in  parallel "Monitored chat" [] (\_ chat -> chat)
								   [  (Detached (normalTask who), chat who chatters)
								   \\ who <- chatters
								   ]
where
	chat :: User [User](TaskList ChatState) -> Task ParallelControl
	chat me chatters tasks
		= viewSharedInformation headerMonitor [] chatState Void ||- forever` enterLine
		>>| return Continue
	where
		headerEditor	= "Chat with "       +++ join "," (map toString chatters)
		headerMonitor	= "Conversation of " +++ join "," (map toString chatters)
		enterLine		=                  enterInformation headerEditor []
						  >>= \(Note n) -> update (addLine me n) chatState

		chatState		= taskListState tasks

:: ChatState2 = { typing	:: [Bool]   	// is chatter i busy with typing
			    , history	:: [String]		// chats so far ....
			    }
derive class iTask ChatState2

initChatState2 n = { typing = repeatn n False, history = []}

setTyping n "" state 	= {state & typing  = updateAt n False state.typing}
setTyping n _ state 	= {state & typing  = updateAt n True  state.typing}

addHistory u lines state = {state & history = state.history ++ [u +++> ": " +++ l \\ l <- lines]}

shared_chat :: Task ChatState2
shared_chat
    =   					get currentUser
    	>>= \me ->			selectUser
		>>= \you ->			parallel "2 Chatters" (initChatState2 2) (\_ s -> s)
								[ (Detached (normalTask me) , chatEditor (me,0) (you,1))
								, (Detached (normalTask you), chatEditor (you,1) (me,0))
								]
where
	chatEditor :: (User,Int) (User,Int) (TaskList ChatState2) -> Task ParallelControl
	chatEditor (me,mine) (you,yours) cs
		= 					updateSharedInformation ("Chat with " <+++ you) [chatView,entryView] (taskListState cs) (Note "")
			>?*				[(ActionQuit, Always (return Stop))]
	where
		chatView	= DisplayView (GetShared toView)
		where
			toView state
				= (you +++> if (state.typing!!yours) " is typing..." " is waiting...", state.history)
		
		entryView	= UpdateView (GetLocal id, SetCombined fromView)
		where
			fromView (Note response) _ state 
				= case split "\n" response of
					[line]		= (Nothing, Just (setTyping mine response state))
					lines
						# newlines	= init lines
						# newentry	= last lines
						= (Just (Note newentry), Just (setTyping mine newentry (addHistory me newlines state)))
				
// N users chatting with each other

multibind_chat :: Task ChatState
multibind_chat 
    =               		get currentUser
    	>>= \me     ->		selectUsers
		>>= \others ->		let names = join "," (map toString [me : others])
							in  parallel "Multibind chat" initChatState (const2 Void)
								   [  (Detached (normalTask who), chat names who)
								   \\ who <- [me : others]
								   ]
where
	chat :: String User (TaskList ChatState) -> Task ParallelControl
	chat names me cs
		= (viewSharedInformation headerMonitor [] chatState Void) ||- enterLine
	where
		headerEditor	= "Chat with "       +++ names
		headerMonitor	= "Conversation of " +++ names
		enterLine		=     	enterInformation headerEditor []
							>?* [ (ActionQuit, Always (return Stop))
								, (ActionOk,   IfValid (\(Note a) ->	 update (addLine me a) chatState
																	 >>| enterLine))
								]
		chatState		= taskListState cs
		
	initChatState :: ChatState
	initChatState = []
