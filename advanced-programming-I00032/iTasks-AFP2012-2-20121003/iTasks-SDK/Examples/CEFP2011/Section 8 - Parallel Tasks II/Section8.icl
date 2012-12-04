implementation module Section8

// Examples showing the usage of editors with multiple buttons

import iTasks
from Section4 import onlyIf
from Section6 import selectUser
from Section7 import normalTask, const2

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows8) world

flows8 :: [Workflow]
flows8 
	=   [ workflow "CEFP/Section 8 - Parallel Tasks II/1. Chat with several users"    	"Chat with several users" 	chat3
		, workflow "CEFP/Section 8 - Parallel Tasks II/2. Editing a text file" 			"Editing a text file" 		editorApplication
		, workflow "CEFP/Section 8 - Parallel Tasks II/3. Petition campaign" 			"Start petition campaign" 	myPetition
//
		]
		
:: Petition  =  { titlePetition      :: String
                , deadlineSubmission :: DateTime
                , description        :: Note
                }
:: Signer    =  { name               :: String
                , profession         :: Maybe String
                , emailAddress       :: String
                , comments           :: Maybe Note
                }

derive class iTask Petition, Signer

myPetition :: Task (Petition,[Signer])
myPetition
    =              enterInformation "Describe the petition" []
       >>= \pet -> campaign pet pet.titlePetition pet.deadlineSubmission
       >>=         viewInformation "The petition has been signed by:" []		

campaign :: pet String DateTime -> Task (pet,[signed]) | iTask pet & iTask signed
campaign pet title deadline
    =                 enterSharedMultipleChoice "Invite people to sign" [] users
      >>= \signers -> parallel ("Sign Petition: " +++ title) []
                      (\_ signed -> (pet,signed))
                      [ (Embedded, waitForDeadline deadline)
                      : [  (Detached (normalTask signer),sign pet)
                        \\ signer <- signers
                        ]
                      ]
waitForDeadline dateTime list
    =       waitForDateTime dateTime
        >>| return Stop

sign :: pet (TaskList [signed]) -> Task ParallelControl | iTask pet & iTask signed
sign pet list
    =       enterInformation ("Please sign the following petition:") [About pet]
        >?* [(Action "Decline", Always  (return Continue))
            ,(Action "Sign",    IfValid signAndAskFriends)
            ]
where
    signAndAskFriends signed
        =        update (\list -> [signed:list]) (taskListState list)
            >>|  viewInformation "Thanks for signing !" [] Void
            >>|  enterSharedMultipleChoice "Invite other people too" [] users
            >>=  askSigners

    askSigners []     = return Continue
    askSigners [c:cs] = appendTask (Detached (normalTask c), sign pet) list
                        >>| askSigners cs
                        
// chat with several users

derive class iTask ChatState, Message

:: ChatState	=	{ chatters  :: [User]
				    , chats		:: [Message]
				    }
:: Message	=		{ chatting	:: User
					, message	:: String
					}
emptyChatState = {chatters = [], chats = []}

addMessage user message cs = {cs & chats = cs.chats ++ [{chatting = user, message = message}]}
addUser user			cs = {cs & chatters = [user:cs.chatters]}
removeUser user			cs = {cs & chatters = removeMember user cs.chatters}

chat3
    =               get currentUser
    	>>= \me ->	parallel "Chat application" emptyChatState (const2 Void) [(Embedded, chatTask me)]

chatTask user cs
	=			update (addUser user) (taskListState cs)
		>>|		viewSharedInformation ("Chat list view") [] (taskListState cs) Void
				||- 
				chatMore user "" cs

chatMore user s cs 
	= 	updateInformation ("Chat with iTask users") [UpdateView (GetLocal toView,SetLocal fromView)] s  	
		>?*	[(ActionAdd,  IfValid (\r  ->	  appendTask newChatter cs
										  >>| chatMore user r cs))
			,(ActionOk,   IfValid (\r  ->	  update (addMessage user r) (taskListState cs)
										  >>| chatMore user "" cs))
			,(ActionQuit, Always (			  update (removeUser user o addMessage user "bye") (taskListState cs)
										  >>| return Stop	))
			]
where		
	toView c =  Note c 
	fromView (Note c) _ _ = c

newChatter = (Embedded, handleNewChatter)

handleNewChatter cs
	=	Window
		@>>					selectUser
		>>= \someone ->		appendTask (newChatTask someone) cs
		>>|					return Continue
where
	newChatTask someone = (Detached (normalTask someone), chatTask someone)


ActionAdd :== Action "Add Chatter" 

// pocket calculator, see Steffens example...


import Text

derive class iTask Replace, TextStatistics, EditorState

:: Replace			=	{ search 		:: String
						, replaceBy 	:: String
						}
:: TextStatistics 	=	{ lines			:: Int
						, words			:: Int
						, characters	:: Int
						}
:: EditorState		=	{ mytext		:: String
						, replace		:: Bool
						, statistics	:: Bool
						}

:: FileName		:== String

initEditorState text 	= 	{mytext = text, replace = False, statistics = False}
updateReplace b  		=  	update (\s ->{s & replace = b}) 
updateStat b 			=	update (\s -> {s & statistics = b}) 
updateText f 			=	update (\s -> {s & mytext = f s.mytext}) 

voidResult _ _ = Void 

normalTask user = { worker = Just user, role = Nothing, startAt = Nothing, completeBefore = Nothing, notifyAt = Nothing, priority = NormalPriority}

ActionReplace 		:== Action "File/Replace" 
ActionStatistics	:== Action "File/Statistics" 

editorApplication ::  Task Void
editorApplication 
	=						enterInformation "Give name of text file you want to edit..." []
		>>= \fileName ->	readTextFile fileName
		>>= \(_,text) -> 	parallel "Editor" (initEditorState text) voidResult [(Embedded, editor fileName)]

editor :: String (TaskList EditorState) -> Task ParallelControl
editor fileName ls
	= 			updateSharedInformation (fileName,"Edit text file \"" +++ fileName +++ "\"") views (taskListState ls) Void
		>?* 	[ (ActionSave, 		IfValid	  save)
		  		, (ActionReplace,	Sometimes (onlyIf (\(s,_) -> not s.replace)    replace))
		  		, (ActionStatistics,Sometimes (onlyIf (\(s,_) -> not s.statistics) statistics))
		  		, (ActionQuit,		Always 	  quit)
		  		]
where	
	views = [UpdateView ( GetShared (\s -> Note s.mytext)
					    , SetShared (\(Note text) _ s -> {s & mytext = text}) 
					    )
			]

	save (val,_)
		=		saveTextFile fileName val.mytext
			>>|	editor fileName ls

	replace _
		=		updateReplace True (taskListState ls)
			>>| appendTask (Embedded, replaceTask {search = "", replaceBy = ""}) ls
			>>| editor fileName ls

	statistics _
		=		updateStat True (taskListState ls)
			>>|	appendTask (Embedded, statisticsTask) ls
			>>| editor fileName ls

	quit
		=		return Stop

replaceTask :: Replace (TaskList EditorState) -> Task ParallelControl
replaceTask replacement ls
	=			Window
		@>>		updateInformation ("Replace","Define replacement...") [] replacement
		>?*		[(ActionOk,   IfValid replace)
				,(Action "Close", Always close)
				]
where
	replace repl
		=		updateText (replaceSubString repl.search repl.replaceBy) (taskListState ls)
			>>|	replaceTask repl ls
	close
		=		updateReplace False (taskListState ls) 
			>>| return Continue


statisticsTask :: (TaskList EditorState) -> Task ParallelControl
statisticsTask ls
	=	Window
		@>>		viewSharedInformation ("Statistics","Statistics of your document") views (taskListState ls) Void
		>?*		[(Action "Close", Always close)]
where
	views = [DisplayView (GetShared showStatistics)]

	showStatistics state 
		=	{ lines 	 = length (split "\n" state.mytext)
			, words 	 = length (split " " (replaceSubString "\n" " " state.mytext))
			, characters = textSize state.mytext
			}
	close
		=		updateStat False (taskListState ls) 
			>>| return Continue

// --- file access utility

import StdFile

saveTextFile ::  FileName String -> Task Bool
saveTextFile  fileName text 
	= 						accWorld (safeFileMonad  fileName text)
where
	safeFileMonad ::  String String *World -> (Bool,*World)
	safeFileMonad  fileName text world 
	# (ok,file,world)  	= fopen fileName FWriteText world
	| not ok			= (False,world)
	# file				= fwrites text file
	= fclose file world

readTextFile ::  FileName  -> Task (Bool,String)
readTextFile  fileName  
	= 						accWorld (readFileMonad fileName)
where
	readFileMonad :: String  *World -> ((Bool,String),*World)
	readFileMonad fileName world 
	# (ok,file,world)  	= fopen fileName FReadText world
	| not ok			= ((False,""),world)
	# (text,file)		= freads file 1000000
	| text == ""		= ((False,""),world)
	# (ok,world)		= fclose file world
	| not ok			= ((False,""),world)
	= ((True,text),world)

