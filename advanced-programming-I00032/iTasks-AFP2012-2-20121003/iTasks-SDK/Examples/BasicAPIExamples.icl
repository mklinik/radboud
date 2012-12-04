implementation module BasicAPIExamples
import iTasks, UserAdmin, WorkflowAdmin
import Text
/**
* This module contains a series of small examples of basic usage of the iTasks API.
*/

//* Running the tasks in a workflow browser

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
costumTypes :== bae +++ "/Interaction with custom types/"
sharedData	:== bae +++ "/Interaction with shared data/"
seqTasks	:== bae +++ "/Sequential task composition/"
parTasks	:== bae +++ "/Parallel task composition/"
distrTask	:== bae +++ "/Distributed tasks/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			helloWorld
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				enterString
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				enterInt
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			enterDateTime
	,workflow (basicTypes +++ "Browse Google Map") 			"Browse the map" 					browseGoogleMap

	,workflow (costumTypes +++ "Enter a person") 		 	"Entering a person" 				enterPerson
	,workflow (costumTypes +++ "Enter multiple persons") 	"Entering multiple persons" 		enterPersons

	,workflow (sharedData +++ "View date and time")		 	"View the current date and time" 	viewCurDateTime
	,workflow (sharedData +++ "Edit stored persons") 	 	"Update a stored list of persons" 	editStoredPersons
	,workflow (sharedData +++ "View stored persons") 	 	"View a stored list of persons" 	viewStoredPersons
	,workflow (sharedData +++ "Editors on shared note") 	"edit notes" 						notes
	,workflow (sharedData +++ "Edit note or List of strings") "Edit note or List of strings" 	linesPar

	,workflow (seqTasks +++ "Hello User") 	 			 	"Enter your name:" 					hello
	,workflow (seqTasks +++ "Positive Number") 	 			"Enter a positive number:" 			positiveNumber
	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				palindrome
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers" 				calculateSum
	,workflow (seqTasks +++ "Sum, with backstep") 	 		"Sum, with backstep" 				calculateSumSteps
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers 2" 				calculateSum2
	,workflow (seqTasks +++ "Add persons 1 by 1") 	 		"Add persons 1 by 1" 				(person1by1 [])

	,workflow (seqTasks +++ "Coffee Machine") 	 			"Coffee Machine" 					coffeemachine
	,workflow (seqTasks +++ "Calculator") 	 				"Calculator" 						calculator
	,workflow (seqTasks +++ "Edit shared list of persons") 	"Edit shared list of persons" 		editPersonList
	,workflow (seqTasks +++ "Edit shared todo list") 		"Edit shared todo list" 			editToDoList
	,workflow (seqTasks +++ "Follow tweets of a user") 		"Follow tweets of a user" 			followTweets


	,workflow (parTasks +++ "Simple editor with statistics")"Edit text" 						editWithStatistics

	,workflow (distrTask +++ "Delegate Enter a person") 	"Delegate Enter a person" 			(delegate enterPerson)
	,workflow (distrTask +++ "Chat with someone")   		"Chat with someone" 				chat
	,workflow (distrTask +++ "Plan meeting") 				"Plan meeting" 						testMeeting

	,workflow "Droste Cacaobus" 							"Start this application as a task" 	(manageWorklist basicAPIExamples)

	,workflow "Manage users" 							 	"Manage system users..." 			manageUsers 
	]
	
Start :: *World -> *World
Start world = startEngine (browseExamples basicAPIExamples) world
where
	browseExamples examples = forever (
		 	(viewTitle "iTasks Example Collection"
		||-
		 	enterInformation ("Login","Enter your credentials and login or press continue to remain anonymous") [])
		>>* [WithResult (Action "Login") (const True) (browseAuthenticated examples)
			,Always (Action "Continue") (browseAnonymous examples)
			])
	
	browseAuthenticated examples {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist examples)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	
	browseAnonymous examples
		= manageWorklist examples
		
		
//* utility functions

undef = undef

always = const True

hasValue (Value _ _) = True
hasValue _ = False

getValue (Value v _) = v

ifValue pred (Value v _) = pred v
ifValue _ _ = False

ifStable (Value v Stable) = True
ifStable _ = False

returnF :: (a -> b) (TaskValue a) -> Task b | iTask b
returnF fun (Value v _) = return (fun v)

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v

toMaybe :: (TaskValue a) -> Maybe a
toMaybe (Value v _) =  (Just v)
toMaybe _   =  Nothing

getUserName :: User -> String
getUserName u = toString u
//getUserName (AuthenticatedUser id _ (Just name)) = name +++ id
//getUserName _ = "Anonymous"

(>||) infixl 1 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(>||) ta tb = ta >>* [ OnValue  ifStable (const tb) ]

//* The example tasks are colelcted in categories:

//* Basic interaction

helloWorld :: Task String
helloWorld = viewInformation "You have a message from iTasks:" [] "Hello world!" 

enterString :: Task String
enterString = enterInformation "Enter a string" []

enterInt :: Task Int
enterInt = enterInformation "Enter an integer" []

enterDateTime :: Task DateTime
enterDateTime = enterInformation "Enter a date and time" []

viewIntList :: Task [Int]
viewIntList = viewInformation "View the numbers from 1 to 10" [] [1..10]

browseGoogleMap :: Task GoogleMap
browseGoogleMap = enterInformation ("Browse Map","Move around on the map. Your perspective is persistent during the task") []

//* Interaction using user-defined types

:: MyPerson =
	{ name			:: String
	, gender		:: MyGender
	, dateOfBirth	:: Maybe Date
	}
	
:: MyGender = Male | Female

derive class iTask MyPerson, MyGender


enterPerson :: Task MyPerson 
enterPerson = enterInformation "Enter your personal information" []

enterPersons :: Task [MyPerson]
enterPersons = enterInformation "Enter personal information of multiple people" []



//* Interaction with shared data

viewCurDateTime :: Task DateTime
viewCurDateTime = viewSharedInformation "The current date and time is:" [] currentDateTime

personStore :: Shared [MyPerson]
personStore = sharedStore "Persons" []

editStoredPersons :: Task [MyPerson]
editStoredPersons = updateSharedInformation "Update the stored list of persons" [] personStore

viewStoredPersons :: Task [MyPerson] 
viewStoredPersons = viewSharedInformation "These are the currently stored persons" [] personStore

notes :: Task Note
notes 
	= withShared (Note "")
		(\note -> 	viewSharedInformation "view on note" [] note
					-||-
					updateSharedInformation "edit shared note 1" [] note
					-||-
					updateSharedInformation "edit shared note 2" [] note
		)

linesPar :: Task (Maybe String)
linesPar
	=	withShared "" doEditor
where
	doEditor state
		= 			noteE state 
					-||- 
					lineE state
			>>* 	[OnAction ActionQuit always (return o toMaybe)]

	noteE state 
		= 			updateSharedInformation ("Text","Edit text") [noteEditor] state
			>>*		[ OnAction (Action "Trim") always 	(\txt -> update trim state >>| noteE state)	
					]

	lineE state
		=	updateSharedInformation ("Lines","Edit lines") [listEditor] state

	noteEditor = UpdateWith (\txt -> Note txt) (\_ (Note txt) -> txt)
	listEditor = UpdateWith (split "\n") (\_ l -> join "\n" l)

//* Sequential task composition

hello :: Task String
hello 
	=           enterInformation "Please enter your name" []
        >>= 	viewInformation ("Hello ") [] 

positiveNumber :: Task Int
positiveNumber 
	= 		enterInformation "Please enter a positive number" []
		>>* [ OnAction  ActionOk (ifValue (\n -> n >= 0))  returnV
            ] 

palindrome :: Task (Maybe String)
palindrome 
	=   	enterInformation "Enter a palindrome" []
		>>* [ OnAction  ActionOk     (ifValue palindrome) (returnF Just)
            , OnAction  ActionCancel always   			  (returnC Nothing)
            ]
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s





person1by1 :: [MyPerson] -> Task [MyPerson]
person1by1 persons
	=       enterInformation "Add a person" [] 	-|| viewInformation "List so far.." [] persons
		>>*	[ OnAction  (Action "Add") 			hasValue  	(\v  -> person1by1  [getValue v : persons])  
		    , OnAction  (Action "Finish")      	always 		(\_  -> return persons)
		    , OnAction  ActionCancel 			always      (\_  -> return [] )
	        ]

// BUG? not always all record fields are shown in a choice...
// sometimes I get several continues... does not looks nice

editPersonList :: Task Void
editPersonList = editSharedList personStore

editSharedList :: (Shared [a]) -> Task Void | iTask a
editSharedList store 
	=			enterSharedChoice "Choose an item to edit" [ChooseWith ChooseFromGrid snd] (mapRead (\ps -> [(i,p) \\ p <- ps & i <- [0..]]) store)
		>>*		[ OnAction (Action "Append")   hasValue (showAndDo append o getValue)
				, OnAction (Action "Delete")   hasValue (showAndDo delete o getValue)
				, OnAction (Action "Edit")     hasValue (showAndDo edit   o getValue)
				, OnAction (Action "Clear")    always   (const (showAndDo append (-1,undef)))
				, OnAction (Action "Quit")     always   (const (return Void))
				]
where
	showAndDo fun ip
		=		viewSharedInformation "In store" [] store
 		 		||- 
 		 		fun ip
 		 	>>* [ OnValue 					 hasValue (const (editSharedList store))
 		 		, OnAction (Action "Cancel") always   (const (editSharedList store))
 		 		]

	append (i,_)
		=			enterInformation "Add new item" []
		>>=	\n ->	update (\ps -> let (begin,end) = splitAt (i+1) ps in (begin ++ [n] ++ end)) store
	delete (i,_)
		=			update (\ps -> removeAt i ps) store
	edit (i,p)
		=			updateInformation "Edit item" [] p 
		 >>= \p -> 	update (\ps ->  updateAt i p ps) store

//

:: ToDo =	{ name     :: String
			, deadline :: Maybe Date
			, remark   :: Maybe Note
			, done     :: Bool
			}
derive class iTask ToDo

toDoList :: Shared [ToDo]
toDoList = sharedStore "My To Do List" []

editToDoList = editSharedList toDoList

//* tweets

:: Tweet  :== (String,String)

twitterId :: String -> Shared [Tweet]
twitterId name  = sharedStore ("Twitter with " +++ name) []

followTweets 
	= 					get currentUser
		>>= \me ->		enterSharedChoice "Whoms tweets you want to see?" [] users
		>>= \user ->	let name = getUserName user in joinTweets (getUserName me) name "type in your tweet" (twitterId name)
where
	joinTweets  :: String String String (Shared [Tweet]) -> Task Void
	joinTweets me follow message tweetsStore
		=			viewSharedInformation ("You are following " +++ follow) [] tweetsStore
					||-
					updateInformation "Add a tweet" [] message
			>>*		[ OnAction (Action "Quit")    always (const (return Void))
	//				, OnAction (Action "Refresh") always (const (joinTweets me follow tweets)) 
					, OnAction (Action "Commit")  (ifValue (\v -> True /*size v > 0*/)) (commit o getValue)
					]
	where
		commit :: String -> Task Void
		commit message
			=				update (\tweets -> tweets ++ [(me,message)]) tweetsStore 
				>>| 		joinTweets me follow "type in your tweet" tweetsStore

//

calculateSum :: Task Int
calculateSum
  =   enterInformation ("Number 1","Enter a number") []
  >>= \num1 ->
      enterInformation ("Number 2","Enter another number") []
  >>= \num2 ->
      viewInformation ("Sum","The sum of those numbers is:") [] (num1 + num2)

calculateSumSteps :: Task Int
calculateSumSteps = step1 0 0
where
	step1 n1 n2		=		updateInformation ("Number 1","Enter first number")  [] n1
						>>*	[ OnAction ActionNext hasValue ((\n1 -> step2 n1 n2) o getValue)
							]
	step2 n1 n2		=		updateInformation ("Number 2","Enter second number") [] n2
						>>*	[ OnAction ActionPrevious always 	(const (step1 n1 n2))
							, OnAction ActionNext     hasValue ((\n2 -> step3 n1 n2) o getValue)]
	step3 n1 n2		=		viewInformation ("Sum","The sum of those numbers is:") [] (n1 + n2)
						>>*	[ OnAction ActionPrevious always 	(const (step2 n1 n2))
						  	, OnAction ActionOk  always  		(returnC (n1 + n2))
						  	]
//
:: MySum = {firstNumber :: Int, secondNumber :: Int, sum :: Display Int}
derive class iTask MySum

calculateSum2 :: Task Int
calculateSum2
  = 				updateInformation ("Sum of 2 numbers, with view","") 
  						[UpdateWith (\(i,j) -> {firstNumber = i, secondNumber = j, sum = Display (i+j)}) 
  						            (\_ res -> (res.firstNumber,res.secondNumber))] (0,0)
  	>>= \(i,j) -> 	return (i+j)

//
coffeemachine :: Task (String,EUR)
coffeemachine  
	=	enterChoice ("Product","Choose your product:") []
					[("Coffee", EUR 100)
					,("Cappucino", EUR 150)
					,("Tea", EUR 50)
					,("Chocolate", EUR 100)
					] 
	>>=  getCoins (EUR 0)
	>>|  coffeemachine

getCoins :: EUR (String,EUR) -> Task (String,EUR)
getCoins paid (product,toPay) 
	= 				viewInformation "Coffee Machine" [ViewWith view1] toPay
					||-		
					enterChoice  ("Insert coins","Please insert a coin...") [ChooseWith ChooseFromRadioButtons id] coins
			>>*		[ OnAction ActionCancel 		always (const (stop ("Cancelled",paid)))
					, OnAction (Action "Insert") 	always (handleMoney o getValue)
					]
where				
	coins	= [EUR 5,EUR 10,EUR 20,EUR 50,EUR 100,EUR 200]

	handleMoney coin 
	| toPay > coin	= getCoins (paid+coin) (product, toPay-coin)
	| otherwise		= stop (product,coin-toPay)
	
	stop (product, money) = viewInformation "Coffee Machine" [ViewWith view2] (product,money)

	view1 toPay 		   = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("To pay: " <+++ toPay)])]
	view2 (product,money)  = [(DivTag [] [Text ("Chosen product: " <+++ product), BrTag [], Text ("Money returned: " <+++ money)])]

// BUG? needs more work on lay-out and should work on reals to allow dividing...

:: CalculatorState = { display :: Int, n :: Int }

derive class iTask CalculatorState

calculator :: Task Int
calculator = calc initSt
where
	calc st
	= 		viewInformation "Calculator" [ViewWith Display] st
		>>* [ OnAction (Action "7") always (updateDigit 7 st) 
			, OnAction (Action "8") always (updateDigit 8 st) 
			, OnAction (Action "9") always (updateDigit 9 st) 
			, OnAction (Action "4") always (updateDigit 4 st) 
			, OnAction (Action "5") always (updateDigit 5 st) 
			, OnAction (Action "6") always (updateDigit 6 st) 
			, OnAction (Action "1") always (updateDigit 1 st) 
			, OnAction (Action "2") always (updateDigit 2 st) 
			, OnAction (Action "3") always (updateDigit 3 st) 
			, OnAction (Action "0") always (updateDigit 0 st) 
			, OnAction (Action "+") always (apply (+) st) 
			, OnAction (Action "-") always (apply (-) st) 
			, OnAction (Action "*") always (apply (*) st) 
			, OnAction (Action "/") always (apply (/) st) 
			]
	where
		updateDigit n st _ = calc {st & n = st.n*10 + n}
	
		apply op st _ = calc {display = op st.display st.n, n = 0}

	initSt = { display = 0, n = 0}


//* Parallel task composition

derive class iTask Statistics, Replace

:: Statistics = {lineCount :: Int
				,wordCount :: Int
				}
:: Replace	 =	{ search  :: String
				, replaceBy :: String
				}

initReplace =	{ search = ""
				, replaceBy = "" 
				}
stat text = {lineCount = lengthLines text, wordCount = lengthWords text}
where
	lengthLines ""   = 0
	lengthLines text = length (split "\n" text)

	lengthWords "" 	 = 0
	lengthWords text = length (split " " (replaceSubString "\n" " " text))
			
editWithStatistics :: Task Void
editWithStatistics 
 =						enterInformation "Give name of text file you want to edit..." []
	>>= \fileName -> 	let file = sharedStore fileName ""
						in	parallel Void 	[ (Embedded, showStatistics file)
									  		, (Embedded, editFile fileName file)
									  		, (Embedded, replace initReplace file)
									  		]
							>>*	 			[ OnAction (ActionQuit) always (const (return Void))
											]
											
editFile :: String (Shared String) (SharedTaskList Void) -> Task Void
editFile fileName sharedFile _
 =						updateSharedInformation ("edit " +++ fileName) [UpdateWith toV fromV] sharedFile
 	@ 					const Void
where
	toV text 			= Note text
	fromV _ (Note text) = text

showStatistics sharedFile _  = noStat 
where
	noStat :: Task Void
	noStat	=			viewInformation Void [] Void
 				>>*		[ OnAction (Action "File/Show Statistics") always (const showStat)
 						]
	showStat :: Task Void 
	showStat =			viewSharedInformation "Statistics:" [ViewWith stat] sharedFile 
 				>>*		[ OnAction (Action "File/Hide Statistics") always (const noStat)
 						]


replace cmnd sharedFile _ = noReplace cmnd
where
	noReplace :: Replace -> Task Void
	noReplace cmnd 
		=		viewInformation Void [] Void
 			>>*	[ OnAction (Action "File/Replace") always (const (showReplace cmnd))
				]

	showReplace :: Replace -> Task Void 
	showReplace cmnd
		=		updateInformation "Replace:" [] cmnd 
 			>>*	[ OnAction (Action "Replace") hasValue (substitute o getValue)
 				, OnAction (Action "Cancel")  always   (const (noReplace cmnd))
 				]
 			
 	substitute cmnd =	update (replaceSubString cmnd.search cmnd.replaceBy) sharedFile 
 						>>| showReplace cmnd


//* Distributing tasks

// delegate

delegate :: (Task a) -> Task a | iTask a
delegate task
	=					enterSharedChoice "Select someone to delegate the task to:" [] users
		>>= \user -> 	user @: (task >>= return)
		>>= \result ->	viewInformation "The result is:" [] result

// chat


chat :: Task Void
chat = 					get currentUser
		>>= \me ->		enterSharedChoice "Select someone to chat with:" [] users
		>>= \you -> 	withShared ("","") (duoChat me you)
where
	duoChat me you notes
		=	chat you toView fromView notes
			-||- 
			(you @: chat me (toView o switch) (\a v -> switch (fromView a v)) notes)

	chat who toView fromView notes
		= 			updateSharedInformation ("Chat with " <+++ who) [UpdateWith toView fromView] notes
			>>*		[OnAction (Action "Stop") always (const (return Void))]

	toView   (me,you) 							= (Display you, Note me)
	fromView _ (Display you, Note me) 	= (me,you) 

	switch (me,you) = (you,me)

// plan meeting

testMeeting :: Task DateTime
testMeeting
	=	enterSharedMultipleChoice ("Choose users","Select the users with whom you want to plan a meeting...") [] users
	>>=	planMeeting
	
planMeeting :: [User] -> Task DateTime
planMeeting users =   enterDateTimeOptions
                  >>* [askPreferences users]
                  >>* [tryAgain users,decide]

enterDateTimeOptions :: Task [DateTime]
enterDateTimeOptions = enterInformation "Propose meeting dates and times..." []

askPreferences :: [User] -> TaskStep [DateTime] [(User,[DateTime])]
askPreferences users
  = OnAction (Action "Continue") hasValue (ask users o getValue)

ask :: [User] [DateTime] -> Task [(User,[DateTime])]
ask users options
	= parallel "Collect possibilities"
	  [ (Embedded, monitor) 
	  :[(Detached (worker u),select u options) \\ u <- users]
	  ]
	  @ \answers -> [a \\ (_,Value a _) <- answers]

monitor :: ParallelTask a | iTask a
monitor = \all_results -> viewSharedInformation "Results so far" [] (mapRead tl (taskListState all_results)) @? \_ -> NoValue

select :: User [DateTime] -> ParallelTask (User,[DateTime])
select user options = \_ -> (enterMultipleChoice "Enter preferences" [] options @ \choice -> (user,choice))
 
tryAgain :: [User] -> TaskStep [(User,[DateTime])] DateTime
tryAgain users
  = OnAction (Action "Try again") (const True) (const (planMeeting users))
 
decide :: TaskStep [(User,[DateTime])] DateTime
decide = OnAction (Action "Make decision") hasValue (pick o getValue)

pick :: [(User,[DateTime])] -> Task DateTime
pick user_dates
  =   (enterChoice "Choose date" [] (transpose user_dates) @ fst)
      -||-
      (enterInformation "Enter override" [])
  >>* [OnAction (Action "Continue") hasValue (return o getValue)]

transpose :: [(a,[b])] -> [(b,[a])] | Eq b
transpose a_bs = [(b,[a \\ (a,bs) <- a_bs | isMember b bs]) \\ b <- removeDup (flatten (map snd a_bs))]

worker :: User -> ManagementMeta
worker (AuthenticatedUser id _ _) = {noMeta & worker = UserWithId id}

//* Customizing interaction with views

//* Layout tuning




