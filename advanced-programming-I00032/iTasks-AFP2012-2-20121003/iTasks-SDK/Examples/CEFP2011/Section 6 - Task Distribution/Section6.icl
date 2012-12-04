implementation module Section6

// Examples showing distributed tasks

import iTasks

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows6) world

flows6 :: [Workflow]
flows6 
	=   [ workflow "CEFP/Section 6 - Task Distribution/1. Delegate a task"	"Delegate a task to some administrated user" 	(delegate someTask)
		, workflow "CEFP/Section 6 - Task Distribution/2. Guess" 			"Number guessing game" 							guessGame
		, workflow "CEFP/Section 6 - Task Distribution/3. Appointment" 		"Try to make an appointment with several user" 	mkAppointment
		]
		

// Delegating a task to someone

delegate :: (Task a) -> Task a | iTask a
delegate task
    =                	selectUser
      >>= \user   -> 	user @: task
      >>= \result ->	updateInformation "Check result" [] result

someTask :: Task [DateTime]
someTask = enterInformation "Please propose some dates for a meeting..." []

// Number guessing game
from Section4 import onlyIf

guessGame :: Task User
guessGame
      =                enterInformation "Please enter a number between 1 and 10 that has to be guessed" [] 
        >?*			   [(ActionOk, Sometimes (onlyIf (\n -> n > 0 && n <= 10) return))]
        >>= \secret -> (delegate (guess secret) -||- delegate (guess secret))
        >>= \winner -> viewInformation (userName winner +++ " has won") [] winner
where
	guess :: Int -> Task User
	guess secret = 		enterInformation "Guess a number between 1 and 10" [] 
						>>= \n -> if (n == secret)  (viewInformation "Welldone, you guessed it" [] n >>| get currentUser)
													(viewInformation "Nope, try again" [] n >>| guess secret)


// Ask everyone if they can meet on acertain time and date

derive class iTask Enquire, YesNoBecause

:: Enquire	=	{ date 		:: Display Date
				, time 		:: Display Time
				, canJoin 	:: YesNoBecause
				}
:: YesNoBecause = Yes | No Note

mkAppointment ::  Task [Enquire]
mkAppointment 
    =                	enterInformation "Which date do you want to meet ?" []
      >>= \date ->		enterInformation "Which time do you want to meet ?" []
      >>= \time ->		selectUsers
      >>= \selected  -> mapTask (\u -> u @: updateInformation "Can we meet ?" [] {date=Display date, time= Display time, canJoin=Yes}) selected
      >>= \answers ->	viewInformation "Users answered" [] answers
      
mapTask :: (a -> Task b) [a] -> Task [b] | iTask b
mapTask f []  			
	= return []
mapTask f [a:as]  
	=				f a
		>>= \b ->	mapTask f as
		>>= \bs ->  return [b:bs]

//	Utility functions:
viewAllUserNames :: Task [String]
viewAllUserNames
    =            get users
      >>= \us -> let names = map displayName us in
      			 viewInformation "The current users are: " [] names

selectUser :: Task User
selectUser
    =     enterSharedChoice "Select a user:" [] users

selectUsers :: Task [User]
selectUsers
	=     enterSharedMultipleChoice "Select users:" [] users
