implementation module DelegateTask

import iTasks

// (c) 2007 MJP

// Quite a difficult workflow exercise given to me by Erik Zuurbier (Thanks Erik).
// First a set of person id's is made to which a task can be delegated
// The task is actually shipped to the first person who accepts the task
// That person can stop the task whenever he wants
// Now again everybody in the set is asked again to accept the task
// The one who accepts can *continue* the work already done so far
// This process can be repeated as many times one likes until finally the task is finished

npersons = 5

:: UserID :== Int

delegateTaskExample :: [Workflow]
delegateTaskExample
= []
/*
= [	{ name	= "Examples/Higher order/Delegate task"
	, label = "Delegated task"
	, roles	= []
	, mainTask = displayHtml foreverTask (delegateTask trivialTask (HtmlTime 0 3 0))
	}
  ]
*/
trivialTask :: Task Int
trivialTask =			enterInformation ("Nr 1","Enter nr 1") []
			>>= \v1 ->	enterInformation ("Nr 2","Enter nr 2") []
			>>= \v2 ->	enterInformation ("Nr 3","Enter nr 3") []
			>>= \v3 -> 	updateInformation ("Result","Your result is:") [] (v1 + v2 + v3)

/*
delegateTask :: (Task a) HtmlTime -> (Task a) | iData a
delegateTask task time 
=					[Text "Choose persons you want to delegate work to:",BrTag [],BrTag []] 
					?>>	determineSet [] 
	>>= \people -> 	delegateToSomeone task people 
	>>= \result -> 	[Text "Result: ", toHtml result] 
					?>> editTask "OK" Void 
	>>= \_ ->		return result
where
	delegateToSomeone :: (Task a) [UserID] -> Task a | iData a
	delegateToSomeone task people = compound "delegateToSet" doDelegate
	where 
		doDelegate						
		 =								orTasks [("Waiting for " <+++ who, who @:: buttonTask "I Will Do It" (return who)) \\ who <- people]	
		 	>>= \who ->					who @:: stopTask2 who -!> task 
		 	>>= \(stopped,task) -> 	if (isJust stopped) (delegateToSomeone task people) task   
	
		stopTask 		= buttonTask "Stop" (return True)					  			

		stopTask2 who	= stopTask -||- (0 @:> ("Stop worker " <+++ who,stopTask))						// alternative : now also user 0 can stop the work
//		stopTask3		= stopTask -||- timerStop time -||- (0 @::> stopTask)	// alternative : now also a timer can stop the work	

		timerStop time	= waitForTimerTask time #>> return True
*/

determineSet :: [User] -> Task [User]
determineSet people = determineSet`
where
	determineSet`	
	=		viewInformation Void [] people			
		||-	enterChoice ("Choose people","Current set:") []
						[ cancelTask choosePerson <<@ Title "Add Person"
						, return Nothing <<@ Title "Finished"
						] 	
		>>= \task	->	task					
		>>= \result -> 	case result of
							(Just new)  -> determineSet (sort (removeDup [new:people])) 
							Nothing		-> if (people == []) (determineSet people) (return people)

	choosePerson = enterSharedChoice ("User","Select a user") [] users >>= \user -> return (Just user)

	cancelTask task = task -||- (viewInformation "Cancel..." [] "Cancel task?" >>| return defaultValue)
	