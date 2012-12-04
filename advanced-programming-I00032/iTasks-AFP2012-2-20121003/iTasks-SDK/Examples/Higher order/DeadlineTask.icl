implementation module DeadlineTask

import iTasks
import StdClass, StdEnum, StdList

// (c) MJP 2007

// One can select a user to whom a task is delegated
// This user will get a certain amount of time to finish the task
// If the task is not finished on time, the task will be shipped back to the original user who has to do it instead
// It is also possible that the user becomes impatient and he can cancel the delegated task even though the deadline is not reached

npersons = 6

deadlineTaskExample :: [Workflow]
deadlineTaskExample
	= [ workflow "Examples/Higher order/Deadline task" "Demo of the deadline property for tasks" (Title "Do task before deadline" @>> (deadline trivialTask))]

trivialTask :: Task Int
trivialTask = enterInformation ("Initial number","Enter a number larger than 42") [] <! (\n -> n <= 42) 
			// (\n -> if (n <= 42) (False,"Error " <+++ n <+++ " should be larger than 42") (True,""))

deadline :: (Task a) -> Task a | iTask a
deadline task
=					enterSharedChoice ("Worker","Choose person you want to delegate work to:") [] users
	>>= \whom ->	enterInformation ("Wait time","How long do you want to wait?") []
	>>= \time ->	(delegateTask whom time task)
					-||-
					(viewInformation ("Cancel...","Cancel delegated work if you are getting impatient:") [] Nothing)
	>>= 			checkDone
where
	checkDone (Just value)
		= viewInformation ("Task result","Result of task:") [] value
	checkDone Nothing
		= viewInformation ("No result","Task expired or canceled, you have to do it yourself!") [] Void >>| task

	delegateTask who time task
	= who  @: (Title "Timed Task" @>> mytask)
	where
		mytask
		=			// wait for timeout and return nothing
					( waitForTimer time >>| return Nothing)									
		 			-||-
		 			// do task and return its result
		  			( (viewInformation ("Hurry!","You have to complete the task in " <+++ time <+++ " time") [] Void)
		  			  ||- task 
					  >>= \v -> return (Just v)
					)	
		