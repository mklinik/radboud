implementation module ReviewTask

import iTasks

// (c) 2007 MJP

// A task is given to user 0
// When finished the result of the task is reviewed by user 1
// He can comment on the task, or approve or cancel it
// When the result needs more work, the whole process is repeated
// Otherwise the task is completed
// The task itself in the example is a quotation form that needs to be filled in

derive class iTask	QForm, Review, Person, Gender

:: PersonData	=	{ name		:: String
					, e_mail	:: String
					}
:: SurfaceMail	=	{ adress 	:: String
					, zipcode 	:: String
					, city	 	:: String
					}

:: QForm = 	{ toComp 			:: String
			, startDate 		:: Date
			, endDate 			:: Date
			, estimatedHours 	:: Int
			, description		:: Note
			, price				:: EUR 	
			}
:: Person = { firstName			:: String
			 , surname			:: String
			 , dateOfBirth		:: Date
			 , gender			:: Gender
			 }
:: Gender = Male | Female
:: Review = Approved | Rejected | NeedsRework Note


reviewTaskExample :: [Workflow]
reviewTaskExample
= [workflow "Examples/Higher order/Review task" "Demo of an iterative process" (Title "Review the results of a task" @>> reviewtask)]

reviewtask :: Task (QForm,Review)
reviewtask = taskToReview AnyUser (defaultValue, mytask)

mytask :: a -> (Task a) | iTask a
mytask v =	updateInformation ("Form","Fill in Form:") [] v

taskToReview :: UserConstraint (a,a -> Task a) -> Task (a,Review) | iTask a 
taskToReview reviewer (v`,task) 
	=					task v`               
		>>= \v ->		reviewer @: (Title "Review" @>> review v) 
		>>= \r ->		viewInformation ("Review","Reviewer " <+++ reviewer <+++ " says ") [] r
		>>|				case r of
							(NeedsRework _) -> taskToReview reviewer (v,task) 	
							else            -> return (v,r)

review :: a -> Task Review | iTask a 
review v
	=		viewInformation Void [] v
		||-	enterChoice ("Review","What is your verdict?") []
			[ updateInformation ("Comments","Please add your comments") [] (NeedsRework (Note "")) <<@ Title "Rework"
			, return Approved <<@ Title "Approved"
			, return Rejected <<@ Title "Reject"
			]
	>>= \task -> task
