implementation module MovingTask

import iTasks

derive class iTask	QForm, Person, Gender

:: QForm = 	{ forCompany 		:: String
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

movingTaskExample :: [Workflow]
movingTaskExample
	= [ workflow "Examples/Higher order/Moving task" "Demo of a dynamic alteration of task properties" (Title "Suspend,Activate or move a task" @>> movingTask ("Task which can be moved", trivialTask))]

trivialTask :: Task QForm
trivialTask = fillInForm 

fillInForm :: Task QForm
fillInForm	
	= 				enterInformation ("Quote information","Please fill in quotation:") []
	>>= \form ->	viewInformation ("Check","Is everything filled in correctly?") [] form
	>>*				[ AnyTime ActionNo (\_ -> fillInForm)
					, AnyTime ActionYes (\_ -> return form)
					] 

movingTask (label,task)
=					newmove
where
	newmove 
	=				selectUser "Assign a user to perform the task"
		>>= \who ->	appendTopLevelTask {noMeta & worker = toUserConstraint who} (task <<@ Title label)
		>>= 		inspect
	
	inspect pref
	=					enterChoice ("Task options","Go ahead impatient boss:") []
							[ getStatus pref <<@ Title "Get status"
							, suspend pref <<@ Title "Suspend"
							, activate pref <<@ Title "Activate"
							, reassign pref <<@ Title "Reassign"
							, delete pref <<@ Title "Delete task"
							//, waitForIt pref <<@ Title "Wait for task"
							]
		>>= \action ->	action
		>>= \finished -> if finished (return Void) (inspect pref)

	getStatus pid
	=			viewInformation "Task status" [] True
	/*
	=						get ProcessStatus pid
		>>= \st	->			case st of
								Finished	-> viewInformation ("Task finished","It is finished") [] True
								Deleted		-> viewInformation ("Task deleted","It is deleted") [] True		
								Running		-> viewInformation ("Task busy","It is running") [] False		
	*/
	suspend pid
	=						//updateManagerProperties pid (\m -> {ManagerProperties | m & status = Suspended})
		/*>>| */			viewInformation ("Task suspended","workflow is suspended") [] False
								
	activate pid
	=						//updateManagerProperties pid (\m -> {ManagerProperties | m & status = Active})
		/*>>| */			viewInformation ("Task activated","workflow is activated") [] False

	delete pid
	=						removeTask pid topLevelTasks 
		>>| 				viewInformation ("Task deleted","workflow is deleted") [] True				

	reassign pid
	=						selectUser "Who is next?"
		//>>= \who ->			setProcessOwner who pid 
		>>| 				return False
	
	/*waitForIt pid
	=						showInformationA ("Waiting","Waiting for the result...") noActionsMsg ||- wait ("Wait for task", "Wait for an external task to finish") (sharedProcResult pid)
		>>= \(Just res) -> 	deleteProcess pid 
		>>| 				showInformationAbout ("Finished","Finished, the result = ") res 
		>>|					return False*/

	
selectUser :: !String -> Task User
selectUser question = enterSharedChoice question [] users