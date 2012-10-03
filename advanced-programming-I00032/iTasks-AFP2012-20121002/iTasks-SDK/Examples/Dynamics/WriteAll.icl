module WriteAll

import iTasks
import DynamicIO

//Business examples
import Vote
import Purchase
import TravelBooking
import OrderProcessing
import ScheduleMeeting

//Higher order examples
import MovingTask
import DeadlineTask
import DelegateTask
import ReviewTask

//Miscellaneous examples
import Coffeemachine
import Newsgroups
import ExceptionHandling
import ChangeHandling
import WebShop

//Administrative tasks
import ProcessAdmin
import StdArray

//Start :: *World -> *World
Start world = startEngine myFlow world

myFlow
=	[ { name	= "Examples/Dynamics/StoreAllExamples"
	, label		= "Store Examples"
	, roles		= ["root"]
	, mainTask	= writeAllWorkflows workflows
	}]


adjustNames :: !String -> String
adjustNames s
=	{# if (c <> '/') c '-' \\ c <-: s}

writeAllWorkflows [] 
	= return Void
writeAllWorkflows [wfl:wfls]
	= 				writeDynamicTask (adjustNames wfl.name) wfl.mainTask
		>>= \ok ->	writeAllWorkflows wfls


workflows = flatten [ voteExample
					, purchaseExample
					, travelBookingExample
					, orderProcessingExample
					, scheduleMeetingExample
					, movingTaskExample
					, deadlineTaskExample
					, delegateTaskExample
					, reviewTaskExample
					, coffeemachineExample
					, newsgroupsExample
					, exceptionHandlingExample
					, changeHandlingExample
					, webShopExample
					, processAdmin
					]