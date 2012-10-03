module AllExamples

import iTasks

//Business examples
import TravelBooking

//Higher order examples
import MovingTask
import DeadlineTask
import DelegateTask
import ReviewTask
import ExceptionHandling

//Miscellaneous examples
import SmallExamples
import GUIDemo
import BugReport
import Coffeemachine
import APIDocumentation

//import Newsgroups
//import textEditor
import CoffeeTime
import Calculator
import TableExamples
import GeoTracker
//import RPCExamples

//Shared Value Examples
import SharedVariables

//Graphical iTask Notation
//import GinExamples

//Client
import WorkflowAdmin

Start :: *World -> *World
Start world = startEngine (manageWorkflows (workflows ++ workflowmw)) world
where
	workflows = flatten [ travelBookingExample
						, movingTaskExample
						, deadlineTaskExample
						, delegateTaskExample
						, reviewTaskExample
						, smallExamples
						, guiDemoExample
						, bugReportExample
						, coffeemachineExample
						//, textEditor
						, coffeeTimeExample
						, calculatorExample
						, tableExamples
						, geoTrackerExamples
						//, newsgroupsExample
						, exceptionHandlingExample				
						, sharedValueExamples
						//, rpcExamples
						//, ginExamples
						, apiDocumentationExamples
						, [restrictedWorkflow "Admin/Users" "Manage users" ["admin"] manageUsers]
						]
	workflowmw	= [workflow "Manage workflows" "Manage other workflows and instances" (manageWorkflows workflows)]
						
