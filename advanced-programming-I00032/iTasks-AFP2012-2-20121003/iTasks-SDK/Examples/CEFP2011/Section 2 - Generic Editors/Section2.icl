implementation module Section2

import iTasks
from Section3 import view

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows2) world

flows2 :: [Workflow]
flows2 
	=   [ workflow "CEFP/Section 2 - Generic Editors/1. Hello world!"        	"The infamous hello world"      hello
		, workflow "CEFP/Section 2 - Generic Editors/2. Enter Int value"    	"Form for an integer value" 	(view taskInt)
		, workflow "CEFP/Section 2 - Generic Editors/3. Enter String value" 	"Form for entering a string"	(view taskString)
		, workflow "CEFP/Section 2 - Generic Editors/4. Enter Person data"  	"Form for entering person data" (view taskPerson)
		, workflow "CEFP/Section 2 - Generic Editors/5. Enter [Person]" 		"Fill in person data" 			(view taskPersonList)
		, workflow "CEFP/Section 2 - Generic Editors/6. Choose one item" 		"Choice of one" 				(view chooseNumber)
		, workflow "CEFP/Section 2 - Generic Editors/7. Choose several items"	"Multiple choice" 				(view pizzaWith)
		, workflow "CEFP/Section 2 - Generic Editors/8. Simple Editor" 			"Using type Note" 				(view simpleEditor)
		, workflow "CEFP/Section 2 - Generic Editors/9. Pick a Date" 			"Using type Date" 				(view chooseDate)
		, workflow "CEFP/Section 2 - Generic Editors/10. Point on a map" 		"Using type GoogleMap" 			(view pointOnMap)
		]

// the ubiquitous hello world example

hello = viewInformation "Press Ok to terminate" [] "Hello world!"

// a simple form to type in an integer value

taskInt :: Task Int
taskInt = enterInformation "Please enter a value" []

// a simple form to type in a String

taskString :: Task String
taskString = enterInformation "Please enter a value" []

// a simple form to type in a value of type Person

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female
derive class iTask Person, Gender

taskPerson :: Task Person
taskPerson = enterInformation "Please enter a value" []

// idem, now for [Person]

taskPersonList :: Task [Person]
taskPersonList = enterInformation "Please enter a value" []

//
chooseNumber :: Task Int
chooseNumber = updateChoice "Choose a number" [ChoiceView (ChooseFromTree,\i -> "take " <+++ i)] [0..10] 3

//
pizzaWith :: Task [String]
pizzaWith = enterMultipleChoice "What do you like on your pizza ?" [] ["Cheese","Tomato","Ansjofish","Salami"]

//
simpleEditor :: Task Note
simpleEditor = enterInformation "Enter a piece of text" []

//
chooseDate :: Task Date
chooseDate = enterInformation "Choose a date" []

//
import GoogleMaps

pointOnMap :: Task GoogleMap
pointOnMap = enterInformation "Show me the location" []

