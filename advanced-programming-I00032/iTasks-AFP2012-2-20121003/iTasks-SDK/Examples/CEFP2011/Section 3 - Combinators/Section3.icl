implementation module Section3

// Examples showing the usage of frequently used iTask combinators

import iTasks
import StdMisc
from   StdFunc import flip

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows3) world

flows3 :: [Workflow]
flows3 
	=	[ workflow "CEFP/Section 3 - Combinators/1. Hello"    						"\"Hello World\" in iTask" 				hello
		, workflow "CEFP/Section 3 - Combinators/2. Numbers"						"Enter a number of numbers"				numbers
		, workflow "CEFP/Section 3 - Combinators/3. Form for [Person]" 			"Form for [Person]"						(view personList)
		, workflow "CEFP/Section 3 - Combinators/4. Choose one from [Person]"  	"Choose one from a list" 				(view personList2)
		, workflow "CEFP/Section 3 - Combinators/5. Choose one or more [Person]"	"Select from a list" 					(view personList3)
		, workflow "CEFP/Section 3 - Combinators/6. Form for [Person]" 			"Form for [Person]" 					(view personList4)
		, workflow "CEFP/Section 3 - Combinators/7. Form for [Person]" 			"Form for [Person]" 					(view personList5)
		, workflow "CEFP/Section 3 - Combinators/8. Tea or coffee" 				"Simple choice..." 						(view teaOrCoffee)
		, workflow "CEFP/Section 3 - Combinators/9. Form for [Person]+check" 		"Form for [Person] and check result" 	(view fillInAndCheckPersons)
		, workflow "CEFP/Section 3 - Combinators/10. Test while"					"Test while"							positive
		]
		
// view combinator function that displays result of argument task:

view :: !(Task a) -> Task a | iTask a
view task = task >>= \result -> viewInformation "The result is:" [] result

// view combinator, after eta-conversion:
view2 :: !(Task a) -> Task a | iTask a
view2 t = t >>= viewInformation "The result is:" []

// show combinator in point-free style:
showOff :: ((Task a) -> Task a) | iTask a
showOff = flip (>>=) (viewInformation "The result is:" [])

// Hello World

hello :: Task String
hello 
	=              		enterInformation "Please enter your name" []
        >>= \name -> 	viewInformation ("Hello " +++ name +++ "!") [] name

// Entering numbers

numbers :: Task Int
numbers = view (numbers` 0)
where
	numbers` :: Int -> Task Int
	numbers` sum
		=				enterInformation "Please enter a positive number" []
		  >>= \n ->		if (n > 0) (numbers` (sum + n)) (return sum)

// Typing in a [Person], viewing the result

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female
derive class iTask Person, Gender

personList :: Task [Person]
personList
	=              		enterInformation "Please fill in the form" []

// Same as w32, showing eta conversion

chooseOneAndEdit:: [a] -> Task a | iTask a
chooseOneAndEdit  list
	=					enterChoice "Choose an item to edit" [] list
 		 >>= \choice ->	updateInformation "Edit item" [] choice
		 >>= \elem -> 	viewInformation "Result:" [] elem

personList2 :: Task Person
personList2
	=              		personList
		>>= 			chooseOneAndEdit

// Same as w32, showing higher order functions

personList3 :: Task [Person]
personList3
	=              		personList
		>>=				enterMultipleChoice "Select one or more: " []

// Same as w32, showing recursive variant

personList4 :: Task [Person]
personList4
	=          			enterInformation "Please fill in the form" []
		>>= \person ->  enterChoice "One more ? "  [] ["Yes","No"]
		>>= \answer ->	case answer of
							"Yes" -> 					personList4 
										>>= \persons ->	return [person:persons]
							"No" ->						return [person]

personList5 :: Task [Person]
personList5
	=          			enterInformation "Please fill in the form" []
		>>= \p -> 		enterChoice "One more ? "  [] 
							[("Yes",Hidden (personList5 >>= \ps -> return [p:ps]))
							,("No", Hidden                        (return [p]   ))
							]
		>>= \(_,Hidden continuation) -> continuation


select1of t = t >>= enterChoice "Choose one: " []
selectNof t = t >>= enterMultipleChoice "Select one or more: " []

// Simple choice

teaOrCoffee = enterChoice "Choose an option" [] ["Tea","Coffee"]

// Check result afterwards, not happy: do it again

fillInAndCheckPersons :: Task [Person]
fillInAndCheckPersons = repeatUntilApproved (enterInformation "Please fill in the form:" [])

repeatUntilApproved :: !(Task a) -> Task a | iTask a
repeatUntilApproved task
    =            task
      >>= \v  -> enterChoice "Approve result: " [ChoiceContext v] 
      				[("Yes",Hidden (return v))
      				,("No", Hidden (repeatUntilApproved task))
      				]
      >>= \(_,Hidden c) -> c
                    
positive :: Task Int
positive = while ((>=) 0) (updateInformation "Please enter a positive number" []) 0

while :: (a -> Bool) (a -> Task a) a -> Task a | iTask a
while cond task v
	= abort "You have not yet implemented this task pattern. Please consult exercise 12 of lecture notes."
