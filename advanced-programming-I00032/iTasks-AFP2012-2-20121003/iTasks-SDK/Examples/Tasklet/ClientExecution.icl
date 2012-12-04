module ClientExecution

import RunOnClient, FastString

bae 		:== "Basic API Examples"
basicTypes	:== bae +++ "/Interaction with basic types/"
costumTypes :== bae +++ "/Interaction with custom types/"
seqTasks	:== bae +++ "/Sequential task composition/"

basicAPIExamples :: [Workflow]
basicAPIExamples =
	[workflow (basicTypes +++ "Hello world") 			 	"View a constant string" 			(runOnClient helloWorld)
	,workflow (basicTypes +++ "Enter a string") 		 	"Entering a string" 				(runOnClient enterString)
	,workflow (basicTypes +++ "Enter an integer") 		 	"Entering an integer" 				(runOnClient enterInt)
	,workflow (basicTypes +++ "Enter a date & time") 	 	"Entering a date & time" 			(runOnClient enterDateTime)

	,workflow (costumTypes +++ "Enter a person") 		 	"Entering a person" 				(runOnClient enterPerson)
	,workflow (costumTypes +++ "Enter multiple persons") 	"Entering multiple persons" 		(runOnClient enterPersons)
	
	,workflow (seqTasks +++ "Hello User") 	 			 	"Enter your name:" 					(runOnClient hello)
	,workflow (seqTasks +++ "Positive Number") 	 			"Enter a positive number:" 			(runOnClient positiveNumber)
	,workflow (seqTasks +++ "Palindrome") 	 			 	"Enter a Palindrome" 				(runOnClient palindrome)
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers" 				(runOnClient calculateSum)
	,workflow (seqTasks +++ "Sum, with backstep") 	 		"Sum, with backstep" 				(runOnClient calculateSumSteps)
	,workflow (seqTasks +++ "Sum of two numbers") 	 		"Sum of two numbers 2" 				(runOnClient calculateSum2)
	,workflow (seqTasks +++ "Coffee Machine") 	 			"Coffee Machine" 					(runOnClient coffeemachine)
	,workflow (seqTasks +++ "Calculator") 	 				"Calculator" 						(runOnClient calculator)		
	]

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
					enterChoice  ("Insert coins","Please insert a coin...") [] coins
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


Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist basicAPIExamples)) world


