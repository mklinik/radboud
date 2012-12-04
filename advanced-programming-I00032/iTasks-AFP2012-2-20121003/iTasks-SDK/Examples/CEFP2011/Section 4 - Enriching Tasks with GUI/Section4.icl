implementation module Section4

// Examples showing the extension of editors with buttons

import iTasks
from Section3 import view, positive

derive bimap (,), Maybe

Start :: *World -> *World
Start world = startEngine (manageWorkflows flows4) world

flows4 :: [Workflow]
flows4 
	=   [ workflow "CEFP/Section 4 - Enriching Tasks with GUI/1. Simple Question" 				"Only one answer possible..." 					(view ask)
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/2. Absolute Int"					"Guaranteed absolute integer"					(view absolute)
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/3. Form for [Person]" 			"Form for [Person]" 							(view personList6)
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/4. Accept only an even number" 	"Type in an even number" 						(view askEven)
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/5. Only even" 					"Either the odd or even buttons can be chosen" 	(view (oddOrEvenButtons True))
//		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/6. Dynamic number of buttons" 	"Dynamic number of buttons to choose from" 		(forever (view (positive >>= actions)))
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/7. Dynamic number of buttons" 	"Order pressed is remembered" 					(view (dynButtons [1..10] []))
		, workflow "CEFP/Section 4 - Enriching Tasks with GUI/8. Palindrome exercise" 			"Palindrome" 									palindrome
		]
		
// simple question with buttons

ask :: Task Bool
ask
	=		viewInformation "Do you like the iTask system ?" [] Void
		>?* [(ActionYes, Always (viewInformation "Thank you !" [] True))
			,(ActionNo,  Always (viewInformation "Perhaps you did not onderstand the question" [] False >>| ask))
			]

// absolute number
absolute :: Task Int
absolute
	=		enterInformation "Enter a number" []
		>?*	[(Action "Always",		Always (return 42))
			,(Action "If Valid",	IfValid (\x -> return (abs x)))
			,(Action "Sometimes",	Sometimes (onlyIf  (\x -> x >= 0) return))
			]

// person list once more

:: Person 	= 	{ firstName    	:: String
		      	, surName  		:: String
		      	, dateOfBirth 	:: Date
		      	, gender	 	:: Gender
		      	}
:: Gender 	=	Male
			|	Female

derive class iTask Person, Gender

personList6 :: Task [Person]
personList6
	=         enterInformation "Please fill in the form" []
		>?*  [(Action "Add one", IfValid (\p -> personList6 >>= \ps -> return [p:ps]))  
			 ,(Action "Done",    IfValid (\p -> return [p]))
			 ,(ActionQuit,    Always  (return []))
			 ]

// accept only an even number 

askEven :: Task Int
askEven
	=		enterInformation "Please type in an even number..." [] 
		>?* [(ActionOk, Sometimes (onlyIf isEven return))
			]

onlyIf :: (a -> Bool) (a -> Task b) (InformationState a) -> Maybe (Task b)
onlyIf pred taskf  s
| s.localValid  && pred  s.modelValue  = Just (taskf s.modelValue)
| otherwise = Nothing


// create n buttons

actions :: Int -> Task Int
actions n = chooseAction
            [  (Action ("label " +++ toString i),i)
            \\ i <- [1..n]
            ]

// n buttons, order pressed is remembered

dynButtons :: [Int] [Int] -> Task [Int]
dynButtons [] accu = return accu
dynButtons numbers accu
	=					updateInformation "Choose a button" [] Void
		>?* 			[(Action (toString i), Always (return index)) \\ i <- numbers & index <- [0..]] 
		>>= \index ->	dynButtons (removeAt index numbers) [numbers!!index:accu]

	

// show only the even or odd buttons

oddOrEvenButtons :: Bool ->Task Int
oddOrEvenButtons even
	=			updateInformation "Choose a button" [] Void
		>?*		[ (Action "Odd",  				Sometimes (onlyIf (\_ -> even)     (\_ -> oddOrEvenButtons (not even))))
				, (Action "Even",				Sometimes (onlyIf (\_ -> not even) (\_ -> oddOrEvenButtons (not even))))
				: [ (Action (toString i),  Always (return i))
				  \\ i <- [0..9] | if even (isEven i) (isOdd i)
				  ]
				] 
	
// palindrome	

palindrome :: Task (Maybe String)
palindrome 
	= 				enterInformation "Please enter a text" []
		>?*	 		[(ActionOk,   Sometimes (onlyIf isPalindrome (return o Just)))
					,(ActionQuit, Always (return Nothing))
					]
where	
	isPalindrome :: String -> Bool
	isPalindrome str	= chars == reverse chars
	where
		chars :: [Char]
		chars			= fromString str
	