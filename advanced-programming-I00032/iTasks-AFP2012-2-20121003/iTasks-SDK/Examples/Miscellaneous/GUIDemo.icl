implementation module GUIDemo

import iTasks

guiDemoExample :: [Workflow]
guiDemoExample
	= [ workflow "Examples/Miscellaneous/GUI Demo" "Demo of user interface elements" guiDemo]

:: Address =
	{ street		:: String
	, number		:: Int
	, postalCode	:: String
	, city			:: String
	}

:: Person =
	{ name			:: String
	, cool			:: Bool
	, dob			:: Date
	, tob			:: Time
	, age			:: Maybe Int
	, address		:: Address
	, grades		:: [Int]
	, pet			:: String
	, note			:: Maybe Note
	, tree			:: Tree` Int String
	, luckyNo		:: Int
	}

:: Tree` a b = Leaf` b | Node` (Tree` a b) a (Tree` a b)

derive class iTask	Person, Address, Tree`

address = {Address | street = "Heyendaalseweg", number = 135, postalCode = "6525 AJ", city = "Nijmegen"}
person	= {Person | name	= "John Doe"
				  , cool	= True
				  , dob		= {Date | year = 1978, mon = 4, day = 1}
				  , tob		= {Time | hour = 13, min = 42, sec = 0}
				  , age		= Just 23
				  , address	= address
				  , grades	= []
				  , pet		= "Cat"
				  , note	= Nothing
				  , tree	= Leaf` "Tree"
				  , luckyNo	= 42
				  }
guiDemo :: Task Person
guiDemo
	=	updateInformation ("Update person","You may change this information") [] person
	>>=	viewInformation ("Summary","This is the information you entered") []