module VisualizeTest

import iTasks

//Testing the visualization of standard types
:: TestType =
	{ name			:: String			//Simple string
	, isCoder		:: Bool				//Simple boolean
	
	, age			:: Int				//Simple integer
	, weight		:: Real				//Simple real
	, initial		:: Char				//Simple char
	, gender		:: Gender			//Simple constructor
	
	, looks			:: Looks			//Nested record
	, contact		:: User				//User reference
	
	, description	:: Note				//Note
	, lastSeen		:: DateTime			//Date + Time
	, products		:: Maybe [Product]	//Optional list of composed items
	, hobby			:: HasHobby			//Constructor with field
	, car			:: String			//String (to test constructor above)
	, account		:: Maybe Account	//Optional record
	}

:: Gender = Male | Female

:: Looks =
	{ eyeColor		:: Maybe (Int,Int,Int)
	, moustache		:: Bool
	}
	
:: Product =
	{ productName	:: String
	, price			:: Currency
	, photo			:: Maybe Document
	, sizes			:: [String]
	}

:: HasHobby = HasNoHobby | HasHobby Note

:: Account =
	{ username		:: String
	, password		:: Password
	}
	
derive class iTask TestType, Gender, Looks, Product, HasHobby, Account
derive bimap (,), Maybe

testValue =
	{ name 	= "John Doe"
	, isCoder = True
	, age	= 35
	, weight = 87.75
	, initial = 'J'
	, gender = Male
	, contact = RootUser
	, looks = { eyeColor = Just (255,23,0), moustache = False}
	, description = Note "Weird fella"
	, lastSeen = fromString "2008-09-12 10:42:00"
	, products = Just [testProduct]
	, hobby = HasNoHobby
	, car = "Ford Mustang"
	, account = Just { username = "John", password = Password "test123"}
	}

testProduct = 
	{ productName = "Shoe"
	, price = EUR 2300
	, photo = Nothing
	, sizes = ["Large", "Medium", "Small"]
	}
	
testTask = (enterInformation  "New value" "Enter a new value"
		 	-||-
		 	updateInformation "Update value" "Or update the test value" testValue	
		 	) -|| showInstructionAbout "Simple Instruction" "Just click me" "This is the test value"
		 	>>= showMessageAbout "Html visualization" "This is what it looks like as html"


Start :: *World -> *World
Start world = startEngine [workflow "Test" "Test for generic visualization" testTask] world