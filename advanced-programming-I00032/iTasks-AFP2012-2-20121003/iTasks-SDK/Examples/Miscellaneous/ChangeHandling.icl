implementation module ChangeHandling

import iTasks
import StdMisc


//Start world = startEngine changeHandlingExample world

//changeHandlingExample :: [Workflow]
//changeHandlingExample = []
/*
changeHandlingExample
=	[{ name		= "Examples/Miscellaneous/Change handling"
	 , label	= "Change example"
	 , roles	= []
	 , mainTask	= addMultiUsertask >>| return Void
	 }]

editSpecial :: Int -> Task Int
editSpecial i 
	= 				chooseTask []
						[( "normal",	mytask)
						, ("absurd",	pushChangeRequest (CC (pred 30)) ourList mytask)
						] >>= editSpecial
	where
		mytask = editTask ("OK" <+++ i) i  <\/> myChange (editSpecial i)

		pred _ tst =	({newCondition = Nothing, 				 isApplicable = False, applyChange = True},tst)
//		pred n tst = (True,Just (CC (pred (n-1))),tst)
		
ourList :: [Dynamic]
ourList
	=				[ dynamic ("Int task",		editTask "repair" 1)
					, dynamic ("Bool task",		editTask "repair" True)
					, dynamic ("String task", 	editTask "repair" "Hello world")
					]

myChange task ourList
	= chooseTask [] (tasksOfSameType task ourList)
where
	tasksOfSameType :: (Task a) [Dynamic] -> [LabeledTask a] | iData a
	tasksOfSameType t [] = [("Standard Task",t),("Simple Editor",editTask "Edit" createDefault)]
	tasksOfSameType t [(t` :: LabeledTask a^) : ts] = [t` : tasksOfSameType t ts]
	tasksOfSameType t [t` : ts] = tasksOfSameType t ts
	


edit` val = editTask ("Normal OK" <+++ val) val  <\/> alternative val
where
	alternative :: a Void -> Task a | iData a & toString a
	alternative val _ = 	editTask ("Alternative OK" <+++ val) val
	

myBunch 
	= 	parallel "andTasks"  (\_ -> False) (\_ list -> list)  [(toString i, edit` i) \\ i <- [0..5]] >>|
		parallel "andTasks"  (\_ -> False) (\_ list -> list)  [(toString i, edit` i) \\ i <- [0..5]]


doTest = pushChangeRequest (CC (pred 30)) Void myBunch
where
		pred 0 tst =	({newCondition = Nothing, 				 isApplicable = False, applyChange = False},tst)
		pred n tst =	({newCondition = Just (CC (pred (n-1))), isApplicable = True,  applyChange = isEven n},tst)



addMultiUsertask :: Task Int 
addMultiUsertask
	=					editTask "First Value" createDefault 
		>>= \s1 ->		chooseUser
		>>= \(i,_) ->	i @: ("Your work", editTask "Second Value" createDefault)
		>>= \s2 ->		editTask "Sum" (s1 + s2)
*/



