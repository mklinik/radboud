module tryout

import iTasks
import dictionaryUtil
import iDataTrivial
import StdMisc
from   TSt			import Task 

Start world = startEngine tryout world

// IMPORTANT: this example can only be used in combination with the very latest Clean compiler
// to be downloaded from the intranet !!!!!!!!!!!!

// workflow to change:

tryout :: [Workflow]
tryout
=	[{ name		= "tryout"
	 , label	= "tryout"
	 , roles	= []
	 , mainTask	= test2 >>| return Void
	 }]

// ***************************************
// this is the flow with handlers that has to be changed

myChangedWorkflow :: (Int -> (Task a)) (Int -> b -> (Task a)) e ChangeCondition -> Task [a] | TC e & iData a & iData b
myChangedWorkflow normalTask alternativeTask whatToApply whenToApply
	= pushChangeRequest whenToApply whatToApply myWorkflow
where
	myWorkflow 
		= 	parallel "andTasks"  (\_ -> False) (\b l -> l)  [(toString i, myTask i) \\ i <- [0..5]] >>|
			parallel "andTasks"  (\_ -> False) (\b l -> l)  [(toString i, myTask i) \\ i <- [0..5]]
	where
		myTask val = normalTask val <\/> alternativeTask val

// ***************************************
// replace 50% of the normaltask by iTaskEditor 

test1 :: Task [Int]
test1 = myChangedWorkflow normalTask alternativeTask whatToApply whenToApply

normalTask :: a -> Task a | iData a 
normalTask val = editTask ("Normal OK") val 

alternativeTask :: a Dynamic -> Task a | iData a
alternativeTask val dyn = fromDynamic dyn val

whatToApply :: Dynamic
whatToApply = dynamic  iTaskEditor :: A.a: a -> Task a | iData a

whenToApply :: ChangeCondition
whenToApply = CC (pred 30)
where
	pred 0 tst =	({newCondition = Nothing, 				 isApplicable = False, applyChange = False},tst)
	pred n tst =	({newCondition = Just (CC (pred (n-1))), isApplicable = True,  applyChange = isEven n},tst)
	
// ***************************************
// same, but now replace normaltask by (iTaskDelegate iTaskEditor) 

test2 ::  Task [Int]
test2 = myChangedWorkflow normalTask alternativeTask whatToApply2 whenToApply

whatToApply2 :: Dynamic
whatToApply2 = toDynamic (iTaskDelegate iTaskEditor)

// ***************************************
// same, but now replace normaltask by (iTaskDelegate normaltask) 

test3 :: Task [Int]
test3 = myChangedWorkflow normalTask alternativeTask whatToApply3 whenToApply

whatToApply3 :: Dynamic
whatToApply3 = toDynamic (iTaskDelegate normalTask)

// ***************************************
// attempt to define an alternative definition for @: 

/*
(@:^) infix 3 :: !UserId !(LabeledTask a) -> Task a	| iData a 
(@:^) uid ltaska =  uid @: (ltaska <\/> catch)
where
	catch ::  MyChange -> Taska
	catch (NewProperties prop) = 
*/

// ***************************************
// Some simple iTask editors to play with...

iTaskEditor :: (a -> Task a) | iData a
iTaskEditor = \a -> editTask "Editor" a

iTaskDelegate :: ((a -> Task b) a -> Task b) | iData b
iTaskDelegate = \ataskb vala -> delegateTask ataskb vala
where
	delegateTask ataskb vala
	=							[Text "Choose persons you want to delegate work to:",BrTag [],BrTag []] 
								?>>	chooseUser
		>>= \(wid,worker) -> 	getCurrentUser
		>>= \(_,me) ->			wid @: ("Task for " +++ me, ataskb vala)
		>>= \result -> 			[Text ("Result from " +++ worker), toHtml result] 
								?>> editTask "OK" Void 
		>>= \_ ->				return result

// ***************************************
// casting section...
// waiting for John to enable the storage of overloaded functions into a Dynamic....

:: TF a :== a -> Task a

toDynamic :: (A.a: TF a | iData a) -> Dynamic
toDynamic ataska
      #! f = cast1 ataska
     = dynamic f :: (A.a: (Dictionary_iData a) a -> Task a)

fromDynamic :: Dynamic -> TF a | iData a
fromDynamic (ataska :: A.b: (Dictionary_iData b) b -> Task b)
= cast2 ataska
fromDynamic else
    = abort "dynamic type error"
    
cast1 :: !(A.a: TF a | iData a) -> ((Dictionary_iData a) -> TF a)
cast1 fun 
= code {	fill_a 0 1
      		pop_a 1
 }

cast2 :: !((Dictionary_iData b) -> TF b) -> TF b | iData b
cast2 fun = cast3 (papdf fun)
where
   papdf :: !((Dictionary_iData a) -> TF a) b -> b | iData a
   papdf f _ = undef

   cast3 :: !(b->b) -> TF b
   cast3 f_d 
   = code {	repl_args 2 2
           	push_a 1
            update_a 1 2
            update_a 0 1
            pop_a 1
        	jsr_ap 1
            fill_a 0 1
            pop_a 1
	}

// ********************************
// some simple tests...

simple :: (A.a: a -> Task a | BoxediData a) -> Task Int
simple ataska = ataska 3

simple2 :: (A.a: a -> Task a | BoxediData a) -> (a -> Task a) | BoxediData a
simple2 ataska = ataska

combine :: (a -> Task a) | BoxediData a
combine = iTaskDelegate iTaskEditor

combine2 :: (A.b: (a -> Task b) a -> Task b | BoxediData b) (A.c: c -> Task c | BoxediData c) -> (a -> Task a) | BoxediData a
combine2 f g = f g

