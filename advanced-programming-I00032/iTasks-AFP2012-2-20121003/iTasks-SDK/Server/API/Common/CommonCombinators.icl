implementation module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, GenRecord, Text, Time, Tuple, List_NG
import Util, Either, GenVisualize, GenUpdate
from StdFunc			import id, const, o
from SystemTypes		import :: User(..), :: Note(..)
from TaskState			import :: TaskTree(..), :: DeferredJSON
from SystemData			import randomInt, topLevelTasks
from Map				import qualified put

import CoreTasks, CoreCombinators, InteractionTasks, LayoutCombinators

(>>*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task b | iTask a & iTask b
(>>*) task steps = step task steps 

(>>=) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>=) taska taskbf = step taska [WithResult ActionContinue (const True) taskbf, WhenStable taskbf]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska [WithResult ActionContinue (const True) taskbf]

(>>|) infixl 1 :: !(Task a) (Task b) -> Task b | iTask a & iTask b
(>>|) taska taskb = step taska [WithResult ActionContinue (const True) (const taskb), WhenStable (const taskb)]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>| return x

(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b | iTask a & iTask b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b | iTask a & iTask b
(@) task f = transform (fmap f) task

(@>) infixl 1 :: !(Task a) !((TaskValue a) r -> Maybe w, ReadWriteShared r w) -> Task a | iTask a
(@>) task (f,share) = project f share task

(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
(@>>) a t = tune a t

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try task handler = step task [WhenStable return, Catch handler]

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll task handler = step task [WhenStable return, CatchAll handler]

//Helper functions for projections
projectJust :: (Maybe a) r -> Maybe (Maybe a)
projectJust mba _ = Just mba
/*
* When a task is assigned to a user a synchronous task instance process is created.
* It is created once and loaded and evaluated on later runs.
*/
assign :: !ManagementMeta !(Task a) -> Task a | iTask a
assign props task
	=	parallel Void
			[(Embedded, \s -> processControl s),(Detached props, \_ -> task)]
	@?	result
where
	processControl tlist
		= viewSharedInformation (Title "Waiting for result") [ViewWith toView] (taskListMeta tlist) @? const NoValue
					
	toView [_,{TaskListItem|progressMeta=Just p,managementMeta=Just m}]=
		{ assignedTo	= toString m.ManagementMeta.worker
		, issuedBy		= toString p.ProgressMeta.issuedBy
		, issuedAt		= p.ProgressMeta.issuedAt
		, priority		= m.ManagementMeta.priority
		, firstWorkedOn	= p.ProgressMeta.firstEvent
		, lastWorkedOn	= p.ProgressMeta.latestEvent
		}	
	result (Value [_,(_,v)] _)	= v
	result _					= NoValue

instance toString UserConstraint
where
	toString AnyUser				= "Anybody"
	toString (UserWithId uid)		= uid
	toString (UserWithRole role)	= "Any user with role " +++ role

:: ProcessControlView =	{ assignedTo	:: !String
						, issuedBy		:: !String
						, issuedAt		:: !DateTime
						, priority		:: !TaskPriority
						, firstWorkedOn	:: !Maybe DateTime
						, lastWorkedOn	:: !Maybe DateTime
						}
derive class iTask ProcessControlView
derive class GenRecord ProcessControlView, UserConstraint, ManagementMeta, TaskPriority

(@:) infix 3 :: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker
(@:) worker task = assign {noMeta & worker = toUserConstraint worker} task

justdo :: !(Task (Maybe a)) -> Task a | iTask a
justdo task
= task >>= \r -> case r of
	Just x	= return x
	Nothing	= throw ("The task returned nothing.")

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence _ tasks = seqTasks tasks
where
	seqTasks []		= return []
	seqTasks [t:ts]	= t >>= \a -> seqTasks ts >>= \as -> return [a:as]

//Repeat task until the predicate holds (loops if the predicate is false)
(<!) infixl 6 :: !(Task a) !(a -> .Bool) -> Task a | iTask a
(<!) task pred
	= parallel Void [(Embedded, checked pred task)] <<@ SetLayout (partLayout 0) @? res
where
	checked pred task tlist
		=	task
		>>* [WhenStable (\a -> if (pred a) (return (Just a)) (restart (checked pred task) tlist))]
		
	res (Value [(_,Value (Just a) _)] s)	= Value a s
	res _									= NoValue
	
	restart task tlist
		=	get (taskListMeta tlist)
		>>= \[{TaskListItem|taskId}:_] ->
			removeTask taskId tlist
		>>| appendTask Embedded task tlist
		@	const Nothing

forever :: !(Task a) -> Task a | iTask a	
forever	t = (t <! (const False))

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = anyTask [taska,taskb]

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res
where
	res	(Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res			
where
	res	(Value [(_,Value (Left a) s),_] _)	= Value a s
	res _									= NoValue
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel Void
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] @? res
where
	res (Value [(_,Value (Left a) _),(_,Value (Right b) _)] s)	= Value (a,b) s
	res _														= NoValue

feedForward :: !d (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | descr d & iTask a & iTask b
feedForward desc taska taskbf = parallel desc
	[(Embedded, \s -> taska @ Left)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (taskListState s))) @ Right)
	] @? res
where
	prj [Value (Left a) _,_]		= Just a
	prj _							= Nothing
	
	res (Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(>&>) infixl 1  :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
(>&>) taska taskbf = feedForward Void taska taskbf
				
:: ProcessOverviewView =	{ index			:: !Hidden Int
							, subject		:: !Display String
							, assignedTo	:: !User
							}

derive class iTask ProcessOverviewView

anyTask :: ![Task a] -> Task a | iTask a
anyTask tasks
	= parallel Void [(Embedded,const t) \\ t <- tasks] @? res
where
	res (Value l _) = hd ([v \\ (_,v=:(Value _ Stable)) <- l] ++ [v \\ (_,v=:(Value _ _)) <- sortBy (\a b -> fst a > fst b) l] ++ [NoValue])
	res _			= NoValue

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks
	= parallel Void
		[(Embedded,const t) \\ t <- tasks] @ res
where
	res l	= [v \\ (_,Value v _) <- l]
				
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = (taska @ Left) -||- (taskb @ Right)

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = get randomInt >>= \i -> return (list !! ((abs i) rem (length list)))

repeatTask :: !(a -> Task a) !(a -> Bool) a -> Task a | iTask a
repeatTask task pred a =
	task a >>= \na -> if (pred na) (return na) (repeatTask task pred na)

whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchanged share task
	= 	((get share >>= \val -> (wait Void ((=!=) val) share @ const Nothing) -||- (task val @ Just) <<@ SetLayout (partLayout 1)) <! isJust)
	@	fromJust

whileUnchangedWith :: !(r r -> Bool) !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchangedWith eq share task
	= 	((get share >>= \val -> (wait Void (eq val) share @ const Nothing) -||- (task val @ Just) <<@ SetLayout (partLayout 1)) <! isJust)
	@	fromJust

appendTopLevelTask :: !ManagementMeta !(Task a) -> Task TaskId | iTask a
appendTopLevelTask props task = appendTask (Detached props) (\_ -> task @ const Void) topLevelTasks

appendTopLevelTaskFor :: !worker !(Task a) -> Task TaskId | iTask a & toUserConstraint worker
appendTopLevelTaskFor worker task = appendTopLevelTask {noMeta & worker = toUserConstraint worker} task
			
instance tune Window
where tune Window task = task

valToMaybe (Value v _)  = Just v
valToMaybe NoValue		= Nothing

Always :: Action (Task b) -> TaskStep a b
Always a t = OnAction a (\_ -> True) (\_ -> t)

AnyTime :: Action ((Maybe a) -> Task b)	-> TaskStep a b
AnyTime a f = OnAction a (\_ -> True) (f o valToMaybe)
	
WithResult :: Action (a -> Bool) (a -> Task b) -> TaskStep a b
WithResult a p f = OnAction a (maybe False p o valToMaybe) (f o fromJust o valToMaybe)
	
WithoutResult :: Action (Task b) -> TaskStep a b
WithoutResult a t = OnAction a (isNothing o valToMaybe) (\_ -> t)
	
WhenValid :: (a -> Bool) (a -> Task b) -> TaskStep a b
WhenValid p f = OnValue (maybe False p o valToMaybe) (f o fromJust o valToMaybe)

WhenStable :: (a -> Task b) -> TaskStep a b
WhenStable f = OnValue isStable (\(Value a _) -> f a)
where
	isStable (Value _ Stable)	= True
	isStable _					= False
	
Catch :: (e -> Task b) -> TaskStep a b | iTask e
Catch f = OnException f

CatchAll :: (String -> Task b) -> TaskStep a b
CatchAll f = OnAllExceptions f
