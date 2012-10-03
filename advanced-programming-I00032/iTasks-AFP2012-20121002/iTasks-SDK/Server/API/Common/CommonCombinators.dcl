definition module CommonCombinators
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators.
*/
import CoreCombinators, LayoutCombinators
import Either
from SystemTypes		import :: User
from Map				import :: Map

/**
* Infix shorthand for step combinator
* 
* @param Task: The task for which continuations are defined
* @param The possible continuations
* @return The continuation's result
*
* @gin False
*/
(>>*) infixl 1 :: !(Task a) ![TaskStep a b] -> Task b | iTask a & iTask b

//Standard monadic operations:

/**
* Combines two tasks sequentially. The first task is executed first. When it is finished
* the second task is executed with the result of the first task as parameter.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
* 
* @gin False
*/
(>>=) infixl 1 	:: !(Task a) !(a -> Task b) 			-> Task b		| iTask a & iTask b
/**
* Combines two tasks sequentially but waits for user input to confirm the completion of
* the first task.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
* 
* @gin False
*/
(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
/**
* Combines two tasks sequentially just as >>=, but the result of the first task is disregarded.
*
* @param First: The first task to be executed
* @param Second: The second task to be executed
* @return The combined task
*
* @gin False
*/
(>>|) infixl 1 :: !(Task a) (Task b)					-> Task b		| iTask a & iTask b
/**
* Combines two tasks sequentially just as >>=, but the result of the second task is disregarded.
*
* @param First: The first task to be executed
* @param Second: The second task to be executed
* 
* @return The combined task
*
* @gin False
*/
(>>^) infixl 1 :: !(Task a) (Task b) -> Task a| iTask a & iTask b
/**
* Infix shorthand for transform combinator
* 
* @param Task: The task on which the transform should be applied
* @param The transformation function to apply
* @return The transformed task
*
* @gin False
*/
(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b | iTask a & iTask b
/**
* Infix shorthand for transform combinator which only deals which only transforms valid results
* 
* @param Task: The task on which the transform should be applied
* @param The transformation function to apply
* @return The transformed task
*
* @gin False
*/
(@) infixl 1 :: !(Task a) !(a -> b) -> Task b | iTask a & iTask b
/**
* Infix shorthand for project combinator
* 
* @param Task: The task of which the result should be projected
* @param The projection function and share
* @return The tasks result
*
* @gin False
*/
(@>) infixl 1 :: !(Task a) !((TaskValue a) r -> Maybe w, ReadWriteShared r w) -> Task a | iTask a
/**
* Infix shorthands for the (overloaded) tune combinator.
*/
(<<@) infixl 2 :: !(Task a) !b	-> Task a | tune b
(@>>) infixr 2 :: !b !(Task a)	-> Task a | tune b
/**
* Exception combinator.
*
* @param Task: The normal task which will possibly raise an exception of type e
* @param Handler: The exception handling task which gets the exception as parameter
* @return The combined task
*
* @gin-title Try block
* @gin-icon catch
*/
try 		:: !(Task a) (e -> Task a) 			-> Task a 	| iTask a & iTask, toString e
/**
* Catches all exceptions.
*
* @param Task: The normal task which will possibly raise an exception of any type
* @param Handler: The exception handling task
* 
* @gin-title Catch all exceptions
* @gin-icon catch
*/
catchAll	:: !(Task a) (String -> Task a)		-> Task a | iTask a
/**
* Assign a task to a(nother) user.
*
* @param Manager properties: The initial manager properties indicating the user to which the task is delegated, a priority and possibly a deadline
* @param Action menu: A function generating a menu for the process delegated to the user
* @param Task: The task that is to be delegated
*
* @return The combined task
*
* @gin False
*/ 
assign :: !ManagementMeta !(Task a) -> Task a | iTask a
/**
* Assign a task to a user. (no deadline, normal priority)
*
* @param User: The initial UserId of the user to which the task is delegated
* @param Task: The task that is to be delegated.
*
* @return The combined task
*
* @gin-title Assign to user
* @gin-icon user
* @gin-shape assign
*/
(@:) infix 3		:: !worker !(Task a) -> Task a | iTask a & toUserConstraint worker

/**
* Execute a Maybe task that you expect to always return Just.
* Throw an exception if it returns nothing
*
* @param The task that could in theory return Nothing
* @return The result of the task
* 
* @gin False
*/
justdo	:: !(Task (Maybe a)) -> Task a | iTask a

/**
* Execute the list of tasks one after another.
* 
* @param Label: A label for tracing
* @param Tasks: The list of tasks to be executed sequentially
* @return The combined task
* 
* @gin-icon sequence
*/
sequence	:: !String ![Task a] 						-> Task [a]		| iTask a

/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param Task: The task to be looped
* @param Predicate: The predicate over the result of the task to determine if the combination is finished
* @return The combined task
* 
* @gin False
*/
(<!)  infixl 6 	:: !(Task a)  !(a -> .Bool) 			-> Task a 		| iTask a

/**
* Repeats a task infinitely. As soon as the task is stable, it is restarted immediately.
* As a consequence, the combined task never stabilizes.
*
* @param Task: The task that has to be repeated infinitely
* @return The combined task
* 
* @gin False
*/
forever :: !(Task a) -> Task a | iTask a 

/**
* Group two tasks in parallel, return the result of the first completed task.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(-||-) infixr 3 	:: !(Task a) !(Task a) 	-> Task a 				| iTask a

/**
* Group two tasks in parallel, return the result of the right task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(||-)  infixr 3		:: !(Task a) !(Task b)	-> Task b				| iTask a & iTask b

/**
* Group two tasks in parallel, return the result of the left task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
* 
* @gin False
*/
(-||)  infixl 3		:: !(Task a) !(Task b)	-> Task a				| iTask a & iTask b

/** 
* Group two tasks in parallel that both need to be completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The results of both tasks 
* 
* @gin-parallel True
* @gin-title Parallel merge (tuple)
* @gin-icon parallel-merge-tuple
*/
(-&&-) infixr 4 	:: !(Task a) !(Task b) 	-> Task (a,b) 			| iTask a & iTask b

/**
* Feed the result of one task as read-only shared to another 
*/
feedForward :: !d (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | descr d & iTask a & iTask b

//Infix version of feedForward
(>&>) infixl 1  :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b

/**
* Group a list of tasks in parallel.
* The group stops as soon as one result is available which is returned.
*
* @param Tasks: The list of tasks
*
* @return The first result
* 
* @gin-parallel True
* @gin-title Take first completed
* @gin-icon parallel-merge-first
*/
anyTask				:: ![Task a]			-> Task a				| iTask a

/**
* Group a list of tasks in parallel.
* The group stops when all tasks are completed.
*
* @param Tasks: The list of tasks
*
* @return The list of results
* 
* @gin-parallel True
* @gin-title Parallel merge (list)
* @gin-icon parallel-merge-list
*/
allTasks			:: ![Task a]			-> Task [a]				| iTask a

/**
* Group two tasks in parallel of which only one needs to be completed.
* The tasks can have different types. The 'Either' results indicates which task completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the first completed task wrapped in an 'Either'.
* 
* @gin False
*/
eitherTask			:: !(Task a) !(Task b) 	-> Task (Either a b)	| iTask a & iTask b	

/**
* Randomly selects one item from a list.
*
* @param Options: The list of options
*
* @return The chosen item
*/
randomChoice		:: ![a]										-> Task a				| iTask a

/**
* Iterate a task as long as a predicate is not valid.
*
* @param Task function: A task function to repeat. At each iteration the result of the previous iteration is given.
* @param Predicate: A predicate to test if we can stop.
* @param Initial value: An initial value for the first iteration.
*
* @return The result of the last iteration (that thus satisfies the predicate)
* 
* @gin False
*/
repeatTask		:: !(a -> Task a) !(a -> Bool) a 			-> Task a					| iTask a

/**
* Do a task as long while monitoring that a shared state remains unchanged.
* When the share changes the task is restarted
*/
whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchangedWith :: !(r r -> Bool) !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
/**
* Append a task to the set of top level tasks
* 
*/
appendTopLevelTask :: !ManagementMeta !(Task a) -> Task TaskId | iTask a

appendTopLevelTaskFor :: !worker !(Task a) -> Task TaskId | iTask a & toUserConstraint worker

// Additional tuning shortcuts
instance tune Window		//Indicate that this task should be a window

//Common derived task steps
Always			:: Action (Task b)						-> TaskStep a b
AnyTime 		:: Action ((Maybe a) -> Task b)			-> TaskStep a b
WithResult 		:: Action (a -> Bool) (a -> Task b)		-> TaskStep a b
WithoutResult	:: Action (Task b)						-> TaskStep a b
WhenValid		:: (a -> Bool) (a -> Task b)			-> TaskStep a b
WhenStable		:: (a -> Task b)						-> TaskStep a b
Catch			:: (e -> Task b)						-> TaskStep a b | iTask e
CatchAll		:: (String -> Task b)					-> TaskStep a b


