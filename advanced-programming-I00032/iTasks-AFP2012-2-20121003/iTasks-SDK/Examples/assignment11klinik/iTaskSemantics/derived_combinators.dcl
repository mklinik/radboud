definition module derived_combinators

import iTask_semantics

//Parallel derivate
(.||.) infixr 3	:: (Task` a) (Task` a) -> Task` a       | iTask` a
(.&&.) infixr 4	:: (Task` a) (Task` b) -> Task` (a,b)   | iTask` a & iTask` b
all`			:: [Task` a] -> Task` [a]               | iTask` a
any`			:: [Task` a] -> Task` a                 | iTask` a

//Test predicate
(##?) infixl 1 :: (Task` a) (a -> Bool) -> (Task` a)	| iTask` a

//Explicit user driven next
(>>>!) infixl 1 :: (Task` a) (a -> Task` b) -> (Task` b)| iTask` a & iTask` b
//Advances as soon as left-hand side is valid
(>>>>) infixl 1 :: (Task` a) (a -> Task` b) -> (Task` b)| iTask` a & iTask` b

//Classic bind and try

(>>>=) infixl 1 :: (Task` a) (a -> Task` b) -> Task` b | iTask` a & iTask` b

try` :: (Task` a) (Ex -> Task` a) -> Task` a | iTask` a
