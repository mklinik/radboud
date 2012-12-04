implementation module derived_combinators

import StdBool, StdTuple, StdMisc, StdInt, StdList, StdOrdList
from   StdFunc import const, id, o
from   StdList import foldr, hd, isEmpty, map, ++, updateAt
import iTask_semantics

//=== Shorthand combinators ===

(@@) infixl 1 :: (Task` a) (a -> b) -> Task` b | iTask` a & iTask` b
(@@) ta f					= ta @@? (fmapt f)

fmapt f NoVal`				= NoVal`
fmapt f (Val` v s)			= Val` (f v) s

(.||.) infixr 3 :: (Task` a) (Task` a) -> Task` a  | iTask` a
(.||.) ta1 ta2				= any` [ta1,ta2]

(.&&.) infixr 4 ::  (Task` a) (Task` b) -> (Task` (a, b))  | iTask` a &  iTask` b
(.&&.) ta tb				= parallel` [const (ta @@? fmapt Left),const (tb @@? fmapt Right)] @@? tup

tup (Val` [(_,Val` (Left a) _), (_,Val` (Right b) _)] s)
							= Val` (a,b) s
tup _						= NoVal`

all` :: [Task` a] -> Task` [a] | iTask` a
all` tasks					= parallel` (map const tasks) @@ (map (getValue o snd))

any` :: [Task` a] -> Task` a | iTask` a
any` tasks					= parallel` (map const tasks) @@? firstResult

firstResult (Val` rs _)		= hd ([Val`  r  Stable` \\ (_,Val`  r Stable`)   <- rs]
	                                     ++ 
								  [Val`  r Unstable`\\ (_,Val`  r Unstable`) <- sortBy (\a b -> fst a > fst b) rs] 
								         ++ 
								  [NoVal`]
								 )
firstResult _				= NoVal`


(##?) infixl 1 :: (Task` a) (a -> Bool) -> (Task` a)	| iTask` a
(##?) ta pred				= ta @@? test 
where
	test NoVal`				= NoVal`
	test (Val` a s)			= if (pred a) (Val` a s) NoVal`

(>>>!) infixl 1 :: (Task` a) (a -> Task` b) -> (Task` b)| iTask` a & iTask` b
(>>>!) ta ftb				= ta >>>* [OnAction` (Action "Next") isValue (ftb o getValue)]

(>>>>) infixl 1 :: (Task` a) (a -> Task` b) -> (Task` b)| iTask` a & iTask` b
(>>>>) ta ftb				= ta >>>* [OnValue` isValue (ftb o getValue)]

(>>>=) infixl 1 :: (Task` a) (a -> Task` b) -> Task` b | iTask` a & iTask` b
(>>>=) ta ftb				= ta >>>* [OnValue`                      isStable (ftb o getValue)
							          ,OnAction` (Action "Continue") isValue  (ftb o getValue)
							          ]

try` :: (Task` a) (Ex -> Task` a) -> Task` a | iTask` a
try` ta handler				= ta >>>* [OnException`  handler]
