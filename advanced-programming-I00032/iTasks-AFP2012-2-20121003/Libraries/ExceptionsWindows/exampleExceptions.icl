module exampleExceptions

import StdInt, StdString, StdException
import StdTuple

:: Tree = Node Tree Tree | Leaf Int | InvalidLeaf

:: MyException = CannotSum Tree

Start world 
//	= throwException world
	= checkException world
//	= catchAnyException world
//	= catchSumException world

throwException :: *World -> (Int, *World)
throwException world = (sumOfTree, world)

checkException :: *World -> (MaybeException Int, *World)
checkException world = getException sumOfTree world

catchAnyException :: *World -> (Int, *World)
catchAnyException world = (sumOfTree catchAll \_ -> 0) world

catchSumException :: *World -> (Int, *World)
catchSumException world = (sumOfTree catch \(CannotSum _) -> 0) world

sumOfTree :: Int
sumOfTree = sumTree (Node (Leaf 1) (Node InvalidLeaf (Leaf 3)))

sumTree :: Tree -> Int
sumTree (Leaf x) = x
sumTree (Node l r) = sumTree l + sumTree r
sumTree x = raise [x] // (CannotSum x)
