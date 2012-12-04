module TreeTest

import iTasks

:: Tree a b = Leaf b | Node (Tree a b) a (Tree a b)

derive bimap (,), Maybe
derive class iTask Tree

test :: Task (Tree Int String)
test = enterInformation "Tree" "Please create a tree of integers and strings"

Start :: *World -> *World
Start world = startEngine [workflow "Tree" "Test with binary tree" test] world