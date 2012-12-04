module v

import StdDynamic, StdEnv
import StdDynamicFileIO

:: Tree a = Node a (Tree a) (Tree a) | Leaf | Test [Tree a] Real

Start world
	#! (ok,world)
		= writeDynamic "value" dt world
	| not ok
		= abort "could not write dynamic"
	= (dt,world)
where 
	dt = dynamic (Node 98 tree2 tree2);
	
	tree2 = (Node 2 (Node 1 Leaf Leaf) Leaf)	// 2 nodes and 3 Leafs

	double_tree t 
		= Node 29 t t;

