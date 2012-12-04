module WriteDynamic

import StdEnv
import StdDynamic
import StdDynamicFileIO

:: Tree a = Node a (Tree a) (Tree a) | Leaf

Start world
	#! (ok,world)
		= writeDynamic "test" dt world
	| not ok
		= abort "could not write dynamic"
	= (dt,world)
where 
	dt = dynamic [Node 'a' Leaf Leaf]
