module apply

import StdDynamic, StdEnv
import StdDynamicFileIO

:: Tree a = Node a (Tree a) (Tree a) | Leaf | Test [Tree a] Real

Start world
	// read function
	# (ok,f,world)
		= readDynamic "function" world
	| not ok
		= abort " could not read function"
		
	// value
	# (ok,v,world)
		= readDynamic "value" world
	| not ok
		= abort " could not read value"

	# applied_dynamic
		= apply f v;
		
	#! (ok2,world)
		= writeDynamic "bool" applied_dynamic world

	| not ok2
		= abort "could not write dynamic"

	= (world);
where
	apply (f :: a -> b) (v :: a)
		= dynamic f v 
	apply _ _
		= abort "unmatched"
