module ReadDynamic

import StdDynamic
import StdEnv
import StdDynamicFileIO

:: Tree b = Leaf | Node b (Tree b) (Tree b)
	
Start world

	# (ok,d,world)
		= readDynamic "test" world
	| not ok
		= abort " could not read"
	= (f d,world)

f (x :: [Tree Char])	= x
f _						= abort "failed"
		
