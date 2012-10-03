module read_read_apply

import StdDynamic
import StdEnv
import StdDynamicFileIO

:: X
	= {
		x_r1	:: Int
	,	x_r2	:: Int 
	};

Start world
	# (ok,d,world)
		= readDynamic "bool2" world
	| not ok
		= abort " could not read";
		
	| size (get_real d) <> 0
	= (d,world)

where
	get_real (_ :: X) 
		= "detected X"
	get_real _
		= "not a real"
