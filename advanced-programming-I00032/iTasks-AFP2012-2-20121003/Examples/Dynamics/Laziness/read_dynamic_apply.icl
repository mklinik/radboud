module read_dynamic_apply

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
		= readDynamic "bool" world
	| not ok
		= abort " could not read";
		
	| size (get_real d) <> 0

	# (_,world)
		= writeDynamic "bool2" d world
	= world
where
	get_real (_ :: X)
		= "detected X"
	get_real _
		= "no real"
