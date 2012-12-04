definition module fixed

//	********************************************************************************
//	Clean Standard Game library, version 1.2.2
//	
//	Author:   Mike Wiering
//	Modified: 7 Sept 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

FIXED_POINT_MUL_VALUE :== 256.0

fix :: !Real -> Int
fxr :: !Int -> Real

/*
class fix a :: a -> Int
instance fix Real
instance fix Int
*/
