implementation module fixed

import	StdInt, StdReal

FIXED_POINT_MUL_VALUE :== 256.0

fix :: !Real -> Int
fix r = toInt (FIXED_POINT_MUL_VALUE * r)

fxr :: !Int -> Real
fxr i = ((toReal i) / FIXED_POINT_MUL_VALUE)

/*
class fix a :: a -> Int

instance fix Real
where
    fix r = toInt ((toReal FIXED_POINT_MUL_VALUE) * r)

instance fix Int
where
    fix i = (FIXED_POINT_MUL_VALUE * i)
*/
