implementation module ostick

import StdEnv
import ostoolbox

::	Tick	:== Int

pack_tick	::	!Int -> Tick
pack_tick i = i

unpack_tick	::	!Tick -> Int
unpack_tick tick = tick

os_getcurrenttick :: !*World -> (!Tick, !*World)
os_getcurrenttick world
	= (fst (winGetTickCount 42), world)

winGetTickCount ::  !*OSToolbox -> (!Int, !*OSToolbox)
winGetTickCount _
	= code
	{
		.inline WinGetTickCount
			ccall WinGetTickCount "I-II"
		.end
	}
