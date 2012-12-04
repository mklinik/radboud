implementation module ostime

import	StdBool, StdClass, StdInt, StdOverloaded
import	ostoolbox

::	OSTime
	=	OSTime !Int

OSMaxTickCount	:==	2^31-1

osMaxTime :: OSTime
osMaxTime = OSTime OSMaxTickCount

osGetTime :: !*OSToolbox -> (!OSTime,!*OSToolbox)
osGetTime tb
	# (tickcount,tb)	= getMessageTime tb
	= (OSTime tickcount,tb)
where
	getMessageTime :: !*OSToolbox -> (!Int,!*OSToolbox)
	getMessageTime tb = winGetTickCount tb

osWait :: !Int .x !*OSToolbox -> (.x,!*OSToolbox)
osWait delay x tb
	= (x,winWait delay tb)

osGetBlinkInterval :: !*OSToolbox -> (!Int,!*OSToolbox)
osGetBlinkInterval tb
	= winGetBlinkTime tb

osGetCurrentTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
osGetCurrentTime tb
	= winGetTime tb

osGetCurrentDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
osGetCurrentDate tb
	= winGetDate tb

instance - OSTime where
	(-) :: !OSTime !OSTime -> OSTime
	(-) (OSTime new) (OSTime old)
		| old<=new	= OSTime (new-old)
		| otherwise	= OSTime (OSMaxTickCount-old+new)

instance < OSTime where
	(<) :: !OSTime !OSTime -> Bool
	(<) (OSTime t1) (OSTime t2) = t1<t2

instance toInt OSTime where
	toInt :: !OSTime -> Int
	toInt (OSTime t) = t

instance fromInt OSTime where
	fromInt :: !Int -> OSTime
	fromInt t = OSTime (max 0 t)


winGetTime :: !*OSToolbox -> (!(!Int,!Int,!Int),!*OSToolbox)
winGetTime tb
	= code
	{
		.inline WinGetTime
			ccall WinGetTime "I-IIII"
		.end
	}

winGetDate :: !*OSToolbox -> (!(!Int,!Int,!Int,!Int),!*OSToolbox)
winGetDate tb
	= code
	{
		.inline WinGetDate
			ccall WinGetDate "I-IIIII"
		.end
	}

winWait :: !Int !*OSToolbox -> *OSToolbox
winWait i tb
	= code
	{
		.inline WinWait
			ccall WinWait "II-I"
		.end
	}

winGetBlinkTime :: !*OSToolbox -> (!Int,!*OSToolbox)
winGetBlinkTime tb
	= code
	{
		.inline WinGetBlinkTime
			ccall WinGetBlinkTime "I-II"
		.end
	}

winGetTickCount ::  !*OSToolbox -> (!Int, !*OSToolbox)
winGetTickCount _
	= code
	{
		.inline WinGetTickCount
			ccall WinGetTickCount "I-II"
		.end
	}
