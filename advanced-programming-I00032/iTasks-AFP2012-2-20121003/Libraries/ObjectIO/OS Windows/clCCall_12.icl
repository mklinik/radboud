implementation module clCCall_12

import	StdClass, StdInt
from	StdIOCommon	import :: Modifiers{..}
from	ostoolbox	import :: OSToolbox
import code from "cCCallSystem_121.",
				 "cCCallWindows_121.",
				 "cCrossCallWindows_121.", 
				 "cCrossCall_121.", 
				 "cdebug_121.", 
				 "cpicture_121.",
				 "util_121."


::	CSTR	:==  Int
::	ACCLPTR	:==  Int

MaxRand				:== 32767

WinHelpKey			:== 5
WinEscapeKey		:== 27
WinReturnKey		:== 13
WinTabKey			:== 9
WinDelKey			:== 127
WinBackSpKey		:== 8
WinEndKey			:== 4
WinBeginKey			:== 1
WinPgDownKey		:== 12
WinPgUpKey			:== 11
WinRightKey			:== 29
WinLeftKey			:== 28
WinDownKey			:== 31
WinUpKey			:== 30
WinF1Key			:==	1001
WinF2Key			:==	1002
WinF3Key			:==	1003
WinF4Key			:==	1004
WinF5Key			:==	1005
WinF6Key			:==	1006
WinF7Key			:==	1007
WinF8Key			:==	1008
WinF9Key			:==	1009
WinF10Key			:==	1010
WinF11Key			:==	1011
WinF12Key			:==	1012

CTRLBIT				:== 4
ALTBIT				:== 2
SHIFTBIT			:== 1

KEYREPEAT			:== 4
KEYUP				:== 2
KEYDOWN				:== 1

BUTTONUP			:== 50
BUTTONSTILLDOWN		:== 40
BUTTONTRIPLEDOWN	:== 3
BUTTONDOUBLEDOWN	:== 2
BUTTONDOWN			:== 1
BUTTONSTILLUP		:== 0		/* PA: new constant for passing mouse move events. */


//	PA: moved from windowevent.icl because also used by menuCrossCall_12
toModifiers :: !Int -> Modifiers
toModifiers i
	=	{	shiftDown	= shifton
		,	optionDown	= alton
		,	commandDown	= ctrlon
		,	controlDown	= ctrlon
		,	altDown		= alton
		}
where
	shifton	= i bitand SHIFTBIT <> 0
	alton	= i bitand ALTBIT   <> 0
	ctrlon	= i bitand CTRLBIT  <> 0


winLaunchApp ::  !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
winLaunchApp _ _ _
	= code
	{
		.inline WinLaunchApp
			ccall WinLaunchApp "SII-II"
		.end
	}

winLaunchApp2 :: !{#Char} !{#Char} !Bool !*OSToolbox -> ( !Bool, !*OSToolbox)
winLaunchApp2 _ _ _ _
	= code
	{
		.inline WinLaunchApp2
			ccall WinLaunchApp2 "SSII-II"
		.end
	}

winCallProcess ::  !CSTR !CSTR !CSTR !CSTR !CSTR !CSTR !*OSToolbox -> ( !Bool, !Int, !*OSToolbox)
winCallProcess _ _ _ _ _ _ _
	= code
	{
		.inline WinCallProcess
			ccall WinCallProcess "IIIIIII-III"
		.end
	}

winGetModulePath ::  {#Char}
winGetModulePath
	= code
	{
		.inline WinGetModulePath
			ccall WinGetModulePath "-S"
		.end
	}

winFileModifiedDate ::  !{#Char} -> ( !Bool, !Int, !Int, !Int, !Int, !Int, !Int)
winFileModifiedDate _
	= code
	{
		.inline WinFileModifiedDate
			ccall WinFileModifiedDate "S-IIIIIII"
		.end
	}

winFileExists ::  !{#Char} ->  Bool
winFileExists _
	= code
	{
		.inline WinFileExists
			ccall WinFileExists "S-I"
		.end
	}

winBeep :: !*OSToolbox -> *OSToolbox
winBeep tb
	= code
	{
		.inline WinBeep
			ccall WinBeep "I-I"
		.end
	}

rand ::  Int
rand
	= code
	{
		.inline Rand
			ccall Rand "-I"
		.end
	}

winReleaseCString ::  !CSTR !*OSToolbox ->  *OSToolbox
winReleaseCString _ _
	= code
	{
		.inline WinReleaseCString
			ccall WinReleaseCString "II-I"
		.end
	}

winGetCStringAndFree ::  !CSTR !*OSToolbox -> ( !{#Char}, !*OSToolbox)
winGetCStringAndFree _ _
	= code
	{
		.inline WinGetCStringAndFree
			ccall WinGetCStringAndFree "II-SI"
		.end
	}

winGetCString ::  !CSTR !*OSToolbox -> ( !{#Char}, !*OSToolbox)
winGetCString _ _
	= code
	{
		.inline WinGetCString
			ccall WinGetCString "II-SI"
		.end
	}

winMakeCString ::  !{#Char} !*OSToolbox -> ( !CSTR, !*OSToolbox)
winMakeCString _ _
	= code
	{
		.inline WinMakeCString
			ccall WinMakeCString "SI-II"
		.end
	}

winGetAppPath ::  CSTR
winGetAppPath
	= code
	{
		.inline WinGetAppPath
			ccall WinGetAppPath "-I"
		.end
	}

winSetDoubleDownDist ::  !Int !*OSToolbox ->  *OSToolbox
winSetDoubleDownDist _ _
	= code
	{
		.inline WinSetDoubleDownDist
			ccall WinSetDoubleDownDist "II-I"
		.end
	}

winGetHorzResolution ::  Int
winGetHorzResolution
	= code
	{
		.inline WinGetHorzResolution
			ccall WinGetHorzResolution "-I"
		.end
	}

winGetVertResolution ::  Int
winGetVertResolution
	= code
	{
		.inline WinGetVertResolution
			ccall WinGetVertResolution "-I"
		.end
	}

winMaxFixedWindowSize :: ( !Int, !Int)
winMaxFixedWindowSize
	= code
	{
		.inline WinMaxFixedWindowSize
			ccall WinMaxFixedWindowSize "-II"
		.end
	}

winMaxScrollWindowSize :: ( !Int, !Int)
winMaxScrollWindowSize
	= code
	{
		.inline WinMaxScrollWindowSize
			ccall WinMaxScrollWindowSize "-II"
		.end
	}

//	PA: interface added for determining screen width and height.
winScreenYSize :: !*OSToolbox -> (!Int,!*OSToolbox)
winScreenYSize _
	= code
	{	
		.inline WinScreenYSize
			ccall WinScreenYSize "I-II"
		.end
	}

winScreenXSize :: !*OSToolbox -> (!Int,!*OSToolbox)
winScreenXSize _
	= code
	{	
		.inline WinScreenXSize
			ccall WinScreenXSize "I-II"
		.end
	}

winMinimumWinSize :: ( !Int, !Int)
winMinimumWinSize
	= code
	{
		.inline WinMinimumWinSize
			ccall WinMinimumWinSize "-II"
		.end
	}

//	PA: function added to get system metrics for width and height of scrollbars.
winScrollbarSize :: !*OSToolbox -> ( !Int, !Int, !*OSToolbox )
winScrollbarSize _
	= code
	{	
		.inline WinScrollbarSize
			ccall WinScrollbarSize "I-III"
		.end
	}

/*	PA: two new routines (win(M/S)DIClientToOuterSizeDims added to convert between the
		client and outer size of (M/S)DI windows. The Int argument contains the style flags 
		of the window.
*/
winMDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)
winMDIClientToOuterSizeDims _ _
	= code
	{
		.inline WinMDIClientToOuterSizeDims
			ccall WinMDIClientToOuterSizeDims "II-III"
		.end
	}

winSDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)
winSDIClientToOuterSizeDims _ _
	= code
	{
		.inline WinSDIClientToOuterSizeDims
			ccall WinSDIClientToOuterSizeDims "II-III"
		.end
	}


winPlaySound :: !{#Char} !*OSToolbox -> (!Bool,!*OSToolbox)
winPlaySound _ _
	= code
	{
		.inline WinPlaySound
			ccall WinPlaySound "SI-II"
		.end
	}
