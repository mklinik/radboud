definition module clCCall_12

//	Clean Object I/O library, version 1.2

from StdIOCommon	import :: Modifiers
from ostoolbox		import :: OSToolbox


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
toModifiers				:: !Int -> Modifiers

winLaunchApp			:: !{#Char} !Bool !*OSToolbox -> (!Bool,!*OSToolbox)
winLaunchApp2			:: !{#Char} !{#Char} !Bool !*OSToolbox -> (!Bool,!*OSToolbox)
winCallProcess			:: !CSTR !CSTR !CSTR !CSTR !CSTR !CSTR !*OSToolbox -> (!Bool,!Int,!*OSToolbox)

winGetModulePath		:: {#Char}
winFileModifiedDate		:: !{#Char} -> (!Bool,!Int,!Int,!Int,!Int,!Int,!Int)
winFileExists			:: !{#Char} ->  Bool

winBeep					:: !*OSToolbox -> *OSToolbox

rand					:: Int

winReleaseCString		:: !CSTR !*OSToolbox -> *OSToolbox
winGetCStringAndFree	:: !CSTR !*OSToolbox -> (!{#Char},!*OSToolbox)
winGetCString			:: !CSTR !*OSToolbox -> (!{#Char},!*OSToolbox)
winMakeCString			:: !{#Char} !*OSToolbox -> (!CSTR,!*OSToolbox)

winGetAppPath			:: CSTR

winSetDoubleDownDist	:: !Int !*OSToolbox ->  *OSToolbox

winGetHorzResolution	:: Int
winGetVertResolution	:: Int

winMaxFixedWindowSize	:: (!Int,!Int)
winMaxScrollWindowSize	:: (!Int,!Int)

//	PA: interfaces added for determining screen width and height.
winScreenYSize			:: !*OSToolbox -> (!Int,!*OSToolbox)
winScreenXSize			:: !*OSToolbox -> (!Int,!*OSToolbox)

winMinimumWinSize		:: (!Int,!Int)

//	PA: function added to get system metrics for width and height of scrollbars.
winScrollbarSize		:: !*OSToolbox -> (!Int,!Int,!*OSToolbox)

/*	PA: two new routines (win(M/S)DIClientToOuterSizeDims added to convert between the
		client and outer size of (M/S)DI windows. The Int argument contains the style flags 
		of the window.
*/
winMDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)
winSDIClientToOuterSizeDims :: !Int !*OSToolbox -> (!Int,!Int,!*OSToolbox)

winPlaySound			:: !{#Char} !*OSToolbox -> (!Bool,!*OSToolbox)
