definition module gameintrface_12

//	********************************************************************************
//	Clean Standard Game library, version 1.2.2
//	
//	Author:   Mike Wiering
//	Modified: 7 Sept 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

from	ostoolbox		import :: OSToolbox
from	pictCCall_12	import :: RGBcolor


::  BID     :== Int  // Bitmap ID
::  MAPID   :== Int  // Map layer ID
::  GRESULT :== Int  // Game function result code
::  SEQID   :== Int  // Block Sequence ID


/* integer value for nothing and everything */
NOTHING    :== 0x80000000
EVERYTHING :== 0x7FFFFFFF

/* constants for display option bits */

DO_BLINK              :==  0x0001
DO_STRETCH            :==  0x0002
DO_MIRROR_LEFT_RIGHT  :==  0x0004
DO_MIRROR_UP_DOWN     :==  0x0008
DO_ROTATE_90          :==  0x0010
DO_ROTATE_180         :==  0x0020
DO_ROTATE_270         :==  0x0040


/* constants for object option bits */

OO_FIXED                        :==  0x0001
OO_IGNORE_STATIC_BOUNDS         :==  0x0002
OO_IGNORE_LEVEL_BOUNDS          :==  0x0004
OO_BOUNCE_AT_COLLISIONS         :==  0x0008
OO_CHECK_MAP_CODES              :==  0x0010
OO_CHECK_KEYBOARD               :==  0x0020
OO_ALLOW_KEYBOARD_REPEAT        :==  0x0040
OO_STATIC                       :==  0x0080
OO_LAST_DIRECTION_LEFT          :==  0x0100  /* 0=right, 1=left */
OO_LAST_DIRECTION_UP            :==  0x0200  /* 0=down, 1=up */
OO_AUTO_MIRROR_LEFT_RIGHT       :==  0x0400
OO_AUTO_MIRROR_UP_DOWN          :==  0x0800
OO_FREEZE                       :==  0x1000  /* when framecounter < 0 */
OO_REMOVE_MAP_CODE              :==  0x2000


/* focus constants */

FC_BOUND            :== 0x80000000  // don't scroll in this direction, generate static bound
FC_OFFSCREEN        :== 0x80000001  // don't scroll, but object can pass


/* statistics options */

SO_X_CENTERED               :==    1
SO_Y_CENTERED               :==    2
SO_X_FROM_SCREEN_CENTER     :==    4
SO_Y_FROM_SCREEN_CENTER     :==    8


/* GK_key indicate the keyboard movements as passed to Clean. */

GK_UNKNOWN          :==   0

GK_RETURN           :==  10
GK_ESCAPE           :==  11
GK_LEFT             :==  12
GK_RIGHT            :==  13
GK_UP               :==  14
GK_DOWN             :==  15
GK_HOME             :==  16
GK_END              :==  17
GK_PAGE_UP          :==  18
GK_PAGE_DOWN        :==  19
GK_F1               :==  20
GK_F2               :==  21
GK_F3               :==  22
GK_F4               :==  23
GK_F5               :==  24
GK_F6               :==  25
GK_F7               :==  26
GK_F8               :==  27
GK_F9               :==  28
GK_F10              :==  29
GK_F11              :==  30
GK_F12              :==  31
GK_SPACE            :==  32


WinBinaryIntStr         :: !Int -> {#Char}
WinBinaryBoolStr        :: !Bool -> {#Char}
WinInitGameBitmap       :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinGameBitmapDone       :: !BID !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinClearAllGameBitmaps  :: !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinSetTransparentColor  :: !BID !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinInitBlockSequence    :: !BID !SEQID !{#Char} !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinInitGameLayerMap     :: !MAPID !BID !{#Char} !Int !Int !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinGameLayerMapDone     :: !MAPID !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinSetGameBoundMap      :: !Int !Int !{#Char} !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinMoveScreenTo         :: !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinInitSpriteAnimation  :: !BID !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
// WinInitGameObject       :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinSetObjectFocus       :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
// WinCreateUserEvent      :: !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinSetObjectRec         :: !Int !Int !Int !Bool  !Int !Int  !Int !Int  !Int !Int  !Int  !Int  !Int !Int !Int  !Int !Int !Int !Int
                                !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int !Int   !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinGetObjectRec         :: !Int !*OSToolbox -> (!Int, !Int, !Bool,  !Int, !Int, !Int, !Int, !Int, !Int, !Int,  !Int,  !Int, !Int, !Int, !Int, !Int, !Int, !Int,
                                !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int, !Int,  !GRESULT, !*OSToolbox)

WinShowStatistic        :: !Int !Int !{#Char} !Int !RGBcolor !{#Char} !Int !Bool !Bool  !Bool !Int !Int !RGBcolor !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinPlayMusic            :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
WinStopMusic            :: !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinGameLevelOptions     :: !RGBcolor !Bool !Bool !Bool !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)

WinInitSoundSample      :: !Int !{#Char} !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
/*
WinPlaySoundSample      :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
*/

WinGetBoundMap          :: !Int !Int !*OSToolbox      -> (!Int, !GRESULT, !*OSToolbox)
WinSetBoundMap          :: !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
