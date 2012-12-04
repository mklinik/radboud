#ifndef _CGAMELIB_H
#define _CGAMELIB_H

/* Clean Game Library by Mike Wiering */


#include "cOSGameLib_121.h"  /* OS specific functions */

/* PA: Definitions moved from intrface_121.h */

extern CLEAN_STRING WinBinaryIntStr (int);
extern CLEAN_STRING WinBinaryBoolStr (BOOL);
extern void WinInitGameBitmap (int,CLEAN_STRING,int,int,int,int,OS,int*,OS*);
extern void WinGameBitmapDone (int,OS,int*,OS*);
extern void WinClearAllGameBitmaps (OS,int*,OS*);
extern void WinSetTransparentColor (int,int,int,OS,int*,OS*);
extern void WinInitBlockSequence (int,int,CLEAN_STRING,OS,int*,OS*);
extern void WinInitGameLayerMap (int,int,CLEAN_STRING,int,int,BOOL,OS,int*,OS*);
extern void WinGameLayerMapDone (int,OS,int*,OS*);
extern void WinSetGameBoundMap (int,int,CLEAN_STRING,int,int,int,int,int,OS,int*,OS*);
extern void WinMoveScreenTo (int,int,OS,int*,OS*);
extern void WinInitSpriteAnimation (int,CLEAN_STRING,BOOL,OS,int*,OS*);
// extern void WinInitGameObject (int,int,int,int,OS,int*,OS*);
extern void WinSetObjectFocus (int,int,int,int,int,int,OS,int*,OS*);
// extern void WinCreateUserEvent (int,int,int,int,OS,int*,OS*);
extern void WinSetObjectRec (int,int,int,BOOL,int,int,
                             int,int,int,int,int,int,
                             int,int,int,int,int,
                             BOOL,int,int,int,int,int,int,int,int,int,int,int,int,
                             int,OS,int*,OS*
                            );
extern void WinGetObjectRec (int,OS,int*,int*,BOOL*,
                             int*,int*,int*,int*,int*,
                             int*,int*,int*,
                             int*,int*,int*,int*,int*,int*,
                             BOOL*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,
                             int*,OS*
                            );
extern void WinShowStatistic (int,int,CLEAN_STRING,int,int,int,int,CLEAN_STRING,int,BOOL,BOOL,BOOL,int,int,int,int,int,int,OS,int*,OS*);
extern void WinPlayMusic (CLEAN_STRING,BOOL,OS,int*,OS*);
extern void WinStopMusic (OS,int*,OS*);
extern void WinGameLevelOptions (int,int,int,BOOL,BOOL,BOOL,BOOL,OS,int*,OS*);
extern void WinInitSoundSample (int,CLEAN_STRING,int,OS,int*,OS*);
extern void WinGetBoundMap (int,int,OS,int*,int*,OS*);
extern void WinSetBoundMap (int,int,int,OS,int*,OS*);
/* PA: up to here. */

/* Nothing value for integer, used with Maybe values */
#define NOTHING (0x80000000)

/* bound directions */
#define UPPER_BOUND (1 << 0)
#define LEFT_BOUND  (1 << 1)
#define LOWER_BOUND (1 << 2)
#define RIGHT_BOUND (1 << 3)

/* predefined bound bits */
#define BND_MAP_CODES       (1 << 30)
#define BND_STATIC_BOUNDS   (1 << 31)

/* object option bits */
#define OO_FIXED                       (1 <<  0)  // obsolete
#define OO_IGNORE_STATIC_BOUNDS        (1 <<  1)  // obsolete
#define OO_IGNORE_LEVEL_BOUNDS         (1 <<  2)
#define OO_BOUNCE_AT_COLLISIONS        (1 <<  3)  // obsolete
#define OO_CHECK_MAP_CODES             (1 <<  4)  // obsolete
#define OO_CHECK_KEYBOARD              (1 <<  5)
#define OO_ALLOW_KEYBOARD_REPEAT       (1 <<  6)
#define OO_STATIC                      (1 <<  7)
#define OO_LAST_DIRECTION_LEFT         (1 <<  8)  /* 0=right, 1=left */
#define OO_LAST_DIRECTION_UP           (1 <<  9)  /* 0=down, 1=up */
#define OO_AUTO_MIRROR_LEFT_RIGHT      (1 << 10)
#define OO_AUTO_MIRROR_UP_DOWN         (1 << 11)
#define OO_FREEZE                      (1 << 12)  /* when timecounter < 0 */
#define OO_REMOVE_MAP_CODE             (1 << 13)

/* statistics options */
#define SO_X_CENTERED             (1 <<  0)
#define SO_Y_CENTERED             (1 <<  1)
#define SO_X_FROM_SCREEN_CENTER   (1 <<  2)
#define SO_Y_FROM_SCREEN_CENTER   (1 <<  3)

/* focus constants */
#define FC_BOUND            (0x80000000)
#define FC_OFFSCREEN        (0x80000001)

/* game keys */
#define GK_UNKNOWN    0

#define GK_BACKSPACE  8
#define GK_RETURN    10
#define GK_ESCAPE    11
#define GK_LEFT      12
#define GK_RIGHT     13
#define GK_UP        14
#define GK_DOWN      15
#define GK_HOME      16
#define GK_END       17
//#define GK_PAGE_UP   18
//#define GK_PAGE_DOWN 19
#define GK_F1        20
#define GK_F2        21
#define GK_F3        22
#define GK_F4        23
#define GK_F5        24
#define GK_F6        25
#define GK_F7        26
#define GK_F8        27
#define GK_F9        28
#define GK_F10       29
#define GK_F11       30
#define GK_F12       31
#define GK_SPACE     32

#define GK_MAX_KEY  255


/* Functions called from cCrossCallGame_121 */
extern void InitGameGlobals (void);
extern void RunGame (void);
extern void PlaySoundSample (int, int, int, int, int);
extern void CreateGameObject (int, int, int, int, int *);
extern void ScheduleUserGameEvent (int, int, int, int, int, int);
extern int  CreateGameWindow (int, int, int, BOOL);
extern void RegisterGameWindowClass (void);

#endif
