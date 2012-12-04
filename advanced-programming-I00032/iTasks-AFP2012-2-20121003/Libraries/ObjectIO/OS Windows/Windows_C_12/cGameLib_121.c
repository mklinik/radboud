/****************************************************************************
    Clean OS Windows library module version 1.2.1.
    This module is part of the Clean Object I/O library, version 1.2.1,
    for the Windows platform.
****************************************************************************/

/****************************************************************************
    About this module:
    Clean Game Library (higher level functions),
    Mike Wiering (mike.wiering@cs.kun.nl).
****************************************************************************/

#include "util_121.h"
#include "intrface_121.h"
#include "cGameLib_121.h"
#include "cCrossCall_121.h"  /* Cross call infrastructure */

// #define SMART_DRAW

typedef struct SPRITEANIMATION
{
    int iSpriteID;
    int iBitmapID;
    int iSequenceLength;
    char *sSequence;
    int iPosition;
    int iCounter;
    struct SPRITEANIMATION *saNext;
    BOOL bLoop;
} SPRITEANIMATION;

typedef struct OBJECTREC
{
    int iObjectID;
    int iMapX;
    int iMapY;
    int iObjType;
    int iSubType;
    BOOL bActive;
    int iXPos;
    int iYPos;
    int iXSize;
    int iYSize;
    int iXOffset;
    int iYOffset;
    int iSpriteID;
    int iSpriteIndex;
    int iDisplayOptions;
    int iOwnBounds;
    int iBounceBounds;
    int iCollideBounds;
    int iForgetX;
    int iForgetY;
    int iLastXPos;
    int iLastYPos;
    int iTimeCounter;
    int iLayer;
    int iXAcc;
    int iYAcc;
    int iXSpeed;
    int iYSpeed;
    int iXBounce;
    int iYBounce;
    int iMaxXSpeed;
    int iMaxYSpeed;
    int iXSlowDown;
    int iYSlowDown;
    int iSkipMove;
    int iOptions;

    int iFixedXPos;
    int iFixedYPos;
    int iLastFixedXPos;
    int iLastFixedYPos;
    int iLastSprite;
    int iPosition;
    int iCounter;

    int iDiag;
    int iDiagUpDn;
    int iDiagX;
    int iDiagY;
    int iDiagLeftX;
    int iDiagLeftY;
    int iDiagRightX;
    int iDiagRightY;

    struct OBJECTREC *objNext;
} OBJECTREC;


typedef struct GAMELAYERMAPINFO
{
    int iMapID;
    int iBitmapID;
    int iMapWidth;
    int iMapHeight;
    char *sMap;
    int iTotalXSize;
    int iTotalYSize;
    BOOL bTile;
    struct GAMELAYERMAPINFO *glmipNext;
} GAMELAYERMAPINFO;


extern BOOL bGameActive;
extern HWND ghGameWindow;
extern int ScreenWidth;
extern int ScreenHeight;
extern int BitsPerPixel;
extern BOOL FullScreen;

extern int XShiftScreen;
extern int YShiftScreen;

static int iFrameCounter;   // PA: changed to static

static GAMELAYERMAPINFO *glmipGameLayerMapInfo = NULL;
static SPRITEANIMATION *saSprites = NULL;
static OBJECTREC *objObjects = NULL;

static int XView = 0;
static int YView = 0;
static int XScrollSpeed = 0;
static int YScrollSpeed = 0;

static int iNextLayer = 0;

/* bound map */
static char *sBoundMap = NULL;
static int BoundMapWidth = 0;
static int BoundMapHeight = 0;
static int BoundBlockWidth = 0;
static int BoundBlockHeight = 0;
static int TotalWidth = 0;
static int TotalHeight = 0;

#ifdef SMART_DRAW
/* smart draw data (only draw changed tiles) */
static char *sLastTile = NULL;
static int iLastTileW;
static int iLastTileH;
static int iTopLayerLastX;
static int iTopLayerLastY;
static int bSmartDraw;
static int bAllowSmartDraw;
#endif

/* min. value for objects */
static int iObjectValueStart;

/* smallest distance to visible screen before initializing object */
static int iStartObjX = 5;
static int iStartObjY = 5;

/* ID of the object that is performing a crosscall */
static int iCCObjectID = 0;

static BOOL bCheckEscape = TRUE;
static BOOL DebugScroll = FALSE;
static BOOL bFillBackground = TRUE;
static COLORREF FillBackgroundRGB = 0;
static BOOL bFadeIn = FALSE;
static BOOL bFadeOut = FALSE;

static char FullScreenGameWindowClassName[] = "__CleanFullScreenGameWindow";
static char GameWindowClassName[] = "__CleanGameWindow";


/* focusing the main character */
static int iFollowID = 0;
static int iFollowX1 = 0;
static int iFollowY1 = 0;
static int iFollowX2 = 0;
static int iFollowY2 = 0;

static int iMaxXScrollSpeed = 1;
static int iMaxYScrollSpeed = 1;
static int iActualXView = 0;
static int iActualYView = 0;

/* info for user defined events and scheduled */
typedef struct USER_EVENT_INFO
{
    int iEventID;
    int iEventParameter1;
    int iEventParameter2;
    int iEventParameter3;
    int iEventParameter4;
    int iDestination;
    int iSubDestination;
    int iTimeCounter;
    struct USER_EVENT_INFO *ueiNext;
} USER_EVENT_INFO;

static USER_EVENT_INFO *ueiUserEventInfo = NULL;

#define EV_PLAY_SOUND  (-1)

//  PA: made static
static void ClearUserEvents (void)
{
    struct USER_EVENT_INFO *ueiNext;

    while (ueiUserEventInfo)
    {
        ueiNext = ueiUserEventInfo->ueiNext;
        rfree (ueiUserEventInfo);
        ueiUserEventInfo = ueiNext;
    }
}


static int NextFrame (int);


/* use one integer to save info for both pages */
static int setflipint (int x, int value)
{
    if ((iFrameCounter % 2) == 0)
        return ((x & 0xFFFF0000) + (value & 0xFFFF));
    else
        return ((x & 0xFFFF) + (value << 16));
}

static int getflipint (int value)
{
    if ((iFrameCounter % 2) == 0)
        return (value & 0xFFFF);
    else
        return (value >> 16);
}

static BOOL cmpflipint (int x, int value)
{
    if ((iFrameCounter % 2) == 0)
        return ((x & 0xFFFF0000) == (value << 16));
    else
        return ((x & 0xFFFF) == (value & 0xFFFF));
}



/*
     3 / 2 =  1
     2 / 2 =  1
     1 / 2 =  0
     0 / 2 =  0
    -1 / 2 =  0   <-- fill up this gap
    -2 / 2 = -1
    -3 / 2 = -1
*/

static int intdiv (int x, int y)
{
    return (x < 0)? (x - y + 1) / y : x / y;
}

static int Min (int x, int y)
{
    return (x < y)? x : y;
}

static int Max (int x, int y)
{
    return (x > y)? x : y;
}


/* get a pointer to the SPRITEANIMATION structure with id ID */
static SPRITEANIMATION *GetSpriteAnimation (int ID)
{
    SPRITEANIMATION *sa = saSprites;
    BOOL bFound = FALSE;

    while (sa && (!bFound))
    {
        if (sa->iSpriteID == ID)
            bFound = TRUE;
        else
            sa = sa->saNext;
    }
    if (bFound)
        return sa;
    else
        return NULL;
}   /* GetSpriteAnimation */

/* free the SPRITEANIMATION list */
static void FreeSpriteAnimationList (void)
{
    SPRITEANIMATION *sa;

    while (saSprites)
    {
        sa = saSprites->saNext;
        if (saSprites->sSequence)
        {
            rfree (saSprites->sSequence);
            saSprites->sSequence = NULL;
        }
        rfree (saSprites);
        saSprites = sa;
    }
}   /* FreeSpriteAnimationList */

/* get a pointer to the GAMELAYERMAPINFO structure with id MID */
static GAMELAYERMAPINFO *GetGameLayerMapInfo (int MapID)
{
    GAMELAYERMAPINFO *glmip = glmipGameLayerMapInfo;
    BOOL bFound = FALSE;

    while (glmip && (!bFound))
    {
        if (glmip->iMapID == MapID)
            bFound = TRUE;
        else
            glmip = glmip->glmipNext;
    }
    return glmip;
}   /* GetGameLayerMapInfo */

/* free the GAMELAYERMAPINFO list and all the map strings */
static void FreeGameLayerMapInfoList (void)
{
    GAMELAYERMAPINFO *glmip;

    while (glmipGameLayerMapInfo)
    {
        glmip = glmipGameLayerMapInfo->glmipNext;
        if (glmipGameLayerMapInfo->sMap != NULL)
        {
            rfree (glmipGameLayerMapInfo->sMap);
            glmipGameLayerMapInfo->sMap = NULL;
        }
        rfree (glmipGameLayerMapInfo);
        glmipGameLayerMapInfo = glmip;
    }
}   /* FreeGameLayerMapInfoList */


/* get a pointer to the OBJECTREC structure with id ID */
static OBJECTREC *GetObjectRec (int ID)
{
    OBJECTREC *obj = objObjects;
    BOOL bFound = FALSE;

    while (obj && (!bFound))
    {
        if (obj->iObjectID == ID)
            bFound = TRUE;
        else
            obj = obj->objNext;
    }
    return obj;
}   /* GetObjectRec */


/* update last position if pos has been changed during an event */
static void UpdatePosition (OBJECTREC *obj)
{
    obj->iLastXPos = obj->iXPos;
    obj->iLastYPos = obj->iYPos;

    obj->iFixedXPos = obj->iXPos << 8;
    obj->iFixedYPos = obj->iYPos << 8;
    obj->iLastFixedXPos = obj->iFixedXPos;
    obj->iLastFixedYPos = obj->iFixedYPos;
}


/* initialize a new game object */
static void InitGameObject (int mapx, int mapy)
{
    OBJECTREC *objNew;
    OBJECTREC *obj1;
    OBJECTREC *obj2;
    int mappos;
    int origmapval, mapval, above;
    int newid;

    if ((mapx < 0) || (mapy < 0))
        return;
    if ((mapx > BoundMapWidth - 1) || (mapy > BoundMapHeight - 1))
        return;
    mappos = (mapy * BoundMapWidth + mapx) * sizeof (int);
    origmapval = (*(int *) &sBoundMap[mappos]);
    mapval = origmapval >> 8;
    if (mapval < iObjectValueStart)
        return;

    (*(int *) &sBoundMap[mappos]) = origmapval & 0x00FF;  // remove object

    // create a unique id number
    newid = 1;
    while (GetObjectRec (newid))
        newid++;

    objNew = rmalloc (sizeof (OBJECTREC));
    objNew->objNext = NULL;
    objNew->iMapX = mapx;
    objNew->iMapY = mapy;
  /*
    objNew->iObjType = mapval;
    objNew->iSubType = above >> 8;
  */
    if (mapy == 0)
        above = 0;
    else
    {
        origmapval = (*(int *) &sBoundMap[mappos - BoundMapWidth * sizeof (int)]);
        above = origmapval >> 8;
    }

    if (above >= iObjectValueStart)
        above = 0;

    if (above > 0)
        (*(int *) &sBoundMap[mappos - BoundMapWidth * sizeof (int)]) =
            origmapval & 0x00FF;  // remove subcode

    objNew->iObjectID = newid;
    objNew->bActive = FALSE;

    objNew->iPosition = 0;
    objNew->iCounter = 0;

    obj1 = objObjects;
    obj2 = NULL;

    while (obj1)
    {
        obj2 = obj1;
        obj1 = obj1->objNext;
    }

    if (obj2)
    {
        obj2->objNext = objNew;
    }
    else
    {
        objObjects = objNew;
    }


    // inform Clean that new object has been created
    iCCObjectID = newid;
    SendMessage6ToClean (CcWmINITOBJECT, mapval, above, newid,
                           mapx * BoundBlockWidth,
                           mapy * BoundBlockHeight, iFrameCounter);
    iCCObjectID = 0;

    UpdatePosition (objNew);
}   /* InitGameObject */

/* remove game object with id ID and all objects that are not active (id==0) */
static void GameObjectDone (int id)
{
    OBJECTREC *objCur = objObjects;
    OBJECTREC *objNext = NULL;
    OBJECTREC *objPrev = NULL;
    int mappos, mapval;

    while (objCur)
    {
        if ((id != -1) &&
            (((id > 0) && (objCur->iObjectID != id)) ||
             ((id == 0) && (objCur->bActive))
            )
           )
        {
            /* skip this object */
            objPrev = objCur;
            objNext = objCur->objNext;
        }
        else
        {
            /* remove this object */

            if (objCur->iObjectID == iFollowID)
                iFollowID = 0;

            /* link previous node to next node */
            if (objPrev)
                objPrev->objNext = objCur->objNext;
            else
                objObjects = objCur->objNext;

            objNext = objCur->objNext;

            /* call object's done function */

            if (id != -1)
            {
                iCCObjectID = objCur->iObjectID;
                SendMessage2ToClean (CcWmOBJECTDONE, objCur->iObjType, objCur->iObjectID);
                iCCObjectID = 0;

              /*  UpdatePosition (objCur);  */
            }
         /*
            objCur->iObjType = gCci.p1;
            objCur->iSubType = gCci.p2;
         */
            /* put ObjType and SubType back into map if >= 0 */
            if (!(objCur->iOptions & OO_REMOVE_MAP_CODE))
                if ((objCur->iMapX >= 0) && (objCur->iMapY >= 0))
                    if (objCur->iObjType >= 0)
                    {
                        mappos = (objCur->iMapY * BoundMapWidth + objCur->iMapX) * sizeof (int);
                        mapval = (*(int *) &sBoundMap[mappos]);
                        (*(int *) &sBoundMap[mappos]) = mapval | (objCur->iObjType << 8);

                        if (objCur->iSubType >= 0)
                        {
                            mappos = ((objCur->iMapY - 1) * BoundMapWidth + objCur->iMapX) * sizeof (int);
                            mapval = (*(int *) &sBoundMap[mappos]);
                            (*(int *) &sBoundMap[mappos]) = mapval | (objCur->iSubType << 8);
                        }
                    }

            /* free the current node */
            rfree (objCur);
        }
        objCur = objNext;
    }
}   /* GameObjectDone */


void InitGameGlobals (void)
{
    ghGameWindow = NULL;
    bGameActive = FALSE;
    iFrameCounter = 0;
}



/* GameTranslateKey translates WM_KEYDOWN wPara fields to GameKey values */
static int GameTranslateKey (WPARAM wPara)
{
    switch (wPara)
    {
        case VK_BACK:        return GK_BACKSPACE; break;
        case VK_RETURN:      return GK_RETURN;    break;
        case VK_ESCAPE:      return GK_ESCAPE;    break;
        case VK_UP:          return GK_UP;        break;
        case VK_DOWN:        return GK_DOWN;      break;
        case VK_LEFT:        return GK_LEFT;      break;
        case VK_RIGHT:       return GK_RIGHT;     break;
        case VK_HOME:        return GK_HOME;      break;
        case VK_END:         return GK_END;       break;
//        case VK_PAGE_UP:     return GK_PAGE_UP;   break;
//        case VK_PAGE_DOWN:   return GK_PAGE_DOWN; break;
        case VK_F1:          return GK_F1;        break;
        case VK_F2:          return GK_F2;        break;
        case VK_F3:          return GK_F3;        break;
        case VK_F4:          return GK_F4;        break;
        case VK_F5:          return GK_F5;        break;
        case VK_F6:          return GK_F6;        break;
        case VK_F7:          return GK_F7;        break;
        case VK_F8:          return GK_F8;        break;
        case VK_F9:          return GK_F9;        break;
        case VK_F10:         return GK_F10;       break;
        case VK_F11:         return GK_F11;       break;
        case VK_F12:         return GK_F12;       break;
        case VK_SPACE:       return GK_SPACE;     break;
        default:
        {
             if ((wPara > 32) && (wPara <= 127))
                 return wPara;
             else
                 return GK_UNKNOWN;
             break;
        }
    }
}   /* GameTranslateKey */



/* buffer which holds the last state of each key */
static BOOL gk_state[GK_MAX_KEY + 1] =
    {FALSE};


//   MessageBox (NULL, "", NULL, MB_OK);


/* GameWindowProcedure is the Windows callback routine for a game window. */
static LRESULT CALLBACK GameWindowProcedure (HWND hWin, UINT uMess, WPARAM wPara, LPARAM lPara)
{
    printMessage ("Game Window", hWin, uMess, wPara, lPara);
    switch (uMess)
    {
        case WM_CLOSE:
            bGameActive = FALSE;
            return 0;
            break;

        case WM_DESTROY:
            bGameActive = FALSE;
            ClearUserEvents ();
            GameObjectDone (-1);
            FreeGameLayerMapInfoList ();
            FreeSpriteAnimationList ();
            OSFreeGameBitmaps ();
            OSStopMusic ();
            OSDeInitSound ();
            OSDeInitGameWindow ();
            ShowCursor (TRUE);
            return 0;
            break;
        case MCI_NOTIFY:
            {
                // MessageBeep (MB_ICONASTERISK);
                if (wPara == MCI_NOTIFY_SUCCESSFUL)
                    OSPlayMusic (NULL, TRUE);  /* restart music */
            }
            break;
        case WM_PAINT:
            if (!FullScreen)
            {
                PAINTSTRUCT ps;
                HDC hdc = BeginPaint (hWin, &ps);

                NextFrame (0);
                EndPaint (hWin, &ps);
            }
            break;
        case WM_KEYUP:
            {
                int keycode = GameTranslateKey (wPara);

                if (gk_state[keycode])
                {
                    OBJECTREC *obj = objObjects;
                    while (obj)
                    {
                        if ((obj->bActive) && (obj->iOptions & OO_CHECK_KEYBOARD))
                        {
                            int ix = obj->iXPos;
                            int iy = obj->iYPos;

                            iCCObjectID = obj->iObjectID;
                            SendMessage3ToClean (CcWmOBJECTKEYUP, obj->iObjType, obj->iObjectID, keycode);
                            iCCObjectID = 0;

                            if ((ix != obj->iXPos) || (iy != obj->iYPos))
                                UpdatePosition (obj);
                        }
                        obj = obj->objNext;
                    }
                    gk_state[keycode] = FALSE;
                }
            }
            break;
        case WM_KEYDOWN:
            {
                int keycode = GameTranslateKey (wPara);
                if (keycode == GK_ESCAPE)
                    if (bCheckEscape)
                        bGameActive = FALSE;

                switch (keycode)
                {
                    case GK_UNKNOWN:
                        return DefWindowProc (hWin, uMess, wPara, lPara);
                        break;
                    default:
                        {
                            BOOL found = FALSE;
                            OBJECTREC *obj = objObjects;

                            while (obj)
                            {
                                if ( (obj->bActive) &&
                                     (obj->iOptions & OO_CHECK_KEYBOARD) &&
                                     ( (!gk_state[keycode]) ||
                                       (obj->iOptions & OO_ALLOW_KEYBOARD_REPEAT)
                                     )
                                   )
                                {
                                    int ix = obj->iXPos;
                                    int iy = obj->iYPos;

                                    iCCObjectID = obj->iObjectID;
                                    SendMessage3ToClean (CcWmOBJECTKEYDOWN, obj->iObjType, obj->iObjectID, keycode);
                                    iCCObjectID = 0;

                                    if ((ix != obj->iXPos) || (iy != obj->iYPos))
                                        UpdatePosition (obj);

                                    found = TRUE;
                                }
                                obj = obj->objNext;
                            }

                            gk_state[keycode] = TRUE;
                        }

                        // debugging level, just move around
                        if (DebugScroll == TRUE)
                        {
                            SendMessage3ToClean (CcWmGAMEKEYBOARD, keycode, XScrollSpeed, YScrollSpeed);
                            XScrollSpeed = gCci.p1;
                            YScrollSpeed = gCci.p2;
                            iMaxXScrollSpeed = 8;
                            iMaxYScrollSpeed = 8;
                        }
                }
            }
            break;
        default:
            return DefWindowProc (hWin, uMess, wPara, lPara);
            break;
    }
    return 0;
}   /* GameWindowProcedure */


/* move XView and YView towards iActualXView and iActualYView */
static void CorrectView (void)
{
    if (iActualXView > XView)
        XView = Min (XView + iMaxXScrollSpeed, iActualXView);
    if (iActualYView > YView)
        YView = Min (YView + iMaxYScrollSpeed, iActualYView);
    if (iActualXView < XView)
        XView = Max (XView - iMaxXScrollSpeed, iActualXView);
    if (iActualYView < YView)
        YView = Max (YView - iMaxYScrollSpeed, iActualYView);

    if (XView > TotalWidth - ScreenWidth)
    {
        XView = TotalWidth - ScreenWidth;
        iActualXView = XView;
        XScrollSpeed = 0;
    }
    if (YView > TotalHeight - ScreenHeight)
    {
        YView = TotalHeight - ScreenHeight;
        iActualYView = YView;
        YScrollSpeed = 0;
    }

    if (XView < 0)
    {
        XView = 0;
        iActualXView = XView;
        XScrollSpeed = 0;
    }
    if (YView < 0)
    {
        YView = 0;
        iActualYView = YView;
        YScrollSpeed = 0;
    }
}   /* CorrectView */


void RunGame (void)
{
    MSG msg;
    int i, j, x, y, w, h;
    int lastx, lasty;
    int fade;

    int result;

    iFrameCounter = 0;
    CorrectView ();

    x = XView / BoundBlockWidth;
    y = YView / BoundBlockHeight;
    w = ScreenWidth / BoundBlockWidth;
    h = ScreenHeight / BoundBlockHeight;
    for (i = -iStartObjX - 1; i < w + iStartObjX + 2; i++)
        for (j = -iStartObjY - 1; j < h + iStartObjY + 2; j++)
            InitGameObject (x + i, y + j);

    lastx = x;
    lasty = y;

#ifdef SMART_DRAW
    bSmartDraw = 0;
    bAllowSmartDraw = 0;
#endif

    /* initialize autoinit object */
    CreateGameObject (0, 0, -1, -1, &result);

    fade = (bFadeIn)? -1 : 0;
    bGameActive = TRUE;
    while (bGameActive)
    {
        if (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
        {
            TranslateMessage (&msg);
            DispatchMessage (&msg);
        }

        if (iFollowID == 0)
        {
            iActualXView += XScrollSpeed;
            iActualYView += YScrollSpeed;
        }

        CorrectView ();

        x = XView / BoundBlockWidth;
        y = YView / BoundBlockHeight;
        if (y != lasty)
            for (i = -iStartObjX - 1; i < w + iStartObjX + 2; i++)
            {
                j = -iStartObjY;
                if (y < lasty)
                    InitGameObject (x + i, y + j);
                j = h + iStartObjY + 1;
                if (y > lasty)
                    InitGameObject (x + i, y + j);
             }
        if (x != lastx)
            for (j = -iStartObjY - 1; j < h + iStartObjY + 2; j++)
            {
                i = -iStartObjX;
                if (x < lastx)
                    InitGameObject (x + i, y + j);
                i = w + iStartObjX + 1;
                if (x > lastx)
                    InitGameObject (x + i, y + j);
            }
        lastx = x;
        lasty = y;

        NextFrame (fade);
        fade = 0;
        iFrameCounter++;

        SendMessage0ToClean (CcWmCHECKQUIT);
        if ((BOOL) gCci.p1)
            bGameActive = FALSE;

    }

    if (bFadeOut)
    {
        fade = 1;
        NextFrame (fade);
    }
    bGameActive = FALSE;

    ClearUserEvents ();
    GameObjectDone (-1);

    iFollowID = 0;

    //  MessageBeep (MB_ICONASTERISK);
}   /* RunGame */


/* create a new event */
static void ScheduleEvent (int event, int par1, int par2, int par3, int par4, int target, int subtarget, int time)
{
    struct USER_EVENT_INFO *uei;

    uei = rmalloc (sizeof (USER_EVENT_INFO));
    uei->iEventID = event;
    uei->iEventParameter1 = par1;
    uei->iEventParameter2 = par2;
    uei->iEventParameter3 = par3;
    uei->iEventParameter4 = par4;
    uei->iDestination = target;
    uei->iSubDestination = subtarget;
    uei->iTimeCounter = time;
    uei->ueiNext = ueiUserEventInfo;
    ueiUserEventInfo = uei;
}   /* ScheduleEvent */


/* play (actually schedule) a sound sample */
void PlaySoundSample (int id, int vol, int pan, int freq, int delay)
{
    ScheduleEvent (EV_PLAY_SOUND, id, vol, pan, freq, 0, -1, delay);
}   /* PlaySoundSample */

/* schedule any user event */
void ScheduleUserGameEvent (int event, int par1, int par2,
                            int target, int subtarget, int time)
{
    struct USER_EVENT_INFO *uei;

    if (event >= 0)
    {
        uei = rmalloc (sizeof (USER_EVENT_INFO));
        uei->iEventID = event;
        uei->iEventParameter1 = par1;
        uei->iEventParameter2 = par2;
        uei->iEventParameter3 = 0;
        uei->iEventParameter4 = 0;
        if (target == 0)
            uei->iDestination = -iCCObjectID;  /* only target current object */
        else
            if (target == -1)
                uei->iDestination = 0;  /* all objects */
            else
                uei->iDestination = target;  /* bounds */
        uei->iSubDestination = subtarget;
        uei->iTimeCounter = time;
        uei->ueiNext = ueiUserEventInfo;
        ueiUserEventInfo = uei;
    }
}   /* ScheduleUserGameEvent */


/* create the game window */
int CreateGameWindow (int w, int h, int bpp, BOOL fs)
{
    ScreenWidth = w;
    ScreenHeight = h;
    BitsPerPixel = bpp;
    FullScreen = fs;

    if (FullScreen)
        ghGameWindow =
            CreateWindowEx (WS_EX_TOPMOST,       /* extended style */
                            (LPCTSTR) FullScreenGameWindowClassName,  /* class */
                            (LPCTSTR) gAppName,  /* title */
                            WS_POPUP,            /* window style */
                            0, 0, ScreenWidth, ScreenHeight,
                            ghMainWindow,        /* ??? parent */
                            NULL,                /* menu */
                            (HANDLE) ghInst,
                            NULL                 /* window creation data */
                           );
    else
        ghGameWindow =
            CreateWindowEx (WS_OVERLAPPED,
                            (LPCTSTR) GameWindowClassName,
                            (LPCTSTR) gAppName,
                            WS_THICKFRAME,
                            CW_USEDEFAULT,
                            CW_USEDEFAULT,
                            ScreenWidth,
                            ScreenHeight,
                            ghMainWindow,
                            NULL,
                            (HANDLE) ghInst,
                            NULL
                           );

    ShowWindow (ghGameWindow, SW_SHOWNORMAL);
    UpdateWindow (ghGameWindow);

    if (OSInitGameWindow ())
    {
        OSClearScreen ();
        if (FullScreen)
            OSFlip ();
        OSClearScreen ();
        OSClearVirtualScreen (FillBackgroundRGB);
        OSInitSound ();
    }
    else
        ErrorExit ("Game Window could not be created");

    iFrameCounter = 0;

    return ((int) ghGameWindow);
}   /* CreateGameWindow */


/* register game window class */
void RegisterGameWindowClass (void)
{
    WNDCLASSEX wclass;

    wclass.cbSize = sizeof (WNDCLASSEX);
    wclass.style = CS_HREDRAW | CS_VREDRAW;
    wclass.lpfnWndProc = (WNDPROC) GameWindowProcedure;
    wclass.cbClsExtra = 0;
    wclass.cbWndExtra = 0;
    wclass.hInstance = ghInst;
    wclass.hIcon = LoadIcon (ghInst, IDI_APPLICATION);
    wclass.hCursor = NULL;
    wclass.hbrBackground = NULL;
    wclass.lpszMenuName = NULL;
    wclass.lpszClassName = FullScreenGameWindowClassName;
    wclass.hIconSm = NULL;
    RegisterClassEx (&wclass);

    wclass.cbSize = sizeof (WNDCLASSEX);
    wclass.style = CS_HREDRAW | CS_VREDRAW;
    wclass.lpfnWndProc = (WNDPROC) GameWindowProcedure;
    wclass.cbClsExtra = 0;
    wclass.cbWndExtra = 0;
    wclass.hInstance = ghInst;
    wclass.hIcon = LoadIcon (ghInst, IDI_APPLICATION);
    wclass.hCursor = NULL;
    wclass.hbrBackground = (HBRUSH) GetStockObject (BLACK_BRUSH);
    wclass.lpszMenuName = NULL;
    wclass.lpszClassName = GameWindowClassName;
    wclass.hIconSm = NULL;
    RegisterClassEx (&wclass);
}   /* RegisterGameWindowClass */


/* convert an integer to a 4-byte string */
CLEAN_STRING WinBinaryIntStr (int x)
{
    static CLEAN_STRING result = NULL;

    if (result)
        rfree (result);

    result = rmalloc (sizeof (x) + 1 + sizeof (int));
    result->length = sizeof (x);
    rsncopy (result->characters, (const char *) &x, sizeof (x));

    return result;
}   /* WinBinaryIntStr */

/* convert a boolean value to a Clean string */
CLEAN_STRING WinBinaryBoolStr (BOOL b)
{
    static CLEAN_STRING result = NULL;

    if (result)
        rfree (result);

    result = rmalloc (sizeof (b) + 1 + sizeof (int));
    result->length = sizeof (b);
    rsncopy (result->characters, (const char *) &b, sizeof (b));

    return result;
}   /* WinBinaryBoolStr */

/* read a new bitmap into (offscreen) memory (id==0: create new id: result) */
void WinInitGameBitmap (int id, CLEAN_STRING name,
                        int bitmapwidth, int bitmapheight,
                        int blockwidth, int blockheight,
                        OS ios, int *result, OS *oos
                       )
{
    int resultcode = OSInitGameBitmap (id, cstring (name),
                                       bitmapwidth, bitmapheight,
                                       blockwidth, blockheight);

    *result = resultcode;
    *oos = ios;
}   /* WinInitGameBitmap */

/* remove all bitmaps from memory, including all sprites */
void WinClearAllGameBitmaps (OS ios, int *result, OS* oos)
{
    FreeSpriteAnimationList ();
    OSFreeGameBitmaps ();

    *result = GR_OK;
    *oos = ios;
}   /* WinClearAllGameBitmaps */

/* remove bitmap from memory */
void WinGameBitmapDone (int id, OS ios, int *result, OS* oos)
{
    *result = OSFreeGameBitmap (id);
    *oos = ios;
}   /* WinGameBitmapDone */

/* set the transparent color for a bitmap: (x,y) is a transparent pixel */
void WinSetTransparentColor (int id, int x, int y, OS ios, int *result, OS* oos)
{
    *result = OSSetTransparentColor (id, x, y);
    *oos = ios;
}   /* WinSetTransparentColor */

/* initialize a block animation sequence (list of repeating blocks) */
void WinInitBlockSequence (int bitmapid, int seqid, CLEAN_STRING seq, OS ios, int *result, OS *oos)
{
    *result = OSInitBlockSequence (bitmapid, seqid,
                                     cstring (seq), seq->length);
    *oos = ios;
}   /* WinInitBlockSequence */

/* initialize the bound map */
void WinSetGameBoundMap (int blockwidth, int blockheight, CLEAN_STRING map,
                         int mapwidth, int mapheight, int objectstartvalue,
                         int startobjx, int startobjy,
                         OS ios, int *result, OS *oos
                        )
{
    int resultcode = GR_OK;

    BoundMapWidth = mapwidth;
    BoundMapHeight = mapheight;
    BoundBlockWidth = blockwidth;
    BoundBlockHeight = blockheight;
    TotalWidth = mapwidth * blockwidth;
    TotalHeight = mapheight * blockheight;

#ifdef SMART_DRAW
    if (sLastTile)
        rfree (sLastTile);
    iLastTileW = ScreenWidth / BoundBlockWidth + 1;
    iLastTileH = ScreenHeight / BoundBlockHeight + 1;
    sLastTile = rmalloc (((iLastTileW + 1) * (iLastTileH + 1) + 1) *
                           sizeof (int));
#endif

    if (sBoundMap)
        rfree (sBoundMap);
    sBoundMap = rmalloc ((map->length) + 1);

    rsncopy (sBoundMap, map->characters, map->length);
    iObjectValueStart = objectstartvalue;
    iStartObjX = startobjx;
    iStartObjY = startobjy;

    XView = 0;
    YView = 0;
    XScrollSpeed = 0;
    YScrollSpeed = 0;

    *result = resultcode;
    *oos = ios;
}   /* WinSetGameBoundMap */


/* set the level's initial position */
void WinMoveScreenTo (int x, int y, OS ios, int *result ,OS *oos)
{
    XView = x;  // ((x * BoundBlockWidth) + BoundBlockWidth / 2) - ScreenWidth / 2;
    YView = y;  // ((y * BoundBlockHeight) + BoundBlockHeight / 2) - ScreenHeight / 2;
    iActualXView = XView;
    iActualYView = YView;

    CorrectView ();

    *result = GR_OK;
    *oos = ios;
}   /* WinMoveScreenTo */


/* initialize a new layer map */
void WinInitGameLayerMap (int mapid, int bitmapid, CLEAN_STRING map,
                          int mapwidth, int mapheight,
                          BOOL tile,
                          /* int scrollspeed,  */
                          OS ios, int *result, OS *oos)
{
    int resultcode;
    GAMELAYERMAPINFO *glmip1;
    GAMELAYERMAPINFO *glmip2;

    resultcode = GR_INVALID_MAP_ID;
    if (!GetGameLayerMapInfo (mapid))  /* identifier not used yet */
    {
        int gbW, gbH, gbBW, gbBH, gbCX, gbCY;
        resultcode = GR_INVALID_BITMAP_ID;

        if (OSGetGameBitmapInfo (bitmapid,
                                    &gbW, &gbH, &gbBW, &gbBH, &gbCX, &gbCY))
        {
            /* find last element of linked list */
            glmip1 = glmipGameLayerMapInfo;
            glmip2 = NULL;
            while (glmip1)
            {
                glmip2 = glmip1;
                glmip1 = glmip1->glmipNext;
            }

            /* create new node */
            glmip1 = rmalloc (sizeof (GAMELAYERMAPINFO));
            glmip1->iMapID = mapid;
            glmip1->iBitmapID = bitmapid;
            glmip1->iMapWidth = mapwidth;
            glmip1->iMapHeight = mapheight;
            glmip1->sMap = rmalloc ((map->length) + 1);
            rsncopy (glmip1->sMap, map->characters, map->length);
            glmip1->iTotalXSize = (mapwidth * gbBW);
            glmip1->iTotalYSize = (mapheight * gbBH);
            glmip1->bTile = tile;
            glmip1->glmipNext = NULL;

            /* glmip2 points to the last element or is NULL */
            if (glmip2)
                glmip2->glmipNext = glmip1;
            else
                glmipGameLayerMapInfo = glmip1;  /* first element */

            resultcode = GR_OK;
        }
    }

    *result = resultcode;
    *oos = ios;
}    /* WinInitGameLayerMap */

/* remove layer map from memory */
void WinGameLayerMapDone (int mapid, OS ios, int *result, OS* oos)
{
    GAMELAYERMAPINFO *glmipCurrent = glmipGameLayerMapInfo;
    GAMELAYERMAPINFO *glmipNext = NULL;
    GAMELAYERMAPINFO *glmipPrevious = NULL;

    *result = GR_INVALID_MAP_ID;
    while (glmipCurrent)
    {
        if (glmipCurrent->iMapID != mapid)
        {
            glmipPrevious = glmipCurrent;
            glmipNext = glmipCurrent->glmipNext;
        }
        else
        {
            /* link previous node to next node */
            if (glmipPrevious)
                glmipPrevious->glmipNext = glmipCurrent->glmipNext;
            else
                glmipGameLayerMapInfo = glmipCurrent->glmipNext;

            glmipNext = glmipCurrent->glmipNext;

            /* free the current node */
            if (glmipCurrent->sMap)
            {
                rfree (glmipCurrent->sMap);
                glmipCurrent->sMap = NULL;
            }
            rfree (glmipCurrent);

            *result = GR_OK;
        }
        glmipCurrent = glmipNext;
    }

    *oos = ios;
}   /* WinGameLayerMapDone */


/* initialize an animation sequence (if succes, result = id) */
void WinInitSpriteAnimation (int bitmapid, CLEAN_STRING seq, BOOL loop, OS ios, int* result, OS* oos)
{
    SPRITEANIMATION *saNew;
    SPRITEANIMATION *sa1;
    SPRITEANIMATION *sa2;
    int newid = 1;
    int resultcode;

    int gbW, gbH, gbBW, gbBH, gbCX, gbCY;

    resultcode = GR_INVALID_BITMAP_ID;
    if (OSGetGameBitmapInfo (bitmapid,
                                &gbW, &gbH, &gbBW, &gbBH, &gbCX, &gbCY))
    {
       /* if (loop) */
        {
            /* first look if animation sequence already is initialized */
            SPRITEANIMATION *sa = saSprites;
            int slen = seq->length / (2 * sizeof (int));
            BOOL equal;
            int i, j, k;

            while (sa)
            {
                if (sa->iBitmapID == bitmapid)
                {
                    if ((sa->bLoop == loop) && (sa->iSequenceLength == slen))
                    {
                        equal = TRUE;
                        for (k = 0; k < slen; k++)
                        {
                            i = (*(int *) &seq->characters[k * sizeof (int)]);
                            j = (*(int *) &sa->sSequence[k * sizeof (int)]);
                            if (i != j)
                                equal = FALSE;
                        }
                        if (equal)
                        {
                            *result = sa->iSpriteID;
                            *oos = ios;
                            return;
                        }
                    }
                }
                sa = sa->saNext;
            }
        }

        saNew = rmalloc (sizeof (SPRITEANIMATION));
        saNew->iBitmapID = bitmapid;
        saNew->iSequenceLength = seq->length / (2 * sizeof (int));
        saNew->sSequence = rmalloc ((seq->length) + 1);
        rsncopy (saNew->sSequence, seq->characters, seq->length);
        saNew->iPosition = 0;  // saNew->iSequenceLength;
        saNew->iCounter = (*(int *) &saNew->sSequence[sizeof (int)]);  // 0
        saNew->saNext = NULL;
        saNew->bLoop = loop;

        sa1 = saSprites;
        sa2 = NULL;
        while (sa1)
        {
            if (sa1->iSpriteID >= newid)
                newid = sa1->iSpriteID + 1;
            sa2 = sa1;
            sa1 = sa1->saNext;
        }
        saNew->iSpriteID = newid;
        if (sa2)
            sa2->saNext = saNew;
        else
            saSprites = saNew;

        resultcode = GR_OK;
    }

    if (resultcode == GR_OK)
        *result = newid;
    else
        *result = resultcode;
    *oos = ios;
}   /* WinInitSpriteAnimation */


/* get value of boundmap[x,y] */
static int ReadBoundMapValue (int x, int y, int options, int bounds, int mccode)
{
    int mappos;
    int result;

    // off map: all bounds, code 0xFF
    if ((x < 0) ||
        (y < 0) ||
        (x > BoundMapWidth - 1) ||
        (y > BoundMapHeight - 1))
    {
        if (options & OO_IGNORE_LEVEL_BOUNDS)
            result = 0x0000;
        else
            result = 0xFF0F;
    }
    else
    {
        mappos = (y * BoundMapWidth + x) * sizeof (int);
        result = (*(int *) &sBoundMap[mappos]);
    }

    if (!(bounds & BND_STATIC_BOUNDS))
        result &= 0xFF00;
    if (bounds & BND_MAP_CODES)
    {
        if ((result >> 8) > 0)
            result |= mccode;
    }
    else
        result &= 0x00FF;

/// diagonal bounds
    if ((result & 0x00F0) != 0)
      result = (result << 16) | 0x00F0;
///

    return result;
}   /* ReadBoundMapValue */


static void Bounce (OBJECTREC *obj, int bound)
{
    int xb = obj->iXBounce;
    int yb = obj->iYBounce;

    if (xb < 0)
        xb = abs (xb * obj->iXSpeed) / 256;
    if (yb < 0)
        yb = abs (yb * obj->iYSpeed) / 256;

    if ((bound & UPPER_BOUND) > 0)
    {
        obj->iYPos = obj->iLastYPos;
        obj->iFixedYPos = obj->iLastFixedYPos;
        obj->iYSpeed = -yb;
    }
    if ((bound & LEFT_BOUND) > 0)
    {
        obj->iXPos = obj->iLastXPos;
        obj->iFixedXPos = obj->iLastFixedXPos;
        obj->iXSpeed = -xb;
    }
    if ((bound & LOWER_BOUND) > 0)
    {
        obj->iYPos = obj->iLastYPos;
        obj->iFixedYPos = obj->iLastFixedYPos;
        obj->iYSpeed = yb;
    }
    if ((bound & RIGHT_BOUND) > 0)
    {
        obj->iXPos = obj->iLastXPos;
        obj->iFixedXPos = obj->iLastFixedXPos;
        obj->iXSpeed = xb;
    }
}   /* Bounce */


/* move objects around */
static void MoveObjects (void)
{
    OBJECTREC *obj;
    int x, y, w, h, i, j, k, xbnd, ybnd, code, bound, Bound;
    int oldmapx1, oldmapy1, oldmapx2, oldmapy2;
    int newmapx1, newmapy1, newmapx2, newmapy2;
    int newx, newy;
    BOOL xblock, yblock, xbump, ybump;
    SPRITEANIMATION *sa;
    int iObjectCount = 0;
    struct USER_EVENT_INFO *uei;
    struct USER_EVENT_INFO *ueiNext;
    struct USER_EVENT_INFO *ueiPrev;
	struct USER_EVENT_INFO *ueiTemp;		// PA++: bugfix Mike Wiering (line 1478-1486)

    // broadcast user events
    uei = ueiUserEventInfo;
    ueiPrev = NULL;
    while (uei)
    {
        ueiNext = uei->ueiNext;
        if (--uei->iTimeCounter <= 0)
        {
            if (uei->iEventID == EV_PLAY_SOUND)
            {
                int id     = uei->iEventParameter1;
                int volume = uei->iEventParameter2;
                int pan    = uei->iEventParameter3;
                int freq   = uei->iEventParameter4;

                OSPlaySoundSample (id, volume, pan, freq);
            }
            else
            {
                obj = objObjects;
                while (obj)
                {
                    if (obj->bActive)
                    {
                        if (((uei->iDestination == 0) ||
                             (uei->iDestination == -obj->iObjectID) ||
                             ((uei->iDestination > 0) &&
                              (uei->iDestination & obj->iOwnBounds)
                             )
                            )
                           &&
                            ((uei->iSubDestination == -1) ||
                             (uei->iSubDestination == obj->iSubType)
                            )
                           )
                        {
                            int ix = obj->iXPos;
                            int iy = obj->iYPos;

                            iCCObjectID = obj->iObjectID;
                            SendMessage5ToClean (CcWmUSEREVENT,
                                                 obj->iObjType,
                                                 obj->iObjectID,
                                                 uei->iEventID,
                                                 uei->iEventParameter1,
                                                 uei->iEventParameter2);
                            iCCObjectID = 0;

                            if ((ix != obj->iXPos) || (iy != obj->iYPos))
                                UpdatePosition (obj);
                        }
                    }
                    obj = obj->objNext;
                }
            }

            // bug fix: create new userevent during a userevent
            ueiTemp = ueiUserEventInfo;
            ueiPrev = NULL;
            while (ueiTemp != uei)
            {
                ueiPrev = ueiTemp;
                ueiTemp = ueiTemp->ueiNext;
            }
            // end of bug fix
            
			if (ueiPrev)
                ueiPrev->ueiNext = ueiNext;
            else
                ueiUserEventInfo = ueiNext;
            rfree (uei);
        }
        else
        {
            ueiPrev = uei;
        }
        uei = ueiNext;
    }


    // check position and call object's move function
    obj = objObjects;
    while (obj)
    {
        if (obj->bActive)
        {
            if (obj->iOptions & OO_STATIC)
            {
                obj->iXPos += XView;
                obj->iYPos += YView;
            }

            // check if the animation sequence has ended
            sa = GetSpriteAnimation (obj->iSpriteID);
            if (sa)
                if (!sa->bLoop)
                {
                    int ps;

                    if (obj->iCounter == 0)
                        obj->iCounter = (*(int *) &sa->sSequence[sizeof (int)]);

                    if (--obj->iCounter <= 0)
                    {
                        obj->iPosition++;
                        if (obj->iPosition >= sa->iSequenceLength)
                            obj->iPosition = -1;
                        if (obj->iPosition > -1)
                        {
                            ps = (2 * obj->iPosition * sizeof (int)) + sizeof (int);
                            obj->iCounter = (*(int *) &sa->sSequence[ps]);
                        }
                    }
                }

            sa = GetSpriteAnimation (obj->iSpriteID);
            if (sa)
                if (!(sa->bLoop))
                    if (obj->iPosition == -1)
                    {
                        // send msg to Clean
                        int ix = obj->iXPos;
                        int iy = obj->iYPos;

                        iCCObjectID = obj->iObjectID;
                        obj->iLastSprite = -1;
                        SendMessage2ToClean (CcWmANIMATION, obj->iObjType, obj->iObjectID);
                        iCCObjectID = 0;

                        if ((ix != obj->iXPos) || (iy != obj->iYPos))
                            UpdatePosition (obj);
                    }


            // object too far away?

            x = obj->iXPos;
            y = obj->iYPos;

            if (obj->bActive)
            {
        /*
            if (sa)
            {
                gbip = GetGameBitmapInfo (sa->iBitmapID);
                if (gbip)
                {
                    w = gbip->iBlockWidth;
                    h = gbip->iBlockHeight;
                */
                    w = obj->iXSize;
                    h = obj->iYSize;

                    if (x + w < XView - ((obj->iForgetX + iStartObjX) * BoundBlockWidth))
                        obj->bActive = FALSE;
                    if (y + h < YView - ((obj->iForgetY + iStartObjY) * BoundBlockHeight))
                        obj->bActive = FALSE;
                    if (x > XView + ScreenWidth + ((obj->iForgetX + iStartObjX) * BoundBlockWidth))
                        obj->bActive = FALSE;
                    if (y > YView + ScreenHeight + ((obj->iForgetY + iStartObjY) * BoundBlockHeight))
                        obj->bActive = FALSE;

                    if ((obj->iTimeCounter >= 0) || (!(obj->iOptions & OO_FREEZE)))
                    {
                        if ((obj->bActive) && (!(obj->iOptions & OO_STATIC)))
                        {
                        //    int md = obj->iMoveDelay;
                            int slx = obj->iXSlowDown;
                            int sly = obj->iYSlowDown;
                            //    int xv, yv;   (PA: these are not used)

                            // save last position
                            obj->iLastXPos = obj->iXPos;
                            obj->iLastYPos = obj->iYPos;

                            obj->iLastFixedXPos = obj->iFixedXPos;
                            obj->iLastFixedYPos = obj->iFixedYPos;

                            // acceleration
                       //     if ((md == 0) || ((iFrameCounter % md) == 0))
                       //     {
                                obj->iXSpeed += obj->iXAcc;
                                obj->iYSpeed += obj->iYAcc;
                       //     }

                            // slow down the object?
                       //     if (obj->iOptions & OO_FIXED)
                       //     {
                                if (slx > 0)
                                {
                                    if (obj->iXSpeed > 0)
                                        obj->iXSpeed = Max (obj->iXSpeed - slx, 0);
                                    if (obj->iXSpeed < 0)
                                        obj->iXSpeed = Min (obj->iXSpeed + slx, 0);
                                }
                                if (sly > 0)
                                {
                                    if (obj->iYSpeed > 0)
                                        obj->iYSpeed = Max (obj->iYSpeed - sly, 0);
                                    if (obj->iYSpeed < 0)
                                        obj->iYSpeed = Min (obj->iYSpeed + sly, 0);
                                }
                       //     }
                       /*
                            else
                            {
                                if ((slx > 0) && ((iFrameCounter % slx) == 0))
                                {
                                    if (obj->iXSpeed > 0) obj->iXSpeed--;
                                    if (obj->iXSpeed < 0) obj->iXSpeed++;
                                }
                                if ((sly > 0) && ((iFrameCounter % sly) == 0))
                                {
                                    if (obj->iYSpeed > 0) obj->iYSpeed--;
                                    if (obj->iYSpeed < 0) obj->iYSpeed++;
                                }
                            }
                       */
                            if (slx < 0)
                                obj->iXSpeed = obj->iXSpeed * (256 + slx) / 256;
                            if (sly < 0)
                                obj->iYSpeed = obj->iYSpeed * (256 + sly) / 256;

                            // check object's maximum speed
                            if (obj->iXSpeed > obj->iMaxXSpeed)
                                obj->iXSpeed = obj->iMaxXSpeed;
                            if (obj->iXSpeed < -obj->iMaxXSpeed)
                                obj->iXSpeed = -obj->iMaxXSpeed;
                            if (obj->iYSpeed > obj->iMaxYSpeed)
                                obj->iYSpeed = obj->iMaxYSpeed;
                            if (obj->iYSpeed < -obj->iMaxYSpeed)
                                obj->iYSpeed = -obj->iMaxYSpeed;

                            // move the object
                        //    if (obj->iOptions & OO_FIXED)
                        //    {
                                obj->iFixedXPos += obj->iXSpeed;
                                obj->iFixedYPos += obj->iYSpeed;
                                obj->iXPos = (obj->iFixedXPos + 128) >> 8;
                                obj->iYPos = (obj->iFixedYPos + 128) >> 8;
                        //    }
                        /*  else
                            {
                                obj->iXPos += obj->iXSpeed;
                                obj->iYPos += obj->iYSpeed;
                            }  */

                            if (obj->iSkipMove > 0)
                                obj->iSkipMove--;
                            else
                                if (obj->iSkipMove == 0)
                                {
                                    // inform Clean that new object may move now
                                    int ix = obj->iXPos;
                                    int iy = obj->iYPos;

                                    iCCObjectID = obj->iObjectID;
                                    SendMessage2ToClean (CcWmMOVEOBJECT, obj->iObjType, obj->iObjectID);
                                    iCCObjectID = 0;
/*
                                    obj->iFixedXPos = obj->iXPos << 8;
                                    obj->iFixedYPos = obj->iYPos << 8;
                                    obj->iLastFixedXPos = obj->iFixedXPos;
                                    obj->iLastFixedYPos = obj->iFixedYPos;
*/

                                    if ((ix != obj->iXPos) || (iy != obj->iYPos))
                                        UpdatePosition (obj);

                                }
                        }
                    }
            /*
                }
            */
            }
        }

        if (obj->bActive)
            iObjectCount++;

        obj = obj->objNext;
    }

    // check collisions between objects

    if (iObjectCount > 1)
    {
        int pos = 0;
        int p = 0;
        int *collisions = rmalloc (4 * iObjectCount *
                            (iObjectCount - 1) * sizeof (int));

        obj = objObjects;
        while (obj)
        {
            if ((obj->bActive) &&
                ((obj->iBounceBounds) || (obj->iCollideBounds)))
            {
                int X1 = obj->iXPos;
                int Y1 = obj->iYPos;
                int X2 = X1 + obj->iXSize - 1;
                int Y2 = Y1 + obj->iYSize - 1;
                int OldX1 = obj->iLastXPos;
                int OldY1 = obj->iLastYPos;
                int OldX2 = OldX1 + obj->iXSize - 1;
                int OldY2 = OldY1 + obj->iYSize - 1;

                OBJECTREC *objTmp = objObjects;

                while (objTmp)
                {
                    if ((objTmp->bActive) &&
                        (obj != objTmp) &&
                        ((obj->iBounceBounds & objTmp->iOwnBounds) ||
                         (obj->iCollideBounds & objTmp->iOwnBounds))
                       )
                    {
                        int x1 = objTmp->iXPos;
                        int y1 = objTmp->iYPos;
                        int x2 = x1 + objTmp->iXSize - 1;
                        int y2 = y1 + objTmp->iYSize - 1;
                        int oldx1 = objTmp->iLastXPos;
                        int oldy1 = objTmp->iLastYPos;
                        int oldx2 = oldx1 + objTmp->iXSize - 1;
                        int oldy2 = oldy1 + objTmp->iYSize - 1;

                        // do the two objects collide?
                        if ((x1 <= X2) && (x2 >= X1) &&
                            (y1 <= Y2) && (y2 >= Y1))
                        {
                            /*
                               bit 0: upper bound
                               bit 1: left bound
                               bit 2: lower bound
                               bit 3: right bound
                            */

                            // for each collision two messages are sent
                            Bound = 0;
                            bound = 0;
                            {
                                // detect directions of new collisions
                                if (OldY1 > oldy2)
                                {
                                    Bound |= UPPER_BOUND;
                                    bound |= LOWER_BOUND;
                                }
                                if (OldX1 > oldx2)
                                {
                                    Bound |= LEFT_BOUND;
                                    bound |= RIGHT_BOUND;
                                }
                                if (OldY2 < oldy1)
                                {
                                    Bound |= LOWER_BOUND;
                                    bound |= UPPER_BOUND;
                                }
                                if (OldX2 < oldx1)
                                {
                                    Bound |= RIGHT_BOUND;
                                    bound |= LEFT_BOUND;
                                }
                            }

                            // first store all collisions, then call
                            //   the collide function for each
                            collisions[pos++] = obj->iObjectID;
                            collisions[pos++] = objTmp->iObjectID;
                            collisions[pos++] = bound;
                            collisions[pos++] = Bound;
                        }
                    }
                    objTmp = objTmp->objNext;
                }
            }
            obj = obj->objNext;
        }

        // maybe sort the list of colliding objects?
        // ...

        // call the collide functions
        while (p < pos)
        {
            OBJECTREC *objTmp;
            int bnds;

            obj = GetObjectRec (collisions[p++]);
            objTmp = GetObjectRec (collisions[p++]);

            bnds = collisions[p++];
            if (obj->iBounceBounds & objTmp->iOwnBounds)
                Bounce (obj, bnds);

            if (obj->iCollideBounds & objTmp->iOwnBounds)
            {
                int ix = obj->iXPos;
                int iy = obj->iYPos;

                iCCObjectID = obj->iObjectID;
                SendMessage5ToClean (CcWmCOLLISION,
                    obj->iObjType, obj->iObjectID,
                    objTmp->iObjType, objTmp->iObjectID, bnds);
                iCCObjectID = 0;

                if ((ix != obj->iXPos) || (iy != obj->iYPos))
                    UpdatePosition (obj);
            }
            p++;
        /*
            if (objTmp->iOptions & OO_BOUNCE_AT_COLLISIONS)
                Bounce (objTmp, collisions[p++]);
            else
            {
                iCCObjectID = objTmp->iObjectID;
                SendMessage5ToClean (CcWmCOLLISION,
                    objTmp->iObjType, objTmp->iObjectID,
                    obj->iObjType, obj->iObjectID, collisions[p++]);
                iCCObjectID = 0;
            }
        */

        }

        rfree (collisions);
    }


    // check collisions with static bounds and map codes
    obj = objObjects;
    while (obj)
    {
        int bbw = BoundBlockWidth;
        int bbh = BoundBlockHeight;

        if ((obj->bActive) &&
            ((obj->iBounceBounds & (BND_STATIC_BOUNDS | BND_MAP_CODES)) ||
             (obj->iCollideBounds & (BND_STATIC_BOUNDS | BND_MAP_CODES)))
           )
        {
            // check if the object touches a bound in the boundmap
            x = obj->iXPos;
            y = obj->iYPos;

            newmapx1 = intdiv (x, BoundBlockWidth);
            newmapy1 = intdiv (y, BoundBlockHeight);
            newmapx2 = intdiv (x + obj->iXSize - 1, BoundBlockWidth);
            newmapy2 = intdiv (y + obj->iYSize - 1, BoundBlockHeight);

            oldmapx1 = intdiv (obj->iLastXPos, BoundBlockWidth);
            oldmapy1 = intdiv (obj->iLastYPos, BoundBlockHeight);
            oldmapx2 = intdiv (obj->iLastXPos + obj->iXSize - 1, BoundBlockWidth);
            oldmapy2 = intdiv (obj->iLastYPos + obj->iYSize - 1, BoundBlockHeight);

            xblock = FALSE;
            yblock = FALSE;

            /* object's X position on map changed? */
            if (newmapx1 < oldmapx1)  // moving left
            {
                xblock = TRUE;
                newx = newmapx1;
                xbnd = RIGHT_BOUND;  // check right bound
            }
            if (newmapx2 > oldmapx2)  // moving right
            {
                xblock = TRUE;
                newx = newmapx2;
                xbnd = LEFT_BOUND;  // check left bound
            }

            /* object's Y position on map changed? */
            if (newmapy1 < oldmapy1)  // moving up
            {
                yblock = TRUE;
                newy = newmapy1;
                ybnd = LOWER_BOUND;  // check lower bound
            }
            if (newmapy2 > oldmapy2)  // moving down
            {
                yblock = TRUE;
                newy = newmapy2;
                ybnd = UPPER_BOUND;  // check upper bound
            }


        /* --------------- bounce -------------- */

            xbump = FALSE;
            ybump = FALSE;
            bound = 0;
            /*
               bit 0: upper bound
               bit 1: left bound
               bit 2: lower bound
               bit 3: right bound
            */

            /* object touches a vertical bound (wall)? */
            if (xblock)
                for (j = oldmapy1; j <= oldmapy2; j++)
                    bound |= (ReadBoundMapValue (newx, j, obj->iOptions,
                                 obj->iBounceBounds, 0x0F) & xbnd);
            if (bound & xbnd)
                xbump = TRUE;


            /* object touches a horizontal bound (floor)? */
            if (yblock)
                for (i = oldmapx1; i <= oldmapx2; i++)
                    bound |= (ReadBoundMapValue (i, newy, obj->iOptions,
                                 obj->iBounceBounds, 0x0F) & ybnd);
            if (bound & ybnd)
                ybump = TRUE;


            /* object touches a corner? */
            if ((xblock && yblock) && ((!xbump) && (!ybump)))
            {
                bound |= (ReadBoundMapValue (newx, newy, obj->iOptions,
                             obj->iBounceBounds, 0x0F) & (xbnd | ybnd));
                xbump = TRUE;
                ybump = TRUE;
            }

            Bounce (obj, bound);


/// diagonal bounds
          if (obj->iBounceBounds & BND_STATIC_BOUNDS)
          {
            if (obj->iYSpeed < - (256))
              obj->iDiag = 0;

            if ((obj->iDiag == 0) && (obj->iYSpeed >= - (256)))
            {
              int CurX = obj->iLastXPos + obj->iXSize / 2;
              int CurY = obj->iLastYPos + obj->iYSize - 1;
              int NewX = obj->iXPos + obj->iXSize / 2;
              int NewY = obj->iYPos + obj->iYSize - 1;
              int CurMapX = intdiv (CurX, bbw);
              int CurMapY = intdiv (CurY, bbh);
              int NewMapX = intdiv (NewX, bbw);
              int NewMapY = intdiv (NewY, bbh);

              int dgx, dgy, dgbnd, bndx, bndy;

              int l = CurMapX;
              int r = NewMapX;
              if (l > r)
              {
                l = NewMapX;
                r = CurMapX;
              }

              bound = 0;
              for (dgx = l; dgx <= r; dgx++)
                for (dgy = CurMapY; dgy <= NewMapY; dgy++)
                {
                  dgbnd = ReadBoundMapValue (dgx, dgy,
                            obj->iOptions, obj->iBounceBounds, 0) >> 16;
                  if (dgbnd >= 0x80)
                  {
                    bndx = dgx;
                    bndy = dgy;
                    bound = dgbnd;
                  }
                }
              if (bound != 0)
              {
                obj->iDiagX = (bndx * bbw + bbw / 2);
                obj->iDiagY = (bndy * bbh + bbh / 2) - 1;
                if (bound & 1 == 1)
                  obj->iDiagUpDn = 1;  // "\"
                else
                  obj->iDiagUpDn = -1;  // "/"
                i = 0;
                while ((ReadBoundMapValue (bndx + i, bndy + obj->iDiagUpDn * i,
                       obj->iOptions, obj->iBounceBounds, 0) >> 16) == bound)
                  i--;
                obj->iDiagLeftX = (bndx + i + 1) * bbw - bbw / 2;
                obj->iDiagLeftY = (bndy + obj->iDiagUpDn * i + 1) * bbh;
                i = 0;
                while ((ReadBoundMapValue (bndx + i, bndy + obj->iDiagUpDn * i,
                       obj->iOptions, obj->iBounceBounds, 0) >> 16) == bound)
                  i++;
                obj->iDiagRightX = (bndx + i - 1) * bbw + bbw / 2;
                obj->iDiagRightY = (bndy + obj->iDiagUpDn * (i - 1) + 1) * bbh;

                obj->iDiag = 1;
              }
            }

            if (obj->iDiag == 1)
            {
              int iCurX = obj->iLastXPos + obj->iXSize / 2;
              int iCurY = obj->iLastYPos + obj->iYSize - 1;
              int iNewX = obj->iXPos + obj->iXSize / 2;
              int iNewY = obj->iYPos + obj->iYSize - 1;

              i = obj->iDiagY + (obj->iDiagUpDn * bbh *
                       (iCurX - obj->iDiagX)) / bbw;
              if (i <= iNewY)
              {
                obj->iYPos = i - obj->iYSize;
                obj->iFixedYPos = obj->iYPos << 8;
                obj->iLastYPos = obj->iYPos;
                obj->iLastFixedYPos = obj->iYPos << 8;
               // obj->iYSpeed = 0;
                Bounce (obj, UPPER_BOUND);
              }
              if ((iCurX < obj->iDiagLeftX + bbw / 2) &&
                  ((iNewY >= obj->iDiagLeftY)))
              {
                obj->iYPos = obj->iDiagLeftY - obj->iYSize;
                obj->iFixedYPos = obj->iYPos << 8;
                obj->iLastYPos = obj->iYPos;
                obj->iLastFixedYPos = obj->iYPos << 8;
               // obj->iYSpeed = 0;
                Bounce (obj, UPPER_BOUND);
              }
              if ((obj->iXPos >= obj->iDiagRightX - bbw / 2) &&
                  ((iNewY >= obj->iDiagRightY)))
              {
                obj->iYPos = obj->iDiagRightY - obj->iYSize;
                obj->iFixedYPos = obj->iYPos << 8;
                obj->iLastYPos = obj->iYPos;
                obj->iLastFixedYPos = obj->iYPos << 8;
               // obj->iYSpeed = 0;
                Bounce (obj, UPPER_BOUND);
              }

              if (obj->iXPos < obj->iDiagLeftX)
                obj->iDiag = 0;
              if (obj->iXPos >= obj->iDiagRightX)
                obj->iDiag = 0;
            }
          }

/// diagonal bounds
/*
            int CurX = obj->iLastXPos + obj->iXSize / 2;
            int CurY = obj->iLastYPos + obj->iYSize - 1;
            int NewX = obj->iXPos + obj->iXSize / 2;
            int NewY = obj->iYPos + obj->iYSize - 1;

            if ((NewX != CurX) || (NewY > CurY))
            {
              int CurMapX = intdiv (CurX, BoundBlockWidth);
              int CurMapY = intdiv (CurY, BoundBlockHeight);
              int NewMapX = intdiv (NewX, BoundBlockWidth);
              int NewMapY = intdiv (NewY, BoundBlockHeight);

              int dgy;
              int dgx;

              for (dgy = Min (CurMapY, NewMapY) - 1;
                      dgy <= Max (CurMapY, NewMapY); dgy++)
                for (dgx = Min (CurMapX, NewMapX);
                        dgx <= Max (CurMapX, NewMapX); dgx++)
                {
                  int dgbnd = ReadBoundMapValue (dgx, dgy,
                                obj->iOptions, obj->iBounceBounds, 0) >> 16;
                  if (dgbnd >= 0x80)
                  {
                    int leftx = dgx * BoundBlockWidth;
                    int lefty = dgy * BoundBlockHeight;
                    int RelNewX;
                    int DiagHeight;

                    int rc = 1;  // "\"
                    if ((dgbnd & 1) == 0)
                    {
                      rc = -rc;       // "/"
                      lefty += BoundBlockHeight;
                    }

                    dgbnd &= 0x7F;
                 //   int RelCurX = CurX - leftx;
                    RelNewX = NewX - leftx;

                    DiagHeight = lefty +
                        (rc * BoundBlockHeight * RelNewX) / BoundBlockWidth;

                    if ((CurY <= DiagHeight) && (NewY >= DiagHeight))
                    {
                      obj->iLastYPos = DiagHeight - obj->iYSize;
                      obj->iLastFixedYPos = (obj->iLastYPos << 8);

                      if ((RelNewX >= 0) && (RelNewX < BoundBlockWidth))
                        Bounce (obj, UPPER_BOUND);
                    }

                  }

                }
            }
*/
///

        /* --------------- collide -------------- */

            xbump = FALSE;
            ybump = FALSE;
            bound = 0;
            /*
               bit 0: upper bound
               bit 1: left bound
               bit 2: lower bound
               bit 3: right bound
            */
            code = 0;

            /* object touches a vertical bound (wall)? */
            if (xblock)
                for (j = oldmapy1; j <= oldmapy2; j++)
                {
                    k = ReadBoundMapValue (newx, j,
                             obj->iOptions, obj->iCollideBounds, 0);
                    if ((k >> 8) > code)
                        code = (k >> 8);
                    bound |= (k & xbnd);
                }
            if (bound & xbnd)
                xbump = TRUE;

            /* object touches a horizontal bound (floor)? */
            if (yblock)
                for (i = oldmapx1; i <= oldmapx2; i++)
                {
                    k = ReadBoundMapValue (i, newy,
                                 obj->iOptions, obj->iCollideBounds, 0);
                    if ((k >> 8) > code)
                        code = (k >> 8);
                    bound |= (k & ybnd);
                }
            if (bound & ybnd)
                ybump = TRUE;

            /* object touches a corner? */
            if ((xblock && yblock) && ((!xbump) && (!ybump)))
            {
                k = ReadBoundMapValue (newx, newy, obj->iOptions,
                                 obj->iCollideBounds, 0);
                if ((k >> 8) > code)
                    code = (k >> 8);
                bound |= (k & (xbnd | ybnd));

                xbump = TRUE;
                ybump = TRUE;
            }

            if ((bound > 0) || (code > 0))
            {
                int ix = obj->iXPos;
                int iy = obj->iYPos;

                // obj has touched something, send msg to Clean
                iCCObjectID = obj->iObjectID;
                SendMessage4ToClean (CcWmTOUCHBOUND, obj->iObjType, obj->iObjectID, bound, code);
                iCCObjectID = 0;

                if ((ix != obj->iXPos) || (iy != obj->iYPos))
                    UpdatePosition (obj);
            }

            /* update last directions */
            if (obj->iXPos < obj->iLastXPos)  /* moving left */
                obj->iOptions |= OO_LAST_DIRECTION_LEFT;
            else
                if (obj->iXPos > obj->iLastXPos)  /* moving right */
                    obj->iOptions &= ~ OO_LAST_DIRECTION_LEFT;

            if (obj->iYPos < obj->iLastYPos)  /* moving up */
                obj->iOptions |= OO_LAST_DIRECTION_UP;
            else
                if (obj->iYPos > obj->iLastYPos)  /* moving down */
                    obj->iOptions &= ~ OO_LAST_DIRECTION_UP;
        }

        // increment object's frame counter
        if (obj->bActive)
        {
            obj->iTimeCounter++;
            if (obj->iTimeCounter == 0)
            {
                // send timer msg
                int ix = obj->iXPos;
                int iy = obj->iYPos;

                iCCObjectID = obj->iObjectID;
                SendMessage2ToClean (CcWmOBJECTTIMER, obj->iObjType, obj->iObjectID);
                iCCObjectID = 0;

                if ((ix != obj->iXPos) || (iy != obj->iYPos))
                    UpdatePosition (obj);
            }

            if (obj->iObjectID == iFollowID)
            {
                // screen focus
                int x1 = obj->iXPos;
                int y1 = obj->iYPos;
                int x2 = x1 + obj->iXSize;
                int y2 = y1 + obj->iYSize;

                if (x1 < obj->iLastXPos)
                    if (iFollowX1 != FC_OFFSCREEN)
                        if (x1 - iFollowX1 < iActualXView)
                            iActualXView = x1 - iFollowX1;
                if (x1 > obj->iLastXPos)
                    if (iFollowX2 != FC_OFFSCREEN)
                        if (x2 + iFollowX2 > iActualXView + ScreenWidth)
                            iActualXView = x2 + iFollowX2 - ScreenWidth;

                if ((y1 < obj->iLastYPos) || (obj->iDiag == 1))
                    if (iFollowY1 != FC_OFFSCREEN)
                        if (y1 - iFollowY1 < iActualYView)
                            iActualYView = y1 - iFollowY1;
                if ((y1 > obj->iLastYPos) || (obj->iDiag == 1))
                    if (iFollowY2 != FC_OFFSCREEN)
                        if (y2 + iFollowY2 > iActualYView + ScreenHeight)
                            iActualYView = y2 + iFollowY2 - ScreenHeight;
            }
        }

        if (obj->iOptions & OO_STATIC)
        {
            obj->iXPos -= XView;
            obj->iYPos -= YView;
        }

        obj = obj->objNext;
    }


    // initialize new objects
    obj = objObjects;
    while (obj)
    {
        if (!(obj->bActive))
        {
            // new object?
            if ((obj->iMapX == -1) && (obj->iMapY == -1))
            {
                obj->iMapX = -2;
                obj->iMapY = -2;

                iCCObjectID = obj->iObjectID;
                SendMessage6ToClean (CcWmINITOBJECT,
                                     obj->iObjType, obj->iSubType,
                                     obj->iObjectID, obj->iXPos, obj->iYPos,
                                     iFrameCounter);
                iCCObjectID = 0;

                if (obj->bActive)
                {
                    obj->iLastXPos = obj->iXPos;
                    obj->iLastYPos = obj->iYPos;

                    obj->iFixedXPos = obj->iXPos << 8;
                    obj->iFixedYPos = obj->iYPos << 8;
                    obj->iLastFixedXPos = obj->iFixedXPos;
                    obj->iLastFixedYPos = obj->iFixedYPos;
                }
            }
        }
        obj = obj->objNext;
    }


    // remove all objects that are not active
    GameObjectDone (0);

    CorrectView ();
}   /* MoveObjects */

#ifdef SMART_DRAW
/* avoid drawing the same tile if it is already there */
static void SaveSmart (int x, int y, int tilenr)
{
    int pos = (y * iLastTileW + x) * sizeof (int);

    (*(int *) &sLastTile[pos]) =
        setflipint ((*(int *) &sLastTile[pos]), tilenr);
}   /* SaveSmart */

static int GetSmart (int x, int y)
{
    int pos = (y * iLastTileW + x) * sizeof (int);

    return (*(int *) &sLastTile[pos]);
}   /* GetSmart */

static void EraseSmart (int x, int y)
{
    if ((x >= 0) && (x <= iLastTileW))
        if ((y >= 0) && (y <= iLastTileH))
            SaveSmart (x, y, 0);
}   /* EraseSmart */

static void EraseSmartArea (int x1, int y1, int x2, int y2)
{
    int x, y;
    int i, j;

    x1--;
    y1--;
    x2++;
    y2++;

    x1 = (x1 - (getflipint (iTopLayerLastX) % BoundBlockWidth));
    x1 = x1 / BoundBlockWidth;
    y1 = (y1 - (getflipint (iTopLayerLastY) % BoundBlockHeight));
    y1 = y1 / BoundBlockHeight;

    x2 = (x2 - (getflipint (iTopLayerLastX) % BoundBlockWidth));
    x2 = x2 / BoundBlockWidth;
    y2 = (y2 - (getflipint (iTopLayerLastY) % BoundBlockHeight));
    y2 = y2 / BoundBlockHeight;

    for (j = y1; j <= (y2 + 1); j++)
        for (i = x1; i <= (x2 + 1); i++)
            EraseSmart (i, j);
}   /* EraseSmartArea */
#endif

/* draw objects */
static void DrawObjects (int FromLayer, int ToLayer)
{
    OBJECTREC *obj = objObjects;
    int x, y, w, h, sw, sh, dw, dh;
    int bmpx, bmpy;
    int seqpos, maxblock, mappos, mapblock;
    BOOL mir, ups;
    int bid;

    SPRITEANIMATION *sa;

    RECT src, dst;
    int fx;     // PA: flags was declared but not used

    while (obj)
    {
        if (
             (obj->bActive)
             &&
             ((!(obj->iDisplayOptions & DO_BLINK)) || (iFrameCounter % 2 == 0))
           )
        if (
            (obj->iLayer >= FromLayer) && (obj->iLayer <= ToLayer)
           )
        {
            x = obj->iXPos + obj->iXOffset;
            y = obj->iYPos + obj->iYOffset;

            if (!(obj->iOptions & OO_STATIC))
            {
                x -= XView;
                y -= YView;
            }

            if ((x < ScreenWidth) && (y < ScreenHeight))
            {
                sa = GetSpriteAnimation (obj->iSpriteID);
                if (sa)
                {
                    int gbW, gbH, gbBW, gbBH, gbCX, gbCY;

                    bid = sa->iBitmapID;
                    if (OSGetGameBitmapInfo (bid,
                                   &gbW, &gbH, &gbBW, &gbBH, &gbCX, &gbCY))
                    {
                        w = gbBW;
                        h = gbBH;

                        dw = w;  // dst size
                        dh = h;
                        if (obj->iDisplayOptions & DO_STRETCH)
                        {
                            dw = obj->iXSize;
                            dh = obj->iYSize;
                        }
          //
                        src.left = 0;
                        src.top = 0;
                        src.right = w;
                        src.bottom = h;

                        dst.left = x;
                        dst.top = y;
                        dst.right = x + dw;
                        dst.bottom = y + dh;

                        seqpos = sa->iPosition;
                        if (!(sa->bLoop))
                            seqpos = obj->iPosition;

                        if ((dst.right >= 0) &&
                            (dst.bottom >= 0) &&
                            (seqpos >= 0))
                        {
                            maxblock = gbCX * gbCY;
                            mappos = 2 * sizeof (int) * seqpos;
                            mapblock = (*(int *) &sa->sSequence[mappos]) - 1;

                            fx = (mapblock / maxblock) & 3;
                            mapblock %= maxblock;
                            bmpx = (mapblock % gbCX) * w;
                            bmpy = (mapblock / gbCX) * h;

                            mir = FALSE;
                            if ( ((fx & 1) == 1)
                               ^ (obj->iDisplayOptions & DO_MIRROR_LEFT_RIGHT)
                               ^ ( (obj->iOptions & OO_AUTO_MIRROR_LEFT_RIGHT) &&
                                   (obj->iOptions & OO_LAST_DIRECTION_LEFT)
                                 )
                               )
                            {
                                mir = TRUE;
                            }

                            ups = FALSE;
                            if ( ((fx & 2) == 2)
                               ^ (obj->iDisplayOptions & DO_MIRROR_UP_DOWN)
                               ^ ( (obj->iOptions & OO_AUTO_MIRROR_UP_DOWN) &&
                                   (obj->iOptions & OO_LAST_DIRECTION_UP)
                                 )
                               )
                            {
                                ups = TRUE;
                            }

                            if (dst.right > ScreenWidth)
                            {
                                sw = ((ScreenWidth - dst.left) * w + dw - 1) / dw;
                                if (!mir)
                                    src.right = src.left + sw;
                                else
                                    src.left = src.right - sw;
                                dw = ScreenWidth - dst.left;
                                dst.right = ScreenWidth;
                            }
                            if (dst.bottom > ScreenHeight)
                            {
                                sh = ((ScreenHeight - dst.top) * h + dh - 1) / dh;
                                if (!ups)
                                    src.bottom = src.top + sh;
                                else
                                    src.top = src.bottom - sh;
                                dh = ScreenHeight - dst.top;
                                dst.bottom = ScreenHeight;
                            }
                            if (dst.left < 0)
                            {
                                sw = ((dst.right) * w + dw - 1) / dw;
                                if (!mir)
                                    src.left = src.right - sw;
                                else
                                    src.right = src.left + sw;
                                dw = -dst.right;
                                dst.left = 0;
                            }
                            if (dst.top < 0)
                            {
                                sh = ((dst.bottom) * h + dh - 1) / dh;
                                if (!ups)
                                    src.top = src.bottom - sh;
                                else
                                    src.bottom = src.top + sh;
                                dh = -dst.bottom;
                                dst.top = 0;
                            }

                            src.left += bmpx;
                            src.top += bmpy;
                            src.right += bmpx;
                            src.bottom += bmpy;

#ifdef SMART_DRAW
                            if (cmpflipint (bAllowSmartDraw, 1))
                                EraseSmartArea (dst.left, dst.top,
                                                dst.right, dst.bottom);
#endif

                            OSDraw (&dst, bid, &src, mir, ups,
                                    obj->iDisplayOptions);
                        }
                    }
                }
            }
        }
        else
        {
            if ((obj->iLayer > ToLayer) && (obj->iLayer < iNextLayer))
                iNextLayer = obj->iLayer;
        }
        obj = obj->objNext;
    }
}   /* DrawObjects */


/* calculate layer offset and draw layer */
int DrawLayer (int mapid)
{
/*
    HRESULT ddrval;
    DDBLTFX ddbltfx;
*/
    RECT r, src, dst;

    int resultcode;
    int xstart, ystart;
    int x, y;
    int w, h;
    int mapx, mapy;
    int mapw, maph;
    int curw, curh;
    int mappos, mapblock;
    int bmpx, bmpy;
    int fx;         // PA: bltflags, flags were declared but not used
    int maxblock;
    BOOL mir, ups;

    GAMELAYERMAPINFO *glmip = GetGameLayerMapInfo (mapid);

    resultcode = GR_INVALID_MAP_ID;
    if (glmip)
    {
        int gbW, gbH, gbBW, gbBH, gbCX, gbCY;

        resultcode = GR_INVALID_BITMAP_ID;
      /*
        gbip = GetGameBitmapInfo (glmip->iBitmapID);
        if (gbip)
      */

        if (OSGetGameBitmapInfo (glmip->iBitmapID,
                                    &gbW, &gbH, &gbBW, &gbBH, &gbCX, &gbCY))
        {
            w = gbBW;
            h = gbBH;

            maxblock = gbCX * gbCY;

            mapw = glmip->iMapWidth;
            maph = glmip->iMapHeight;


            /* calculate layer position */

            SendMessage4ToClean (CcWmSCROLL, mapid, XView, YView, iFrameCounter);
            xstart = gCci.p1;
            ystart = gCci.p2;

#ifdef SMART_DRAW
            if ((glmip->glmipNext == NULL) &&
                (mapid == 1) &&
                (bFillBackground == FALSE) &&
                (cmpflipint (iTopLayerLastX, xstart)) &&
                (cmpflipint (iTopLayerLastY, ystart)))
            {
                if (cmpflipint (bAllowSmartDraw, 1))
                    bSmartDraw = setflipint (bSmartDraw, 1);
                else
                    bSmartDraw = setflipint (bSmartDraw, 0);
                bAllowSmartDraw = setflipint (bAllowSmartDraw, 1);
            }
            else
            {
                bSmartDraw = setflipint (bSmartDraw, 0);
                bAllowSmartDraw = setflipint (bAllowSmartDraw, 0);
            }

            if (glmip->glmipNext == NULL)
            {
                iTopLayerLastX = setflipint (iTopLayerLastX, xstart);
                iTopLayerLastY = setflipint (iTopLayerLastY, ystart);
            }
#endif
            mapy = ystart / h;
            r.top = ystart % h;

            for (y = 0; y < ScreenHeight; )
            {
                mapx = xstart / w;
                r.left = xstart % w;
                curh = Min (ScreenHeight - y, h - r.top);

                for (x = 0; x < ScreenWidth; )
                {
                    curw = Min (ScreenWidth - x, w - r.left);

                    /* calculate position in map, read block number */
                    mappos = ((mapx % mapw) + ((mapy % maph) * mapw));
                    mappos *= sizeof (int);
                    mapblock = (*(int *) &glmip->sMap[mappos]);

                    if (mapblock < 0)  // sequence ID
                        mapblock = OSGetCurrentBlock (glmip->iBitmapID,
                                                      mapblock);

#ifdef SMART_DRAW
                    if (cmpflipint (bAllowSmartDraw, 1))
                        SaveSmart (x / w, y / h, mapblock);
                    if (cmpflipint (bSmartDraw, 1))
                        if (cmpflipint (GetSmart (x / w, y / h), mapblock))
                            mapblock = 0;
#endif
                    mapblock--;
                    fx = (mapblock / maxblock) & 3;

                    if (mapblock >= 0)
                    {
                        mapblock %= maxblock;
                        bmpx = (mapblock % gbCX) * w;
                        bmpy = (mapblock / gbCX) * h;
                        src.left = r.left + bmpx;
                        src.top = r.top + bmpy;

                        mir = FALSE;
                        ups = FALSE;
                        if ((fx & 1) == 1)
                        {
                            mir = TRUE;
                            src.left = bmpx + w - curw - (r.left % w);
                        }
                        if ((fx & 2) == 2)
                        {
                            ups = TRUE;
                            src.top = bmpy + h - curh - (r.top % w);
                        }
                        src.right = src.left + curw;
                        src.bottom = src.top + curh;
                        dst.left = x;
                        dst.top = y;
                        dst.right = x + curw;
                        dst.bottom = y + curh;
                        OSDraw (&dst, glmip->iBitmapID, &src, mir, ups, 0);

                    }
                    x += curw;
                    r.left = 0;
                    mapx++;
                }
                y += curh;
                r.top = 0;
                mapy++;
            }
            resultcode = GR_OK;
        }
    }
    return resultcode;
}   /* DrawLayer */


/* run sequences, draw layers and flip pages */
static int NextFrame (int fade)
{
    int resultcode;
    //    DDBLTFX ddbltfx;     PA: applications of ddbltfx have been commented out
    BOOL clear;
    GAMELAYERMAPINFO *glmip = glmipGameLayerMapInfo;
    SPRITEANIMATION *sa = saSprites;
    int i;

    resultcode = GR_FAILED;
    if (bGameActive)
    {
        /* run block sequences */
        OSRunBlockSequences ();

        /* run sprite animations */
        while (sa)
        {
            if ((sa->bLoop) && (--sa->iCounter <= 0))
            {
                sa->iPosition++;
                if (sa->iPosition >= sa->iSequenceLength)
                    sa->iPosition = 0;
                i = (2 * sa->iPosition * sizeof (int)) + sizeof (int);
                sa->iCounter = (*(int *) &sa->sSequence[i]);
            }
            sa = sa->saNext;
        }

        MoveObjects ();


        if (iFrameCounter < 2)
            clear = TRUE;
        else
            clear = bFillBackground;

        if (clear)
            OSClearVirtualScreen (FillBackgroundRGB);
    /* */

        DrawObjects (-0x80000000, 0);
        iNextLayer = 0x7FFFFFFF;

        i = 1;
        while (glmip)
        {
            DrawLayer (glmip->iMapID);
            iNextLayer = 0x7FFFFFFF;
            DrawObjects (i, i);
            i++;
            glmip = glmip->glmipNext;
        }
        i = iNextLayer;
        while (i < 0x7FFFFFFF)
        {
            iNextLayer = 0x7FFFFFFF;
            DrawObjects (i, i);
            i = iNextLayer;
        }
        DrawObjects (0x7FFFFFFF, 0x7FFFFFFF);  /* InFront */

        /* now ask for the statistics */
        SendMessage0ToClean (CcWmSTATISTICS);

        resultcode = GR_OK;
    }

    if (fade != 0)  /* fade in */
    {
        int i;
       /*
        int flags = DDBLT_WAIT + DDBLT_DDFX;
        DDBLTFX ddbltfx;
        memset (&ddbltfx, 0, sizeof (ddbltfx));
        ddbltfx.dwSize = sizeof (ddbltfx);
        ddbltfx.dwDDFX = DDBLTFX_NOTEARING;
       */

        if (fade == 1)
            for (i = 0; i < ScreenHeight / 2 - 1; i++)
            {
                RECT r = {0, 0, ScreenWidth, ScreenHeight};

                RECT r1 = {0, 0, ScreenWidth, i};
                RECT r2 = {0, 0, i, ScreenHeight};
                RECT r3 = {ScreenWidth - i, 0, ScreenWidth, ScreenHeight};
                RECT r4 = {0, ScreenHeight - i, ScreenWidth, ScreenHeight};

                OSFillBlack (FALSE, r1);
                OSFillBlack (FALSE, r2);
                OSFillBlack (FALSE, r3);
                OSFillBlack (FALSE, r4);

                OSBlit (&r);
              /*
                ddrval = IDirectDrawSurface_Blt
                     (lpDDSFront, &dst, lpDDSBack, &src, flags, &ddbltfx);
                if (ddrval == DDERR_SURFACELOST)
                    DDRestoreAll ();
              */
            }

        OSClearScreen ();

        if (fade == -1)
            for (i = ScreenHeight / 2 - 1; i > 0; i--)
            {
                RECT r = {i, i, ScreenWidth - i, ScreenHeight - i};

                RECT r1 = {0, 0, ScreenWidth, i};
                RECT r2 = {0, 0, i, ScreenHeight};
                RECT r3 = {ScreenWidth - i, 0, ScreenWidth, ScreenHeight};
                RECT r4 = {0, ScreenHeight - i, ScreenWidth, ScreenHeight};

                OSFillBlack (TRUE, r1);
                OSFillBlack (TRUE, r2);
                OSFillBlack (TRUE, r3);
                OSFillBlack (TRUE, r4);

                OSBlit (&r);
            /*
                ddrval = IDirectDrawSurface_Blt
                     (lpDDSFront, &dst, lpDDSBack, &src, flags, &ddbltfx);
                if (ddrval == DDERR_SURFACELOST)
                    DDRestoreAll ();
            */
            }
    }

    if (fade == 0)
    {
        if (FullScreen)
            OSFlip ();
        else
        {
            /* running in a window */
            RECT r = {0, 0, ScreenWidth, ScreenHeight};
            OSBlit (&r);
         /*
            int flags = DDBLTFAST_WAIT;
            RECT src = {0, 0, ScreenWidth, ScreenHeight};
        / /
            RECT dst = {20, 20, ScreenWidth - 40, ScreenHeight - 40};
            DDBLTFX ddbltfx;

            memset (&ddbltfx, 0, sizeof (ddbltfx));
            ddbltfx.dwSize = sizeof (ddbltfx);
            ddbltfx.dwAlphaDestConst = 0x001020F0;
            ddbltfx.dwAlphaEdgeBlend = 0x10203040;
            ddbltfx.dwFillDepth = 0x10709040;

            flags = DDBLT_ALPHADESTCONSTOVERRIDE
                  | DDBLT_ALPHAEDGEBLEND
                  | DDBLT_ALPHASRC
                  | DDBLT_DEPTHFILL
                  | DDBLT_WAIT;
            ddrval = IDirectDrawSurface_Blt
                 (lpDDSFront, &dst, lpDDSBack, &src, flags, &ddbltfx);
          / /
            ddrval = IDirectDrawSurface_BltFast (lpDDSFront, 0, 0, lpDDSBack, &src, flags);

            if (ddrval == DDERR_SURFACELOST)
                DDRestoreAll ();
        */
        }
    }

    return resultcode;
}   /* NextFrame */


/* make the screen follow an object */
void WinSetObjectFocus (int x1, int y1, int x2, int y2, int maxxv, int maxyv, OS ios, int *result, OS *oos)
{
    OBJECTREC *obj = GetObjectRec (iCCObjectID);
    int resultcode = GR_NOT_FOUND;

    if (obj)
    {
        iFollowID = iCCObjectID;

        iFollowX1 = x1;
        iFollowY1 = y1;
        iFollowX2 = x2;
        iFollowY2 = y2;

        iMaxXScrollSpeed = maxxv;
        iMaxYScrollSpeed = maxyv;

        iActualXView = XView;
        iActualYView = YView;

        XScrollSpeed = 0;
        YScrollSpeed = 0;

        resultcode = GR_OK;
    }

    *result = resultcode;
    *oos = ios;
}   /* WinSetObjectFocus */


/* create a new game object during the game */
void CreateGameObject (int mapval, int above, int x, int y,
                       int *result)
{
    OBJECTREC *objNew;
    OBJECTREC *obj1;
    OBJECTREC *obj2;
    int newid;


    // create a unique id number
    newid = 1;
    while (GetObjectRec (newid))
        newid++;

    objNew = rmalloc (sizeof (OBJECTREC));
    objNew->iMapX = -1;
    objNew->iMapY = -1;
    objNew->iObjType = mapval;
    objNew->iSubType = above;
    objNew->iXPos = x;
    objNew->iYPos = y;
    objNew->iObjectID = newid;
    objNew->bActive = FALSE;
    objNew->objNext = NULL;

    obj1 = objObjects;
    obj2 = NULL;

    while (obj1)
    {
        obj2 = obj1;
        obj1 = obj1->objNext;
    }

    if (obj2)
        obj2->objNext = objNew;
    else
        objObjects = objNew;

    // object remains inactive until moveobject() is run

    *result = GR_OK;
}   /* CreateGameObject */


void WinSetObjectRec (int id, int objtype, int subtype, BOOL active,
                      int xpos, int ypos, int xsize, int ysize,
                      int xoffset, int yoffset, int spriteid,
                      int displayoptions, int ownbounds, int bouncebounds,
                      int collidebounds, int forgetx, int forgety, int time,
                      int layer, int xacc, int yacc, int xv, int yv,
                      int xbnc, int ybnc, int maxxv, int maxyv,
                      int xslowdown, int yslowdown, int skipmove, int options,
                      OS ios, int *result, OS *oos
                     )
{
    OBJECTREC *obj = GetObjectRec (id);
    int resultcode = GR_NOT_FOUND;

    if (obj)
    {
        obj->iObjType = objtype;
        obj->iSubType = subtype;
        obj->bActive = active;
        obj->iXPos = xpos;
        obj->iYPos = ypos;
        obj->iXSize = xsize;
        obj->iYSize = ysize;
        obj->iXOffset = xoffset;
        obj->iYOffset = yoffset;
        if (spriteid != obj->iLastSprite)
        {
            obj->iPosition = 0;
            obj->iCounter = 0;
            obj->iLastSprite = spriteid;
        }
        obj->iSpriteID = spriteid & 0xFFFF;
        obj->iSpriteIndex = spriteid >> 16;
        obj->iDisplayOptions = displayoptions;
        obj->iOwnBounds = ownbounds;
        obj->iBounceBounds = bouncebounds;
        obj->iCollideBounds = collidebounds;
        obj->iForgetX = forgetx;
        obj->iForgetY = forgety;
        obj->iTimeCounter = time;
        obj->iLayer = layer;
        obj->iXAcc = xacc;
        obj->iYAcc = yacc;
        obj->iXSpeed = xv;
        obj->iYSpeed = yv;
        obj->iXBounce = xbnc;
        obj->iYBounce = ybnc;
        obj->iMaxXSpeed = maxxv;
        obj->iMaxYSpeed = maxyv;
        obj->iXSlowDown = xslowdown;
        obj->iYSlowDown = yslowdown;
        obj->iSkipMove = skipmove;
        obj->iOptions = options;

        resultcode = GR_OK;
    }

    *result = resultcode;
    *oos = ios;
}   /* WinSetObjectRec */


void WinGetObjectRec (int id, OS ios, int *objtype, int *subtype, BOOL *active,
                      int *xpos, int *ypos, int *xsize, int *ysize,
                      int *xoffset, int *yoffset, int *spriteid,
                      int *displayoptions, int *ownbounds, int *bouncebounds,
                      int *collidebounds, int *forgetx, int *forgety, int *time,
                      int *layer, int *xacc, int *yacc, int *xv, int *yv,
                      int *xbnc, int *ybnc, int *maxxv, int *maxyv,
                      int *xslowdown, int *yslowdown, int *skipmove, int *options,
                      int *result, OS *oos
                     )
{
    OBJECTREC *obj = GetObjectRec (id);
    int resultcode = GR_NOT_FOUND;

    if (obj)
    {
        *objtype = obj->iObjType;
        *subtype = obj->iSubType;
        *active = obj->bActive;
        *xpos = obj->iXPos;
        *ypos = obj->iYPos;
        *xsize = obj->iXSize;
        *ysize = obj->iYSize;
        *xoffset = obj->iXOffset;
        *yoffset = obj->iYOffset;
        *spriteid = (obj->iSpriteID & 0xFFFF) | (obj->iSpriteIndex << 16);
        *displayoptions = obj->iDisplayOptions;
        *ownbounds = obj->iOwnBounds;
        *bouncebounds = obj->iBounceBounds;
        *collidebounds = obj->iCollideBounds;
        *forgetx = obj->iForgetX;
        *forgety = obj->iForgetY;
        *time = obj->iTimeCounter;
        *layer = obj->iLayer;
        *xacc = obj->iXAcc;
        *yacc = obj->iYAcc;
        *xv = obj->iXSpeed;
        *yv = obj->iYSpeed;
        *xbnc = obj->iXBounce;
        *ybnc = obj->iYBounce;
        *maxxv = obj->iMaxXSpeed;
        *maxyv = obj->iMaxYSpeed;
        *xslowdown = obj->iXSlowDown;
        *yslowdown = obj->iYSlowDown;
        *skipmove = obj->iSkipMove;
        *options = obj->iOptions;

        resultcode = GR_OK;
    }
    *result = resultcode;
    *oos = ios;
}   /* WinGetObjectRec */


void WinShowStatistic (int x, int y, CLEAN_STRING format, int value,
                       int r, int g, int b,
                       CLEAN_STRING font, int fontsize, BOOL bold, BOOL italic,
                       BOOL shadow, int shadowx, int shadowy,
                       int shadowr, int shadowg, int shadowb,
                       int options,
                       OS ios, int *result, OS *oos
                      )
{
    HDC hdc;
    char Buffer[1024];
    RECT rect = {XShiftScreen, YShiftScreen,
                 ScreenWidth + XShiftScreen, ScreenHeight + YShiftScreen};

    if (OSGetGameWindowHDC (&hdc))
    {
        static HFONT hfont = NULL;
        SIZE size;

        if (hfont)
            DeleteObject (hfont);
        hfont = CreateFont (fontsize, 0, 0, 0,
                            bold? FW_BOLD : FW_NORMAL,
                            italic,
                            FALSE,
                            FALSE,
                            ANSI_CHARSET,
                            OUT_DEFAULT_PRECIS,
                            CLIP_DEFAULT_PRECIS,
                            DEFAULT_QUALITY,
                            VARIABLE_PITCH,
                            cstring (font));
        if (hfont)
        {
            SelectObject (hdc, hfont);

            if (value == NOTHING)
                wsprintf (Buffer, cstring (format));
            else
                wsprintf (Buffer, cstring (format), value);

            SetBkColor (hdc, 0);
            SetBkMode (hdc, TRANSPARENT);

            GetTextExtentPoint (hdc, Buffer, lstrlen (Buffer), &size);

            if (options & SO_X_FROM_SCREEN_CENTER)
                x += ScreenWidth / 2;
            if (options & SO_X_CENTERED)
                x -= size.cx / 2;
            if (Buffer[0] != ' ')
                if (x < 0)
                    x = ScreenWidth - size.cx + x;

            if (options & SO_Y_FROM_SCREEN_CENTER)
                y += ScreenHeight / 2;
            if (options & SO_Y_CENTERED)
                y -= size.cy / 2;
            if (Buffer[0] != ' ')
                if (y < 0)
                    y = ScreenHeight - size.cy + y;

            x += XShiftScreen;
            y += YShiftScreen;

            if (shadow)
            {
                SetTextColor (hdc, RGB (shadowr, shadowg, shadowb));
                ExtTextOut (hdc, x + shadowx, y + shadowy, ETO_CLIPPED,
                            &rect, Buffer, lstrlen (Buffer), NULL);
            }

            SetTextColor (hdc, RGB (r, g, b));
            ExtTextOut (hdc, x, y, ETO_CLIPPED, &rect,
                        Buffer, lstrlen (Buffer), NULL);

        }
        OSReleaseGameWindowHandle (hdc);
    }


    *result = GR_OK;
    *oos = ios;
}   /* WinShowStatistic */

void WinPlayMusic (CLEAN_STRING midifile, BOOL restart, OS ios, int *result, OS *oos)
{
    int resultcode = GR_OS_ERROR;

    if (OSPlayMusic (cstring (midifile), restart))
        resultcode = GR_OK;

    *result = resultcode;
    *oos = ios;
}   /* WinPlayMusic */

void WinStopMusic (OS ios, int *result, OS *oos)
{
    *result = OSStopMusic ();
    *oos = ios;
}   /* WinStopMusic */

/* set extra level options */
void WinGameLevelOptions (int r, int g, int b,
                          BOOL esc, BOOL debug,
                          BOOL fadein, BOOL fadeout,
                          OS ios, int *result, OS *oos
                         )
{
    bCheckEscape = esc;
    DebugScroll = debug;
    bFillBackground = FALSE;
    if (r * g * b >= 0)
    {
        bFillBackground = TRUE;
        FillBackgroundRGB = RGB (r, g, b);
    }
    bFadeIn = fadein;
    bFadeOut = fadeout;
    *result = GR_OK;
    *oos = ios;
}   /* WinGameLevelOptions */

/* init a sound sample or clear all samples if id = -1 */
void WinInitSoundSample (int id, CLEAN_STRING name, int buffers, OS ios, int *result, OS *oos)
{
    *result = GR_OK;
    if (id == -1)
    {
        OSFreeSoundSamples ();
    }
    else
    {
        if (!OSInitSoundSample (id, cstring (name), buffers))
            *result = GR_OS_ERROR;
    }
    *oos = ios;
}   /* WinGameLevelOptions */


void WinGetBoundMap (int x, int y, OS ios, int *mapvalue, int *result, OS *oos)
{
    int mappos;

    *result = GR_FAILED;
    *oos = ios;

    *mapvalue = -1;

    if (!(sBoundMap))
        return;
    if ((x < 0) || (y < 0))
        return;
    if ((x > BoundMapWidth - 1) || (y > BoundMapHeight - 1))
        return;

    mappos = (y * BoundMapWidth + x) * sizeof (int);
    *mapvalue = (*(int *) &sBoundMap[mappos]);
}   /* WinGetBoundMap */

void WinSetBoundMap (int x, int y, int newvalue, OS ios, int *result, OS *oos)
{
    int mappos;

    *result = GR_FAILED;
    *oos = ios;

    if (!(sBoundMap))
        return;
    if ((x < 0) || (y < 0))
        return;
    if ((x > BoundMapWidth - 1) || (y > BoundMapHeight - 1))
        return;

    mappos = (y * BoundMapWidth + x) * sizeof (int);
    (*(int *) &sBoundMap[mappos]) = newvalue;
}   /* WinSetBoundMap */

/*
void WinPlaySoundSample (int id, int volume, int pan, int freq, int delay, OS ios, int *result, OS *oos)
{

    if (delay == 0)
    {
        SOUNDSAMPLEINFO *ssi = GetSoundSampleInfo (id);

        if (ssi)
        {
            IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer (ssi->hsoSoundBuffer);

            if (bSoundEnabled)
            {
                if (pDSB)
                {
                    IDirectSoundBuffer_SetVolume (pDSB, (LONG) volume);
                    IDirectSoundBuffer_SetPan (pDSB, (LONG) pan);
                    IDirectSoundBuffer_SetFrequency (pDSB, (LONG) freq);
                    IDirectSoundBuffer_Play (pDSB, 0, 0, 0);
                }
            }
        }
    }
    else
    {
        ScheduleEvent (EV_PLAY_SOUND, id, volume, pan, freq, 0, delay);
    }

    *result = GR_OK;
    *oos = ios;
}
*/
