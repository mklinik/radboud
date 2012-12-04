implementation module osgame

import	StdArray, StdBool, StdChar, StdInt
import	StdMaybe
from	ospicture	import toRGBtriple
from	clCCall_12	import winBeep
import	gameCrossCall_12, gamehandle, gameobjectutils, gst


::  OSGameData gs
    =   {   scroll    :: [(MAPID, Movement)]        // The layer movement functions
        ,   gamest    :: gs                         // The game state
        ,   gamehnd   :: GameHandle gs              // Complete game definition
        }

OSinitialiseGame :: !*OSToolbox -> *OSToolbox
OSinitialiseGame tb
	= WinInitialiseGame tb

OSBinaryIntStr :: !Int -> {#Char}
OSBinaryIntStr x = WinBinaryIntStr x

OSBinaryBoolStr :: !Bool -> {#Char}
OSBinaryBoolStr x = WinBinaryBoolStr x

OSIntListArrayToString :: ![{#Int}] -> {#Char}
OSIntListArrayToString irs
    = IntListArrayToString "" irs
where
    IntListArrayToString :: !{#Char} ![{#Int}] -> {#Char}
    IntListArrayToString cs [] = cs
    IntListArrayToString cs [ir:irs] = IntListArrayToString (cs+++intarrayToString ir) irs
    where
        intarrayToString :: !{#Int} -> {#Char}
        intarrayToString ir
            = encode 0 ir zeroChars
        where
            zeroChars   = createArray (4*(size ir)) zero
            encode :: !Int !{#Int} !*{#Char} -> *{#Char}
            encode i ir zeroChars
                | i<size ir
                    = encode (i+1) ir {zeroChars & [j]=toChar (maskshift 0 x),[j+1]=toChar (maskshift 8 x),[j+2]=toChar (maskshift 16 x),[j+3]=toChar (maskshift 24 x)}
                | otherwise
                    = zeroChars
            where
                x   = ir.[i]
                j   = 4*i
                maskshift :: !Int !Int -> Int
                maskshift nrbits x = (x>>nrbits) bitand 0xFF

OScreateGameWindow :: !Bool !(!Int,!Int) !Int !*OSToolbox -> (![DelayActivationInfo],!OSWindowPtr,!*OSToolbox)
OScreateGameWindow fullscreen size bpp tb
	= WinCreateGameWindow fullscreen size bpp tb

OSInitGameBitmap :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitGameBitmap id filename w h blockwidth blockheight tb
    = WinInitGameBitmap id filename w h blockwidth blockheight tb

OSGameBitmapDone :: !BID !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSGameBitmapDone id tb
    = WinGameBitmapDone id tb

OSClearAllGameBitmaps :: !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSClearAllGameBitmaps tb
    = WinClearAllGameBitmaps tb

OSSetTransparentColor :: !BID !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSSetTransparentColor id x y tb
    = WinSetTransparentColor id x y tb

OSInitBlockSequence :: !BID !SEQID !{#Char} !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitBlockSequence bid seqid seq tb
    = WinInitBlockSequence bid seqid seq tb

OSInitGameLayerMap :: !MAPID !BID !{#Char} !Int !Int !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitGameLayerMap mapid bid map w h tile tb
    = WinInitGameLayerMap mapid bid map w h tile tb

OSGameLayerMapDone :: !MAPID !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSGameLayerMapDone mapid tb
    = WinGameLayerMapDone mapid tb

OSRunGameEngine :: !(OSGameData .gs) !*OSToolbox -> (.gs,!*OSToolbox)
OSRunGameEngine gd tb
    # (gd,tb)   = WinRunGameEngine handleGameEvents gd 0 0 0 tb
    = (gd.gamest,tb)

OSSetGameBoundMap :: !Int !Int !{#Char} !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSSetGameBoundMap w h map mw mh objstart startobjx startobjy tb
    = WinSetGameBoundMap w h map mw mh objstart startobjx startobjy tb

OSMoveScreenTo :: !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSMoveScreenTo x y tb
    = WinMoveScreenTo x y tb

OSInitSpriteAnimation :: !BID !{#Char} !Bool !*OSToolbox -> (!GRESULT,!*OSToolbox)
OSInitSpriteAnimation bid seq loop tb
    = WinInitSpriteAnimation bid seq loop tb

OSInitGameObject :: !ObjectCode !SubCode !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSInitGameObject ot st p tb
    = WinInitGameObject ot st p.x p.y tb

OSSetObjectFocus :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSSetObjectFocus x1 y1 x2 y2 maxxv maxyv tb
    = WinSetObjectFocus x1 y1 x2 y2 maxxv maxyv tb

// modified 01/11/99
OSCreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSCreateUserEvent ev evpar1 evpar2 dest subdest time tb
    = WinCreateUserEvent ev evpar1 evpar2 dest subdest time tb

OSShowStatistic :: !Int !Int !{#Char} !Int !Colour !{#Char} !Int !Bool !Bool  !Bool !Int !Int !Colour !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSShowStatistic x y format value color font size bold italic shadow sx sy scolor options tb
    = WinShowStatistic x y format value colorRGB font size bold italic shadow sx sy scolorRGB options tb
where
    colorRGB  = toRGBtriple color
    scolorRGB = toRGBtriple scolor

OSPlayMusic :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSPlayMusic midifile restart tb
    = WinPlayMusic midifile restart tb

OSStopMusic :: !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSStopMusic tb
    = WinStopMusic tb


OSGameLevelOptions :: !Bool !Colour !Bool !Bool !Bool !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSGameLevelOptions fill rgb esc debug fadein fadeout tb
    = WinGameLevelOptions (if fill (toRGBtriple rgb) (~1,~1,~1)) esc debug fadein fadeout tb

OSInitSoundSample :: !Int !{#Char} !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSInitSoundSample id name buffers tb
    = WinInitSoundSample id name buffers tb

OSPlaySoundSample :: !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSPlaySoundSample id vol pan freq delay tb
    = WinPlaySoundSample id vol pan freq delay tb

OSGetBoundMap :: !Int !Int !*OSToolbox -> (!(!Int, !GRESULT), !*OSToolbox)
OSGetBoundMap x y tb
    # (value, gresult, tb) = WinGetBoundMap x y tb
    = ((value, gresult), tb)

OSSetBoundMap :: !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
OSSetBoundMap x y newvalue tb
    = WinSetBoundMap x y newvalue tb

handleGameEvents :: !CrossCallInfo !v:(OSGameData u:gs) !*OSToolbox -> (!CrossCallInfo,!v:OSGameData u:gs,!*OSToolbox), [v<=u]
handleGameEvents fromOSCci=:{ccMsg=CcWmGAMEKEYBOARD,p1=key,p2=x,p3=y} state tb
    = (return2Cci x` y`,state,tb)
where
    (x`,y`) = case key of
                GK_LEFT     -> (x-1, y)
                GK_RIGHT    -> (x+1, y)
                GK_UP       -> (x, y-1)
                GK_DOWN     -> (x, y+1)
                otherwise   -> (x, y)

handleGameEvents fromOSCci=:{ccMsg=CcWmCHECKQUIT} state=:{gamest,gamehnd={quitlevel`=quitfunction}} tb
    #   gst                 =   toGSt gamest tb
    #   (quit, gst)         =   quitfunction gst
    #   (newstate, tb)      =   fromGSt gst
    =   (return1Cci (toInt quit), {state & gamest = newstate}, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmSCROLL,p1=id,p2=x,p3=y,p4=t} state=:{scroll} tb
    = (return2Cci x` y`, state, tb)
where
    {x = x`, y = y`}    =   f (MakePoint x y) t
    f                   =   FindMovement (id, scroll)
	
	FindMovement :: !(!MAPID, ![(MAPID, Movement)]) -> Movement
	FindMovement (mapid, [(id, mv): rest])
	    | id == mapid   =   mv
	    | otherwise     =   FindMovement (mapid, rest)

handleGameEvents fromOSCci=:{ccMsg=CcWmINITOBJECT, p1=objtype, p2=subtype, p3=id, p4=x, p5=y, p6=time} state tb
    # (state,tb)        =   initialiseGameObject objtype subtype id {x=x,y=y} time state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTDONE, p1=objtype, p2=id} state tb
    # (state, tb) = doneGameObject objtype id state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmMOVEOBJECT, p1=objtype, p2=id} state tb
    # (state,tb)        =   moveGameObject objtype id state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmTOUCHBOUND, p1=objtype, p2=id, p3=dir, p4=mapcode} state tb
    # (state,tb)        =   touchBound objtype id dir mapcode state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmCOLLISION, p1=ot1, p2=id1, p3=ot2, p4=id2, p5=dir} state tb
    # (state,tb)        =   handleCollision ot1 id1 ot2 id2 dir state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmANIMATION, p1=objtype, p2=id} state tb
    # (state,tb)        =   handleAnimationEvent objtype id state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmUSEREVENT, p1=objtype, p2=id, p3=ev, p4=par1, p5=par2} state tb
    # (state,tb)        =   handleUserEvent objtype id ev par1 par2 state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTTIMER, p1=objtype, p2=id} state tb
    # (state,tb)        =   handleTimerEvent objtype id state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTKEYDOWN, p1=objtype, p2=id, p3=key} state tb
    # (state,tb)        =   handleKeyDown objtype id key state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmOBJECTKEYUP, p1=objtype, p2=id, p3=key} state tb
    # (state,tb)        =   handleKeyUp objtype id key state tb
    = (return0Cci, state, tb)

handleGameEvents fromOSCci=:{ccMsg=CcWmSTATISTICS} state=:{gamest,gamehnd={textitems`=stats}} tb
    #   gst             =   toGSt gamest tb
    #   (statlist, gst) =   stats gst
    #   (newstate, tb)  =   fromGSt gst
    #   tb              =   showall statlist tb
    = (return0Cci, {state & gamest = newstate}, tb)
where
    showall :: ![GameText] !*OSToolbox -> *OSToolbox
    showall [] tb = tb
    showall [x:xs] tb
        # tb = showstat x tb
        = showall xs tb
    where
        showstat :: !GameText !*OSToolbox -> *OSToolbox
        showstat s tb
            | isJust s.shadow
                # (_,tb) = OSShowStatistic s.position.x s.position.y s.format (mi s.value)
                            s.color s.style.fontname s.style.fontsize s.style.bold
                            s.style.italic True sh.shadowpos.x sh.shadowpos.y
                            sh.shadowcolor (alint s.alignment) tb
                = tb
            # (_,tb) = OSShowStatistic s.position.x s.position.y s.format (mi s.value)
                            s.color s.style.fontname s.style.fontsize s.style.bold
                            s.style.italic False 0 0 Black (alint s.alignment) tb
            = tb
        where
            mi :: (Maybe Int) -> Int
            mi Nothing  = NOTHING
            mi (Just x) = x
            sh = fromJust s.shadow

            alint :: TextAlignment -> Int
            alint al = CompressBools (False, False, False, False, sy, sx, cy, cx)
              where
                (cx,cy) = al.xycentered
                (sx,sy) = al.xyfromscreencenter

handleGameEvents fromOSCci state tb
    = (return0Cci,state,winBeep tb)


MakePoint :: !Int !Int -> Point2
MakePoint a b = {x = a, y = b}


handleUserEvent :: !Int !InstanceID !Int !Int !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:(OSGameData u:gs), !*OSToolbox), [v<=u]
handleUserEvent objtype id ev par1 par2 data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doUserEvent id obj objrec ev par1 par2 gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj gamehnd,gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doUserEvent :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !Int !Int !Int !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doUserEvent id (GameObjectHandleLS obj=:{instances`,userevent`}) objrec ev par1 par2 gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   userevent` ev par1 par2 {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



handleAnimationEvent :: !Int !InstanceID !v:(OSGameData u:gs) !*OSToolbox -> (!v:(OSGameData u:gs), !*OSToolbox), [v<=u]
handleAnimationEvent objtype id data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doAnimation id obj objrec gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd, gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doAnimation :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doAnimation id (GameObjectHandleLS obj=:{instances`,animation`}) objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   animation` {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



handleTimerEvent :: !Int !InstanceID !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox), [v<=u]
handleTimerEvent objtype id data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doTimer id obj objrec gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd, gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doTimer :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doTimer id (GameObjectHandleLS obj=:{instances`,frametimer`}) objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   frametimer` {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)



moveGameObject :: !Int !InstanceID !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox)
moveGameObject objtype id data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doMove id obj objrec gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj gamehnd,gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doMove :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doMove id (GameObjectHandleLS obj=:{instances`,move`}) objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   move` {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)


touchBound :: !Int !InstanceID !Int !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox)
touchBound objtype id dir mapcode data=:{gamest,gamehnd} tb
    #   directions              =   makeDirectionSet dir
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doTouchBound id obj objrec directions mapcode gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd,gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doTouchBound :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !DirectionSet !Int !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doTouchBound id (GameObjectHandleLS obj=:{instances`,touchbound`}) objrec bounds mapcode gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   touchbound` bounds mapcode {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)



initialiseGameObject :: !Int !Int !InstanceID !Point2 !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox)
initialiseGameObject objtype subtype id p time data=:{scroll,gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doInit id subtype p time obj gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj gamehnd, gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doInit :: !InstanceID !Int !Point2 !GameTime !(GameObjectHandleLS .gs) !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doInit id subtype p time (GameObjectHandleLS obj=:{instances`,init`}) gst
    //  #   ((state,objrec), gst)       =   init` subtype p time gst
        #   {st=state,or=objrec,gs=gst}
                                        =   init` subtype p time gst
        #   newinstances                =   [(id,state):instances`]
        =   (GameObjectHandleLS {obj & instances`=newinstances},objrec,gst)


doneGameObject :: !Int !InstanceID !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox)
doneGameObject objtype id data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,gst)           =   doDone id obj objrec gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj gamehnd, gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doDone :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !.gs -> (!GameObjectHandleLS .gs,.gs)
    doDone id (GameObjectHandleLS obj=:{instances`,done`}) objrec gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj, gst)
        #   state               =   fromJust maybestate
        #   gst                 =   done` {st=state,or=objrec,gs=gst}
        #   newinstances        =   removeinstance id instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj, gst)
//      =   ({obj & instances`=newinstances}, gst)



handleCollision :: !Int !InstanceID !Int !InstanceID !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox), [v<=u]
handleCollision ot1 id1 ot2 id2 dir data=:{gamest,gamehnd} tb
    #   directions              =   makeDirectionSet dir
    #   maybefound              =   getobject ot1 gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec2, tb) =   GetObjectRec id2 tb
        #   (_, _, objrec1, tb) =   GetObjectRec id1 tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec1,gst)   =   doCollision id1 obj objrec1 directions ot2 objrec2 gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd, gamest=newstate}
        #   (_, tb)             =   SetObjectRec id1 ot1 objrec1 (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doCollision :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !DirectionSet !Int !GameObjectRec !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doCollision id (GameObjectHandleLS obj=:{instances`,collide`}) objrec1 bounds ot2 objrec2 gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec1,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec1,gs=gst}
                                =   collide` bounds ot2 objrec2 {st=state,or=objrec1,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec1,gst)
//      =   ({obj & instances`=newinstances},objrec1,gst)



handleKeyDown :: !Int !InstanceID !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox), [v<=u]
handleKeyDown objtype id key data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doKeyDown id obj objrec key gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj gamehnd,gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doKeyDown :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !Int !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doKeyDown id (GameObjectHandleLS obj=:{instances`,keydown`}) objrec key gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   keydown` key {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)


handleKeyUp :: !Int !InstanceID !Int !v:(OSGameData u:gs) !*OSToolbox -> (!v:OSGameData u:gs, !*OSToolbox), [v<=u]
handleKeyUp objtype id key data=:{gamest,gamehnd} tb
    #   maybefound              =   getobject objtype gamehnd
    |   isJust maybefound
        #   obj                 =   fromJust maybefound
        #   (_, _, objrec, tb)  =   GetObjectRec id tb
        #   gst                 =   toGSt gamest tb
        #   (obj,objrec,gst)    =   doKeyUp id obj objrec key gst
        #   (newstate, tb)      =   fromGSt gst
        #   data                =   {data & gamehnd = putobject obj data.gamehnd,gamest=newstate}
        #   (_, tb)             =   SetObjectRec id objtype objrec (getGameObjectHandleLS_spriteids` obj) tb
        =   (data, tb)
    |   otherwise
        =   (data, tb)
where
    doKeyUp :: !InstanceID !(GameObjectHandleLS .gs) !GameObjectRec !Int !.gs -> (!GameObjectHandleLS .gs,!GameObjectRec,.gs)
    doKeyUp id (GameObjectHandleLS obj=:{instances`,keyup`}) objrec key gst
        #   maybestate          =   findinstance instances` id
        |   isNothing maybestate
            =   (GameObjectHandleLS obj,objrec,gst)
        #   state               =   fromJust maybestate
        #   {st=s,or=objrec,gs=gst}
                                =   keyup` key {st=state,or=objrec,gs=gst}
        #   newinstances        =   updateinstance id s instances`
        #   obj                 =   {obj & instances`=newinstances}
        =   (GameObjectHandleLS obj,objrec,gst)
//      =   ({obj & instances`=newinstances},objrec,gst)
