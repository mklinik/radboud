implementation module StdGame

import	StdArray, StdBool, StdClass, StdFunc, StdInt, StdList, StdMisc
import	StdId, StdProcess
import	fixed, GameFunctions, gamehandle, gameutils, gst
from	gameobjectutils	import toBoundMapCode, fromBoundMapCode
from	StdPSt			import appPIO, accPIO
import	commondef, iostate, windowdevice
from	windowaccess	import initWindowHandle, checkZeroWindowHandlesBound, decreaseWindowHandlesBound, addWindowHandlesWindow
from	windowcreate	import bufferDelayedEvents
from	windowvalidate	import validateWindowId
from	ostypes			import OSNoWindowPtr

/* predefined bounds */
BND_MAP_CODES      :== 1 << 30
BND_STATIC_BOUNDS  :== 1 << 31

/* skipmove constant */
SK_FOREVER         :== -1


StdGameFatalError :: String String -> .x
StdGameFatalError function error = fatalError function "StdGame" error


startGame :: .(Game a) a [.GameAttribute a] !*World -> .World
startGame gamedef initialstate options world
    = startIO SDI 0 init [ProcessClose closeProcess] world
where
    init ps
        #   (finalstate, _, ps) = openGame initialstate gamedef options ps
        =   closeProcess ps


openGame :: .gs !(Game .gs) ![GameAttribute .gs] !(PSt .l) -> (.gs, !ErrorReport, !PSt .l)
openGame gs gdef attr ps
    #   (wId, ps)       = accPIO openId ps
    #   size            = findSize attr {w=320,h=240}
    #   bpp             = findBPP attr 8
    #   (_, ps)         = openGameWindow wId size bpp True ps
    #   (tb,ps)         = accPIO getIOToolbox ps
    #   gst             = toGSt gs tb
    #   (initLevel,gst) = gdef.nextlevel gst
    #   (gs,tb)         = fromGSt gst
    #   (gs, _, tb)     = PlayLevels initLevel gs gdef tb
    #   ps              = appPIO (setIOToolbox tb) ps
    =   (gs, NoError, ps)
where
    findSize :: ![GameAttribute .gs] !Size -> Size
    findSize [] s = s
    findSize [ScreenSize x:xs] s = x
    findSize [x:xs] s = findSize xs s
    findBPP :: ![GameAttribute .gs] !Int -> Int
    findBPP [] s = s
    findBPP [ColorDepth x:xs] s = x
    findBPP [x:xs] s = findBPP xs s

    // always full screen, game in a window not implemented yet
	openGameWindow :: !Id !Size !Int !Bool !(PSt .l) -> (!ErrorReport, !PSt .l)
	openGameWindow id gamewindowsize bitsperpixel fullscreen pState
		# pState					= windowFunctions.dOpen pState	// Install the window device
		# maybe_id					= Just id
		# (maybe_okId,ioState)		= validateWindowId maybe_id pState.io
		| isNothing maybe_okId
			= (ErrorIdsInUse,{pState & io=ioState})
		# (found,wDevice,ioState)	= ioStGetDevice WindowDevice ioState
		| not found					// This condition should never occur: WindowDevice must have been 'installed'
			= StdGameFatalError "openGame" "could not retrieve WindowSystemState from IOSt"
		# windows					= windowSystemStateGetWindowHandles wDevice
		# (isZero,windows)			= checkZeroWindowHandlesBound windows
		| isZero
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			= (ErrorViolateDI,{pState & io=ioState})
		| otherwise
			# info					= {	gamewindowDDPtr      = OSNoWindowPtr
			  						  ,	gamewindowCDepth     = bitsperpixel
			  						  ,	gamewindowSize       = gamewindowsize
			  						  ,	gamewindowFullScreen = fullscreen
			  						  }
			  okId					= fromJust maybe_okId
			# wH					= initWindowHandle "" Modeless IsGameWindow (GameWindowInfo info) [] [WindowId okId]
			# wH					= {wH & whSize=gamewindowsize}
			# (tb,ioState)			= getIOToolbox ioState
			# tb					= OSinitialiseGame tb
			# (delayinfo,wPtr,tb)	= OScreateGameWindow fullscreen (toTuple gamewindowsize) bitsperpixel tb
			# ioState				= setIOToolbox tb ioState
			  wlsH					= {wlsState=undef,wlsHandle=wH}
			  wIds					= {wId=okId,wPtr=wPtr,wActive=False}
			  wsH					= {wshIds=wIds,wshHandle=Just wlsH}
			  windows				= addWindowHandlesWindow 0 wsH windows
			  windows				= decreaseWindowHandlesBound windows
			# ioState				= ioStSetDevice (WindowSystemState windows) ioState
			# ioState				= bufferDelayedEvents delayinfo ioState
			= (NoError,{pState & io=ioState})


PlayLevels :: !Int .gs !(Game .gs) !*OSToolbox -> (.gs, !ErrorReport, !*OSToolbox)
PlayLevels level gs gdef tb
    |   level == 0
    =   (gs, NoError, tb)
    #   ghnd             =  createGameHandle gdef
    #   (_, gs, tb)      =  PlayLevel level gs ghnd tb
    #   gst              =  toGSt gs tb
    #   (nextlevel, gst) =  gdef.nextlevel gst
    #   (gs,tb)          =  fromGSt gst
    =   PlayLevels nextlevel gs gdef tb

FindMaxID :: a ![a] -> a  | < a
FindMaxID x [] = x
FindMaxID x [y:ys]
    |   y > x       =   FindMaxID y ys
    |   otherwise   =   FindMaxID x ys

InitLayers :: ![Layer] ![BID] ![MAPID] !*OSToolbox -> (![BID], ![MAPID], !*OSToolbox)
InitLayers [] bids mapids tb    =   (bids, mapids, tb)
InitLayers [l:ls] bids mapids tb
    #   (bids, mapids, tb)      =   InitLayer l bids mapids tb
    =   InitLayers ls bids mapids tb


MaybeSetTransparentColor :: !BID !(Maybe Point2) !*OSToolbox -> (!GRESULT, !*OSToolbox)
MaybeSetTransparentColor _ Nothing tb = (GR_OK, tb)
MaybeSetTransparentColor bid (Just p) tb = SetTransparentColor bid p tb

MovementFunctions :: ![Layer] -> [Movement]
MovementFunctions [] = []
MovementFunctions [l:ls] = [l.movement] ++ MovementFunctions ls

InitLayer :: !Layer ![BID] ![MAPID] !*OSToolbox -> (![BID], ![MAPID], !*OSToolbox)
InitLayer l bids mapids tb
    #   (newbid, tb)    =   InitGameBitmap 0 b.bitmapname (us.w * nh) (us.h * nv) us.w us.h tb  // newbid
    #   (_, tb)         =   MaybeSetTransparentColor newbid b.transparent tb
    #   tb              =   InitBlockSequences newbid l.sequences tb
    #   (_, tb)         =   InitGameLayerMap newmapid newbid l.layermap True tb     // l.tile
    =   (bids++[newbid], mapids++[newmapid], tb)
where
    b           =   l.bmp
    us          =   b.unitsize
    (nh, nv)    =   b.dimensions
    newmapid    =   (FindMaxID 0 mapids) + 1

InitBlockSequences :: !BID ![TileSequence] !*OSToolbox -> *OSToolbox
InitBlockSequences bid [] tb = tb
InitBlockSequences bid [s:ss] tb
    #   (_, tb) =   InitBlockSequence bid s tb
    =   InitBlockSequences bid ss tb

LayersDone :: ![BID] ![MAPID] !*OSToolbox -> *OSToolbox
LayersDone bids mapids tb
    #   tb      =   MapsDone mapids tb
    #   tb      =   BitmapsDone bids tb
    =   tb

MapsDone :: ![MAPID] !*OSToolbox -> *OSToolbox
MapsDone [] tb = tb
MapsDone [m:ms] tb
    #   (_, tb)     =   GameLayerMapDone m tb
    =   MapsDone ms tb

BitmapsDone :: ![BID] !*OSToolbox -> *OSToolbox
BitmapsDone [] tb = tb
BitmapsDone [b:bs] tb
    #   (_, tb)     =   GameBitmapDone b tb
    =   BitmapsDone bs tb

PlayLevel :: !Int .gs !(GameHandle .gs) !*OSToolbox -> (!ErrorReport,.gs,!*OSToolbox)
PlayLevel levelnumber gs gamehnd tb
    #   (_, tb)             =   SetGameBoundMap wid ht bm os stx sty  tb
    #   (_, tb)             =   MoveScreenTo curLevelHnd.initpos` tb
    #   lyrs                =   curLevelHnd.layers`
    #   (bids, mapids, tb)  =   InitLayers lyrs [] [] tb
    #   movements           =   zip2 mapids (MovementFunctions lyrs)
    #   (_, tb)             =   initsoundsamples curLevelHnd.soundsamples` tb
    #   tb                  =   maybePlayMusic curLevelHnd.music` tb
    #   gst                 =   toGSt gs tb
    #   firstlevel          =   curLevelHnd
    #   (obj, gst)          =   map2 convertobjsprites firstlevel.objects` gst
    #   firstlevel          =   {firstlevel & objects` = obj}
    #   curgamehnd          =   {gamehnd & levels` = [firstlevel]}
    #   (gs,tb)             =   fromGSt gst
    #   (_, tb)             =   OSGameLevelOptions fill rgb esc dbg fdin fdout tb
    #   (gs, tb)            =   RunGameEngine { scroll  = movements
                                              , gamest  = gs
                                              , gamehnd = curgamehnd} tb
    #   tb                  =   maybeStopMusic curLevelHnd.music` tb
    #   (_, tb)             =   OSInitSoundSample (-1) "" 0 tb  // remove samples
    #   tb                  =   LayersDone bids mapids tb
    #   (_, tb)             =   ClearAllGameBitmaps tb
    =   (NoError, gs, tb)
where
    curLevelHnd         =   gamehnd.levels`!!(levelnumber-1)
    options             =   curLevelHnd.leveloptions`
    { map = bm,
      blocksize = bs,
      objstart = os,
      startobjx = stx,
      startobjy = sty}  =   curLevelHnd.boundmap`
    { w = wid, h = ht } =   bs
    maybePlayMusic :: !(Maybe Music) !*OSToolbox -> *OSToolbox
    maybePlayMusic Nothing tb = tb
    maybePlayMusic (Just m) tb
        #   (_, tb)     =   PlayMusic m.musicfile m.restart tb
        = tb
    maybeStopMusic :: !(Maybe Music) !*OSToolbox -> *OSToolbox
    maybeStopMusic Nothing tb = tb
    maybeStopMusic (Just m) tb
        |   m.continue
            = tb
        #   (_, tb)     =   StopMusic tb
        = tb
    esc = options.escquit
    dbg = options.debugscroll
    fdin = options.fadein
    fdout = options.fadeout
//  rgb :: Colour
    rgb = if fill
             (fromJust options.fillbackground)
             (RGB {r= -1, g= -1, b= -1})
    fill = isJust options.fillbackground

initsoundsamples sndlist gs
    = map2 initsoundsample sndlist gs

initsoundsample sample gs
    = OSInitSoundSample sample.soundid sample.soundfile sample.soundbuffers gs

convertobjsprites (GameObjectHandleLS obj) gst
    # (sprids, gst) = convertsprites obj.sprites` gst
    = (GameObjectHandleLS {obj & spriteids` = sprids}, gst)
where
	convertsprites :: ![Sprite] !(GSt .gs) -> (![SpriteID], !GSt .gs)
	convertsprites spr gst
	    # (idlst, gst) = map2 createAnimation spr gst
	    # idlst        = map (\x->0-x)/*(~)*/ idlst
	    = (idlst, gst)


createGameBitmap :: !GameBitmap !(GSt .gs) -> (!GRESULT, !GSt .gs)
createGameBitmap bitmap=:{bitmapname, unitsize, dimensions, transparent} gst
    #   (bid, gst) = accGStTb (InitGameBitmap 0 bitmapname (w * nh) (h * nv) w h) gst
    #   (_, gst)   = accGStTb (MaybeSetTransparentColor bid transparent) gst
    =   (bid, gst)
where
    w       = unitsize.w
    h       = unitsize.h
    (nh,nv) = dimensions


createAnimation :: !Sprite !(GSt .gs) -> (!GRESULT, !GSt .gs)
createAnimation sprite=:{bitmap, sequence, loop} gst
    #   (bid, gst)   = createGameBitmap bitmap gst
    #   (sprid, gst) = accGStTb (InitSpriteAnimation bid sequence loop) gst
    = (0-sprid, gst)

createNewGameObject :: !ObjectCode !SubCode !Point2 !(GSt .gs) -> (!GRESULT, !GSt .gs)
createNewGameObject ot st p gst
    =   accGStTb (InitGameObject ot st p) gst

:: ObjectFocus
   = { scrollleft      :: !Int
     , scrollup        :: !Int
     , scrollright     :: !Int
     , scrolldown      :: !Int
     , maxxscrollspeed :: !Int
     , maxyscrollspeed :: !Int
     }

instance zero ObjectFocus where
    zero = { scrollleft      = 0
           , scrollup        = 0
           , scrollright     = 0
           , scrolldown      = 0
           , maxxscrollspeed = 0
           , maxyscrollspeed = 0
           }

createObjectFocus :: !ObjectFocus !(GSt .gs) -> (!GRESULT, !GSt .gs)
createObjectFocus o gst
    =   accGStTb (SetObjectFocus o.scrollleft o.scrollup o.scrollright
                      o.scrolldown o.maxxscrollspeed o.maxyscrollspeed) gst


MAX_VOLUME :==  10000
MIN_VOLUME :==      0

:: Volume
   :== Int

PAN_LEFT   :== -10000
PAN_CENTER :==      0
PAN_RIGHT  :==  10000

:: Pan
   :== Int

DEFAULT_FREQUENCY :== 0

:: Frequency
   :== Int

playSoundSample :: !SoundID !Volume !Pan !Frequency !GameTime !(GSt .gs) -> (!GRESULT, !GSt .gs)
playSoundSample id vol pan freq delay gst
    =   accGStTb (OSPlaySoundSample id (vol - 10000) pan freq delay) gst

:: EventTarget
   = Self | AllObjects | BoundType Bounds

EventTargetToInt :: !EventTarget -> Int
EventTargetToInt Self          = 0
EventTargetToInt AllObjects    = -1
EventTargetToInt (BoundType b) = b

// modified 01/11/99
createUserGameEvent :: !EventCode !EventPar !EventPar !EventTarget !SubCode !GameTime !(GSt .gs) -> (!GRESULT, !GSt .gs)
createUserGameEvent ev evpar1 evpar2 dest subdest time gst
    =   accGStTb (CreateUserEvent ev evpar1 evpar2 (EventTargetToInt dest) subdest time) gst

// added 01/11/99
ANY_SUBTYPE :== -1

getBoundMap :: !Int !Int !(GSt .gs) -> (!GRESULT, !(!Int, !DirectionSet), !GSt .gs)
getBoundMap x y gst
    #   (result, gst) =   accGStTb (OSGetBoundMap x y) gst
    #   (gr, val)     =   result
    =   (gr, fromBoundMapCode val, gst)
    

setBoundMap :: !Int !Int !(!Int, !DirectionSet) !(GSt .gs) -> (!GRESULT, !GSt .gs)
setBoundMap x y newvalue gst
    =   accGStTb (OSSetBoundMap x y (toBoundMapCode newvalue)) gst



defaultObjectOptions :: ObjectOptions
defaultObjectOptions
 = { // fixed               = False
  // , ignorestaticbounds  = False
    ignorelevelbounds   = False
  // , bounceatcollisions  = False
  // , checkmapcodes       = False
   , checkkeyboard       = False
   , allowkeyboardrepeat = False
   , static              = False
   , hdirection          = DirRight
   , vdirection          = DirDown
   , automirrorleftright = False
   , automirrorupdown    = False
   , freeze              = False
   , removemapcode       = False
   }

defaultObjectRec :: SubCode Point2 Size GameTime !*(GSt .gs) -> (!GameObjectRec,!*GSt .gs)
defaultObjectRec objsubcode position size time gs
  = ( { active          = True
      , subcode         = objsubcode
      , size            = size
      , pos             = position
      , offset          = zero
      , currentsprite   = 1
      , displayoptions  = { blink           = False
                          , stretch         = False
                          , mirrorleftright = False
                          , mirrorupdown    = False
                          , rotation        = NoRotation }
      , ownbounds       = 0
      , bouncebounds    = 0
      , collidebounds   = 0
      , forgetdistance  = {x = 1, y = 1}
      , framecounter    = time
      , layer           = InFront
      , acceleration    = zero
      , speed           = zero
      , bounce          = {fvx = Value 0.0, fvy = Value 0.0}
      , maxspeed        = {rx  = fxr EVERYTHING,ry = fxr EVERYTHING}
      , slowdown        = {fvx = Value 0.0, fvy = Value 0.0}
      , skipmove        = SK_FOREVER
      , options         = defaultObjectOptions
      }
    , gs)

defaultInitObject :: Size state SubCode Point2 GameTime !*(GSt .gs) -> GameObjectState state *(GSt .gs)
defaultInitObject size state subtype pos time gs
    # (newobjrec, gs) = defaultObjectRec subtype pos size time gs
    = {st=state,gs=gs,or=newobjrec}

defaultGameObject :: !ObjectCode !Size state -> GameObject state *(GSt .gs)
defaultGameObject objcode size state
  = { objectcode = objcode
    , sprites    = []
    , init       = defaultInitObject size state
    , done       = \{gs} -> gs
    , move       = id
    , animation  = id
    , touchbound = \_ _   -> id
    , collide    = \_ _ _ -> id
    , frametimer = id
    , keydown    = \_     -> id
    , keyup      = \_     -> id
    , userevent  = \_ _ _ -> id
    }


blankScreen :: Level (GSt .gs)
blankScreen
  = { boundmap     = { map = [{0}]
                     , blocksize = {w = 10000, h = 10000}
                     , objstart  = 1
                     , startobjx = 1
                     , startobjy = 1
                     }
    , initpos      = zero
    , layers       = []
    , objects      = []
    , music        = Nothing
    , soundsamples = []
    , leveloptions = { fillbackground = Just Black
                     , escquit        = True
                     , debugscroll    = False
                     , fadein         = False
                     , fadeout        = False
                     }
    }


defaultShadow :: !Int -> Shadow
defaultShadow n =
    { shadowpos   = {x = n, y = n}
    , shadowcolor = Black
    }

defaultMovement :: Movement
defaultMovement = const

defaultScrollMovement :: !Int -> Movement
defaultScrollMovement n = \p _ -> {x = p.x / n, y = p.y / n}


alignCentered :: TextAlignment
alignCentered = { xyfromscreencenter = (True, True)
                , xycentered         = (True, True)
                }
