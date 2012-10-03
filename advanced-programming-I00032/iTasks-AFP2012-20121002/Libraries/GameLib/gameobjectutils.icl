implementation module gameobjectutils

import	StdBool, StdList
import	StdMaybe
import	gameCrossCall_12
import	fixed, gamehandle, gst

CompressBools :: !(!Bool, !Bool, !Bool, !Bool, !Bool, !Bool, !Bool, !Bool) -> Int
CompressBools (b7, b6, b5, b4, b3, b2, b1, b0)
  = ((toInt01 b7) << 7) bitor
    ((toInt01 b6) << 6) bitor
    ((toInt01 b5) << 5) bitor
    ((toInt01 b4) << 4) bitor
    ((toInt01 b3) << 3) bitor
    ((toInt01 b2) << 2) bitor
    ((toInt01 b1) << 1) bitor
    ((toInt01 b0) << 0)

toInt01 :: !Bool -> Int
toInt01 True  = 1
toInt01 False = 0

// store an objectrec in the game engine
SetObjectRec :: !InstanceID !ObjectCode !GameObjectRec ![SpriteID] !*OSToolbox -> (!GRESULT, !*OSToolbox)
SetObjectRec id ot or spr tb
    = WinSetObjectRec id ot
        or.subcode
        or.active
        or.pos.x or.pos.y
        or.size.w or.size.h
        or.offset.x or.offset.y
        ((findlistvalue or.currentsprite spr) bitor (or.currentsprite << 16))
        (CompressBools (False,
            r270,
            r180,
            r90,
            or.displayoptions.mirrorupdown,
            or.displayoptions.mirrorleftright,
            or.displayoptions.stretch,
            or.displayoptions.blink))
        or.ownbounds or.bouncebounds or.collidebounds
        or.forgetdistance.x or.forgetdistance.y
        or.framecounter
        (layerpositionint or.layer)
        (fix or.acceleration.rx) (fix or.acceleration.ry)
        (fix or.speed.rx) (fix or.speed.ry)
        (FVToInt or.bounce.fvx) (FVToInt or.bounce.fvy)
        (fix or.maxspeed.rx) (fix or.maxspeed.ry)
        (FVToInt or.slowdown.fvx) (FVToInt or.slowdown.fvy)
        or.skipmove
        (((CompressBools (False, False,
            opt.removemapcode,
            opt.freeze,
            opt.automirrorupdown,
            opt.automirrorleftright,
            vdir,
            hdir)) << 8) bitor
        (CompressBools (
            opt.static,
            opt.allowkeyboardrepeat,
            opt.checkkeyboard,
            False,
            False,
            opt.ignorelevelbounds,
            False,
            False)))
        tb
where
    opt = or.options
    hdir = case opt.hdirection of
            DirLeft    -> True
            DirRight   -> False
    vdir = case opt.vdirection of
            DirUp      -> True
            DirDown    -> False
    (r90,r180,r270) =
        case or.displayoptions.rotation of
            Rotate90   -> (True,  False, False)
            Rotate180  -> (False, True,  False)
            Rotate270  -> (False, False, True)
            NoRotation -> (False, False, False)
    layerpositionint :: LayerPosition -> Int
    layerpositionint InFront   = EVERYTHING
    layerpositionint (AtLayer n) = n
    FVToInt :: FV -> Int
    FVToInt (Factor r) = (~ (fix r))
    FVToInt (Value  r) = (fix r)

findlistvalue :: !Int ![Int] -> Int
findlistvalue _ []     = 0
findlistvalue 1 [x:xs] = x
findlistvalue n [x:xs]
    | n == 1    = x
    | n > 1     = findlistvalue (n-1) xs
    | otherwise = ~n

// load an GameObjectRec from the game engine
GetObjectRec :: !Int !*OSToolbox -> (!GRESULT, !ObjectCode, !GameObjectRec, !*OSToolbox)
GetObjectRec id tb
    #  (ot, st, act, x, y, w, h,
        xofs, yofs, spr, do,
        bnds, bncbnds, colbnds, fx, fy, time,
        lyr, xacc, yacc, xv, yv, xbnc, ybnc, maxxv, maxyv, xsl, ysl, md, opt,
        result, tb) = WinGetObjectRec id tb
    =  (result, ot,
        {subcode = st, active = act, size = {w=w,h=h},
        pos = {x=x,y=y}, offset = {x=xofs,y=yofs}, currentsprite = (spr >> 16),

        displayoptions =
        {blink           = (not ((do bitand DO_BLINK)             == 0)),
         stretch         = (not ((do bitand DO_STRETCH)           == 0)),
         mirrorleftright = (not ((do bitand DO_MIRROR_LEFT_RIGHT) == 0)),
         mirrorupdown    = (not ((do bitand DO_MIRROR_UP_DOWN)    == 0)),

         rotation = (if (not ((do bitand DO_ROTATE_90)  == 0)) Rotate90
                    (if (not ((do bitand DO_ROTATE_180) == 0)) Rotate180
                    (if (not ((do bitand DO_ROTATE_270) == 0)) Rotate270
                                                               NoRotation)))
        },

        ownbounds = bnds, bouncebounds = bncbnds, collidebounds = colbnds,
        forgetdistance = {x=fx,y=fy}, framecounter = time,
        layer = if (lyr == EVERYTHING) InFront (AtLayer lyr),
        acceleration = {rx=fxr xacc,ry=fxr yacc},
        speed = {rx=fxr xv,ry=fxr yv},
        bounce = {fvx=IntToFV xbnc,fvy=IntToFV ybnc},
        maxspeed = {rx=fxr maxxv,ry=fxr maxyv},
        slowdown = {fvx=IntToFV xsl,fvy=IntToFV ysl},
        skipmove = md,

        options =
          { // fixed               = (not ((opt bitand OO_FIXED)                  == 0))
        //  , ignorestaticbounds  = (not ((opt bitand OO_IGNORE_STATIC_BOUNDS)   == 0))
           ignorelevelbounds   = (not ((opt bitand OO_IGNORE_LEVEL_BOUNDS)    == 0))
        //  , bounceatcollisions  = (not ((opt bitand OO_BOUNCE_AT_COLLISIONS)   == 0))
        //  , checkmapcodes       = (not ((opt bitand OO_CHECK_MAP_CODES)        == 0))
          , checkkeyboard       = (not ((opt bitand OO_CHECK_KEYBOARD)         == 0))
          , allowkeyboardrepeat = (not ((opt bitand OO_ALLOW_KEYBOARD_REPEAT)  == 0))
          , static              = (not ((opt bitand OO_STATIC)                 == 0))
          , hdirection          = (if ((opt bitand OO_LAST_DIRECTION_LEFT) == 0) DirRight DirLeft)
          , vdirection          = (if ((opt bitand OO_LAST_DIRECTION_UP)   == 0) DirDown DirUp)
          , automirrorleftright = (not ((opt bitand OO_AUTO_MIRROR_LEFT_RIGHT) == 0))
          , automirrorupdown    = (not ((opt bitand OO_AUTO_MIRROR_UP_DOWN)    == 0))
          , freeze              = (not ((opt bitand OO_FREEZE)                 == 0))
          , removemapcode       = (not ((opt bitand OO_REMOVE_MAP_CODE)        == 0))
          }
        },
        tb)
where
    IntToFV :: !Int -> FV
    IntToFV x
        | x < 0     = Factor (~ (fxr x))
        | otherwise = Value  (fxr x)

// get the definition of an object by it's ObjectType
getobject :: !ObjectCode !(GameHandle .gs) -> Maybe (GameObjectHandleLS (GSt .gs))
getobject objcode gamehnd
    #   curlevel    =   hd gamehnd.levels`
    #   objectlist  =   curlevel.objects`
    #   found       =   filter cmpobjtypes objectlist
    |   isEmpty found
    =   Nothing
    =   Just (hd found)
where
    cmpobjtypes :: !(GameObjectHandleLS .gst) -> Bool
    cmpobjtypes (GameObjectHandleLS ot) = ot.objectcode` == objcode

// store the definition of an object in the game definition
putobject :: !(GameObjectHandleLS (GSt .gs)) !(GameHandle .gs) -> GameHandle .gs
putobject obj gamehnd
    #   curlevel    =   hd gamehnd.levels`
    #   newobjlist  =   replaceobj curlevel.objects` obj []
    #   curlevel    =   {curlevel & objects`=newobjlist}
    =   {gamehnd & levels`=[curlevel]}
where
    replaceobj :: ![GameObjectHandleLS .gst] (GameObjectHandleLS .gst) ![GameObjectHandleLS .gst] -> [GameObjectHandleLS .gst]
    replaceobj [] _ l = l
/*
    replaceobj [x:xs] y
        | x.objecttype` == y.objecttype`    =   [y:replaceobj xs y]
        | otherwise                         =   [x:replaceobj xs y]
*/  
// optimization:
    replaceobj [GameObjectHandleLS x:xs] obj=:(GameObjectHandleLS y) l
        | x.objectcode` == y.objectcode` = l ++ [obj:xs]
        | otherwise                      = replaceobj xs obj (l ++ [GameObjectHandleLS x])

// find object state in tuple list
findinstance :: ![(a,b)] a -> Maybe b  | ==a
findinstance [] _ = Nothing
findinstance [(x,y):s] z
    | x == z    = Just y
    | otherwise = findinstance s z

// replace object state in tuple list
updateinstance :: a b ![(a,b)] -> [(a,b)]  | ==a
updateinstance _ _ [] = []
/*
updateinstance v w [(x,y):z]
    | v == x    = [(v,w):updateinstance v w z]
    | otherwise = [(x,y):updateinstance v w z]
*/
// optimization:
updateinstance v w l = updinst v w l []
where
    updinst _ _ [] l = l
    updinst v w [(x,y):z] l
        | v == x    = l ++ [(v,w):z]
        | otherwise = updinst v w z ([(x,y)] ++ l)


// remove object from tuple list
removeinstance :: a ![(a,b)] -> [(a,b)]  | ==a
removeinstance _ [] = []
/*
removeinstance v [(x,y):z]
    | v == x    = removeinstance v z
    | otherwise = [(x,y):removeinstance v z]
*/
// optimization:
removeinstance v l = reminst v l []
where
    reminst _ [] _ = []
    reminst v [(x,y):z] l
        | v == x    = l ++ z
        | otherwise = reminst v z ([(x,y)] ++ l)

// create a DirectionSet from an integer value (0-15)
makeDirectionSet :: !Int -> DirectionSet
makeDirectionSet x
    # top    = (((x >> 0) bitand 1) == 1)
      left   = (((x >> 1) bitand 1) == 1)
      bottom = (((x >> 2) bitand 1) == 1)
      right  = (((x >> 3) bitand 1) == 1)
    = {top=top,left=left,bottom=bottom,right=right}

fromDirectionSet :: !DirectionSet -> Int
fromDirectionSet d = CompressBools (False, False, False, False, 
                                    d.right, d.bottom, d.left, d.top)

toBoundMapCode :: !(!Int,!DirectionSet) -> Int
toBoundMapCode (c, d) = (c << 8) + (fromDirectionSet d)

fromBoundMapCode :: !Int -> (!Int,!DirectionSet)
fromBoundMapCode x = (x >> 8, makeDirectionSet x)
