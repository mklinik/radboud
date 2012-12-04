implementation module StdGameDef

import	StdString
from	StdFunc			import :: St
from	StdOverloaded	import class zero
from	StdIOBasic		import :: Point2, :: Size, :: IdFun
from	StdMaybe		import :: Maybe
from	StdPictureDef	import :: Colour
import	StdGSt

:: GameAttribute gs
   = ScreenSize Size     // screen resolution, default 320x240
   | ColorDepth Int      // screen color depth, default 8 (256 colors)

:: Game gs
   = { levels        :: [Level (GSt gs)]        // levels
     , quitlevel     :: St (GSt gs) Bool        // True quits the game
     , nextlevel     :: St (GSt gs) Int         // new level if >0 (0 quits)
     , textitems     :: St (GSt gs) [GameText]  // all text items
     }

:: Level state
   = { boundmap      :: !BoundMap               // map of all static bounds in a level
     , initpos       :: !Point2                 // center of screen in boundmap
     , layers        :: ![Layer]                // all layers [back..front]
     , objects       :: ![GameObjectLS state]   // all other objects in the level
     , music         :: !Maybe Music            // background music
     , soundsamples  :: ![SoundSample]          // list of sound samples
     , leveloptions  :: !LevelOptions           // level options
     }

:: LevelOptions
   = { fillbackground  :: !Maybe Colour    // fill the screen before drawing the layers
     , escquit         :: !Bool            // quit the level whenever the Esc key is pressed
     , debugscroll     :: !Bool            // scroll through the level with the arrow keys
     , fadein          :: !Bool            // fade in from black
     , fadeout         :: !Bool            // fade out to black
     }

:: Music
   = { musicfile :: !String     // MIDI file
     , restart   :: !Bool       // restart the music when it ends?
     , continue  :: !Bool       // music continues after end of level
     }

:: SoundSample
   = { soundid      :: !SoundID    // id for the sample (any number)
     , soundfile    :: !String     // WAV file
     , soundbuffers :: !Int        // max times sample can be played together
     }

:: SoundID
   :== Int

:: BoundMap
   = { map       :: ![{#Int}]  // map of all static bounds
     , blocksize :: !Size      // size of the map units (in pixels)
     , objstart  :: !Int       // min. value for objects (lower values are ignored)
     , startobjx :: !Int       // X-distance from screen where objects are initialized
     , startobjy :: !Int       // Y-distance from screen where objects are initialized
     }

:: GameRegion
   :== [Int]

:: Bounds
   :== Int

:: DirectionSet
   = { top    :: !Bool
     , left   :: !Bool
     , bottom :: !Bool
     , right  :: !Bool
     }

:: Layer
   = { bmp       :: !GameBitmap        // bitmap that contains all the tiles
     , layermap  :: !LayerMap          // map of the tiles in the level
     , sequences :: ![TileSequence]    // tiles that change repeatedly
     , movement  :: !Movement          // function to scroll the layer
     }

:: GameTime
   :== Int       // time in frames

:: GameBitmap
   = { bitmapname  :: !String        // bitmap that contains smaller blocks
     , unitsize    :: !Size          // size of these blocks (width, height)
     , dimensions  :: !(!Int,!Int)   // number of blocks (horizontal, vertical)
     , transparent :: !Maybe Point2  // position of a transparent pixel
     }

:: LayerMap
   :== [{#Int}]  // map of block numbers in a bitmap
                 // 0: empty; 1..n: block number; n+1..2n: mirror block;
                 // 2n+1..3n: upsidedown; 3n+1..4n: mirror and upsidedown;
                 // -1..-m: block sequence number
                 // (n = # blocks in gamebitmap; m = # sequences)

:: TileSequence
   :== (!Int, Sequence)   // block sequence number, Sequence

:: Sequence
   :== [(Int, Int)]   // tile number, duration

:: Movement
   :== Point2 GameTime -> Point2   // calculate layer's position from game position

:: SpriteID
   :== Int

:: Sprite
   = { bitmap   :: !GameBitmap   // sprites may have their own bitmap
     , sequence :: !Sequence     // seqence of blocks
     , loop     :: !Bool         // if FALSE, callback animation function
     }

:: GameObjectLS gs
   = E. state: GameObjectLS (GameObject state gs)
:: GameObject state gs
// = E. state:
   = { objectcode :: !ObjectCode    // code for object type (0 AutoInitObject)
     , sprites    :: ![Sprite]      // sprite 1..n
     , init       :: !SubCode Point2 GameTime gs -> GameObjectState state gs
     , done       :: !(GameObjectState state gs)            -> gs
     , move       :: !                                         ObjectFun state gs
     , animation  :: !                                         ObjectFun state gs
     , touchbound :: !DirectionSet MapCode                  -> ObjectFun state gs
     , collide    :: !DirectionSet ObjectCode GameObjectRec -> ObjectFun state gs
     , frametimer :: !                                         ObjectFun state gs
     , keydown    :: !KeyCode                               -> ObjectFun state gs
     , keyup      :: !KeyCode                               -> ObjectFun state gs
     , userevent  :: !EventCode EventPar EventPar           -> ObjectFun state gs
     }
     
:: *GameObjectState state gs
   = { st :: state             // object state
     , or :: GameObjectRec     // object record
     , gs :: gs                // game state
     }

:: ObjectFun state gs
   :== IdFun (GameObjectState state gs)

:: ObjectCode
   :== Int

:: SubCode
   :== Int

:: MapCode
   :== Int

:: KeyCode
   :== Int

:: EventCode
   :== Int

:: EventPar
   :== Int

:: FVXY
   = { fvx  :: !FV
     , fvy  :: !FV
     }

:: FV
   = Factor !Real
   | Value !Real

:: RealXY
   = { rx :: !Real
     , ry :: !Real
     }

:: GameObjectRec
   = { active              :: !Bool            // move and check collisions?
     , subcode             :: !SubCode         // object's sub code
     , size                :: !Size            // the actual size
     , pos                 :: !Point2          // current position
     , offset              :: !Point2          // relative offset for sprite
     , currentsprite       :: !Int             // current animation sequence
     , displayoptions      :: !DisplayOptions  // invisible/mirror etc.
     , ownbounds           :: !Bounds          // bound(s) of the object (bits)
     , bouncebounds        :: !Bounds          // just bounce against these bounds
     , collidebounds       :: !Bounds          // call collide func for these bounds
     , forgetdistance      :: !Point2          // make object inactive at distance
     , framecounter        :: !GameTime        // frame counter
     , layer               :: !LayerPosition   // layer the object moves in front of
     , acceleration        :: !RealXY          // x/y acceleration
     , speed               :: !RealXY          // object's x/y speed
     , bounce              :: !FVXY            // x/y bounce at static bounds * 256
     , maxspeed            :: !RealXY          // x/y maximum speed
     , slowdown            :: !FVXY            // x/y slow down
     , skipmove            :: !Int             // acceleration delay
     , options             :: !ObjectOptions   // object options
     }

:: DisplayOptions
   = { blink               :: !Bool       // object blinks
     , stretch             :: !Bool       // stretch sprite to fit in size
     , mirrorleftright     :: !Bool       // mirror the sprite
     , mirrorupdown        :: !Bool       // draw sprite up side down
     , rotation            :: !Rotation   // rotation
     }

:: Rotation
   = NoRotation | Rotate90 | Rotate180 | Rotate270

:: ObjectOptions
   = { ignorelevelbounds   :: !Bool         // object can move out of the level
     , checkkeyboard       :: !Bool         // generate keydown event for this object
     , allowkeyboardrepeat :: !Bool         // allow pressed key to repeat
     , static              :: !Bool         // object always moves with screen
     , hdirection          :: !HDirection   // horizontal direction of the object
     , vdirection          :: !VDirection   // vertical direction of the object
     , automirrorleftright :: !Bool         // mirror object if horizontal direction changes
     , automirrorupdown    :: !Bool         // mirror object if vertical direction changes
     , freeze              :: !Bool         // no movement at all until framecounter reaches 0
     , removemapcode       :: !Bool         // remove the object completely from the map?
     }

:: HDirection
   = DirLeft | DirRight

:: VDirection
   = DirUp | DirDown

:: LayerPosition
   = InFront | AtLayer Int

:: GameText
   = { format    :: !String        // text to display or formatstring for value
     , value     :: Maybe Int      // value to display
     , position  :: !Point2        // position on screen
     , style     :: !Style         // style to write the text in
     , color     :: Colour         // color for the text
     , shadow    :: Maybe Shadow   // shadow
     , alignment :: TextAlignment  // text alignment
     }

:: TextAlignment
   = { xyfromscreencenter :: !(!Bool,!Bool)   // center position on the screen
     , xycentered         :: !(!Bool,!Bool)   // center text around position
     }

:: Style
   = { fontname :: !String   // any font name
     , fontsize :: !Int      // size of the text
     , bold     :: !Bool     // bold
     , italic   :: !Bool     // italic
     }

:: Shadow
   = { shadowpos   :: !Point2   // relative position of the shadow to the text
     , shadowcolor :: !Colour    // color of the shadow
     }


instance zero RealXY
where
   zero = {rx = 0.0, ry = 0.0}

instance zero TextAlignment
where
   zero = { xyfromscreencenter = (False, False)
          , xycentered = (False, False)
          }
