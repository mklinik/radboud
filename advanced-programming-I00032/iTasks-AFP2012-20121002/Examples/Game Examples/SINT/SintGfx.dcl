
definition module SintGfx

import StdEnv, StdGameDef
import SintGfx2

OBJ_START :== 0x10

L1Bitmap :: GameBitmap

L1LevelMap :: [{#Int}]
L1LevelBoundMapData :: [{#Int}]
L1LevelBoundMap :: BoundMap
L1LevelWideBoundMap :: BoundMap
L1BonusMap :: [{#Int}]
L1BonusBoundMapData :: [{#Int}]
L1BonusBoundMap :: BoundMap
L1BonusWideBoundMap :: BoundMap
L1BackGrMap :: [{#Int}]
L1BackGrBoundMapData :: [{#Int}]
L1BackGrBoundMap :: BoundMap
L1BackGrWideBoundMap :: BoundMap
L1InFrontMap :: [{#Int}]
L1InFrontBoundMapData :: [{#Int}]
L1InFrontBoundMap :: BoundMap
L1InFrontWideBoundMap :: BoundMap
L1BonusBackGrMap :: [{#Int}]
L1BonusBackGrBoundMapData :: [{#Int}]
L1BonusBackGrBoundMap :: BoundMap
L1BonusBackGrWideBoundMap :: BoundMap

L1Seq1 :: (Int, [(Int, Int)])
L1Seq2 :: (Int, [(Int, Int)])
L1Seq3 :: (Int, [(Int, Int)])
L1Seq4 :: (Int, [(Int, Int)])
L1Seq5 :: (Int, [(Int, Int)])
L1Seq6 :: (Int, [(Int, Int)])
L1Seq7 :: (Int, [(Int, Int)])

L1Sequences :: [(Int, [(Int, Int)])]

L1LevelLayer :: Layer
L1BonusLayer :: Layer
L1BackGrLayer :: Layer
L1InFrontLayer :: Layer
L1BonusBackGrLayer :: Layer

LevelSprite8 :: Sprite
LevelAnimation8 :: Sprite
InFrontSprite1 :: Sprite
InFrontAnimation1 :: Sprite
BonusSprite10 :: Sprite
BonusAnimation10 :: Sprite
L1Sprite3 :: Sprite
L1Animation3 :: Sprite
InFrontSprite2 :: Sprite
InFrontAnimation2 :: Sprite
InFrontSprite3 :: Sprite
InFrontAnimation3 :: Sprite
InFrontSprite4 :: Sprite
InFrontAnimation4 :: Sprite
L1_40x24Bitmap :: GameBitmap

L1_40x24Sky1Map :: [{#Int}]
L1_40x24Sky1BoundMapData :: [{#Int}]
L1_40x24Sky1BoundMap :: BoundMap
L1_40x24Sky1WideBoundMap :: BoundMap
L1_40x24BonusSkyMap :: [{#Int}]
L1_40x24BonusSkyBoundMapData :: [{#Int}]
L1_40x24BonusSkyBoundMap :: BoundMap
L1_40x24BonusSkyWideBoundMap :: BoundMap

L1_40x24Seq1 :: (Int, [(Int, Int)])
L1_40x24Seq2 :: (Int, [(Int, Int)])
L1_40x24Seq3 :: (Int, [(Int, Int)])
L1_40x24Seq4 :: (Int, [(Int, Int)])
L1_40x24Seq5 :: (Int, [(Int, Int)])
L1_40x24Seq6 :: (Int, [(Int, Int)])
L1_40x24Seq7 :: (Int, [(Int, Int)])
L1_40x24Seq8 :: (Int, [(Int, Int)])

L1_40x24Sequences :: [(Int, [(Int, Int)])]

L1_40x24Sky1Layer :: Layer
L1_40x24BonusSkyLayer :: Layer

Sky1Sprite2 :: Sprite
Sky1Animation2 :: Sprite
Sky1Sprite4 :: Sprite
Sky1Animation4 :: Sprite
Sky1Sprite1 :: Sprite
Sky1Animation1 :: Sprite
Sky1Sprite3 :: Sprite
Sky1Animation3 :: Sprite
L1_40x24Sprite4 :: Sprite
L1_40x24Animation4 :: Sprite
BonusSkySprite3 :: Sprite
BonusSkyAnimation3 :: Sprite
L1_40x24Sprite6 :: Sprite
L1_40x24Animation6 :: Sprite
L1_40x24Sprite7 :: Sprite
L1_40x24Animation7 :: Sprite
L1_32x32Bitmap :: GameBitmap

L1_32x32BoatsMap :: [{#Int}]
L1_32x32BoatsBoundMapData :: [{#Int}]
L1_32x32BoatsBoundMap :: BoundMap
L1_32x32BoatsWideBoundMap :: BoundMap


L1_32x32Sequences :: [(Int, [(Int, Int)])]

L1_32x32BoatsLayer :: Layer
