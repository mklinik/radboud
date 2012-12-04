
definition module WormsGfx

import StdEnv, StdGameDef

OBJ_START :== 0x10

Level1Bitmap :: GameBitmap

Level1Map1Map :: [{#Int}]
Level1Map1BoundMapData :: [{#Int}]
Level1Map1BoundMap :: BoundMap
Level1Map1WideBoundMap :: BoundMap


Level1Sequences :: [(Int, [(Int, Int)])]

Level1Map1Layer :: Layer

Level2Bitmap :: GameBitmap

Level2Map1Map :: [{#Int}]
Level2Map1BoundMapData :: [{#Int}]
Level2Map1BoundMap :: BoundMap
Level2Map1WideBoundMap :: BoundMap


Level2Sequences :: [(Int, [(Int, Int)])]

Level2Map1Layer :: Layer

Level3Bitmap :: GameBitmap

Level3Map1Map :: [{#Int}]
Level3Map1BoundMapData :: [{#Int}]
Level3Map1BoundMap :: BoundMap
Level3Map1WideBoundMap :: BoundMap


Level3Sequences :: [(Int, [(Int, Int)])]

Level3Map1Layer :: Layer

SegmentBitmap :: GameBitmap

SegmentWormSegmentMap :: [{#Int}]
SegmentWormSegmentBoundMapData :: [{#Int}]
SegmentWormSegmentBoundMap :: BoundMap
SegmentWormSegmentWideBoundMap :: BoundMap

SegmentSeq1 :: (Int, [(Int, Int)])

SegmentSequences :: [(Int, [(Int, Int)])]

SegmentWormSegmentLayer :: Layer

WormSegmentSprite1 :: Sprite
WormSegmentAnimation1 :: Sprite
HeadBitmap :: GameBitmap

HeadWormHeadMap :: [{#Int}]
HeadWormHeadBoundMapData :: [{#Int}]
HeadWormHeadBoundMap :: BoundMap
HeadWormHeadWideBoundMap :: BoundMap

HeadSeq1 :: (Int, [(Int, Int)])
HeadSeq2 :: (Int, [(Int, Int)])
HeadSeq3 :: (Int, [(Int, Int)])
HeadSeq4 :: (Int, [(Int, Int)])

HeadSequences :: [(Int, [(Int, Int)])]

HeadWormHeadLayer :: Layer

WormHeadSprite1 :: Sprite
WormHeadAnimation1 :: Sprite
WormHeadSprite2 :: Sprite
WormHeadAnimation2 :: Sprite
WormHeadSprite3 :: Sprite
WormHeadAnimation3 :: Sprite
WormHeadSprite4 :: Sprite
WormHeadAnimation4 :: Sprite
FoodBitmap :: GameBitmap

FoodFoodMap :: [{#Int}]
FoodFoodBoundMapData :: [{#Int}]
FoodFoodBoundMap :: BoundMap
FoodFoodWideBoundMap :: BoundMap

FoodSeq1 :: (Int, [(Int, Int)])

FoodSequences :: [(Int, [(Int, Int)])]

FoodFoodLayer :: Layer

FoodSprite1 :: Sprite
FoodAnimation1 :: Sprite
