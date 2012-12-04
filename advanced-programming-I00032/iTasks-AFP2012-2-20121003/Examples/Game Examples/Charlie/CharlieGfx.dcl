
definition module CharlieGfx

import StdEnv, StdGameDef

OBJ_START :== 0x10

ChBitmap :: GameBitmap

ChTitleMap :: [{#Int}]
ChTitleBoundMapData :: [{#Int}]
ChTitleBoundMap :: BoundMap
ChTitleWideBoundMap :: BoundMap
ChLevel1Map :: [{#Int}]
ChLevel1BoundMapData :: [{#Int}]
ChLevel1BoundMap :: BoundMap
ChLevel1WideBoundMap :: BoundMap
ChLevel2Map :: [{#Int}]
ChLevel2BoundMapData :: [{#Int}]
ChLevel2BoundMap :: BoundMap
ChLevel2WideBoundMap :: BoundMap
ChInFrontMap :: [{#Int}]
ChInFrontBoundMapData :: [{#Int}]
ChInFrontBoundMap :: BoundMap
ChInFrontWideBoundMap :: BoundMap

ChSeq1 :: (Int, [(Int, Int)])
ChSeq2 :: (Int, [(Int, Int)])
ChSeq3 :: (Int, [(Int, Int)])

ChSequences :: [(Int, [(Int, Int)])]

ChTitleLayer :: Layer
ChLevel1Layer :: Layer
ChLevel2Layer :: Layer
ChInFrontLayer :: Layer

InFrontSprite1 :: Sprite
InFrontAnimation1 :: Sprite
InFrontSprite2 :: Sprite
InFrontAnimation2 :: Sprite
InFrontSprite3 :: Sprite
InFrontAnimation3 :: Sprite
MainCharBitmap :: GameBitmap

MainCharCharlieMap :: [{#Int}]
MainCharCharlieBoundMapData :: [{#Int}]
MainCharCharlieBoundMap :: BoundMap
MainCharCharlieWideBoundMap :: BoundMap

MainCharSeq1 :: (Int, [(Int, Int)])
MainCharSeq2 :: (Int, [(Int, Int)])
MainCharSeq3 :: (Int, [(Int, Int)])
MainCharSeq4 :: (Int, [(Int, Int)])
MainCharSeq5 :: (Int, [(Int, Int)])
MainCharSeq6 :: (Int, [(Int, Int)])

MainCharSequences :: [(Int, [(Int, Int)])]

MainCharCharlieLayer :: Layer

CharlieSprite1 :: Sprite
CharlieAnimation1 :: Sprite
CharlieSprite2 :: Sprite
CharlieAnimation2 :: Sprite
CharlieSprite3 :: Sprite
CharlieAnimation3 :: Sprite
CharlieSprite4 :: Sprite
CharlieAnimation4 :: Sprite
CharlieSprite5 :: Sprite
CharlieAnimation5 :: Sprite
CharlieSprite6 :: Sprite
CharlieAnimation6 :: Sprite
ItemsBitmap :: GameBitmap

ItemsItemMap :: [{#Int}]
ItemsItemBoundMapData :: [{#Int}]
ItemsItemBoundMap :: BoundMap
ItemsItemWideBoundMap :: BoundMap

ItemsSeq1 :: (Int, [(Int, Int)])
ItemsSeq2 :: (Int, [(Int, Int)])
ItemsSeq3 :: (Int, [(Int, Int)])
ItemsSeq4 :: (Int, [(Int, Int)])
ItemsSeq5 :: (Int, [(Int, Int)])
ItemsSeq6 :: (Int, [(Int, Int)])
ItemsSeq7 :: (Int, [(Int, Int)])
ItemsSeq8 :: (Int, [(Int, Int)])

ItemsSequences :: [(Int, [(Int, Int)])]

ItemsItemLayer :: Layer

ItemSprite1 :: Sprite
ItemAnimation1 :: Sprite
ItemSprite2 :: Sprite
ItemAnimation2 :: Sprite
ItemSprite3 :: Sprite
ItemAnimation3 :: Sprite
ItemSprite4 :: Sprite
ItemAnimation4 :: Sprite
ItemSprite5 :: Sprite
ItemAnimation5 :: Sprite
ItemSprite6 :: Sprite
ItemAnimation6 :: Sprite
ItemSprite7 :: Sprite
ItemAnimation7 :: Sprite
ItemSprite8 :: Sprite
ItemAnimation8 :: Sprite
BeesBitmap :: GameBitmap

BeesBeeMap :: [{#Int}]
BeesBeeBoundMapData :: [{#Int}]
BeesBeeBoundMap :: BoundMap
BeesBeeWideBoundMap :: BoundMap

BeesSeq1 :: (Int, [(Int, Int)])

BeesSequences :: [(Int, [(Int, Int)])]

BeesBeeLayer :: Layer

BeeSprite1 :: Sprite
BeeAnimation1 :: Sprite
CloudsBitmap :: GameBitmap

CloudsCloudMap :: [{#Int}]
CloudsCloudBoundMapData :: [{#Int}]
CloudsCloudBoundMap :: BoundMap
CloudsCloudWideBoundMap :: BoundMap

CloudsSeq1 :: (Int, [(Int, Int)])
CloudsSeq2 :: (Int, [(Int, Int)])

CloudsSequences :: [(Int, [(Int, Int)])]

CloudsCloudLayer :: Layer

CloudSprite1 :: Sprite
CloudAnimation1 :: Sprite
CloudSprite2 :: Sprite
CloudAnimation2 :: Sprite
EndingBitmap :: GameBitmap

EndingEndingMap :: [{#Int}]
EndingEndingBoundMapData :: [{#Int}]
EndingEndingBoundMap :: BoundMap
EndingEndingWideBoundMap :: BoundMap

EndingSeq1 :: (Int, [(Int, Int)])
EndingSeq2 :: (Int, [(Int, Int)])
EndingSeq3 :: (Int, [(Int, Int)])
EndingSeq4 :: (Int, [(Int, Int)])
EndingSeq5 :: (Int, [(Int, Int)])
EndingSeq6 :: (Int, [(Int, Int)])
EndingSeq7 :: (Int, [(Int, Int)])
EndingSeq8 :: (Int, [(Int, Int)])
EndingSeq9 :: (Int, [(Int, Int)])

EndingSequences :: [(Int, [(Int, Int)])]

EndingEndingLayer :: Layer

EndingSprite1 :: Sprite
EndingAnimation1 :: Sprite
EndingSprite2 :: Sprite
EndingAnimation2 :: Sprite
EndingSprite3 :: Sprite
EndingAnimation3 :: Sprite
EndingSprite4 :: Sprite
EndingAnimation4 :: Sprite
EndingSprite5 :: Sprite
EndingAnimation5 :: Sprite
EndingSprite6 :: Sprite
EndingAnimation6 :: Sprite
EndingSprite7 :: Sprite
EndingAnimation7 :: Sprite
EndingSprite8 :: Sprite
EndingAnimation8 :: Sprite
EndingSprite9 :: Sprite
EndingAnimation9 :: Sprite
EnemiesBitmap :: GameBitmap

EnemiesEnemyMap :: [{#Int}]
EnemiesEnemyBoundMapData :: [{#Int}]
EnemiesEnemyBoundMap :: BoundMap
EnemiesEnemyWideBoundMap :: BoundMap

EnemiesSeq1 :: (Int, [(Int, Int)])
EnemiesSeq2 :: (Int, [(Int, Int)])

EnemiesSequences :: [(Int, [(Int, Int)])]

EnemiesEnemyLayer :: Layer

EnemySprite1 :: Sprite
EnemyAnimation1 :: Sprite
EnemySprite2 :: Sprite
EnemyAnimation2 :: Sprite
FrogsBitmap :: GameBitmap

FrogsFrogMap :: [{#Int}]
FrogsFrogBoundMapData :: [{#Int}]
FrogsFrogBoundMap :: BoundMap
FrogsFrogWideBoundMap :: BoundMap

FrogsSeq1 :: (Int, [(Int, Int)])
FrogsSeq2 :: (Int, [(Int, Int)])

FrogsSequences :: [(Int, [(Int, Int)])]

FrogsFrogLayer :: Layer

FrogSprite1 :: Sprite
FrogAnimation1 :: Sprite
FrogSprite2 :: Sprite
FrogAnimation2 :: Sprite
PalmFrontBitmap :: GameBitmap

PalmFrontPalmMap :: [{#Int}]
PalmFrontPalmBoundMapData :: [{#Int}]
PalmFrontPalmBoundMap :: BoundMap
PalmFrontPalmWideBoundMap :: BoundMap

PalmFrontSeq1 :: (Int, [(Int, Int)])
PalmFrontSeq2 :: (Int, [(Int, Int)])
PalmFrontSeq3 :: (Int, [(Int, Int)])

PalmFrontSequences :: [(Int, [(Int, Int)])]

PalmFrontPalmLayer :: Layer

PalmSprite1 :: Sprite
PalmAnimation1 :: Sprite
PalmSprite2 :: Sprite
PalmAnimation2 :: Sprite
PalmSprite3 :: Sprite
PalmAnimation3 :: Sprite
CratePartBitmap :: GameBitmap

CratePartPartMap :: [{#Int}]
CratePartPartBoundMapData :: [{#Int}]
CratePartPartBoundMap :: BoundMap
CratePartPartWideBoundMap :: BoundMap

CratePartSeq1 :: (Int, [(Int, Int)])

CratePartSequences :: [(Int, [(Int, Int)])]

CratePartPartLayer :: Layer

PartSprite1 :: Sprite
PartAnimation1 :: Sprite
StatusBitmap :: GameBitmap

StatusStatusMap :: [{#Int}]
StatusStatusBoundMapData :: [{#Int}]
StatusStatusBoundMap :: BoundMap
StatusStatusWideBoundMap :: BoundMap

StatusSeq1 :: (Int, [(Int, Int)])
StatusSeq2 :: (Int, [(Int, Int)])
StatusSeq3 :: (Int, [(Int, Int)])
StatusSeq4 :: (Int, [(Int, Int)])
StatusSeq5 :: (Int, [(Int, Int)])
StatusSeq6 :: (Int, [(Int, Int)])

StatusSequences :: [(Int, [(Int, Int)])]

StatusStatusLayer :: Layer

StatusSprite1 :: Sprite
StatusAnimation1 :: Sprite
StatusSprite2 :: Sprite
StatusAnimation2 :: Sprite
StatusSprite3 :: Sprite
StatusAnimation3 :: Sprite
StatusSprite4 :: Sprite
StatusAnimation4 :: Sprite
StatusSprite5 :: Sprite
StatusAnimation5 :: Sprite
StatusSprite6 :: Sprite
StatusAnimation6 :: Sprite
WaterBitmap :: GameBitmap

WaterWaterMap :: [{#Int}]
WaterWaterBoundMapData :: [{#Int}]
WaterWaterBoundMap :: BoundMap
WaterWaterWideBoundMap :: BoundMap

WaterSeq1 :: (Int, [(Int, Int)])
WaterSeq2 :: (Int, [(Int, Int)])
WaterSeq3 :: (Int, [(Int, Int)])

WaterSequences :: [(Int, [(Int, Int)])]

WaterWaterLayer :: Layer

WaterSprite1 :: Sprite
WaterAnimation1 :: Sprite
WaterSprite2 :: Sprite
WaterAnimation2 :: Sprite
WaterSprite3 :: Sprite
WaterAnimation3 :: Sprite
BackGr1Bitmap :: GameBitmap

BackGr1Map1Map :: [{#Int}]
BackGr1Map1BoundMapData :: [{#Int}]
BackGr1Map1BoundMap :: BoundMap
BackGr1Map1WideBoundMap :: BoundMap


BackGr1Sequences :: [(Int, [(Int, Int)])]

BackGr1Map1Layer :: Layer

BackGr2Bitmap :: GameBitmap

BackGr2Map1Map :: [{#Int}]
BackGr2Map1BoundMapData :: [{#Int}]
BackGr2Map1BoundMap :: BoundMap
BackGr2Map1WideBoundMap :: BoundMap


BackGr2Sequences :: [(Int, [(Int, Int)])]

BackGr2Map1Layer :: Layer

BackGr3Bitmap :: GameBitmap

BackGr3Map1Map :: [{#Int}]
BackGr3Map1BoundMapData :: [{#Int}]
BackGr3Map1BoundMap :: BoundMap
BackGr3Map1WideBoundMap :: BoundMap


BackGr3Sequences :: [(Int, [(Int, Int)])]

BackGr3Map1Layer :: Layer

FlashBitmap :: GameBitmap

FlashFlashMap :: [{#Int}]
FlashFlashBoundMapData :: [{#Int}]
FlashFlashBoundMap :: BoundMap
FlashFlashWideBoundMap :: BoundMap

FlashSeq1 :: (Int, [(Int, Int)])

FlashSequences :: [(Int, [(Int, Int)])]

FlashFlashLayer :: Layer

FlashSprite1 :: Sprite
FlashAnimation1 :: Sprite
