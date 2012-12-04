implementation module GameFunctions

import	StdArray, StdList, StdString
import  osgame

// game result codes (GRESULT)
GR_OK                  :==  0
GR_FAILED              :== -1  // very unlikely errors
GR_OS_ERROR            :== -2  // OS specific error
GR_INVALID_BITMAP_ID   :== -3  // bitmap ID doesn't exist or already used
GR_INVALID_SPRITE_ID   :== -4  // sprite ID not found
GR_INVALID_MAP_ID      :== -5  // layer map ID is invalid
GR_NOT_FOUND           :== -6  // file or resource not found

// BinaryIntStr :: !Int -> {#Char}
// BinaryIntStr x = OSBinaryIntStr x

BinaryIntListStr :: [[Int]] -> {#Char}
BinaryIntListStr [] = ""
BinaryIntListStr [x:xs] = IntListStr x +++ BinaryIntListStr xs

IntListStr :: [Int] -> {#Char}
IntListStr [] = ""
IntListStr [x:xs] = OSBinaryIntStr x +++ IntListStr xs

MapWidth :: [{#Int}] -> Int
MapWidth [] = 0
MapWidth [x:xs] = size x

MapHeight :: [a] -> Int
MapHeight x = length x

TupleStr :: (Int,Int) -> {#Char}
TupleStr (x,y) = OSBinaryIntStr x +++ OSBinaryIntStr y

TupleListStr :: [(Int,Int)] -> {#Char}
TupleListStr [] = ""
TupleListStr [t:ts] = TupleStr t +++ TupleListStr ts

InitGameBitmap :: !BID !{#Char} !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
InitGameBitmap id name width height blockwidth blockheight tb
    = OSInitGameBitmap id name width height blockwidth blockheight tb

GameBitmapDone :: !BID !*OSToolbox -> (!GRESULT, !*OSToolbox)
GameBitmapDone id tb
    = OSGameBitmapDone id tb

ClearAllGameBitmaps :: !*OSToolbox -> (!GRESULT, !*OSToolbox)
ClearAllGameBitmaps tb
    = OSClearAllGameBitmaps tb

SetTransparentColor :: !BID !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)
SetTransparentColor id p tb
    = OSSetTransparentColor id p.x p.y tb

InitBlockSequence :: !BID !(!SEQID, ![(Int,Int)]) !*OSToolbox -> (!GRESULT, !*OSToolbox)
InitBlockSequence bid (seqid, seq) tb
    = OSInitBlockSequence bid seqid (TupleListStr seq) tb

InitGameLayerMap :: !MAPID !BID ![{#Int}] !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
InitGameLayerMap mapid bid levelmap tile tb
    = OSInitGameLayerMap mapid bid (OSIntListArrayToString levelmap)
          (MapWidth levelmap) (MapHeight levelmap) tile tb

GameLayerMapDone :: !MAPID !*OSToolbox -> (!GRESULT, !*OSToolbox)
GameLayerMapDone mapid tb
    = OSGameLayerMapDone mapid tb

RunGameEngine :: !(OSGameData .gs) !*OSToolbox -> (.gs, !*OSToolbox)
RunGameEngine gd tb
    = OSRunGameEngine gd tb

SetGameBoundMap :: !Int !Int [{#Int}] !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
SetGameBoundMap w h boundmap objstart startobjx startobjy tb
    = OSSetGameBoundMap w h (OSIntListArrayToString boundmap)
          (MapWidth boundmap) (MapHeight boundmap) objstart startobjx startobjy tb

MoveScreenTo :: !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)
MoveScreenTo p tb
    = OSMoveScreenTo p.x p.y tb

InitSpriteAnimation :: !BID ![(Int,Int)] !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
InitSpriteAnimation bid seq loop tb
    = OSInitSpriteAnimation bid (TupleListStr seq) loop tb

InitGameObject :: !ObjectCode !SubCode !Point2 !*OSToolbox -> (!GRESULT, !*OSToolbox)
InitGameObject ot st p tb
    = OSInitGameObject ot st p tb

SetObjectFocus :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
SetObjectFocus x1 y1 x2 y2 maxxv maxyv tb
    = OSSetObjectFocus x1 y1 x2 y2 maxxv maxyv tb

// modified 01/11/99
CreateUserEvent :: !Int !Int !Int !Int !Int !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
CreateUserEvent ev evpar1 evpar2 dest subdest time tb
    = OSCreateUserEvent ev evpar1 evpar2 dest subdest time tb


/*
ShowStatistic :: !Int !Int !{#Char} !Int !Colour !{#Char} !Int !Bool !Bool  !Bool !Int !Int !Colour !Int !*OSToolbox -> (!GRESULT, !*OSToolbox)
ShowStatistic x y format value color font size bold italic shadow sx sy scolor options tb
    = OSShowStatistic x y format value color font size bold italic shadow sx sy scolor options tb
*/

PlayMusic :: !{#Char} !Bool !*OSToolbox -> (!GRESULT, !*OSToolbox)
PlayMusic midifile restart tb
    = OSPlayMusic midifile restart tb

StopMusic :: !*OSToolbox -> (!GRESULT, !*OSToolbox)
StopMusic tb
    = OSStopMusic tb
