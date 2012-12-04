definition module gameobjectutils

//	********************************************************************************
//	Clean Standard Game library, version 1.2.2
//
//	Utilities for working with game objects	
//	Author:   Mike Wiering
//	Modified: 7 Sept 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdOverloaded
import	gameintrface_12
import	gamehandle

// make an 8-bit integer value from eight bools, bit 7..0
CompressBools :: !(!Bool, !Bool, !Bool, !Bool, !Bool, !Bool, !Bool, !Bool) -> Int

// integer value for Bool: only 0 (False) or 1 (True)
toInt01 :: !Bool -> Int

// store an GameObjectRec in the game engine
SetObjectRec :: !InstanceID !ObjectCode !GameObjectRec ![SpriteID] !*OSToolbox -> (!GRESULT, !*OSToolbox)

// load an GameObjectRec from the game engine
GetObjectRec :: !Int !*OSToolbox -> (!GRESULT, !ObjectCode, !GameObjectRec, !*OSToolbox)

// get the definition of an object by it's ObjectType
getobject :: !ObjectCode !(GameHandle .gs) -> Maybe (GameObjectHandleLS (GSt .gs))

// store the definition of an object in the game definition
putobject :: !(GameObjectHandleLS (GSt .gs)) !(GameHandle .gs) -> GameHandle .gs

// find object in tuple list
findinstance :: ![(a,b)] a -> Maybe b  | ==a

// replace object state in tuple list
updateinstance :: a b ![(a,b)] -> [(a,b)]  | ==a

// remove object from tuple list
removeinstance :: a ![(a,b)] -> [(a,b)]  | ==a

// create a DirectionSet from an integer value (0-15)
makeDirectionSet :: !Int -> DirectionSet

fromDirectionSet :: !DirectionSet -> Int

toBoundMapCode :: !(!Int,!DirectionSet) -> Int

fromBoundMapCode :: !Int -> (!Int,!DirectionSet)
