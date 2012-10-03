definition module ClientOverride

import StdDynamic, SystemTypes

// The functions of this module have different implementation
// at clint side. It is achieved by excluding this module from linking.

onClient :: Bool

cast :: a -> b | TC a & TC b
cast_to_TaskValue :: a -> TaskValue b | TC a & TC b
