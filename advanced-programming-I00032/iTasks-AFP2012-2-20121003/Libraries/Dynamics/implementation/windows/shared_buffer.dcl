definition module shared_buffer

from StdDynamicLowLevelInterface import class BinaryDynamicIO, :: LazyDynamicReference;
from DynamicGraphConversion import :: EncodedDynamic2;
from DefaultElem import class DefaultElemU;

instance BinaryDynamicIO (!Int,!*StdDynamicSharedBufferInfo)

:: *StdDynamicSharedBufferInfo
instance DefaultElemU StdDynamicSharedBufferInfo

// used by StdDynamic.icl
CreateSharedBufferFromFile :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),!String)
CreateSharedBufferFromFile2 :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),(!Int,!Int))
CloseSharedBufferFromFile :: (!Int,!*StdDynamicSharedBufferInfo) -> Bool

CreateSharedBufferFromPageFile :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),!String)
CloseSharedBufferFromPageFile :: (!Int,!*StdDynamicSharedBufferInfo) -> Bool

call_debugger :: !Int -> Int


// used by dynamic rts
OpenExistingSharedBuffer :: !Int !Int -> (Bool,(!Int,!*StdDynamicSharedBufferInfo))
OpenExistingSharedBuffer2 :: (!Int,!Int) -> (Bool,(!Int,!*StdDynamicSharedBufferInfo))

CloseExistingSharedBuffer :: !(!Int,!*StdDynamicSharedBufferInfo) -> Bool
