definition module Shared

import IWorld, Void, Error
from SharedDataSource import :: RWShared, :: Hash, null, ::ROShared, :: WOShared, mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly, >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError

:: ReadWriteShared r w	:== RWShared r w IWorld
:: Shared a				:== ReadWriteShared a a
:: ReadOnlyShared a		:== ReadWriteShared a Void
:: WriteOnlyShared a	:== ReadWriteShared Void a
	
// Use the value of one share as parameter for another
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1