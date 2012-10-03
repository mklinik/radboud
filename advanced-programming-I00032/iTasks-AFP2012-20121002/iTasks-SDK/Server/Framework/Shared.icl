implementation module Shared

import IWorld, Void, Error, StdFunc
from SharedDataSource import :: RWShared, :: Hash, null, ::ROShared, :: WOShared, mapRead, mapWrite, mapReadWrite, mapReadError, mapWriteError, mapReadWriteError, toReadOnly, >+<, >+|, |+<, |+|, createChangeOnWriteSDS, createReadOnlySDS, createReadOnlySDSError
from SharedDataSource import >!>, >?>, :: WriteShare(..)
	
(>+>) infixl 6 :: !(ReadWriteShared r0 w0) !(r0 -> (ReadWriteShared r1 w1)) -> ReadWriteShared r1 w1
(>+>) share shareGenF = share >!> (const (Ok share), \w1 r0 -> Ok [Write w1 (shareGenF r0)]) >?> \r0 -> Ok (shareGenF r0)
	