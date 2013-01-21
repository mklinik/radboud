definition module stack

import genLibTest
import gen

:: Stack a

newStack :: (Stack a)
push :: a (Stack a) -> Stack a
top :: (Stack a) -> a
pop :: (Stack a) -> Stack a
isEmptyStack :: (Stack a) -> Bool

derive genShow Stack
derive ggen Stack
