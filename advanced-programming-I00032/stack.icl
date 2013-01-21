implementation module stack

import genLibTest
import gen

:: Stack a :== [a]

derive genShow Stack
derive ggen Stack

newStack :: (Stack a)
newStack = []

push :: a (Stack a) -> Stack a
push x xs = [x:xs]

top :: (Stack a) -> a
top [x:xs] = x

pop :: (Stack a) -> Stack a
pop [x:xs] = xs

isEmptyStack :: (Stack a) -> Bool
isEmptyStack [] = True
isEmptyStack _ = False
