module exam2011_5

import StdEnv
import StdMisc

combine2With :: (a a -> a) a a -> a
combine2With f x y = f x y

Start = combine2With (/) 1 2
