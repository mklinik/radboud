implementation module StdListExt

import StdList

lreplace :: (a a -> Bool) a [a] -> [a]
lreplace cond new []         = [new]
lreplace cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : lreplace cond new xs]
