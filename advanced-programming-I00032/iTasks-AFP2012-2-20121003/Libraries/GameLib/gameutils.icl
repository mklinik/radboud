implementation module gameutils

map2 :: (.a *gs -> (.b, *gs)) ![.a] !*gs -> (![.b], !*gs)
map2 f [a:x] gs
     # (fa, gs) = f a gs
     # (rest, gs) = map2 f x gs
     = ([fa:rest], gs)
map2 f [] gs = ([], gs)
