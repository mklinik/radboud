module ratTest
/*
	Pieter Koopman, pieter@cs.ru.nl
	AFP, 2011
*/

import StdEnv, Rat, gast

p1 :: Property
p1 = name "Exists pos" (Exists pos)
where
	pos :: Rat -> Bool
	pos r = r>zero

p2 :: Rat Rat -> Property
p2 x y = name "p2: x+y == y+x" (x+y == y+x)

Start
 =	[ test p1
	, test p2
	]
