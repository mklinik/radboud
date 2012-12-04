definition module gen

/*
	GAST: A Generic Automatic Software Test-system
	
	gen: generic generation of values of a type

	Pieter Koopman, 2004
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdGeneric

randomize :: [a] [Int] Int ([Int] -> [a]) -> [a]

generic ggen a :: Int [Int] -> [a]

derive ggen Int, Bool, Real, Char, UNIT, PAIR, EITHER, CONS, OBJECT, FIELD, (,), (,,), (,,,), [], String

maxint	:: Int
minint	:: Int
StrLen	:== 80
