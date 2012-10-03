module testInt

/*
	Pieter Koopman, 2010
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
	
	Run with
	- environment gast
	- "Basic Values Only" in the project options
	- 4M heap and 1M stack in the project options
*/

import gast, StdEnv

pInt1 :: Int -> Property
pInt1 i = i <> 2147483647 ==> i+1>i

pInt2 :: Int -> Property
pInt2 i = i <> -2147483648 ==> i-1<i

pInt3 :: Int Int -> Bool
pInt3 x y = x+y == y+x

pInt4 :: Int Int -> Bool
pInt4 x y = x*y == y*x

pInt5 :: Int Int -> Bool
pInt5 x y = x-y == 0-(y-x)

pInt6 :: Int Int Int -> Bool
pInt6 x y z = (x+y)+z == x+(y+z)

Start
 =	[ TestList [] 
		[ (\i.pInt1 i)
		, pInt2
		]
	, TestList []
		[ pInt3
		, pInt4
		, pInt5
		]
	, Test [] pInt6
	]

/*
Expected result:
 Passed after 1000 tests one case rejected
 Passed after 1000 tests one case rejected
 Passed after 1000 tests
 Passed after 1000 tests
 Passed after 1000 tests
 Passed after 1000 tests
*/
