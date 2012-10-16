module unitTestEx

import unitTest

//	Here are some of the functions that were discussed in the lecture:
rot13 :: Char -> Char
rot13 c
	| isUpper c	= f (fromChar 'A')
	| isLower c	= f (fromChar 'a')
				= c
where f b = toChar (((fromChar c - b + 13) rem 26) + b)

pow x 1 = x							// to obtain a correct implementation:
pow x n
	| n<=0	= 0						// alter result 0 to result 1
//	| isEven n						// uncomment this line and
		= y*y
//		= x * (pow x (n-1)) 		// uncomment this line
where y = pow x (n>>1)

pPow :: Int Int -> Property
pPow x n = n>=0 ==> pow x n == x^n

pRot13a :: Char -> Bool
pRot13a c = c == rot13 (rot13 c)

pRot13b :: Char -> Property
pRot13b c = isAlpha c ==> c <> rot13 c

pInc :: Int -> Bool
pInc i = inc i > i

//	These are the tests that were discussed in the lecture
Start
 = doTest ( test1 ` test2 )
// = doTest test3
// = test pInc
// = quietnm 100 6 aStream pPow
// = test pRot13a

test1 =
    testEqual "rot13 'a'" 'n' (rot13 'a')	`
	testEqual "rot13 'b'" 'o' (rot13 'b')	`
	testEqual "rot13 'c'" 'p' (rot13 'c')
test2
 =	testEqual "rot13 'A'" 'N' (rot13 'A')	`
	testEqual "rot13 'n'" 'a' (rot13 'n')	`
	testEqual "rot13 'N'" 'A' (rot13 'N')	`
	testEqual "rot13 '1'" '1' (rot13 '1')	`
	testEqual "rot13 '.'" '.' (rot13 '.')	
test3
 =	testEqual "pow 1 1"       1   (pow 1 1)		`
	testEqual "pow 1 0"       1   (pow 1 0)		`
	testEqual "pow 3 2"       9   (pow 3 2)		`
	testEqual "pow 2 3"       8   (pow 2 3)		`
	testEqual "pow 2 4"      16   (pow 2 4)		`
	testEqual "pow 3 3"      27   (pow 3 3)		`
	testEqual "pow 2 6"      64   (pow 2 6)		`
	testEqual "pow 3 6"     (3^6) (pow 3 6)		`
	testOp  "256 == pow 2 8"   256 (==) (pow 2 8)  `
	testOp "1024 == pow 2 10" 1024 (==) (pow 2 10) `
	testOp "pow 2 3 > 2" (pow 2 3) (>) 4

f 1 x y = x < y
f 2 x y = x == y
f i x y = x > y