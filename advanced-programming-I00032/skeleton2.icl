module skeleton2

/*
	Skeleton for Exercise 2 of Advanced Programming.
	Works fine with the environment Everything, but you can also use 
	StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
	
	Pieter Koopman, 2011
	Peter Achten,   2012 P.Achten@cs.ru.nl
*/

import StdEnv, StdMaybe

/**************** Prelude *************************/

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)
:: TG		:== CONS UNIT

// Conversions
fromList :: [a]	-> ListG a
fromList []		= LEFT (CONS "Nil" UNIT)
fromList [a:as]	= RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS "Nil" UNIT)) = []
toList (RIGHT (CONS "Cons" (PAIR a as))) = [a:as]


/**************** End Prelude *************************/

/**************** Part 1 *******************************/

:: Tree a = Tip | Bin (Tree a) a (Tree a)

class Container t
where
	Cinsert   :: a (t a) -> t a      | <        a
	Ccontains :: a (t a) -> Bool     | <, Eq    a
	Cshow     ::   (t a) -> [String] | toString a
	Cnew	  :: t a

// Possible test:
//Start = (Ccontains 3 c,Cshow c) where c = ..

/**************** Part 3 *******************************/
//	Example types
show :: a -> [String] | show_ a
show a = show_ a []

class show_ a where show_ :: a [String] -> [String]

instance show_ Int  where show_ i c = ["Int"  : toString i : c]
instance show_ Bool where show_ b c = ["Bool" : toString b : c]

instance show_ UNIT where show_ _ c = ["UNIT" : c]

instance show_ (Tree a) | show_ a where show_ t c = ["a tree":c] // should be improved

/**************** Part 4 *******************************/
:: Result a = Fail | Match a [String]
class parse a :: [String] -> Result a

instance parse Int where
	parse ["Int",i : r]  = Match (toInt i) r
	parse _              = Fail
instance parse Bool where
	parse ["Bool",b : r] = Match (b=="True") r
	parse _              = Fail
instance parse UNIT where
	parse ["UNIT" : r]   = Match UNIT r
	parse _              = Fail

instance parse (Tree a) | parse a where parse list = Fail // should be improved

:: T = C

/**************** Starts *******************************/

Start = runTests
  [ Testcase "foobar" $
	    1 shouldBe 2
	]

// Possible tests:
//Start1 :: ([String],Result T)
//Start1 = (strings,parse strings) where strings = show C

//Start2 :: ([String],Result (Int,Bool))
//Start2 = (strings,parse strings) where strings = show (1,False)

//Start3 :: ([String],Result [Int])
//Start3 = (strings,parse strings) where strings = show l; l :: [Int]; l = [1..4]

Start4 :: ([String],Result (Tree Int))
Start4 = (strings,parse strings)
where
	strings = show t
	
	t :: Tree Int
	t = Bin (Bin Tip 2 (Bin Tip 3 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 Tip)

/**************** Test Library *******************************/
:: Testcase = Testcase String TestResult
:: TestResult = Passed | Failed String

(shouldBe) :: a a -> TestResult
  | == a
  & toString a
(shouldBe) x y
  | x == y    = Passed
  | otherwise = Failed ("expected '" +++ toString y +++ "' but got '" +++ toString x +++ "'")

runTests :: [Testcase] -> String
runTests tests
  | any failed tests = unlines (map reason (filter failed tests))
  | otherwise        = "all ok\n"
  where
    failed (Testcase _ (Failed _)) = True
    failed (Testcase _  Passed   ) = False
    reason (Testcase description (Failed r)) = description +++ ": " +++ r
    reason (Testcase _            Passed   ) = "ok"
    unlines :: [String] -> String
    unlines xs = foldr (\x y = x +++ "\n" +++ y) "" xs

($) infixr 0 :: (a -> b) a -> b
($) f a = f a
