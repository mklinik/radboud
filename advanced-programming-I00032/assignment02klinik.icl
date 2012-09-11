module assignment02klinik

/*
    Skeleton for Exercise 2 of Advanced Programming.
    Works fine with the environment Everything, but you can also use 
    StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
    
    Pieter Koopman, 2011
    Peter Achten,   2012 P.Achten@cs.ru.nl
*/

import StdEnv, StdMaybe

/**************** Prelude *************************/

//  Binary sums and products (in generic prelude)
:: UNIT         = UNIT
:: PAIR   a b   = PAIR a b
:: EITHER a b   = LEFT a | RIGHT b
:: CONS   a     = CONS String a

//  Generic type representations
:: ListG a  :== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a  :== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b :== CONS (PAIR a b)
:: TG       :== CONS UNIT

// Conversions
fromList :: [a] -> ListG a
fromList []     = LEFT (CONS "Nil" UNIT)
fromList [a:as] = RIGHT (CONS "Cons" (PAIR a as))

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
    Cnew      :: t a

instance Container [] where
    Cinsert   x xs  = [x:xs]
    Ccontains x xs  = any ((==) x) xs
    Cshow    []     = []
    Cshow    [x:xs] = [toString x : Cshow xs]
    Cnew            = []

instance Container Tree where
    Cinsert x Tip = Bin Tip x Tip
    Cinsert x (Bin left item right)
      | x < item  = Bin (Cinsert x left) item right
      | otherwise = Bin left             item (Cinsert x right)

    Ccontains _ Tip = False
    Ccontains x (Bin left item right)
      | x == item = True
      | x <  item = Ccontains x left
      | otherwise = Ccontains x right

    Cshow Tip = []
    Cshow (Bin left item right) = Cshow left ++ [toString item] ++ Cshow right

    Cnew          = Tip

/**************** Part 3 *******************************/
//  Example types
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

manyIntegers = [100, 2, 50, 1, 3, 17, 15, 42]
emptyContainer = Cnew
oneElementContainer = Cinsert 1 Cnew
twoElementContainer = Cinsert 2 oneElementContainer
manyElementContainer = foldl (flip Cinsert) Cnew manyIntegers


emptyList :: [Int]
emptyList =: emptyContainer

oneElementList :: [Int]
oneElementList =: oneElementContainer

twoElementList :: [Int]
twoElementList =: twoElementContainer

manyElementList :: [Int]
manyElementList =: manyElementContainer


emptyTree :: Tree Int
emptyTree =: emptyContainer

oneElementTree :: Tree Int
oneElementTree =: oneElementContainer

twoElementTree :: Tree Int
twoElementTree =: twoElementContainer

manyElementTree :: Tree Int
manyElementTree = manyElementContainer

// for testing
instance == (Tree a) | == a where
  (==) Tip Tip = True
  (==) Tip _   = False
  (==) _   Tip = False
  (==) (Bin leftX x rightX) (Bin leftY y rightY) =
    x == y && leftX == leftY && rightY == rightY

Start = runTests
    // 1 Type Constructor Classes
    // 1.1 instance []
    [ Testcase "empty list" $
        emptyList shouldBe []
    , Testcase "one-element list" $
        (Cinsert 1 Cnew) shouldBe [1]
    , Testcase "list contains 1" $
        assert $ Ccontains 1 oneElementList
    , Testcase "empty list doesn't contain 1" $
        assert $ not $ Ccontains 1 emptyList
    , Testcase "two-element list doesn't contain 3" $
        assert $ not $ Ccontains 3 twoElementList
    , Testcase "many-element list contains 17" $
        assert $ Ccontains 17 manyElementList
    , Testcase "show a two element list" $
        (Cshow twoElementList) shouldBe_ ["2", "1"]
    , Testcase "show the many-element list" $
        (foldl (+++) "" (Cshow manyElementList)) shouldBe (foldl (+++) "" $ map toString $ reverse manyIntegers)

    // 1.2 instance Tree
    , Testcase "empty tree" $
        emptyTree shouldBe_ Tip
    , Testcase "one-element tree" $
        (Cinsert 1 Cnew) shouldBe_ (Bin Tip 1 Tip)
    , Testcase "one-element tree contains 1" $
        assert $ Ccontains 1 oneElementTree
    , Testcase "empty tree doesn't contain 1" $
        assert $ not $ Ccontains 1 emptyTree
    , Testcase "many-element tree contains 17" $
        assert $ Ccontains 17 manyElementTree
    , Testcase "many-element tree doesn't contain 1000000" $
        assert $ not $ Ccontains 1000000 manyElementTree
    , Testcase "all integers are in the many-element tree" $
        assert $ and $ map (\x = Ccontains x manyElementTree) manyIntegers
    , Testcase "Cshow of two different trees with the same values on-order yields the same result" $
        foldl (+++) "" (Cshow $ Cinsert 1 $ Cinsert 2 $ Cinsert 3 emptyTree) shouldBe "123"
    , Testcase "Cshow of two different trees with the same values reverse-order yields the same result" $
        foldl (+++) "" (Cshow $ Cinsert 3 $ Cinsert 2 $ Cinsert 1 emptyTree) shouldBe "123"
    , Testcase "show the many-element tree" $
        (foldl (+++) "" (Cshow manyElementTree)) shouldBe (foldl (+++) "" $ map toString $ sort manyIntegers)
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

(shouldBe_) :: a a -> TestResult | == a
(shouldBe_) x y = assert $ x == y

assert :: Bool -> TestResult
assert True  = Passed
assert False = Failed "failed"

runTests :: [Testcase] -> String
runTests tests
  | any failed tests = unlines (map reason (filter failed tests))
  | otherwise        = "all tests passed\n"
  where
    failed (Testcase _ (Failed _)) = True
    failed (Testcase _  Passed   ) = False
    reason (Testcase description (Failed r)) = description +++ ": " +++ r
    reason (Testcase _            Passed   ) = "ok"
    unlines :: [String] -> String
    unlines xs = foldr (\x y = x +++ "\n" +++ y) "" xs

($) infixr 0 :: (a -> b) a -> b
($) f a = f a
