module assignment02klinik

/*
    Skeleton for Exercise 2 of Advanced Programming.
    Works fine with the environment Everything, but you can also use 
    StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
    
    Pieter Koopman, 2011
    Peter Achten,   2012 P.Achten@cs.ru.nl
*/

import StdEnv, StdMaybe
import StdListExtensions

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

    Cnew = Tip

/**************** Part 2 *******************************/

//      IntList :: *
//         List :: * -> *
// List IntList :: *
//         Tree :: * -> * -> *
//           T1 :: (* -> *) -> * -> *
//           T2 :: (* -> *) -> (* -> *) -> * -> *
//           T3 :: (* -> * -> *) -> * -> * -> *
//           T4 :: (* -> *) -> (* -> *) -> * -> *

/**************** Part 3 *******************************/

show :: a -> [String] | show_ a
show a = show_ a []

class show_ a where show_ :: a [String] -> [String]

instance show_ Int  where show_ i c = ["Int"  : toString i : c]
instance show_ Bool where show_ b c = ["Bool" : toString b : c]


/*
 * instances for generic representation
 */

instance show_ UNIT where show_ _ c = ["UNIT" : c]

instance show_ (CONS a) | show_ a where
  show_ (CONS name a) c = ["(" : name : show_ a [")":c]]

instance show_ (PAIR a b)
  | show_ a
  & show_ b
where
  show_ (PAIR x y) c = show_ x $ show_ y c

instance show_ (EITHER a b)
  | show_ a
  & show_ b
where
  show_ (LEFT  a) c = show_ a c
  show_ (RIGHT b) c = show_ b c


/*
 * instances for:
 *  - list
 *  - tree
 *  - tuple
 */

instance show_ [a] | show_ a where
  show_ l c = show_ (fromList l) c


fromTree :: (Tree a) -> (TreeG a)
fromTree Tip = LEFT $ CONS "Tip" UNIT
fromTree (Bin left item right) = RIGHT $ CONS "Bin" $ PAIR item $ PAIR left right

instance show_ (Tree a) | show_ a where
  show_ t c = show_ (fromTree t) c


fromTuple :: (a, b) -> (TupG a b)
fromTuple (x, y) = CONS "Tuple" $ PAIR x y

instance show_ (a, b)
  | show_ a
  & show_ b
where
  show_ t c = show_ (fromTuple t) c



/**************** Part 4 *******************************/

:: Result a = Fail | Match a [String]

// for testing
instance == (Result a) | == a where
  (==) Fail Fail = True
  (==) Fail _    = False
  (==) _    Fail = False
  (==) (Match x xs) (Match y ys) = x == y && xs == ys

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

instance parse (CONS a) | parse a where
    parse ["(" : name : a] = case parse a of
      Match parsedA [")" : rest] = Match (CONS name parsedA) rest
      _ = Fail
    parse _ = Fail

instance parse (PAIR a b)
  | parse a
  & parse b
where
  parse input = case parse input of
    Match a restA = case parse restA of
      Match b restB = Match (PAIR a b) restB
      Fail = Fail
    Fail = Fail

instance parse (EITHER a b)
  | parse a
  & parse b
where
  parse input = case parse input of
    Match a rest = Match (LEFT a) rest
    Fail = case parse input of
      Match b rest = Match (RIGHT b) rest
      Fail = Fail

(fmap) :: (a -> b) (Result a) -> (Result b)
(fmap) f Fail = Fail
(fmap) f (Match a r) = Match (f a) r

instance parse [a] | parse a where
  parse input = toList fmap parse input

toTree :: (TreeG a) -> (Tree a)
toTree (LEFT  (CONS "Tip" UNIT)) = Tip
toTree (RIGHT (CONS "Bin" (PAIR item (PAIR left right)))) = Bin left item right

instance parse (Tree a) | parse a where
  parse input = toTree fmap parse input

toTuple :: (TupG a b) -> (a, b)
toTuple (CONS "Tuple" (PAIR x y)) = (x, y)

instance parse (a, b)
  | parse a
  & parse b
where
  parse input = toTuple fmap parse input

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
        assert $ (Cshow twoElementList) == ["2", "1"]
    , Testcase "show the many-element list" $
        (concat (Cshow manyElementList)) shouldBe (concat $ map toString $ reverse manyIntegers)

    // 1.2 instance Tree
    , Testcase "empty tree" $
        assert $ emptyTree == Tip
    , Testcase "one-element tree" $
        assert $ (Cinsert 1 Cnew) == (Bin Tip 1 Tip)
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
        concat (Cshow $ Cinsert 1 $ Cinsert 2 $ Cinsert 3 emptyTree) shouldBe "123"
    , Testcase "Cshow of two different trees with the same values reverse-order yields the same result" $
        concat (Cshow $ Cinsert 3 $ Cinsert 2 $ Cinsert 1 emptyTree) shouldBe "123"
    , Testcase "show the many-element tree" $
        (concat (Cshow manyElementTree)) shouldBe (concat $ map toString $ sort manyIntegers)

    // 3 Generic Printing
    , Testcase "show the empty list" $
        unwords (show emptyList) shouldBe "( Nil UNIT )"
    , Testcase "show a one-element list" $
        unwords (show [1]) shouldBe "( Cons Int 1 ( Nil UNIT ) )"
    , Testcase "show a multi-element list" $
        unwords (show [True, False, False]) shouldBe
        "( Cons Bool True ( Cons Bool False ( Cons Bool False ( Nil UNIT ) ) ) )"

    , Testcase "show the empty tree" $
        unwords (show emptyTree) shouldBe "( Tip UNIT )"
    , Testcase "show a one-empty tree" $
        unwords (show $ Bin Tip 42 Tip) shouldBe "( Bin Int 42 ( Tip UNIT ) ( Tip UNIT ) )"

    , Testcase "show (100, 42)" $
        unwords (show (100, 42)) shouldBe "( Tuple Int 100 Int 42 )"
    , Testcase "show (42, True)" $
        unwords (show (42, True)) shouldBe "( Tuple Int 42 Bool True )"
    , Testcase "show ([True, False], (100, 42))" $
        unwords (show ([True, False], (100, 42))) shouldBe
        "( Tuple ( Cons Bool True ( Cons Bool False ( Nil UNIT ) ) ) ( Tuple Int 100 Int 42 ) )"

    // 4 Generic Parsing
    // Ints and Bools
    , Testcase "parse 'Int 1' as an integer" $
        assert $ parse ["Int", "1"] == (Match 1 [])
    , Testcase "parse 'Bool True' as a bool" $
        assert $ parse ["Bool", "True"] == (Match True [])

    // Lists
    , Testcase "parse (Nil UNIT) as the empty list" $
        assert $ parse ["(", "Nil", "UNIT", ")"] == (Match emptyList [])
    , Testcase "show and parse on a one-element list must give the original value" $
        assert $ (parse $ show oneElementList) == (Match oneElementList [])
    , Testcase "show and parse on a two-element list must give the original value" $
        assert $ (parse $ show twoElementList) == (Match twoElementList [])
    , Testcase "show and parse on a many-element list must give the original value" $
        assert $ (parse $ show manyElementList) == (Match manyElementList [])

    // Trees
    , Testcase "parse (Tip UNIT) as the empty tree" $
        assert $ parse ["(", "Tip", "UNIT", ")"] == (Match emptyTree [])
    , Testcase "show and parse on a one-element tree must give the original value" $
        assert $ (parse $ show oneElementTree) == (Match oneElementTree [])
    , Testcase "show and parse on a two-element tree must give the original value" $
        assert $ (parse $ show twoElementTree) == (Match twoElementTree [])
    , Testcase "show and parse on a many-element tree must give the original value" $
        assert $ (parse $ show manyElementTree) == (Match manyElementTree [])

    // some failing parses
    , Testcase "'( (' is an invalid input for lists" $
        assert $ (parse ["(","("]) == failedIntList

    // The following would really be nice, but constructor names are ignored by
    // the parser.  This means that the empty list and the empty tree are the
    // same value in the untyped, a.k.a. stringly-typed generic domain.
    , Testcase "'(Tip UNIT)' does not parse as a [Int]" $
        assert $ (parse ["(", "Tip", "UNIT", ")"]) == failedIntList
    , Testcase "'(Nil UNIT)' does not parse as a Tree Int" $
        assert $ (parse ["(", "Nil", "UNIT", ")"]) == failedIntTree

    // Tuples
    , Testcase "'(Tuple Int 42 Bool True)' is the tuple (42, True)" $
        assert $ (parse ["(", "Tuple", "Int", "42", "Bool", "True", ")"]) == Match (42, True) []
    , Testcase "'(Cons (Tuple Int 1 Int 2) (Cons (Tuple Int 42 Int 1024) (Nil UNIT)))' is [(1, 2), (42, 1024)]" $
        assert $ (parse ["(", "Cons", "(", "Tuple", "Int",  "1", "Int",    "2", ")",
                         "(", "Cons", "(", "Tuple", "Int", "42", "Int", "1024", ")",
                         "(", "Nil", "UNIT", ")", ")", ")"])
          == Match [(1, 2), (42, 1024)] []
    ]

failedIntList :: Result [Int]
failedIntList = Fail

failedIntTree :: Result (Tree Int)
failedIntTree = Fail

/**************** Test Library *******************************/

:: Testcase = Testcase String TestResult
:: TestResult = Passed | Failed String

(shouldBe) :: a a -> TestResult
  | == a
  & toString a
(shouldBe) x y
  | x == y    = Passed
  | otherwise = Failed ("\n expected: '" +++ toString y +++
                       "'\n  but got: '" +++ toString x +++ "'")

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


/**************** Helper Functions *******************************/

($) infixr 0 :: (a -> b) a -> b
($) f a = f a

unwords = concat o intersperse " "
concat = foldl (+++) ""

instance == (Tree a) | == a where
  (==) Tip Tip = True
  (==) Tip _   = False
  (==) _   Tip = False
  (==) (Bin leftX x rightX) (Bin leftY y rightY) =
    x == y && leftX == leftY && rightY == rightY
