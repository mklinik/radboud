module assignment03bKlinik

/*
    Advanced Programming 2012.
    Skeleton for exercise 3.3 and 3.4.
    To be used in a project with the environment Everything,
    or StdEnv with an import of StdMaybe from StdLib

    Pieter Koopman
    Peter  Achten, P.Achten@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe
import StdListExtensions

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show_{|UNIT|} _ c = c
show_{|CONS of {gcd_name, gcd_arity}|} showX (CONS x) c
  | gcd_arity > 0 = ["(" : gcd_name : showX x [")" : c]]
  | otherwise     = [      gcd_name : showX x c]
show_{|PAIR|} showX showY (PAIR x y) c = showX x $ showY y c
show_{|EITHER|} showL _     (LEFT  l) c = showL l c
show_{|EITHER|} _     showR (RIGHT r) c = showR r c
show_{|OBJECT|} showX (OBJECT x) c = showX x c

derive show_ Color, T, Tree, Foobar, Maybe
instance toString (Tree a) | show_{|*|} a where toString t = unwords $ show t
instance toString (Foobar a) | show_{|*|} a where toString t = unwords $ show t
instance toString (Maybe a) | show_{|*|} a where toString t = unwords $ show t

show a = show_{|*|} a []

//------------------ parse --------------
:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} [] = Nothing
parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)

parse{|Int|} [] = Nothing
parse{|Int|} [i:r] = Just (toInt i, r)

parse{|UNIT|} r = Just (UNIT, r)


// ---- ad-hoc parser combinators ---
:: Parser a :== ([String] -> Result a)

// drop the head of the list if it matches the given token, otherwise fail
match :: String -> Parser String
match token = \input = case input of
  [] = Nothing
  [head:tail] =
    if (token == head)
       (Just (head, tail))
       (Nothing)

inject :: a -> Parser a
inject a = \input = Just (a, input)

(<*>) infixl :: (Parser (a -> b)) (Parser a) -> (Parser b)
(<*>) parserAB parserA = \input =
  case parserAB input of
    Nothing = Nothing
    Just (f, restAB) = case parserA restAB of
      Nothing = Nothing
      Just (a, restA) = Just (f a, restA)

(<*) infixl :: (Parser a) (Parser b) -> (Parser a)
(<*) parserA parserB = const <$> parserA <*> parserB

(<$>) infixl :: (a -> b) (Parser a) -> (Parser b)
(<$>) f parser = inject f <*> parser

(<$) infixl :: a (Parser b) -> (Parser a)
(<$) f parser = inject f <* parser
// ---- END parser combinators ------

parse{|CONS|} _ [] = Nothing
parse{|CONS of {gcd_name, gcd_arity}|} parseA input =
  (CONS <$ chomp "(" <* match gcd_name <*> parseA <* chomp ")") input
    where
      chomp s = if (gcd_arity > 0) (match s) (inject "")

parse{|PAIR|} parseA parseB input = (PAIR <$> parseA <*> parseB) input

parse{|EITHER|} parseL parseR input =
  case parseL input of
    Nothing = case parseR input of
      Nothing = Nothing
      Just (r, rest) = Just (RIGHT r, rest)
    Just (l, rest) = Just (LEFT l, rest)

parse{|OBJECT|} parseA input = (OBJECT <$> parseA) input

derive parse Color, T, Tree, Foobar

//------------------- eq -----------------
generic eq a :: a a -> Bool

eq{|Bool|} x y = x == y
eq{|Int|} x y = x == y

eq{|UNIT|} _ _ = True
eq{|CONS|} f (CONS x) (CONS y) = f x y
eq{|PAIR|} eqx eqy (PAIR leftX leftY) (PAIR rightX rightY) = eqx leftX rightX && eqy leftY rightY
eq{|EITHER|} eql _   (LEFT  x) (LEFT  y) = eql x y
eq{|EITHER|} _   eqr (RIGHT x) (RIGHT y) = eqr x y
eq{|EITHER|} _ _ _ _ = False
eq{|OBJECT|} f (OBJECT x) (OBJECT y) = f x y

derive eq Color, T, Tree, Foobar
instance == Color where (==) x y = eq{|*|} x y
instance == (Tree a) | eq{|*|} a where (==) x y = eq{|*|} x y
instance == (Foobar a) | eq{|*|} a where (==) x y = eq{|*|} x y

//------------------ some data types --------------

:: T        = C
:: Color    = Red | Yellow | Blue
:: Tree a   = Tip | Bin a (Tree a) (Tree a)
:: Foobar a = Foobar a

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, [], T, Color

//------------------ tests --------------

//Start = runTests
  //[ Testcase "foobar" $ assert $ testTrue == (Just (True, []))
  //]


Start = runTests
    [ Testcase "parse o show for Bool" $ assert $ and $ [test True, test False]
    , Testcase "toColor o fromColor" $
        assert $ and [ c == bimap{|*|}.map_to (bimap{|*|}.map_from c) \\ c <- [Red, Yellow, Blue]]
    , Testcase "show for Color Red" $ StringList (show Red) shouldBe StringList ["Red"]
    , Testcase "show for T" $ StringList (show C) shouldBe StringList ["C"]
    , Testcase "show for Color Yellow" $ StringList (show Yellow) shouldBe StringList ["Yellow"]
    //, Testcase "show for [1]" $ StringList (show [1]) shouldBe StringList ["(", "Cons", "1", "Nil", ")"]
    , Testcase "show for T" $ StringList (show C) shouldBe StringList ["C"]
    , Testcase "show for aTree" $
        StringList (show aTree) shouldBe StringList ["(", "Bin", "2", "Tip", "(", "Bin", "4", "Tip", "Tip", ")", ")"]
    , Testcase "show for Foobar" $
        StringList (show $ Foobar 42) shouldBe StringList ["(", "Foobar", "42", ")"]
    //, Testcase "show for (1, True)" $
        //StringList (show (1, True)) shouldBe StringList ["Tuple2", "1", "True"]

    , Testcase "parse o show for Int" $ assert $ and [test i \\ i <- [-25 .. 25]]
    , Testcase "parse o show for T" $ assert $ test C
    , Testcase "parse o show for Color" $ assert $ and [test c \\ c <- [Red,Yellow,Blue]]
    , Testcase "parse o show for Tree" $ assert $ test aTree
    , Testcase "parse o show for Tree" $ assert $ test $ Foobar 42
    //, Testcase "parse o show for [Int]" $ assert $ test [1 .. 3]
    //, Testcase "parse o show for (Int, Int)" $ assert $ test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
    //, Testcase "parse o show for (Bool, [Int])" $
        //assert $ test [(a,b) \\ a <- [True, False], b <- [[1 .. 5], [10 .. 100], [-300 .. 100]]]
    ]

aTree = Bin 2 Tip (Bin 4 Tip Tip)

/**************** to test if parse and show work properly *************************/

test :: t -> Bool | eq{|*|}, show_{|*|}, parse{|*|} t
test x
    = case parse{|*|} (show x) of
        Just (y,[]) = eq{|*|} x y
        _           = False

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

// Newtype wrappers for [String] and [Int]. Because toString [String] doesn't work in Clean.
listToString :: (a -> String) [a] -> String
listToString f xs = listToString` f xs (\x = "[" +++ x)
  where
    listToString` :: (a -> String) [a] (String -> String) -> String
    listToString` _ []     c = c "]"
    listToString` f [s:[]] c = c (listToString` f [] (\x = f s +++ x))
    listToString` f [s:ss] c = c (listToString` f ss (\x = f s +++ "," +++ x))

:: StringList = StringList [String]
instance toString StringList where
  toString (StringList l) = listToString id l

instance == StringList where
  (==) (StringList ss) (StringList ts) = ss == ts

:: IntList = IntList [Int]
instance toString IntList where
  toString (IntList l) = listToString toString l

instance == IntList where
  (==) (IntList ss) (IntList ts) = ss == ts

/**************** Helper Functions *******************************/

($) infixr 0 :: .(a -> b) a -> b
($) f a = f a

unwords = concat o intersperse " "
concat = foldl (+++) ""

mapFst :: (a -> c) (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

mapPair f g (a, b) = (f a, g b)
