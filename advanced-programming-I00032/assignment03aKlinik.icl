module assignment03aKlinik

/*
    Advanced Programming 2012.
    Skeleton for exercise 3.1 and 3.2.
    To be used in a project with the environment Everything,
    or StdEnv with an import of StdMaybe from StdLib

    Pieter Koopman
    Peter  Achten, P.Achten@cs.ru.nl
*/

import StdEnv, StdMaybe
import StdListExtensions

/************* showing *******************/

:: Result a :== Maybe (a,[String])

class show_0 a where show_0 :: a [String] -> [String]

instance show_0 Int  where show_0 i    c = [toString i:c]
instance show_0 Bool where show_0 b    c = [toString b:c]
instance show_0 UNIT where show_0 unit c = c

show :: a -> [String] | show_0 a
show a = show_0 a []

/**************** parsing *************************/

class parse0 a :: [String] -> Result a

instance parse0 Int
where
    parse0 [i:r] = Just (toInt i, r)
    parse0 [] = Nothing
instance parse0 Bool
where
    parse0 ["True" :r] = Just (True,  r)
    parse0 ["False":r] = Just (False, r)
    parse0 [] = Nothing
instance parse0 UNIT
where
    parse0 r = Just (UNIT, r)

/**************** Example Types and conversions *************************/

:: T        = C
:: Color    = Red | Yellow | Blue
:: Tree a   = Tip | Bin a (Tree a) (Tree a)

//  Binary sums and products (in generic prelude)
:: UNIT         = UNIT
:: PAIR   a b   = PAIR a b
:: EITHER a b   = LEFT a | RIGHT b
:: CONS   a     = CONS String a

//  Generic type representations
:: TG       :== CONS UNIT
:: ColorG   :== EITHER (EITHER (CONS UNIT) (CONS UNIT)) (CONS UNIT)
:: ListG a  :== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a  :== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b :== CONS (PAIR a b)

// CONSversions

fromT :: T                                  -> TG
fromT c                                     = CONS "C" UNIT

fromColor :: Color                          -> ColorG
fromColor Red                               = LEFT (LEFT  (CONS "Red"    UNIT))
fromColor Yellow                            = LEFT (RIGHT (CONS "Yellow" UNIT))
fromColor Blue                              =       RIGHT (CONS "Blue"   UNIT)

fromList :: [a]                             -> ListG a
fromList []                                 = LEFT  (CONS "Nil"  UNIT)
fromList [a:as]                             = RIGHT (CONS "Cons" (PAIR a as))

fromTree :: (Tree a)                        -> TreeG a
fromTree Tip                                = LEFT  (CONS "Tip" UNIT)
fromTree (Bin a l r)                        = RIGHT (CONS "Bin" (PAIR a (PAIR l r)))

fromTup :: (a,b)                            -> TupG a b
fromTup (a,b)                               = CONS "Tuple2" (PAIR a b)

toT :: TG                                   -> T
toT (CONS s UNIT)                           = C

toColor :: ColorG                           -> Color
toColor (LEFT (LEFT  (CONS _ UNIT)))        = Red
toColor (LEFT (RIGHT (CONS _ UNIT)))        = Yellow
toColor       (RIGHT (CONS _ UNIT))         = Blue

toList :: (ListG a)                         -> [a]
toList (LEFT  (CONS s UNIT))                = []
toList (RIGHT (CONS s (PAIR a as)))         = [a:as]

toTree :: (TreeG a)                         -> Tree a
toTree (LEFT  (CONS s UNIT))                = Tip
toTree (RIGHT (CONS s (PAIR a (PAIR l r)))) = Bin a l r

toTup :: (TupG a b)                         -> (a,b)
toTup (CONS s (PAIR a b))                   = (a,b)

/**************** to test if parse and show work properly *************************/

test :: t -> Bool | eq0, show_0, parse0 t
test x
    = case parse0 (show x) of
        Just (y,[]) = eq0 x y
        _           = False

/**************** equality with a class for each kind *************************/

class eq0 t ::                              t       t      -> Bool
class eq1 t :: (a a -> Bool)               (t a)   (t a)   -> Bool
class eq2 t :: (a a -> Bool) (b b -> Bool) (t a b) (t a b) -> Bool

instance eq0 UNIT           where eq0 _ _                       = True
instance eq0 Int            where eq0 n m                       = n == m

instance eq1 CONS           where eq1 f   (CONS s x) (CONS t y) = s == t && f x y

instance eq2 PAIR           where eq2 f g (PAIR a b) (PAIR x y) = f a x && g b y
instance eq2 EITHER         where eq2 f g (LEFT  x)  (LEFT  y)  = f x y
                                  eq2 f g (RIGHT x)  (RIGHT y)  = g x y
                                  eq2 f g _          _          = False

instance eq0 [a] | eq0 a    where eq0   l m = eq1 eq0 l m
instance eq1 []             where eq1 f l m = eq2 (eq1 eq0) (eq1 (eq2 f (eq1 f))) (fromList l) (fromList m)

/**************** map *************************/

class map0 t ::                    t      -> t
class map1 t :: (a -> b)          (t a)   -> t b
class map2 t :: (a -> b) (c -> d) (t a c) -> t b d

instance map0 Int           where map0 i              = i
instance map0 UNIT          where map0 UNIT           = UNIT

instance map1 CONS          where map1 f   (CONS n x) = CONS n (f x)

instance map2 PAIR          where map2 f g (PAIR x y) = PAIR  (f x) (g y)
instance map2 EITHER        where map2 f g (LEFT  x)  = LEFT  (f x)
                                  map2 f g (RIGHT y)  = RIGHT (g y)

/**************** End Prelude *************************/

/**************** please add all new code below this line *************************/

// Implement show_ and parse at least for the types
//  - Int,      [x]
//  - Bool,     [x]
//  - T,        [x]
//  - Color,    [x]
//  - [a], and  [x]
//  - Tree a,   [x]
//  - (a,b)     [x]


// ---------- equality --------------

instance eq0 Color  where
  eq0  c1 c2 = eq2 (eq2 (eq1 eq0) (eq1 eq0)) (eq1 eq0) (fromColor c1) (fromColor c2)
instance ==  Color  where
  (==) c1 c2 = eq0 c1 c2    // just to use the well-known notation...

instance eq1 Tree where
  eq1 eqa x y = eq2 (eq1 eq0) (eq1 (eq2 eqa (eq2 (eq1 eqa) (eq1 eqa)))) (fromTree x) (fromTree y)

instance eq0 (Tree a) | eq0 a where
  eq0 x y = eq1 eq0 x y
instance == (Tree a) | eq0 a where
  (==) x y = eq0 x y

instance eq2 (,) where
  eq2 eqa eqb x y = eq1 (eq2 eqa eqb) (fromTup x) (fromTup y)

instance eq0 (a, b) | eq0 a & eq0 b where
  eq0 x y = eq2 eq0 eq0 x y

instance eq0 T where
  eq0 x y = eq1 eq0 (fromT x) (fromT y)

instance eq0 Bool where
  eq0 x y = x == y

// ------- printing ------------

class show_1 t :: (a [String] -> [String]) (t a) [String] -> [String]

instance show_1 CONS where
  show_1 showA (CONS s a) c = [s : showA a c]

class show_2 t :: (a [String] -> [String]) (b [String] -> [String]) (t a b) [String] -> [String]

instance show_2 PAIR where
  show_2 showA showB (PAIR a b) c = showA a $ showB b c

instance show_2 EITHER where
  show_2 showA _     (LEFT  a) c = showA a c
  show_2 _     showB (RIGHT b) c = showB b c

instance show_0 T where
  show_0 t c = show_1 show_0 (fromT t) c

instance show_0 Color where
  show_0 color c =
    show_2 (show_2 (show_1 show_0) (show_1 show_0)) (show_1 show_0) (fromColor color) c

instance show_1 [] where
  show_1 showA list c =
    show_2 (show_1 show_0) (show_1 (show_2 showA $ show_1 showA)) (fromList list) c

instance show_0 [a] | show_0 a where
  show_0 list c = show_1 show_0 list c

instance show_1 Tree where
  show_1 showA tree c =
    show_2 (show_1 show_0)
           (show_1 (show_2 showA (show_2 (show_1 showA) (show_1 showA))))
           (fromTree tree)
           c

instance show_0 (Tree a) | show_0 a where
  show_0 tree c = show_1 show_0 tree c
instance toString (Tree a) | show_0 a where
  toString t = unwords $ show t

instance show_2 (,) where
  show_2 showA showB tuple c = show_1 (show_2 showA showB) (fromTup tuple) c

instance show_0 (a, b) | show_0 a & show_0 b where
  show_0 tuple c = show_2 show_0 show_0 tuple c

// ------------ parsing --------------

class parse1 t :: String ([String] -> Result a) [String] -> Result (t a)

instance parse1 CONS where
  parse1 _      _        []    = Nothing
  parse1 consName parseA [name:input]
    | consName == name = case parseA input of
        Nothing = Nothing
        Just (a, rest) = Just (CONS name a, rest)
    | otherwise = Nothing

class parse2 t :: ([String] -> Result a) ([String] -> Result b) [String] -> Result (t a b)

instance parse2 PAIR where
  parse2 parseA parseB input = case parseA input of
    Nothing = Nothing
    Just (a, restA) = case parseB restA of
      Nothing = Nothing
      Just (b, restB) = Just (PAIR a b, restB)

instance parse2 EITHER where
  parse2 parseL parseR input = case parseL input of
    Just (a, restL) = Just (LEFT a, restL)
    Nothing = case parseR input of
      Just (b, restR) = Just (RIGHT b, restR)
      Nothing = Nothing

instance parse0 T where
  parse0 input = mapMaybe (mapFst toT) $ parse1 "C" parse0 input

instance parse0 Color where
  parse0 input = mapMaybe (mapFst toColor) $
    parse2 (parse2 (parse1 "Red" parse0)
                   (parse1 "Yellow" parse0))
           (parse1 "Blue" parse0)
           input

instance parse1 [] where
  parse1 name parseA input = mapMaybe (mapFst toList) $
    parse2 (parse1 "Nil" parse0)
           (parse1 "Cons" (parse2 parseA $ parse1 name parseA))
           input

instance parse0 [a] | parse0 a where
  parse0 input = parse1 "" parse0 input

instance parse1 Tree where
  parse1 name parseA input = mapMaybe (mapFst toTree) $
    parse2 (parse1 "Tip" parse0)
           (parse1 "Bin" (parse2 parseA (parse2 (parse1 name parseA) (parse1 name parseA))))
           input

instance parse0 (Tree a) | parse0 a where
  parse0 input = parse1 "" parse0 input

instance parse2 (,) where
  parse2 parseA parseB input = mapMaybe (mapFst toTup) $
    parse1 "Tuple2" (parse2 parseA parseB) input

instance parse0 (a, b) | parse0 a & parse0 b where
  parse0 input = parse2 parse0 parse0 input

// ------------ mapping -----------------

instance map1 [] where
  map1 f x = toList $ map2 (map1 map0) (map1 (map2 f (map1 f))) $ fromList x

instance map1 Tree where
  map1 f x = toTree $ map2 (map1 map0) (map1 (map2 f (map2 (map1 f) (map1 f)))) $ fromTree x

instance map2 (,) where
  map2 f g x = toTup $ map1 (map2 f g) $ fromTup x

Start = runTests
    [ Testcase "toColor o fromColor" $
        assert $ and [ c == toColor (fromColor c) \\ c <- [Red, Yellow, Blue]]
    , Testcase "show for Color Red" $ StringList (show Red) shouldBe StringList ["Red"]
    , Testcase "show for Color Yellow" $ StringList (show Yellow) shouldBe StringList ["Yellow"]
    , Testcase "show for [1]" $ StringList (show [1]) shouldBe StringList ["Cons", "1", "Nil"]
    , Testcase "show for T" $ StringList (show C) shouldBe StringList ["C"]
    , Testcase "show for aTree" $
        StringList (show aTree) shouldBe StringList ["Bin", "2", "Tip", "Bin", "4", "Tip", "Tip"]
    , Testcase "show for (1, True)" $
        StringList (show (1, True)) shouldBe StringList ["Tuple2", "1", "True"]

    , Testcase "parse o show for Int" $ assert $ and [test i \\ i <- [-25 .. 25]]
    , Testcase "parse o show for Bool" $ assert $ and $ [test True, test False]
    , Testcase "parse o show for T" $ assert $ test C
    , Testcase "parse o show for Color" $ assert $ and [test c \\ c <- [Red,Yellow,Blue]]
    , Testcase "parse o show for [Int]" $ assert $ test [1 .. 3]
    , Testcase "parse o show for Tree Int" $ assert $ test aTree
    , Testcase "parse o show for (Int, Int)" $ assert $ test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
    , Testcase "parse o show for (Bool, [Int])" $
        assert $ test [(a,b) \\ a <- [True, False], b <- [[1 .. 5], [10 .. 100], [-300 .. 100]]]

    // maps

    // Use these deﬁnitions to map the factorial function over the expressions
    //  - [1 . . 10] ,
    //  - Bin 2 Tip (Bin 4 Tip Tip)
    //  - ([1 . . 10], Bin 2 Tip (Bin 4 Tip Tip)) .
    , Testcase "map +1 over a list of integers" $ map1 ((+) 1) [0 .. 5] shouldBe [1 .. 6]
    , Testcase "map the factorial function over [1 .. 10]" $
        (IntList $ map1 fac [1 .. 10]) shouldBe IntList [1,2,6,24,120,720,5040,40320,362880,3628800]
    , Testcase "map the factorial function over a tree" $
        map1 fac aTree shouldBe (Bin 2 Tip (Bin 24 Tip Tip))
    , Testcase "map the factorial function over ([1 . . 10], Bin 2 Tip (Bin 4 Tip Tip))" $
        assert $ map2 (map1 fac) (map1 fac) ([1 .. 10], aTree)
          == ([1,2,6,24,120,720,5040,40320,362880,3628800], Bin 2 Tip (Bin 24 Tip Tip))

    // Use the generic map to apply \i.(i, fac i) to all elements in the list [1 .. 10].
    , Testcase "map \i.(i, fac i) to the list [1 .. 10]" $
        assert $ map1 (\i = (i, fac i)) [1 .. 10] ==
          [ (1, 1)
          , (2, 2)
          , (3, 6)
          , (4, 24)
          , (5, 120)
          , (6, 720)
          , (7, 5040)
          , (8, 40320)
          , (9, 362880)
          , (10, 3628800)
          ]
    ]


aTree = Bin 2 Tip (Bin 4 Tip Tip)

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

($) infixr 0 :: (a -> b) a -> b
($) f a = f a

fac 0 = 1
fac n = n * fac (n - 1)

unwords = concat o intersperse " "
concat = foldl (+++) ""

mapFst :: (a -> c) (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)
