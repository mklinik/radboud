module skeleton1

/*
  Course I00032 Advanced Programming 2012
  Skeleton for assignment 1
  Pieter Koopman
  Peter  Achten  (P.Achten@cs.ru.nl)
*/

import StdEnv

/**************** Prelude: *******************************/
//  Example types
:: Color  = Red | Yellow | Blue
:: Tree a = Tip | Bin a (Tree a) (Tree a)
:: Rose a = Rose a [Rose a]

//  Binary sums and products (in generic prelude)
:: UNIT     = UNIT
:: PAIR   a b = PAIR a b
:: EITHER a b = LEFT a | RIGHT b

//  Generic type representations
:: RoseG a  :== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a)  -> RoseG a
fromRose (Rose a l)   = PAIR a l

// Oerdering

::  Ordering = Smaller | Equal | Bigger

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where   // Standard ordering for Int
  (><) x y
  | x < y   = Smaller
  | x > y   = Bigger
  | otherwise = Equal

instance >< Char where    // Standard ordering for Char
  (><) x y
  | x < y   = Smaller
  | x > y   = Bigger
  | otherwise = Equal

instance >< String where  // Standard lexicographical ordering
  (><) x y
  | x < y   = Smaller
  | x > y   = Bigger
  | otherwise = Equal

instance >< Bool where    // False is smaller than True
  (><) False True  = Smaller
  (><) True  False = Bigger
  (><) _     _     = Equal

/**************** End Prelude *************************/

// for ad-hoc testing
instance == Ordering where
  (==) Smaller Smaller = True
  (==) Bigger  Bigger  = True
  (==) Equal   Equal   = True
  (==) _       _       = False

// Define instances of (><) for the types:
//  * Color
//  * Tree a
//  * Rose
//  * (,)
//  * []

instance toString Color where
  toString Blue   = "Blue"
  toString Yellow = "Yellow"
  toString Red    = "Red"

// textual ordering of constructor names
instance >< Color where
  (><) x y = toString x >< toString y


// lexicographical or:
//  if the left operand is Equal, the right operand determines the result
//  if the left operand is not Equal, the left operand is the result
(lexiOr) infixr :: Ordering Ordering -> Ordering
(lexiOr) Equal b = b
(lexiOr) a     _ = a


// generalized lexicographical ordering:
//  a Tip is always smaller than a Bin
//  if the Bin elements are not Equal, this is our result
//  if the Bin elements are Equal, let the left subtree determine the result
//  if the left subtrees are Equal, let the right subtree determine the result
instance >< (Tree a) | >< a where
  (><) Tip Tip = Equal
  (><) Tip _   = Smaller
  (><) _   Tip = Bigger
  (><) (Bin x leftX rightX) (Bin y leftY rightY) =
    (x >< y) lexiOr (leftX >< leftY) lexiOr (rightX >< rightY)


// lexicographical order:
//   the first non-equal pair of elements determines the value
instance >< [a] | >< a where
  (><) [] [] = Equal
  (><) [] _  = Smaller
  (><) _  [] = Bigger
  (><) [x:xs] [y:ys] = (x >< y) lexiOr (xs >< ys)

// lexicographical order:
//   the first non-equal pair of elements determines the value
instance >< (a, b) | >< a
                   & >< b where
  (><) (leftX, rightX) (leftY, rightY) =
    (leftX >< leftY) lexiOr (rightX >< rightY)

// if the Rose elements are not equal, we have our result
// if the Rose elemest are equal, recurse into the rose list using the [] instance
instance >< (Rose a) | >< a where
  (><) (Rose x xs) (Rose y ys) = (x >< y) lexiOr (xs >< ys)

tip :: Tree Int
tip = Tip

bin1 = Bin 1 Tip Tip
bin2 = Bin 2 Tip Tip

nil :: [Int]
nil = []

Start =
  // === Color ===
  [ (Red >< Blue) == Bigger
  , (Red >< Red) == Equal
  , (Red >< Yellow) == Smaller

  // === Tree Int ===
  , (tip >< tip) == Equal
  , (Tip >< Bin 1 Tip Tip) == Smaller
  , (Bin 1 Tip Tip >< Tip) == Bigger

  , (bin1 >< bin2) == Smaller
  , (bin2 >< bin1) == Bigger

  , (Bin 1 bin1 Tip >< Bin 1 bin1 Tip) == Equal
  , (Bin 1 bin1 Tip >< Bin 1 bin2 Tip) == Smaller // left subtree is Smaller
  , (Bin 1 bin2 Tip >< Bin 1 bin1 Tip) == Bigger // left subtree is Bigger

  , (Bin 1 Tip bin1 >< Bin 1 Tip bin2) == Smaller // right subtree is Smaller
  , (Bin 1 Tip bin2 >< Bin 1 Tip bin1) == Bigger // right subtree is Bigger

  // left subtree determines the result: Smaller
  , (Bin 1 bin1 bin2 >< Bin 1 bin2 bin1) == Smaller

  // === Tree (Tree Int) ===
  // and of course this works recursively: node items are trees
  , (Bin bin1 Tip Tip >< Bin bin2 Tip Tip) == Smaller

  // === Lists ===
  , ([] >< nil) == Equal
  , ([] >< [1]) == Smaller
  , ([1] >< []) == Bigger

  , ([1, 2, 3] >< [1..3]) == Equal
  , ([1, 2, 1] >< [1..3]) == Smaller
  , ([1, 2, 4] >< [1..3]) == Bigger

  , ([1] >< [1, 1]) == Smaller
  , ([1, 1] >< [1]) == Bigger
  , ([1, 1] >< [5]) == Smaller
  , ([5] >< [1, 1]) == Bigger

  // === Tuples ===
  , ((1, "a") >< (1, "a")) == Equal
  , ((1, "a") >< (1, "z")) == Smaller // second element determines result
  , ((42, "a") >< (1, "z")) == Bigger // first element determines result

  // === Roses ===
  , (Rose 1 [] >< Rose 1 []) == Equal
  , (Rose 1 [] >< Rose 2 []) == Smaller
  , (Rose 2 [] >< Rose 1 []) == Bigger
  , (Rose 1 [Rose 1 []] >< Rose 1 [Rose 2 []]) == Smaller
  , (Rose 1 [Rose 2 []] >< Rose 1 [Rose 1 []]) == Bigger
  ]
