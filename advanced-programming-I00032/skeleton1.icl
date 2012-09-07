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

// for ad-hoc testing (Question: can Clean derive this definition?)
instance == Ordering where
  (==) Smaller Smaller = True
  (==) Bigger  Bigger  = True
  (==) Equal   Equal   = True
  (==) _       _       = False

/*
 * 1. Define instances of (><) for the types:
 *   - Color
 *   - Tree
 *   - Rose
 *   - (,)
 *   - []
 */

/*
 * Question: can Clean derive this definition?  In console mode it obviously
 * can, but can we use this?
 */
instance toString Color where
  toString Blue   = "Blue"
  toString Yellow = "Yellow"
  toString Red    = "Red"

/*
 * textual ordering of constructor names
 */
//instance >< Color where
  //(><) x y = toString x >< toString y


/*
 * lexicographical or:
 *   - if the left operand is Equal, the right operand determines the result
 *   - if the left operand is not Equal, the left operand is the result
 */
(lexiOr) infixr :: Ordering Ordering -> Ordering
(lexiOr) Equal b = b
(lexiOr) a     _ = a


/*
 * generalized lexicographical ordering:
 *   - a Tip is always smaller than a Bin
 *   - if the Bin elements are not Equal, this is our result
 *   - if the Bin elements are Equal, compare the left subtrees
 *   - if the left subtrees are Equal, let the right subtrees determine the
 *     result
 */
//instance >< (Tree a) | >< a where
  //(><) Tip Tip = Equal
  //(><) Tip _   = Smaller
  //(><) _   Tip = Bigger
  //(><) (Bin x leftX rightX) (Bin y leftY rightY) =
    //(x >< y) lexiOr (leftX >< leftY) lexiOr (rightX >< rightY)


/*
 * lexicographical order:
 *   - the empty list is always smaller than a non-empty list
 *   - the first non-equal pair of elements (from left to right) determines the
 *     result
 */
instance >< [a] | >< a where
  (><) [] [] = Equal
  (><) [] _  = Smaller
  (><) _  [] = Bigger
  (><) [x:xs] [y:ys] = (x >< y) lexiOr (xs >< ys)

/*
 * the first non-equal pair of elements (from left to right) determines the
 * value
 */
//instance >< (a, b) | >< a
                   //& >< b where
  //(><) (leftX, rightX) (leftY, rightY) =
    //(leftX >< leftY) lexiOr (rightX >< rightY)

/*
 * - if the Rose elements are not equal, we have our result
 * - if the Rose elemest are equal, recurse into the rose list using the []
 *   instance
 */
instance >< (Rose a) | >< a where
  (><) (Rose x xs) (Rose y ys) = (x >< y) lexiOr (xs >< ys)

/*
 * 2 Generic Representation
 * 
 * 2.1 Give generic representations for the types
 *   - Color
 *   - [a]
 */

:: ColorG :== EITHER UNIT (EITHER UNIT UNIT)
:: ListG a :== EITHER UNIT (PAIR a [a])

/*
 * 2.2 Define a function listToGen that transforms lists to their generic
 * representation
 */

listToGen :: [a] -> ListG a
listToGen [] = LEFT UNIT
listToGen [x:xs] = RIGHT (PAIR x xs)

/*
 * 2.3 The complete generic representation of [1, 2, 3] is:
 * RIGHT (PAIR 1 (RIGHT (PAIR 2 (RIGHT (PAIR 3 (LEFT UNIT))))))
 *
 * This is not the same as the result of listToGen [1,2,3] because listToGen
 * only transforms the outermost constructor to generic representation:
 * (RIGHT (PAIR 1 [2,3]))
 */

/*
 * 2.4 In the implementation for lists and tuples, we want instances that have
 * the same type variable more than once:
 *
 *   instance toGen [a] (ListG a) where ...
 *
 * which for some reasen is not allowed.  We get the error message: "type
 * variable occurs more than once in an instance type".
 *
 * When selecting such an instance, the compiler would not only have to
 * substitute the type arguments, but also have to perform some sort of
 * constraints checking that the same type appears in the positions where the
 * same type variables are.  My guess: It should be possible to implement that,
 * but it hasn't been done.
 */

// does not work
/*
instance toGen ([a]) (ListG a) where
  toGen l = listToGen l
*/

/*
 * 3 Ordering via a generic representation
 *
 * 3.1 Define the instances of >< for UNIT, PAIR and EITHER. Use these to
 * implement the ordering on Color, tuples and Tree
 */

instance >< UNIT where
  (><) UNIT UNIT = Equal

instance >< (PAIR a b) | >< a & >< b
  where
    (><) (PAIR leftX rightX) (PAIR leftY rightY) =
      (leftX >< leftY) lexiOr (rightX >< rightY)

/*
 * LEFT is always smaller than RIGHT. This is a completely arbitrary decision
 * and may at some point bite us badly.
 */
instance >< (EITHER a b) | >< a & >< b
  where
    (><) (LEFT  a) (LEFT  b) = a >< b
    (><) (RIGHT a) (RIGHT b) = a >< b
    (><) (LEFT  _) (RIGHT _) = Smaller
    (><) (RIGHT _) (LEFT  _) = Bigger


colorToGen :: Color -> ColorG
colorToGen Blue   = LEFT UNIT
colorToGen Red    = RIGHT (LEFT UNIT)
colorToGen Yellow = RIGHT (RIGHT UNIT)

instance >< Color where
  (><) x y = colorToGen x >< colorToGen y


:: TupleG a b :== PAIR a b

tupleToGen :: (a, b) -> TupleG a b
tupleToGen (a, b) = PAIR a b

instance >< (a, b) | >< a
                   & >< b where
  (><) a b = tupleToGen a >< tupleToGen b


:: TreeG a :== EITHER UNIT (PAIR a (PAIR (Tree a) (Tree a)))

treeToGen :: (Tree a) -> TreeG a
treeToGen Tip = LEFT UNIT
treeToGen (Bin x left right) = RIGHT (PAIR x (PAIR left right))

instance >< (Tree a) | >< a where
  (><) a b = treeToGen a >< treeToGen b

/*
 * 3.2 Yes, the results are the same as the non-generic ones, but only because
 * we payed special attention that the order of LEFT and RIGHT corresponds with
 * the order of Nil and Cons, Tip and Bin, and the constructors of Color.
 */

/*
 * 3.3 The advantage of the generic approach is that once we define a
 * conversion from our new data type to uniform representation, we unlock all
 * algorithms written for the uniform representation so far for our new type
 * immediately.
 */

/*
 * 3.4 Die Gleichheit fordert das Nachdenken heraus durch Fragen, die sich daran
 * knuepfen und nicht ganz leicht zu beantwortend sind.
 *
 * It is a lot easier to generally define equality, as we have done in the
 * lecture, than to generally define ordering.  Everything is equal to itself,
 * and to nothing else.  Two values are equal if they have the same structure,
 * no matter what that structure is.  Nota bene, this only holds if our
 * injections are injective.  But to determine if one value is *bigger* than
 * another value, we actually need to know the creatures we're dealing with.
 * You cannot compare the size of two houses by just comparing the size of their
 * bricks, and a musical composition may sound horrible, despite it consisting
 * only of nice harmonies.  But I digress.  The point to take home here is that
 * some algorithms lend itself nicely to being generalized, while other
 * algorithms don't.
 */

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
  , (Blue >< Yellow) == Smaller

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
