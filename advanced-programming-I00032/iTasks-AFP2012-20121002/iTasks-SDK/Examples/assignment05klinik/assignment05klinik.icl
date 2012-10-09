module assignment05klinik

import iTasks
import StdMisc

enterInt :: Task Int
enterInt = enterInformation "enter one integer" []

viewInt :: String Int -> Task Int
viewInt name value = viewInformation name [] value

enterInts :: Task [Int]
enterInts = enterInformation "enter some integers" []

viewInts` :: String (TaskValue [Int]) -> Task [Int]
viewInts` name NoValue = viewInformation name [] []
viewInts` name (Value value _) = viewInformation name [] value

viewInts :: String [Int] -> Task [Int]
viewInts name value = viewInformation name [] value

updateInts :: (TaskValue [Int]) -> Task [Int]
updateInts NoValue = enterInts
updateInts (Value ints _) = updateInformation "enter some integers" [] ints

Start :: *World -> *World
Start world = startEngine theTask world
  where
    theTask = reallyAllTasks [enterInt, enterInt, enterInt] >>= viewInts "blah"

reallyAllTasks :: [Task a] -> Task [a] | iTask a
reallyAllTasks tasks = parallel Void [(Embedded, const t) \\ t <- tasks]
    @ (map snd) // discard TaskTimes
    @? res
  where
    res :: (TaskValue ([TaskValue a])) -> TaskValue [a]
    res x = join (fmap (foldl (liftA2 (flip cons)) (Value [] Stable)) x)
    /* In case the above line gives you a headache (it sure gives me one) here
     * is what it does, from inside to outside. As TaskValue is just a fancy
     * Maybe, let's talk about Maybe for this explanation.
     *  - foldl with the lifted list constructor turns [Maybe a] into
     *    Maybe [a], such that if any element is Nothing, the final result is
     *    Nothing.
     *  - because the list of Maybes is inside another Maybe, we must fmap
     *    the left-fold.
     *  - this leaves us with Maybe (Maybe [a]) which we flatten into Maybe [a]
     *    using the monadic vleeshamer also known as "join".
     *  - Never mind that TaskValue is probably not a proper Monad or
     *    Applicative, but this way of thinking helps.
     */

always = const True

cons x xs = [x:xs]

flip f x y = f y x

join :: (TaskValue (TaskValue a)) -> TaskValue a
join NoValue = NoValue
join (Value NoValue _) = NoValue
join (Value (Value v s) _) = Value v s // not sure about the stability

class Applicative f where
  pure :: a -> f a
  (<*>) infixl 1 :: (f (a -> b)) (f a) -> (f b)

instance Applicative TaskValue where
  pure v = Value v Stable
  (<*>) NoValue _ = NoValue
  (<*>) _ NoValue = NoValue
  (<*>) (Value f _) (Value x s) = Value (f x) s // not sure about the stability.

(<$>) infixl 2 :: (a -> b) (f a) -> f b | Applicative f
(<$>) f a = (pure f) <*> a

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f
liftA2 f x y = f <$> x <*> y
