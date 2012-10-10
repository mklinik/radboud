module assignment05klinik

import iTasks
import StdMisc

enterInt :: Task Int
enterInt = enterInformation "Please enter an integer" []

viewInts :: String [Int] -> Task [Int]
viewInts name value = viewInformation name [] value

chooseTask = enterChoice "Please choose one of the exercises"
  /* For some reason, radiobuttons don't work. I always get the combo box. */
  [ChooseWith ChooseFromRadioButtons fst]
  [ ("allTasks", allTasksDemo >>| rootTask)
  , ("reallyAllTasks", reallyAllTasksDemo >>| rootTask)
  , ("2-person chat", bogusLogin rootTask)
  , ("n-person chat (fixed)", rootTask)
  , ("n-person chat (dynamic)", rootTask)
  ] @ snd

bogusLogin c =
  enterInformation "Enter a username" []
    >>* [WithResult (Action "Login") always (performBogusAuthentication c)]

performBogusAuthentication :: (Task a) String -> Task a | iTask a
performBogusAuthentication c username
    = authenticateUser (Username username) (Password "") >>=
        \mbUser -> case mbUser of
          Just user   = workAs user c
          Nothing     = createUser ({ credentials = { username = Username username
                                                    , password = Password "" }
                                    , title = Nothing
                                    , roles = [] } ) >>| performBogusAuthentication c username

Start :: *World -> *World
Start world = startEngine rootTask world

rootTask :: Task Void
rootTask = chooseTask >>= id

allTasksDemo = allTasks [enterInt, enterInt, enterInt] >>=
  viewInts "Not all values may be present."

reallyAllTasksDemo = reallyAllTasks [enterInt, enterInt, enterInt] >>=
  viewInts "And now for something completely different."

reallyAllTasks :: [Task a] -> Task [a] | iTask a
reallyAllTasks tasks = parallel Void [(Embedded, const t) \\ t <- tasks]
    @ (map snd) // discard TaskTimes
    @? secretAlienTechnology
  where
    secretAlienTechnology :: (TaskValue ([TaskValue a])) -> TaskValue [a]
    secretAlienTechnology x = join (fmap (foldl (liftA2 (flip cons)) (Value [] Stable)) x)
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
