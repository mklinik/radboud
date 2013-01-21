module exam2011_3

import StdEnv
import gast

import stack

instance == (Stack a) | == a where
  (==) left right =
        (isEmptyStack left && isEmptyStack right)
    ||  (top left == top right && pop left == pop right)

newStackIsEmpty :: Bool
newStackIsEmpty = isEmptyStack newStack

:: Three = One | Two | Three

derive genShow Three
derive ggen Three
derive bimap Three, []

foo :: (Stack Three) Three -> Property
foo s x = isEmptyStack s ==> not (isEmptyStack (push x s))

Start = test foo
