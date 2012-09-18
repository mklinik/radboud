module skeleton3b

/*
  Advanced Programming 2012. 
  Skeleton for exercise 3.3 and 3.4.
  To be used in a project with the environment Everything, 
  or StdEnv with an import of StdMaybe from StdLib

  Pieter Koopman
  Peter  Achten, P.Achten@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show a = show_{|*|} a []

//------------------ parse --------------
:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

//------------------ some data types --------------

:: T    = C
:: Color  = Red | Yellow | Blue
:: Tree a = Tip | Bin a (Tree a) (Tree a)

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, []

//------------------ tests --------------

Start = testTrue

testTrue :: Result Bool
testTrue = parse{|*|} (show True)
