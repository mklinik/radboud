module exam2011

import StdEnv
import StdGeneric
import StdMisc



generic show a :: a [String] -> [String]

show {|Int|} i c = [toString i: c]
show {|Bool|} b c = [toString b: c]
show {|String|} s c = [s:c]
show {|UNIT|} u c=c
show {|EITHER|} sa sb (LEFT a) c = sa a c
show {|EITHER|} sa sb (RIGHT b) c = sb b c
show {|PAIR|} sa sb (PAIR a b) c = sa a (sb b c)
show {|CONS of {gcd_name, gcd_arity} |} sa (CONS a) c
  | gcd_arity > 0
  = ["(" ,gcd_name:sa a [")":c] ]
  = [gcd_name:sa a c]
show {|OBJECT|} so (OBJECT o) c = so o c




:: Parser b :== [String] -> b
generic parse a :: (a -> Parser b) b -> Parser b

parse{|String|} sc fc = \[s:rest] -> sc s rest
parse{|Int|} sc fc = \[i:rest] -> sc (toInt i) rest
parse{|UNIT|} sc fc = sc UNIT
parse{|EITHER|} parseL parseR sc fc = \input -> parseL (sc o LEFT) (parseR (sc o RIGHT) fc input) input
parse{|PAIR|} parseA parseB sc fc = \input -> parseA (\a -> \input2 -> parseB (\b -> sc (PAIR a b)) fc input2) fc input
parse{|OBJECT|} parseO sc fc = \input -> parseO (sc o OBJECT) fc input

parse{|CONS of {gcd_name}|} parseC sc fc = \[cons:rest] -> if (gcd_name == cons) (parseC (sc o CONS) fc rest) fc

:: Color = Blue | Red | Green | Fail
:: List a = Nil | Cons a (List a)
:: Option a = None | Some a
:: Foobar a b = Foobar a b

derive parse Color, List, Option, Foobar, ([])
derive show ([])

nili :: List Int
nili = Nil

nonei :: Option Int
nonei = None

nili_ :: [Int]
nili_ = []

//Start = parse{|*|} const 0 ["42"]
//Start = parse{|*|} const Fail ["Green"]
//Start = parse{|*|} const (Foobar 0 0) ["Foobar", "42", "100"]
//Start = parse{|*|} const nili ["Cons", "42", "Nil"]
//Start = parse{|*|} const nonei ["Some", "42"]
//Start = parse{|*|} const nili_ ["[", "42", "]"]

Start = show{|*|} [100] []
