definition module List

import Functor, GenEq

unzip3			:: ![(.a,.b,.c)]			-> ([.a],[.b],[.c])
unzip4			:: ![(.a,.b,.c,.d)]			-> ([.a],[.b],[.c],[.d])
unzip5			:: ![(.a,.b,.c,.d,.e)]		-> ([.a],[.b],[.c],[.d],[.e])
replaceInList	:: !(a a -> Bool) !a ![a]	-> [a]
splitWith		:: !(a -> Bool) ![a]		-> (![a],![a])
sortByIndex		:: ![(!Int,!a)]				-> [a]
intersperse		:: !a ![a]					-> [a]
getItems		:: ![a] ![Int]				-> [a]
isMemberGen		:: !a !.[a]					-> Bool | gEq{|*|} a

instance Functor []