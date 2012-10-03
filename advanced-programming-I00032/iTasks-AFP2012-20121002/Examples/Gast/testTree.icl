module testTree

/*
	Pieter Koopman, 2010
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
	
	Run with
	- environment gast
	- "Basic Values Only" in the project options 
*/

import StdEnv, gast

:: Color  = Red | Yellow | Blue
:: Tree a = Leaf | Node (Tree a) a (Tree a)

mirror :: (Tree a) -> Tree a
mirror Leaf         	= Leaf
mirror (Node l a r)	= Node (mirror r) a (mirror l)

propMirror1 :: (Tree Color) -> Bool
propMirror1 t = mirror (mirror t) === t

propMirror2 :: (Tree Color) -> Property
propMirror2 t = not (symmetric t) ==> mirror t =!= t

symmetric Leaf         = True
symmetric (Node l _ r) = l === r && symmetric l && symmetric r

Start =
	[ Test [Quiet] propMirror1
	, Test [Quiet] propMirror2
	]

derive gEq		Color, Tree
derive genShow	Color, Tree
derive ggen		Color, Tree
derive bimap	[]

/*
Expected result:
 Passed after 1000 tests
Counterexample 1 found after 35 tests: (Node (Node Leaf Red (Node Leaf Red Leaf)) Red (Node (Node Le
af Red Leaf) Red Leaf))
1 counterexamples found, after 35 tests 5 cases rejected
*/