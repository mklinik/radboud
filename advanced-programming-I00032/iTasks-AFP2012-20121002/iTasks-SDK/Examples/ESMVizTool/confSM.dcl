definition module confSM

/*
	GAST: A Generic Automatic Software Test-system
	
	ioco: Input Output COnformance of reactive systems

	Pieter Koopman, 2004-2008
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdFile
import Random, gen,/* genLibTest,*/ testable, StdMaybe

:: Spec state input output :== state input -> [Trans output state]
:: Trans output state = Pt [output] state | Ft ([output]->[state])
derive genShow Trans

:: IUTstep t i o :== t -> .(i -> .([o],t))
SpectoIUTstep :: (Spec t i o) (t i -> [[o]]) -> IUTstep (t,RandomStream) i o | genShow{|*|} t & genShow{|*|} i & genShow{|*|} o

toSpec :: (state input -> [(state,[output])]) -> Spec state input output

genLongInputs :: s (Spec s i o) [i] Int [Int] -> [[i]]
generateFSMpaths :: s (Spec s i o) [i] (s->[i]) -> [[i]] | gEq{|*|} s

:: TestOption s i o
	= Ntests Int
	| Nsequences Int
	| Seed Int
	| Randoms [Int]
	| FixedInputs [[i]]
	| InputFun (RandomStream s -> [i])
	| OnPath Int
	//	| OutputFun ([s] i -> o)
	| FSM [i] (s->[i]) // inputs state_identification
	| MkTrace Bool
	| OnTheFly
	| SwitchSpec (Spec s i o)
	| OnAndOffPath
	| ErrorFile String
	| TStop ([s] -> Bool)
	| Inconsistent ([o] [s] -> Maybe [String])

testConfSM :: [TestOption s i o] (Spec s i o) s (IUTstep .t i o) .t (.t->.t) *d -> (.t,*d)
			| FileSystem d & ggen{|*|} i & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o
//testConfSM :: [TestOption s i o] (Spec s i o) s (IUTstep .t i o) .t (.t->.t) *File *File -> (.t,*File,*File)
//			| ggen{|*|} i & gEq{|*|} s & gEq{|*|} o & genShow{|*|} s & genShow{|*|} i & genShow{|*|} o

(after) infix 0 :: [s] (Spec s i o) -> ([i] -> [s])

propDeterministic :: (Spec s i o) s i -> Bool
propTotal :: (Spec s i o) s i -> Bool
//propComplete :: (Spec s i o) s i -> Bool // equal to propTotal
propComplete spec s i :== propTotal spec s i
