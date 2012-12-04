definition module testOption

/*
	Pieter Koopman, 2010
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import MersenneTwister, confSM, StdMaybe

:: TestOption s i o
	= Ntests Int
	| Nsequences Int
	| Seed Int
	| Randoms [Int]
	| FixedInputs [[i]]
	| InputFun (RandomStream s -> [i])
	| OnPath Int
	| FSM [i] (s->[i]) // inputs state_identification
	| MkTrace Bool
	| OnTheFly
	| SwitchSpec (Spec s i o)
	| OnAndOffPath
	| ErrorFile String
	| Stop ([s] -> Bool)
	| Inconsistent ([o] [s] -> Maybe [String])