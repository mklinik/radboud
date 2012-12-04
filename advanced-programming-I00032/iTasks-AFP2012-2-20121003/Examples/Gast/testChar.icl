module testChar

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

pChar c = isAlpha c || toUpper c == c

Start = Test [] pChar

/*
Expected result:
 Proof: success for all arguments after 98 tests
*/