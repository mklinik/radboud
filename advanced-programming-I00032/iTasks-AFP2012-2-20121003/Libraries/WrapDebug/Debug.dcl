/*
	Debug functions.

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
definition module Debug

:: DebugShowFunction a :== a -> [{#Char}]

// print (show a), then evaluate b
debugBefore :: !.a !(DebugShowFunction .a) .b -> .b
// evaluate b, then print (show a)
debugAfter :: .a !(DebugShowFunction .a) !.b -> .b
// evaluate and print (show a)
debugValue :: !(DebugShowFunction .a) !.a -> .a

// generic show function
debugShowWithOptions :: [DebugShowOption] .a -> [{#Char}]

:: DebugShowOption 
	=	DebugMaxDepth !Int			// default no limit
	|	DebugMaxBreadth !Int		// default no limit
	|	DebugMaxChars !Int			// default no limit
	|	DebugTerminator !{#Char}	// default "\n"

