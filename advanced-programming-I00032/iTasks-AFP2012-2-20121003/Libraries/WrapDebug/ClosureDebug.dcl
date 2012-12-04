/*
	Closure Debug functions.

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
definition module ClosureDebug

/*
	Show the closures in expressions. This is still experimental and only
	tested under Windows. The names of functions in closures are only shown
	when memory profiling is enabled or when dynamics are enabled. Otherwise
	<?> is shown for the function names.
*/

// generic show function
import Debug

:: ClosureOption
	=	ClosureEvaluate	// evaluate closures (reverts to debugShowWithOptions)
	|	ClosureShow		// show closures
	|	ClosureHide		// replace closures with placeholder .*.
	
closureDebugShowWithOptions :: ClosureOption [DebugShowOption] .a -> [{#Char}]
