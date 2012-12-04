/*
	Examples of Debug behaviour

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
module ClosureExamples

import StdEnv
import ClosureDebug

// show function with debug options
closureShow closureOption
	=	closureDebugShowWithOptions closureOption
			[DebugMaxChars 79, DebugMaxDepth 5, DebugMaxBreadth 20]

show
	=	closureShow ClosureShow

Start
	=		// closureShow is useful for avoiding bottom values in tracing
		[	closureShow ClosureShow undef		// <undef>
			// replace closure with placeholder .*.
		,	closureShow ClosureHide undef		// .*.
			// nested closure
		,	show (id undef)						// <id <undef>>
			// non-closures behave the same as with Debug
		,	show "not a closure"				// "not a closure"
			// thunk-lifted expressions may surprise you, and this stuff is
			// certainly referentially opaque!
		,	show [17 == 3, id 17 == 3]			// <==;16 17 3>, <_f0.Start.==;16.id;16id>]
		,	closureShow ClosureEvaluate undef 	// *aborts"
		]
