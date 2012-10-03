/*
	Show Wrapped Node

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module ClosureShowWrapped

import StdEnv
import ClosureWrap
import Wrap, ShowWrapped

instance showWrapped (ClosureWrappedNode arg) | showWrapped arg where
	showWrapped options (Closure descriptor args unboxed)
		=	["<" : showApplication Don`tShowParentheses (toString descriptor) args]
				++ if (unboxed > 0) [" #" +++ toString unboxed +++ ">"] [">"]
	showWrapped options (NotAClosure wrappedNode)
		=	showWrapped options wrappedNode
