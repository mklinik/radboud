/*
	Show Wrapped Node

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
definition module ClosureShowWrapped

from ClosureWrap import ::ClosureWrappedNode
from ShowWrapped import class showWrapped

instance showWrapped (ClosureWrappedNode arg) | showWrapped arg
