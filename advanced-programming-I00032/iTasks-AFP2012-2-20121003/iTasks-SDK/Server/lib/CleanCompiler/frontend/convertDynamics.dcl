/*
	module owner: Ronny Wichers Schreur
*/
definition module convertDynamics

import syntax , checksupport
from trans import ::Component

:: TypeCodeVariableInfo
:: DynamicValueAliasInfo

convertDynamicPatternsIntoUnifyAppls :: !{# CommonDefs} !Int  {#DclModule} !IclModule [String] !Int !Int
		!*{!Component} !*{#FunDef} !*PredefinedSymbols !*VarHeap !*TypeHeaps !*ExpressionHeap !(Optional *File)
	-> (!*{#{#CheckedTypeDef}},
		!*{!Component},!*{#FunDef},!*PredefinedSymbols,!*VarHeap,!*TypeHeaps,!*ExpressionHeap,!(Optional *File))
