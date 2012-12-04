/*
	module owner: Ronny Wichers Schreur
*/
definition module convertcases

import syntax
from trans import ::Component

:: LetVarInfo
:: LetExpressionInfo
:: RefCountsInCase
:: SplitsInCase

convertCasesOfFunctions :: !*{!Component} !Int !{# {# FunType} } !{# CommonDefs} !*{#FunDef} !*{#{# CheckedTypeDef}}
		!ImportedConstructors !*VarHeap !*TypeHeaps !*ExpressionHeap
			-> (!ImportedFunctions, !*{!Component}, !*{#FunDef}, !*{#{# CheckedTypeDef}}, !ImportedConstructors, !*VarHeap, !*TypeHeaps, !*ExpressionHeap)
