definition module explicitimports

import syntax, checksupport

:: ImportNrAndIdents =
	{	ini_symbol_nr	:: !Index
	,	ini_imp_decl	:: !ImportDeclaration
	}

:: ExplicitImport = ! {
		ei_module_n :: !Int,
		ei_position :: !Position,
		ei_symbols  :: ![ImportNrAndIdents],
		ei_qualified:: !ImportQualified
	}

:: SolvedImports =
	{	si_explicit				:: ![([Declaration], Position)]
	,	si_qualified_explicit	:: ![([Declaration], ModuleN, Position)]
	,	si_implicit				:: ![(ModuleN, Position)]
	}

markExplImpSymbols :: !Int !*(!*ExplImpInfos,!*SymbolTable) -> (!.[Ident],!(!*ExplImpInfos,!*SymbolTable))

updateExplImpForMarkedSymbol :: !Index !Declaration !SymbolTableEntry !u:{#DclModule} !*ExplImpInfos !*SymbolTable
																  -> (!u:{#DclModule},!*ExplImpInfos,!*SymbolTable)

solveExplicitImports :: !(IntKeyHashtable [ExplicitImport]) !{#Int} !Index 
								!*(!v:{#DclModule},!*{#Int},!{!*ExplImpInfo},!*CheckState)
			-> (!.SolvedImports,! (!v:{#DclModule},!.{#Int},!{!.ExplImpInfo},!.CheckState))

checkExplicitImportCompleteness :: ![([Declaration], Position)] ![([Declaration], Int, Position)]
										!*{#DclModule} !*{#*{#FunDef}} !*ExpressionHeap !*CheckState
									-> (!.{#DclModule},!*{#*{#FunDef}},!.ExpressionHeap,!.CheckState)

store_qualified_explicit_imports_in_symbol_table :: ![([Declaration],Int,Position)] ![(SymbolPtr,STE_Kind)] !*SymbolTable !*{#DclModule} -> (![(SymbolPtr,STE_Kind)],!*SymbolTable,!*{#DclModule})

:: NameSpaceN:==Int

ExpressionNameSpaceN:==0
TypeNameSpaceN:==1
ClassNameSpaceN:==2
FieldNameSpaceN:==3
OtherNameSpaceN:==4

search_qualified_ident :: !Ident {#Char} !NameSpaceN !*CheckState -> (!Bool,!DeclarationRecord,!*CheckState)
search_qualified_import :: !String !SortedQualifiedImports !NameSpaceN -> (!Bool,!DeclarationRecord)
search_qualified_imports :: !String !SortedQualifiedImports !NameSpaceN -> [DeclarationRecord]

restore_module_ste_kinds_in_symbol_table :: ![(SymbolPtr,STE_Kind)] !*SymbolTable -> *SymbolTable
