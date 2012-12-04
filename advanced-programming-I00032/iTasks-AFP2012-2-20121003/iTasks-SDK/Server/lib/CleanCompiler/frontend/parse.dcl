definition module parse

import syntax, hashtable, scanner, predef

::	*ParseErrorAdmin = 
	{	pea_file	:: !*File
	,	pea_ok		:: !Bool
	}

:: *ParseState =
	{	ps_scanState		:: !ScanState
	,	ps_error			:: !*ParseErrorAdmin
	,	ps_flags			:: !Int
	,	ps_hash_table		:: !*HashTable
	}
	
PS_SkippingMask :== 1
PS_SupportGenericsMask :==2
PS_DynamicTypeUsedMask :== 4

::	ParseContext			:== Int

SetGlobalContext :: Bool -> ParseContext

cWantIclFile :== True	
cWantDclFile :== False	

wantModule :: !Bool !Ident !Position !Bool !*HashTable !*File !SearchPaths (ModTimeFunction *Files) !*Files
	-> (!Bool,!Bool,!ParsedModule, !*HashTable, !*File, !*Files)

wantDefinitions :: !ParseContext !ParseState -> (![ParsedDefinition], !ParseState)

wantExpression :: !Bool !ParseState -> (!ParsedExpr, !ParseState)

wantType :: !ParseState -> (!Type,!ParseState)

