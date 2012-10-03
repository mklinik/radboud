implementation module CleanDocParser

import StdEnv

import File
import Text
import Maybe
import Error
import ParserCombinators
import _Unsafe

import GenEq

from general import :: Optional(..)
from checksupport import ::Heaps

from Heap		import	::Heap, newHeap, ::Ptr
from hashtable	import	::HashTable, newHashTable, set_hte_mark
from predef		import	init_identifiers

from scanner	import
						::Token(..),
						::Priority(..),
						::Assoc(..),
						::ScanState(..),
						::RScanState(..),
						::Buffer(..),
						::LongToken(..),
						::FilePosition(..),
						::ScanInput(..),
						::Input(..),
						::SBuffer(..),
						::InputStream(..),
						setUseLayout,
						::ScanContext(..),
						GeneralContext,
						class nextToken(..),
						instance nextToken ScanState,
						class tokenBack(..),
						instance tokenBack ScanState,
						setUseUnderscoreIdents,
						instance == Token
						
from syntax		import
						::SymbolTable,
						::SymbolTableEntry,
						::SymbolPtr,
						::ModuleKind(..),
						::ParsedDefinition,
						::ParsedExpr(..),
						::ArrayKind,
						::TypeKind,
						::Ident(..),
						::Env,
						::FieldNameOrQualifiedFieldName,
						::Bind,
						::DynamicType,
						::CodeBinding,
						::Global,
						::FieldSymbol,
						::Sequence,
						::Qualifier,
						::LocalDefs,
						::CaseAlt,
						::ParsedSelection,
						::ParsedSelectorKind,
						::ElemAssignment,
						::FieldAssignment,
						::OptionalRecordName,
						::Position,
						::BoundExpr,
						::Type(TE),
						::BasicValue
from parse		import	
						::ParseErrorAdmin(..),
						::ParseState(..),
						::ParseContext(..),					
						SetGlobalContext,
						wantDefinitions,
						wantExpression,
						wantType,
						PS_SupportGenericsMask
						
parseModule :: !String !Bool *File -> ([ParsedDefinition], *File)
parseModule input iclmodule error 
# hash_table = newHashTable newHeap
# scanState = stringScanner input
# (ok, moduleKind, moduleName, scanState) = try_module_header iclmodule scanState
| not ok = ([], error)
# hash_table = set_hte_mark (if iclmodule 1 0) hash_table
# scanState = setUseLayout True scanState
# (_, scanState) = nextToken GeneralContext scanState
# parseContext = SetGlobalContext iclmodule
# parseState =
	{ ps_scanState = scanState
	, ps_error = { pea_file = error, pea_ok = True }
	, ps_flags = PS_SupportGenericsMask
	, ps_hash_table = hash_table
	}
# (defs,parseState) = wantDefinitions parseContext parseState
= (defs, parseState.ps_error.pea_file)

try_module_header :: !Bool !ScanState -> (!Bool,!ModuleKind,!String,!ScanState)
try_module_header is_icl_mod scanState
	# (token, scanState) = nextToken GeneralContext scanState
	| is_icl_mod && token == ImpModuleToken 
			= try_module_token MK_Module scanState
	| token == DefModuleToken
	  	= try_module_token MK_Module scanState

try_module_token :: !ModuleKind !ScanState -> (!Bool,!ModuleKind,!String,!ScanState)
try_module_token mod_type scanState
	# (token, scanState) = nextToken GeneralContext scanState
	| token == ModuleToken
		# (token, scanState) = nextToken GeneralContext scanState
		= try_module_name token mod_type scanState
		= (False, mod_type, "", tokenBack scanState)

try_module_name :: Token !ModuleKind !ScanState -> (Bool, !ModuleKind, !String, !ScanState)
try_module_name (IdentToken name) mod_type scanState
	= (True, mod_type, name, scanState)
try_module_name (UnderscoreIdentToken name) mod_type scanState
	= (True, mod_type, name, setUseUnderscoreIdents True scanState)
try_module_name token mod_type scanState
	= (False, mod_type, "", tokenBack scanState)

stringScanner :: !String -> ScanState
stringScanner input
	# lines			= split "\n" input   //TODO: dont use Text.split
	# inputStream	= foldr (\a b -> OldLine 0 (a +++ "\n") b) EmptyStream lines
	= ScanState	{	ss_input 		= Input
											{ inp_stream		= inputStream
								 			, inp_filename		= ""
								 			, inp_pos			= {fp_line = 1, fp_col = 0}
											, inp_tabsize		= 4
											}
					,	ss_offsides		=	[(1,False)] // to generate offsides between global definitions
					,	ss_scanOptions	=	0
					,	ss_tokenBuffer	=	Buffer0
					}
					
parseUnsafe :: !String (*ParseState -> (a, *ParseState)) (a -> Bool) -> Maybe a
parseUnsafe input parser isValid = accUnsafe parseUnsafe`
where
	parseUnsafe` world
	# errorFilename = "errors.txt"
	# (ok, file, world) = fopen errorFilename FWriteText world
	| not ok = (Nothing, world)
	# (exp, file) = parse input parser file
	# (ok,world) = fclose file world
	| not ok = (Nothing, world)
	# (res,world) = deleteFile errorFilename world
	| isError res = (Nothing, world)
	| not (isValid exp) = (Nothing, world)
	= (Just exp, world)

parse :: !String (*ParseState -> (a, *ParseState)) *File -> (a, *File)
parse input parser error
# hash_table = newHashTable newHeap
# scanState = stringScanner input
# hash_table = set_hte_mark 1 hash_table
# parseState =
	{ ps_scanState = scanState
	, ps_error = { pea_file = error, pea_ok = True }
	, ps_flags = PS_SupportGenericsMask
	, ps_hash_table = hash_table
	}
# (result,parseState) = parser parseState
= (result,parseState.ps_error.pea_file)

parseExpressionUnsafe :: !String -> Maybe ParsedExpr
parseExpressionUnsafe input = parseUnsafe input (wantExpression False) (\p -> case p of PE_Empty = False; _ = True)

parseTypeUnsafe :: !String -> Maybe Type
parseTypeUnsafe input = parseUnsafe input wantType (\t -> case t of TE = False; _ = True)

//Lexer for documentation blocks
:: DocToken	= ParamDocToken
			| DefaultDocToken
			| ThrowsDocToken
			| ReturnDocToken
			| TitleDocToken
			| IconDocToken
			| ShapeDocToken
			| ParallelSplitDocToken
			| ParallelDocToken
			| VisibleDocToken
			| GinDocToken
			| ColonDocToken
			| TextDocToken !String
			| NewLineDocToken
			
derive gEq DocToken
instance == DocToken
where
	(==) a b = a === b

isText :: !DocToken -> Bool
isText (TextDocToken _) = True
isText _ = False

:: LexFunction :== String Int -> Maybe (DocToken, Int)
lex :: !String -> [DocToken]
lex input = (lex` 0 0 lexFunctions)
where
	lexFunctions :: [LexFunction]
	lexFunctions	=	[ lexFixed "@param"			ParamDocToken
						, lexFixed "@default"		DefaultDocToken
						, lexFixed "@throws"		ThrowsDocToken
						, lexFixed "@return"		ReturnDocToken
						, lexFixed "@gin-title"		TitleDocToken
						, lexFixed "@gin-icon" 		IconDocToken
						, lexFixed "@gin-shape"		ShapeDocToken
						, lexFixed "@gin-parallel"	ParallelDocToken
						, lexFixed "@gin-visible"	VisibleDocToken
						, lexFixed "@gin"			GinDocToken
						, lexFixed ":"				ColonDocToken
						, lexFixed "\n*"			NewLineDocToken
						]
						
	lex` :: !Int !Int ![LexFunction] -> [DocToken]
	lex` offset start _ | offset >= size input
		= if (offset <> start) [TextDocToken (trim (input % (start, offset - 1)))] []
	lex` offset start [] = lex` (offset + 1) start lexFunctions 
	lex` offset start [f:fs]
		# text = if (offset <> start) [TextDocToken (trim (input % (start, offset - 1)))] []
		= case f input offset of
			Just (NewLineDocToken,offset) = text ++ lex` offset offset lexFunctions
			Just (token,offset) = text ++ [token : lex` offset offset lexFunctions]
			Nothing = lex` offset start fs
	 
	//Lex token of fixed size
	lexFixed chars token input offset
		| input % (offset,offset + (size chars) - 1) == chars	= Just (token, offset + size chars)
																= Nothing

parseWith :: (Parser DocToken a) !String  -> MaybeErrorString a
parseWith parser str
# doc = parser (lex str)
| isEmpty doc = Error "Parse error"
= Ok (snd (hd doc))

//Parser for module comments

:: ModuleComment = 
	{ description	:: !Maybe String
	}

emptyModuleComment :: ModuleComment
emptyModuleComment = 
	{ ModuleComment | description = Nothing }

parseModuleComment :: !String -> MaybeErrorString ModuleComment
parseModuleComment str = parseWith pModuleComment str

pModuleComment :: Parser DocToken ModuleComment
pModuleComment = begin1 pModuleComment`
where
	pModuleComment` = pText <&> \description -> yield { ModuleComment | emptyModuleComment & description = Just description }

//Parser for function comments

emptyFunctionComment :: FunctionComment
emptyFunctionComment = 
	{ FunctionComment
	| description	= Nothing
	, params		= []
	, return		= Nothing
	, throws		= []
	, gin			= True
	, title			= Nothing
	, icon			= Nothing
	, parallel		= False
	, shape			= Nothing
	}

parseFunctionComment :: !String -> MaybeErrorString FunctionComment
parseFunctionComment str = parseWith pFunctionComment str

pFunctionComment :: Parser DocToken FunctionComment
pFunctionComment = begin1 pFunctionComment`
where
	pFunctionComment` = 
		pDescription <&> \description ->
		(<*> (pParam <!> pReturn <!> pThrows <!> pGin <!> pTitle <!> pIcon <!> pShape <!> pParallel)) <&> \args ->
		yield	((seq args) (description emptyFunctionComment))
	
	pDescription = pText <&> \description ->
		yield (\doc -> { FunctionComment | doc & description = Just description })
	pParam = symbol ParamDocToken &> pText <&> \title -> symbol ColonDocToken &> pText <&> \description -> 
		(<?@> (symbol DefaultDocToken &> pText) Just Nothing) <&> \defaultValue ->
		((symbol VisibleDocToken &> pBool) <!> yield True) <&> \visible ->
		yield (\doc -> { FunctionComment | doc & params = doc.params 
		++ [{ ParamComment 
			| name 			= makeIdent title
			, title			= Just title
			, description	= Just description 
			, defaultValue	= defaultValue
			, visible		= visible
			}] })
	where
		makeIdent s = replaceSubString " " "_" (toLowerCase s)
	pReturn = symbol ReturnDocToken &> pText <&> \return ->
		yield (\doc -> { FunctionComment | doc & return = Just return })
	pThrows = symbol ThrowsDocToken &> pText <&> \throws ->
		yield (\doc -> { FunctionComment | doc & throws = doc.throws ++ [throws]})
	pGin = symbol GinDocToken &> pBool <&> \gin ->
		yield (\doc -> { FunctionComment | doc & gin = gin })
	pTitle = symbol TitleDocToken &> pText <&> \title ->
		yield (\doc -> { FunctionComment | doc & title = Just title })
	pIcon = symbol IconDocToken &> pText <&> \icon ->
		yield (\doc -> { FunctionComment | doc & icon = Just icon })
	pShape = symbol ShapeDocToken &> pText <&> \shape ->
		yield (\doc -> { FunctionComment | doc & shape = Just shape })
	pParallel = symbol ParallelDocToken &> pBool <&> \parallel -> 
		yield (\doc -> { FunctionComment | doc & parallel = parallel })

pBool = pText <&> \value -> yield (toLowerCase value == "true")
pText = satisfy isText <@ \(TextDocToken t) -> t

//Parser for Type comments

:: TypeComment = 
	{ description	:: !Maybe String
	}

emptyTypeComment :: TypeComment
emptyTypeComment = 
	{ TypeComment | description = Nothing }

parseTypeComment :: !String -> MaybeErrorString TypeComment
parseTypeComment str = parseWith pTypeComment str

pTypeComment :: Parser DocToken TypeComment
pTypeComment = begin1 pTypeComment`
where
	pTypeComment` = pText <&> \description -> yield { TypeComment | emptyTypeComment & description = Just description }

allTokens :: ScanState -> [Token]
allTokens sState
# (token, sState) = nextToken GeneralContext sState
= case token of
	EndOfFileToken = [EndOfFileToken]
	t = [t : allTokens sState]
	
Start = allTokens (stringScanner "a")