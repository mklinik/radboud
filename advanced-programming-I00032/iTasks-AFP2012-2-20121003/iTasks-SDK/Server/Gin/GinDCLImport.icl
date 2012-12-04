implementation module GinDCLImport

import StdEnv

import File
import FilePath
import Error
import Text
import ParserCombinators

import CleanDocParser
import GinSyntax

from general 	import 	::Optional(..)
from Heap 		import 	::Ptr

from scanner	import
						::Assoc(..),
						::Priority(..)
from syntax		import	
						::AttributeVar, 
						::AttrInequality, 
						::AType(..), 
						::ATypeVar,
						::BasicType,
						::ClassDef,
						::ConsVariable,
						::DefinedSymbol,
						::FunKind,
						::FunSpecials,
						::GenericCaseDef,
						::GenericDef,
						::Global,
						::GlobalIndex,
						::Ident(..), 
						::Import,
						::ImportedObject,
						::Index,
						::ParsedDefinition(..),
						::ParsedExpr,
						::ParsedImport,
						::ParsedInstanceAndMembers,
						::ParsedTypeDef,
						::Position,
						::Rhs,
						::RhsDefsOfType,
						::StrictnessList, 
						::SymbolPtr,
						::SymbolTableEntry,
						::SymbolType(..), 
						::TempVarId,
						::Type(..),
						::TypeAttribute(..),
						::TypeContext, 
						::TypeDef,
						::TypeKind,
						::TypeSymbIdent(..),
						::TypeSymbProperties,
						::TypeVar(..),
						::TypeVarInfo,
						::TypeVarInfoPtr,
						instance toString BasicType

importDCL :: !String !String *World -> (MaybeErrorString GModule, *World)
importDCL filename source world
# errorFilename 		= "errors"
# (ok,errorFile,world) 	= fopen errorFilename FWriteText world
| not ok 				= (Error "Failed to open errors file", world)
# (defs, errorFile) 	= parseModule source False errorFile
# (ok,world)			= fclose errorFile world
| not ok 				= (Error "Failed to close errors file", world)
# (ok, world)			= deleteFile errorFilename world

# gMod 					= mapModule (dropExtension (dropDirectory filename)) defs
= (Ok gMod, world)

mapModule :: !String [ParsedDefinition] -> GModule
mapModule name defs =
	{ GModule
	| name = name
	, types = []
	, moduleKind = GCleanModule (mapDefinitions defs)
	, imports = []
	}

mapDefinitions :: [ParsedDefinition] -> [Binding]
mapDefinitions [] = []
mapDefinitions [PD_Documentation docstr: PD_TypeSpec pos ident prio optSymbtype specials: defs]
	# res = parseFunctionComment docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyFunctionComment
	= case mapFunction doc ident prio optSymbtype of
		Just binding = [binding:mapDefinitions defs]
		Nothing = mapDefinitions defs
mapDefinitions [def:defs] = mapDefinitions defs

mapFunction :: !FunctionComment Ident Priority (Optional SymbolType) -> Maybe Binding
mapFunction _ _	_ No       = Nothing
mapFunction _ _ _ (Yes st) | not (isTask (mapAType st.st_result)) = Nothing
mapFunction doc _ _ _
	| not doc.FunctionComment.gin = Nothing
mapFunction doc ident prio (Yes st) 
	| doc.FunctionComment.parallel && isJust doc.FunctionComment.title && isJust doc.FunctionComment.icon
	= Just	( ParallelBinding	{ split =	{ GDeclaration
											| name = "_split"
											, title = Just "Parallel split"
											, description = Just "Splits a parallel branch into multiple concurrent branches"
											, formalParams = []
											, returnType = GUndefinedTypeExpression
											, returnDescription = Nothing
											, icon = Just "parallel-split"
											, shape = GDefaultShape
											}
								, merge =	{ GDeclaration
											| name = ident.id_name
											, title = doc.FunctionComment.title
											, description = doc.FunctionComment.description
											, formalParams = []
											, returnType = GUndefinedTypeExpression
											, returnDescription = Nothing
											, icon = doc.FunctionComment.icon
											, shape = GDefaultShape
											}
								, type = mapAType st.st_result
								, fixedNrBranches = case prio of
									NoPrio   = Nothing
									Prio _ _ = Just 2
								, parameterMap = case prio of
									NoPrio         = App [Var ident.id_name,Extension PBBranchList]
									Prio assoc pri = AppInfix ident.id_name (mapAssoc assoc) pri (Extension (PBBranch 0)) (Extension (PBBranch 1))
								}
			)

mapFunction doc ident prio (Yes st)
	| not doc.FunctionComment.parallel
	= (Just	( NodeBinding	
				{ NodeBinding 
				| declaration = 
					{ GDeclaration
					| name = ident.id_name
					, title = doc.FunctionComment.title
					, description = doc.FunctionComment.description
					, formalParams = if (length doc.FunctionComment.params == length st.st_args)
						[ mapFormalParameter d p \\ d <- doc.FunctionComment.params & p <- st.st_args ]
						[ mapFormalParameter	{ ParamComment
												| name = "param" +++ toString i
												, title = Just (toString (mapAType p))
												, description = Nothing 
												, defaultValue = Nothing
												, visible = True
												} 
												p 
						  \\ p <- st.st_args & i <- [1..] ]
					, returnType = mapAType st.st_result
					, returnDescription = doc.FunctionComment.return
					, icon = doc.FunctionComment.icon
					, shape = case doc.FunctionComment.shape of
						Just filename	= GExternalShape filename
						Nothing 		= GDefaultShape 
					}				
				, parameterMap = case prio of
					NoPrio = NBPrefixApp
					Prio assoc pri = NBInfixApp (mapAssoc assoc) pri
				}
			)
		)

mapFunction _ _ _ _ = Nothing

mapAssoc :: Assoc -> AFix
mapAssoc LeftAssoc  = Infixl
mapAssoc RightAssoc = Infixr
mapAssoc NoAssoc    = Infix

mapFormalParameter :: !ParamComment AType -> GFormalParameter
mapFormalParameter doc type = 
	{ GFormalParameter
	| name			= doc.ParamComment.name
	, title			= doc.ParamComment.title
	, description 	= doc.ParamComment.description
	, type			= mapAType type
	, defaultValue	= doc.ParamComment.defaultValue
	, visible		= doc.ParamComment.visible
	}

mapAType :: AType -> GTypeExpression
mapAType atype = mapType atype.at_type

mapType :: Type -> GTypeExpression
mapType (TA ident [])  = GConstructor (ident.type_ident.id_name)
mapType (TA ident [param]) 
	| ident.type_ident.id_name == "_List" = GList (mapAType param)
mapType (TA ident params) = GTypeApplication [GConstructor ident.type_ident.id_name : map mapAType params]
mapType (TAS ident [] _)  = GConstructor (ident.type_ident.id_name)
mapType (TAS ident params _)  = GTypeApplication [GConstructor ident.type_ident.id_name : map mapAType params]
mapType (TV tv) = GTypeVariable (tv.tv_ident.id_name)
mapType (--> a b) = GFunction (mapAType a) (mapAType b)
mapType (TB bt) = GConstructor (toString bt)
mapType _ = GTypeVariable "(Unknown type)"

	

