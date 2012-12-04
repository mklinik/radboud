implementation module APIDocumentation

from StdFunc import id
import StdFile
import StdList
import StdString

import Error
import Maybe
import File
import FilePath
import Directory
import Text
import TaskState

from LaTeX import :: LaTeX (CleanCode, CleanInline, EmDash, Environment, Index, Item, NewParagraph, Paragraph, Section, Subsection), printLaTeX
from LaTeX import qualified :: LaTeX (Text)

from PPrint import class Pretty(..), ::Doc, <+>, empty, int, hsep, parens, text

import iTasks
import DocumentStore
import CleanDocParser
import CleanPrettyPrinter
	
from general 	import 	qualified ::Optional(..)
from general	import	::Bind(..), ::Env(..), ::BITVECT
from Heap 		import 	::Ptr
from scanner	import
						::Assoc(..),
						::Priority(..)
from syntax		import	
						::Annotation,
						::AttributeVar, 
						::AttrInequality, 
						::AType(..), 
						::ATypeVar,
						::BasicType(..),
						::ClassDef,
						::ConsVariable,
						::DefinedSymbol(..),
						::FunKind,
						::FunSpecials(..),
						::GenericCaseDef,
						::GenericDef,
						::GenericTypeContext(..),
						::Global(..),
						::GlobalIndex,
						::Ident(..), 
						::Import,
						::ImportedObject,
						::Index,
						::OptionalDocBlock(..),
						::ParsedConstructor(..),
						::ParsedDefinition(..),
						::ParsedExpr,
						::ParsedImport,
						::ParsedInstanceAndMembers,
						::ParsedSelector(..),
						::ParsedTypeDef,
						::Position,
						::Rhs,
						::RhsDefsOfType(..),
						::Special(..),
						::SpecialSubstitution(..),
						::StrictnessList, 
						::SymbolPtr,
						::SymbolTableEntry,
						::SymbolType(..), 
						::TCClass(..),
						::TempVarId,
						::Type(..),
						::TypeAttribute(..),
						::TypeContext(..), 
						::TypeDef(..),
						::TypeKind,
						::TypeSymbIdent(..),
						::TypeSymbProperties,
						::TypeVar(..),
						::TypeVarInfo,
						::TypeVarInfoPtr,
						::VarInfo,
						::VarInfoPtr,
						instance toString BasicType

apiDocumentationExamples :: [Workflow]
apiDocumentationExamples = 
	[ workflow	"Examples/Miscellaneous/Generate API documentation" "Generate iTasks API documentation in LaTeX format" generateTeXExample ]

generateTeXExample :: Task Void	
generateTeXExample = updateInformation "Enter API Directory:" [] (".." </> "Server" </> "API")
//	>>= \path			-> catchAll (importJSONFile (path </> settingsFile)) (\_ -> return ([],[]))
	>>= \path			-> importJSONFile (path </> settingsFile)
	>>= \(selectedFiles, selectedIdents) -> 
							findAllFiles path ".dcl"
	>>= \dclFiles 		-> 	updateMultipleChoice "Select modules to include in documentation" [] dclFiles selectedFiles
	>>= \selectedFiles 	->	sequence "Parsing modules" [ accWorldError (getIdentifiers file) id \\ file <- selectedFiles ] @ (sort o flatten)
	>>= \idents			-> 	updateMultipleChoice "Select definitions to include in documentation" [] idents selectedIdents
	>>= \selectedIdents ->	exportJSONFile (path </> settingsFile) (selectedFiles, selectedIdents)
	>>| sequence "Generating LaTeX" [ accWorldError (dclToTeX selectedIdents file) id \\ file <- selectedFiles ] @ (printLaTeX o flatten)
	>>= \tex -> createDocumentTask "iTasks_API_documentation.tex" "application/x-tex" tex
	>>= viewInformation "Download iTasks API documentation in LaTeX format" []
	>>| return Void
	where
		settingsFile =  "selectedAPIDocumentation.json"
			
findAllFiles :: !FilePath !String -> Task [FilePath]
findAllFiles path extension
	| endsWith extension path = return [path]
	| otherwise = 
		accWorldOSError (isSubDirectory path) >>= \isSubDir ->
		if isSubDir
			(accWorldOSError (readDirectory path) >>= \entries ->
			 sequence ("Searching directory " +++ path) 
				[ findAllFiles (path </> e) extension \\ e <- entries | e <> "." && e <> ".." ] @ flatten
			)
			(return [])
where
	isSubDirectory :: !String *World -> (MaybeOSError Bool, *World)
	isSubDirectory filename world
	# (res, world) = getFileInfo filename world
	| isError res = (liftError res, world)
	= (Ok (fromOk res).directory, world)

derive class iTask LaTeX

:: ModuleDoc = 
	{ ident			:: !String
	, description	:: !String
	, types			:: ![TypeDoc]
	, functions		:: ![FunctionDoc]
	}
	
:: FunctionDoc = 
	{ ident				:: !String
	, operator			:: !Maybe Doc
	, title				:: !String
	, params			:: ![ParameterDoc]
	, description		:: !String
	, returnType		:: !Doc
	, context			:: !Doc
	, returnDescription	:: !String
	, throws			:: ![String]
	}

:: ParameterDoc = 
	{ title			:: !String
	, description	:: !String
	, type			:: !Doc
	}
	
:: TypeDoc = 
	{ ident				:: !String
	, type				:: !Doc
	, description		:: !String
	, typeRhsDoc		:: !TypeRhsDoc
	}
	
:: TypeRhsDoc	= AlgebraicTypeRhsDoc AlgebraicTypeRhsDoc
				| RecordTypeRhsDoc RecordTypeRhsDoc
				| EmptyTypeRhsDoc
			
:: AlgebraicTypeRhsDoc = 
	{ constructors		:: ![ConstructorDoc]
	}
	
:: ConstructorDoc = 
	{ ident				:: !String
	, description		:: !Maybe String
	}

:: RecordTypeRhsDoc = 
	{ fields			:: ![FieldDoc]
	}

:: FieldDoc = 
	{ ident				:: !String
	, type				:: !Doc
	, description		:: !Maybe String
	}

dclToTeX :: ![String] !FilePath *World -> (MaybeErrorString [LaTeX], *World)
dclToTeX idents filename world
# (res, world) = documentDCL filename world
| isError res = (liftError res, world)
= (Ok (moduleToTeX (filterIdentifiers idents (fromOk res))), world)
where
 	filterIdentifiers :: [String] ModuleDoc -> ModuleDoc
	filterIdentifiers selected doc =
	 	{ ModuleDoc 
		| doc 
		& types		= filter (\t -> isMember ("::" +++ t.TypeDoc.ident) selected) doc.ModuleDoc.types
		, functions	= filter (\t -> isMember t.FunctionDoc.ident selected) doc.ModuleDoc.functions
		} 

moduleToTeX :: !ModuleDoc -> [LaTeX]
moduleToTeX {ModuleDoc | ident, description, types, functions} = 
	[ Section ident
	, 'LaTeX'.Text description
	]
	++ flatten (map typeDocToTeX types)
	++ flatten (map functionToTeX functions)

typeDocToTeX :: !TypeDoc -> [LaTeX]
typeDocToTeX { TypeDoc | ident, type, description, typeRhsDoc }
	=	[ Subsection (ident +++ " type")
		, Index (ident +++ " type")
		, 'LaTeX'.Text description
		, CleanCode [ prettyPrint 120 type ]
		] ++ typeRhsToTeX typeRhsDoc

typeRhsToTeX :: !TypeRhsDoc -> [LaTeX]
typeRhsToTeX (AlgebraicTypeRhsDoc { AlgebraicTypeRhsDoc | constructors })
	| and [isNothing c.ConstructorDoc.description \\ c <- constructors] = []
typeRhsToTeX (AlgebraicTypeRhsDoc { AlgebraicTypeRhsDoc | constructors }) = 
	[ Paragraph "Constructors"
	, Environment "itemize*" (flatten (map constructorToTeX constructors))
	] where
	constructorToTeX :: ConstructorDoc -> [LaTeX]
	constructorToTeX { ConstructorDoc | ident, description }
		| isNothing description = []
	constructorToTeX { ConstructorDoc | ident, description } = 
		[ Item	[ CleanInline ident
				, EmDash
				, 'LaTeX'.Text (fromJust description)
				]
		]
typeRhsToTeX (RecordTypeRhsDoc { RecordTypeRhsDoc | fields })
	| and [isNothing f.FieldDoc.description \\ f <- fields] = []
typeRhsToTeX (RecordTypeRhsDoc { RecordTypeRhsDoc | fields }) = 
	[ Paragraph "Fields"
	, Environment "itemize*" (flatten (map fieldToTeX fields))
	] where
	fieldToTeX :: FieldDoc -> [LaTeX]
	fieldToTeX { FieldDoc | ident, description }
		| isNothing description = []
	fieldToTeX { FieldDoc | ident, description } = 
		[ Item	[ CleanInline ident
				, EmDash
				, 'LaTeX'.Text (fromJust description)
				]
		]
typeRhsToTeX _ = []

functionToTeX :: !FunctionDoc -> [LaTeX]
functionToTeX {FunctionDoc | ident, operator, params, description, returnType, returnDescription, context, throws }
	# name = case operator of
			Just _  = ident +++ " operator"
			Nothing = ident
	=	[ Subsection name
		, Index name
		, 'LaTeX'.Text description
		, CleanCode
			[	prettyPrint 120
				(	(case operator of
						Just op = parens (text ident <+> op)
						Nothing = text ident
					)
					<+> text "::"
					<+> hsep [p.ParameterDoc.type \\ p <- params]
					<+> (if (isEmpty params) empty (text " -> ")) 
					<+> returnType
					<+> context
				)
		    ]
		, Paragraph "Parameters"
		, (if (isEmpty params)
			('LaTeX'.Text "(none)")
			(Environment "itemize*" (map parameterToTeX params))
		  )
		, Paragraph "Returns"
		, CleanInline (prettyPrint 120 returnType)
		, EmDash
		, 'LaTeX'.Text returnDescription
		]
		++
		(if (isEmpty throws)
			[]
			[ Paragraph "Possible exceptions"
			, (Environment "itemize*" [ Item ['LaTeX'.Text e] \\ e <- throws ])
			]			
		)

parameterToTeX :: !ParameterDoc -> LaTeX
parameterToTeX {ParameterDoc | title, type, description} = 
	Item	[ 'LaTeX'.Text title
			, CleanInline (" :: " +++ prettyPrint 120 type)
			, EmDash
			, 'LaTeX'.Text description
			]

getIdentifiers :: !FilePath *World -> (MaybeErrorString [String], *World)
getIdentifiers filename world
# (res, world) = documentDCL filename world
| isError res = (liftError res, world)
# doc = fromOk res
= (Ok (map (\t -> "::" +++ t.TypeDoc.ident ) doc.ModuleDoc.types 
       ++ map (\t -> t.FunctionDoc.ident) doc.ModuleDoc.functions
       )
  , world)
  
documentDCL :: !FilePath *World -> (MaybeErrorString ModuleDoc, *World)
documentDCL filename world
	# (res, world)				= readFile filename world
	| isError res				= (Error ("Failed to read file: " +++ toString (fromError res)), world)
	# (ok, errorFile, world) 	= fopen "errors.txt" FWriteText world
	| not ok					= (Error "Failed to open errors.txt file", world)
	# (defs, errorFile)			= parseModule (fromOk res) False errorFile 
	# (ok,world)				= fclose errorFile world
	| not ok					= (Error "Failed to close errors.txt file", world)
	# (_,world)					= deleteFile "errors.txt" world
	# res						= case defs of
									[PD_Documentation docstr:_]	= parseModuleComment docstr
									_							= Ok emptyModuleComment
	| isError res 				= (liftError res, world)
	# moduleComment 			= fromOk res
	= (Ok	{ ModuleDoc 
			| ident = dropExtension (dropDirectory filename) 
			, description = fromMaybe "" moduleComment.ModuleComment.description 
			, functions = documentFunctions defs
			, types = documentTypeDefs defs
			}
	  , world)

documentFunctions :: ![ParsedDefinition] -> [FunctionDoc]
documentFunctions [] = []
documentFunctions [PD_Documentation docstr: PD_TypeSpec pos ident prio optSymbtype specials: defs]
	# res = parseFunctionComment docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyFunctionComment
	= case documentFunction doc ident prio optSymbtype of
		Just fd = [fd:documentFunctions defs]
		Nothing = documentFunctions defs
documentFunctions [PD_TypeSpec pos ident prio optSymbtype specials: defs]
	= case documentFunction emptyFunctionComment ident prio optSymbtype of
		Just fd = [fd:documentFunctions defs]
		Nothing = documentFunctions defs
documentFunctions [def:defs] = documentFunctions defs

documentFunction :: !FunctionComment Ident Priority ('general'.Optional SymbolType) -> Maybe FunctionDoc
documentFunction doc ident prio 'general'.No = Nothing
documentFunction doc ident prio ('general'.Yes st) = Just
	{ FunctionDoc
	| ident				= ident.id_name
	, operator			= case prio of
							Prio LeftAssoc i = Just (text "infixl" <+> int i)
							Prio RightAssoc i = Just (text "infixr" <+> int i)
							Prio NoAssoc i = Just (text "infix" <+> int i)
							NoPrio = Nothing
	, title				= ""
	, params			= [documentParameter d p \\ d <- doc.FunctionComment.params & p <- st.st_args ]
	, description		= fromMaybe "" doc.FunctionComment.description
	, returnType		= printAType False st.st_result
	, returnDescription	= fromMaybe "" doc.FunctionComment.return
	, context			= pretty st.st_context
	, throws			= doc.FunctionComment.throws
	}

documentParameter :: !ParamComment AType -> ParameterDoc
documentParameter  doc type = 
	{ ParameterDoc
	| title			= fromMaybe "(No title)" doc.ParamComment.title
	, description	= fromMaybe "(No description)" doc.ParamComment.description
	, type			= printAType True type
	}

documentTypeDefs :: ![ParsedDefinition] -> [TypeDoc]
documentTypeDefs [] = []
documentTypeDefs [PD_Documentation docstr: PD_Type typedef: defs]
	# res = parseTypeComment docstr
	# doc = case res of
		Ok doc = doc
		Error err = emptyTypeComment
	= [documentTypeDef doc typedef : documentTypeDefs defs]
documentTypeDefs [PD_Type typedef: defs] = 
	[documentTypeDef emptyTypeComment typedef: documentTypeDefs defs]
documentTypeDefs [def: defs] = documentTypeDefs defs

documentTypeDef :: !TypeComment !ParsedTypeDef -> TypeDoc
documentTypeDef doc td=:{td_ident,td_rhs} = 
	{ TypeDoc
	| ident	= td_ident.id_name
	, type	= pretty td
	, description = fromMaybe "" doc.TypeComment.description
	, typeRhsDoc = documentTypeRhs td_rhs
	}

documentTypeRhs :: !RhsDefsOfType -> TypeRhsDoc
documentTypeRhs (ConsList constructors) 
	= AlgebraicTypeRhsDoc { AlgebraicTypeRhsDoc | constructors = map documentConstructor constructors } 
	where
	documentConstructor { ParsedConstructor | pc_cons_ident, pc_docblock } = 
		{ ConstructorDoc 
		| ident = pc_cons_ident.id_name
		, description = optionalToMaybe pc_docblock
		}
documentTypeRhs (SelectorList ident typeVars isBoxed selectors) 
	= RecordTypeRhsDoc { RecordTypeRhsDoc | fields = map documentField selectors } 
	where
	documentField ps=:{ ParsedSelector | ps_field_ident, ps_docblock } = 
		{ FieldDoc 
		| ident = ps_field_ident.id_name
		, type = pretty ps
		, description = optionalToMaybe ps_docblock
		}	
documentTypeRhs _ = EmptyTypeRhsDoc

optionalToMaybe :: ('general'.Optional a) -> Maybe a
optionalToMaybe ('general'.Yes x)	= Just x
optionalToMaybe 'general'.No		= Nothing

createDocumentTask :: !String !String !String -> Task Document
createDocumentTask name mime content = mkInstantTask create
where
	create taskId iworld=:{taskTime}
		# (res,iworld)	= createDocument name mime content iworld
		= (ValueResult (Value res Stable) taskTime (TaskRep (SingleTask,Nothing,[],[]) []) (TCEmpty taskId taskTime),iworld)