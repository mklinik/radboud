definition module scanner

import StdEnv, general

// RWS Proof ... ::	SearchPaths	:== [String]

:: SearchPaths = 
	{ sp_locations   :: [(String, String)]       // (module, path)
	, sp_paths       :: [String]
	}

:: ModTimeFunction f
	:== ({#Char} f -> *(!{#Char}, !f))

// ... RWS

::	*ScanState = ScanState !RScanState

::	* RScanState =
	{	ss_input		::	ScanInput
	,	ss_offsides		::	! [(Int, Bool) ]	// (column, defines newDefinition)
	,	ss_scanOptions	::	! Int
	,	ss_tokenBuffer	::	! Buffer LongToken
	}

::	* ScanInput
	=	Input			Input
	|	PushedToken		LongToken ScanInput

::	* Input =
	{	inp_stream		::	! * InputStream
	,	inp_filename	::	!String
	,	inp_pos			::	! FilePosition
	,	inp_tabsize		::	! Int
	}

::	* InputStream
	=	InFile			* File
	|	OldLine 		!Int !{#Char} !InputStream
	|	EmptyStream

::	FilePosition =
	{	fp_line			::	! Int
	,	fp_col			::	! Int
	}

instance <<< FilePosition

::	LongToken =
	{	lt_position		::	! FilePosition	// Start position of this token
	,	lt_index		::	! Int			// The index in the current line
	,	lt_token		::	! Token			// The token itself
	,	lt_context		::	! ScanContext	// The context of the token
	}

::	*Buffer x:==SBuffer x
::	SBuffer x
	=	Buffer0
	|	Buffer1 x
	|	Buffer2 x x
	|	Buffer3 x x x // buffer size is 3.

::	Token
	= 	IdentToken !.String		//		an identifier
	| 	UnderscoreIdentToken !.String//	an identifier that starts with a '_'
	| 	QualifiedIdentToken !String !.String	//	a qualified identifier
	|	IntToken !.String		//		an integer
	|	RealToken !.String		//		a real
	|	StringToken !.String	//		a string
	|	CharToken !.String		//		a character
	|	CharListToken !.String	//		a character list '{char}*'
	|	BoolToken !Bool			//		a boolean
	|	OpenToken				//		(
	|	CloseToken				//		)
	|	CurlyOpenToken			//		{
	|	CurlyCloseToken			//		}
	|	SquareOpenToken			//		[
	|	SquareCloseToken		//		]

	|	DotToken				//		.
	|	SemicolonToken			//		;
	|	ColonToken				//		:
	|	DoubleColonToken		//		::
	|	CommaToken				//		,
	|	ExclamationToken		//		!
	|	BarToken				//		|
	|	ArrowToken				//		->
	|	DoubleArrowToken		//		=>
	|	EqualToken				//		=
	|	DefinesColonToken		//		=:
	|	ColonDefinesToken		//		:==
	|	WildCardToken			//		_
	|	BackSlashToken			//		\
	|	DoubleBackSlashToken	//		\\
	|	LeftArrowToken			//		<-
	|	LeftArrowColonToken		//		<-:
	|	LeftArrowWithBarToken	//		<|-
	|	DotDotToken				//		..
	|	AndToken				//		&
	|	HashToken				//		#
	|	AsteriskToken			//		*
	|	LessThanOrEqualToken	//		<=

	|	ModuleToken				//		module
	|	ImpModuleToken			//		implementation
	|	DefModuleToken			//		definition
	|	SysModuleToken			//		system

	|	ImportToken				//		import
	|	FromToken				//		from
	|	SpecialToken			//		special
	|	ForeignToken			//		foreign

	|	IntTypeToken			//		Int
	|	CharTypeToken			//		Char
	|	RealTypeToken			//		Real
	|	BoolTypeToken			//		Bool
	|	StringTypeToken			//		String
	|	FileTypeToken			//		File
	|	WorldTypeToken			//		World
	|	ClassToken				//		class
	|	InstanceToken			//		instance
	|	OtherwiseToken			//		otherwise

	|	IfToken					//		if
	|	WhereToken				//		where
	|	WithToken				//		with
	|	CaseToken				//		case
	|	OfToken					//		of
	|	LetToken Bool			//		let!, let
	|	SeqLetToken Bool		//		Let!, Let
	|	InToken					//		in
	
	|	DynamicToken			//		dynamic
	|	DynamicTypeToken		//		Dynamic

	|	PriorityToken Priority	//		infixX N

	|	CodeToken				//		code
	|	InlineToken				//		inline
	|	CodeBlockToken [String]	//		{...}

	|	NewDefinitionToken		//		generated automatically
	|	EndGroupToken			//		generated automatically
	|	EndOfFileToken			//		end of file
	|	ErrorToken String		//		if an error occured

	| 	GenericToken			//		generic	
	| 	DeriveToken				//		derive
	|	GenericOpenToken		//		{|
	|	GenericCloseToken		//		|}
	|	GenericOfToken			//		of

	|	ExistsToken				//		E.
	|	ForAllToken				//		A.

	|	DocBlockToken String	//		/**...*/ or //*...
	
:: ScanContext
	=	GeneralContext
	|	TypeContext
	|	FunctionContext
	|	CodeContext
	| 	GenericContext

::	Assoc	= LeftAssoc | RightAssoc | NoAssoc

::	Priority = Prio Assoc Int | NoPrio
DefaultPriority :: Priority
    
class getFilename state :: !*state -> (!String,!*state)
instance getFilename ScanState

class tokenBack state :: !*state -> *state
instance tokenBack ScanState

class nextToken state :: !ScanContext !*state -> (!Token, !*state)
instance nextToken ScanState

class currentToken state :: !*state -> (!Token, !*state)
instance currentToken ScanState
/*
class insertToken state :: !Token !ScanContext !*state -> *state
instance insertToken ScanState

class replaceToken state :: !Token !*state -> *state
instance replaceToken ScanState
*/
class getPosition state :: !*state -> (!FilePosition,!*state)  // Position of current Token (or Char)
instance getPosition ScanState

openScanner :: !String !SearchPaths (ModTimeFunction *Files) !*Files -> (!Optional (ScanState, {#Char}), !*Files) // state, file time
closeScanner :: !ScanState !*Files -> *Files

setUseLayout :: !Bool !ScanState -> ScanState
UseLayout :: !ScanState -> (!Bool, !ScanState)
dropOffsidePosition :: !ScanState -> ScanState

setUseUnderscoreIdents :: !Bool !ScanState -> ScanState

isLhsStartToken :: ! Token -> Bool
isOffsideToken :: ! Token -> Bool
isEndGroupToken :: ! Token -> Bool

instance == Token

instance <<< Token

instance toString Token, Priority


/* Sjaak ... */

// instance < Priority

determinePriority :: !Priority !Priority -> Optional Bool

/* ... Sjaak */


setNoNewOffsideForSeqLetBit :: !ScanState -> ScanState

clearNoNewOffsideForSeqLetBit :: !ScanState -> ScanState
