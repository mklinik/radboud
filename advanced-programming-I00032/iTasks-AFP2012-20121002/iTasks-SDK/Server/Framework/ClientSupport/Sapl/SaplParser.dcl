definition module SaplParser

import SaplTokenizer, Map, Void, Error

// Cannot be abstract because code generator uses it
:: ParserState = { ps_level 		:: Int
				 , ps_constructors  :: Map SaplTerm [SaplTerm]
				 , ps_functions		:: Map SaplTerm [SaplTerm]
				 , ps_CAFs			:: Map SaplTerm Void
		      	 }

/**
* Possible function types and language constructs.
*/
:: FuncType = FTRecord SaplTerm [SaplTerm]
			| FTADT SaplTerm [SaplTerm]
			| FTCAF SaplTerm SaplTerm
			| FTMacro SaplTerm SaplTerm [SaplTerm]
			| FTFunc SaplTerm SaplTerm [SaplTerm]

:: SaplTerm = SLambda SaplTerm [SaplTerm]
			| SConst Const
			| SName String Int
			| SStrictName String
			| SApplication SaplTerm [SaplTerm]
			| SCase SaplTerm SaplTerm SaplTerm
			| SSelect [SaplTerm]
			| SAbortBody
			| SLet SaplTerm [SaplTerm]
			| SLetDefinition SaplTerm SaplTerm
			| SConstructor SaplTerm Int [SaplTerm]

instance ==	SaplTerm
instance <	SaplTerm			

:: ErrorMsg :== String

/**
* Convert a list of position wrapped tokens into a list of AST per function.
* On error an error message is provided. It is assumed that the tokens encode
* function definitions.
*
* @param a token stream
* @return an [AST] or an error message
*/			
parse :: [PosToken] -> MaybeError ErrorMsg ([FuncType], ParserState) 

/**
* Convert a list of position wrapped tokens into an AST.
* On error an error message is provided. It is assumed that the tokens encode
* a simple expression.
*
* @param a token stream
* @return an AST, ParserState pair or an error message
*/
parseExpr :: [PosToken] -> MaybeError ErrorMsg (SaplTerm, ParserState) 

/**
* Merge a ParserState record into another, by adding the elements of the Map fields
* to the Map fields of the other structure.
*
* @param first parser state
* @param second parser state
* @return merged parser state
*/
mergeParserStates :: ParserState (Maybe ParserState) -> ParserState
			
