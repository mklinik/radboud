definition module SaplTokenizer

import StdString

/**
* Possible token types of a SAPL program
*/
:: Token = TIdentifier String 
	     | TStrictIdentifier String
	     | TComment String 
	     | TInlineAnnotation	     
	     | TAssignmentOp	     
	     | TMacroAssignmentOp
	     | TCAFAssignmentOp	     
     	 | TLambda
     	 | TColon
     	 | TVerticalBar     	 
		 | TOpenParenthesis
 		 | TCloseParenthesis
 		 | TOpenBracket
 		 | TCloseBracket
 		 | TTypeDef
		 | TConst Const
		 | TSelectKeyword
		 | TCaseKeyword
		 | TLetKeyword
		 | TInKeyword
		 | TEndOfLine

instance toString Const	
instance toString Token

/**
* Token wrapped around position information (line and column numbers)
*/
:: PosToken = PosToken Int Int Token

// String and Char constants may contain Clean escape sequences. If the target
// language uses different escaping technique the code generator must replace the
// escape sequences
:: Const = CString String | CChar String | CInt Int | CReal Real | CBool Bool

/**
* Low level function to read a token from a given position of the input string.
* 
* @param start position (from zero)
* @param input string
* @return start position of the token (can differ from the argument)
* @return start position of next token
* @return the read token
*/
read_token :: !Int !String -> (!Int, !Int, !Token)

/**
* Convert a given SAPL string to a list of tokens.
*/
tokens              :: !String -> [Token]		// used by linker
tokensWithPositions :: !String -> [PosToken]	// used by parser

	