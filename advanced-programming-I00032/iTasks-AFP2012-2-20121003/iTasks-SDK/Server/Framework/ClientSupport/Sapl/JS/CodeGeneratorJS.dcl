definition module CodeGeneratorJS

/**
* SAPL to JS compiler.
*
* Two kinds: 
*   1. expression compiler (an expression doesn't contain any function definition),
*	2. full compiler
*
* Because for proper expression generation the code generator has to know whether
* the dependent functions have strict entry point, the exprGenerateJS has an optional
* argument of ParserState which contains this information. The necessary ParserState
* instance is made by generateJS which is supposed to generate the function definitions
* needed by the expression.
*/

import StringAppender, Error
from SaplParser import :: ParserState

/**
* Convert SAPL function name to JS name
*
* @param Sapl fn name
* @param String appender
* @return String appender
*/
escapeName :: String StringAppender -> StringAppender

/**
* Generates JS from Sapl source
*
* @param Sapl source
* @return (JS source / error message, error)
*/
generateJS :: String -> (MaybeErrorString (StringAppender, ParserState))

/**
* Generates JS from Sapl source of sapl expression only
*
* @param souce of Sapl expression
* @return (JS source / error message, error)
*/
exprGenerateJS :: String (Maybe ParserState) -> (MaybeErrorString StringAppender)

