definition module BuiltInJS

import SaplTokenizer, SaplParser

/**
* Definitions of built-in and inline functions for the JavaScript code generator.
*/

import SaplParser, StringAppender, Map

:: Arity :== Int

/**
* Returns a map of function names which are built-in. For every such function
* a implementation specific name and its arity is provided.
*/
builtInFunctions :: Map String (String, Arity)

/**
* Describes a function which generates code from an AST using a specific strategy.
* (normal, force, ...)
*/
:: TermCoderFunc :== SaplTerm StringAppender -> StringAppender

/**
* Describes a function which generates inline code using the elements of its
* second argument as the arguments of the inline function.
*/
:: InlineCoderFunc :== TermCoderFunc [SaplTerm] StringAppender -> StringAppender

/**
* Returns a map of function names which are built-in and can be inlined. For every such function
* a generator function and the arity of the built-in functions is provided.
*/
inlineFunctions :: Map String (InlineCoderFunc, Arity)