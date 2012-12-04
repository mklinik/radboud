definition module LazyLinker

/**
* Lazy SAPL linker
*
* Starting from a initial expression, generates the necessary dependency
* functions recursively.
* It can use a LoaderState. Passing the same LoaderState to the subsequent
* calls, multiple referenced functions will be linked only once.

* It's lazy, because only those modules touched to which have reference
* from some functions.
*/

import StdString, StringAppender, IWorld

:: LoaderStateExt

/**
* Generate loader state:
* 1. read module lists from the appropriate directories
* 2. exclude some modules from linking by deleting their names from the module name lists:
* a) not possible to run them on the client: 
*       graph_to_sapl_string, graph_to_string_with_descriptors, dynamic_string, sapldebug
* b) there is a specialized implementation for them: 
*       Base64, SaplHtmlClientSupport
* c) we don't want to use them on the client: 
*       LazyLinker, CodeGeneratorJS
*/
generateLoaderState :: !*IWorld -> *(LoaderStateExt, !*IWorld)

/**
* Link an expression using a LoaderState
*
* @param LoaderState
* @param StringAppender as the output stream
* @param the expression to link for
* @param *IWorld for accessing referenced modules
* @return new LoaderState
* @return new output stream of generated source code with dependencies
* @return the original expression after macro substitution 
* @return *IWorld
*/
linkSaplforExprByLoaderState :: LoaderStateExt !StringAppender !String !*IWorld -> *(LoaderStateExt, !StringAppender, !String, !*IWorld)

/**
* Simplified linker
*
* @param Expression for linking
* @param *IWorld for accessing referenced modules
* @return generated source code with dependencies
* @return the original expression after macro substitution
* @return *IWorld
*/
linkSaplforExpr :: !String *IWorld -> *(!String, !String,!*IWorld)

