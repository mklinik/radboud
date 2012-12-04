definition module SaplLinkerShared

/**
* Types and function definitions used by the different SAPL linker implementations
*/

import StdString, StdClass, Maybe, SaplTokenizer, StringAppender
from Map import :: Map

/**
* A line in a SAPL source file can be the following:
*/
:: LineType = LT_REDIRECT String 			  // e.g. names of data constructor redirect to their type 
            | LT_FUNC String DependencyType   
            | LT_MACRO String DependencyType

:: DependencyType = DT_NO_DEPENDENCY | DT_NEED_PROCESS [Token]

// Function name <-> LineType
:: FuncTypeMap :== Map String LineType

:: IdGenerator :== Int
:: Warnings    :== [String]

/**
* Loads a function by its name, that is: String -> Maybe LineType + side effects
*
* @param inner state
* @param function name
* @param FuncTypeMap
* ...
*/
:: LoaderFunction st :== st String FuncTypeMap *World -> *(Maybe LineType, FuncTypeMap, st, *World)
:: Loader st         :== (LoaderFunction st, st)

instance toString LineType

/**
* Appends the names of function dependencies to the list of second argument
* by the token stream of the first argument.
*
* @param token stream
* @param the list the dependencies to be appended
* @return extended dependency list
*/
generate_dependencies :: [Token] [String] -> [String]

/**
* Reads a list of modules, given by their module names, and generates mappings and 
* possibly some warning messages (e.g. one of the modules is not found).
*
* @param module name list
* @param initial FuncTypeMap (can be empty)
* @param initial message list 
* @param *World to access files 
* @return extended FuncTypeMap
* @return possibly extended message list
* @return entry function if it is found (ends with .Start)
* @return *World
*/
read_modules :: [String] FuncTypeMap Warnings !*World -> (FuncTypeMap, Warnings, Maybe String, *World)

/**
* Reads a modules, given by its name, and generates mappings and 
* possibly some warning messages (e.g. one of the modules is not found).
*
* @param module name
* @param initial FuncTypeMap (can be empty)
* @param initial message list 
* @param a numeric id which is unique between the calls of this function
* @param *World to access files 
* @return extended FuncTypeMap
* @return new numeric id to pass to the same function on next call
* @return possibly extended message list
* @return *World
*/
read_module :: !String FuncTypeMap Warnings IdGenerator !*World -> (FuncTypeMap, IdGenerator, Warnings, *World)

/**
* Generates SAPL source of a given function (by name) including its dependencies.
* This is a general function, the concrete loader logic is done by the function given in its second argument.
*
* @param initial FuncTypeMap
* @param loader function (and its current state)
* @param the function name
* @param *World
* @param output stream
* @return new FuncTypeMap (loader my changed it)
* @return loader function (and its new state)
* @return *World
* @return updated output stream
*/
generate_source :: !FuncTypeMap !(Loader st) !String !*World !StringAppender -> *(!FuncTypeMap, !(Loader st), !*World, !StringAppender)

/**
* Substitute macros in a given expression.
*
* @param initial FuncTypeMap
* @param dependencies of the expression
* @param loader function (and its current state)
* @param the expression
* @param *World
* @param output stream
* @return new FuncTypeMap (loader my changed it)
* @return loader function (and its new state)
* @return *World
* @return updated output stream
*/
substitute_macros :: !FuncTypeMap ![String] !(Loader st) !String !*World !StringAppender -> (!FuncTypeMap, !(Loader st), !*World, StringAppender)

// .gfp
sapl_module_name_extension  :: String
// .sapl
sapl_program_name_extension :: String					 

