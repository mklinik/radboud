implementation module BuiltInJS

import StdEnv, Map, StringAppender

builtInFunctions :: Map String (String, Arity)
builtInFunctions = fromList
				   [("mult", ("_mult", 2))
				   ,("div", ("_div", 2))
				   ,("divreal", ("_divreal", 2))
				   ,("add", ("_add", 2))
				   ,("sub", ("_sub", 2))
				   ,("eq", ("_eq", 2))
				   ,("neq", ("_neq", 2))
				   ,("mod", ("_mod", 2))
				   ,("qt", ("_qt", 2))
				   ,("qe", ("_qe", 2))
				   ,("not", ("_not", 1))
				   ,("strlen", ("_strlen", 1))
				   ,("string_select", ("_string_select", 2))
				   ,("_string_create", ("__string_create", 1))
				   ,("string_create", ("_string_create", 2))
				   ,("string_update", ("_string_update", 3))
				   ,("string_slice", ("_string_slice", 3))
				   ,("string_append", ("_string_append", 2))
				   ,("toInt_char", ("_toInt_char", 1))
				   ,("toInt_str", ("_toInt_str", 1))
				   ,("toInt_real", ("_toInt_real", 1))
				   ,("toReal", ("_toReal", 1))
				   ,("toChar", ("_toChar", 1))
				   ,("sqrt", ("_sqrt", 1))
				   ,("bitand", ("_bitand", 2))
				   ,("shiftleft", ("_shiftleft", 2))
				   ,("shiftright", ("_shiftright", 2))
				   ,("abort", ("_abort", 1))
				   ,("error", ("_error", 1))
				   ]

inlineFunctions :: Map String (InlineCoderFunc, Arity)
inlineFunctions = fromList
				  [("add", (inline_add, 2))
				  ,("sub", (inline_sub, 2))
				  ,("mult", (inline_mult, 2))
				  ,("div", (inline_div, 2))
				  ,("divreal", (inline_divreal, 2))
				  ,("eq", (inline_eq, 2))
				  ,("neq", (inline_neq, 2))
				  ,("mod", (inline_mod, 2))
				  ,("gt", (inline_gt, 2))
				  ,("ge", (inline_ge, 2))
				  ,("not", (inline_not, 1))
				  ,("strlen", (inline_strlen, 1))
				  ,("string_select", (inline_string_select, 2))
				  ,("string_update", (inline_string_update, 3))
				  ,("string_append", (inline_string_append, 2))
				  ,("string_slice", (inline_string_slice, 3))
				  ,("toInt", (inline_toInt, 1))
				  ,("toReal", (inline_toReal, 1))
				  ,("toChar", (inline_toChar, 1))
				  ,("bitand", (inline_bitand, 2))
				  ,("shiftleft", (inline_shiftleft, 2))
				  ,("shiftright", (inline_shiftright, 2))
				  ]

a1 args :== hd (args)
a2 args :== hd (tl args)
a3 args :== hd (tl (tl args))
		
inline_add eval args a
	= a <++ eval (a1 args) <++ "+" <++ eval(a2 args)

inline_sub eval args a
	= a <++ eval (a1 args) <++ "-" <++ eval(a2 args)

inline_mult eval args a
	= a <++ eval (a1 args) <++ "*" <++ eval(a2 args)

inline_div eval args a
	= a <++ "Math.floor(" <++ eval (a1 args) <++ "/" <++ eval (a2 args) <++ ")"

inline_divreal eval args a
	= a <++ eval (a1 args) <++ "/" <++ eval (a2 args)

inline_eq eval args a
	= a <++ eval (a1 args) <++ "==" <++ eval (a2 args)

inline_neq eval args a
	= a <++ eval (a1 args) <++ "!=" <++ eval (a2 args)

inline_mod eval args a
	= a <++ eval (a1 args) <++ "%" <++ eval (a2 args)

inline_gt eval args a
	= a <++ eval (a1 args) <++ ">" <++ eval (a2 args)

inline_ge eval args a
	= a <++ eval (a1 args) <++ ">=" <++ eval (a2 args)

inline_not eval args a
	= a <++ "!" <++ eval (a1 args)

inline_strlen eval args a
	= a <++ eval (a1 args) <++ ".length"

inline_string_select eval args a
	= a <++ eval (a1 args) <++ ".charAt(" <++ eval (a2 args) <++ ")"
    
inline_string_update eval args a
	= a <++ eval (a1 args) 
		<++ ".substr(0," <++ eval (a2 args) <++ ")+" 
		<++ eval (a3 args) <++ "+" <++ eval (a1 args) 
    	<++ ".substr(" <++ eval (a2 args) <++ "+" <++ eval (a3 args) <++ ".length)"
    	   
inline_string_append eval args a
	= a <++ eval (a1 args) <++ "+" <++ eval (a2 args)

inline_string_slice eval args a
	= a <++ eval (a1 args) <++ ".substr(" <++ eval (a2 args) <++ ",(" <++ eval (a3 args) <++ "-" <++ eval (a2 args) <++ "+1))"

inline_toInt eval args a
	= a <++ "(" <++ eval (a1 args) <++ ".length>1)?1:(" <++ eval (a1 args) <++ ".charCodeAt(0))"

inline_toReal eval args a
	= a <++ "parseFloat(" <++ eval (a1 args) <++ ")"
    
inline_toChar eval args a
	= a <++ "String.fromCharCode(" <++ eval (a1 args) <++ ")"

inline_bitand eval args a
	= a <++ eval (a1 args) <++ "&" <++ eval (a2 args)

inline_shiftleft eval args a
	= a <++ eval (a1 args) <++ "<<" <++ eval (a2 args)

inline_shiftright eval args a
	= a <++ eval (a1 args) <++ ">>>" <++ eval (a2 args)

