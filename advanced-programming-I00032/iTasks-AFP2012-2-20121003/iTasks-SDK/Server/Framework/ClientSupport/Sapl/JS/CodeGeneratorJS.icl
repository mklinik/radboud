implementation module CodeGeneratorJS

import StdEnv, Maybe, Void, StringAppender, FastString
import SaplTokenizer, SaplParser, BuiltInJS

:: CoderState = { cs_inbody 		:: Bool			     // The body of a function is generated (not signature)
				, cs_intrfunc		:: Maybe SaplTerm    // The name of the currently genrenated function if it is tail recursive
				, cs_inletdef		:: Maybe [SaplTerm]  // used for generating cross referencing (e.g. recursive) lets
				, cs_current_vars 	:: [SaplTerm]
				, cs_constructors	:: Map SaplTerm [SaplTerm]
				, cs_functions		:: Map SaplTerm [SaplTerm]
				, cs_CAFs			:: Map SaplTerm Void				
				, cs_builtins		:: Map String (String, Int)
				, cs_inlinefuncs	:: Map String (InlineCoderFunc, Int)
		      	}

newState :: ParserState -> CoderState
newState p = { cs_inbody 		= False
			 , cs_intrfunc 		= Nothing
			 , cs_inletdef 		= Nothing
			 , cs_current_vars 	= []
			 , cs_constructors 	= p.ps_constructors
			 , cs_functions		= p.ps_functions
			 , cs_CAFs			= p.ps_CAFs
			 , cs_builtins		= builtInFunctions
			 , cs_inlinefuncs	= inlineFunctions
			 }

// Returns True if a term can be inlined, i.e. no separate statement is needed
inline :: SaplTerm -> Bool
inline (SLet _ _) = False
inline (SSelect _) = False
inline (SLambda _ _) = False
inline (SCase _ _ _) = False
inline _ = True

pushArgs :: CoderState [SaplTerm] -> CoderState
pushArgs s [t:ts] = pushArgs {s & cs_current_vars = [t:s.cs_current_vars]} ts
pushArgs s [] = s

prefix = "__"

//escapeTable :: Char -> Maybe String
escapeTable '<' = Just "$3C"
escapeTable '>' = Just "$3E"
escapeTable '{' = Just "$7B"
escapeTable '}' = Just "$7D"
escapeTable '(' = Just "$28"
escapeTable ')' = Just "$29"
escapeTable '*' = Just "$2A"
escapeTable '+' = Just "$2B"
escapeTable '-' = Just "$2D"
escapeTable ',' = Just "$2C"
escapeTable '~' = Just "$7E"
escapeTable '`' = Just "$60"
escapeTable ';' = Just "$3B"
escapeTable '!' = Just "$21"
escapeTable '=' = Just "$3D"
escapeTable '#' = Just "$23"
escapeTable ':' = Just "$3A"
escapeTable '/' = Just "$2F"
escapeTable '|' = Just "$2F"
escapeTable '%' = Just "$7C"
escapeTable '@' = Just "$40"
escapeTable '.' = Just "_"	// this is the module name separator
escapeTable _ = Nothing

// Escape identifier, except the "$eval" part if it ends like that		
escapeName :: String StringAppender -> StringAppender
escapeName name a
	| (endsWith "$eval" name)
		= escapeName_ (a <++ prefix) 0 0 ((size name)- 5) (size name)
		= escapeName_ (a <++ prefix) 0 0 (size name) (size name)
where 
	escapeName_ a start offs size realsize
		| offs > (size - 1)
			| start == 0
				= append a name
			| (start < offs) || (not (size == realsize))
				= append a (name % (start, (realsize-1)))
				= a					
		= case escapeTable name.[offs] of
			Just r # a = append a (name % (start, (offs-1)))
				   # a = append a r
				   = escapeName_ a (offs + 1) (offs + 1) size realsize
				   = escapeName_ a start (offs + 1) size realsize

callWrapper :: SaplTerm CoderState StringAppender -> StringAppender
callWrapper t s a
	| not (inline t)
		= termCoder t s a		
	| (isJust s.cs_intrfunc) && (isTailRecursive (fromJust s.cs_intrfunc) t)
		= forceTermCoder t s a
//		= a <++ "return " <++ termCoder t s <++ ";" // Optimize for stack use. BUGGY
		= a <++ "return " <++ forceTermCoder t s <++ ";" // Optimize for speed

unpackName (SStrictName name) = name
unpackName (SName name _) = name

isStrictName (SStrictName _) = True
isStrictName _ = False

isTailRecursive :: SaplTerm SaplTerm -> Bool
isTailRecursive name (SSelect args) = any (isTailRecursive name) args
isTailRecursive name (SCase pred lhs rhs) = isTailRecursive name lhs || isTailRecursive name rhs
isTailRecursive name (SApplication aname _) = (unpackName name) == (unpackName aname)
isTailRecursive name (SLambda body _) = isTailRecursive name body
isTailRecursive _ _ = False

funcCoder :: FuncType CoderState StringAppender -> StringAppender
funcCoder (FTFunc name body args) s a = normalFunc name body args s a
funcCoder (FTMacro name body args) s a = normalFunc name body args s a
funcCoder (FTCAF name body) s a = normalFunc name body [] s a
funcCoder (FTADT name args) s a = foldl (\a t = termCoder t s a) a args
funcCoder (FTRecord name args) s a 
	= a <++ constructorCoder name 0 args s <++ termCoder name s <++ ".fields=[" <++ recordFieldCoder args <++ "];";

normalFunc :: SaplTerm SaplTerm [SaplTerm] CoderState StringAppender -> StringAppender
normalFunc name body args s a
	// Generate $eval function if any of its arguments is annotated as strict	
	# a = if (any isStrictName args) 
				(makeStrictClosure a (unpackName name) args) a
				
	// Generate function signature			
	# a = a <++ "function " <++ termCoder name s
			<++ "(" <++ termArrayCoder args "," s <++ "){"

	// Update coder state with the new local arguments, ...
	# s = {s & cs_inbody = True
			 , cs_current_vars = args
			 , cs_intrfunc = if (isTailRecursive name body) (Just name) Nothing}

	// Generate body (in a while(1) if the function is tail recursive)				
	# a = if (isJust s.cs_intrfunc) (a <++ "while(1){") a
	# a = callWrapper body s a
	# a = if (isJust s.cs_intrfunc) (a <++ "}") a
	= a <++ "};"  

where
	// The (i-1) is to be compatible with the original compiler written in JavaScript
	makeStrictClosure a name args
		= a <++ "function " 
			<++ escapeName name <++ "$eval("
			<++ joinList "," ["a"+++toString (i-1) \\ i <- [1..length args]]
			<++ "){return " <++ escapeName name <++ "("
			<++ (\a = fst (foldl strictsep (a,1) args))
			<++ ");};"
	where
		strictsep (a,i) arg
			# a = case isStrictName arg of
					True = a <++ "Sapl.feval(a" <++ (i-1) <++ ")"
						 = a <++ "a" <++ (i-1)
			| i < (length args)
				= (a <++ ",", i+1)
				= (a, i)

make_app_args :: SaplTerm [SaplTerm] CoderState StringAppender -> StringAppender
make_app_args func args s a 
	= case get func s.cs_functions of
		Just func_args = a <++ maa_ func_args args 0 s
					   = a <++ maa_ [] args 0 s // this is the case of built-in functions
where
	// fargs: formal, aargs: actual	
	maa_ [(SStrictName _):fargs] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a
		= a <++ forceTermCoder aa s <++ maa_ fargs aargs (i+1) s 
	maa_ [_:fargs] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a		
		= a <++ termCoder aa s <++ maa_ fargs aargs (i+1) s 
	maa_ [] [aa:aargs] i s a 
		# a = if (i>0) (a <++ ",") a		
		= a <++ termCoder aa s <++ maa_ [] aargs (i+1) s 
	maa_ _ [] _ _ a = a

constructorCoder :: SaplTerm Int [SaplTerm] CoderState StringAppender -> StringAppender
constructorCoder name id args s a
	// Original field names are not necessary, they can be shorten
	# newargs = [SName ("a"+++toString i) 0 \\ i <- [1..length args]]
	
	# a = a <++ "function " <++ termCoder name s <++ "(" <++ termArrayCoder newargs "," s
			<++ "){return [" <++ id <++ ",'" <++ unpackName name <++ "'" 
	# a = case length args of
		0 = a
		  = a <++ "," <++ termArrayCoder newargs "," s 
	= a	<++ "];};"

recordFieldCoder :: [SaplTerm] StringAppender -> StringAppender
recordFieldCoder [t] a = a <++ "\"" <++ unpackName t <++ "\""
recordFieldCoder [t:ts] a
	= a <++ "\"" <++ unpackName t <++ "\"," <++ recordFieldCoder ts
recordFieldCoder [] a = a

termArrayCoder :: [SaplTerm] String CoderState StringAppender -> StringAppender
termArrayCoder [t] sep s a = termCoder t s a
termArrayCoder [t:ts] sep s a
	= a <++ termCoder t s <++ sep <++ termArrayCoder ts sep s
termArrayCoder [] _ s a = a

letDefCoder :: [SaplTerm] CoderState StringAppender -> StringAppender
letDefCoder [t] s a = termCoder t s a
letDefCoder [t:ts] s a
	= a <++ termCoder t s <++ "," <++ letDefCoder ts {s & cs_inletdef = Just (tl (fromJust s.cs_inletdef))}
letDefCoder [] _ a = a

// Generate code that forces the evaluation of the given term
forceTermCoder :: SaplTerm CoderState StringAppender -> StringAppender
forceTermCoder t=:(SApplication name args) s a
	| isdynamic name
		= a <++ "[__dynamic_handler]"
	// local variable
	| isMember name s.cs_current_vars
		= a <++ "Sapl.feval(" <++ termCoder t s <++ ")" 

	// global function or constructor (not curried)
	| (isJust constructor_args && (length (fromJust constructor_args) == length args)) || 
	  (isJust function_args && (length (fromJust function_args) == length args))

		= case (isJust s.cs_intrfunc) && 
			   (isTailRecursive (fromJust s.cs_intrfunc) t) of
			
			// It is posible that a tail recursive call has the same function as its
			// argument. In this case, the deeper call can't be handled as tail recursive!
			True = a <++ make_tr_app args {s & cs_intrfunc = Nothing}
				 = a <++ func_name <++ "(" <++ make_app_args name args s <++ ")"

	// more arguments than needed
	| (isJust function_args && (length (fromJust function_args) < length args))
		= a <++ "Sapl.feval(" <++ termCoder t s <++ ")"  
	
	// TODO: inline (...) doc
	| (isJust inlinefunc && (snd (fromJust inlinefunc)) == length args)
		= a <++ "(" <++ (fst (fromJust inlinefunc)) (\t a = forceTermCoder t s a) args <++ ")"

	| (isJust builtin && (snd (fromJust builtin)) == length args)
		= a <++ func_name <++ "(" <++ make_app_args name args s <++ ")"
	
	| (isNothing function_args) && (isNothing builtin)
		= a <++ "Sapl.feval(" <++ termCoder t s <++ ")"

	// Otherwise
		= a <++ termCoder t s
where
	func_name a = a <++ escapeName (unpackName name) // skip level information

	//is_constructor = isJust (get name s.cs_constructors)
	constructor_args = get name s.cs_constructors
	function_args = get name s.cs_functions
	tr_function_args = fromJust (get (fromJust s.cs_intrfunc) s.cs_functions)
	builtin = get (unpackName name) s.cs_builtins
	inlinefunc = get (unpackName name) s.cs_inlinefuncs	

	make_tr_app args s a 
		= a <++ "var " <++ mta_1 tr_function_args args 0 s <++ ";" 
			<++ mta_2 tr_function_args 0 s <++ "continue;"
	where
		mta_1 [(SStrictName _):fargs] [aa:aargs] i s a 
			# a = if (i>0) (a <++ ",") a
			= a <++ "t" <++ i <++ "=" <++ forceTermCoder aa s <++ mta_1 fargs aargs (i+1) s 
		mta_1 [_:fargs] [aa:aargs] i s a 
			# a = if (i>0) (a <++ ",") a		
			= a <++ "t" <++ i <++ "=" <++ termCoder aa s <++ mta_1 fargs aargs (i+1) s 
		mta_1 [] _ i s a = a

		mta_2 [fa:fargs] i s a 
			= a <++ escapeName (unpackName fa) <++ "=t" <++ i <++ ";" <++ mta_2 fargs (i+1) s // skip level information for TR!
		mta_2 [] i s a = a

forceTermCoder t=:(SName name _) s a
	| any isStrictEq s.cs_current_vars
		= termCoder t s a
	| isMember t s.cs_current_vars
		= a <++ "Sapl.feval(" <++ termCoder t s <++ ")" 
	| (isJust constructor_args) && (length (fromJust constructor_args) == 0)
		= a <++ escapeName name <++ "()"	
	| isCAF
		= a <++ escapeName name <++ "()"	
	| (isJust function_args && (length (fromJust function_args) == 0))
		= a <++ escapeName name <++ "()"	
		= a <++ "Sapl.feval(" <++ termCoder t s <++ ")"
where
	isStrictEq (SStrictName aname) = aname == name
	isStrictEq _ = False	
	constructor_args = get t s.cs_constructors
	function_args = get t s.cs_functions	
	isCAF = isJust (get t s.cs_CAFs)

forceTermCoder t s a = termCoder t s a
		
termCoder :: SaplTerm CoderState StringAppender -> StringAppender
termCoder t=:(SName name level) s a
	| (s.cs_inbody) && (not isLocalVar) && (isJust constructor_args) && (length (fromJust constructor_args) == 0)
		= a <++ escapeName name <++ "()"	
	| (s.cs_inbody) && (not isLocalVar) && isCAF
		= a <++ escapeName name <++ "()"	
	| (s.cs_inbody) && (not isLocalVar) && isStrictFunction
		= a <++ escapeName name <++ "$eval"	

		// else
		| (isJust s.cs_inletdef) && (isMember t (fromJust s.cs_inletdef))
			= a <++ "[function(){return Sapl.feval(" <++ var_name <++ ");},[]]"

			// else
			= a <++ var_name
			  
where 
	// TODO: doc
	findLocalVar [(SName cn level):cs] = if (cn == name) level (findLocalVar cs)
	findLocalVar [(SStrictName cn):cs] = if (cn == name) 0 (findLocalVar cs)
	findLocalVar [] = 0
	isLocalVar = isMember t s.cs_current_vars
	
	constructor_args = get t s.cs_constructors
	isBuiltIn = isJust (get name s.cs_builtins)
	isFunction = isJust (get t s.cs_functions)		
	isConstructor = isJust (constructor_args)	
	isCAF = isJust (get t s.cs_CAFs)
	isStrictFunction = case get t s.cs_functions of
							Just args = any isStrictName args
									  = False

	var_name a # decl_level = findLocalVar s.cs_current_vars
			   = case decl_level of
					0 = a <++ escapeName name
			  		  = a <++ escapeName name <++ "_" <++ decl_level

termCoder (SStrictName name) s a = a <++ escapeName name
termCoder (SSelect args) s a 
	# a = a <++ "var ys=" <++ forceTermCoder (hd args) {s & cs_intrfunc = Nothing} <++  ";"
	= case length args > 2 of
		True = a <++ "switch(ys[0]){" <++ selectArgs (tl args) s 0 <++ "}"
			 = callWrapper (hd (tl (args))) s a
where
	selectArgs [] s i a = a
	selectArgs [t:ts] s i a 
		= a <++ "case " <++ toString i <++ ":" <++ callWrapper t s <++ selectArgs ts s (i+1)

termCoder (SLambda body args) s a 
	# s = pushArgs s args
	= a <++ "var " <++ lambdaArgs args 0 s <++ callWrapper body s 
where 
	lambdaArgs [t] i s a = a <++ termCoder t s <++ "=ys[" <++ i+2 <++ "];"
	lambdaArgs [t:ts] i s a = a <++ termCoder t s <++ "=ys[" <++ i+2 <++ "]," <++ lambdaArgs ts (i+1) s
	lambdaArgs [] i s a = a

termCoder (SCase pred lhs rhs) s a 
	// in the predicate of an 'if' there can't be tail recursive call
	= a <++ "if(" <++ forceTermCoder pred {s & cs_intrfunc = Nothing} <++ "){"
		<++ callWrapper lhs s 
		<++ "}else{"
		<++ callWrapper rhs s
		<++ "}"

termCoder (SApplication name args) s a
	| isdynamic name
		= a <++ "[__dynamic_handler]"
	| (isJust constructor_args && (length (fromJust constructor_args) == length args))
		= a <++ func_name name <++ "(" <++ make_app_args name args s <++ ")"
		= a <++ "[" <++ termCoder name s <++ ",[" 
			<++ termArrayCoder args "," s
			<++ "]]"
where
	constructor_args = get name s.cs_constructors
	func_name name a = a <++ escapeName (unpackName name) // skip level information


termCoder (SConst (CString str)) s a = a <++ "\"" <++ str <++ "\""
termCoder (SConst (CChar chr)) s a = a <++ "'" <++ chr <++ "'"
termCoder (SConst (CInt int)) s a = a <++ int
termCoder (SConst (CReal real)) s a = a <++ real
termCoder (SConst (CBool True)) s a = a <++ "true"
termCoder (SConst (CBool False)) s a = a <++ "false"

termCoder (SConstructor name id args) s a
	= constructorCoder name id args s a

/* Let definitions can be cross references to each other.
 * If a let definition has reference to an other which is not yet declared
 * (or recursive) the referenced variable must be wrap into a closure.
 * cs_inletdef contains all the remaining let definitions (letDefCoder 
 * removes the elements step by step)
 */
termCoder (SLet body defs) s a
	# defnames = map (\(SLetDefinition name _) = name) defs
	# s = pushArgs s defnames
	= a <++ "var " <++ letDefCoder defs {s & cs_inletdef = Just defnames} <++ ";\n " 
		<++ callWrapper body s <++ ";"

termCoder (SLetDefinition name body) s a
	= a <++ termCoder name {s & cs_inletdef = Nothing} <++ "=" <++ termCoder body s

termCoder _ s a = abort "???"

isdynamic (SName name _) | startsWith "_SystemDynamic." name
	= True
	= False

isdynamic (SStrictName name) | startsWith "_SystemDynamic." name
	= True
	= False

isdynamic _ = False

generateJS :: String -> (MaybeErrorString (StringAppender, ParserState))
generateJS saplsrc
	# pts = tokensWithPositions saplsrc
	= case parse pts of
		Ok (funcs, s) # state = newState s 
					  # a = foldl (\a curr = funcCoder curr state a) newAppender funcs
					  = Ok (a, s)
		Error msg = Error msg

exprGenerateJS :: String (Maybe ParserState) -> (MaybeErrorString StringAppender)
exprGenerateJS saplsrc mbPst
	# pts = tokensWithPositions saplsrc
	= case parseExpr pts of
		Ok (body, s) # state = newState (mergeParserStates s mbPst)
					 # a = termCoder body {state & cs_inbody=True} newAppender
					 = Ok a
		Error msg = Error msg

