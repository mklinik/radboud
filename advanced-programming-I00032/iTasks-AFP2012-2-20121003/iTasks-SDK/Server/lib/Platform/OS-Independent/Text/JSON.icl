implementation module JSON

import StdGeneric, Maybe, StdList, StdOrdList, StdString, _SystemArray, StdTuple, StdBool, StdFunc, StdOverloadedList
import Text

//Token type which is the intermediary representation during JSON parsing
:: Token	= TokenInt !Int
			| TokenReal	!Real
			| TokenString !String
			| TokenBool	!Bool
			| TokenNull
			| TokenBracketOpen
			| TokenBracketClose
			| TokenBraceOpen
			| TokenBraceClose
			| TokenColon
			| TokenComma
			| TokenFail	

//Basic JSON serialization
instance toString JSONNode
where
	//Escape all strings -> make target string -> copy characters
	//The reason why first a big string is made into which the characters are copied is to
	//avoid many string concatenations with big strings
	toString node
		# node = escapeAll node
		# len = sizeOf node
		= snd (copyNode 0 node (createArray len '\0'))

//Determine serialized size of a JSON datastructure
sizeOf :: !JSONNode -> Int
sizeOf (JSONNull)		= 4
sizeOf (JSONBool True)	= 4
sizeOf (JSONBool False)	= 5
sizeOf (JSONInt x)		= size (toString x)
sizeOf (JSONReal x)		= size (toString x)
sizeOf (JSONString x)	= size x + 2
sizeOf (JSONArray x)	= let len = length x in (if (len > 0) (foldl (\s x -> s + sizeOf x) (len - 1) x) 0) + 2
sizeOf (JSONObject x)	= let len = length x in (if (len > 0) (foldl (\s (l,o) -> s + size l + 2 + 1 + sizeOf o) (len - 1) x) 0) + 2
sizeOf (JSONRaw x)		= size x
sizeOf (JSONError)		= 0

//Escape all strings in a JSON structure
escapeAll :: !JSONNode -> JSONNode
escapeAll (JSONString s)	= JSONString (jsonEscape s)
escapeAll (JSONArray x)		= JSONArray (map escapeAll x)
escapeAll (JSONObject x)	= JSONObject (map (\(l,o) -> (l,escapeAll o)) x)
escapeAll node				= node

//Copy structure to a string
copyNode :: !Int !JSONNode !*{#Char} -> *(!Int, !*{#Char})
copyNode start (JSONNull) buffer		= (start + 4, copyChars start 0 4 "null" buffer)
copyNode start (JSONBool True) buffer	= (start + 4, copyChars start 0 4 "true" buffer)
copyNode start (JSONBool False) buffer	= (start + 5, copyChars start 0 5 "false" buffer)
copyNode start (JSONInt x) buffer		= let s = toString x in (start + size s, copyChars start 0 (size s) s buffer)
copyNode start (JSONReal x) buffer		= let s = toString x in (start + size s, copyChars start 0 (size s) s buffer)
copyNode start (JSONString x) buffer	= let len = size x in (start + len + 2 , copyChars (start + 1) 0 len x {buffer & [start] = '"', [start + len + 1] = '"'})
copyNode start (JSONArray items) buffer
	# (start,buffer)	= (start + 1, {buffer & [start] = '['})
	# (start,buffer)	= copyArrayItems start items buffer
	= (start + 1, {buffer & [start] = ']'})
where
	copyArrayItems start [] buffer = (start,buffer)
	copyArrayItems start [x] buffer = copyNode start x buffer
	copyArrayItems start [x:xs] buffer
		# (start,buffer) = copyNode start x buffer
		= copyArrayItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONObject items) buffer
	# (start,buffer)	= (start + 1, {buffer & [start] = '{'})
	# (start,buffer)	= copyObjectItems start items buffer
	= (start + 1, {buffer &	[start] = '}'})
where
	copyObjectItems start [] buffer = (start,buffer)
	copyObjectItems start [(l,x)] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) 0 len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		= copyNode start x buffer
	copyObjectItems start [(l,x):xs] buffer
		# (start,buffer) = let len = size l in (start + len + 3 , copyChars (start + 1) 0 len l {buffer & [start] = '"', [start + len + 1] = '"', [start + len + 2] = ':'})
		# (start,buffer) = copyNode start x buffer
		= copyObjectItems (start + 1) xs {buffer & [start] = ','}
copyNode start (JSONRaw x) buffer	= (start + size x, copyChars start 0 (size x) x buffer) 	
copyNode start _ buffer				= (start,buffer)

copyChars :: !Int !Int !Int !String !*String -> *String
copyChars offset i num src dst
	| i == num		= dst
	| otherwise		= copyChars offset (inc i) num src {dst & [offset + i] = src.[i]}

//Basic JSON deserialization (just structure)
instance fromString JSONNode
where
	fromString s = fst (parse (lex 0 s))

IsDigit c :== c >= '0' && c <= '9'

lex :: !Int !String -> [Token]
lex offset input
	| offset<size input
		# c = input.[offset]
		| c=='['
			= [TokenBracketOpen : lex (offset+1) input]
		| c==']'
			= [TokenBracketClose : lex (offset+1) input]
		| c=='{'
			= [TokenBraceOpen : lex (offset+1) input]
		| c=='}'
			= [TokenBraceClose : lex (offset+1) input]
		| c==':'
			= [TokenColon : lex (offset+1) input]
		| c==','
			= [TokenComma : lex (offset+1) input]
		| c=='n' && offset+3<size input && input.[offset+1]=='u' && input.[offset+2]=='l' && input.[offset+3]=='l'
			= [TokenNull : lex (offset+4) input]
		| c=='t' && offset+3<size input && input.[offset+1]=='r' && input.[offset+2]=='u' && input.[offset+3]=='e'
			= [TokenBool True : lex (offset+4) input]
		| c=='f' && offset+4<size input && input.[offset+1]=='a' && input.[offset+2]=='l' && input.[offset+3]=='s' && input.[offset+4]=='e'
			= [TokenBool False : lex (offset+5) input]
		| c==' ' || c=='\t' || c=='\n' || c=='\r' || c=='\f' || c=='\v' // inlined isSpace c
			= lex (offset+1) input
		| c=='"'
			# offset = offset+1;
			= lexString offset offset input
		| IsDigit c
			= lexNumber (offset+1) offset input
		| c=='-' && offset+1<size input && IsDigit input.[offset+1]
			= lexNumber (offset+2) offset input
			= [TokenFail]
		= []
where
	lexString :: !Int !Int !{#Char} -> [Token]
	lexString offset stringCharsOffset input
		| offset>=size input
			= [TokenFail] // missing '"'
		| input.[offset] == '"'
			#! string = input % (stringCharsOffset,offset-1)
			= [TokenString string : lex (offset+1) input]
		| input.[offset] == '\\'
			= lexString (offset + 2) stringCharsOffset input // skip the escaped character
			= lexString (offset + 1) stringCharsOffset input

	lexNumber :: !Int !Int !{#Char} -> [Token]
	lexNumber offset numberOffset input
		| offset>=size input
			#! i = toInt (input % (numberOffset,offset-1))
			= [TokenInt i]
		# c = input.[offset]
		| IsDigit c
			= lexNumber (offset+1) numberOffset input
		| c<>'.'
			#! i = toInt (input % (numberOffset,offset-1))
			= [TokenInt i : lex offset input]
			= lexReal (offset+1) numberOffset input

	lexReal :: !Int !Int !{#Char} -> [Token]
	lexReal offset numberOffset input
		| offset>=size input
			#! r = toReal (input % (numberOffset,offset-1))
			= [TokenReal r]
		# c = input.[offset]
		| IsDigit c
			= lexReal (offset+1) numberOffset input
		| c<>'e' && c<>'E'
			#! r = toReal (input % (numberOffset,offset-1))
			= [TokenReal r : lex offset input]
		| offset+1<size input && IsDigit input.[offset+1]
			= lexRealWithExponent (offset+2) numberOffset input
		| offset+2<size input && input.[offset+1]=='-' && IsDigit input.[offset+2]
			= lexRealWithExponent (offset+3) numberOffset input
			#! r = toReal (input % (numberOffset,offset-1))
			= [TokenReal r : lex offset input]

	lexRealWithExponent :: !Int !Int !{#Char} -> [Token]
	lexRealWithExponent offset numberOffset input
		| offset>=size input
			#! r = toReal (input % (numberOffset,offset-1))
			= [TokenReal r]
		| IsDigit input.[offset]
			= lexRealWithExponent (offset+1) numberOffset input
			#! r = toReal (input % (numberOffset,offset-1))
			= [TokenReal r : lex offset input]

//Simple recursive descent parser
parse :: ![Token] -> (!JSONNode,![Token])
parse [TokenNull:ts] 							= (JSONNull, ts)
parse [TokenBool x:ts] 							= (JSONBool x, ts)
parse [TokenInt x:ts]							= (JSONInt x, ts)
parse [TokenReal x:ts] 							= (JSONReal x, ts)
parse [TokenString x:ts]						= (JSONString (jsonUnescape x), ts)
parse [TokenBracketOpen,TokenBracketClose:ts]	= (JSONArray [], ts)
parse [TokenBracketOpen:ts]
	= case (parseArrayItems ts []) of
		([TokenBracketClose:ts`],items)	= (JSONArray (reverse items), ts`)
		_								= (JSONError, ts)
where
	parseArrayItems :: ![Token] ![JSONNode] -> (![Token],![JSONNode])
	parseArrayItems tokens nodes
		= case (parse tokens) of
			(node,[TokenComma:ts])	= parseArrayItems ts [node:nodes]
			(node,ts)				= (ts,[node:nodes])
parse [TokenBraceOpen:TokenBraceClose:ts]		= (JSONObject [], ts)
parse [TokenBraceOpen:ts]
	= case (parseObjectItems ts []) of
		([TokenBraceClose:ts`],items)	= (JSONObject (reverse items), ts`)
		_								= (JSONError, ts)
where
	parseObjectItems :: ![Token] ![(!String,!JSONNode)] -> (![Token],![(!String,!JSONNode)])
	parseObjectItems tokens nodes
		= case (parse tokens) of
			(JSONString label,[TokenColon:ts])
				= case (parse ts) of
					(node,[TokenComma:ts`])	= parseObjectItems ts` [(label,node):nodes]
					(node,ts`)				= (ts`,[(label,node):nodes])
			_
				= (tokens,nodes)
parse tokens = (JSONError,tokens)

//Escape a string
jsonEscape :: !String -> String
jsonEscape src
	# reps = findChars 0 src
	= case reps of
		[!!] -> src
		reps -> copyAndReplaceChars 0 0 reps src (createArray (size src + Length reps) '\0')
where
	//Find the special characters
	findChars :: Int String -> [!(Int,Char)!]
	findChars i s
		| i >= size s
			= [!!]
		# c = s.[i]
		| c == '\\' || c == '"' || c == '/'
			= [!(i,c): findChars (i + 1) s!]
		| c == '\b'
			= [!(i,'b'): findChars (i + 1) s!]
		| c == '\f'
			= [!(i,'f'): findChars (i + 1) s!]
		| c == '\n'
			= [!(i,'n'): findChars (i + 1) s!]
		| c == '\r'
			= [!(i,'r'): findChars (i + 1) s!]
		| c == '\t'
			= [!(i,'t'): findChars (i + 1) s!]
			= findChars (i + 1) s

	//Build the escaped string from the original and the replacements		
	copyAndReplaceChars :: !Int !Int ![!(Int,Char)!] !String !*String -> *String
	copyAndReplaceChars is id reps=:[!(ir,c):rs!] src dest
		# (is,id,src,dest) = copyCharsI is id ir src dest
		= copyAndReplaceChars (is + 1) (id + 2) rs src {dest & [id] = '\\', [id + 1] = c}
	copyAndReplaceChars is id [!!] src dest
		= copyRemainingChars is id src dest
	
//Unescape a string
jsonUnescape :: !String -> String
jsonUnescape src
	# reps = findChars 0 src
	= case reps of
		[!!] -> src
		reps -> copyAndReplaceChars 0 0 reps src (createArray (size src - length reps) '\0')
where
	//Find the special characters
	findChars :: Int String -> [!(Int,Char)!]
	findChars i s
		| i+1>=size s
			= [!!]
		# c0 = s.[i]
		| c0 == '\\'
			# c1 = s.[i+1]
			#! rc = rep c1
			= [!(i,rc): findChars (i + 2) s!]
			= findChars (i + 1) s
	where
			rep '\\'	= '\\'
			rep '"'		= '"'
			rep '/'		= '/'
			rep 'b'		= '\b'
			rep 'f'		= '\f'
			rep 'n'		= '\n'
			rep 'r'		= '\r'
			rep 't'		= '\t'
			rep c		= c

	//Build the escaped string from the original and the replacements		
	copyAndReplaceChars :: Int Int [!(Int,Char)!] String *String -> *String
	copyAndReplaceChars is id reps=:[!(ir,c):rs!] src dest
		# (is,id,src,dest) = copyCharsI is id ir src dest
		=	copyAndReplaceChars (is + 2) (id + 1) rs src {dest & [id] = c}
	copyAndReplaceChars is id [!!] src dest
		= copyRemainingChars is id src dest

copyCharsI :: !Int !Int !Int !String !*String -> (!Int,!Int,!String,!*String)
copyCharsI is id iend src dest
	| is < iend		= copyCharsI (is + 1) (id + 1) iend src {dest & [id] = src.[is]}
					= (is,id,src,dest)

copyRemainingChars :: !Int !Int !String !*String -> *String
copyRemainingChars is id src dest
	| is < size src	= copyRemainingChars (is + 1) (id + 1) src {dest & [id] = src.[is]}
					= dest

//Intersperse an element on a list
intersperse :: a [a] -> [a]
intersperse i [] = []
intersperse i [x] = [x]
intersperse i [x:xs] = [x,i:intersperse i xs]

//-------------------------------------------------------------------------------------------

toJSON :: !a -> JSONNode | JSONEncode{|*|} a
toJSON x = case (JSONEncode{|*|} x) of
	[node]	= node
	_		= JSONError 

/*
* Generic JSON encoder
*/
generic JSONEncode t :: !t -> [JSONNode]

JSONEncode{|Int|} x = [JSONInt x]
JSONEncode{|Real|} x = [JSONReal x]
JSONEncode{|Char|} x = [JSONString {x}]
JSONEncode{|Bool|} x = [JSONBool x]
JSONEncode{|String|} x = [JSONString x]
JSONEncode{|UNIT|} (UNIT) = []
JSONEncode{|PAIR|} fx fy (PAIR x y) = fx x ++ fy y
JSONEncode{|EITHER|} fx fy (LEFT x) = fx x
JSONEncode{|EITHER|} fx fy (RIGHT y) = fy y
JSONEncode{|OBJECT|} fx (OBJECT x) = fx x
JSONEncode{|CONS of {gcd_fields=gcd_fields=:[_:_]}|} fx (CONS x)
	//Record
	= [JSONObject [(f.gfd_name, o) \\ o <- fx x & f <- gcd_fields | isNotNull o]]
where
	isNotNull JSONNull = False
	isNotNull _ = True
JSONEncode{|CONS of {gcd_arity=0,gcd_name}|} fx (CONS x)
	//Constructor without parameters
	= [JSONString gcd_name]
JSONEncode{|CONS of {gcd_name}|} fx (CONS x)
	//Constructor with parameters				
	= [JSONArray [JSONString gcd_name : fx x]]

JSONEncode{|FIELD|} fx (FIELD x) = fx x							
JSONEncode{|[]|} fx x = [JSONArray (flatten [fx e \\ e <- x])]
JSONEncode{|(,)|} fx fy (x,y) = [JSONArray (fx x ++ fy y)]
JSONEncode{|(,,)|} fx fy fz (x,y,z) = [JSONArray (fx x ++ fy y ++ fz z)]
JSONEncode{|(,,,)|} fx fy fz fi (x,y,z,i) = [JSONArray (fx x ++ fy y ++ fz z ++ fi i)]
JSONEncode{|(,,,,)|} fx fy fz fi fj (x,y,z,i,j) = [JSONArray (fx x ++ fy y ++ fz z ++ fi i ++ fj j)]
JSONEncode{|{}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSONEncode{|{!}|} fx x = [JSONArray (flatten [fx e \\ e <-: x])]
JSONEncode{|Maybe|} fx (Just x) = fx x
JSONEncode{|Maybe|} fx (Nothing) = [JSONNull]
JSONEncode{|JSONNode|} node = [node]

//-------------------------------------------------------------------------------------------
fromJSON :: !JSONNode -> Maybe a | JSONDecode{|*|} a
fromJSON node = fst (JSONDecode{|*|} [node])

/*
* Generic JSON parser, using a list of tokens
*/
generic JSONDecode t :: ![JSONNode] -> (!Maybe t, ![JSONNode])

JSONDecode{|Int|} [JSONInt i:xs]		= (Just i, xs)
JSONDecode{|Int|} l						= (Nothing, l)

JSONDecode{|Real|} [JSONReal r:xs]		= (Just r, xs)
JSONDecode{|Real|} [JSONInt i:xs]		= (Just (toReal i), xs)
JSONDecode{|Real|} l					= (Nothing, l)

JSONDecode{|Char|} l =: [JSONString s:xs]
	| size s == 1						= (Just s.[0],xs)
										= (Nothing, l)
JSONDecode{|Char|} l					= (Nothing, l)

JSONDecode{|Bool|} [JSONBool b:xs]		= (Just b,xs)
JSONDecode{|Bool|} l					= (Nothing, l)

JSONDecode{|String|} [JSONString s:xs]	= (Just s, xs)
JSONDecode{|String|} l					= (Nothing, l)

JSONDecode{|UNIT|} l					= (Just UNIT, l)

JSONDecode{|PAIR|} fx fy l = case fx l of
	(Just x,xs)	= case fy xs of
		(Just y, ys)			= (Just (PAIR x y), ys)
		_						= (Nothing, l)
	_							= (Nothing, l)
	
JSONDecode{|EITHER|} fx fy l = case fx l of
	(Just x, xs)				= (Just (LEFT x),xs)
	(Nothing, xs)				= case fy l of
		(Just y, ys)			= (Just (RIGHT y),ys)
		(Nothing, ys)			= (Nothing, l)

JSONDecode{|OBJECT|} fx l = case fx l of
	(Just x, xs)	= (Just (OBJECT x),xs)
	_				= (Nothing, l)

JSONDecode{|CONS of {gcd_fields=[_:_]}|} fx l=:[JSONObject fields: xs]
	//Records
	 = case fx [JSONObject fields] of
		(Just x, _)					= (Just (CONS x),xs)
		_							= (Nothing, l)
JSONDecode{|CONS of {gcd_arity=0,gcd_name}|} fx l=:[JSONString name: xs]
	//Constructor without parameters
	| name == gcd_name				= case fx xs of
		(Just x, ys)				= (Just (CONS x),ys)
		_							= (Nothing, l)
	| otherwise						= (Nothing, l)
JSONDecode{|CONS of {gcd_name}|} fx l=:[JSONArray [JSONString name:fields] :xs]
	//Constructor with parameters
	| name == gcd_name				= case fx fields of
		(Just x, _)					= (Just (CONS x), xs)
		_							= (Nothing, l)
	| otherwise						= (Nothing, l)		
JSONDecode{|CONS|} fx l = (Nothing, l)

JSONDecode{|FIELD of {gfd_name}|} fx l =: [JSONObject fields]
	# field = findField gfd_name fields
	= case fx field of
		(Just x, _)	= (Just (FIELD x), l)
		_			= (Nothing, l)
where
	findField match [] 	= []
	findField match [(l,x):xs]
		| l == match 	= [x]
						= findField match xs
						
JSONDecode{|FIELD|} fx l = (Nothing, l)

JSONDecode{|[]|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just x, xs)
		_				= (Nothing, l)
JSONDecode{|[]|} fx l 	= (Nothing, l)

JSONDecode{|(,)|} fx fy l =:[JSONArray [xo,yo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)		= (Just (x,y), xs)
			_				= (Nothing, l)
		_					= (Nothing, l)
JSONDecode{|(,)|} fx fy l	= (Nothing, l)

JSONDecode{|(,,)|} fx fy fz l =:[JSONArray [xo,yo,zo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)			= case fz [zo] of
				(Just z,_)		= (Just (x,y,z), xs)
				_				= (Nothing, l)
			_					= (Nothing, l)
		_						= (Nothing, l)
JSONDecode{|(,,)|} fx fy fz l	= (Nothing, l)

JSONDecode{|(,,,)|} fx fy fz fi l =:[JSONArray [xo,yo,zo,io]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)		= (Just (x,y,z,i), xs)
					_				= (Nothing, l)
				_					= (Nothing, l)
			_						= (Nothing, l)
		_							= (Nothing, l)
JSONDecode{|(,,,)|} fx fy fz fi l	= (Nothing, l)

JSONDecode{|(,,,,)|} fx fy fz fi fj l =:[JSONArray [xo,yo,zo,io,jo]:xs]
	= case fx [xo] of
		(Just x,_)	= case fy [yo] of
			(Just y,_)	= case fz [zo] of
				(Just z,_) = case fi [io] of
					(Just i,_)	= case fj [jo] of
						(Just j,_)		= (Just (x,y,z,i,j), xs)
						_				= (Nothing, l)
					_					= (Nothing, l)
				_						= (Nothing, l)
			_							= (Nothing, l)
		_								= (Nothing, l)
JSONDecode{|(,,,,)|} fx fy fz fi fj l	= (Nothing, l)

JSONDecode{|{}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{}|} fx l 	= (Nothing, l)

JSONDecode{|{!}|} fx l =:[JSONArray items:xs]
	= case decodeItems fx items of
		(Just x)		= (Just {e \\ e <- x}, xs)
		_				= (Nothing, l)
JSONDecode{|{!}|} fx l 	= (Nothing, l)

decodeItems fx [] 		= Just []
decodeItems fx [ox:oxs]	= case fx [ox] of
	(Just x, _)	= case decodeItems fx oxs of
		(Just xs)	= Just [x:xs]
		_ 			= Nothing
	_			= Nothing

JSONDecode{|Maybe|} fx []				= (Just Nothing, [])
JSONDecode{|Maybe|} fx [JSONNull:xs]	= (Just Nothing, xs)
JSONDecode{|Maybe|} fx l = case fx l of
	(Just x,xs)							= (Just (Just x), xs)
	_									= (Nothing,l)

JSONDecode{|JSONNode|} []				= (Just JSONNull, [])
JSONDecode{|JSONNode|} [x:xs]			= (Just x, xs)
JSONDecode{|JSONNode|} l				= (Nothing, l)

jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a
jsonQuery path node
	= case (findNode (split "/" path) node ) of
		Just child	= fromJSON child
		Nothing		= Nothing
where
	findNode :: ![String] !JSONNode -> (Maybe JSONNode)
	findNode [] node	= Just node
	findNode [s:ss] (JSONObject fields)
		= case findField s fields of
			Just f	= findNode ss f
			Nothing	= Nothing
	findNode [s:ss] (JSONArray items)
		# index = toInt s
		| index >= 0 && index < length items	= findNode ss (items !! index)
		| otherwise								= Nothing
	findNode _ _		= Nothing
	
	findField s []			= Nothing
	findField s [(l,x):xs]	= if (l == s) (Just x) (findField s xs)

instance == JSONNode
where
	(==) JSONNull 			JSONNull 			= True
	(==) (JSONBool x) 		(JSONBool y)		= x == y
	(==) (JSONInt x) 		(JSONInt y)			= x == y
	(==) (JSONReal x)		(JSONReal y)		= toString x == toString y
	(==) (JSONInt x)		(JSONReal y)		= toString (toReal x) == toString y
	(==) (JSONReal x)		(JSONInt y)			= toString x == toString (toReal y)
	(==) (JSONString x)		(JSONString y)		= x == y
	(==) (JSONArray xs) 	(JSONArray ys)		= xs == ys
	(==) (JSONObject xs) 	(JSONObject ys)		= sortBy cmpFst (filter (notNull o snd) xs) == sortBy cmpFst (filter (notNull o snd) ys)
	where
		cmpFst a b = fst a < fst b
		notNull JSONNull	= False
		notNull _			= True
	(==) (JSONRaw x)		(JSONRaw y)			= x == y
	(==) JSONError			JSONError			= True
	(==) _ 					_ 					= False
