implementation module SaplTokenizer

import StdEnv, FastString

matchCharAt c1 s1 p 
	:== s1.[p] == c1

is_numchar ::  String !Char -> Bool
is_numchar prev char 
	= isDigit char

is_stopchar :: String !Char -> Bool
is_stopchar prev char 
	= (char == '=') || (char == ':') || (char == ')') || (char == '(') || 
	  (char == '|') || (char == '{') || (char == '}') || (char == ',') || 
	  (char == ';') || isSpace char

is_string_stopchar :: String !Char -> Bool
is_string_stopchar prev char 
	= ((char == '"') || (char == '\n')) && ((not (endsWith "\\" prev) || (endsWith "\\\\" prev)))

is_char_stopchar :: String !Char -> Bool
is_char_stopchar prev char 
	= ((char == '\'') || (char == '\n')) && ((not (endsWith "\\" prev) || (endsWith "\\\\" prev)))
	
not_numchar = (\s c = not (is_numchar s c))	
not_stopchar = (\s c = not (is_stopchar s c))
not_string_stopchar = (\s c = not (is_string_stopchar s c))
not_char_stopchar = (\s c = not (is_char_stopchar s c))
not_space = (\s c = not (is_space s c))
is_space = (\s c = isSpace c && not (c == '\n'))
not_eol = (\s c = not (c == '\n'))

find_first_string :: !String !Int (Char String Int -> Bool) -> Int
find_first_string line start f
	| start == size line
		= size line
	| f line.[start-1] line start
		= find_first_string line (start + 1) f 
		= start

find_first_char :: !String !Int (String Char -> Bool) -> Int
find_first_char	line start f
	| start == size line
		= size line
//	| f line.[start-1] line.[start]
	| f prev line.[start]
		= find_first_char line (start + 1) f 
		= start
where
	prev = if (start > 1) {line.[start-2], line.[start-1]} {line.[start-1]} 
    
read_token :: !Int !String -> (!Int, !Int, !Token)
read_token base line
	| start > ((size line)-1) 
		= rnoarg TEndOfLine 0	
	| matchCharAt ';' line start || matchCharAt '\n' line start
		= rnoarg TEndOfLine 1
	// Skip <{ and }> from the identifier. It's to help parsing "strange" function names
	| matchAt "!<{" line start
		# stop = find_first_string line (start+3) (\prev str base = not (matchAt "}>" str base))
		= return (TStrictIdentifier (line % (start + 3, stop - 1)), stop + 2)		
	| matchAt "<{" line start
		# stop = find_first_string line (start+2) (\prev str base = not (matchAt "}>" str base))
		= return (TIdentifier (line % (start + 2, stop - 1)), stop + 2)		
	| matchAt "=:" line start
		= rnoarg TCAFAssignmentOp 2
	| matchAt "::" line start
		= rnoarg TTypeDef 2		
	| matchAt "||" line start // skip comment
		# stop = (find_first_char line (start+2) not_eol)
		= read_token stop line // skip, but leave the EOL there
	| matchCharAt '|' line start
		= rnoarg TVerticalBar 1		
	| matchCharAt '=' line start 
		= rnoarg TAssignmentOp 1
	| matchAt ":==" line start
		= rnoarg TMacroAssignmentOp 3
// This is a dangeorus annotation for Jan Martin, I don't support it any more
	| matchCharAt '@' line start 
		= read_token (start+1) line // skip
	| matchCharAt '\\' line start 
		= rnoarg TLambda 1
	| matchCharAt ',' line start 
		= rnoarg TColon 1
	| matchCharAt '(' line start 
		= rnoarg TOpenParenthesis 1
	| matchCharAt ')' line start 
		= rnoarg TCloseParenthesis 1
	| matchCharAt '{' line start 
		= rnoarg TOpenBracket 1
	| matchCharAt '}' line start 
		= rnoarg TCloseBracket 1
	| matchCharAt '"' line start 
		# stop = (find_first_char line (start+1) not_string_stopchar)
		= return (TConst (CString (line % (start + 1, stop - 1))), stop + 1)
	| matchCharAt '\'' line start 
		# stop = (find_first_char line (start+1) not_char_stopchar)
		= return (TConst (CChar (line % (start + 1, stop - 1))), stop + 1)
	| matchCharAt '+' line start 
		= numberToken 1
	| matchCharAt '-' line start 
		= numberToken 1
// This is not a number!
//	| startsWith "." lline
//		= numberToken 0
	| isDigit (line.[start])
		= numberToken 0
	| otherwise
		# stop = find_first_char line start not_stopchar
		= case tstr stop of
			"False"  = return (TConst (CBool False), stop)
			"false"  = return (TConst (CBool False), stop)
			"True"   = return (TConst (CBool True), stop)
			"true"   = return (TConst (CBool True), stop)
			"select" = return (TSelectKeyword, stop)
			"if"     = return (TCaseKeyword, stop)
			"case"   = return (TCaseKeyword, stop)
			"let"	 = return (TLetKeyword, stop)
			"in"	 = return (TInKeyword, stop)
			str		 = if (str.[0] == '!') 
							(return (TStrictIdentifier (str % (1, size str)), stop))
							(return (TIdentifier str, stop))
where
	tstr stop = line % (start, stop - 1)
	start = (find_first_char line base is_space)
	
	rnoarg t length = (start, start + length, t)	
	return (a, newbase) = (start, newbase, a)
	
	numberToken p1
		# fpart = find_first_char line (start+p1) is_numchar
		# (real, stop) = if ((size line) > fpart && line.[fpart] == '.') 
			(True, find_first_char line (fpart+1) is_numchar) (False, fpart)
		= return (TConst (if real 
					(CReal (toReal (tstr stop)))
					(CInt (toInt (tstr stop)))), stop)	
	
tokensWithPositions :: !String -> [PosToken]
tokensWithPositions inp = tokens_ 1 1 0 [] 
where 
	finalise _ _ ts=:[PosToken _ _ TEndOfLine:_] = reverse ts
	finalise lp cp ts = reverse [PosToken lp cp TEndOfLine:ts]
	
	tokens_ lp cp base ts 
			= case base < (size inp) of
				True = let (cp2, newbase, t) = read_token base inp in 
							case t of
								TEndOfLine  = tokens_ (lp+1) 1 newbase [PosToken lp (cp+cp2-base) t:ts]
											= tokens_ lp (cp+newbase-base) newbase [PosToken lp (cp+cp2-base) t:ts]
					 = finalise lp cp ts
				
tokens :: !String -> [Token]
tokens inp = tokens_ 0 [] 
where 
	tokens_ base ts 
			= case base < (size inp) of
				True = let (_, newbase, t) = read_token base inp in tokens_ newbase [t:ts]
					 = reverse ts	

instance toString Const	
where
	toString (CString str) = "\"" +++ str +++ "\""
	toString (CChar chr) = "'" +++ chr +++ "'"	
	toString (CInt int) = toString int
	toString (CReal real) = toString real
	toString (CBool True) = "True"
	toString (CBool False) = "False"
					 
instance toString Token	
where
	toString (TIdentifier name) = escape name
	toString (TStrictIdentifier name) = "!" +++ (escape name)
	toString (TComment comment) = "||" +++ comment
	toString TInlineAnnotation = ""	// skip
	toString TAssignmentOp = "="
	toString TMacroAssignmentOp = ":=="
	toString TCAFAssignmentOp = "=:"
	toString TLambda = "\\"
	toString TColon = ","
	toString TVerticalBar = "|"
	toString TOpenParenthesis = "("
	toString TCloseParenthesis = ")"
	toString TOpenBracket = "{"
	toString TCloseBracket = "}"
	toString TTypeDef = "::"
	toString (TConst const) = toString const
	toString TSelectKeyword = "select"
	toString TCaseKeyword = "if"
	toString TLetKeyword = "let"
	toString TInKeyword = "in"
	toString TEndOfLine = "\n"
	toString _ = "\n"	

escape f | ss f                            
	= "<{" +++ f +++ "}>"
	= f
where 
	ss f = or [is_ss c \\ c <-: f]
    is_ss c = not (isAlphanum c || c == '_' || c == '.')  

	
