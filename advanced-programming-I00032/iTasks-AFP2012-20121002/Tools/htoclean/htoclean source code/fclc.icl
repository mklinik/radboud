implementation module fclc;

// author: John van Groningen
// University of Nijmegen

import StdEnv;

TypeDefNormal :== 0;
TypeDefAbs :== 1;
TypeDefSemiAbs :== 2;

::	HDefinition
	=	Constant String String
	|	Enum [String]
	|	CFunction String String FunctionArgumentTypes String
	|	CleanFunction String CleanTypes CleanType
	|	CTypeDefinition String String
	|	CleanTypeDefinition String CleanType IsUnique Int /*abs_kind*/ 
	|	Include String
	|	Import String
	|	ErrorHFile !String !Int;

:: HError
	= HError !String !Int;

::	FunctionArgumentTypes
	=	NilFunctionArgumentTypes
	|	FunctionArgumentType String OptionalArgumentName FunctionArgumentTypes;

::	OptionalArgumentName
	=	ArgumentName !String
	|	NoArgumentName;

::	CleanTypes
	=	CleanTypes CleanType CleanTypes
	|	NilCleanTypes;

:: IsUnique :== Bool;
IsUnique :== True;
IsNotUnique :== False;
UnqDon`tCare :== undef;

::	CleanType
	=	CleanType IsUnique String
	|	CleanTupleType IsUnique CleanTypes
	|	CleanErrorType;

(>:) infixl;
(>:) f g:==g f;

(<:) infixr;
(<:) f g:==f g;

(<::) infixr;
(<::) f g:==let { (a,b)=g; } in f a b;

(<:::) infixr;
(<:::) f g:==let { (a,b,c)=g; } in f a b c;

skip_comment line_n hf
	# (ok,c,hf) = freadc hf;
	| not ok
		= (False,c,line_n,hf);
	| c=='*'
		= skip_comment2 line_n hf;
	| c=='\n'
		#! line_n=line_n+1;
		= skip_comment line_n hf;
		= skip_comment line_n hf;
	
	skip_comment2 line_n hf
		# (ok,c,hf) = freadc hf;
		| not ok
			= (False,c,line_n,hf);
		| c=='/'
			= read_char_and_skip_space line_n hf;
		| c=='*'
			= skip_comment2 line_n hf;
		| c=='\n'
			#! line_n=line_n+1;
			= skip_comment line_n hf;
			= skip_comment line_n hf;

read_char_and_skip_space line_n hf
	# (ok,c,hf) = freadc hf;
	| not ok
		= (False,c,line_n,hf);
		= skip_space c line_n hf;

skip_space_and_read_char c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (ok,c,line_n,hf);
	# (ok,c,hf) = freadc hf;
	= (ok,c,line_n,hf);

skip_space c line_n hf
	| c==' ' || c=='\t'
		= read_char_and_skip_space line_n hf;
	| c=='\n'
		#! line_n=line_n+1;
		= read_char_and_skip_space line_n hf;
	| c=='/'
		# (ok,c2,hf) = freadc hf;
		| not ok
			= (True,c,line_n,hf);
		| c2=='*'
			= skip_comment line_n hf;
		| c2=='/'
			= skip_line_comment line_n hf;
			# (_,hf) = fseek hf (-1) FSeekCur;
			= (True,c,line_n,hf);
		= (True,c,line_n,hf);
	where {
		skip_line_comment line_n hf
			# (ok,c,hf) = freadc hf;
			| not ok
				= (False,c,line_n,hf);
			| c=='\n'
				#! line_n=line_n+1;
				= read_char_and_skip_space line_n hf;
				= skip_line_comment line_n hf;
	}

skip_line line_n hf
	# (ok,c,hf) = freadc hf;
	| not ok
		= (line_n,hf);
	| c=='\n'
		= (line_n+1,hf);
		= skip_line line_n hf;

is_ident_character c 
	# cl=(toInt c) bitor 0x20;
	| (cl>=toInt 'a' && cl<=toInt 'z') || (c>='0' && c<='9') || c=='_'
		= True;
		= False;

read_identifier_c s c hf
	| is_ident_character c
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,hf);
			= read_identifier_c s nc hf;
		= (True,s,c,hf); 

read_identifier_and_skip_space_c c line_n hf
	# (ok,identifier,c,hf) = read_identifier_c "" c hf;
	| not ok
		= (ok,"",c,line_n,hf);	
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	= (ok,identifier,c,line_n,hf);

read_identifier_and_skip_space line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (False,"",c,line_n,hf);
		= read_identifier_and_skip_space_c c line_n hf;

read_integer_or_char c l hf
	| c=='\''
		# (ok,c2,hf) = freadc hf;
		| not ok
			= (ok,"",c2,l,hf);
		# (ok,c,hf) = freadc hf;
		| not ok
			= (ok,"",c,l,hf);
		| c<>'\''
			= (True,"",c,[ErrorHFile "\' expected" 0:l],hf);
			# (ok,c,hf) = freadc hf;
			= (True,"'"+++toString c2+++"'",c,l,hf);
	| c=='+' || c=='-'
		# (ok,nc,hf) = freadc hf;
		| not ok
			= (ok,"",nc,l,hf);
		# (ok,s,nc,l,hf) = read_integer_denotation "" nc l hf;
		= (ok,toString c+++s,nc,l,hf)
		= read_integer_denotation "" c l hf;

read_integer_denotation s c l hf
	| c=='0'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,l,hf);
			| nc=='x'
				# (ok,nnc,hf) = freadc hf;
				  s = (s+++toString nc);
				| not ok
					= (True,s,nc,[ErrorHFile "Hex digit expected" 0:l],hf);
					= read_hex_digits s nnc l hf;
				= read_more_digits s nc l hf;
		= read_digits s c l hf;

read_digits s c l hf
	| c>='0' && c<='9'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (ok,s,nc,l,hf);
			= read_more_digits s nc l hf;
		= (True,s,c,[ErrorHFile "Digit expected" 0:l],hf);

read_more_digits s c l hf
	| c>='0' && c<='9'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,l,hf);
			= read_more_digits s nc l hf;
		= (True,s,c,l,hf); 

read_hex_digits s c l hf
	| c>='0' && c<='9' || c>='a' && c<='f' || c>='A' && c<='F'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (ok,s,nc,l,hf);
			= read_more_hex_digits s nc l hf;
		= (True,s,c,[ErrorHFile "Hex digit expected" 0:l],hf);

read_more_hex_digits s c l hf
	| c>='0' && c<='9' || c>='a' && c<='f' || c>='A' && c<='F'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,l,hf);
			= read_more_hex_digits s nc l hf;
		= (True,s,c,l,hf); 

read_declaration_specifiers_and_pointers type l c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (ok,type,l,c,line_n,hf);
	| c=='*'
		# (ok,c,hf) = freadc hf;
		| not ok
			= (ok,type,l,c,line_n,hf);
			= read_declaration_specifiers_and_pointers (type+++"*") l c line_n hf;
		= (ok,type,l,c,line_n,hf);

read_optional_struct_name_and_declaration_specifiers_and_pointers type=:"struct" l c line_n hf
	| is_ident_character c
		# (ok,struct_name,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| not ok
			= (ok,type,l,c,line_n,hf);
			= read_declaration_specifiers_and_pointers (type+++" "+++struct_name) l c line_n hf;
		= (True,"",[ErrorHFile "struct name expected" line_n:l],c,line_n,hf);
read_optional_struct_name_and_declaration_specifiers_and_pointers type l c line_n hf
	= read_declaration_specifiers_and_pointers type l c line_n hf;

read_char_space_declaration_specifiers_and_pointers l line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,"",l,c,line_n,hf);
	# (ok,identifier,c,line_n,hf)=read_identifier_and_skip_space_c c line_n hf;
	| not ok
		= (ok,identifier,l,c,line_n,hf);
		= read_optional_struct_name_and_declaration_specifiers_and_pointers identifier l c line_n hf;

read_function_argument_types l line_n hf
	# (ok,identifier,l,c,line_n,hf) = read_char_space_declaration_specifiers_and_pointers l line_n hf
	| not ok || identifier=="void"
		= (ok,c,NilFunctionArgumentTypes,hf);
		= read_more_function_argument_types_s identifier l c line_n hf;

read_more_function_argument_types l line_n hf
	# (ok,identifier,l,c,line_n,hf) = read_char_space_declaration_specifiers_and_pointers l line_n hf
	| not ok
		= (ok,c,NilFunctionArgumentTypes,hf);
		= read_more_function_argument_types_s identifier l c line_n hf;

read_more_function_argument_types_s identifier l c line_n hf
	| size identifier>0
		| c==','
			# (ok,c,argument_types,hf) = read_more_function_argument_types l line_n hf;
			= (ok,c,FunctionArgumentType identifier NoArgumentName argument_types,hf);
		// read optional argument name
		| is_ident_character c
			# (ok,argument_name,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
			| not ok
				= (False,c,FunctionArgumentType identifier NoArgumentName NilFunctionArgumentTypes,hf);
			| c==','
				# (ok,c,argument_types,hf) = read_more_function_argument_types l line_n hf;
				= (ok,c,FunctionArgumentType identifier (ArgumentName argument_name) argument_types,hf);
				= (True,c,FunctionArgumentType identifier (ArgumentName argument_name) NilFunctionArgumentTypes,hf);

			= (True,c,FunctionArgumentType identifier NoArgumentName NilFunctionArgumentTypes,hf);
		= (True,c,NilFunctionArgumentTypes,hf);

read_function_type type c l line_n hf
	| size type>0
		# (ok,type,l,c,line_n,hf) = read_optional_struct_name_and_declaration_specifiers_and_pointers type l c line_n hf;
		| not ok
			= (l,hf);
		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| not ok
			= (l,hf);	
		| size identifier>0
			| c=='('
				# (ok,c,argument_types,hf) = read_function_argument_types l line_n hf;
				| not ok
					= (l,hf)
				| c==')'
					# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
					| not ok
						= (l,hf);
					# l = [CFunction identifier type argument_types "":l];
					| c==';'
						= read_h_file l line_n hf;
						= read_h_file [ErrorHFile "; expected" line_n:l] line_n hf;
					= read_h_file_c [ErrorHFile ") expected" line_n:l] c line_n hf;
				= read_h_file_c [ErrorHFile "( expected" line_n:l] c line_n hf;
			= read_h_file_c [ErrorHFile "function name expected" line_n:l] c line_n hf;
		= read_h_file [ErrorHFile ("Declaration expected: "+++toString c) line_n:l] line_n hf;

skip_to end_c line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (line_n,hf);
	| c==end_c
		= (line_n,hf);
	| c=='\n'
		#! line_n=line_n+1;
		= skip_to end_c line_n hf;
	| c=='('
		= skip_to end_c <:: skip_to ')' line_n hf;
	| c=='{'
		= skip_to end_c <:: skip_to '}' line_n hf;
		= skip_to end_c line_n hf;


read_typedef l c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (eof_error line_n l,hf);
	# (ok,type,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
	| not ok
		= (eof_error line_n l,hf);
	| size type>0
		| type=="enum"
			= read_typedef_enum l c line_n hf;
		# (ok,type,l,c,line_n,hf) = read_optional_struct_name_and_declaration_specifiers_and_pointers type l c line_n hf;
		| not ok
			= (eof_error line_n l,hf);
		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| not ok
			= (eof_error line_n l,hf);
		| size identifier>0
			# (ok,c,line_n,hf) = skip_space c line_n hf;
			| not ok
				= (eof_error line_n l,hf);
			# l = [CTypeDefinition identifier type:l];
			| c==';'
				= read_h_file l line_n hf;
				= read_h_file [ErrorHFile "; expected" line_n:l] line_n hf;
			= read_h_file_c [ErrorHFile ("Identifier expected: "+++toString c) line_n:l] c line_n hf;
		= read_h_file [ErrorHFile ("Type expected: "+++toString c) line_n:l] line_n hf;

read_enum l c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (True,[],eof_error line_n l,line_n,hf);
	| c<>'{'
		= (False,[],[ErrorHFile ("{ expected: "+++toString c) line_n:l],line_n,hf);
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (True,[],eof_error line_n l,line_n,hf);
	# (enum_constants,l,c,line_n,hf) = read_enum_constants l c line_n hf;
	| c<>'}'
		= (False,enum_constants,[ErrorHFile ("} expected: "+++toString c) line_n:l],line_n,hf);
		= (False,enum_constants,l,line_n,hf);

read_enum_semicolon_and_h_file l c line_n hf
	# (eof,enum_constants,l,line_n,hf) = read_enum l c line_n hf;
	| eof
		= (l,hf);
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (eof_error line_n l,hf);
	| c<>';'
		= read_h_file [ErrorHFile ("; expected: "+++toString c) line_n:l] line_n hf;
		= read_h_file [Enum enum_constants:l] line_n hf;

read_typedef_enum l c line_n hf
	# (eof,enum_constants,l,line_n,hf) = read_enum l c line_n hf;
	| eof
		= (l,hf);
	# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space line_n hf;
	| not ok
		= (eof_error line_n l,hf);
	| size identifier<=0
		= read_h_file_c [ErrorHFile ("Identifier expected: "+++toString c) line_n:l] c line_n hf;
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (eof_error line_n l,hf);
	# l = [Enum enum_constants:l];
	| c==';'
		= read_h_file l line_n hf;
		= read_h_file [ErrorHFile "; expected" line_n:l] line_n hf;

read_enum_constants l c line_n hf
	| is_ident_character c
		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| c==','
			# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
			| not ok
				= ([identifier],eof_error line_n l,c,line_n,hf);
				# (enum_constants,l,c,line_n,hf) = read_enum_constants l c line_n hf;
				= ([identifier:enum_constants],l,c,line_n,hf);
			= ([identifier],l,c,line_n,hf);
		= ([],l,c,line_n,hf);

/*
skip_struct l line_n hf
	= read_h_file l <:: skip_to ';' line_n hf;
*/

eof_error line_n l = [ErrorHFile "Unexpected end of file" line_n:l];

want_char wanted_c l line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (False,ok,eof_error line_n l,c,line_n,hf);
	| c<>wanted_c
		= (False,ok,[ErrorHFile (toString wanted_c+++" expected, got "+++toString c) line_n:l],c,line_n,hf);
		= (True,ok,l,c,line_n,hf);

want_char_c wanted_c c l line_n hf
	| c<>wanted_c
		= (False,[ErrorHFile (toString wanted_c+++" expected, got "+++toString c) line_n:l],c,line_n,hf);
		= (True,l,c,line_n,hf);

read_clean_type_def l line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,eof_error line_n l,c,line_n,hf);
	| c=='*'
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,eof_error line_n l,c,line_n,hf);
			= read_clean_type_def2 IsUnique l c line_n hf;
			= read_clean_type_def2 IsNotUnique l c line_n hf;

read_clean_type_def2 is_unique_type l c line_n hf
	# (ok,type_name,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
	| not ok
		= (ok,eof_error line_n l,c,line_n,hf);
	| size type_name==0
		= (ok,[ErrorHFile ("Type name expected, got "+++toString c) line_n:l],c,line_n,hf);
	| c == '('
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,l,c,line_n,hf);
		# (ok,clean_type,_,l,c,line_n,hf) = read_clean_type_def_rhs False l c line_n hf
		| isCleanErrorType clean_type
			= (ok,l,c,line_n,hf);
		# (parse_ok,ok,l,c,line_n,hf) = want_char ')' l line_n hf;
		| not parse_ok
			= (ok,l,c,line_n,hf);
			= (ok,[CleanTypeDefinition type_name clean_type is_unique_type TypeDefSemiAbs:l],c,line_n,hf);
	# (ok,clean_type,abs,l,c,line_n,hf) = read_clean_type_def_rhs True l c line_n hf
	| isCleanErrorType clean_type
		= (ok,l,c,line_n,hf);
		# abs_kind = if abs TypeDefAbs TypeDefNormal
	= (ok,[CleanTypeDefinition type_name clean_type is_unique_type abs_kind:l],c,line_n,hf);

read_clean_type_def_rhs abs_allowed l c line_n hf
	# (parse_ok,l,c,line_n,hf) = want_char_c ':' c l line_n hf;
	| not parse_ok
		= (parse_ok,CleanErrorType,False,l,c,line_n,hf);
	# (parse_ok,ok,l,c,line_n,hf) = want_char '=' l line_n hf;
	| not parse_ok
		= (ok,CleanErrorType,False,l,c,line_n,hf);
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,CleanErrorType,False,l,c,line_n,hf);
	| abs_allowed && c<>'='
		# (ok,clean_type,c,l,line_n,hf) = read_attr_clean_type c l line_n hf;
		= (ok,clean_type,True,l,c,line_n,hf);
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,CleanErrorType,False,eof_error line_n l,c,line_n,hf);
	# (ok,clean_type,c,l,line_n,hf) = read_attr_clean_type c l line_n hf;
	| not ok
		= (ok,CleanErrorType,False,l,c,line_n,hf);
	= (ok,clean_type,False,l,c,line_n,hf);

isNilCleanTypes NilCleanTypes = True;
isNilCleanTypes _ = False;

read_attr_clean_type c l line_n hf
	| c == '*'
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,CleanErrorType,c,l,line_n,hf);
			= read_clean_type IsUnique c l line_n hf;
		= read_clean_type IsNotUnique c l line_n hf;

read_clean_type is_unique c l line_n hf
	| c=='('
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,CleanErrorType,c,l,line_n,hf);
			# (ok,clean_types,c,l,line_n,hf) = read_clean_tuple_argument_types c l line_n hf;
			| not ok
				= (ok,CleanErrorType,c,eof_error line_n l,line_n,hf);
			| isNilCleanTypes clean_types
				= (ok,CleanErrorType,c,l,line_n,hf)
			| c==')'
				# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
				| not ok
					= (ok,CleanErrorType,c,eof_error line_n l,line_n,hf);
					= (ok,CleanTupleType is_unique clean_types,c,l,line_n,hf)
				= (ok,CleanErrorType,c,l,line_n,hf);
	| c=='{'
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok || c<>'#'
			= (False,CleanErrorType,c,l,line_n,hf);
		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space line_n hf;
		| not ok
			= (False,CleanErrorType,c,eof_error line_n l,line_n,hf);
		| identifier<>"Int" && identifier<>"Real" && identifier<>"Char"
			= (False,CleanErrorType,c,l,line_n,hf);
		| c<>'}'
			= (False,CleanErrorType,c,l,line_n,hf);				
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (False,CleanErrorType,c,eof_error line_n l,line_n,hf);
			= (True,CleanType is_unique ("{#"+++identifier+++"}"),c,l,line_n,hf);

		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| not ok
			= (ok,CleanErrorType,c,eof_error line_n l,line_n,hf);
		| size identifier>0
			= (ok,CleanType is_unique identifier,c,l,line_n,hf);
	= (ok,CleanErrorType,c,l,line_n,hf);

isCleanErrorType CleanErrorType = True;
isCleanErrorType t = False;

read_clean_function_argument_types c l line_n hf
	# (ok,clean_type,c,l,line_n,hf) = read_attr_clean_type c l line_n hf;
	| isCleanErrorType clean_type
		= (ok,NilCleanTypes,c,line_n,hf);
	 	# (ok,clean_types,c,line_n,hf) = read_clean_function_argument_types c l line_n hf;
		= (ok,CleanTypes clean_type clean_types,c,line_n,hf);

read_clean_tuple_argument_types c l line_n hf
	# (ok,clean_type,c,l,line_n,hf) = read_attr_clean_type c l line_n hf;
	| not ok
		= (ok,NilCleanTypes,c,l,line_n,hf);
	| isCleanErrorType clean_type
		= (ok,NilCleanTypes,c,[ErrorHFile "Type expected" line_n:l],line_n,hf);
	| c==','
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,NilCleanTypes,c,eof_error line_n l,line_n,hf);
	 	# (ok,clean_types,c,l,line_n,hf) = read_clean_tuple_argument_types c l line_n hf;
		= (ok,CleanTypes clean_type clean_types,c,l,line_n,hf);
		= (ok,CleanTypes clean_type NilCleanTypes,c,l,line_n,hf);

read_clean_function function_name l line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,eof_error line_n l,c,line_n,hf);
	# (ok,argument_types,c,line_n,hf) = read_clean_function_argument_types c l line_n hf;
	| not ok
		= (ok,eof_error line_n l,c,line_n,hf);
	| c=='-'
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,eof_error line_n l,c,line_n,hf);
		| c=='>'
			# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
			| not ok
				= (ok,eof_error line_n l,c,line_n,hf);
			# (ok,result_type,c,l,line_n,hf) = read_attr_clean_type c l line_n hf;
			| not ok
				= (ok,eof_error line_n l,c,line_n,hf);
			| isCleanErrorType result_type
				= (ok,[ErrorHFile "Function result type expected" line_n:l],c,line_n,hf);
				= (ok,[CleanFunction function_name argument_types result_type:l],c,line_n,hf);
			= (ok,[ErrorHFile ("> expected, got "+++toString c) line_n:l],c,line_n,hf);
	| c==')'
		= case argument_types of {
			(CleanTypes type NilCleanTypes)
				-> (ok,[CleanFunction function_name NilCleanTypes type:l],c,line_n,hf);
			_
				-> (ok,[ErrorHFile ("-> expected, got "+++toString c) line_n:l],c,line_n,hf);
		}
		= (ok,[ErrorHFile ("-> or ) expected, got "+++toString c) line_n:l],c,line_n,hf);

read_import_file_name_and_skip_space_c s c line_n hf
	| toInt c>=33 && c<>',' && c<>')' && c<>';'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,line_n,hf);
			= read_import_file_name_and_skip_space_c s nc line_n hf;
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	= (ok,s,c,line_n,hf);

read_clean_import l c line_n hf
 	# (ok,file_name,c,line_n,hf) = read_import_file_name_and_skip_space_c "" c line_n hf;
 	| not ok
 		= (ok,eof_error line_n l,c,line_n,hf);
 	| size file_name==0
 		= (ok,[ErrorHFile "Header name expected" line_n:l],c,line_n,hf);
 	# l=[Import file_name:l];
 	| c==','
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (ok,eof_error line_n l,c,line_n,hf);
 		= read_clean_import l c line_n hf;
	= (True,l,c,line_n,hf);

read_clean l c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (ok,eof_error line_n l,line_n,hf);
	| c<>'('
		= (ok,[ErrorHFile ("( expected, got "+++toString c) line_n:l],line_n,hf);
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		= read_clean_lines ok c l line_n hf;

	read_clean_lines ok c l line_n hf
		# (ok,l,c,line_n,hf)=read_clean_line ok c l line_n hf;
		# (ok,c,line_n,hf) = skip_space c line_n hf;
		| not ok
			= (ok,eof_error line_n l,line_n,hf);
		| c==')'
			= (ok,l,line_n,hf);
		| c==';'
			# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
			| c==')'
				= (ok,l,line_n,hf);
				= read_clean_lines ok c l line_n hf;
			= (ok,[ErrorHFile (") or ; expected, got "+++toString c) line_n:l],line_n,hf);

	read_clean_line ok c l line_n hf
		| not ok
			= (ok,eof_error line_n l,c,line_n,hf);
		| c==':'
			# (parse_ok,ok,l,c,line_n,hf) = want_char ':' l line_n hf;
			| not parse_ok
				= (ok,l,c,line_n,hf);
				= read_clean_type_def l line_n hf;
		# (ok,function_name,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
		| not ok
			= (ok,eof_error line_n l,c,line_n,hf);
		| size function_name==0
			= (ok,[ErrorHFile ("import or clean function name expected, got "+++toString c) line_n:l],c,line_n,hf);
		| function_name=="import"
			= read_clean_import l c line_n hf;
		# (parse_ok,l,c,line_n,hf) = want_char_c ':' c l line_n hf;
		| not parse_ok
			= (True,l,c,line_n,hf);
		# (parse_ok,ok,l,c,line_n,hf) = want_char ':' l line_n hf;
		| not parse_ok
			= (ok,l,c,line_n,hf);
			= read_clean_function function_name l line_n hf;

read_h_file_name line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (ok,"",c,line_n,hf);
	# (ok,file_name,c2,hf) = read_h_file_name_c "" c hf;
	= (ok,file_name,c2,line_n,hf);

read_h_file_name_c s c hf
	| toInt c>=32 && c<>'>' && c<>'"'
		# (ok,nc,hf) = freadc hf;
		  s = (s+++toString c);
		| not ok
			= (False,s,nc,hf);
			= read_h_file_name_c s nc hf;
		= (True,s,c,hf); 

read_h_file_c l c line_n hf
	# (ok,c,line_n,hf) = skip_space c line_n hf;
	| not ok
		= (eof_error line_n l,hf);
		= read_h_file_no_space_c l c line_n hf;

read_h_file l line_n hf
	# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
	| not ok
		= (l,hf);
		= read_h_file_no_space_c l c line_n hf;

read_h_file_no_space_c l c line_n hf
	| c=='#'
		# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space line_n hf;
		| not ok
			= (l,hf);
		| identifier=="define"
			# (ok,c,line_n,hf) = skip_space c line_n hf;
			| not ok
				= (l,hf);
			# (ok,variable,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
			| not ok
				= (l,hf);
			# (ok,integer,c,l,hf) = read_integer_or_char c l hf;
			| not ok
				= (l,hf);
				= read_h_file_c [Constant variable integer:l] c line_n hf;
		| identifier=="include"
			# (ok,c1,line_n,hf) = skip_space c line_n hf;
			| not ok
				= (eof_error line_n l,hf);
			| c1<>'"' && c1<>'<'
				= ([ErrorHFile ("\" or < expected, got: "+++toString c1) line_n:l],hf);
			# (ok,file_name,c2,line_n,hf) = read_h_file_name line_n hf;
			| not ok
				= (eof_error line_n l,hf);
			| size file_name==0
				= ([ErrorHFile "File name expected" line_n:l],hf);
			# c1=if (c1=='<') '>' c1;
			| c1<>c2
				= ([ErrorHFile (toString c1+++" expected") line_n:l],hf);
				= read_h_file [Include file_name:l] line_n hf;
		| identifier=="pragma"
			# (line_n,hf) = skip_line line_n hf;
			= read_h_file l line_n hf
			= read_h_file_c [ErrorHFile "define expected" line_n:l] c line_n hf;
	# (ok,identifier,c,line_n,hf) = read_identifier_and_skip_space_c c line_n hf;
	| not ok
		= (l,hf);
	| identifier=="typedef"
		= read_typedef l c line_n hf;
	| identifier=="enum"
		= read_enum_semicolon_and_h_file l c line_n hf;
/*
	| identifier=="struct"
		= skip_struct l line_n hf;
*/
	| identifier=="Clean"
		# (ok,l,line_n,hf) = read_clean l c line_n hf;
		| not ok
			= (l,hf);
		# (ok,c,line_n,hf) = read_char_and_skip_space line_n hf;
		| not ok
			= (l,hf);
		| c==';'
			= read_h_file l line_n hf;
			= read_h_file_c l c line_n hf;
		= read_function_type identifier c l line_n hf;

find_clean_function function_name f=:[CleanFunction name _ _:l]
	| name==function_name
		= f;
		= find_clean_function function_name l;
find_clean_function function_name [_:l]
	= find_clean_function function_name l;
find_clean_function function_name []
	= [];

find_clean_type type_name f=:[CleanTypeDefinition name _ _ _:l]
	| name==type_name
		= f;
		= find_clean_type type_name l;
find_clean_type type_name [_:l]
	= find_clean_type type_name l;
find_clean_type type_name []
	= [];

find_c_type type_name f=:[CTypeDefinition name _:l]
	| name==type_name
		= f;
		= find_c_type type_name l;
find_c_type type_name [_:l]
	= find_c_type type_name l;
find_c_type type_name []
	= [];

occurs_in_list a []
	= False;
occurs_in_list a [b:l]
	| a==b
		= True;
		= occurs_in_list a l;

arity_of_clean_type type h_all_types
	# (n,cyclic_synonyms)=add_arity_of_clean_type type 0 [] [] h_all_types;
	= n;

arity_and_cycles_of_clean_type type h_all_types
	=add_arity_of_clean_type type 0 [] [] h_all_types;

arity_of_clean_types types h_all_types
	# (n,cyclic_synonyms)=add_arity_of_clean_types types 0 [] [] h_all_types;
	= n;

arity_and_cycles_of_clean_types types h_all_types
	= add_arity_of_clean_types types 0 [] [] h_all_types;

	add_arity_of_clean_type (CleanType _ clean_type_name) n expanded_synonyms cyclic_synonyms h_all_types
		| occurs_in_list clean_type_name expanded_synonyms
			= (n+1,[clean_type_name:cyclic_synonyms]);
			= case (find_clean_type clean_type_name h_all_types) of {
				[CleanTypeDefinition _ clean_type _ _:_]
					-> add_arity_of_clean_type clean_type n [clean_type_name:expanded_synonyms] cyclic_synonyms h_all_types;
				_
					-> (n+1,cyclic_synonyms);
			  };
	add_arity_of_clean_type (CleanTupleType _ tuple_arguments) n expanded_synonyms cyclic_synonyms h_all_types
		= add_arity_of_clean_types tuple_arguments n expanded_synonyms cyclic_synonyms h_all_types;
	add_arity_of_clean_types NilCleanTypes n expanded_synonyms cyclic_synonyms h_all_types
		= (n,cyclic_synonyms);
	add_arity_of_clean_types (CleanTypes clean_type clean_types) n expanded_synonyms cyclic_synonyms h_all_types
		# (n,cyclic_synonyms) = add_arity_of_clean_type clean_type n expanded_synonyms cyclic_synonyms h_all_types;
		= add_arity_of_clean_types clean_types n expanded_synonyms cyclic_synonyms h_all_types;

add_arity_of_c_types NilFunctionArgumentTypes n
	= n;
add_arity_of_c_types (FunctionArgumentType argument_type _ argument_types) n
	= add_arity_of_c_types argument_types (add_arity_of_c_type argument_type n);

add_arity_of_c_type type_name n
	| type_name<>"void"
		= n+1;
		= n;

apply_to_first_of_2 f t :== let{ (a,b) = t; }in (f a,b);

take_n_arguments2 0 rest_arguments
	= (NilFunctionArgumentTypes,rest_arguments);
take_n_arguments2 n (FunctionArgumentType argument argument_name arguments)
	= apply_to_first_of_2 (FunctionArgumentType argument argument_name) (take_n_arguments2 (n-1) arguments);

take_n_arguments 0 _
	= NilFunctionArgumentTypes;
take_n_arguments n (FunctionArgumentType argument argument_name arguments)
	= FunctionArgumentType argument argument_name (take_n_arguments (n-1) arguments);

take_n_arguments n NilFunctionArgumentTypes
	= NilFunctionArgumentTypes;

write_icl_file module_name h_imports h_constants h_types h_functions h_all_types iclf
	= iclf
		>: fwrites "implementation module " >: fwrites module_name >: fwrites ";\n\n"
		>: write_imports h_imports
		>: write_icl_file_types h_types >: write_icl_file h_functions >: write_file_constants h_constants;
{
	write_icl_file [CFunction function_name result_type arguments extra_ccall_arg_types:h_info] iclf
		= case (find_clean_function function_name h_functions) of {
			[CleanFunction _ clean_argument_types clean_result_type:_]
				# n_clean_arguments = arity_of_clean_types clean_argument_types h_all_types;
				# n_clean_results = arity_of_clean_type clean_result_type h_all_types;
				# n_c_arguments = add_arity_of_c_types arguments (add_arity_of_c_type result_type 0);
				# n_extra_arguments_2 = n_clean_arguments+n_clean_results-n_c_arguments;
				# n_extra_arguments = n_extra_arguments_2 >> 1;
				# iclf = iclf
					>: fwritec '\n' >: fwrites function_name >: fwrites " :: "
					>: write_clean_function_type clean_argument_types clean_result_type >: fwrites ";\n"
					>: fwrites function_name >: fwrites " "
					>: write_clean_function_arguments 0 clean_argument_types >: fwrites " = code {\n"
				
					>: fwrites "\tccall " >: fwrites function_name >: fwrites " \"";
				# (result_c_arguments,iclf) = write_n_ccall_arguments (n_clean_arguments-n_extra_arguments) arguments iclf;
					with {
						write_n_ccall_arguments 0 arguments iclf
							= (arguments,iclf);
						write_n_ccall_arguments n NilFunctionArgumentTypes iclf
							= (NilFunctionArgumentTypes,iclf);
						write_n_ccall_arguments n (FunctionArgumentType s _ arguments) iclf
							= write_n_ccall_arguments (n-1) arguments (iclf >: write_ccall_type_char s h_all_types);
					}
				# iclf = iclf
					>: fwritec ':'
					>: write_ccall_type_char result_type h_all_types
					>: write_ccall_result_arguments result_c_arguments h_all_types
					>: fwrites extra_ccall_arg_types
					>: fwrites "\"\n"

				| is_world_type clean_result_type h_all_types
					# iclf = iclf
						>: fwrites "\tfill_a 0 1\n"
						>: fwrites "\tpop_a 1\n"
						>: fwrites "}\n"
						>: write_c_function_type_in_icl_or_dcl_file result_type function_name arguments;
	
					-> write_icl_file h_info iclf;
					# iclf = iclf
						>: fwrites "}\n"
						>: write_c_function_type_in_icl_or_dcl_file result_type function_name arguments;
	
					-> write_icl_file h_info iclf;
			_
				-> write_icl_file h_info
					(iclf >: fwritec '\n'
					>: fwrites function_name >: fwrites " :: " >: write_clean_from_c_function_type arguments result_type >: fwrites ";\n"

					>: fwrites function_name >: fwritec ' ' >: write_clean_c_function_arguments 0 arguments  >: fwrites " = code {\n"
				
					>: fwrites "\tccall " >: fwrites function_name >: fwrites " \"" >: write_ccall_arguments arguments h_all_types
					>: fwritec ':' >: write_ccall_type_char result_type h_all_types >: fwrites "\"\n}\n"
					>: write_c_function_type_in_icl_or_dcl_file result_type function_name arguments
					);
			};
	write_icl_file [CleanFunction function_name argument_types result_type:h_info] iclf
		= write_icl_file h_info iclf;
	write_icl_file [] iclf
		= iclf;
}

	write_icl_file_types [CTypeDefinition s1 s2:h_info] clf
		= write_icl_file_types h_info clf;
	write_icl_file_types [CleanTypeDefinition s1 s2 is_unique_type _:h_info] clf
		# type = CleanTypeDefinition s1 s2 is_unique_type TypeDefNormal
		= write_icl_file_types h_info (write_clean_type_definition type clf);
	write_icl_file_types [] clf
		= clf;

	write_dcl_file_types [CTypeDefinition s1 s2:h_info] clf
		= write_dcl_file_types h_info clf;
	write_dcl_file_types [type:h_info] clf
		= write_dcl_file_types h_info (write_clean_type_definition type clf);
	write_dcl_file_types [] clf
		= clf;

	write_clean_type_definition (CleanTypeDefinition s1 s2 is_unique_type abs_kind) clf
		= clf	>: fwrites (if is_unique_type ":: *" ":: ")
				>: fwrites s1 >: write_clean_type_rhs s2 abs_kind >: fwrites ";\n";

	write_clean_type_rhs type_rhs abs_kind clf
		| abs_kind == TypeDefAbs
			= clf;
		| abs_kind == TypeDefSemiAbs
			= clf
				>: fwrites " (:== "
				>: write_clean_type type_rhs
				>: fwritec ')';
		| abs_kind == TypeDefNormal
			= clf
				>: fwrites " :== "
				>: write_clean_type type_rhs;

	write_file_constants [Constant s1 s2:h_info] iclf
		= write_file_constants h_info (iclf >: fwrites s1 >: fwrites ":==" >: fwrites s2 >: fwrites ";\n");
	write_file_constants [Enum enum_constants:h_info] iclf
		= write_file_constants h_info (iclf >: write_read_enum_constants enum_constants 0);
	write_file_constants [] iclf
		= iclf;

	write_read_enum_constants [] n iclf
		= iclf;
	write_read_enum_constants [v:vs] n iclf
		= write_read_enum_constants vs (n+1) (iclf >: fwrites v >: fwrites ":==" >: fwritei n >: fwrites ";\n");

	is_world_type (CleanType _ clean_result_type) h_all_types
		| clean_result_type=="World"
			= True;
		 	= case (find_clean_type clean_result_type h_all_types) of {
				[CleanTypeDefinition _ clean_result_type _ _:_]
					-> is_world_type clean_result_type h_all_types;
				_
					-> False
		 	 };
	is_world_type _ h_all_types
		= False;

write_clean_function_type NilCleanTypes result_type iclf
	= iclf >: write_clean_type result_type;
write_clean_function_type argument_types result_type iclf
	= iclf >: write_clean_argument_types ' ' argument_types >: fwrites " -> " >: write_clean_type result_type;

write_clean_argument_types separator_char NilCleanTypes iclf
	= iclf;
write_clean_argument_types separator_char (CleanTypes argument_type NilCleanTypes) iclf
	= iclf >: fwritec '!' >: write_clean_type argument_type;
write_clean_argument_types separator_char (CleanTypes argument_type argument_types) iclf
	= write_clean_argument_types separator_char argument_types (iclf
		>: fwritec '!' >: write_clean_type argument_type >: fwritec separator_char);

write_clean_type (CleanType is_unique s) iclf
	= iclf >: write_unq_attribute is_unique >: fwrites s;
write_clean_type (CleanTupleType is_unique t) iclf
	= iclf >: write_unq_attribute is_unique >: fwritec '(' >: write_clean_argument_types ',' t >: fwritec ')';
write_clean_type CleanErrorType iclf
	= iclf;

write_unq_attribute is_unique clf
	| is_unique
		= clf >: fwritec '*';
	= clf;

write_imports [] clf
	= clf;
write_imports [Import module_name:imports_and_includes] clf
	= write_imports imports_and_includes (clf >: fwrites "import " >: fwrites module_name >: fwrites ";\n");
write_imports [Include _:imports_and_includes] clf
	= write_imports imports_and_includes clf;

write_dcl_file module_name h_imports h_constants h_types h_functions dclf
	= dclf
		>: fwrites "definition module " >: fwrites module_name >: fwrites ";\n\n"
		>: write_imports h_imports
		>: write_dcl_file_types h_types >: write_dcl_file_functions h_functions >: write_file_constants h_constants;
{
	write_dcl_file_functions [CFunction function_name result_type arguments _:h_info] dclf
		= case (find_clean_function function_name h_functions) of {
			[CleanFunction _ clean_argument_types clean_result_type:_]
				# dclf = dclf
					>: fwrites function_name >: fwrites " :: "
					>: write_clean_function_type clean_argument_types clean_result_type >: fwrites ";\n"
					>: write_c_function_type_in_icl_or_dcl_file result_type function_name arguments;
				-> write_dcl_file_functions h_info dclf;
			_
				# dclf = dclf
					>: fwrites function_name >: fwrites " :: "
					>: write_clean_from_c_function_type arguments result_type >: fwrites ";\n"
					>: write_c_function_type_in_icl_or_dcl_file result_type function_name arguments;
				-> write_dcl_file_functions h_info dclf;
		  };
	write_dcl_file_functions [CleanFunction s1 at rt:h_info] dclf
		= write_dcl_file_functions h_info dclf;
	write_dcl_file_functions [] dclf
		= dclf;
}

write_c_function_type_in_icl_or_dcl_file result_type function_name arguments f
	= f >: fwrites "// " >: fwrites result_type >: fwritec ' ' >: fwrites function_name >: fwrites " (" >: write_c_function_arguments arguments >: fwrites ");\n";

write_c_function_arguments NilFunctionArgumentTypes iclf
	= iclf;
write_c_function_arguments (FunctionArgumentType s optional_argument_name NilFunctionArgumentTypes) iclf
	= iclf >: fwrites s >: write_c_optional_argument_name optional_argument_name;
write_c_function_arguments (FunctionArgumentType s optional_argument_name arguments) iclf
	= write_c_function_arguments arguments (iclf >: fwrites s >: write_c_optional_argument_name optional_argument_name >: fwritec ',');

write_c_optional_argument_name NoArgumentName iclf
	=	iclf;
write_c_optional_argument_name (ArgumentName argument_name) iclf
	=	iclf >: fwrites " " >: fwrites argument_name;

write_clean_from_c_function_type NilFunctionArgumentTypes result_type clf
	= clf	>: fwrites (c_type_to_clean_type result_type);
write_clean_from_c_function_type arguments result_type clf
	= clf	>: write_clean_from_c_function_argument_types arguments  >: fwrites " -> "
			>: fwrites (c_type_to_clean_type result_type);

write_clean_from_c_function_argument_types NilFunctionArgumentTypes iclf
	= iclf;
write_clean_from_c_function_argument_types (FunctionArgumentType s _ NilFunctionArgumentTypes) iclf
	= iclf >: fwritec '!' >: fwrites (c_type_to_clean_type s);
write_clean_from_c_function_argument_types (FunctionArgumentType s _ arguments) iclf
	= write_clean_from_c_function_argument_types arguments
		(iclf >: fwritec '!' >: fwrites (c_type_to_clean_type s) >: fwritec ' ');

c_type_to_clean_type "int"
	= "Int";
c_type_to_clean_type "double"
	= "Real";
c_type_to_clean_type "CleanString"
	= "{#Char}";
c_type_to_clean_type "CleanIntArray"
	= "{#Int}";
c_type_to_clean_type "CleanRealArray"
	= "{#Real}";
c_type_to_clean_type "CleanCharArray"
	= "{#Char}";
c_type_to_clean_type t
	# s=size t;
	| s>0 && t.[s-1]=='*'
//		= c_type_to_clean_type (t % (0,s-2)) +++ "Ptr";
		= "Int";
		= t;

expand_typedefs_in_type c_type h_types = expand_typedefs_in_type_except c_type h_types [];

expand_typedefs_in_type_to_typedef c_type typedef_name h_types = expand_typedefs_in_type_except c_type h_types [typedef_name];

expand_typedefs_in_type_except c_type h_types already_expanded_typedefs
	| c_type=="int" || c_type=="char" || c_type=="double" || c_type=="CleanString" || c_type=="CleanIntArray" || c_type=="CleanRealArray" || c_type=="CleanCharArray"
		= c_type;
	| occurs_in_list c_type already_expanded_typedefs
		= c_type;
		= case (find_c_type c_type h_types) of {
			[CTypeDefinition _ expanded_c_type:_]
				-> expand_typedefs_in_type_except expanded_c_type h_types [c_type:already_expanded_typedefs];
			_
				-> c_type;
		};

is_basic_clean_type clean_type_name
	=	clean_type_name=="Int" || clean_type_name=="Char" || clean_type_name=="Bool" || clean_type_name=="Real"
		|| clean_type_name=="String" || clean_type_name=="World";

basic_clean_and_c_types_ok c_type clean_type_name
	=	(c_type=="int" && (clean_type_name=="Int" || clean_type_name=="Char" || clean_type_name=="Bool")) ||
		(c_type=="char" && clean_type_name=="Char") ||
		(c_type=="double" && clean_type_name=="Real") ||
		(c_type=="CleanString" && clean_type_name=="String") ||
		(c_type=="CleanIntArray" && clean_type_name=="{#Int}") ||
		(c_type=="CleanRealArray" && clean_type_name=="{#Real}") ||
		(c_type=="CleanCharArray" && clean_type_name=="{#Char}") ||
		(size c_type>0 && c_type.[size c_type-1]=='*' && clean_type_name=="Int");

basic_clean_and_c_result_types_ok c_type clean_type_name
	=	(c_type=="int" && (clean_type_name=="Int" || clean_type_name=="Char" || clean_type_name=="Bool")) ||
		(c_type=="char" && clean_type_name=="Char") ||
		(c_type=="double" && clean_type_name=="Real") ||
		(c_type=="CleanString" && clean_type_name=="String") ||
		(size c_type>0 && c_type.[size c_type-1]=='*' && clean_type_name=="Int");

remove_pointer_from_c_type c_type all_h_info
	# size_c_type=size c_type;
	| size_c_type>0 && c_type.[size_c_type-1]=='*'
		= (True,c_type % (0,size_c_type-2));
		= case (find_c_type c_type all_h_info) of {
			[CTypeDefinition _ expanded_c_type:_]
				# size_expanded_c_type=size expanded_c_type;
				| size_expanded_c_type>0 && expanded_c_type.[size_c_type-1]=='*'
					-> (True,expanded_c_type % (0,size_expanded_c_type-2));
			_
				-> (False,c_type);
		  };

write_clean_c_function_arguments argument_n NilFunctionArgumentTypes iclf
	= iclf;
write_clean_c_function_arguments argument_n (FunctionArgumentType _ _ NilFunctionArgumentTypes) iclf
	= iclf >: fwritec 'a' >: fwritei argument_n;
write_clean_c_function_arguments argument_n (FunctionArgumentType _ _ arguments) iclf
	= write_clean_c_function_arguments (argument_n+1) arguments
		(iclf >: fwritec 'a' >: fwritei argument_n >: fwritec ' ');

write_clean_function_arguments argument_n NilCleanTypes iclf
	= iclf;
write_clean_function_arguments argument_n (CleanTypes _ NilCleanTypes) iclf
	= iclf >: fwritec 'a' >: fwritei argument_n;
write_clean_function_arguments argument_n (CleanTypes _ arguments) iclf
	= write_clean_function_arguments (argument_n+1) arguments
		(iclf >: fwritec 'a' >: fwritei argument_n >: fwritec ' ');

write_ccall_arguments NilFunctionArgumentTypes h_types iclf
	= iclf;
write_ccall_arguments (FunctionArgumentType s _ arguments) h_types iclf
	= write_ccall_arguments arguments h_types (iclf >: write_ccall_type_char s h_types);

write_ccall_result_arguments NilFunctionArgumentTypes h_types iclf
	= iclf;
write_ccall_result_arguments (FunctionArgumentType s _ arguments) h_types iclf
	= write_ccall_result_arguments arguments h_types (iclf >: write_ccall_result_type_char s h_types);

write_ccall_result_type_char s h_types clf
	| size s>0 && s.[size s-1]=='*'
		= write_ccall_type_char (s % (0,size s-2)) h_types clf;
	# s=expand_typedefs_in_type s h_types;
	| size s>0 && s.[size s-1]=='*'
		= write_ccall_type_char (s % (0,size s-2)) h_types clf;	

write_ccall_type_char s h_types clf
	# s=expand_typedefs_in_type s h_types;
	| s=="int"
		= fwritec 'I' clf;
	| size s>0 && s.[size s-1]=='*'
		= fwritec 'p' clf;
	| s=="char"
		= fwritec 'I' clf;
	| s=="double"
		= fwritec 'R' clf;
	| s=="void"
		= fwritec 'V' clf;
	| s=="CleanString"
		= fwritec 'S' clf;
	| s=="CleanIntArray" || s=="CleanRealArray"
		= fwritec 'A' clf;
	| s=="CleanCharArray"
		= fwritec 's' clf;

:: HF = {
			hf_constants::![HDefinition],
			hf_types::![HDefinition],
			hf_functions::![HDefinition],
			hf_includes_and_imports::![HDefinition],
			hf_all_types::![HDefinition],
			hf_errors::![HError]
		};

reverse_h_info [n=:Constant s1 s2:h_info] hf
	= reverse_h_info h_info {hf & hf_constants=[n:hf.hf_constants]};
reverse_h_info [n=:Enum enum_constants:h_info] hf
	= reverse_h_info h_info {hf & hf_constants=[n:hf.hf_constants]};
reverse_h_info [n=:CFunction s1 s2 a extra_ccall_arg_types:h_info] hf
	= reverse_h_info h_info {hf & hf_functions=[n:hf.hf_functions]};
reverse_h_info [n=:CleanFunction s1 at rt:h_info] hf
	= reverse_h_info h_info {hf & hf_functions=[n:hf.hf_functions]};
reverse_h_info [n=:CTypeDefinition s1 s2:h_info] hf
	= reverse_h_info h_info {hf & hf_types=[n:hf.hf_types]};
reverse_h_info [n=:CleanTypeDefinition s1 s2 is_unique_type _:h_info] hf
	= reverse_h_info h_info {hf & hf_types=[n:hf.hf_types]};
reverse_h_info [ErrorHFile error_s line_n:h_info] hf
	= reverse_h_info h_info {hf & hf_errors=[HError error_s line_n:hf.hf_errors]};
reverse_h_info [n=:Include file_name:h_info] hf
	= reverse_h_info h_info {hf & hf_includes_and_imports=[n:hf.hf_includes_and_imports]};
reverse_h_info [n=:Import file_name:h_info] hf
	= reverse_h_info h_info {hf & hf_includes_and_imports=[n:hf.hf_includes_and_imports]};
reverse_h_info [] hf
	= hf;

append_clean_types NilCleanTypes types = types;
append_clean_types (CleanTypes clean_type clean_types) types = CleanTypes clean_type (append_clean_types clean_types types);

check_interface_function_argument_types function_name clean_argument_types c_arguments all_h_info errors
	= check_interface_function_argument_types clean_argument_types c_arguments errors NilCleanTypes;
{
	check_interface_function_argument_types clean_argument_types NilFunctionArgumentTypes errors extra_clean_arguments
		= (errors,clean_argument_types);
	check_interface_function_argument_types NilCleanTypes _ errors extra_clean_arguments
		= (errors,extra_clean_arguments);
	check_interface_function_argument_types (CleanTypes clean_type clean_argument_types) (FunctionArgumentType c_type _ c_arguments) errors extra_clean_arguments
		# (c_arguments,errors,extra_clean_arguments) = check_interface_type clean_type c_type c_arguments errors extra_clean_arguments;
		= check_interface_function_argument_types clean_argument_types c_arguments errors extra_clean_arguments;

	check_interface_type (CleanType _ clean_type_name) c_type c_arguments errors extra_clean_arguments
		| is_c_typedef clean_type_name all_h_info && expand_typedefs_in_type_to_typedef c_type clean_type_name all_h_info<>clean_type_name
			= (c_arguments,[HError ("Error in function type of: "+++function_name) 0: errors],extra_clean_arguments);

			# c_type = expand_typedefs_in_type c_type all_h_info;
			| basic_clean_and_c_types_ok c_type clean_type_name
				= (c_arguments,errors,extra_clean_arguments);
				= case (find_clean_type clean_type_name all_h_info) of {
					[CleanTypeDefinition _ clean_type _ _:_]
						-> check_interface_type clean_type c_type c_arguments errors extra_clean_arguments;
					_
						-> (c_arguments,[HError ("Error in function type of: "+++function_name) 0:errors],extra_clean_arguments);
				};
	check_interface_type (CleanTupleType _ (CleanTypes (CleanType _ first_tuple_element_type_name) clean_tuple_argument_types)) c_type c_arguments errors extra_clean_arguments
		# c_type = expand_typedefs_in_type c_type all_h_info;
		| basic_clean_and_c_types_ok c_type first_tuple_element_type_name
			= check_interface_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_arguments;
			= case (find_clean_type first_tuple_element_type_name all_h_info) of {
				[CleanTypeDefinition _ clean_type _ _:_]
					#  (c_arguments,errors,extra_clean_arguments) = check_interface_type clean_type c_type c_arguments errors extra_clean_arguments;
					-> check_interface_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_arguments;
				_
					#  errors = [HError ("Error in function type of: "+++function_name) 0:errors];
					-> check_interface_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_arguments;
			};

	check_interface_type (CleanTupleType _ (CleanTypes (CleanTupleType _ first_tuple_type) clean_tuple_argument_types)) c_type c_arguments errors extra_clean_arguments
		= check_interface_type (CleanTupleType UnqDon`tCare (append_clean_types first_tuple_type clean_tuple_argument_types)) c_type c_arguments errors extra_clean_arguments;

	check_interface_tuple_arguments NilCleanTypes c_arguments errors extra_clean_arguments
		= (c_arguments,errors,extra_clean_arguments);
	check_interface_tuple_arguments clean_tuple_argument_types NilFunctionArgumentTypes errors extra_clean_arguments
		= (c_arguments,errors,append_clean_types clean_tuple_argument_types extra_clean_arguments);
	check_interface_tuple_arguments (CleanTypes (CleanType _ tuple_element_type_name) clean_tuple_argument_types) (FunctionArgumentType c_argument _ c_arguments) errors extra_clean_arguments
		# c_argument = expand_typedefs_in_type c_argument all_h_info;
		| basic_clean_and_c_types_ok c_argument tuple_element_type_name
			= check_interface_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_arguments;
			# errors = [HError ("Error in function type of: "+++function_name) 0:errors];
			= check_interface_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_arguments;

	check_interface_tuple_arguments (CleanTypes (CleanTupleType _ first_tuple_arguments) clean_tuple_argument_types) function_argument_types errors extra_clean_arguments
		= check_interface_tuple_arguments (append_clean_types first_tuple_arguments clean_tuple_argument_types) function_argument_types errors extra_clean_arguments;

}

is_c_typedef c_type all_h_info
	| c_type=="int" || c_type=="char" || c_type=="double" || c_type=="CleanString" || c_type=="CleanIntArray" || c_type=="CleanRealArray" || c_type=="CleanCharArray"
			|| (size c_type>0 && c_type.[size c_type-1]=='*')
		= False;
		= case (find_c_type c_type all_h_info) of {
			[CTypeDefinition _ expanded_c_type:_]
				-> True;
			_
				-> False;
		};

check_interface_result_type function_name c_result_type clean_type c_arguments all_h_info errors
	| c_result_type=="void"
		# (_,errors,extra_result_types)=check_interface_void_result_type clean_type c_arguments errors;
		= (errors,extra_result_types);
		# (_,errors,extra_result_types)=check_interface_non_pointer_result_type clean_type c_result_type c_arguments errors NilCleanTypes;
		= (errors,extra_result_types);
where {
	check_interface_void_result_type (CleanType _ clean_type_name) (FunctionArgumentType c_type _ c_arguments) errors
		# (is_pointer,c_type) = remove_pointer_from_c_type c_type all_h_info;
		| not is_pointer
			= (c_arguments,[HError ("Error in function type of: "+++function_name) 0:errors],NilCleanTypes);
			= check_interface_non_pointer_result_type (CleanType UnqDon`tCare clean_type_name) c_type c_arguments errors NilCleanTypes;
	check_interface_void_result_type (CleanTupleType _ (CleanTypes (CleanType _ first_tuple_element_type_name) clean_tuple_argument_types)) (FunctionArgumentType c_type _ c_arguments) errors
		# (is_pointer,c_type) = remove_pointer_from_c_type c_type all_h_info;
		| not is_pointer
			# errors = [HError ("Error in function type of: "+++function_name) 0:errors];
			= check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors NilCleanTypes;
			# (c_arguments,errors,extra_clean_types) = check_interface_non_pointer_result_type (CleanType UnqDon`tCare first_tuple_element_type_name) c_type c_arguments errors NilCleanTypes;
			= check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;

	check_interface_void_result_type (CleanTupleType _ (CleanTypes (CleanTupleType _ first_tuple_types) clean_tuple_argument_types)) function_argument_types errors
		= 	check_interface_void_result_type (CleanTupleType UnqDon`tCare (append_clean_types first_tuple_types clean_tuple_argument_types)) function_argument_types errors;
		
	check_interface_void_result_type clean_type NilFunctionArgumentTypes errors
		= (NilFunctionArgumentTypes,errors,CleanTypes clean_type NilCleanTypes);
	check_interface_non_pointer_result_type (CleanType _ clean_type_name) c_type c_arguments errors extra_clean_types
		| is_c_typedef clean_type_name all_h_info && expand_typedefs_in_type_to_typedef c_type clean_type_name all_h_info<>clean_type_name
			= (c_arguments,[HError ("Error in function type of: "+++function_name) 0: errors],extra_clean_types);
			
			# c_type = expand_typedefs_in_type c_type all_h_info;
			| basic_clean_and_c_result_types_ok c_type clean_type_name
				= (c_arguments,errors,extra_clean_types);
				= case (find_clean_type clean_type_name all_h_info) of {
					[CleanTypeDefinition _ clean_type _ _:_]
						-> check_interface_non_pointer_result_type clean_type c_type c_arguments errors extra_clean_types;
					_
						-> (c_arguments,[HError ("Error in function type of: "+++function_name) 0: errors],extra_clean_types);
				  };
	check_interface_non_pointer_result_type (CleanTupleType _ (CleanTypes (CleanType _ first_tuple_element_type_name) clean_tuple_argument_types)) c_type c_arguments errors extra_clean_types
		| basic_clean_and_c_result_types_ok c_type first_tuple_element_type_name
			= check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;
			= case (find_clean_type first_tuple_element_type_name all_h_info) of {
				[CleanTypeDefinition _ clean_type _ _:_]
					# (c_arguments,errors,extra_clean_types)=check_interface_non_pointer_result_type clean_type c_type c_arguments errors extra_clean_types;
					-> check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;
				_
					#  errors = [HError ("Error in function type of: "+++function_name) 0:errors];
					-> check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;
			  };
	check_interface_non_pointer_result_type (CleanTupleType _ (CleanTypes (CleanTupleType _ first_tuple_element_types) clean_tuple_argument_types)) c_type c_arguments errors extra_clean_types
		= check_interface_non_pointer_result_type (CleanTupleType UnqDon`tCare (append_clean_types first_tuple_element_types clean_tuple_argument_types)) c_type c_arguments errors extra_clean_types;

	check_interface_result_tuple_arguments NilCleanTypes c_arguments errors extra_clean_types
		= (c_arguments,errors,extra_clean_types);
	check_interface_result_tuple_arguments clean_tuple_argument_types NilFunctionArgumentTypes errors extra_clean_types
		= (c_arguments,errors,append_clean_types clean_tuple_argument_types extra_clean_types);
	check_interface_result_tuple_arguments (CleanTypes (CleanType _ tuple_element_type_name) clean_tuple_argument_types) (FunctionArgumentType c_argument _ c_arguments) errors extra_clean_types
		# (is_pointer,c_argument) = remove_pointer_from_c_type c_argument all_h_info;
		| not is_pointer
			# errors = [HError ("Error in function type of: "+++function_name) 0:errors];
			= check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;
			# (c_arguments,errors,extra_clean_types) = check_interface_non_pointer_result_type (CleanType UnqDon`tCare tuple_element_type_name) c_argument c_arguments errors extra_clean_types;
			= check_interface_result_tuple_arguments clean_tuple_argument_types c_arguments errors extra_clean_types;
	check_interface_result_tuple_arguments (CleanTypes (CleanTupleType _ tuple_elements) clean_tuple_argument_types) (FunctionArgumentType c_argument x c_arguments) errors extra_clean_types
		= check_interface_result_tuple_arguments (append_clean_types tuple_elements clean_tuple_argument_types) (FunctionArgumentType c_argument x c_arguments) errors extra_clean_types;
	check_interface_tuple_arguments (CleanTypes (CleanTupleType _ first_tuple_arguments) clean_tuple_argument_types) function_argument_types errors extra_clean_arguments
		= check_interface_tuple_arguments (append_clean_types first_tuple_arguments clean_tuple_argument_types) function_argument_types errors extra_clean_arguments;
}

/*
write_graph :: !a -> Bool;
write_graph g = code {
.d 1 0
	jsr	_print_graph
.o 0 0
	pushB TRUE
}
*/

check_extra_arguments_and_results extra_clean_arguments extra_clean_results all_h_info function_name errors
	= check_extra_arguments_and_results extra_clean_arguments extra_clean_results "" errors;
{		
	check_extra_arguments_and_results NilCleanTypes NilCleanTypes extra_ccall_arg_types errors
		| size extra_ccall_arg_types==0
			= (extra_ccall_arg_types,errors);
			= (":"+++extra_ccall_arg_types,errors);
	check_extra_arguments_and_results (CleanTypes (CleanType _ clean_argument_type) clean_argument_types) (CleanTypes (CleanType _ clean_result_type) clean_result_types) extra_ccall_arg_types errors
		| is_basic_clean_type clean_argument_type
			| is_basic_clean_type clean_result_type
				# extra_ccall_arg_type = types_to_extra_ccall_arg_type clean_argument_type clean_result_type
				= check_extra_arguments_and_results clean_argument_types clean_result_types (extra_ccall_arg_type+++extra_ccall_arg_types) errors;
				= case (find_clean_type clean_result_type all_h_info) of {
					[CleanTypeDefinition _ clean_result_type _ _:_]
						-> check_extra_arguments_and_results (CleanTypes (CleanType UnqDon`tCare clean_argument_type) clean_argument_types) (CleanTypes clean_result_type clean_result_types) extra_ccall_arg_types errors;
					_
						#  errors = [HError ("Error in extra argument or result types in :"+++function_name) 0:errors];
						-> check_extra_arguments_and_results clean_argument_types clean_result_types extra_ccall_arg_types errors;
				  }
			= case (find_clean_type clean_argument_type all_h_info) of {
				[CleanTypeDefinition _ clean_argument_type _ _:_]
					-> check_extra_arguments_and_results (CleanTypes clean_argument_type clean_argument_types) (CleanTypes (CleanType UnqDon`tCare clean_result_type) clean_result_types) extra_ccall_arg_types errors;
				_
					#  errors = [HError ("Error in extra argument or result types in :"+++function_name) 0:errors];
					-> check_extra_arguments_and_results clean_argument_types clean_result_types extra_ccall_arg_types errors;
			  };
	check_extra_arguments_and_results (CleanTypes (CleanTupleType _ clean_tuple_arguments) clean_argument_types) clean_result_types extra_ccall_arg_types errors
		= check_extra_arguments_and_results (append_clean_types clean_tuple_arguments clean_argument_types) clean_result_types extra_ccall_arg_types errors;
	check_extra_arguments_and_results clean_argument_types (CleanTypes (CleanTupleType _ clean_tuple_result) clean_result_types) extra_ccall_arg_types errors
		= check_extra_arguments_and_results clean_argument_types (append_clean_types clean_tuple_result clean_result_types) extra_ccall_arg_types errors;
	check_extra_arguments_and_results NilCleanTypes result_types extra_ccall_arg_types errors
		= ("",[HError ("Too many extra result types in :"+++function_name) 0:errors]);
	check_extra_arguments_and_results _ NilCleanTypes extra_ccall_arg_types errors
		= ("",[HError ("Too many extra argument types in : "+++function_name) 0:errors]);

	types_to_extra_ccall_arg_type clean_argument_type clean_result_type
		| (clean_argument_type=="World" || clean_argument_type=="String") && (clean_result_type=="World" || clean_result_type=="String")
			= "A";
			= "p";
}

not_nil [] = False;
not_nil _ = True;

synonym_cycle_error function_name [type:_] cycles2
	= "Cycle in type synonym: "+++type+++" in function: "+++function_name;
synonym_cycle_error function_name [] cycles2=:[type:_]
	= synonym_cycle_error function_name cycles2 [];

check_interface h_all_types h_functions errors
	= check_interface h_functions errors;
{
	check_interface [CFunction function_name result_type arguments extra_ccall_arg_types:h_info] errors
		= case (find_clean_function function_name h_functions) of {
			[CleanFunction _ clean_argument_types clean_result_type:_]
				# (n_clean_arguments,cycles1) = arity_and_cycles_of_clean_types clean_argument_types h_all_types;
				# (n_clean_results,cycles2) = arity_and_cycles_of_clean_type clean_result_type h_all_types;
				| not_nil cycles1 || not_nil cycles2
					# (h_info,errors) = check_interface h_info errors;
					# errors = [HError (synonym_cycle_error function_name cycles1 cycles2) 0:errors];
					-> ([CFunction function_name result_type arguments extra_ccall_arg_types:h_info],errors);
				# n_c_arguments = add_arity_of_c_types arguments (add_arity_of_c_type result_type 0);
				# n_extra_arguments_2 = n_clean_arguments+n_clean_results-n_c_arguments;
				# n_extra_arguments = n_extra_arguments_2 >> 1;
				| n_clean_arguments+n_clean_results < n_c_arguments
					# (h_info,errors)=check_interface h_info [HError ("Too few arguments or results in clean type of: "+++function_name) 0:errors];
					-> ([CFunction function_name result_type arguments extra_ccall_arg_types:h_info],errors);
				| n_extra_arguments_2 bitand 1<>0
					-> check_interface h_info [HError "Incorrect number of arguments or results in clean type" 0:errors];
					#  (input_arguments,rest_arguments) = take_n_arguments2 (n_clean_arguments-n_extra_arguments) arguments;
					#  output_arguments=take_n_arguments (n_clean_results-n_extra_arguments) rest_arguments;
					#  (errors,extra_clean_arguments) = check_interface_function_argument_types function_name clean_argument_types input_arguments h_all_types errors;
					#  (errors,extra_clean_results) = check_interface_result_type function_name result_type clean_result_type output_arguments h_all_types errors;
					#  (extra_ccall_arg_types,errors) = check_extra_arguments_and_results extra_clean_arguments extra_clean_results h_all_types function_name errors;
					#  (h_info,errors) = check_interface h_info errors;
					-> ([CFunction function_name result_type arguments extra_ccall_arg_types:h_info],errors);
			_
				# errors=errors >: check_c_result_type result_type h_all_types >: check_c_arguments arguments h_all_types;
				# (h_info,errors) = check_interface h_info errors;				 
				-> ([CFunction function_name result_type arguments extra_ccall_arg_types:h_info],errors);
				where {
					check_c_arguments NilFunctionArgumentTypes h_all_types errors
						= errors;
					check_c_arguments (FunctionArgumentType s _ l) h_all_types errors
						= check_c_arguments l h_all_types (check_c_argument_type s h_all_types errors);

					check_c_argument_type s1 h_types errors
						# s=expand_typedefs_in_type s1 h_types;
						| s=="int" || (size s>0 && s.[size s-1]=='*') || s=="char" || s=="double" || s=="void" || s=="CleanString" || s=="CleanIntArray" || s=="CleanRealArray" || s=="CleanCharArray"
							= errors;
							= [HError ("Error in argument type: '"+++toString s1+++"' of function: "+++function_name) 0:errors];

					check_c_result_type s1 h_types errors
						# s=expand_typedefs_in_type s1 h_types;
						| s=="int" || (size s>0 && s.[size s-1]=='*') || s=="char" || s=="double" || s=="CleanString"
							= errors;
							= [HError ("Error in result type: '"+++toString s1+++"' of function: "+++function_name) 0:errors];
				}
		  };
	check_interface [CleanFunction function_name argument_types result_type:h_info] errors
		# (h_info,errors) = check_interface h_info errors;
		= ([CleanFunction function_name argument_types result_type:h_info],errors);
	check_interface [] errors
		= ([],errors);

}

file_name_occurs_in_list file_name [] = False;
file_name_occurs_in_list file_name [e:l] = e==file_name || file_name_occurs_in_list file_name l;

append_included_hf_lists hf [] = hf;
append_included_hf_lists hf [e:l] = append_included_hf hf (append_included_hf_lists e l);
{
	append_included_hf hf=:{hf_constants,hf_types,hf_functions,hf_includes_and_imports,hf_errors} hf2
		= {	hf &
			hf_constants=hf_constants++hf2.hf_constants,
			hf_types=hf_types++hf2.hf_types,
			hf_functions=hf_functions++hf2.hf_functions,
			hf_includes_and_imports=hf_includes_and_imports++hf2.hf_includes_and_imports,
			hf_errors=hf_errors++hf2.hf_errors
		  };
}

append_imported_hf_lists [] types errors
	= (types,errors);
append_imported_hf_lists [e:l] types errors
	# (types,errors) = append_imported_hf_lists l types errors;
	= (e.hf_types++types,e.hf_errors++errors);

read_h_files directory_name h_file_name files
	# (hf,files) = read_h_file_without_include_files h_file_name files;
 	# (hf_list,file_name_list,import_list,files) = read_included_files hf.hf_includes_and_imports [] [h_file_name] [] files;
 	# hf = append_included_hf_lists hf (reverse hf_list);
	# (imported_hf_list,file_name_list,files) = read_imported_files import_list [] file_name_list files;
	# (imported_types,imported_errors) = append_imported_hf_lists (reverse imported_hf_list) [] [];
	# hf = {hf & hf_errors=hf.hf_errors++imported_errors,hf_all_types=hf.hf_types++imported_types};
	= (hf,files);
{
	read_h_file_without_include_files h_file_name files
		# hf={hf_constants=[],hf_types=[],hf_functions=[],hf_includes_and_imports=[],hf_errors=[],hf_all_types=[]};
		# (ok,hfile,files) = fopen (directory_name+++h_file_name) FReadText files;
		| not ok
			= ({hf & hf_errors=[HError ("Cannot read file: "+++directory_name+++h_file_name) 0]},files);
		# (h_info,hfile) = read_h_file [] 1 hfile;
		  (ok,files) = fclose hfile files;
 		  hf = reverse_h_info h_info hf;
		= (hf,files);

	read_included_files include_list hf_list file_name_list import_list files
	 = case include_list of {
		[]
			-> ([],file_name_list,import_list,files);
		[Include h_file_name:include_list]
			| file_name_occurs_in_list h_file_name file_name_list
				-> read_included_files include_list hf_list file_name_list import_list files;
				# (hf,files) = read_h_file_without_include_files h_file_name files;
		 		# (hf_list,file_name_list,import_list,files) = read_included_files hf.hf_includes_and_imports hf_list [h_file_name:file_name_list] import_list files;
				# (hf_list,file_name_list,import_list,files) = read_included_files include_list hf_list file_name_list import_list files;
				-> ([hf:hf_list],file_name_list,import_list,files);
		[Import h_file_name:include_list]
			-> read_included_files include_list hf_list file_name_list [Import h_file_name:import_list] files;
	};

	read_imported_files [] hf_list file_name_list files
		= ([],file_name_list,files);
	read_imported_files include_list hf_list file_name_list files
		= case include_list of {
			[Include h_file_name:include_list] -> read_imported_or_included_file h_file_name include_list;
			[Import h_file_name:include_list]  -> read_imported_or_included_file (h_file_name+++".h") include_list;
		}
		where {
			 read_imported_or_included_file h_file_name include_list
				| file_name_occurs_in_list h_file_name file_name_list
					= read_imported_files include_list hf_list file_name_list files;
					# (hf,files) = read_h_file_without_include_files h_file_name files;
				 	# (hf_list,file_name_list,files) = read_imported_files hf.hf_includes_and_imports hf_list [h_file_name:file_name_list] files;
					# (hf_list,file_name_list,files) = read_imported_files include_list hf_list file_name_list files;
					= ([hf:hf_list],file_name_list,files);
		}
}

compile_header :: !{#Char} !{#Char} !*Files -> *(![HError],!*Files);
compile_header directory_name file_name files
	# (hf,files) = read_h_files directory_name (file_name+++".h") files;
	| not (case hf.hf_errors of {[] -> True ; _ -> False})
		= (hf.hf_errors,files);
	# (hf_functions,interface_errors) = check_interface hf.hf_all_types hf.hf_functions [];
	# hf = {hf & hf_functions=hf_functions};
	| not (case interface_errors of {[] -> True ; _ -> False})
	  = (interface_errors,files);
	# icl_file_name = directory_name+++file_name+++".icl";
	# (ok,iclf,files) = fopen icl_file_name FWriteText files;
	| not ok
		= ([HError ("Cannot create file: "+++icl_file_name) 0],files);
 	# iclf=write_icl_file file_name hf.hf_includes_and_imports hf.hf_constants hf.hf_types hf.hf_functions hf.hf_all_types iclf;
	  (ok,files) = fclose iclf files;
 	  dcl_file_name = directory_name+++file_name+++".dcl";
 	  (ok,dclf,files) = fopen dcl_file_name FWriteText files;
	| not ok
		= ([HError ("Cannot create file: "+++dcl_file_name) 0],files);
 	# dclf=write_dcl_file file_name hf.hf_includes_and_imports hf.hf_constants hf.hf_types hf.hf_functions dclf;
	  (ok,files) = fclose dclf files;
	= (hf.hf_errors,files);
/*
Start :: *World -> (![HError],!*World);
Start world
//	= accFiles (compile_header "" "interface") world;
	= accFiles (compile_header "" "types") world;
*/
