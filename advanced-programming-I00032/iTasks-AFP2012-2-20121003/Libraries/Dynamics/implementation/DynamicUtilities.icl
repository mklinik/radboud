implementation module DynamicUtilities

import StdEnv

WriteLong :: !*{#Char} !Int !Int -> *{#Char}
WriteLong array i v
	= { array & [i] 	= (toChar v)		,	[i+1] = (toChar (v>>8)),
				[i+2]	= (toChar (v>>16))  ,	[i+3] = (toChar (v>>24))}

FromStringToInt :: !{#Char} !Int -> Int
FromStringToInt array i
	= (toInt v0)+(toInt v1<<8)+(toInt v2<<16)+(toInt v3<<24)
where
	v0= array.[i]
	v1
		= array.[i+1]
	v2 
		= array.[i+2]
	v3  
		= array.[i+3]

NF :: !.a -> .a
NF _ = code
 {
    push_a 0
    .d 1 0
    jsr _eval_to_nf
    .o 0 0
 }

ends :: !String !String -> Bool;
ends s postfix
	#! s_length
		= size s;
	#! postfix_length 
		= size postfix; 
	= (s % (s_length-postfix_length, s_length-1)) == postfix;

// copied from ExtFile ...
path_separator :== '\\';

ExtractPathAndFile :: !String -> (!String,!String)
ExtractPathAndFile path_and_file 
	#! (dir_delimiter_found,i)
		= CharIndexBackwards path_and_file (size path_and_file - 1) path_separator
	| dir_delimiter_found
		# file_name_with_extension
			= path_and_file % (i+1,size path_and_file - 1)
		= (if (i == 0) (toString path_separator) (path_and_file % (0,i-1)),file_name_with_extension)
		= ("",path_and_file)

ExtractPathFileAndExtension :: !String -> (!String,!String)
ExtractPathFileAndExtension path_and_file 
	| dot_found && not path_separator_after_dot_found
		#! extension
			= path_and_file % (inc dot_index,size path_and_file-1)
		#! pathfile
			= path_and_file % (0, dot_index-1)
		= (pathfile,extension)
		
		= (path_and_file,"")
where
	(dot_found,dot_index)
		= CharIndexBackwards path_and_file (size path_and_file - 1) '.'
	(path_separator_after_dot_found,path_sep_index)
		= CharIndex path_and_file (inc dot_index) path_separator;
// ... copied from ExtFile

// copied from ExtString ....
CharIndex  :: !String !Int !Char -> (!Bool,!Int); 
CharIndex s i char
	| i == (size s)
		= (False,size s);
		
		| i < (size s)
			| s.[i] == char
				= (True,i);
				= CharIndex s (inc i) char;
			= abort "CharIndex: index out of range";
			
CharIndexBackwards :: !String !Int !Char -> (!Bool,!Int);
CharIndexBackwards s i char
	| i == (-1)
		= (False,size s);
		
		| s.[i] == char
			= (True,i);
			= CharIndexBackwards s (dec i) char;
// ... copied from ExtString
