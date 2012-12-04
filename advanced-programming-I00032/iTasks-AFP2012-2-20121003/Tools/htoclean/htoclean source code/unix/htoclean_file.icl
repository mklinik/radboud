implementation module htoclean_file;

// for unix

import StdEnv;

import ArgEnv;

args=:getCommandLine;
n_args:==size args;
program_arg i:==args.[i];

DirectorySeparator :== '/';

wait_for_keypress :: !*World -> *World;
wait_for_keypress w
	= w;

get_path_name :: !*World -> (!Bool,!String,!*World);
get_path_name w
	# n_arguments=n_args;
	| n_arguments<>2
		# stderr=fwrites "Usage: htoclean h_file_name\n" stderr;
		  stderr=fwrites "Generates a .icl and .dcl file for a c header file\n" stderr;
		  (_,w) = fclose stderr w;
		= (False,"",w);
		# path_name = program_arg 1;
		= (True,path_name,w);

/*  old unix code ?/
	n_args:==GetArgC;
	program_arg i:==GetArgvN i;
	
	GetArgC :: Int;
	GetArgC = code {
			ccall get_argc ":I"
		}
	
	GetArgvN :: !Int -> String;
	GetArgvN n = code {
			ccall get_argv_n "I:S"
		}
*/
