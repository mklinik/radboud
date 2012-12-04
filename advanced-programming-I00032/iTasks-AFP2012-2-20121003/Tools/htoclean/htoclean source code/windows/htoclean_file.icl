implementation module htoclean_file;

// for windows

import StdEnv;

/*
import code from "program_args.o";

n_args :: Int;
n_args = code {
	ccall n_args "-I"
}

program_arg :: !Int -> {#Char};
program_arg i = code {
	ccall program_arg "I-S"
}
*/

import code from "ArgEnvC.";

copy :: !Int !Int -> {#.Char};
copy length cString
	= code inline {
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
	}

getCommandLineCount :: Int;
getCommandLineCount 
	= code inline {
			ccall ArgEnvGetCommandLineCountC "-I"
	}

getCommandLineArgument :: !Int -> (!Int, !Int);
getCommandLineArgument _
	= code inline {
			ccall ArgEnvGetCommandLineArgumentC "I-II"
	}

getArg :: !Int -> {#.Char};
getArg i
	# (size, cString) = getCommandLineArgument i
	= copy size cString;

DirectorySeparator :== '\\';

wait_for_keypress :: !*World -> *World;
wait_for_keypress w
	# (stdio,w) = stdio w;
	  stdio = stdio <<< "Press any key to exit";
	  (ok,c,stdio) = freadc stdio;
	  (ok,w) = fclose stdio w;
	= w;

get_path_name :: !*World -> (!Bool,!String,!*World);
get_path_name w
//	# n_arguments=n_args;
	# n_arguments=getCommandLineCount;
	| n_arguments<>2
		# stderr=fwrites "Usage: htoclean h_file_name\n" stderr;
		  stderr=fwrites "Generates a .icl and .dcl file for a c header file\n" stderr;
		  (_,w) = fclose stderr w;
		  w = wait_for_keypress w;
		= (False,"",w);
//		# path_name = program_arg 1;
		# path_name = getArg 1;
		= (True,path_name,w);
