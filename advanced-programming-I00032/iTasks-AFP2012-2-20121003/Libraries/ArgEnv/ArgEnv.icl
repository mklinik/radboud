/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
implementation module ArgEnv

import code from "ArgEnvC."

import StdEnv

:: CString :== Int
NULL :== 0

:: EnvironmentVariable
    =	EnvironmentVariableUndefined
    |   EnvironmentVariable !.{#Char}


getEnvSize :: !{#Char} -> Int
getEnvSize _
	= code inline {
			ccall ArgEnvGetEnvironmentVariableSizeC "S-I"
	}

copyEnv :: !Int !{#Char} -> {#.Char}
copyEnv _ _
	= code inline {
			create_array_	CHAR 0 1

			push_a	1
			push_a	1
			ccall	ArgEnvGetEnvironmentVariableCharsC "SS-I"
			pop_b	1
			update_a	0 1
			pop_a	1
	}

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable name
	| size == 0
		=	EnvironmentVariableUndefined
	| otherwise
		=	EnvironmentVariable (copyEnv size name`)
	where
		size
			=	getEnvSize name`
		name`
			=	name +++ "\0"

copy :: !Int !CString -> {#.Char}
copy length cString
	= code inline {
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
		.end
	}

getCommandLineCount :: Int
getCommandLineCount 
	= code inline {
			ccall ArgEnvGetCommandLineCountC "-I"
	}

getCommandLineArgument :: !Int -> (!Int, !Int)
getCommandLineArgument _
	= code inline {
			ccall ArgEnvGetCommandLineArgumentC "I-II"
	}

getArg :: !Int -> {#.Char}
getArg i
	=	copy size cString
	where
		(size, cString)
			=	getCommandLineArgument i

getCommandLine :: {.{#Char}}
getCommandLine
	=	{getArg i \\ i <- [0 .. getCommandLineCount-1]}
