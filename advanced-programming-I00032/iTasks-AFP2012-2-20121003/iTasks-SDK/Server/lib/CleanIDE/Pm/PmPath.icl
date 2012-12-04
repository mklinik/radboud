implementation module PmPath

import StdClass,StdString, StdChar, StdBool, StdChar,StdInt, StdMisc,StdArray;
import StdPathname

import PmTypes
import Platform
import UtilStrictLists

/* The name of the system directory */
SystemDir			:== "Clean System Files";

//--

IsDefPathname :: !Pathname -> Bool;
IsDefPathname name =  equal_suffix ".dcl" name;

IsImpPathname :: !Pathname -> Bool;
IsImpPathname name =  equal_suffix ".icl" name;
	
IsABCPathname :: !Pathname -> Bool;
IsABCPathname name =  equal_suffix ".abc" name;
	
IsPrjPathname :: !Pathname -> Bool;
IsPrjPathname name =  equal_suffix ".prj" name;

MakeDefPathname	:: !String -> Pathname;
MakeDefPathname name =  RemoveSuffix name  +++ ".dcl";

MakeImpPathname	:: !String -> Pathname;
MakeImpPathname name = RemoveSuffix name  +++ ".icl";
			
MakeABCPathname	:: !String -> Pathname;
MakeABCPathname name = RemoveSuffix name  +++ ".abc";
	
MakeObjPathname	:: !Processor !String -> Pathname;
MakeObjPathname processor name
	= RemoveSuffix name +++ ProcessorSuffix processor
/*
	| processor == CurrentProcessor
//		= RemoveSuffix name  +++ ".o";
		= RemoveSuffix name  +++ ".xo";
	| processor == MC68000
		= RemoveSuffix name +++ ".obj0";
	| processor == MC68020
		= RemoveSuffix name +++ ".obj1";
	| processor == MC68020_and_68881
		= RemoveSuffix name +++ ".obj2";
		= abort ("MakeObjPathname: " +++  toString processor +++ " : No such processor ");
*/	
MakeProjectPathname	:: !String -> Pathname;
MakeProjectPathname name = RemoveSuffix name   +++ ".prj";

MakeExecPathname :: !String -> Pathname;
MakeExecPathname name
	= PlatformDependant
		(RemoveSuffix name+++".exe")	// Win
		(RemoveSuffix name)				// Mac

MakeSystemPathname :: !Pathname -> Pathname;
MakeSystemPathname pathname
	| equal_suffix SystemDir pathname
		= pathname
	| size pathname > 0 && pathname.[size pathname - 1] == DirSeparator
		= pathname +++ SystemDir
	| otherwise
		= pathname +++ sep +++ SystemDir;
where
	sep = toString DirSeparator;

MakeABCSystemPathname :: !Pathname -> Pathname
MakeABCSystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ ".abc"
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix abcname);
	
MakeObjSystemPathname :: !Processor !Pathname -> Pathname
MakeObjSystemPathname processor name
	= files_and_path (ProcessorSuffix processor)
/*
	| processor == CurrentProcessor
//		= files_and_path ".o";
		= files_and_path ".xo";
	| processor == MC68000
		= files_and_path ".obj0";
	| processor == MC68020
		= files_and_path ".obj1";
	| processor == MC68020_and_68881
		= files_and_path ".obj2";
		= abort ("MakeObjSystemPathname: " +++  toString processor +++ " : No such processor ");
*/
where
		files_and_path extension = directory_name_plus_system_dir +++ sep +++ file+++extension
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename name;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix name);

MakeAssemblySystemPathname :: !Pathname -> Pathname
MakeAssemblySystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ suffix
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix abcname);
		suffix	= PlatformDependant
					".s"	// Win
					".a"	// Mac
	
/*
MakeABCSystemPathname :: !Pathname !Files -> (!Pathname,!Files);
MakeABCSystemPathname abcname files
	= (directory_name_plus_system_dir +++ sep +++ file +++ ".abc",files);
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix abcname);
	
MakeObjSystemPathname :: !Processor !Pathname !Files -> (!Pathname,!Files);
MakeObjSystemPathname processor name files
	| processor == CurrentProcessor
		= files_and_path ".o";
	| processor == MC68000
		= files_and_path ".obj0";
	| processor == MC68020
		= files_and_path ".obj1";
	| processor == MC68020_and_68881
		= files_and_path ".obj2";
		= abort ("MakeObjSystemPathname: " +++  toString processor +++ " : No such processor ");
where
		files_and_path extension = (directory_name_plus_system_dir +++ sep +++ file+++extension,files);
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename name;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix name);
*/	

GetModuleName :: !Pathname -> Modulename;
GetModuleName name =  RemoveSuffix (RemovePath name);

//==

symPath :: !Pathname !Pathname !Pathname -> Pathname
symPath ap pp l
	| size ap >= size pp		// generate shortest symbolic path...
		#	l = replace_prefix_path ap "{Application}" l
			l = replace_prefix_path pp "{Project}" l
		= l
	| otherwise
		#	l = replace_prefix_path pp "{Project}" l
			l = replace_prefix_path ap "{Application}" l
		= l

fulPath :: !Pathname !Pathname !Pathname -> Pathname
fulPath ap pp l
	#	l = replace_prefix_path "{Application}" ap l
		l = replace_prefix_path "{Project}" pp l
	// ensure full pathname is just that...
	| IsFullPathname l
		= l
	// if not put it in the project directory...
	= MakeFullPathname pp l

get_separator_and_convert_path :: !Pathname -> (!Char,!Pathname) 
get_separator_and_convert_path path
	# prefix="{Project}"
	# prefix_size=size prefix
	# i=first_not_equal_character_index prefix path
	| i==prefix_size && size path>prefix_size
		= replace_prefix path.[prefix_size] path
	# prefix="{Application}"
	# prefix_size=size prefix
	# i=first_not_equal_character_index prefix path
	| i==prefix_size && size path>prefix_size
		= replace_prefix path.[prefix_size] path
	= (DirSeparator,path)
where
	replace_prefix separator path
		| separator==DirSeparator
			= (separator,path)
			= (separator,replace_character_in_string separator DirSeparator path)
	
	replace_character_in_string :: !Char !Char !{#Char} -> {#Char}
	replace_character_in_string old_c new_c string
		= {if (c==old_c) new_c c \\ c<-:string}

	first_not_equal_character_index s1 s2
		#! max_index=if (size s1<=size s2) (size s1) (size s2)
		= first_not_equal_character_index 0 max_index s1 s2
		where
			first_not_equal_character_index :: !Int !Int !{#Char} !{#Char} -> Int
			first_not_equal_character_index i n s1 s2
				| i<n && s1.[i]==s2.[i]
					= first_not_equal_character_index (i+1) n s1 s2
					= i

convert_path_separators :: !Pathname -> Pathname 
convert_path_separators path
	# (separator,path) = get_separator_and_convert_path path
	= path

convert_exec_path_separators_and_extension :: !Pathname -> Pathname 
convert_exec_path_separators_and_extension path
	# (separator,path) = get_separator_and_convert_path path
	| separator==DirSeparator
		= path
	| DirSeparator=='\\'
		= path+++".exe"
	| separator=='\\'
		# l=size path
		| l>4 && path.[l-4]=='\\'
			&& (path.[l-3]=='e' || path.[l-3]=='e')
			&& (path.[l-2]=='x' || path.[l-2]=='x')
			&& (path.[l-1]=='e' || path.[l-1]=='e')
			= path % (0,l-4)
		= path
	= path

symPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
symPaths ap pp l = Map (symPath ap pp) l

fulPaths :: !Pathname !Pathname !(List Pathname) -> List Pathname
fulPaths ap pp l = Map (fulPath ap pp) l

symAppPath :: !Pathname !Pathname -> Pathname
symAppPath ap p
	= replace_prefix_path ap "{Application}" p

fulAppPath :: !Pathname !Pathname -> Pathname
fulAppPath ap p
	= replace_prefix_path "{Application}" ap p

symAppPaths :: !Pathname !(List Pathname) -> List Pathname
symAppPaths ap l = Map (symAppPath ap) l

fulAppPaths :: !Pathname !(List Pathname) -> List Pathname
fulAppPaths ap l = Map (fulAppPath ap) l
