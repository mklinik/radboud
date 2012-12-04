implementation module DynID

import StdEnv
import md5
import DynamicUtilities
from DynamicLinkerInterface import GetDynamicLinkerPath
from StdDynamicTypes import NAME_PREFIXES
import DynIDMacros

::DynamicID1 :== String

createDynIDFromString :: String -> DynamicID1
createDynIDFromString szoveg=
	getMd5DigestFromString szoveg

createDynIDFromFile	:: String *World -> (DynamicID1,*World)
createDynIDFromFile fileName world=
	getMd5DigestFromFile fileName world

instance toString DynamicID1 where
	toString id = id

instance fromString DynamicID1 where
	fromString s = s

// Applications used by dynamics
DS_DYNAMIC_LINKER		:== "DynamicLinker.exe";

// Predefined directories (w.r.t. root-directory):
DS_CONVERSION_DIR		:== "conversion"
DS_UTILITIES_DIR		:== "utilities"
DS_LOGS_DIR				:== "logs"

// The identification of a {type,code}-library is determined by the static linker which uses
// the CREATE_ENCODED_LIBRARY_FILE_NAME-macro to create an identification for a particular
// type/library-pair.
//
// Further the identification of a {user,system}-dynamic is determined by StdDynamicFileIO.
//
// Other tools than the creators of the identifications should respect the here established
// convention by using the macros provided below.
	
// IDS FOR DYNAMICS:
// encoded file_name:												<link_name>_<md5>
// run-time file_name:  	<DLINK PATH>\\<DS_SYSTEM_DYNAMICS_DIR>\\<link_name>_<md5>
CREATE_ENCODED_DYNAMIC_FILE_NAME link_name md5_dynamic_identification
	:== (NAME_PREFIXES (link_name +++ "_") "") +++ md5_dynamic_identification;
	
CONVERTED_ENCODED_DYNAMIC_FILE_NAME_INTO_PATH base_directory encoded_dynamic_file_name
	:== base_directory +++ "\\" +++ DS_SYSTEM_DYNAMICS_DIR +++ "\\" +++ encoded_dynamic_file_name +++ "." +++ EXTENSION_SYSTEM_DYNAMIC;
	
EXTENSION_SYSTEM_DYNAMIC
	:== "sysdyn"
	
extract_dynamic_or_library_identification :: !String -> String
extract_dynamic_or_library_identification file_name
	= (snd (ExtractPathAndFile (fst (ExtractPathFileAndExtension file_name))));

get_system_dynamic_identification :: !String !*f -> (!Bool,!String,!*f) | FileSystem f
get_system_dynamic_identification file_name1 files
	| ends file_name1 ("." +++ EXTENSION_SYSTEM_DYNAMIC)
		= (True,file_name1,files);
	| ends file_name1 ("." +++ EXTENSION_USER_DYNAMIC)
		#! (ok,userFile,files)
			= fopen file_name1 FReadText files
		| not ok
			#! (_,files)
				= fclose userFile files;
			= (False,"",files)
		
		# (databaseFileName,userFile)
			= freadline userFile
		# databaseFileName
			= (%) databaseFileName (0,(size databaseFileName)-2) //for deleting the "\n".
		
		#! (_,files)
			= fclose userFile files;
		= (True,CONVERTED_ENCODED_DYNAMIC_FILE_NAME_INTO_PATH GetDynamicLinkerPath databaseFileName,files)

		= abort "get_system_dynamic_file: no user or system dynamic dropped";
