definition module DynID

from StdOverloaded import class fromString, class toString, class +++ (+++)
from StdFile import class FileSystem
from StdString import instance +++ {#Char}
from StdDynamicTypes import NAME_PREFIXES
import DynIDMacros

::DynamicID1

createDynIDFromString :: String -> DynamicID1
createDynIDFromFile	:: String *World -> (DynamicID1,*World)
	//creates unique ID for the dynamic file named by the string

instance toString DynamicID1
instance fromString DynamicID1

// Applications used by dynamics
DS_DYNAMIC_LINKER		:== "DynamicLinker.exe";

// Predefined directories (w.r.t. root-directory):
DS_CONVERSION_DIR		:== "conversion"
DS_UTILITIES_DIR		:== "utilities"
DS_LOGS_DIR				:== "logs"

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
	
get_system_dynamic_identification :: !String !*f -> (!Bool,!String,!*f) | FileSystem f
