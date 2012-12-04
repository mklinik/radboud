definition module DynIDMacros

from StdOverloaded import class +++ (+++)
from StdString import instance +++ {#Char}
from StdDynamicTypes import NAME_PREFIXES

// IDS FOR LIBRARIES
// encoded file_name:	<library name>_<md5 from code>_<md5 from type>
// run-time file_name:	<DLINK PATH>\\<DS_SYSTEM_DYNAMICS_DIR>\\<library name>_<md5 from code>_<md5 from type>.{typ,lib}
CREATE_ENCODED_LIBRARY_FILE_NAME library_name code_md5 type_md5
	:== (NAME_PREFIXES (library_name +++ "_") "") +++ code_md5 +++ "_" +++ type_md5;

CONVERT_ENCODED_LIBRARY_IDENTIFICATION_INTO_RUN_TIME_LIBRARY_IDENTIFICATION base_directory encoded_library_identification
	:== base_directory +++ "\\" +++ DS_LIBRARIES_DIR +++ "\\" +++ encoded_library_identification;

ADD_TYPE_LIBRARY_EXTENSION library_identification
	:== library_identification +++ "." +++ EXTENSION_TYPE_LIBRARY;
ADD_CODE_LIBRARY_EXTENSION library_identification
	:== library_identification +++ "." +++ EXTENSION_CODE_LIBRARY;

EXTENSION_TYPE_LIBRARY
	:== "typ"
EXTENSION_CODE_LIBRARY
	:== "lib"

// Predefined directories (w.r.t. root-directory):
DS_LIBRARIES_DIR		:== "libraries"
DS_SYSTEM_DYNAMICS_DIR	:== "system dynamics"

EXTENSION_USER_DYNAMIC
	:== "dyn"
