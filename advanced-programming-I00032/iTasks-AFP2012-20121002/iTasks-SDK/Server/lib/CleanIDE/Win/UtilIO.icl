implementation module UtilIO

import code from library "util_io_kernel_lib" // GetShortPathNameA@12

import StdArray, StdBool, StdClass, StdFile, StdList, StdTuple, StdString

GetShortPathName :: !String -> (!Bool,!String);
GetShortPathName long_path
	#! long_path = if null_terminated long_path (long_path+++"\0")
	#! (result,short_path) = Helper long_path
	#! short_path = if null_terminated short_path (short_path%(0,size short_path - 2))
	= (result <> 0,short_path);
where
	lsize = size long_path
	null_terminated = long_path.[lsize-1] == '\0'
	
	Helper long_path
		#! s_short_path
			= GetShortPathName_ long_path "\0" 0;
		#! short_path
			= createArray s_short_path '\0';
		#! result
			= GetShortPathName_ long_path short_path s_short_path;
		= (result,short_path)

	GetShortPathName_ :: !String !String !Int -> Int;
	GetShortPathName_ long_path short_path s_short_path
		= code {
			ccall GetShortPathNameA@12 "PssI:I"
			}

