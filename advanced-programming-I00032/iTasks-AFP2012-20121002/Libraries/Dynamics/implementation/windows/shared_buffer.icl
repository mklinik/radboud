implementation module shared_buffer

import StdEnv

import memory_mapped_files

import memory

import StdDynamicLowLevelInterface
import DefaultElem

instance BinaryDynamicIO (!Int,!*StdDynamicSharedBufferInfo)
where
	bd_readi (ptr,mem)
		# (word,mem)
			= readWord1 ptr mem
		= (True,word,(ptr + 4,mem))

	bd_reads (ptr,mem) n_characters
		# string
			= createArray n_characters ' '
		# (ptr,string,mem)
			= bd_reads_loop 0 ptr string mem
		= (string,(ptr,mem))
	where
		// kan veel efficienter
		bd_reads_loop i ptr string mem
			| i == n_characters
				= (ptr,string,mem)
			# (c,mem)
				= readByte1 ptr mem
			= bd_reads_loop (inc i) (inc ptr) {string & [i] = toChar c} mem
			
	bd_seek (_,buffer=:{view}) offset FSeekSet
		= (True,(view + offset ,buffer))
		
	bd_delta_fp file
		= (0,file)
		
	bd_freadsubstring i n s (ptr,mem)
		#! (s_s,s)
			= usize s;
		#! (new_offset,s,mem)
			= bd_read_substring i s_s s ptr mem
		= (new_offset - ptr,s,(new_offset,mem));
	where
		bd_read_substring :: !Int !Int !*{#Char} !Int !*StdDynamicSharedBufferInfo -> (!Int,!*{#Char},!*StdDynamicSharedBufferInfo);
		bd_read_substring i s_s s offset mem
			| i >= s_s
				= (offset,s,mem);
		
			#! (c,mem)
				= readByte1 offset mem
			= bd_read_substring (inc i) s_s {s & [i] = toChar c} (inc offset) mem;

:: *StdDynamicSharedBufferInfo
	= {
		hfile	:: !HFILE
	,	hmap	:: !HANDLE
	,	view	:: !Int
	};
	
instance DefaultElemU StdDynamicSharedBufferInfo
where
	default_elemU
		= {
			hfile	= default_elem
		,	hmap	= default_elem
		,	view	= default_elem
		}

// ATTENTION:
// The first 2 words of a dynamic are overwritten by a pointer to the string descriptor and
// the length of the string. The header_size 
CreateSharedBufferFromFile2 :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),(!Int,!Int))
CreateSharedBufferFromFile2 dynamic_rts_process_handle file_name
	// open file
	# (file_handle,st)
		= CreateFile (file_name +++ "\0") GENERIC_READ FILE_SHARE_READ NULL OPEN_EXISTING  FILE_ATTRIBUTE_NORMAL 0 initialState
	| file_handle == INVALID_HANDLE_VALUE
		= abort "CreateSharedBuffer: CreateFile failed"

	# (file_size,st)
		= GetFileSize file_handle NULL st
	| file_size == 0xFFFFFFFF 
		= abort "CreateSharedBuffer: GetFileSize"	
	
	// create file mapping
	# (file_mapping_handle,st)
		= CreateFileMapping file_handle NULL PAGE_READONLY 0 0 NULL st
	| file_mapping_handle == 0
		= abort ("CreateSharedBuffer: CreateFileMapping failed" +++ toString GetLastError)		

	// map view of file
	# (view,st)
		= MapViewOfFile file_mapping_handle FILE_MAP_READ 0 0 0 st
	| view == 0
		= abort "CreateSharedBuffer: MapViewOfFile"	

	# (current_process_handle,st)
		= GetCurrentProcess st

	// duplicate handle to that string to dynamic rts
	# (a,st)
		= address_of_buffer st;
	# (bool,st)
		= DuplicateHandle current_process_handle file_mapping_handle dynamic_rts_process_handle (Ptr a) 0 0 DUPLICATE_SAME_ACCESS st
	| bool == 0
		= abort ("copy_to_dynamic_rts: failure of DuplicateHandle" +++ toString GetLastError)
	# (file_mapping_handle_in_server,st)
		= readWord1 a st;

	# stdDynamic_shared_buffer_info
		= {	hfile	= file_handle
		,	hmap	= file_mapping_handle
		,	view	= view
		};		
	# file
		= (0,stdDynamic_shared_buffer_info)
	| CloseST st
		= (True,file,(file_mapping_handle_in_server,file_size));
//	= (True,file,toString file_mapping_handle_in_server +++ "\n" +++ toString file_size)

CreateSharedBufferFromFile :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),!String)
CreateSharedBufferFromFile dynamic_rts_process_handle file_name
	#! (ok,file,(file_mapping_handle_in_server,file_size))
		= CreateSharedBufferFromFile2 dynamic_rts_process_handle file_name;
	= (True,file,toString file_mapping_handle_in_server +++ "\n" +++ toString file_size);

OpenExistingSharedBuffer2 :: (!Int,!Int) -> (Bool,(!Int,!*StdDynamicSharedBufferInfo))
OpenExistingSharedBuffer2 (file_mapping_handle,s_buffer)
	= OpenExistingSharedBuffer file_mapping_handle s_buffer;

OpenExistingSharedBuffer :: !Int !Int -> (Bool,(!Int,!*StdDynamicSharedBufferInfo))
OpenExistingSharedBuffer file_mapping_handle s_buffer
	# (view,st)
		= MapViewOfFile file_mapping_handle FILE_MAP_READ 0 0 s_buffer initialState
	| view == 0
		= abort ("OpenExistingSharedBuffer: MapViewOfFile failed" +++ toString GetLastError)
	# stdDynamic_shared_buffer_info
		= {	hfile	= 0
		,	hmap	= file_mapping_handle
		,	view	= view
		};
	# file = (0,stdDynamic_shared_buffer_info)
	= (True,file);

CloseExistingSharedBuffer :: !(!Int,!*StdDynamicSharedBufferInfo) -> Bool
CloseExistingSharedBuffer (_,{hmap,view})
	# st = initialState
	# (bool,st)
		= UnmapViewOfFile view st
	| bool == 0
		= abort ("get_tables_from_dynamic; could not close view" +++ toString GetLastError);	
	# (bool,st)
		= CloseHandle hmap st
	| bool == 0
		= abort "get_tables_from_dynamic; could not close view"
	= CloseST st

CloseSharedBufferFromFile :: (!Int,!*StdDynamicSharedBufferInfo) -> Bool
CloseSharedBufferFromFile (_,{hfile,hmap,view})
	#! (bool,st)
		= UnmapViewOfFile view initialState
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseSharedBufferFromFile"

	# st = initialState		
	# (bool,st)
		= CloseHandle hmap st
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseHandle (1)"
		
	# (bool,st)
		= CloseHandle hfile st
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseHandle (2)"

	= CloseST st

CreateSharedBufferFromPageFile :: !Int !String -> (Bool,(!Int,!*StdDynamicSharedBufferInfo),!String)
CreateSharedBufferFromPageFile dynamic_rts_process_handle dynamic_as_string
	# file_size = size dynamic_as_string
	// create file mapping
	# (file_mapping_handle,st)
		= CreateFileMapping 0xFFFFFFFF NULL PAGE_READWRITE 0 file_size NULL initialState
	| file_mapping_handle == 0
		= abort ("CreateSharedBufferFromPageFile: CreateFileMapping failed" +++ toString GetLastError)		
	
	// map view of file
	# (view,st)
		= MapViewOfFile file_mapping_handle FILE_MAP_WRITE 0 0 file_size st
	| view == 0
		= abort "CreateSharedBufferFromPageFile: MapViewOfFile failed"

	# (current_process_handle,st)
		= GetCurrentProcess st

	// duplicate handle to that string to dynamic rts
	# (a,st)
		= address_of_buffer st;
	# (bool,st)
		= DuplicateHandle current_process_handle file_mapping_handle dynamic_rts_process_handle (Ptr a) 0 0 DUPLICATE_SAME_ACCESS st
	| bool == 0
		= abort "copy_to_dynamic_rts: failure of DuplicateHandle"
	# (file_mapping_handle_in_server,st)
		= readWord1 a st;
		
	# st
		= copy_string 0 file_size dynamic_as_string view st
	| CloseST st
//		= abort (toString (call_debugger view));

	# stdDynamic_shared_buffer_info
		= {	hfile	= 0
		,	hmap	= file_mapping_handle
		,	view	= view
		};		
	# file = (0,stdDynamic_shared_buffer_info)
//	| CloseST st
	= (True,file,toString file_mapping_handle_in_server +++ "\n" +++ toString file_size)		
where
	copy_string :: !Int !Int !String !Int !ST -> ST;
	copy_string i limit dynamic_as_string view st
		#! rest
			= limit rem 4
		#! st
			= copy_string_per_word i (limit - rest) dynamic_as_string view st
		#! st
			= copy_string_per_byte (limit-rest) limit dynamic_as_string view st
		= st		
	where
		copy_string_per_word :: !Int !Int !String !Int !ST -> ST;
		copy_string_per_word i limit dynamic_as_string view st
			| i >= limit
				= st 
			# v
				= FromStringToInt dynamic_as_string i
			= copy_string_per_word (i + 4) limit dynamic_as_string view (writeWord1 v (view + i) st)
			
			
		copy_string_per_byte :: !Int !Int !String !Int !ST -> ST;
		copy_string_per_byte i limit dynamic_as_string view st
			| i == limit
				= st
			= copy_string_per_byte (inc i) limit dynamic_as_string view  (writeByte1 dynamic_as_string.[i] (view + i) st)

CloseSharedBufferFromPageFile :: (!Int,!*StdDynamicSharedBufferInfo) -> Bool
CloseSharedBufferFromPageFile (_,{hmap,view})
	#! (bool,st)
		= UnmapViewOfFile view initialState
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseSharedBufferFromFile"

	# st = initialState		
	# (bool,st)
		= CloseHandle hmap st
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseHandle (1)"
		
	= CloseST st
	
FromStringToInt :: !String !Int -> Int
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

/*
CreateSharedBufferFromPageFile :: !String -> (Bool,(!Int,!*{#Char}),!String,!*StdDynamicSharedBufferInfo)
CreateSharedBufferFromPageFile string
	# s_buffer
		= size string
		
	// create shared clean string
	# (file_mapping_handle,st)
		= CreateFileMapping 0xFFFFFFFF NULL PAGE_READWRITE 0 s_buffer (Ptr "zomaar\0") initialState
	| file_mapping_handle == 0
		= abort ("CreateSharedBufferFromPageFile: CreateFileMapping failed" +++ toString GetLastError)		


	# (view,st)
		= MapViewOfFile file_mapping_handle FILE_MAP_WRITE 0 0 s_buffer st
	| view == 0
		= abort "CreateSharedBufferFromPageFile: MapViewOfFile failed"
		
	# (buffer,st)
		= CreateSharedString view s_buffer st
		
	# (current_process_handle,st)
		= GetCurrentProcess st
	# (dynamic_rts_process_handle,st)
		= GetHandleToServer st

	// duplicate handle to that string to dynamic rts
	# (bool,st)
		= DuplicateHandle current_process_handle file_mapping_handle dynamic_rts_process_handle (Ptr (view + 8)) 0 0 DUPLICATE_SAME_ACCESS st
	| bool == 0
		= abort "copy_to_dynamic_rts: failure of DuplicateHandle"
	| CloseST st
	# (handle_to_shared_string_in_dynamic_rts,buffer)
		= FromStringToIntU buffer 0;
		
		
	# buffer 
		= copy_string 8 s_buffer string 0 buffer

/*		
	# (s_buffer,buffer)
		= usize buffer
	| s_buffer <> 0
	
	= abort (toString (call_debugger  view))
*/


	# stdDynamic_shared_buffer_info
		= {
			hfile	= 0
		,	hmap	= file_mapping_handle
		,	view	= view
		};
	= (True,(0,buffer),toString handle_to_shared_string_in_dynamic_rts +++ "\n" +++ toString s_buffer,stdDynamic_shared_buffer_info)
where 
	copy_string i limit src j dest
		| i == limit
			= dest
		= copy_string (inc i) limit src (inc j) {dest & [j] = src.[i] }
		
CloseSharedBufferFromPageFile :: !*StdDynamicSharedBufferInfo -> Bool
CloseSharedBufferFromPageFile {hmap,view}
/*
	#! (bool,st)
		= UnmapViewOfFile view initialState
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseSharedBufferFromFile"
*/
	# st = initialState		
	# (bool,st)
		= CloseHandle hmap st
	| bool == 0
		= abort "CloseSharedBufferFromFile; failure of CloseHandle (1)"
	= CloseST st
*/	
call_debugger :: !Int -> Int
call_debugger _
	= code {
		instruction 204
	}

GetHandleToServer :: !ST -> (!HANDLE,!ST)
GetHandleToServer st
	= (GetHandleToServer2,st)

GetHandleToServer2 :: HANDLE;
GetHandleToServer2 
	= code {
		ccall GetHandleToServer ":I"
	}
		
FromStringToIntU :: !*{#Char} !Int -> (!Int,!*{#Char});
FromStringToIntU array i
	#! (v0,array)
		= array![i];
	#! (v1,array)
		= array![i+1];
	#! (v2,array)
		= array![i+2];
	#! (v3,array)
		= array![i+3];
	#! i
		= (toInt v0)+(toInt v1<<8)+(toInt v2<<16)+(toInt v3<<24);
	= (i,array);
	
WriteLong :: !*{#Char} !Int !Int -> *{#Char};
WriteLong array i v
	= { array & [i] 	= (toChar v)		,	[i+1] = (toChar (v>>8)),
				[i+2]	= (toChar (v>>16))  ,	[i+3] = (toChar (v>>24))};
