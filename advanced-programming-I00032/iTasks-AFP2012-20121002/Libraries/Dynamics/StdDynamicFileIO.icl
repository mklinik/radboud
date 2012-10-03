implementation module StdDynamicFileIO

import StdEnv
import StdDynamicLowLevelInterface
import DynamicGraphConversion
from DynamicUtilities import WriteLong, NF
import Directory
import DynID
import md5
import DynamicLinkerInterface
import StdDynamicTypes
import StdDynamic

open_dynamic_for_read file_name1 files 
	#! (ok,file_name,files)
		= FILE_IDENTIFICATION (get_system_dynamic_identification file_name1 files) (True,file_name1,files)
	| not ok
		= (False,"",default_dynamic_header,stderr,files)

	#! (ok,dynamic_header,file1,files)
		= open_dynamic_as_binary file_name files
	| not ok
		#! (_,files)
			= close_dynamic_as_binary file1 files
		= (False,"",dynamic_header,stderr,files)
		
		= (True,file_name,dynamic_header,file1,files)

// file_name should contain an *absolute* path
readDynamic :: String *f -> (Bool,Dynamic,*f) | FileSystem f
readDynamic file_name files
	#! file_name
		= create_dynamic_file_name file_name;	

	#! (ok,file_name,dynamic_header=:{block_table_i,graph_i},file,files)
		= open_dynamic_for_read file_name files
	| not ok
		= (False,undef,files)

	// initializes the dynamic info-structure (gdid)
	#! (ok,gdid,file)
		= init_dynamic file_name dynamic_header file
	| not ok
		= abort "readDynamic: error; more detailled error information must be implemented";

	// construct top-level dynamic
	#! dyn
		= build_block (NF make_start_node_index) (NF { gdid & gdid.di_dummy = ""})
		
	#! (_,files)
		= close_dynamic_as_binary file files
	= (ok,dyn,files)

writeDynamic :: String Dynamic *f -> (Bool,*f) | FileSystem f
writeDynamic file_name1 dynamic_value files
	#! file_name1
		= create_dynamic_file_name file_name1;
	# temp_encoded_dynamic_file_name
		= CONVERTED_ENCODED_DYNAMIC_FILE_NAME_INTO_PATH GetDynamicLinkerPath (CREATE_ENCODED_DYNAMIC_FILE_NAME "temp" "md5");

	# file_name
		= create_dynamic_file_name (FILE_IDENTIFICATION temp_encoded_dynamic_file_name file_name1);

	// write system dynamic
	#! (ok,file,files)
		= fopen file_name FWriteData files
	| not ok
		= (False,files)

	// this forces evaluation of the type part of the dynamic
	| typeCodeOfDynamic dynamic_value == TypeVar (-1)
		=	abort "StdDynamicFileIO, writeDynamic: malformed dynamic type"

	# (ok,encoded_dynamic)
		= dynamic_to_string dynamic_value
	| not ok
		= (False,files)

	#! file
		= write_encoded_dynamic encoded_dynamic file

	# (ok,files)
		= fclose file files

	// write user dynamic
	#! (ok,files)
		= write_user_dynamic file_name1 file_name files
	| not ok
		= (False,files)

	= (ok,files)
where
	write_encoded_dynamic {ed_encoded_graph,ed_dynamic_rts_info} file
		// Offset/Size
		# (s_ed_encoded_graph,ed_encoded_graph)
			= usize ed_encoded_graph
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_OFFSET - HEADER_SIZE_OFFSET) s_ed_encoded_graph
		
		# (s_ed_dynamic_rts_info,ed_dynamic_rts_info)
			= usize ed_dynamic_rts_info
		# ed_encoded_graph
			= WriteLong ed_encoded_graph (DYNAMIC_RTS_INFO_SIZE - HEADER_SIZE_OFFSET) s_ed_dynamic_rts_info
		 
		#! file
			= fwrites ed_encoded_graph file
		#! file
			= fwrites ed_dynamic_rts_info file
		= file
		where
			s_ed_dynamic_rts_info
				= size ed_dynamic_rts_info
				
	write_user_dynamic file_name1 file_name files
		// system dynamics-temp dynamic
		#! link_name
			= extract_dynamic_or_library_identification file_name1
		#! (md5_dynamic_id,files)
			= getMd5DigestFromFile file_name files
		#! dynamic_id
			= CREATE_ENCODED_DYNAMIC_FILE_NAME link_name md5_dynamic_id 

		#! system_dynamic
			= CONVERTED_ENCODED_DYNAMIC_FILE_NAME_INTO_PATH GetDynamicLinkerPath dynamic_id

		#! ((_,file_name_p),files)
			= pd_StringToPath file_name files
		#! ((_,system_dynamic_p),files)
			= pd_StringToPath system_dynamic files
		#! (_,files)
			= fmove OverwriteFile file_name_p system_dynamic_p files

		// create link file
//		#! link_file_name
//			= (fst (ExtractPathAndFile file_name1) ) +++ "\\" +++ link_name +++ "." +++ EXTENSION_USER_DYNAMIC;
		#! (ok1,userFile,files)
			= fopen file_name1 /*link_file_name*/ FWriteData files
		| not ok1
			= (False,files)
			
		#! userFile
			= fwrites (dynamic_id +++ "\n") userFile //the ID is in that file.
		//ID in this case is e.g. "af12g42a3aa234ab2f324b34f2345bf5"
		# (ok2,files)
			= fclose userFile files
		= (ok2,files);
