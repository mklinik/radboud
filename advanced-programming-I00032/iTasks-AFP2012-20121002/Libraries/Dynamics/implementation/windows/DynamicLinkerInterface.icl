implementation module DynamicLinkerInterface

import StdEnv
import EnDecode
import DefaultElem
import DynamicUtilities
import StdDynamicLowLevelInterface
import shared_buffer
import memory_mapped_files
import code from "read_function.obj"

// -------------------------------------------------------------------------------------------
// communication between application and client
:: TypeReference
	= {
		tr_type_name	:: !String
	,	tr_module_name1	:: !String
	,	tr_module_name2	:: !String
	,	tr_library1		:: !LibraryID
	,	tr_library2		:: !LibraryID
	}

instance DefaultElem TypeReference
where
	default_elem 
		 = {tr_type_name	= {}
		,	tr_module_name1	= {}
		,	tr_module_name2	= {}
		,	tr_library1		= default_elem
		,	tr_library2		= default_elem
		}

instance EnDecode TypeReference
where
	to_size {tr_type_name,tr_module_name1,tr_module_name2,tr_library1,tr_library2}
		= to_size tr_type_name + to_size tr_module_name1 + to_size tr_module_name2 + to_size tr_library1 + to_size tr_library2
		
	to_string {tr_type_name,tr_module_name1,tr_module_name2,tr_library1,tr_library2} offset buffer
		# (offset,buffer)
			= to_string tr_type_name offset buffer
		# (offset,buffer)
			= to_string tr_module_name1 offset buffer
		# (offset,buffer)
			= to_string tr_module_name2 offset buffer
		# (offset,buffer)
			= to_string tr_library1 offset buffer
		# (offset,buffer)
			= to_string tr_library2 offset buffer
		= (offset,buffer)
			
	from_string offset buffer
		# (tr_type_name,offset)
			= from_string offset buffer
		# (tr_module_name1,offset)
			= from_string offset buffer
		# (tr_module_name2,offset)
			= from_string offset buffer
		# (tr_library1,offset)
			= from_string offset buffer
		# (tr_library2,offset)
			= from_string offset buffer
			
		# type_ref
			= { tr_type_name 	= tr_type_name
			,	tr_module_name1	= tr_module_name1
			,	tr_module_name2	= tr_module_name2
			,	tr_library1		= tr_library1
			,	tr_library2		= tr_library2
			}
		= (type_ref,offset)
	
:: LibraryID
	= Address !Int
	| Number !Int				// always RunTime valid (kan geen diskID zijn)

instance DefaultElem LibraryID
where
	default_elem 
		= Address 0

ENDECODE_LIBRARYID_SIZE	:== 1
ENDECODE_ADDRESS 	:== 0
ENDECODE_NUMBER		:== 1

instance EnDecode LibraryID
where
	to_size (Address i)
		= ENDECODE_LIBRARYID_SIZE + to_size i
	to_size (Number i)
		= ENDECODE_LIBRARYID_SIZE + to_size i

	to_string x=:(Address i) offset buffer 	
		# buffer = { buffer & [offset] = toChar ENDECODE_ADDRESS }
	    = (offset + to_size x, WriteLong buffer (offset + ENDECODE_LIBRARYID_SIZE) i)
	    
	to_string x=:(Number i) offset buffer 	
		# buffer = { buffer & [offset] = toChar ENDECODE_NUMBER }
	    = (offset + to_size x, WriteLong buffer (offset + ENDECODE_LIBRARYID_SIZE) i)
	
	from_string offset buffer	
		# library_id = buffer.[offset]
		# i = FromStringToInt buffer (offset + ENDECODE_LIBRARYID_SIZE)
		# library_id = toInt library_id
		| library_id == ENDECODE_ADDRESS
			#! x = Address i
			= (x, offset + to_size x)
		| library_id == ENDECODE_NUMBER
			#! x = Number i
			= (x, offset + to_size x)

// -------------------------------------------------------------------------------------------
:: *RegisterLazyDynamic_Out
	= {
		rld_o_file							:: !*(!Int,!*StdDynamicSharedBufferInfo) //!(!Int,!Int)
	,	rld_o_filename						:: !String
	,	rld_o_diskid_to_runtimeid			:: !{#Int}			// conversion from DiskId (disguished as RunTimeId) to *real* runtimeID (library instances)
	,	rld_o_disk_to_rt_dynamic_indices	:: !{#Int} 			// conversion from disk to runtime index for lazy dynamics
	,	rld_o_id							:: !Int
	,	rld_o_rt_type_redirection_table		:: !{#RunTimeIDW}	
	}

// Decode
RegisterLazyDynamic :: !Int -> RegisterLazyDynamic_Out
RegisterLazyDynamic lazy_dynamic_index
	#! msg
		= "RegisterLazyDynamic" +++ (encode lazy_dynamic_index) +++
			"\n"
	#! s_adr = doreqS msg

	#! ((file,file_name),j) = from_string  0 s_adr
	#! (diskid_to_runtimeid,j) = from_string j s_adr
	#! (di_disk_to_rt_dynamic_indices,j) = from_string j s_adr
	# (id,j) = from_string j s_adr;
	# (type_redirection_table,j) = from_string j s_adr;	
	
	# (ok,rld_o_file)
		= OpenExistingSharedBuffer2 file
	| not ok
		= abort "RegisterLazyDynamic: OpenExistingSharedBuffer failed"
		
	= { 	rld_o_file							= rld_o_file
		,	rld_o_filename						= file_name
		,	rld_o_diskid_to_runtimeid			= diskid_to_runtimeid
		,	rld_o_disk_to_rt_dynamic_indices	= di_disk_to_rt_dynamic_indices
		,	rld_o_id							= id
		,	rld_o_rt_type_redirection_table		= type_redirection_table
		}

// -------------------------------------------------------------------------------------------
:: GetBlockAddress_In
	= {
		gba_i_filename				:: !String
	,	gba_i_first_time			:: !Bool
	,	gba_i_id					:: !Int
	,	gba_i_block_i				:: !Int
	,	gba_i_dynamic_rts_string	:: !String
	}
	
instance DefaultElem GetBlockAddress_In
where
	default_elem
		= {	gba_i_filename				= default_elem
		,	gba_i_first_time			= default_elem
		,	gba_i_id					= default_elem
		,	gba_i_block_i				= default_elem
		,	gba_i_dynamic_rts_string	= default_elem
		}
	
:: GetBlockAddress_Out	
	= {
		gba_o_diskid_to_runtimeid			:: !{#Int}
	,	gba_o_disk_to_rt_dynamic_indices	:: !{#Int}
	,	gba_o_id							:: !Int
	,	gba_o_addresses						:: !String
	,	gba_o_rt_type_redirection_table		:: !{#RunTimeIDW}
	}
	
instance EnDecode GetBlockAddress_Out
where
	to_size {gba_o_diskid_to_runtimeid,gba_o_disk_to_rt_dynamic_indices,gba_o_id,gba_o_addresses,gba_o_rt_type_redirection_table}
		= to_size gba_o_diskid_to_runtimeid 
		+ to_size gba_o_disk_to_rt_dynamic_indices
		+ to_size gba_o_id
		+ to_size gba_o_addresses
		+ to_size gba_o_rt_type_redirection_table

	to_string {gba_o_diskid_to_runtimeid,gba_o_disk_to_rt_dynamic_indices,gba_o_id,gba_o_addresses,gba_o_rt_type_redirection_table} offset buffer
		# (offset,buffer)
			= to_string gba_o_diskid_to_runtimeid offset buffer
		# (offset,buffer)
			= to_string gba_o_disk_to_rt_dynamic_indices offset buffer
		# (offset,buffer)
			= to_string gba_o_id offset buffer
		# (offset,buffer)
			= to_string gba_o_addresses offset buffer
		# (offset,buffer)
			= to_string gba_o_rt_type_redirection_table offset buffer
		= (offset,buffer)

	from_string offset buffer
		#! (gba_o_diskid_to_runtimeid,offset)
			= from_string offset buffer
		#! (gba_o_disk_to_rt_dynamic_indices,offset)
			= from_string offset buffer
		#! (gba_o_id,offset)
			= from_string offset buffer
		#! (gba_o_addresses,offset)
			= from_string offset buffer
		#! (gba_o_rt_type_redirection_table,offset)
			= from_string offset buffer
			
		#! di
			= { default_elem &
				gba_o_diskid_to_runtimeid			= gba_o_diskid_to_runtimeid
			,	gba_o_disk_to_rt_dynamic_indices	= gba_o_disk_to_rt_dynamic_indices
			,	gba_o_id							= gba_o_id
			,	gba_o_addresses						= gba_o_addresses
			,	gba_o_rt_type_redirection_table		= gba_o_rt_type_redirection_table
			}
		= (di,offset)	
	
instance DefaultElem GetBlockAddress_Out
where 
	default_elem
		= {
			gba_o_diskid_to_runtimeid			= default_elem
		,	gba_o_disk_to_rt_dynamic_indices	= default_elem
		,	gba_o_id							= default_elem
		,	gba_o_addresses						= default_elem
		,	gba_o_rt_type_redirection_table		= default_elem
		}
		
GetBlockAddresses2 :: !GetBlockAddress_In -> (a,!GetBlockAddress_Out)
GetBlockAddresses2 {gba_i_filename,gba_i_first_time,gba_i_id,gba_i_block_i,gba_i_dynamic_rts_string}
	// do request
	#! msg
		= "Compute2DescAddressTable" +++ gba_i_filename +++
			"\n" +++ toString gba_i_first_time +++
			"\n" +++ toString gba_i_id +++ 
			"\n" +++ toString gba_i_block_i +++
			(if (gba_i_first_time && size gba_i_dynamic_rts_string <> 0)
				("\n" +++ gba_i_dynamic_rts_string +++ "\n")
				"\n")

	#! s_adr = doreqS msg

	#! gba_o
		= case gba_i_first_time of
			True
				# (gba_o=:{gba_o_addresses})
					= decode s_adr
				-> gba_o
			False
				-> { default_elem & gba_o_id = gba_i_id, gba_o_addresses = s_adr }
	# (ok,copy_string_to_graph)
		= read_function ((FromStringToInt gba_o.gba_o_addresses 4 )) //4))
	= (copy_string_to_graph,gba_o)
	
// -------------------------------------------------------------------------------------------
:: *GetDynamicRTSInfo_In
	= {
		gdri_i_type_library_instances		:: !*{#Int}
	,	gdri_i_lazy_dynamics_references		:: !{#LazyDynamicReference}
	,	gdri_i_runtime_ids					:: !{#RunTimeIDW}
	}
	
// changed also graph_to_string.c; CONVERT_LAZY_RUN_TIME_ID
:: RunTimeIDW
	= {
		rtid_runtime_id			:: !Int
	,	rtid_assigned_disk_id	:: !Int			// id reference from type
	}
	
instance EnDecode RunTimeIDW
where
	to_size {rtid_runtime_id,rtid_assigned_disk_id}
		= to_size rtid_runtime_id + to_size rtid_assigned_disk_id

	to_string {rtid_runtime_id,rtid_assigned_disk_id} offset buffer
		# (offset,buffer) = to_string rtid_runtime_id offset buffer
		# (offset,buffer) = to_string rtid_assigned_disk_id offset buffer
		= (offset,buffer)

	from_string offset buffer
		#! (rtid_runtime_id,offset) = from_string offset buffer
		#! (rtid_assigned_disk_id,offset) = from_string offset buffer
		#! di
			= { default_elem &
				rtid_runtime_id			= rtid_runtime_id
			,	rtid_assigned_disk_id	= rtid_assigned_disk_id
			}
		= (di,offset)	
				 
instance DefaultElem RunTimeIDW
where
	default_elem
		= {	rtid_runtime_id			= default_elem
		,	rtid_assigned_disk_id	= 0
		}

GetDynamicRTSInfo :: !GetDynamicRTSInfo_In -> *{#Char}
GetDynamicRTSInfo {gdri_i_type_library_instances,gdri_i_lazy_dynamics_references,gdri_i_runtime_ids}
	#! dynamic_rts_info
		= doreqS ("GetDynamicRTSInfo" +++ 
			encode gdri_i_type_library_instances
		+++ encode gdri_i_lazy_dynamics_references
		+++ encode gdri_i_runtime_ids)
	= dynamic_rts_info
	
// -------------------------------------------------------------------------------------------	
:: GetGraphToStringFunction_Out
	= {
		ggtsf_o_n_library_instances		:: !Int
	,	ggtsf_o_range_table				:: !String
	}

copy_graph_to_string2
	= doreqS ("GetGraphToStringFunction")
	
copy_graph_to_string_addr
	=: FromStringToInt copy_graph_to_string2 0
	
GetGraphToStringFunction :: (a,GetGraphToStringFunction_Out)
GetGraphToStringFunction
	# (_,copy_graph_to_string)
		= read_function ((FromStringToInt copy_graph_to_string2 0))
	# n_bytes_to_skip = 4
	# ggtsf
		= {	ggtsf_o_n_library_instances		= FromStringToInt copy_graph_to_string2 (n_bytes_to_skip + RID_N_TYPE_TABLES_OFFSET)
		,	ggtsf_o_range_table				= copy_graph_to_string2 % (n_bytes_to_skip,dec (size copy_graph_to_string2))
		}
	= (copy_graph_to_string,ggtsf)

read_function :: !Int -> (!Bool,a)
read_function _ =
	code {
		jmp read_function
	}

doreqS :: !String -> .{#Char}
doreqS _ =
	code { 
		ccall DoReqS "S-S"
	}

// -------------------------------------------------------------------------------------------	
:: *OpenDynamicToLinker_Out
	= {
		odtl_o_ok					:: !Bool
	,	odtl_o_file 				:: !*(!Int,!*StdDynamicSharedBufferInfo)
	,	odtl_o_dynamic_rts_string	:: !String
	}	

OpenDynamicToLinker :: !String -> *OpenDynamicToLinker_Out
OpenDynamicToLinker dynamic_as_string
	# (ok,file,dynamic_rts_string)
		= CreateSharedBufferFromPageFile GetHandleToServer dynamic_as_string
	= {	odtl_o_ok					= ok
	,	odtl_o_file 				= file
	,	odtl_o_dynamic_rts_string	= dynamic_rts_string
	}
where
	// LowLevel-interface
	GetHandleToServer :: HANDLE
	GetHandleToServer 
		= code {
			ccall GetHandleToServer ":I"
		}

CloseDynamicToLinker :: *OpenDynamicToLinker_Out -> Bool
CloseDynamicToLinker {odtl_o_file}
	| CloseSharedBufferFromPageFile odtl_o_file
		= True
		= abort "CloseDynamicToLinker; error closing file" // False

GetDynamicLinkerPath :: String;
GetDynamicLinkerPath
	=: doreqS ("GetDynamicLinkerDir\n");

CleanNewKey :: !String !String -> Bool;
CleanNewKey _ _ = code {
 		ccall CleanNewKey "SS:I"
	};

// -------------------------------------------------------------------------------------------			
SendEncodedDynamic :: !EncodedDynamic2 !String -> String;
SendEncodedDynamic encoded_dynamic file_name
	= abort "SendEncodedDynamic";
	