definition module DynamicLinkerInterface

from DefaultElem import class DefaultElem
from EnDecode import class EnDecode
from shared_buffer import :: StdDynamicSharedBufferInfo
from StdDynamicLowLevelInterface import :: LazyDynamicReference
from DynamicGraphConversion import :: EncodedDynamic2

// communication between application and client
:: TypeReference
	= {
		tr_type_name	:: !String
	,	tr_module_name1	:: !String
	,	tr_module_name2	:: !String
	,	tr_library1		:: !LibraryID
	,	tr_library2		:: !LibraryID
	};
	
instance EnDecode TypeReference
	
:: LibraryID
	= Address !Int
	| Number !Int				// always RunTime valid (kan geen diskID zijn)
	;
	
instance DefaultElem TypeReference;

:: *RegisterLazyDynamic_Out
	= {
		rld_o_file							:: !*(!Int,!*StdDynamicSharedBufferInfo) //!(!Int,!Int)
	,	rld_o_filename						:: !String
	,	rld_o_diskid_to_runtimeid			:: !{#Int}			// conversion from DiskId (disguished as RunTimeId) to *real* runtimeID (library instances)
	,	rld_o_disk_to_rt_dynamic_indices	:: !{#Int} 			// conversion from disk to runtime index for lazy dynamics
	,	rld_o_id							:: !Int
	,	rld_o_rt_type_redirection_table		:: !{#RunTimeIDW}	
	}
	
RegisterLazyDynamic :: !Int -> *RegisterLazyDynamic_Out

:: GetBlockAddress_In
	= {
		gba_i_filename				:: !String
	,	gba_i_first_time			:: !Bool
	,	gba_i_id					:: !Int
	,	gba_i_block_i				:: !Int
	,	gba_i_dynamic_rts_string	:: !String
	}
	
instance DefaultElem GetBlockAddress_In
	
:: GetBlockAddress_Out	
	= {
		gba_o_diskid_to_runtimeid			:: !{#Int}
	,	gba_o_disk_to_rt_dynamic_indices	:: !{#Int}
	,	gba_o_id							:: !Int
	,	gba_o_addresses						:: !String
	,	gba_o_rt_type_redirection_table		:: !{#RunTimeIDW}
	}

instance EnDecode GetBlockAddress_Out

instance DefaultElem GetBlockAddress_Out
	
GetBlockAddresses2 :: !GetBlockAddress_In -> (a,!GetBlockAddress_Out)


:: *GetDynamicRTSInfo_In
	= {
		gdri_i_type_library_instances		:: !*{#Int}
	,	gdri_i_lazy_dynamics_references		:: !{#LazyDynamicReference}
	,	gdri_i_runtime_ids					:: !{#RunTimeIDW}
	}

:: RunTimeIDW
	= {
		rtid_runtime_id			:: !Int
	,	rtid_assigned_disk_id	:: !Int			// id reference from type
	}
	
instance DefaultElem RunTimeIDW

instance EnDecode RunTimeIDW

GetDynamicRTSInfo :: !GetDynamicRTSInfo_In -> *{#Char}

:: GetGraphToStringFunction_Out
	= {	ggtsf_o_n_library_instances		:: !Int
	,	ggtsf_o_range_table				:: !String
	}
	
GetGraphToStringFunction :: (a,GetGraphToStringFunction_Out)

:: *OpenDynamicToLinker_Out
	= {	odtl_o_ok					:: !Bool
	,	odtl_o_file 				:: !*(!Int,!*StdDynamicSharedBufferInfo)
	,	odtl_o_dynamic_rts_string	:: !String
	}	

OpenDynamicToLinker :: !String -> *OpenDynamicToLinker_Out

CloseDynamicToLinker :: *OpenDynamicToLinker_Out -> Bool

GetDynamicLinkerPath :: String;

CleanNewKey :: !String !String -> Bool;

SendEncodedDynamic :: !EncodedDynamic2 !String -> String;
