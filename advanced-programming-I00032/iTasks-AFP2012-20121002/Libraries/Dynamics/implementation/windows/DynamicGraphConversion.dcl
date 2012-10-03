definition module DynamicGraphConversion

from StdDynamicTypes import :: DummyModuleID
from StdDynamicLowLevelInterface import :: BlockTable, :: Block,
	:: LazyDynamicReference, :: DynamicHeader, class BinaryDynamicIO
from DynamicLinkerInterface import :: RunTimeIDW
// FIXME: remove this when the compiler no longer translates
// :: Dynamic to :: DynamicTemp
from _SystemDynamic import :: DynamicTemp

:: GlobalDynamicInfo = {
	// general
		gdi_file_name					:: !String
	,	first_time						:: !Bool
	// block table 
	,	id								:: !Int				// id from Dynamic Linker
	,	block_table						:: !BlockTable		
	,	graph_blocks					:: !{String}		// filepointer to start of graph
	,	graph_pointers					:: !{#.{Int}}
	// 
	,	diskid_to_runtimeid				:: !{#Int}			// conversion from DiskId (disguished as RunTimeId) to *real* runtimeID (library instances)
	,	di_disk_to_rt_dynamic_indices	:: !{#Int} 			// conversion from disk to runtime index for lazy dynamics
	,	di_dummy						:: !String
	,	di_type_redirection_table		:: !{#RunTimeIDW}
	,	di_shared_blocks				:: !{#{#Int}}
	}
// The # above ensure that no ARRAY node is inserted.

// force compiler to pass entire records instead of all entries
:: GlobalDynamicInfoDummy = {
		gdid			:: !GlobalDynamicInfo
	}

:: Pointer

build_lazy_block :: !Int !Int -> a

build_block :: !Int !GlobalDynamicInfoDummy -> a /* algemeen: een stuk graaf */

build_block_without_evaluating_graph :: !Int !GlobalDynamicInfoDummy -> (a,!Int) /* algemeen: een stuk graaf */
build_block_indirection :: a -> a;

//build_block :: !Int !GlobalDynamicInfoDummy -> Pointer /* algemeen: een stuk graaf */
// John: Pointer should be changed in BuildBlockResult. I think it should left as it is
// because normally the user doesn't see this.

instance EncodedDynamic String

// -----------------------------------------------------------------
:: Wrap a 
	= { 
		wrap_info		:: !a
	};

// aanpassen van gesharde type door alle library instanties		
:: *CopyGraphToStringArguments
	= {
		cgtsa_dynamic					:: Dynamic
	,	cgtsa_code_library_instances	:: !*{#Int}
	,	cgtsa_type_library_instances	:: !*{#Int}
	,	cgtsa_range_table				:: !{#Char}
	};
		
:: *CopyGraphToStringResults
	= {
		cgtsr_encoded_dynamic			:: !*{#Char}
	,	cgtsr_code_library_instances	:: !*{#T_ypeConsSymbolInfo}		// unused
	,	cgtsr_type_library_instances	:: !*{#Int}
	,	cgtsr_lazy_dynamic_references	:: !{#LazyDynamicReference}
	,	cgtsr_runtime_ids				:: !{#RunTimeIDW}
	}
	
:: T_ypeConsSymbolInfo
	= {
		tcsi_type_and_module_name		:: !String
	,	tcsi_rt_library_instance		:: !Int
	}	
	
class EncodedDynamic a
where 
	dynamic_to_string :: !Dynamic -> (!Bool,!*a)

:: *EncodedDynamic2
	= { 
		ed_encoded_graph	:: !*{#Char}
	,	ed_dynamic_rts_info	:: !*{#Char}
	}

instance EncodedDynamic EncodedDynamic2

string_to_dynamic :: !String -> (!Bool,!Dynamic)

init_dynamic :: !String !DynamicHeader !*a -> *(.Bool,.GlobalDynamicInfoDummy,*a) | BinaryDynamicIO a
copy_graph_to_string_OK :: !(Wrap CopyGraphToStringArguments) -> (Wrap CopyGraphToStringResults);

