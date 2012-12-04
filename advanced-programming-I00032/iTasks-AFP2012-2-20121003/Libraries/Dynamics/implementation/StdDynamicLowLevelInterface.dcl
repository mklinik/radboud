definition module StdDynamicLowLevelInterface;

from StdOverloaded import class toString, class + (+), class - (-), class == (==);
from StdClass import class Eq, <>;
from StdInt import <<, >>, bitand, bitor, instance + (Int), instance - (Int), instance == (Int);
from StdBool import ||, not;
from StdFile import class FileSystem;
from StdMisc import abort;
from BitSet import :: BitSet;
from DefaultElem import class DefaultElem;
from EnDecode import class EnDecode;
from DynamicLinkerInterface import ::RunTimeIDW;
from StdDynamicTypes import :: LibraryInstanceTypeReference, :: LibRef;
from StdDynamicVersion import :: Version;

// low-level interface for dynamics
// General
make_start_node_index :== (0 bitor 3);

is_external_entry_node node_index :== ((node_index bitand 3) <> 3);

// Deconstructors for a NodeIndex (external references)
get_block_i		node_index :== ((node_index bitand 0x0000ffff) >> 2);
get_en_node_i	node_index :== (node_index >> 16);

// Constructors for a NodeIndex (with entry node 0)
create_node_index block_i	:== (block_i << 2) bitor 3;

// internal references; is_internal_reference and DYNAMIC_CONTAINS_BLOCKTABLE *must* hold
get_offset_from_internal_reference	internal_reference :== internal_reference >> 2;

// dereference an internal reference to the offset realtive from block start
dereference_internal_reference offset_of_reference internal_reference
	:== offset_of_reference - (get_offset_from_internal_reference internal_reference);

// Deconstructors for a prefix_set_and_string_ptr
is_reference prefix_set_and_desc_ptr				:== (is_internal_reference	prefix_set_and_desc_ptr) || (is_external_reference	prefix_set_and_desc_ptr);
is_internal_reference	prefix_set_and_desc_ptr		:== prefix_set_and_desc_ptr bitand 3 == 1;
is_external_reference	prefix_set_and_desc_ptr		:== prefix_set_and_desc_ptr bitand 3 == 3;

get_encoded_descriptor prefix_set_and_desc_ptr	:== get_string_offset prefix_set_and_desc_ptr;

// to be applied after get_encoded_descriptor
is_boxed encoded_descP 	:== encoded_descP == 0;
virtual_offset_start	:== 4;						// for a block

// to be applied after get_encoded_descriptor and is_boxed must be false
convert_to_descriptor_usage_entry encoded_descP :== convert_to_descriptor_usage_entry encoded_descP
where {
	convert_to_descriptor_usage_entry encoded_descP
		| is_boxed encoded_descP
			= abort "convert_to_descriptor_usage_entry: internal error";
		= (encoded_descP - 4) >> 2;
};

// obsolete ...
get_string_offset	prefix_set_and_string_ptr	:== prefix_set_and_string_ptr bitand 0x00ffffff;
// ... obsolete
get_prefix_set		prefix_set_and_string_ptr	:== (prefix_set_and_string_ptr >> 24)  bitand 0x000000ff;

// To be used on result of (get_prefix_set prefix_set_and_string_ptr) application
get_n_prefix	prefix_set	:==	prefix_set bitand 1;
get_d_prefix	prefix_set	:== prefix_set bitand 2;
get_k_prefix	prefix_set 	:== prefix_set bitand 4;
get_c_prefix	prefix_set	:== prefix_set bitand 8;
get_t_prefix	prefix_set	:== prefix_set bitand 16;
get_r_prefix	prefix_set	:== prefix_set bitand 32;

is_record		prefix_set	:== (get_r_prefix prefix_set <> 0); // || (get_k_prefix prefix_set <> 0);

// To be used on result of (get_?_prefix prefix_set)
to_char_prefix :: !Int -> Char;

// Order is important because in case of a labelname more than one used prefix, the offsets in
// the encoded graph are encoded in the order below. Should be tested.
GET_PREFIX_FUNC :== [get_r_prefix,get_t_prefix,get_c_prefix,get_k_prefix,get_d_prefix,get_n_prefix];

DATA_PREFIXES	:== ['r','t','k','d'];

// Header
:: DynamicHeader = {
		header_size			:: !Int			// size in bytes
	,	version_number		:: !Int			// version number representation
	,	graph_i				:: !Int			
	,	graph_s				:: !Int	
	,	block_table_i		:: !Int
	,	block_table_s		:: !Int
	,	dynamic_rts_info_i	:: !Int
	,	dynamic_rts_info_s	:: !Int
	,	stringtable_i		:: !Int			
	,	stringtable_s		:: !Int
	,	descriptortable_i	:: !Int
	,	descriptortable_s	:: !Int
	,	n_nodes				:: !Int
	,	descriptor_bitset_i	:: !Int
	,	descriptor_bitset_s	:: !Int
	};

default_dynamic_header :: DynamicHeader;

class BinaryDynamicIO m
where {
	bd_readi :: !*m -> (!Bool,!Int,!*m);
	bd_reads :: !*m !Int -> (!*{#Char},!*m);
	bd_seek :: !*m !Int !Int -> (!Bool,!*m);
	bd_delta_fp :: !*m -> (!Int,!*m);
	bd_freadsubstring :: !Int !Int !*{#Char} !*m -> (!Int,!*{#Char},!*m)
};

instance BinaryDynamicIO File;

:: *BinaryDynamicIO_String;
open_binary_dynamic_io_string :: !*{#Char} -> *BinaryDynamicIO_String;
close_binary_dynamic_io_string :: *BinaryDynamicIO_String -> *{#Char};
instance BinaryDynamicIO BinaryDynamicIO_String;

// Offset; should correspond with graph_to_string.c
HEADER_SIZE_OFFSET			:== 8;		// header size (in bytes)
VERSION_NUMBER_OFFSET		:== 12;		// version (major,minor) 		// little or big endian format?
GRAPH_OFFSET				:== 16;		// graph offset
GRAPH_SIZE					:== 20;		// graph size
BLOCK_TABLE_OFFSET			:== 24;
BLOCK_TABLE_SIZE			:== 28;
DYNAMIC_RTS_INFO_OFFSET		:== 32;		// info from dynamic rts; filled in by StdDynamic.icl
DYNAMIC_RTS_INFO_SIZE		:== 36;
// End sharing for StdDynamic.icl

STRINGTABLE_OFFSET			:== 40;		// stringtable offset
STRINGTABLE_SIZE			:== 44;		// stringtable size
DESCADDTRESTABLE_OFFSET		:== 48;		// descriptor address table offset
DESCADDRESSTABLE_SIZE		:== 52;		// descriptor address table size
N_NODES						:== 56;
DESCRIPTOR_BITSET_OFFSET	:== 60;		// descriptor bitset offset (determines what descriptors are)
DESCRIPTOR_BITSET_SIZE		:== 64; 	// descriptor bitset size

read_dynamic_header :: !*f -> *(Bool,DynamicHeader,*f) | BinaryDynamicIO f;

open_dynamic_as_binary :: !String *a -> *(Bool,DynamicHeader,*File,*a) | FileSystem a;

close_dynamic_as_binary :: !*File !*f -> (!Bool,!*f) | FileSystem f;

DYNAMIC_CONTAINS_BLOCKTABLE dynamic_header :== dynamic_header.block_table_i <> 0;

// Graph
read_graph_from_dynamic :: DynamicHeader !*f -> (!Bool,!String,!*f) | BinaryDynamicIO f;

// BlockTable
:: Block
	= {
		bk_block_n			:: !Int		// block identification
	,	bk_offset			:: !Int		// offset where block starts in encoded graph (fp)
	,	bk_size				:: !Int		// block size
	,	bk_n_node_entries	:: !Int		// # block entries - 1
	,	bk_entries			:: {#Int}	// if bk_n_node_entries > 0 then offsets in graph
	};
	
default_block_table :: BlockTable;
	
:: BlockTable
	:== {#Block};

read_block_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!BlockTable,!*f) | BinaryDynamicIO f;

read_block_table_as_string_from_dynamic :: DynamicHeader !*File -> (!Bool,.{#Char},!*File);

// Descriptor Usage Table
:: DescriptorUsageEntry
	= {
		prefix_set_and_string_ptr		:: !Int
	,	dus_library_instance_nr_on_disk	:: !Int
	,	bitset							:: !BitSet
	};
	
:: DescriptorUsageTable
	:== {#.DescriptorUsageEntry};
	
default_descriptor_usage_table :: DescriptorUsageTable;

default_descriptor_usage_entry :: !Int -> .DescriptorUsageEntry;
	

read_descriptor_usage_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!.DescriptorUsageTable,!*f) | BinaryDynamicIO f;

read_descriptor_bitset_from_dynamic :: DynamicHeader !*f -> (!Bool,!BitSet,!*f) | BinaryDynamicIO f;

// String Table
:: StringTable
	:== String;
	
read_string_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!StringTable,!*f) | BinaryDynamicIO f;

// Interface to dynamic run-time system
LinkBlock :: !String !Bool !Int !Int -> (!Int,Int,!String);

// in reply from a GraphToString-request; RangeID table
:: RangeID
	= {
		rid_n_range_id_entries	:: !Int
	,	rid_n_type_tables		:: !Int
	,	rid_range_entries		:: !{#RangeIDEntry}
	};
	
:: RangeIDEntry
	= {
		ride_begin_address		:: !Int
	,	ride_end_address		:: !Int
	,	ride_type_table_i		:: !Int
	};
	
default_range_id_entry :: RangeIDEntry;

RID_N_RANGE_ENTRIES_OFFSET		:== 0;
RID_N_TYPE_TABLES_OFFSET		:== 4;
RID_HEADER_SIZE					:== RID_N_TYPE_TABLES_OFFSET + 4;

RIDE_BEGIN_ADDRESS_OFFSET		:== 0;
RIDE_END_ADDRESS_OFFSET			:== 4;
RIDE_RUNTIME_ID_LIB_NUMBER		:== 8;
RIDE_SIZE						:== RIDE_RUNTIME_ID_LIB_NUMBER + 4;
	
instance toString RangeID;

// Type table usage table; constructed in dynamic_to_string
TTUT_UNUSED	:== 0xffffffff;

::	DynamicInfoArray :== {!DynamicInfo};

// dynamic info
:: DynamicInfo
	= {
		// Header
		di_version					:: !Version
	,	di_string_table				:: !StringTable
	,	di_descriptor_usage_table	:: !DescriptorUsageTable
	,	di_file_name				:: !String
	,	di_n_blocks					:: !Int
	
	// read_rts_info_from_dynamic reads the following fields
	// begin
	,	di_library_instance_to_library_index	:: !{LibraryInstanceKind}		// indexed by a RunTimeID, index in di_library_index_to_library_name
	,	di_library_index_to_library_name		:: !{#{#Char}}	// indexed by index from above array, string reference to {code,type}-library
	,	di_disk_type_equivalent_classes			:: !{#{LibraryInstanceTypeReference}}
	,	di_lazy_dynamics_a						:: !{#{#Char}}
	,	di_type_redirection_table				:: !{LibraryInstanceTypeReference}
	// end

	,	di_disk_id_to_library_instance_i		:: !{#Int}		// indexed by diskID
	,	di_disk_to_rt_dynamic_indices			:: !{#Int}		// ibdexed by disk_dynamic_index
	
	,	di_has_block_been_used					:: !{#Bool}
	,	di_rt_type_redirection_table			:: !{#RunTimeIDW}
	};	

:: LibraryInstanceKind
	= LIK_LibraryInstance !LIK_LibraryInstance
	| LIK_Empty
	;
	
instance DefaultElem LibraryInstanceKind;

get_index_in_di_library_index_to_library_name :: !LibraryInstanceKind -> Int;
	
:: LIK_LibraryInstance
	= {
		lik_index_in_di_library_index_to_library_name			:: !Int
	};

instance DefaultElem DynamicInfo;
	
default_dynamic_info :: DynamicInfo;

read_rts_info_from_dynamic :: DynamicHeader !*f -> (!Bool,!DynamicInfo,!*f) | BinaryDynamicIO f;

class DynamicInfoOps s
where {
	UpdateDynamicInfo :: !Int !DynamicInfo !*s -> *s
};

instance DynamicInfoOps (DynamicInfoArray);

instance EnDecode DynamicInfo;

// RunTimeID/DiskID  at run-time:
RTID_DATA_DYNAMIC				:== 0; // no code needed

// Library instances at run-time are numbered from RTID_LIBRARY_INSTANCE_ID_START
RTID_LIBRARY_INSTANCE_ID_START	:== 1; // by default, a dummy element is created

// Otherwise
RTID_DISKID_RENUMBER_START		:== 1; // change also gts_range_id; counting from 1

// Build (lazy) block labels
BUILD_BLOCK_LABEL				:== "e__DynamicGraphConversion__nbuild__block";
BUILD_LAZY_BLOCK_LABEL			:== "e__DynamicGraphConversion__nbuild__lazy__block";

// BUILD_BLOCK (run-time) format, from gts_build_block.c, DynamicGraphConversion.{dcl,icl}
BUILD_DYNAMIC_NODE__INDEX_PTR	:== 4;
BUILD_DYNAMIC_GDID__PTR			:== 8;

// BUILD_BLOCK (on disk) format, from gts_build_block.c:
BUILD_LAZY_DYNAMIC_ON_DISK__NODE_INDEX		:== 0;
BUILD_LAZY_DYNAMIC_ON_DISK__DYNAMIC_ID		:== 4;

BUILD_LAZY_DYNAMIC_ON_DISK__LAST_FIELD		:== (BUILD_LAZY_DYNAMIC_ON_DISK__DYNAMIC_ID + 4);
BUILD_LAZY_DYNAMIC_ON_DISK__BSIZE			:== BUILD_LAZY_DYNAMIC_ON_DISK__LAST_FIELD;

// A lazy dynamic reference is generated by the graph_to_string conversion routine. Update also
// gts_lazy_dynamic_reference.c
:: LazyDynamicReference
	= { 
		ldr_id							:: !Int			// run-time id of lazy dynamic (non-lazy id)
	,	ldr_lazy_dynamic_index			:: !Int			// disk id for lazy dynamic (block)
	};
// all instance of the graph_to_string-conversion function *must* use the same LazyDynamicReference.

LazyDynamicReference_String		:== "LazyDynamicReference";

instance DefaultElem LazyDynamicReference;

instance EnDecode LazyDynamicReference;

StdDynamicLowLevelInterfaceModule_String	:== "StdDynamicLowLevelInterface";

GlobalDynamicInfoDummy_String 	:== "GlobalDynamicInfoDummy";

RunTimeIDW_String :== "RunTimeIDW";

DynamicLinkerInterfaceModule_String	:== "DynamicLinkerInterface";

//instance DefaultElem LazyDynamicReference;
//instance EnDecode LazyDynamicReference;

INITIAL_LAZY_DYNAMIC_INDEX	:== 100;

// interface to graph_to_string-routine
// update gts_code_and_type_runtime_ids.c
CODE_LIBRARY_INSTANCE	:== 0x80000000;
TYPE_LIBRARY_INSTANCE	:== 0x40000000;
LIBRARY_INSTANCE_MASK	:== 0x3fffffff;

GET_LIBRARY_INSTANCE_I x	:== x bitand LIBRARY_INSTANCE_MASK;

IS_CODE_LIBRARY_INSTANCE x 	:== (x bitand CODE_LIBRARY_INSTANCE) <> 0;
IS_TYPE_LIBRARY_INSTANCE x	:== (x bitand TYPE_LIBRARY_INSTANCE) <> 0;

:: LazyTypeReference
	= {
		ltr_lazy_dynamic_index	:: !Int		// within main dynamic (on disk)
	,	ltr_library_instance_i	:: !Int		// relative from the lazy dynamic (on disk)
	};
	
instance EnDecode LazyTypeReference;
instance DefaultElem LazyTypeReference;

// Lazy library instances; change also gts_runtime_id.c
// A *lazy* library instance is part of a lazy dynamic and is referenced from its main
// dynamic.
// RunTimeID bit_field(bits 31-0)
// - bit31 == 0, then bits 0-30 hold the library instance number within the main dynamic
// - bit31 == 1,
//               0-15	lazy dynamic index				(16 bits)
//				 16-30	lazy library instance index		(15 bits)

class encode_lib_ref a :: !a -> Int;

instance encode_lib_ref LibRef;

instance encode_lib_ref LibraryInstanceTypeReference;

decode_lib_ref :: !Int -> LibRef;
LLI_IS_MAIN_LIBRARY_INSTANCE x	:== (x bitand 0x80000000) == 0;
LLI_IS_LAZY_LIBRARY_INSTANCE x	:== not (LLI_IS_MAIN_LIBRARY_INSTANCE x);

LLI_EXTRACT_MAIN_LIBRARY_INSTANCE_INDEX x :== lli_extract_main_library_instance_index x
where {
	lli_extract_main_library_instance_index x
		| LLI_IS_MAIN_LIBRARY_INSTANCE x 
			= x;
			= abort "LLI_EXTRACT_MAIN_LIBRARY_INSTANCE_INDEX (macro): not a library instance of main dynamic";
};

instance EnDecode LibraryInstanceTypeReference;

create_dynamic_file_name :: !String -> String;

// see gts_range_id.c
INITIAL_TYPE_REFERENCE_NUMBER	:== 0;

:: EncodedTypeReference
	= {
		etr_type_module_name	:: !String
	,	etr_library_instance_i	:: !Int 		// lazy or not
	};
	
instance DefaultElem EncodedTypeReference;

hex_int2 :: !Int -> String;