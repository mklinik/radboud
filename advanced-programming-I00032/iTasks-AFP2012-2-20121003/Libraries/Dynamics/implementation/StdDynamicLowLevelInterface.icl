implementation module StdDynamicLowLevelInterface;

import StdEnv;
import StdDynamicVersion;
import StdDynamicTypes;
import BitSet;
import DefaultElem;
import StdMaybe;
from DynamicUtilities import ends;
import DynamicLinkerInterface;
from DynIDMacros import EXTENSION_USER_DYNAMIC;
import EnDecode;

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
to_char_prefix 1	= 'n';
to_char_prefix 2	= 'd';
to_char_prefix 4	= 'k'; 
to_char_prefix 8	= 'c'; 
to_char_prefix 16	= 't'; 
to_char_prefix 32	= 'r'; 

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
default_dynamic_header	
	= { DynamicHeader |
		header_size			= 0		
	,	version_number		= 0			
	,	graph_i				= 0			
	,	graph_s				= 0	
	,	block_table_i		= 0
	,	block_table_s		= 0
	,	dynamic_rts_info_i	= 0
	,	dynamic_rts_info_s	= 0
	,	stringtable_i		= 0		
	,	stringtable_s		= 0
	,	descriptortable_i	= 0
	,	descriptortable_s	= 0
	,	n_nodes				= 0
	,	descriptor_bitset_i	= 0
	,	descriptor_bitset_s	= 0
	};
	
N_DUMMY_BYTES_BEFORE_DYNAMIC_HEADER		:== 0;
	
class BinaryDynamicIO m
where {
	bd_readi :: !*m -> (!Bool,!Int,!*m);
	bd_reads :: !*m !Int -> (!*{#Char},!*m);
	bd_seek :: !*m !Int !Int -> (!Bool,!*m);
	bd_delta_fp :: !*m -> (!Int,!*m);
	bd_freadsubstring :: !Int !Int !*{#Char} !*m -> (!Int,!*{#Char},!*m)
};

instance BinaryDynamicIO File
where {

	bd_readi file		= freadi file;
	bd_reads file i		= freads file i;
	bd_seek file i1 i2	= fseek file i1 i2;
	bd_delta_fp	file	= (0,file);
	bd_freadsubstring i n s f = freadsubstring i n s f;
};

:: *BinaryDynamicIO_String 
	= {
		offset	:: !Int
	,	data	:: !*{#Char}
	};
	
open_binary_dynamic_io_string :: !*{#Char} -> *BinaryDynamicIO_String;
open_binary_dynamic_io_string data
	= {
		offset	= 0
	,	data	= data
	};
	
close_binary_dynamic_io_string :: *BinaryDynamicIO_String -> *{#Char};
close_binary_dynamic_io_string {data}
	= data;

between start middle end	:==  start <= middle && middle <= end;

instance BinaryDynamicIO BinaryDynamicIO_String;
where {
	bd_readi file=:{offset,data}	
		#! s_data = size data;
		| not (between 0 offset (dec s_data))
			= (False,0,file)
			
		#! (i,data)
			= FromStringToIntU data offset;
		#! file
			= { 
				offset 	= offset + 4
			,	data	= data
			};
		= (True,i,file);
		
	bd_reads file=:{data, offset} i
//AvW		= abort "bd_reads; BinaryDynamicIO String";
		# newoffset = offset + i;																		//AvW
		= ("" +++. data % (offset, newoffset - 1), {file & data = "" +++. data, offset = newoffset}); 	//AvW

	bd_seek file=:{data} offset seek_mode
		| seek_mode == FSeekSet
			#! (s_data,data)
				= usize data;
			= (between 0 offset (dec s_data),{ offset = offset, data = data });
		= abort "bd_seek; BinaryDynamicIO; internal error; seek mode unimplemented";
	
	bd_delta_fp	file	= abort "bd_delta_fp; BinaryDynamicIO String";

	bd_freadsubstring i n s file=:{data,offset}
		#! (s_s,s)
			= usize s;
		#! (s_data,data)
			= usize data;
		#! (new_offset,s,data)
			= bd_read_substring i s_s s offset s_data data;
		#! file 
			= {
				offset	= new_offset
			,	data	= data
			};
		= (new_offset - offset,s,file);
	where {
		bd_read_substring i s_s s offset s_data data
			| i >= s_s || offset >= s_data
				= (offset,s,data);
				
			#! (c,data)
				= data![offset];
			= bd_read_substring (inc i) s_s {s & [i] = c} (inc offset) s_data data;
	};
};

open_dynamic_as_binary :: !String *a -> *(Bool,DynamicHeader,*File,*a) | FileSystem a;
open_dynamic_as_binary file_name files
	#! (ok1,file,files)
		= fopen file_name FReadData files;
	| not ok1
		= error_reading_dynamic_header file files;
				
	# (ok,dynamic_header,file)
		= read_dynamic_header file;
	| not ok
		= error_reading_dynamic_header file files;
	= (ok,dynamic_header,file,files);
where {
	error_reading_dynamic_header file files
		#! (_,files)
			= close_dynamic_as_binary file files;
		= (False,default_dynamic_header, stderr, files);
};
		
N_BYTES_BEFORE_HEADER_START	:== 0;

read_dynamic_header :: !*f -> *(Bool,DynamicHeader,*f) | BinaryDynamicIO f;
read_dynamic_header file
	#! (_,file)
		= bd_seek file N_BYTES_BEFORE_HEADER_START FSeekSet
 	#! (ok3,header_s,file)
		= bd_readi file;

	#! header
		= WriteLong (createArray (header_s + 4) ' ') (HEADER_SIZE_OFFSET - 8) header_s;
	#! (_,header,file)
		= bd_freadsubstring (VERSION_NUMBER_OFFSET - 8) header_s header file;
		
	#! (dynamic_header,file)
		= build_dynamic_header header_s header file;

	= (ok3,dynamic_header,file);

// Offset; should correspond with graph_to_string.c
HEADER_SIZE_OFFSET			:== 8;		// header size (in bytes)
VERSION_NUMBER_OFFSET		:== 12;		// version (major,minor) 		// little endian format
GRAPH_OFFSET				:== 16;		// graph offset
GRAPH_SIZE					:== 20;		// graph size
BLOCK_TABLE_OFFSET			:== 24;
BLOCK_TABLE_SIZE			:== 28;
DYNAMIC_RTS_INFO_OFFSET		:== 32;		// info from dynamic rts; filled in by StdDynamic.icl
DYNAMIC_RTS_INFO_SIZE		:== 36;
// End sharing for _SystemDynamic.icl

STRINGTABLE_OFFSET			:== 40;		// stringtable offset
STRINGTABLE_SIZE			:== 44;		// stringtable size
DESCADDTRESTABLE_OFFSET		:== 48;		// descriptor address table offset
DESCADDRESSTABLE_SIZE		:== 52;		// descriptor address table size
N_NODES						:== 56;
DESCRIPTOR_BITSET_OFFSET	:== 60;		// descriptor bitset offset (determines what descriptors are)
DESCRIPTOR_BITSET_SIZE		:== 64; 	// descriptor bitset size

build_dynamic_header :: !Int !String !*f -> (DynamicHeader,!*f) | BinaryDynamicIO f;
build_dynamic_header header_s header file
	#! version_number = FromStringToInt header (VERSION_NUMBER_OFFSET - HEADER_SIZE_OFFSET)
	#! dynamic_header 
		= case (getVersionNumber version_number) of {
			0x00010101
				#! dynamic_header
					= { default_dynamic_header &
						// required
							header_size			= header_s
						,	version_number		= version_number
						,	graph_i				= (FromStringToInt header (GRAPH_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	graph_s				= FromStringToInt header (GRAPH_SIZE - HEADER_SIZE_OFFSET)
						,	block_table_i		= (FromStringToInt header (BLOCK_TABLE_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	block_table_s		= FromStringToInt header (BLOCK_TABLE_SIZE - HEADER_SIZE_OFFSET)
						,	dynamic_rts_info_i	= (FromStringToInt header (DYNAMIC_RTS_INFO_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	dynamic_rts_info_s	= FromStringToInt header (DYNAMIC_RTS_INFO_SIZE - HEADER_SIZE_OFFSET)


						,	stringtable_i		= (FromStringToInt header (STRINGTABLE_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	stringtable_s		= FromStringToInt header (STRINGTABLE_SIZE - HEADER_SIZE_OFFSET)
						,	descriptortable_i	= (FromStringToInt header (DESCADDTRESTABLE_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	descriptortable_s	= FromStringToInt header (DESCADDRESSTABLE_SIZE - HEADER_SIZE_OFFSET)
						,	n_nodes				= FromStringToInt header (N_NODES - HEADER_SIZE_OFFSET)

						,	descriptor_bitset_i	= (FromStringToInt header (DESCRIPTOR_BITSET_OFFSET - HEADER_SIZE_OFFSET)) //+ delta_fp
						,	descriptor_bitset_s	= FromStringToInt header (DESCRIPTOR_BITSET_SIZE - HEADER_SIZE_OFFSET)
					};
				-> dynamic_header;		
			_
				-> abort ("BuildDynamicHeader; unknown version number <" +++ hex_int (getVersionNumber version_number));
		};
	= (dynamic_header,file);

close_dynamic_as_binary :: !*File !*f -> (!Bool,!*f) | FileSystem f;
close_dynamic_as_binary file files
	= fclose file files;
	
DYNAMIC_CONTAINS_BLOCKTABLE dynamic_header :== dynamic_header.block_table_i <> 0;
	
// Graph
read_graph_from_dynamic :: DynamicHeader !*f -> (!Bool,!String,!*f) | BinaryDynamicIO f;
read_graph_from_dynamic {graph_i,graph_s} file
	// set file pointer to start of encoded graph
	#! (ok,file)
		= bd_seek file graph_i FSeekSet;
	| not ok
		= (False,{},file);
		
	// read the graph
	#! (graph,file)
		= bd_reads file graph_s;
	= (size graph == graph_s,graph,file);

// BlockTable
:: Block
	= {
		bk_block_n			:: !Int		// block identification
	,	bk_offset			:: !Int		// offset where block starts in encoded graph
	,	bk_size				:: !Int		// block size
	,	bk_n_node_entries	:: !Int		// # block entries - 1
	,	bk_entries			:: {#Int}	// if bk_n_node_entries > 0 then offsets in graph
	};
	
:: BlockTable
	:== {#Block};
	
default_block_table :: BlockTable;
default_block_table = {};
	
default_block :: Block;
default_block
	= {
		bk_block_n			= 0
	,	bk_offset			= 0
	,	bk_size				= 0
	,	bk_n_node_entries	= 0
	,	bk_entries			= {}
	};

read_block_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!BlockTable,!*f) | BinaryDynamicIO f;
read_block_table_from_dynamic dynamic_header=:{block_table_s,block_table_i} file
	| block_table_i == 0
		= abort "read_block_table_from_dynamic: block_table_i <> 0 is not permitted";
		
	// set file pointer to start of block table
	#! (ok,file)
		= bd_seek file block_table_i FSeekSet;
	| not ok
		= (False,{},file);
	
	// read amount of blocks	
	#! (ok,n_blocks,file)
		= bd_readi file;
	| not ok
		= (False,{},file);
		
	#! (delta_fp,file) = bd_delta_fp file
	#! (ok,block_table,file)
		= read_block_table 0 n_blocks delta_fp (createArray n_blocks default_block) file;

	= (ok,block_table,file);
where {
	read_block_table i limit delta_fp block_table file
		| i == limit
			= (True,block_table,file);
			
		#! (ok1,bk_block_n,file)
			= bd_readi file;
		#! (ok2,bk_offset,file)
			= bd_readi file;
		#! (ok3,bk_size,file)
			= bd_readi file;
		#! (ok4,bk_n_node_entries,file)
			= bd_readi file;
	
		#! (ok5,bk_entries,file)
			= read_entry_node_offsets 0 bk_n_node_entries (createArray bk_n_node_entries 0) file;
		| ok1 && ok2 && ok3 && ok4 && ok5
			#! block
				= { Block |
					bk_block_n			= bk_block_n
				,	bk_offset			= bk_offset
				,	bk_size				= bk_size
				,	bk_n_node_entries	= bk_n_node_entries
				,	bk_entries			= bk_entries
				};
			= read_block_table (inc i) limit delta_fp {block_table & [bk_block_n] = block} file;
			
			= abort "read_block_table: corrupt dynamic (or internal error)";
	where {
		read_entry_node_offsets :: !Int !Int !*{#Int} !*f -> (!Bool,!*{#Int},!*f) | BinaryDynamicIO f;
		read_entry_node_offsets i limit bk_entries file
			| i == limit			
				= (True,bk_entries,file);
				
				#! file
					= case (i == 0) of {
						True	-> file;
						False	-> file;
					};
				#! (ok,en_node_offset,file)
					= bd_readi file;
				| not ok
					= (False,bk_entries,file);
				= read_entry_node_offsets (inc i) limit {bk_entries & [i] = en_node_offset} file;
	} // read_block_table
} // read_block_table_from_dynamic
	
read_block_table_as_string_from_dynamic :: DynamicHeader !*File -> (!Bool,.{#Char},!*File);
read_block_table_as_string_from_dynamic dynamic_header=:{block_table_s,block_table_i} file
	| block_table_i == 0
		= abort "read_block_table_from_dynamic: block_table_i <> 0 is not permitted";
		
	// set file pointer to start of block table
	#! (ok,file)
		= fseek file block_table_i FSeekSet;
	| not ok
		= (False,{},file);
		
	#! (block_table_as_string,file)
		= freads file block_table_s;
	#! (s_block_table_as_string,block_table_as_string)
		= usize block_table_as_string;
		
	= (s_block_table_as_string == block_table_s,block_table_as_string,file);

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
default_descriptor_usage_table = {};
	
default_descriptor_usage_entry :: !Int -> .DescriptorUsageEntry;
default_descriptor_usage_entry n_elements
	= { DescriptorUsageEntry |
		prefix_set_and_string_ptr			= 0
	,	dus_library_instance_nr_on_disk		= 0
	,	bitset								= EmptyBitSet
	};

read_descriptor_usage_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!.DescriptorUsageTable,!*f) | BinaryDynamicIO f;
read_descriptor_usage_table_from_dynamic dynamic_header=:{descriptortable_i,descriptortable_s} file
	// set file pointer to start of block table
	#! (ok,file)
		= bd_seek file descriptortable_i FSeekSet;
	| not ok
		= (False,{},file);

	// read size of usage bit set and the amount of usage entries
	#! (ok,usage_bit_set_wsize,n_usage_entries,file)
		= case (DYNAMIC_CONTAINS_BLOCKTABLE dynamic_header) of {
			True
				#! (ok1,usage_bit_set_wsize,file)
					= bd_readi file;
				#! (ok2,n_usage_entries,file)
					= bd_readi file;
				-> (ok1&&ok2,usage_bit_set_wsize,n_usage_entries,file);
			False
				-> (True,0,descriptortable_s >> 2,file);
		};
	| not ok
		= (False,{},file);	
		
	// read descriptor usage table
	#! descriptor_usage_table
		= {	(default_descriptor_usage_entry (usage_bit_set_wsize << 5)) \\ i <- [1..n_usage_entries] };
	#! (ok,descriptor_usage_table,file)
		= read_descriptor_usage_table 0 n_usage_entries descriptor_usage_table usage_bit_set_wsize n_usage_entries file;
		
	= (ok,descriptor_usage_table,file);
where {
	read_descriptor_usage_table i limit descriptor_usage_table usage_bit_set_wsize n_usage_entries file
		| i == limit
			= (True,descriptor_usage_table,file);
		
		#! (ok1,prefix_set_and_string_ptr,file)
			= bd_readi file;
		#! (ok2,dus_library_instance_nr_on_disk,file)
			= bd_readi file;
		| dus_library_instance_nr_on_disk == 0
			// there must at least be one library providing an implementation for the types
			// in the dynamic.
			= abort "read_descriptor_usage_table; corrupt dynamic";

		#! (ok3,bitset,file)
			= read_bitset 0 usage_bit_set_wsize (createArray usage_bit_set_wsize 0) file;
		| not ok1 || not ok2 || not ok3
			= (False,descriptor_usage_table,file);
			
		// problem with 2.0 compiler
		#! dut 
			= { DescriptorUsageEntry |
				prefix_set_and_string_ptr		= prefix_set_and_string_ptr
			,	dus_library_instance_nr_on_disk	= dus_library_instance_nr_on_disk
			,	bitset							= { BitSet |
													n_elements	= (usage_bit_set_wsize << 5) // upper bound n_usage_entries
												,	map			= bitset
												}
			};
		#! descriptor_usage_table
			= { descriptor_usage_table & [i] = dut};
		= read_descriptor_usage_table (inc i) limit descriptor_usage_table usage_bit_set_wsize n_usage_entries file;
	where {
	} // read_descriptor_usage_table
} // read_descriptor_usage_table_from_dynamic
////3.1

read_bitset :: !Int !Int !*{#Int} !*f -> (!Bool,!*{#Int},!*f) | BinaryDynamicIO f;
read_bitset j limit bitset file
	| j == limit
		= (True,bitset,file);
		
	#! (ok,word_from_bitset,file)
		= bd_readi file;
	| not ok
		= (False,bitset,file);
	
	= read_bitset (inc j) limit { bitset & [j] = word_from_bitset } file;

read_descriptor_bitset_from_dynamic :: DynamicHeader !*f -> (!Bool,!BitSet,!*f) | BinaryDynamicIO f;
read_descriptor_bitset_from_dynamic dynamic_header=:{descriptor_bitset_i,descriptor_bitset_s,graph_s} file
	// set file pointer to start of block table
	#! (ok,file)
		= bd_seek file descriptor_bitset_i FSeekSet;
	| not ok
		= (False,EmptyBitSet,file);
		
	| not (DYNAMIC_CONTAINS_BLOCKTABLE dynamic_header)
		= abort "read_descriptor_bitset_from_dynamic; a dynamic should contain a block table";		
	
	// 
	#! n_words
		= descriptor_bitset_s >> 2;
	#! computed_bitset_size
		= ((graph_s >> 2) + 31) >> 5;
	| n_words <> computed_bitset_size
		= abort ("read_descriptor_bitset_from_dynamic; internal error; descriptor bitset size mismatch " +++
			 hex_int descriptor_bitset_s +++ " - " +++ toString computed_bitset_size);

	#! (ok3,bitset,file)
		= read_bitset 0 n_words (createArray n_words 0) file;
	
	#! descriptor_bitset
		= {
			n_elements	= graph_s >> 2
		,	map			= bitset
		};
	= (ok3,descriptor_bitset,file);
		
// String Table
:: StringTable
	:== String;
	
read_string_table_from_dynamic :: DynamicHeader !*f -> (!Bool,!StringTable,!*f) | BinaryDynamicIO f;
read_string_table_from_dynamic {stringtable_i,stringtable_s} file
	// set file pointer to start of string table
	#! (ok,file)
		=  bd_seek file stringtable_i FSeekSet;
	| not ok
		= (False,{},file);
		
	#! (string_table,file)
		= bd_reads file stringtable_s;
	= (size string_table == stringtable_s,string_table,file);

// Interface to dynamic run-time system
LinkBlock :: !String !Bool !Int !Int -> (!Int,Int,!String);
LinkBlock file_name first_time id block_i
	# gba_i
		= {	gba_i_filename				= file_name
		,	gba_i_first_time			= first_time
		,	gba_i_id					= id
		,	gba_i_block_i				= block_i
		,	gba_i_dynamic_rts_string	= ""
		};
	# (_,{gba_o_id,gba_o_addresses})
		= GetBlockAddresses2 gba_i;
	| size gba_o_addresses <= 8
		= abort "error";
	#! gba_o_addresses
		= gba_o_addresses  % (8, dec (size gba_o_addresses));
	= (gba_o_id,abort "LinkBlock",gba_o_addresses ); //% (4,size gba_o_addresses));
	
doreqS :: !String -> String;
doreqS _ =
	code { 
		ccall DoReqS "S-S"
	};
			
// Utilities
FromStringToInt :: !String !Int -> Int;
FromStringToInt array i
	= (toInt v0)+(toInt v1<<8)+(toInt v2<<16)+(toInt v3<<24);
where {
	v0= array.[i];
	v1
		= array.[i+1];
	v2 
		= array.[i+2];
	v3  
		= array.[i+3];
} // FromStringToInt

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
	
hex_int :: !Int -> String;
hex_int i
	#! b0 
		= hex (i bitand 0x000000ff);
	#! b1
		= hex ((i bitand 0x0000ff00) >> 8);
	#! b2 
		= hex ((i bitand 0x00ff0000) >> 16);
	#! b3
		= hex ((i bitand 0xff000000) >> 24);
	= b3 +++ b2 +++ b1 +++ b0;

hexdigit :: !Int -> Char;
hexdigit i
	| i<10
		= toChar (toInt '0'+i);
		= toChar (toInt 'A'+i-10);

hex :: !Int -> String;
hex i
	#! i1 
		=(i bitand 0xf0) >> 4;
	#! i2
		=i bitand 0xf;
	= toString (hexdigit i1)+++toString (hexdigit i2);
	
:: LibraryInfo
	= {
		li_code_start	:: !Int
	,	li_code_end		:: !Int
	,	li_data_start	:: !Int
	,	li_data_end		:: !Int
	,	li_name			:: !String		// location where to find the library; access path to library
	,	li_set			:: !Int			// used iff li_name1 == li_name2 and indicates the set of labels to be linked with one instance of library
										// it might be possible to collapse several sets but that is future optimization
	};
	
// in reply from a GraphToString-request; RangeID table
// dependencies: 
// - graph_to_string-conversion routine which uses it to identify the library to which a particular type or
//   piece of code belongs to.
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
default_range_id_entry
	= {
		ride_begin_address		= 0
	,	ride_end_address		= 0
	,	ride_type_table_i		= 0
	};

RID_N_RANGE_ENTRIES_OFFSET		:== 0;
RID_N_TYPE_TABLES_OFFSET		:== 4;
RID_HEADER_SIZE					:== RID_N_TYPE_TABLES_OFFSET + 4;

RIDE_BEGIN_ADDRESS_OFFSET		:== 0;
RIDE_END_ADDRESS_OFFSET			:== 4;
RIDE_RUNTIME_ID_LIB_NUMBER		:== 8;
RIDE_SIZE						:== RIDE_RUNTIME_ID_LIB_NUMBER + 4;

instance DefaultElem RangeIDEntry
where {
	default_elem
		= {
			ride_begin_address		= 0
		,	ride_end_address		= 0
		,	ride_type_table_i		= 0
		};
};	

instance fromString RangeID
where {
	fromString s
		#! rid_n_range_id_entries = FromStringToInt s RID_N_RANGE_ENTRIES_OFFSET
		#! rid_n_type_tables = FromStringToInt s RID_N_TYPE_TABLES_OFFSET
		
		#! range_ids = createArray rid_n_range_id_entries default_elem
		#! range_ids = decode_entry 0 rid_n_range_id_entries RID_HEADER_SIZE range_ids
		
		#! range_id
			= {
				rid_n_range_id_entries	= rid_n_range_id_entries
			,	rid_n_type_tables		= rid_n_type_tables
			,	rid_range_entries		= range_ids
			}
		= range_id
	where {
		decode_entry i limit offset range_id 
			| i == limit
				= range_id;
				
				#! ride_begin_address
					= FromStringToInt s (offset +  RIDE_BEGIN_ADDRESS_OFFSET);
				#! ride_end_address
					= FromStringToInt s (offset +  RIDE_END_ADDRESS_OFFSET);
				#! ride_type_table_i
					= FromStringToInt s (offset +  RIDE_RUNTIME_ID_LIB_NUMBER);
					
				#! ride 
					= {
						ride_begin_address	= ride_begin_address
					,	ride_end_address	= ride_end_address
					,	ride_type_table_i	= ride_type_table_i
					};
				= decode_entry (inc i) limit (offset + RIDE_SIZE) { range_id & [i] = ride };
	}
}

instance toString RangeID
where {
	toString {rid_n_range_id_entries,rid_n_type_tables,rid_range_entries}
		#! range_id
			= createArray (RID_HEADER_SIZE + RIDE_SIZE * rid_n_range_id_entries) '\0';
			
		// Header
		#! range_id
			= WriteLong range_id RID_N_RANGE_ENTRIES_OFFSET rid_n_range_id_entries;
		#! range_id
			= WriteLong range_id RID_N_TYPE_TABLES_OFFSET rid_n_type_tables;
			
		// Entries
		# range_id
			= encode_entry 0 rid_n_range_id_entries RID_HEADER_SIZE range_id
		= range_id;
	where {
		encode_entry i limit offset range_id 
			| i == limit
				= range_id;

				#! {ride_begin_address,ride_end_address,ride_type_table_i}
					= rid_range_entries.[i];
				#! range_id
					= WriteLong range_id (offset +  RIDE_BEGIN_ADDRESS_OFFSET) ride_begin_address;
				#! range_id
					= WriteLong range_id (offset +  RIDE_END_ADDRESS_OFFSET) ride_end_address;
				#! range_id
					= WriteLong range_id (offset +  RIDE_RUNTIME_ID_LIB_NUMBER) ride_type_table_i; //(ride_type_table_i << 12);
				= encode_entry (inc i) limit (offset + RIDE_SIZE) range_id;
	};
};

// Type table usage table; constructed in dynamic_to_string
TTUT_UNUSED	:== 0xffffffff;

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
	,	di_library_index_to_library_name		:: !{#{#Char}}					// indexed by index from above array, string reference to {code,type}-library
	,	di_disk_type_equivalent_classes			:: !{#{LibraryInstanceTypeReference}}
	,	di_lazy_dynamics_a						:: !{#{#Char}}
	,	di_type_redirection_table				:: !{LibraryInstanceTypeReference}
	// end
	
	//
	,	di_disk_id_to_library_instance_i		:: !{#Int}		// indexed by diskID
	,	di_disk_to_rt_dynamic_indices			:: !{#Int}		// ibdexed by disk_dynamic_index

	,	di_has_block_been_used					:: !{#Bool}		// indexed by block index from BlockTable
	,	di_rt_type_redirection_table			:: !{#RunTimeIDW}
	};	

:: LibraryInstanceKind
	= LIK_LibraryInstance !LIK_LibraryInstance
	| LIK_Empty
	;
	
instance DefaultElem LibraryInstanceKind
where {
	default_elem 
		= LIK_Empty;
};
	
get_index_in_di_library_index_to_library_name :: !LibraryInstanceKind -> Int;
get_index_in_di_library_index_to_library_name (LIK_LibraryInstance {LIK_LibraryInstance | lik_index_in_di_library_index_to_library_name})
	= lik_index_in_di_library_index_to_library_name;

:: LIK_LibraryInstance
	= {
		lik_index_in_di_library_index_to_library_name			:: !Int
	};
	
ENDECODE_LIBRARY_INSTANCE_KIND_SIZE	:== 1;
ENDECODE_LIK_LIBRARY_REDIRECTION	:== 0;
ENDECODE_LIK_LAZY_LIBRARY_INSTANCE	:== 1;
ENDECODE_LIK_LIBRARY_INSTANCE		:== 2;
ENDECODE_LIK_EMPTY					:== 3;

instance EnDecode LibraryInstanceKind
where {
	to_size (LIK_LibraryInstance lik_library_instance)
		= ENDECODE_LIBRARY_INSTANCE_KIND_SIZE + to_size lik_library_instance;
	to_size LIK_Empty
		= ENDECODE_LIBRARY_INSTANCE_KIND_SIZE;
	
	to_string x=:(LIK_LibraryInstance i) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_LIK_LIBRARY_INSTANCE };
		# (offset,buffer)
			= to_string i (offset + ENDECODE_LIBRARY_INSTANCE_KIND_SIZE) buffer;
		= (offset,buffer);		
	    
	to_string x=:(LIK_Empty) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_LIK_EMPTY };
		= (offset + ENDECODE_LIBRARY_INSTANCE_KIND_SIZE,buffer);
	
	from_string offset buffer	
		# disk_type_reference_id
			= toInt (buffer.[offset]);
		| disk_type_reference_id == ENDECODE_LIK_LIBRARY_INSTANCE
			#! (disk_type_ref,offset)
				= from_string (offset + ENDECODE_LIBRARY_INSTANCE_KIND_SIZE) buffer;
			#! x = LIK_LibraryInstance disk_type_ref
			= (x, offset);
		| disk_type_reference_id == ENDECODE_LIK_EMPTY
			#! x = LIK_Empty
			= (x, offset + ENDECODE_LIBRARY_INSTANCE_KIND_SIZE);
};

read_rts_info_from_dynamic :: DynamicHeader !*f -> (!Bool,!DynamicInfo,!*f) | BinaryDynamicIO f;
read_rts_info_from_dynamic {dynamic_rts_info_i,dynamic_rts_info_s} file
	// set file pointer to start of string table
	#! (ok,file)
		=  bd_seek file dynamic_rts_info_i FSeekSet;
	| not ok
		= (False,default_elem,file);
	#! (dynamic_rts_info,file)
		= bd_reads file dynamic_rts_info_s;
	| size dynamic_rts_info <> dynamic_rts_info_s
		= (False,default_elem,file);
		= (True,decode dynamic_rts_info,file);

f :: !DynamicInfo -> DynamicInfo;
f t = t;
	
class DynamicInfoOps s
where {
	UpdateDynamicInfo :: !Int !DynamicInfo !*s -> *s
};

instance DynamicInfoOps (DynamicInfoArray)
where {
	UpdateDynamicInfo dynamic_info_index dynamic_info a
		# (n_dynamic_infos,a) = usize a;
		| dynamic_info_index < n_dynamic_infos
			= { a & [dynamic_info_index] = dynamic_info };
		# new_dynamic_infos = createArray (inc dynamic_info_index) default_dynamic_info;
		# new_dynamic_infos = { new_dynamic_infos & [dynamic_info_i] = a.[dynamic_info_i] \\ dynamic_info_i <- [0..dec n_dynamic_infos] }
		= { new_dynamic_infos & [dynamic_info_index] = dynamic_info };
};

instance EnDecode DynamicInfo
where {
	to_size {di_library_instance_to_library_index,di_library_index_to_library_name,di_disk_type_equivalent_classes,di_lazy_dynamics_a,di_type_redirection_table}
		= to_size di_library_instance_to_library_index + to_size  di_library_index_to_library_name + to_size di_disk_type_equivalent_classes + to_size di_lazy_dynamics_a + to_size di_type_redirection_table;

	to_string {di_library_instance_to_library_index,di_library_index_to_library_name,di_disk_type_equivalent_classes,di_lazy_dynamics_a,di_type_redirection_table} offset buffer
		# (offset,buffer)
			= to_string di_library_instance_to_library_index offset buffer;
		# (offset,buffer)
			= to_string di_library_index_to_library_name offset buffer;
		# (offset,buffer)
			= to_string di_disk_type_equivalent_classes offset buffer;
		# (offset,buffer) = to_string di_lazy_dynamics_a offset buffer;
		# (offset,buffer)
			= to_string di_type_redirection_table offset buffer;
		= (offset,buffer);

	from_string offset buffer
		#! (di_library_instance_to_library_index,offset)
			= from_string offset buffer;
		#! (di_library_index_to_library_name,offset)
			= from_string offset buffer;
		#! (di_disk_type_equivalent_classes,offset)
			= from_string offset buffer;
		#! (di_lazy_dynamics_a,offset) = from_string offset buffer;
		#! (di_type_redirection_table,offset)
			= from_string offset buffer;
			
		#! di
			= { default_elem &
				di_library_instance_to_library_index	= di_library_instance_to_library_index
			,	di_library_index_to_library_name		= di_library_index_to_library_name
			,	di_disk_type_equivalent_classes			= di_disk_type_equivalent_classes
			,	di_lazy_dynamics_a						= di_lazy_dynamics_a
			,	di_type_redirection_table				= di_type_redirection_table
			};
		= (di,offset);	
};

instance DefaultElem DynamicInfo
where {
	default_elem
		= default_dynamic_info;
};
		
default_dynamic_info :: DynamicInfo;
default_dynamic_info 
	= {
		di_version								= DefaultVersion
	,	di_string_table							= {}
	,	di_descriptor_usage_table				= default_descriptor_usage_table
	,	di_file_name							= {}
	,	di_n_blocks								= 0
	
	// part being stored in a dynamic ...
	,	di_library_instance_to_library_index	= {}
	,	di_library_index_to_library_name		= {}
	,	di_disk_type_equivalent_classes			= default_elem
	,	di_lazy_dynamics_a						= {}
	,	di_type_redirection_table				= {}
	// ... part being stored in a dynamic 
	
	,	di_disk_id_to_library_instance_i		= {}		// indexed by diskID
	,	di_disk_to_rt_dynamic_indices			= {}
	
	,	di_has_block_been_used					= {}
	,	di_rt_type_redirection_table			= {}
	};

resolve_overloading2 :: {#a} -> {#a} | EnDecode, DefaultElem a & Array {#} a;
resolve_overloading2 i = i;

// RunTimeID/DiskID  at run-time:
RTID_DATA_DYNAMIC				:== 0; // no code needed

// RunTimeID at run-time
RTID_LIBRARY_INSTANCE_ID_START	:== 1; // by default, a dummy element is created

// Otherwise
RTID_DISKID_RENUMBER_START		:== 1; // change also gts_range_id

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
		ldr_id							:: !Int			// run-time id of lazy dynamic
	,	ldr_lazy_dynamic_index			:: !Int			// disk id for lazy dynamic (block)
	};

// all instance of the graph_to_string-conversion function *must* use the same LazyDynamicReference.
LazyDynamicReference_String		:== "LazyDynamicReference";

instance DefaultElem LazyDynamicReference
where {
	default_elem 
		= { 
			ldr_id					= default_elem
		,	ldr_lazy_dynamic_index	= default_elem
		}
}

instance EnDecode LazyDynamicReference
where {
	to_size {ldr_id,ldr_lazy_dynamic_index}
		= to_size ldr_id + to_size ldr_lazy_dynamic_index;
		
	to_string {ldr_id,ldr_lazy_dynamic_index} offset buffer
		# (offset,buffer) = to_string ldr_id offset buffer;
		# (offset,buffer) = to_string ldr_lazy_dynamic_index offset buffer;
		= (offset,buffer);
			
	from_string offset buffer
		# (ldr_id,offset) = from_string offset buffer;
		# (ldr_lazy_dynamic_index,offset)= from_string offset buffer;
		# lazy_dynamic_reference = { ldr_id = ldr_id,ldr_lazy_dynamic_index = ldr_lazy_dynamic_index }
		= (lazy_dynamic_reference,offset);
}

StdDynamicLowLevelInterfaceModule_String	:== "StdDynamicLowLevelInterface";

GlobalDynamicInfoDummy_String 	:== "GlobalDynamicInfoDummy";

RunTimeIDW_String :== "RunTimeIDW";

DynamicLinkerInterfaceModule_String	:== "DynamicLinkerInterface";

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
	
instance EnDecode LazyTypeReference
where {
	to_size {ltr_lazy_dynamic_index,ltr_library_instance_i}
		= to_size ltr_lazy_dynamic_index + to_size ltr_library_instance_i;

	to_string {ltr_lazy_dynamic_index,ltr_library_instance_i} offset buffer
		# (offset,buffer)
			= to_string ltr_lazy_dynamic_index offset buffer;
		# (offset,buffer)
			= to_string ltr_library_instance_i offset buffer;
		= (offset,buffer);

	from_string offset buffer
		#! (ltr_lazy_dynamic_index,offset)
			= from_string offset buffer;
		#! (ltr_library_instance_i,offset)
			= from_string offset buffer;
			
		#! di
			= { default_elem &
				ltr_lazy_dynamic_index		= ltr_lazy_dynamic_index
			,	ltr_library_instance_i		= ltr_library_instance_i
			};
		= (di,offset);	
};

instance DefaultElem (Maybe m)
where {
	default_elem
		= Nothing;
};

instance DefaultElem LazyTypeReference
where {
	default_elem
		= {
			ltr_lazy_dynamic_index	= 0
		,	ltr_library_instance_i	= 0
		};			

};

// Lazy library instances
// A *lazy* library instance is part of a lazy dynamic and is referenced from its main
// dynamic.
// RunTimeID bit_field(bits 31-0)
// - bit31 == 0, then bits 0-30 hold the library instance number within the main dynamic
// - bit31 == 1,
//               0-15	lazy dynamic index				(16 bits)
//				 16-30	lazy library instance index		(15 bits)
LLI_IS_MAIN_LIBRARY_INSTANCE x	:== (x bitand 0x80000000) == 0;
LLI_IS_LAZY_LIBRARY_INSTANCE x	:== not (LLI_IS_MAIN_LIBRARY_INSTANCE x);

LLI_EXTRACT_MAIN_LIBRARY_INSTANCE_INDEX x :== lli_extract_main_library_instance_index x
where {
	lli_extract_main_library_instance_index x
		| LLI_IS_MAIN_LIBRARY_INSTANCE x 
			= x;
			= abort "LLI_EXTRACT_MAIN_LIBRARY_INSTANCE_INDEX (macro): not a library instance of main dynamic";
};

// RunTimeID bit_field(bits 31-0)
// - bit31 == 0, then bits 0-30 hold the library instance number within the main dynamic
// - bit31  == 1,
//		0 	- 10		ith type redirection
//		11	- 20		lazy dynamic index
//		21	- 30		type table index	
	
class encode_lib_ref a :: !a -> Int;

instance encode_lib_ref LibRef
where {
	encode_lib_ref :: !LibRef -> Int;
	encode_lib_ref (LibRef i)
		= i;
};	

instance encode_lib_ref LibraryInstanceTypeReference
where {
	encode_lib_ref (LIT_TypeReference lib_ref _)
		= encode_lib_ref lib_ref;
};
		
decode_lib_ref :: !Int -> LibRef;
decode_lib_ref i
	| LLI_IS_MAIN_LIBRARY_INSTANCE i
		= LibRef i;
		
		= abort "decode_lib_ref; internal error";

instance EnDecode LibraryInstanceTypeReference
where {
	to_size (LIT_TypeReference lib_ref tio_type_ref)
		= to_size lib_ref + to_size tio_type_ref;

	to_string (LIT_TypeReference lib_ref tio_type_ref) offset buffer
		# (offset,buffer)
			= to_string lib_ref offset buffer;
		# (offset,buffer)
			= to_string tio_type_ref offset buffer;
		= (offset,buffer);

	from_string offset buffer
		#! (lib_ref,offset)
			= from_string offset buffer;
		#! (tio_type_ref,offset)
			= from_string offset buffer;
			
		#! litr
			= LIT_TypeReference lib_ref tio_type_ref;
		= (litr,offset);	
};

instance EnDecode TIO_TypeReference
where {
	to_size {tio_type_without_definition,tio_tr_module_n,tio_tr_type_def_n}
		= to_size tio_type_without_definition + to_size tio_tr_module_n + to_size tio_tr_type_def_n;

	to_string {tio_type_without_definition,tio_tr_module_n,tio_tr_type_def_n} offset buffer
		# (offset,buffer)
			= to_string tio_type_without_definition offset buffer;
		# (offset,buffer)
			= to_string tio_tr_module_n offset buffer;
		# (offset,buffer)
			= to_string tio_tr_type_def_n offset buffer;
		= (offset,buffer);

	from_string offset buffer
		#! (tio_type_without_definition,offset)
			= from_string offset buffer;
		#! (tio_tr_module_n,offset)
			= from_string offset buffer;
		#! (tio_tr_type_def_n,offset)
			= from_string offset buffer;
			
		#! tio_type_reference
			= {
				tio_type_without_definition  = tio_type_without_definition
			,   tio_tr_module_n    			 = tio_tr_module_n
			,   tio_tr_type_def_n  			 = tio_tr_type_def_n
			}
		= (tio_type_reference,offset);	
};

instance DefaultElem [a] 
where {
	default_elem 
		= [];
};

instance DefaultElem {a} | EnDecode, DefaultElem a & Array {} a
where {
	default_elem 
		= {};
};

ENDECODE_LIBREF_SIZE			:== 1;
ENDECODE_LIBREF		 			:== 0;

instance EnDecode LibRef
where {
	to_size (LibRef i)
		= ENDECODE_LIBREF_SIZE + to_size i;
	
	to_string x=:(LibRef i) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_LIBREF };
		# (offset,buffer)
			= to_string i (offset + ENDECODE_LIBREF_SIZE) buffer;
		= (offset,buffer);		

	from_string offset buffer	
		# disk_type_reference_id
			= toInt (buffer.[offset]);
		| disk_type_reference_id == ENDECODE_LIBREF
			#! (x,offset)
				= from_string (offset + ENDECODE_LIBREF_SIZE) buffer;
			#! x = LibRef x
			= (x, offset);
};

instance EnDecode LIK_LibraryInstance
where {
	to_size {LIK_LibraryInstance | lik_index_in_di_library_index_to_library_name}
		= to_size lik_index_in_di_library_index_to_library_name;

	to_string {LIK_LibraryInstance | lik_index_in_di_library_index_to_library_name} offset buffer
		# (offset,buffer)
			= to_string lik_index_in_di_library_index_to_library_name offset buffer;
		= (offset,buffer);

	from_string offset buffer
		#! (lik_index_in_di_library_index_to_library_name,offset)
			= from_string offset buffer;
						
		#! lr
			= { LIK_LibraryInstance | default_elem &
				lik_index_in_di_library_index_to_library_name = lik_index_in_di_library_index_to_library_name
			};
		= (lr,offset);	
};

instance DefaultElem LIK_LibraryInstance
where {
	default_elem
		= { LIK_LibraryInstance |
			lik_index_in_di_library_index_to_library_name = default_elem
		};
};

create_dynamic_file_name :: !String -> String;
create_dynamic_file_name file_name
	# user_dynamic_extension
		= "." +++ EXTENSION_USER_DYNAMIC;
	| ends file_name user_dynamic_extension
		= file_name;
		= (file_name +++ user_dynamic_extension);
	
// see gts_range_id.c
INITIAL_TYPE_REFERENCE_NUMBER	:== 0;

:: EncodedTypeReference
	= {
		etr_type_module_name	:: !String
	,	etr_library_instance_i	:: !Int 		// lazy or not
	};
	
instance DefaultElem EncodedTypeReference
where {
	default_elem	
		= {
			etr_type_module_name	= default_elem
		,	etr_library_instance_i	= default_elem
		};
};

hex_int2 :: !Int -> String;
hex_int2 d = hex_int d;