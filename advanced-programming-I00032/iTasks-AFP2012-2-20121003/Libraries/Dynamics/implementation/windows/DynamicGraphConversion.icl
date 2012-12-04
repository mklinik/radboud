implementation module DynamicGraphConversion

import StdEnv
import StdDynamicTypes
import StdDynamicLowLevelInterface
import DynamicLinkerInterface
import DynamicUtilities
import DefaultElem
import StdMaybe
import BitSet
import shared_buffer

// --------------------------------------------------------------------------------------------------------------------------			
init_dynamic :: !String !DynamicHeader !*a -> *(.Bool,.GlobalDynamicInfoDummy,*a) | BinaryDynamicIO a
init_dynamic file_name dynamic_header=:{block_table_i,graph_i} file
	// a block table
	#! (ok,block_table,file)
		= read_block_table_from_dynamic dynamic_header file
	#! n_blocks = size block_table
	| not ok
		= (False,undef,file)
		
	// read graph blocks
	#! (ok,graph_blocks,file)
		= read_graph_blocks 0 n_blocks (createArray n_blocks "hallo") block_table dynamic_header file
	| not ok
		= (False,undef,file)

	// create global dynamic info
	#! gdi
		= { GlobalDynamicInfo |
			gdi_file_name					= file_name
		,	first_time						= True	
		,	block_table						= block_table
		,	id								= 0
		,	graph_blocks					= graph_blocks
		,	graph_pointers					= { {} \\ i <- [1..n_blocks] }
		,	diskid_to_runtimeid 			= {}
		,	di_disk_to_rt_dynamic_indices	= {}
		, 	di_dummy						= {}
		,	di_type_redirection_table		= {}
		,	di_shared_blocks				= {}
		}
	#! gdid
		= {gdid=gdi};
	
	#! (ok,gdid,file)
		= analyze_blocks gdid file;
	#! ok = True
	= (ok,gdid,file)
where 
	analyze_blocks gdid=:{gdid={block_table,graph_blocks}} file
		// read descriptor bitsize
		#! (ok,di_descriptor_bitset,file)
			= read_descriptor_bitset_from_dynamic dynamic_header file
		| not ok
			= (False,gdid,file)
			
		// read descriptor usage table
		#! (ok,di_descriptor_usage_table,file)
			= read_descriptor_usage_table_from_dynamic dynamic_header file
		| not ok
			= (False,gdid,file)
		
		// lookup DynamicTemp
		#! descriptor_usage_entry_index_of_DynamicTemp
			= find 0 (size di_descriptor_usage_table) 
			with
				find :: !Int !Int -> Int
				find i limit
					| i == limit
						= abort "No DynamicTemp found";
					#! element
						= di_descriptor_usage_table.[i];					
					| not (fst (isBitSetMember element.bitset 0))
						= find (inc i) limit
						= i;
	
		#! n_blocks
			= size block_table;
		#! di_blocks_to_built
			= { {} \\ _ <- [1..n_blocks] } 
		#! di_blocks_to_built
			= loop 0 n_blocks di_blocks_to_built
			with
				loop :: !Int !Int !{#*{#Int}} -> *{#*{#Int}};
				loop block_i n_blocks di_blocks_to_built
					| block_i == n_blocks
						= di_blocks_to_built;
						
						#! shared_blocks
							= { i \\ i <- analyze_block block_i block_table graph_blocks di_descriptor_bitset di_descriptor_usage_table descriptor_usage_entry_index_of_DynamicTemp };
						= loop (inc block_i) n_blocks {di_blocks_to_built & [block_i] = shared_blocks};
			
		#! gdid = { gdid & gdid.di_shared_blocks = di_blocks_to_built };
		= (True,gdid,file);
		
	read_graph_blocks :: !Int !Int !*{String} !BlockTable !DynamicHeader !*f -> (!Bool,!{String},!*f) | BinaryDynamicIO f
	read_graph_blocks block_i n_blocks graph_blocks block_table dynamic_header=:{graph_i} file
		| block_i == n_blocks
			= (True,graph_blocks,file)

			#! block = block_table.[block_i]
	
			// read graph block
			#! (ok,file)
				= bd_seek file (block.bk_offset + graph_i) FSeekSet
			#! (graph_block,file)
				= bd_reads file block.bk_size
			| not ok || (size graph_block <> block.bk_size)
				= abort "read_graph_block: dynamic is corrupt"
			= read_graph_blocks (inc block_i) n_blocks {graph_blocks & [block_i] = graph_block} block_table dynamic_header file

// --------------------------------------------------------------------------------------------------------------------------			
:: *EncodedDynamic2
	= { 
		ed_encoded_graph	:: !*{#Char}
	,	ed_dynamic_rts_info	:: !*{#Char}
	}
	
class EncodedDynamic a
where 
	dynamic_to_string :: !Dynamic -> (!Bool,!*a)

instance EncodedDynamic EncodedDynamic2
where
	dynamic_to_string d
		# (copy_graph_to_string,{ggtsf_o_n_library_instances=n_library_instances,ggtsf_o_range_table=range_table})
			= GetGraphToStringFunction
			
		# type_table_usage
			= (NF (createArray n_library_instances TTUT_UNUSED))	// indexed by RunTimeID or indirectly by converting a ModuleID to a RunTimeID

		#! cgtsa
			= { cgtsa_dynamic					= d
			,	cgtsa_code_library_instances	= {} //createArray n_library_instances TTUT_UNUSED
			,	cgtsa_type_library_instances	= createArray n_library_instances TTUT_UNUSED
			,	cgtsa_range_table				= range_table
			}
		
		#! copy_graph_to_string_argument = /* NF */ {wrap_info = cgtsa}
		#! ({wrap_info = {cgtsr_encoded_dynamic,cgtsr_type_library_instances,cgtsr_lazy_dynamic_references,cgtsr_runtime_ids}})
			= copy_graph_to_string copy_graph_to_string_argument
		
		#! gdri
			= {	gdri_i_type_library_instances		= cgtsr_type_library_instances
			,	gdri_i_lazy_dynamics_references		= cgtsr_lazy_dynamic_references
			,	gdri_i_runtime_ids					= cgtsr_runtime_ids
			}
		#! dynamic_rts_info
			= GetDynamicRTSInfo	gdri
	/*
		// test ...
		#! file
			= open_binary_dynamic_io_string cgtsr_encoded_dynamic
			
		#! (ok,dynamic_header,file)
			= read_dynamic_header file;
		| not ok
			= abort "error reading header";
			
		#! (ok,descriptor_bitset,file)
			= read_descriptor_bitset_from_dynamic dynamic_header file;
		| True <<- descriptor_bitset
			= abort (toString descriptor_bitset.n_elements);

		#! cgtsr_encoded_dynamic
			= close_binary_dynamic_io_string file;
		// ... test
	*/
		# encoded_dynamic1
			= {	ed_encoded_graph	= cgtsr_encoded_dynamic
			,	ed_dynamic_rts_info	= dynamic_rts_info
			}
		= (True,encoded_dynamic1)
	where 
		encode_type_table_usage :: !*{#Int} -> *{#Char}
		encode_type_table_usage type_table_usage
			#! encoded_type_table_usage
				= createArray (s_type_table_usage << 2) ' '
			= encode_type_table_usage 0 0 encoded_type_table_usage
		where
			encode_type_table_usage i offset encoded_type_table_usage
				| i == s_type_table_usage
					= encoded_type_table_usage
					
					# encoded_type_table_usage
						= WriteLong encoded_type_table_usage offset type_table_usage.[i]
					= encode_type_table_usage (inc i) (offset + 4) encoded_type_table_usage
		
			s_type_table_usage
				= size type_table_usage
		
instance EncodedDynamic String
where
	dynamic_to_string d
		# (ok,{ed_encoded_graph,ed_dynamic_rts_info})
			= dynamic_to_string d
			
		// size of arrays
		# (s_ed_encoded_graph,ed_encoded_graph)
			= usize ed_encoded_graph
		# (s_ed_dynamic_rts_info,ed_dynamic_rts_info)
			= usize ed_dynamic_rts_info
		# s_encoded_dynamic
			= s_ed_encoded_graph + s_ed_dynamic_rts_info
			
		// copy
		# (j,encoded_dynamic)
			= copy 0 s_ed_encoded_graph ed_encoded_graph 0 (createArray s_encoded_dynamic ' ')
			
		# (_,encoded_dynamic)
			= copy 0 s_ed_dynamic_rts_info ed_dynamic_rts_info j encoded_dynamic
			
		// patch encoded dynamic
		# encoded_dynamic
			= WriteLong encoded_dynamic (DYNAMIC_RTS_INFO_OFFSET - HEADER_SIZE_OFFSET) s_ed_encoded_graph
		# encoded_dynamic
			= WriteLong encoded_dynamic (DYNAMIC_RTS_INFO_SIZE - HEADER_SIZE_OFFSET) s_ed_dynamic_rts_info
		= (ok,encoded_dynamic)
	where 
		copy :: !Int !Int !{#Char} !Int !*{#Char} -> (!Int,!*{#Char})
		copy i limit src j dest
			| i == limit
				= (j,dest)
			= copy (inc i) limit src (inc j) {dest & [j] = src.[i]}

string_to_dynamic :: !String -> (!Bool,!Dynamic)
string_to_dynamic dynamic_as_string
	#! (odtl=:{odtl_o_ok,odtl_o_file,odtl_o_dynamic_rts_string})
		= OpenDynamicToLinker dynamic_as_string
	| not odtl_o_ok
		= abort "string_to_dynamic: error communicating with linker"
	# (ok,dynamic_header,file)
		= read_dynamic_header odtl_o_file
	| not ok
		= abort "readDynamic"
	#! (ok,gdid,file)
		= init_dynamic "string_to_dynamic" dynamic_header file
	| not ok
		= abort "DynamicGraphConversion; internal error"
	#! dyn
		= build_block (NF make_start_node_index) (NF { gdid & gdid.di_dummy = odtl_o_dynamic_rts_string})
	| CloseDynamicToLinker {odtl & odtl_o_file = file}
	= (ok,dyn)
	
// --------------------------------------------------------------------------------------------------------------------------			
:: Wrap a 
	= { 
		wrap_info		:: !a
	}

// aanpassen van gesharde type door alle library instanties		
:: *CopyGraphToStringArguments
	= {
		cgtsa_dynamic					:: Dynamic
	,	cgtsa_code_library_instances	:: !*{#Int}						// unused
	,	cgtsa_type_library_instances	:: !*{#Int}
	,	cgtsa_range_table				:: !{#Char}
	}
	
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
		
// --------------------------------------------------------------------------------------------------------------------------				
/*
** the dynamic to be decoded is identified by (encoded_graph_i,graph)
**
**
** ugid				= returned by the dynamic rts on readDynamic's behalf.
** ulid				= local id of dynamic (within ugid; 0 for a top-level dynamic)
** encoded_graph_i	= index of dynamic to be decoded (0 for a top-level dynamic)
** graph			= string encoding of the *complete* dynamic to be decoded
*/

// change also string_to_graph.c; gts_gdi.c
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
	
// to prevent the unboxing of GlobalDynamicInfo
:: GlobalDynamicInfoDummy = {
		gdid			:: !GlobalDynamicInfo
	}

is_block_i_already_present block_i gdid=:{gdid={graph_pointers}}
	= size graph_pointers.[block_i] <> 0

:: Pointer = Pointer

GetBlockAddresses node_index gdid=:{gdid={id,first_time,gdi_file_name,di_dummy=dynamic_rts_string}}
	# block_i = get_block_i node_index
	#! gba_in
		= { default_elem &
			gba_i_filename				= gdi_file_name
		,	gba_i_first_time			= first_time
		,	gba_i_id					= id
		,	gba_i_block_i				= block_i
		,	gba_i_dynamic_rts_string	= dynamic_rts_string
		}
	#! (copy_string_to_graph,{gba_o_diskid_to_runtimeid,gba_o_disk_to_rt_dynamic_indices,gba_o_id,gba_o_addresses,gba_o_rt_type_redirection_table})
		= GetBlockAddresses2 gba_in;
	#! gdid
		= case first_time of
			True
				-> { gdid & gdid = { gdid.gdid &
									first_time						= False
								,	id								= gba_o_id
								,	diskid_to_runtimeid 			= gba_o_diskid_to_runtimeid
								,	di_disk_to_rt_dynamic_indices	= gba_o_disk_to_rt_dynamic_indices
								,	di_type_redirection_table		= gba_o_rt_type_redirection_table
								}
					}
			False
				-> gdid
	= (copy_string_to_graph,gba_o_addresses,gdid)
	
analyze_block :: !Int !BlockTable !{String} !BitSet !DescriptorUsageTable !Int -> [Int]
analyze_block block_i block_table graph_blocks di_descriptor_bitset di_descriptor_usage_table di_dynamic_temp_dus_index
	#! maybe_virtual_offset
		= compute_virtual_offset_of_DynamicTemp block_i
		with
			compute_virtual_offset_of_DynamicTemp block_i
				| fst (isBitSetMember di_descriptor_usage_table.[di_dynamic_temp_dus_index].bitset block_i)
					#! virtual_offset
						= compute_virtual_offset 0 di_dynamic_temp_dus_index virtual_offset_start
					= Just virtual_offset;
					= Nothing
			where 
				compute_virtual_offset i index offset
					| i == size di_descriptor_usage_table
						= abort ("compute_virtual_offset; internal error; index " +++ toString index +++ " not found")
					| i == index 
						= offset;
						
					#! new_offset
						= if (fst (isBitSetMember di_descriptor_usage_table.[i].bitset block_i)) (offset + 4) offset;
					= compute_virtual_offset (inc i) index new_offset;
	#! shared_blocks
		= analyze_block 0 (size graph_blocks.[block_i]) block_i maybe_virtual_offset [];
		with 
			analyze_block offset limit block_i maybe_virtual_offset shared_blocks
				| offset == limit
					= shared_blocks;
					
					#! (is_descriptor,_)
						= isBitSetMember di_descriptor_bitset ((block_table.[block_i].bk_offset + offset) >> 2);
					| not is_descriptor 
						= analyze_block (offset + 4) limit block_i maybe_virtual_offset shared_blocks;
						
						#! word = FromStringToInt graph_blocks.[block_i] offset;
						| isDynamicTemp word maybe_virtual_offset
							= analyze_block (offset + 12) limit block_i maybe_virtual_offset shared_blocks;
							
						#! referenced_block = get_block_i word
						#! shared_blocks
							= if (is_external_reference word && not (isMember referenced_block shared_blocks)) [referenced_block:shared_blocks] shared_blocks;
						= analyze_block (offset + 4) limit block_i maybe_virtual_offset shared_blocks;
			where
				isDynamicTemp word Nothing
					= False;
				isDynamicTemp word (Just dynamic_temp_virtual_offset)
					= get_encoded_descriptor word == dynamic_temp_virtual_offset
	= shared_blocks

build_block :: !Int !GlobalDynamicInfoDummy -> a //Pointer
build_block node_index gdid
	# (graph,_) = build_block_without_evaluating_graph node_index gdid;
	= graph;

build_block_indirection :: a -> a;
build_block_indirection a = a;

build_block_without_evaluating_graph :: !Int !GlobalDynamicInfoDummy -> (a,!Int) /* algemeen: een stuk graaf */
build_block_without_evaluating_graph node_index gdid=:{gdid={id,first_time,block_table,graph_blocks,graph_pointers,di_shared_blocks}}
	| is_external_entry_node node_index 
		= abort ("build_block: internal error" +++ toString node_index)
	| not first_time && is_block_i_already_present block_i gdid
		// block has already been constructed
		// There are multiple references from some decoded i.e. built block to some unbuilt 
		// i.e. undecoded block. If one of these references is evaluated, then the undecoded
		// block will be constructed.		
		= extract_already_built_graph_with_result_in_tuple block_i en_node_i gdid

	#! state
		= build_blocks di_shared_blocks.[block_i] {};
		with
			build_blocks :: {#Int} !*{#Char} -> *{#Char};
			build_blocks a s
				= build_blocks_loop 0 (size a) s
			where
				build_blocks_loop :: !Int !Int !*{#Char} -> *{#Char};
				build_blocks_loop i limit s
					| i == limit
						= s
					#! block_i = a.[i];
					// first build its children
					#! s = build_blocks di_shared_blocks.[block_i] s;
					// build the parent block
					#! s = build_a_block_for_me (create_node_index block_i) gdid s
						with
							build_a_block_for_me node_index gdid s
								| build_a_block node_index gdid == 0 // force building the block
									= s;
									= s;
					= build_blocks_loop (inc i) limit s

	#! (s_state,state) = usize state;	
	| s_state <> s_state // Force evaluation, False <<- (s_state)
		= undef;	
		
	#! (copy_string_to_graph,s_adr,gdid)
		= GetBlockAddresses node_index gdid;

	// fetch graph (semantics problem: not referentially transparent if dynamics are to be overwritten)
	// In the near future blocks may be read lazily from disk. This creates other problems because the
	// dynamic cannot be overwritten if there are still references to its encoded graph. Also if copied
	// dynamics are supported, then it is the same problem.
	#! graph_block = graph_blocks.[block_i]
	
	// build graph
	// - for the time being it assumed that each block only has one entry
	//   node
	// - should create a unique array containg pointers to already built blocks. 
	// en-nodes

	// copy_string_to_graph
	// input:
	// - entry node needed
	// - offsets to be update din the the graph_pointers table
	
	// output:
	// - graph belong to entry node
	// (- destructively updated graph_pointers table)
	# bk_entries
		= if (size (block_table.[block_i].bk_entries) == 0) 
			{block_table.[block_i].bk_offset - block_table.[block_i].bk_offset}	// and block_table.[block_i].bk_n_node_entries == 0
			(to_help_the_type_checker { en_offset - block_table.[block_i].bk_offset \\ en_offset <-: block_table.[block_i].bk_entries })		

	= copy_string_to_graph
			(s_adr % (8,size s_adr)) 			// %edx
			0									// %ebx offset in graph_block 
			graph_block							// %ecx graph
			gdid								// -4(%esi) unboxed GlobalDynamicInfo
			bk_entries 							// -8(%esi)
			block_i 							// %eax
			en_node_i							// (%esp)
where
	block_i = get_block_i node_index
	en_node_i = get_en_node_i node_index
	block = block_table.[block_i]
		
	// Ideas:
	// - If there are no references more to a particular block and not all blocks have been built, then there is
	//   a space leak because the graph_pointers-array still contains pointers to at least one entry node.
	// - Version information per block instead per dynamic. In case of a copied dynamic i.e. a reference to a
	//   piece of graph in another dynamic, another version of the string_to_graph-routine might have been used.
	//   This version of the conversion routine should then be called.
	// - Reading blocks lazily. The dynamic run-time system must guarantee that the dynamic from which the blocks
	//   are lazily read remains available until there are no references to that dynamic or all blocks have been
	//   read from the dynamic.
	// - If during building a block some external node i.e. in an another block is referenced more than once, then
	//   the build_block-closure is built multiple times. This could be optimized.
	// - Can internal entry nodes occur in absence of existential types?
	//
	// The function below with its local function should *not* call the garbage collector because the pointer to
	// an already existing piece of graph can change because of gc.
extract_already_built_graph :: !Int !Int !GlobalDynamicInfoDummy -> a //Pointer
extract_already_built_graph block_i en_node_i {gdid={graph_pointers}}
	#! (p,graph_pointers) = graph_pointers![block_i,en_node_i]
	# (g,_) = cast_to_dynamic p
	= g

extract_already_built_graph_with_result_in_tuple :: !Int !Int !GlobalDynamicInfoDummy -> (a /*Pointer*/,!Int)
extract_already_built_graph_with_result_in_tuple block_i en_node_i {gdid={graph_pointers}}
	#! (p,graph_pointers) = graph_pointers![block_i,en_node_i]
	= cast_to_dynamic p

cast_to_dynamic :: Int -> (a /*Pointer*/,!Int)
cast_to_dynamic _
	= code {
			pushI	0
	}

build_a_block :: !Int !GlobalDynamicInfoDummy -> Int
build_a_block node_index gdid=:{gdid={id,first_time,block_table,graph_blocks,graph_pointers,di_shared_blocks}}
	| is_external_entry_node node_index 
		= abort ("build_block: internal error" +++ toString node_index)
	| not first_time && is_block_i_already_present block_i gdid
		// block has already been constructed
		// There are multiple references from some decoded i.e. built block to some unbuilt 
		// i.e. undecoded block. If one of these references is evaluated, then the undecoded
		// block will be constructed.		
		= extract_already_built_graph block_i en_node_i gdid

	#! (copy_string_to_graph,s_adr,gdid)
		= GetBlockAddresses node_index gdid;

	// fetch graph (semantics problem: not referentially transparent if dynamics are to be overwritten)
	// In the near future blocks may be read lazily from disk. This creates other problems because the
	// dynamic cannot be overwritten if there are still references to its encoded graph. Also if copied
	// dynamics are supported, then it is the same problem.
	#! graph_block = graph_blocks.[block_i]
	
	// build graph
	// - for the time being it assumed that each block only has one entry
	//   node
	// - should create a unique array containg pointers to already built blocks. 
	// en-nodes

	// copy_string_to_graph
	// input:
	// - entry node needed
	// - offsets to be update din the the graph_pointers table
	
	// output:
	// - graph belong to entry node
	// (- destructively updated graph_pointers table)
	# bk_entries
		= if (size (block_table.[block_i].bk_entries) == 0) 
			{block_table.[block_i].bk_offset - block_table.[block_i].bk_offset}	// and block_table.[block_i].bk_n_node_entries == 0
			(to_help_the_type_checker { en_offset - block_table.[block_i].bk_offset \\ en_offset <-: block_table.[block_i].bk_entries })		

	# (graph2,i)
		= copy_string_to_graph 
			(s_adr % (8,size s_adr)) 			// %edx
			0									// %ebx offset in graph_block 
			graph_block							// %ecx graph
			gdid								// -4(%esi) boxed GlobalDynamicInfo
			bk_entries 							// -8(%esi)
			block_i 							// %eax
			en_node_i							// (%esp)

		// netter zou zijn om de graph_pointers uniek te maken
	| i == 0	// force block construction
		= i;
		= i
where
	block_i = get_block_i node_index
	en_node_i = get_en_node_i node_index
	block = block_table.[block_i]

//build_lazy_block :: !Int !Int !{#.{Int}} -> a
//build_lazy_block node_index lazy_dynamic_index graph_pointers 
build_lazy_block :: !Int !Int -> a
build_lazy_block node_index lazy_dynamic_index
	| IS_SHARING_ACROSS_CONVERSIONS
		= abort "sharing across conversions not supported";

	#! node_index = node_index;
	| is_internal_reference node_index
		= abort "build_lazy_block; internal error; internal reference"

	#! rld_o = RegisterLazyDynamic lazy_dynamic_index;
	#! (rld_o_file,{rld_o_filename,rld_o_diskid_to_runtimeid,rld_o_disk_to_rt_dynamic_indices,rld_o_id,rld_o_rt_type_redirection_table})
		= extract_rld_o_file rld_o;
	
	# (ok,dynamic_header,rld_o_file)
		= read_dynamic_header rld_o_file

	# (ok,gdid,rld_o_file)
		= init_dynamic rld_o_filename dynamic_header rld_o_file
	| not ok
		= abort "init_dynamic: init_dynamic failed"

	#! gdid = { gdid & gdid	= { gdid.gdid &	first_time						= False
										,	id								= rld_o_id
										,	diskid_to_runtimeid 			= rld_o_diskid_to_runtimeid
										,	di_disk_to_rt_dynamic_indices	= rld_o_disk_to_rt_dynamic_indices
										,	di_type_redirection_table		= rld_o_rt_type_redirection_table
										}
		}
	= build_block (NF node_index) (NF gdid)
where
	extract_rld_o_file rld_o=:{rld_o_file}
		= (rld_o_file,{rld_o & rld_o_file = (0,default_elemU)})

copy_graph_to_string_OK :: !(Wrap CopyGraphToStringArguments) -> (Wrap CopyGraphToStringResults)
copy_graph_to_string_OK _ 
	= {wrap_info={cgtsr_encoded_dynamic={},cgtsr_code_library_instances={},cgtsr_type_library_instances={},cgtsr_lazy_dynamic_references={},cgtsr_runtime_ids={}}}

to_help_the_type_checker i :== to_help_the_type_checker2 i
where
	to_help_the_type_checker2 :: !{#Int} -> {#Int}
	to_help_the_type_checker2 i
		= i	