implementation module WriteOptionsFile

import StdArray,StdChar,StdFile, StdInt
import PmTypes

ApplicationOptionsToFlags :: !ApplicationOptions -> Int
ApplicationOptionsToFlags {sgc,pss,marking_collection,set,o,memoryProfiling,write_stderr_to_file}
	= flags
where
	flags					
		= showgc+printstacksize+showexectime+cons+marking_collection_mask+memory_profiling_mask+write_stderr_to_file_mask
	showgc					
		| sgc	= 2
				= 0
	printstacksize			
		| pss	= 4
				= 0
	showexectime 
		| set	= 8 
				= 0
	write_stderr_to_file_mask
		| write_stderr_to_file	= 128 
								= 0
	marking_collection_mask 
		| marking_collection 	= 64  
								= 0	
	memory_profiling_mask
		| memoryProfiling	= 32  
							= 0
	cons
		= case o of
			BasicValuesOnly		-> 1
			ShowConstructors	-> 0
			NoReturnType		-> 16
			NoConsole			-> 16

FlagsToApplicationOptions :: !Int !ApplicationOptions -> ApplicationOptions;
FlagsToApplicationOptions flags applicationOptions
	=	{applicationOptions & sgc=showgc,pss=printstacksize,marking_collection=marking_collection,set=showexectime,
				o=output, memoryProfiling=memoryProfiling, write_stderr_to_file=write_stderr_to_file};
where
		showgc
			= (flags bitand 2) <> 0;
		printstacksize
			= (flags bitand 4) <> 0;
		showexectime
			= (flags bitand 8) <> 0;
		memoryProfiling
			= (flags bitand 32) <> 0;
		marking_collection
			= (flags bitand 64) <> 0;
		write_stderr_to_file
			= (flags bitand 128) <> 0;
		output
			| (flags bitand 16) <> 0
				= NoConsole;
			|  (flags bitand 1) <> 0
				= BasicValuesOnly;
			// otherwise
				= ShowConstructors;

write_options_file :: !{#.Char} !.Int !.Int !.Int !.Int !.Int !.Int !Bool !*a -> *(!Bool,!*a) | FileSystem a;

write_options_file options_file_name flags heap_size stack_size initial_heap_size heap_size_multiple min_write_heap_size use_64_bit_processor files
	| use_64_bit_processor
		= write_options_file_64 options_file_name flags heap_size stack_size initial_heap_size heap_size_multiple min_write_heap_size files
		= write_options_file_32 options_file_name flags heap_size stack_size initial_heap_size heap_size_multiple min_write_heap_size files

write_options_file_32 :: !{#.Char} !.Int !.Int !.Int !.Int !.Int !.Int !*a -> *(!Bool,!*a) | FileSystem a;

write_options_file_32 options_file_name flags heap_size stack_size initial_heap_size heap_size_multiple min_write_heap_size files
	# (opened,file,files) 
		= fopen options_file_name FWriteData files
	| not opened
		= (False,files)
	#! file = file
	
	// header offset 0
		FWW machine_type	
		FWW n_sections	
		FWI time_date_stamp   
		FWI symbol_table_pointer
		FWI n_symbols
		FWW optional_header_size
		FWW characteristics
		
	// text section header offset 20
		FWS ".text\0\0\0"
		FWI text_virtual_size
		FWI text_rva_offset
		FWI text_raw_data_size
		FWI text_raw_data_pointer
		FWI text_relocs_pointer
		FWI text_linenumbers_pointer
		FWW text_n_relocs
		FWW text_n_linenumbers
		FWI text_section_flags
		
	// data section header offset 60
		FWS ".data\0\0\0"
		FWI data_virtual_size
		FWI data_rva_offset
		FWI data_raw_data_size
		FWI data_raw_data_pointer
		FWI data_relocs_pointer
		FWI data_linenumbers_pointer
		FWW data_n_relocs
		FWW data_n_linenumbers
		FWI data_section_flags
		
	// bss section header offset 100
		FWS ".bss\0\0\0\0"
		FWI bss_virtual_size
		FWI bss_rva_offset
		FWI bss_raw_data_size
		FWI bss_raw_data_pointer
		FWI bss_relocs_pointer
		FWI bss_linenumbers_pointer
		FWW bss_n_relocs
		FWW bss_n_linenumbers
		FWI bss_section_flags
		
	// data section offset 140
		FWI heap_size  FWI  stack_size FWI flags FWI initial_heap_size FWI heap_size_multiple FWI min_write_heap_size
		
	// symbol table offset
		// .file at 164
		FWS file_name
		FWI file_value
		FWW file_section_n
		FWW file_type
		FWB file_storage_class
		FWB file_n_aux_sections
		
		// fake (aux to .file) at 182 
		FWS file_aux_name
		FWI	file_aux_value
		FWW file_aux_section_n
		FWW file_aux_type
		FWB file_aux_storage_class
		FWB file_aux_n_aux_sections
		
		// .text at 200
		FWS text_name
		FWI text_value
		FWW text_section_n
		FWW text_type
		FWB	text_storage_class
		FWB	text_n_aux_sections
		
		// null to .text at 214
		FWI text_raw_data_size FWS null_aux_entry
		
		// .data at 236
		FWS data_name
		FWI data_value
		FWW data_section_n
		FWW data_type
		FWB data_storage_class
		FWB data_n_aux_sections
		
		// null to .data at 254
		FWI data_raw_data_size FWS null_aux_entry
		
		// .bss at 272
		FWS bss_name
		FWI bss_value
		FWW bss_section_n
		FWW bss_type
		FWB bss_storage_class
		FWB bss_n_aux_sections
		
		// null to .bss at 290
		FWI bss_raw_data_size FWS null_aux_entry
		
		// _heap_size at 306
		FWI 0 FWI heap_size_offset
		FWI heap_size_value
		FWW heap_size_section_n		
		FWW heap_size_type
		FWB heap_size_class
		FWB heap_size_n_aux_sections
	
		
		// _ab_stack_size at 326
		FWI 0 FWI ab_stack_size_offset
		FWI ab_stack_size_value
		FWW ab_stack_size_section_n
		FWW ab_stack_size_type
		FWB ab_stack_size_class
		FWB ab_stack_size_n_aux_sections
	
				
		// _flags at 344
		FWS flags_name
		FWI flags_value
		FWW flags_section_n
		FWW flags_type
		FWB flags_class
		FWB flags_n_aux_sections
		
		// _initial_heap_size at 362
		FWI	0 FWI initial_heap_size_offset
		FWI initial_heap_size_value
		FWW initial_heap_size_section_n
		FWW initial_heap_size_type
		FWB initial_heap_size_class
		FWB initial_heap_size_n_aux_sections
		
		// _heap_size_multiple at 378
		FWI 0 FWI heap_size_multiple_offset
		FWI heap_size_multiple_value
		FWW heap_size_multiple_section_n
		FWW heap_size_multiple_type
		FWB heap_size_multiple_class
		FWB heap_size_multiple_n_aux_sections
		
		// _min_write_heap_size at 396
		FWI	0 FWI min_write_heap_size_offset
		FWI min_write_heap_size_value
		FWW min_write_heap_size_section_n
		FWW min_write_heap_size_type
		FWB min_write_heap_size_class
		FWB min_write_heap_size_n_aux_sections
		
	// string table at 414
		FWI (size string_table + 4)
		FWS string_table
	# (close_ok,files) 
		= fclose file files
	= (close_ok,files)
where
	// coff header
	n_sections					= 3
	machine_type				= 0x14c
	time_date_stamp				= 817729185
	symbol_table_pointer		= 164 //160
	n_symbols					= 14  //13
	optional_header_size		= 0
	characteristics				= 0x0104
		
	// .text section
	text_virtual_size			= 0
	text_rva_offset				= 0
	text_raw_data_size			= 0
	text_raw_data_pointer		= 0
	text_relocs_pointer			= 0
	text_linenumbers_pointer	= 0
	text_n_relocs				= 0
	text_n_linenumbers			= 0
	text_section_flags			= 0x60000020
		
	// .data section
	data_virtual_size			= 0
	data_rva_offset				= 0 
	data_raw_data_size			= 24 //20
	data_raw_data_pointer		= 140
	data_relocs_pointer			= 0
	data_linenumbers_pointer	= 0
	data_n_relocs				= 0
	data_n_linenumbers			= 0
	data_section_flags			= 0xc0000040	
		
	//  .bss section
	bss_virtual_size			= 0
	bss_rva_offset				= 20
	bss_raw_data_size			= 0
	bss_raw_data_pointer		= 0
	bss_relocs_pointer			= 0
	bss_linenumbers_pointer		= 0
	bss_n_relocs				= 0
	bss_n_linenumbers			= 0
	bss_section_flags			= 0xc0000080
	
	//	symbol table
	// entry #1
	file_name					= ".file\0\0\0"
	file_value					= 0
	file_section_n				= IMAGE_SYM_DEBUG
	file_type					= 0
	file_storage_class			= IMAGE_SYM_CLASS_FILE
	file_n_aux_sections			= 1
	
	// entry #2
	file_aux_name				= "fake\0\0\0\0"
	file_aux_value				= 0
	file_aux_section_n			= IMAGE_SYM_UNDEFINED
	file_aux_type				= 0
	file_aux_storage_class		= IMAGE_SYM_CLASS_NULL
	file_aux_n_aux_sections		= 0
	
	// entry #3: .text
	text_name				= ".text\0\0\0"
	text_value				= 0
	text_section_n			= 1
	text_type				= 0
	text_storage_class		= IMAGE_SYM_CLASS_STATIC
	text_n_aux_sections		= 1
	
	// entry #5: .data
	data_name				= ".data\0\0\0"
	data_value				= 0
	data_section_n			= 2
	data_type				= 0
	data_storage_class		= IMAGE_SYM_CLASS_STATIC
	data_n_aux_sections		= 1
	
	// entry #7: .bss
	bss_name				= ".bss\0\0\0\0"
	bss_value				= 20
	bss_section_n			= 3
	bss_type				= 0
	bss_storage_class		= IMAGE_SYM_CLASS_STATIC
	bss_n_aux_sections		= 1
	
	// entry #9: _heap_size
	heap_size_value 		= 0								// offset 0 in data section
	heap_size_section_n		= 2
	heap_size_type			= 0
	heap_size_class			= IMAGE_SYM_CLASS_EXTERNAL
	heap_size_n_aux_sections = 0
	
	// entry #10: _ab_stack_size
	ab_stack_size_value		= 4								// offset 4 in data section
	ab_stack_size_section_n	= 2
	ab_stack_size_type		= 0
	ab_stack_size_class		= IMAGE_SYM_CLASS_EXTERNAL
	ab_stack_size_n_aux_sections = 0
	
	// entry #11: _flags
	flags_name				= "_flags\0\0"
	flags_value				= 8								// offset 8 in data section
	flags_section_n		 	= 2	
	flags_type				= 0
	flags_class				= IMAGE_SYM_CLASS_EXTERNAL
	flags_n_aux_sections 	= 0
	
	// entry #12: _initial_heap_size
	initial_heap_size_value	= 12							// offset 12 in data section
	initial_heap_size_section_n	= 2
	initial_heap_size_type	= 0
	initial_heap_size_class	= IMAGE_SYM_CLASS_EXTERNAL
	initial_heap_size_n_aux_sections = 0
	
	// entry #13: _heap_size_multiple
	heap_size_multiple_value = 16							// offset 16 in data section
	heap_size_multiple_section_n = 2
	heap_size_multiple_type	= 0
	heap_size_multiple_class = IMAGE_SYM_CLASS_EXTERNAL
	heap_size_multiple_n_aux_sections = 0
	
	// entry #14: _min_write_heap_size
	min_write_heap_size_value	= 20						// offset 20 in data section
	min_write_heap_size_section_n = 2
	min_write_heap_size_type	= 0
	min_write_heap_size_class	= IMAGE_SYM_CLASS_EXTERNAL
	min_write_heap_size_n_aux_sections = 0
	
	null_aux_entry				= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
	
	string_table
		//      10         20         30		40		   50		 60         70        80 
		// 45678901234 567890123456789 0123456789012345678 90123456789012345678 901234567890123456789 
		= "_heap_size\0_ab_stack_size\0_initial_heap_size\0_heap_size_multiple\0_min_write_heap_size\0"
	
	heap_size_offset			= 4;
	ab_stack_size_offset		= 15;
	initial_heap_size_offset	= 30;
	heap_size_multiple_offset	= 49;
	min_write_heap_size_offset	= 69;
		
write_options_file_64 :: !{#.Char} !.Int !.Int !.Int !.Int !.Int !.Int !*a -> *(!Bool,!*a) | FileSystem a;

write_options_file_64 options_file_name flags heap_size stack_size initial_heap_size heap_size_multiple min_write_heap_size files
	# (opened,file,files) 
		= fopen options_file_name FWriteData files
	| not opened
		= (False,files)
	#! file = file
	
	// header offset 0
		FWW machine_type_64
		FWW n_sections	
		FWI time_date_stamp   
		FWI symbol_table_pointer
		FWI n_symbols
		FWW optional_header_size
		FWW characteristics
		
	// text section header offset 20
		FWS ".text\0\0\0"
		FWI text_virtual_size
		FWI text_rva_offset
		FWI text_raw_data_size
		FWI text_raw_data_pointer
		FWI text_relocs_pointer
		FWI text_linenumbers_pointer
		FWW text_n_relocs
		FWW text_n_linenumbers
		FWI text_section_flags
		
	// data section header offset 60
		FWS ".data\0\0\0"
		FWI data_virtual_size
		FWI data_rva_offset
		FWI data_raw_data_size
		FWI data_raw_data_pointer
		FWI data_relocs_pointer
		FWI data_linenumbers_pointer
		FWW data_n_relocs
		FWW data_n_linenumbers
		FWI data_section_flags
		
	// bss section header offset 100
		FWS ".bss\0\0\0\0"
		FWI bss_virtual_size
		FWI bss_rva_offset
		FWI bss_raw_data_size
		FWI bss_raw_data_pointer
		FWI bss_relocs_pointer
		FWI bss_linenumbers_pointer
		FWW bss_n_relocs
		FWW bss_n_linenumbers
		FWI bss_section_flags
		
	// data section offset 140
		FWI heap_size
		FWI	0
		FWI stack_size
		FWI	0
		FWI flags 
		FWI	0
		FWI initial_heap_size 
		FWI	0
		FWI heap_size_multiple 
		FWI	0
		FWI min_write_heap_size
		FWI	0

	// symbol table offset
		// .file at 188
		FWS file_name
		FWI file_value
		FWW file_section_n
		FWW file_type
		FWB file_storage_class
		FWB file_n_aux_sections
		
		// fake (aux to .file) at 206
		FWS file_aux_name
		FWI	file_aux_value
		FWW file_aux_section_n
		FWW file_aux_type
		FWB file_aux_storage_class
		FWB file_aux_n_aux_sections
		
		// .text at 224
		FWS text_name
		FWI text_value
		FWW text_section_n
		FWW text_type
		FWB	text_storage_class
		FWB	text_n_aux_sections
		
		// null to .text at 238
		FWI text_raw_data_size FWS null_aux_entry
		
		// .data at 260
		FWS data_name
		FWI data_value
		FWW data_section_n
		FWW data_type
		FWB data_storage_class
		FWB data_n_aux_sections
		
		// null to .data at 278
		FWI data_raw_data_size FWS null_aux_entry
		
		// .bss at 296
		FWS bss_name
		FWI bss_value
		FWW bss_section_n
		FWW bss_type
		FWB bss_storage_class
		FWB bss_n_aux_sections
		
		// null to .bss at 3140
		FWI bss_raw_data_size FWS null_aux_entry
		
		// _heap_size at 330
		FWI 0 FWI heap_size_offset
		FWI heap_size_value
		FWW heap_size_section_n		
		FWW heap_size_type
		FWB heap_size_class
		FWB heap_size_n_aux_sections
	
		// _ab_stack_size at 360
		FWI 0 FWI ab_stack_size_offset
		FWI ab_stack_size_value
		FWW ab_stack_size_section_n
		FWW ab_stack_size_type
		FWB ab_stack_size_class
		FWB ab_stack_size_n_aux_sections
	
		// _flags at 368
		FWS flags_name
		FWI flags_value
		FWW flags_section_n
		FWW flags_type
		FWB flags_class
		FWB flags_n_aux_sections
		
		// _initial_heap_size at 386
		FWI	0 FWI initial_heap_size_offset
		FWI initial_heap_size_value
		FWW initial_heap_size_section_n
		FWW initial_heap_size_type
		FWB initial_heap_size_class
		FWB initial_heap_size_n_aux_sections
		
		// _heap_size_multiple at 302
		FWI 0 FWI heap_size_multiple_offset
		FWI heap_size_multiple_value
		FWW heap_size_multiple_section_n
		FWW heap_size_multiple_type
		FWB heap_size_multiple_class
		FWB heap_size_multiple_n_aux_sections
		
		// _min_write_heap_size at 420
		FWI	0 FWI min_write_heap_size_offset
		FWI min_write_heap_size_value
		FWW min_write_heap_size_section_n
		FWW min_write_heap_size_type
		FWB min_write_heap_size_class
		FWB min_write_heap_size_n_aux_sections
		
	// string table at 438
		FWI (size string_table + 4)
		FWS string_table
	# (close_ok,files) 
		= fclose file files
	= (close_ok,files)
where
	// coff header
	n_sections					= 3
	machine_type_64				= 0x8664
	time_date_stamp				= 817729185
	symbol_table_pointer		= 188
	n_symbols					= 14
	optional_header_size		= 0
	characteristics				= 0x0104

	// .text section
	text_virtual_size			= 0
	text_rva_offset				= 0
	text_raw_data_size			= 0
	text_raw_data_pointer		= 0
	text_relocs_pointer			= 0
	text_linenumbers_pointer	= 0
	text_n_relocs				= 0
	text_n_linenumbers			= 0
	text_section_flags			= 0x60000020
		
	// .data section
	data_virtual_size			= 0
	data_rva_offset				= 0 
	data_raw_data_size			= 48
	data_raw_data_pointer		= 140
	data_relocs_pointer			= 0
	data_linenumbers_pointer	= 0
	data_n_relocs				= 0
	data_n_linenumbers			= 0
	data_section_flags			= 0xc0000040	
		
	//  .bss section
	bss_virtual_size			= 0
	bss_rva_offset				= 20
	bss_raw_data_size			= 0
	bss_raw_data_pointer		= 0
	bss_relocs_pointer			= 0
	bss_linenumbers_pointer		= 0
	bss_n_relocs				= 0
	bss_n_linenumbers			= 0
	bss_section_flags			= 0xc0000080
	
	//	symbol table
	// entry #1
	file_name					= ".file\0\0\0"
	file_value					= 0
	file_section_n				= IMAGE_SYM_DEBUG
	file_type					= 0
	file_storage_class			= IMAGE_SYM_CLASS_FILE
	file_n_aux_sections			= 1
	
	// entry #2
	file_aux_name				= "fake\0\0\0\0"
	file_aux_value				= 0
	file_aux_section_n			= IMAGE_SYM_UNDEFINED
	file_aux_type				= 0
	file_aux_storage_class		= IMAGE_SYM_CLASS_NULL
	file_aux_n_aux_sections		= 0
	
	// entry #3: .text
	text_name				= ".text\0\0\0"
	text_value				= 0
	text_section_n			= 1
	text_type				= 0
	text_storage_class		= IMAGE_SYM_CLASS_STATIC
	text_n_aux_sections		= 1
	
	// entry #5: .data
	data_name				= ".data\0\0\0"
	data_value				= 0
	data_section_n			= 2
	data_type				= 0
	data_storage_class		= IMAGE_SYM_CLASS_STATIC
	data_n_aux_sections		= 1
	
	// entry #7: .bss
	bss_name				= ".bss\0\0\0\0"
	bss_value				= 20
	bss_section_n			= 3
	bss_type				= 0
	bss_storage_class		= IMAGE_SYM_CLASS_STATIC
	bss_n_aux_sections		= 1
	
	// entry #9: _heap_size
	heap_size_value 		= 0
	heap_size_section_n		= 2
	heap_size_type			= 0
	heap_size_class			= IMAGE_SYM_CLASS_EXTERNAL
	heap_size_n_aux_sections = 0
	
	// entry #10: _ab_stack_size
	ab_stack_size_value		= 8
	ab_stack_size_section_n	= 2
	ab_stack_size_type		= 0
	ab_stack_size_class		= IMAGE_SYM_CLASS_EXTERNAL
	ab_stack_size_n_aux_sections = 0
	
	// entry #11: _flags
	flags_name				= "flags\0\0\0"
	flags_value				= 16
	flags_section_n		 	= 2	
	flags_type				= 0
	flags_class				= IMAGE_SYM_CLASS_EXTERNAL
	flags_n_aux_sections 	= 0

	// entry #12: _initial_heap_size
	initial_heap_size_value	= 24
	initial_heap_size_section_n	= 2
	initial_heap_size_type	= 0
	initial_heap_size_class	= IMAGE_SYM_CLASS_EXTERNAL
	initial_heap_size_n_aux_sections = 0
	
	// entry #13: _heap_size_multiple
	heap_size_multiple_value = 32
	heap_size_multiple_section_n = 2
	heap_size_multiple_type	= 0
	heap_size_multiple_class = IMAGE_SYM_CLASS_EXTERNAL
	heap_size_multiple_n_aux_sections = 0
	
	// entry #14: _min_write_heap_size
	min_write_heap_size_value	= 40
	min_write_heap_size_section_n = 2
	min_write_heap_size_type	= 0
	min_write_heap_size_class	= IMAGE_SYM_CLASS_EXTERNAL
	min_write_heap_size_n_aux_sections = 0
	
	null_aux_entry				= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
	
	string_table
		//      10         20         30		40		   50		 60         70        80 
		// 45678901234 567890123456789 0123456789012345678 90123456789012345678 901234567890123456789 
		= "heap_size\0\0ab_stack_size\0\0initial_heap_size\0\0heap_size_multiple\0\0min_write_heap_size\0\0"
	
	heap_size_offset			= 4;
	ab_stack_size_offset		= 15;
	initial_heap_size_offset	= 30;
	heap_size_multiple_offset	= 49;
	min_write_heap_size_offset	= 69;

IMAGE_SYM_DEBUG 	:== 65534
IMAGE_SYM_UNDEFINED	:== 0

// Storage class
IMAGE_SYM_CLASS_NULL		:== 0
IMAGE_SYM_CLASS_EXTERNAL	:== 2;
IMAGE_SYM_CLASS_STATIC		:== 3
IMAGE_SYM_CLASS_FILE 		:== 103 

(FWI) infixl
(FWI) f i :== fwritei i f

(FWS) infixl
(FWS) f s :== fwrites s f

(FWB) infixl
(FWB) f c :== fwritec (toChar c) f

(FWW) infixl
(FWW) f w :== fwritec (toChar (w >> 8)) (fwritec (toChar w) f)
