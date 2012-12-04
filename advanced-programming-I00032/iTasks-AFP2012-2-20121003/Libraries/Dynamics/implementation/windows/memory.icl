implementation module memory;

import StdDynamicTypes;
import code from "mem.obj";
import StdEnv;

:: *Mem = Mem;

getMemory :: !*World -> (!*Mem,!*World);
getMemory world
	= (Mem,world);
	
putMemory :: !*Mem !*World -> *World;
putMemory mem world
	= world;

readWord :: !Int !*Mem -> (!Int,!*Mem);
readWord address mem
	= code {
	.d 1 1 i
		jsr read_word
	.o 1 1 i
	};

readHalfWord :: !Int !*Mem -> (!Int,!*Mem);
readHalfWord address mem
	= code {
	.d 1 1 i
		jsr read_half_word
	.o 1 1 i
	};
	
readByte:: !Int !*Mem -> (!Int,!*Mem);
readByte address mem
	= code {
	.d 1 1 i
		jsr read_byte
	.o 1 1 i
	};
	
readWord1 :: !Int !*a -> (!Int,!*a);
readWord1 address mem
	= code {
	.d 1 1 i
		jsr read_word
	.o 1 1 i
	};
	
readHalfWord1 :: !Int !*a -> (!Int,!*a);
readHalfWord1 address mem
	= code {
	.d 1 1 i
		jsr read_half_word
	.o 1 1 i
	};
	
readByte1 :: !Int !*a -> (!Int,!*a);
readByte1 address mem
	= code {
	.d 1 1 i
		jsr read_byte
	.o 1 1 i
	};
	
address_of_buffer :: !*a -> (!Int,!*a);
address_of_buffer mem
	= code {
	.d 1 0
		jsr address_of_buffer
	.o 1 1 i
	};

writeWord1 :: !Int !Int !*a -> *a;
writeWord1 value ptr _
	= code {
	.d 1 2 ii
		jsr write_word
	.o 1 0
	};
	
writeByte1 :: !Char !Int !*a -> *a;
writeByte1 value ptr _
	= code {
	.d 1 2 ci
		jsr write_byte
	.o 1 0
	};

// FIXME: why the DummyModuleID argument?
// (defining module_name,data address)
get_module_id :: !DummyModuleID -> (!String,!Int);
get_module_id module_id
	# address
		= get_module_id1 module_id;
	# (length,mem)
		= readWord address Mem;
	# module_name
		= createArray length ' ';
	# (module_name,mem)
		= collect 0 length (address + 4) module_name mem;
	= (module_name,address);
where {
	get_module_id1 :: !DummyModuleID -> Int;
	get_module_id1 _
		= code {
		.d 1 0
			jsr	get_module_id
		.o 0 1 i
		};
		
	collect i limit address module_name mem
		| i == limit
			= (module_name,mem);
			
			# (byte,mem)
				= readByte address mem;
			= collect (inc i) limit (inc address) {module_name & [i] = toChar byte} mem;
}