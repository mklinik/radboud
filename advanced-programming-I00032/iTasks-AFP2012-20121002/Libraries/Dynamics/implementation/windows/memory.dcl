definition module memory;

from StdDynamicTypes import :: DummyModuleID;

:: *Mem = Mem;

getMemory :: !*World -> (!*Mem,!*World);
putMemory :: !*Mem !*World -> *World;

readByte :: !Int !*Mem -> (!Int,!*Mem);
readWord :: !Int !*Mem -> (!Int,!*Mem);
readHalfWord :: !Int !*Mem -> (!Int,!*Mem);


readWord1 :: !Int !*a -> (!Int,!*a);
readHalfWord1 :: !Int !*a -> (!Int,!*a);
readByte1 :: !Int !*a -> (!Int,!*a);

address_of_buffer :: !*a -> (!Int,!*a);

writeWord1 :: !Int !Int !*a -> *a;
writeByte1 :: !Char !Int !*a -> *a;

get_module_id :: !DummyModuleID -> (!String,!Int);
