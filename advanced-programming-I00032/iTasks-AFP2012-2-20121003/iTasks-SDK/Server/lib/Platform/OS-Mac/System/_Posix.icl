implementation module _Posix

import _Pointer, Time
import StdInt

errno :: !*World -> (!Int,!*World)
errno world = (getErrno,world)
where
	getErrno :: Int
	getErrno = readInt4S errnoAddr 0
	
	errnoAddr :: Pointer
	errnoAddr = code {
		ccall __error ":p"
	}

strerr :: !Int -> Pointer
strerr world = code {
	ccall strerror "I:p"
}

stat :: !{#Char} !{#Char} !*World -> (!Int,!*World)
stat path buf world = code {
	ccall stat$INODE64 "ss:I:A"
}

unlink :: !{#Char} !*World -> (!Int,!*World)
unlink path world = code {
	ccall unlink "s:I:A"
}
fork :: !*World -> (!Int,!*World)
fork world = code {
	ccall fork ":I:A"
}
execvp :: !{#Char} !{#Pointer} !*World -> (!Int,!*World)
execvp name argv world = code {
	ccall execvp "sA:I:A"
}
waitpid :: !Int !{#Int} !Int !*World -> (!Int,!*World)
waitpid pid status_p options world = code {
    ccall waitpid "IAI:I:A"
}
exit :: !Int !*World -> (!.a,!*World)
exit num world = code {
	ccall exit "I:p:A"
}
getcwd :: !{#Char} !Int !*World -> (!Pointer,!*World)
getcwd buf size_t world = code {
	ccall getcwd "sI:p:A"
}
chdir :: !{#Char} !*World -> (!Int,!*World)
chdir name world = code {
	ccall chdir "s:I:A"
}
mkdir :: !{#Char} !Int !*World -> (!Int,!*World)
mkdir name mode world = code {
	ccall mkdir "sI:I:A"
}
rmdir :: !{#Char} !*World -> (!Int,!*World)
rmdir name world = code {
	ccall rmdir "s:I:A"
}
rename :: !{#Char} !{#Char} !*World -> (!Int,!*World)
rename old new world = code {
	ccall rename "ss:I:A"
}
opendir	:: !{#Char} !*World -> (!Pointer,!*World)
opendir path world = code {
	ccall opendir "s:p:A"
}
closedir :: !Pointer !*World -> (!Int,!*World)
closedir dir world = code {
	ccall closedir "p:I:A"
}
readdir	:: !Pointer !*World -> (!Pointer,!*World)
readdir dir world = code {
	ccall readdir "p:p:A"
}

malloc :: !Int -> Pointer
malloc num = code {
	ccall malloc "p:p"
}
free :: !Pointer -> Int 
free ptr = code {
	ccall free "p:I"
}
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer
memcpy_string_to_pointer p s n = code {
    ccall memcpy "psp:p"
}

//Mapping to/from byte arrays
unpackStat :: !{#Char} -> Stat
unpackStat s =
    { st_dev			= unpackInt4S s 0
    , st_ino			= unpackInt8  s 8
    , st_mode			= unpackInt2S s 4 
    , st_nlink			= unpackInt2S s 6 
    , st_uid			= unpackInt4S s 16
    , st_gid			= unpackInt4S s 20
	, st_rdev			= unpackInt8  s 24 
    , st_atimespec  	= unpackInt8  s 32
    , st_mtimespec  	= unpackInt8  s 48
    , st_ctimespec  	= unpackInt8  s 64 
	, st_birthtimespec	= unpackInt8  s 80
    , st_size       	= unpackInt8  s 96 
    , st_blocks    		= unpackInt8  s 104 
    , st_blksize    	= unpackInt4S s 112
    , st_flags      	= unpackInt4S s 116
    , st_gen        	= unpackInt4S s 120
    }

sizeOfStat :: Int
sizeOfStat = 144
