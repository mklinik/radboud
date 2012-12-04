definition module _Posix

from _Pointer import :: Pointer
from Time import :: Tm

WNOHANG		:==	0x00000001  
WUNTRACED	:== 0x00000002
MAXPATHLEN	:== 1024

DIRENT_D_NAME_OFFSET	:== 8

S_IFMT		:== 0170000
S_IFIFO		:== 0010000
S_IFCHR		:== 0020000
S_IFDIR		:== 0040000
S_IFBLK		:== 0060000
S_IFREG		:== 0100000
S_IFLNK		:== 0120000
S_IFSOCK	:== 0140000
S_IFWHT		:== 0160000

//Posix API calls
errno		:: !*World -> (!Int,!*World)
strerr		:: !Int -> Pointer
stat		:: !{#Char} !{#Char} !*World -> (!Int,!*World)
unlink		:: !{#Char} !*World -> (!Int,!*World)
fork		:: !*World -> (!Int,!*World)
execvp		:: !{#Char} !{#Pointer} !*World -> (!Int,!*World)
waitpid		:: !Int !{#Int} !Int !*World -> (!Int,!*World)
exit		:: !Int !*World -> (!.a,!*World) 
getcwd		:: !{#Char} !Int !*World -> (!Pointer,!*World)
chdir		:: !{#Char} !*World -> (!Int,!*World)
mkdir		:: !{#Char} !Int !*World -> (!Int,!*World)
rmdir		:: !{#Char} !*World -> (!Int,!*World)
rename		:: !{#Char} !{#Char} !*World -> (!Int,!*World)
opendir		:: !{#Char} !*World -> (!Pointer,!*World)
closedir	:: !Pointer !*World -> (!Int,!*World)
readdir		:: !Pointer !*World -> (!Pointer,!*World)

//Memory (impure)
malloc	:: !Int -> Pointer
free	:: !Pointer -> Int
memcpy_string_to_pointer :: !Pointer !{#Char} !Int -> Pointer

//Posix datastructures
:: Stat =
	{ st_dev			:: !Int
	, st_ino			:: !Int
	, st_mode			:: !Int
	, st_nlink			:: !Int
	, st_uid			:: !Int
	, st_gid			:: !Int
	, st_rdev			:: !Int
	, st_size			:: !Int
	, st_blocks			:: !Int
	, st_blksize		:: !Int
	, st_ctimespec		:: !Int
	, st_mtimespec		:: !Int
	, st_atimespec		:: !Int
	}
//Mapping to/from byte arrays
unpackStat	:: !{#Char} -> Stat
sizeOfStat	:: Int
