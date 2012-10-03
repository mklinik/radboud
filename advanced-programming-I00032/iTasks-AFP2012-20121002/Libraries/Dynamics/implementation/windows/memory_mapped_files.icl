implementation module memory_mapped_files

import StdEnv

import code from library "StdDynamic_kernel32_library"
//import code from library "winbase_library"
import code from "low.obj"

//-- Windows interface

:: HINSTANCE			:== Int
:: PSTR					:== Int
:: HWND					:== Int
:: UINT					:== Int
:: WPARAM				:== Int
:: LPARAM				:== Int
:: LRESULT				:== Int
:: DWORD				:== Int
:: LPDWORD				:== Int
:: POINT				:== (Int,Int)
:: MSG					:== Int
:: BOOL					:== Int
:: LPCSTR				:== String
:: LPCTSTR				:== String
:: HMODULE				:== Int
:: HDC					:== Int
:: LPPAINTSTRUCT		:== Int
:: LPRECT				:== Int
:: HMENU				:== Int
:: LPVOID				:== Int
:: LPCVOID				:== Int
:: LPWNDCLASSEX			:== Int
:: ATOM					:== Int
:: HANDLE 				:== Int
:: HFILE				:== HANDLE
:: PLHANDLE				:== Int	// ptr
:: SECURITY_ATTRIBUTES	:== Int

PAGE_NOACCESS		:== 0x01     
PAGE_READONLY		:== 0x02     
PAGE_READWRITE		:== 0x04     
PAGE_WRITECOPY		:== 0x08

GENERIC_READ	:== 0x80000000
GENERIC_WRITE	:== 0x40000000
GENERIC_EXECUTE :==	0x20000000
GENERIC_ALL     :== 0x10000000

FILE_SHARE_READ		:==	0x00000001  
FILE_SHARE_WRITE	:== 0x00000002  
FILE_SHARE_DELETE	:== 0x00000004  

CREATE_NEW          :==	1
CREATE_ALWAYS       :==	2
OPEN_EXISTING       :==	3
OPEN_ALWAYS         :==	4
TRUNCATE_EXISTING   :== 5

SECTION_QUERY       :==	0x0001
SECTION_MAP_WRITE   :== 0x0002
SECTION_MAP_READ    :== 0x0004
SECTION_MAP_EXECUTE :== 0x0008
SECTION_EXTEND_SIZE :== 0x0010

FILE_MAP_COPY       :== SECTION_QUERY
FILE_MAP_WRITE      :== SECTION_MAP_WRITE
FILE_MAP_READ       :== SECTION_MAP_READ

FILE_ATTRIBUTE_NORMAL	:== 0x00000080  

INVALID_HANDLE_VALUE	:== -1

DUPLICATE_CLOSE_SOURCE  	:== 0x00000001  
DUPLICATE_SAME_ACCESS       :==	0x00000002 

FALSE			:== 0
TRUE 			:== (<>) FALSE

SYNCHRONIZE					:== 0x00100000

STANDARD_RIGHTS_REQUIRED	:== 0x000F0000

PROCESS_ALL_ACCESS        	:== STANDARD_RIGHTS_REQUIRED bitor SYNCHRONIZE bitor 0xFFF;

:: *ST = ST

initialState :: ST
initialState = ST

CloseST :: !ST -> Bool
CloseST _
	= True

GetLastError :: Int
GetLastError = code {
	ccall GetLastError@0 "P:I"
	}
	
:: Ptr a 
	= NULL
	| Ptr a
	
GetCurrentDirectory :: String
GetCurrentDirectory
	# (s_current_directory,_)
		= GetCurrentDirectory 0 NULL;
	# current_directory
		= createArray s_current_directory ' '
	# (result,current_directory)
		= GetCurrentDirectory 512 (Ptr current_directory)
	| result == 0
		= abort ("GetCurrentDirectory; error calling GetCurrentDirectory");
	# current_directory
		= current_directory % (0,(size current_directory) - 2)
	= current_directory
where 	
	GetCurrentDirectory :: !Int !(Ptr LPCTSTR) -> (!Int,!String)
	GetCurrentDirectory buffer_size (Ptr lpName)
		= (GetCurrentDirectory buffer_size lpName,lpName)
	where
		GetCurrentDirectory :: !Int !LPCTSTR -> DWORD
		GetCurrentDirectory _ _
			= code {
				ccall GetCurrentDirectoryA@8 "PIs:I"
			}
			
	GetCurrentDirectory buffer_size NULL
		= (GetCurrentDirectory buffer_size 0,"")
	where
		GetCurrentDirectory :: !Int !Int -> DWORD
		GetCurrentDirectory _ _
			= code {
				ccall GetCurrentDirectoryA@8 "PII:I"
			}

CreateFileMapping :: !HANDLE !(Ptr SECURITY_ATTRIBUTES) !DWORD !DWORD !DWORD !(Ptr LPCTSTR) !ST -> (!HANDLE,!ST)
CreateFileMapping hFile NULL flProtect dwMaximumSizeHigh dwMaximumSizeLow (Ptr lpName) st
	= (CreateFileMapping hFile 0 flProtect dwMaximumSizeHigh dwMaximumSizeLow lpName,st)
where 
	CreateFileMapping :: !HANDLE !SECURITY_ATTRIBUTES !DWORD !DWORD !DWORD !LPCTSTR -> HANDLE
	CreateFileMapping hFile lpFileMappingAttributes flProtect dwMaximumSizeHigh dwMaximumSizeLow lpName
		= code {
			ccall CreateFileMappingA@24 "PIIIIIs:I"
		}
		
CreateFileMapping hFile NULL flProtect dwMaximumSizeHigh dwMaximumSizeLow NULL st
	= (CreateFileMapping hFile 0 flProtect dwMaximumSizeHigh dwMaximumSizeLow 0,st)
where 
	CreateFileMapping :: !HANDLE !SECURITY_ATTRIBUTES !DWORD !DWORD !DWORD !Int -> HANDLE
	CreateFileMapping hFile lpFileMappingAttributes flProtect dwMaximumSizeHigh dwMaximumSizeLow lpName
		= code {
			ccall CreateFileMappingA@24 "PIIIIII:I"
		}
		
MapViewOfFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD !ST -> (!Int,!ST)
MapViewOfFile hFileMappingObject dwDesiredAccess dwFileOffsetHigh dwFileOffsetLow dwNumberOfBytesToMap st
	= (MapViewOfFile hFileMappingObject dwDesiredAccess dwFileOffsetHigh dwFileOffsetLow dwNumberOfBytesToMap,st)
where
	MapViewOfFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD -> Int
	MapViewOfFile hFileMappingObject dwDesiredAccess dwFileOffsetHigh dwFileOffsetLow dwNumberOfBytesToMap
		= code {
			ccall MapViewOfFile@20 "PIIIII:I"
		}
		
DuplicateHandle :: !HANDLE !HANDLE !HANDLE !(Ptr PLHANDLE) !DWORD !BOOL !DWORD !ST -> (!BOOL,!ST)
DuplicateHandle hSourceProcessHandle hSourceHandle hTargetProcessHandle (Ptr lpTargetHandle) dwDesiredAccess bInheritHandle dwOptions st
	= (DuplicateHandle hSourceProcessHandle hSourceHandle hTargetProcessHandle lpTargetHandle dwDesiredAccess bInheritHandle dwOptions,st)
where
	DuplicateHandle :: !HANDLE !HANDLE !HANDLE !PLHANDLE !DWORD !BOOL !DWORD -> BOOL
	DuplicateHandle hSourceProcessHandle hSourceHandle hTargetProcessHandle lpTargetHandle dwDesiredAccess bInheritHandle dwOptions	
		= code { 
			ccall DuplicateHandle@28 "PIIIIIII:I"
		}
		
GetCurrentProcess :: !ST -> (!HANDLE,!ST)
GetCurrentProcess st
	= (GetCurrentProcess2,st)

GetCurrentProcess2 :: HANDLE
GetCurrentProcess2
	= code {
		ccall GetCurrentProcess@0 "P:I"
	};

CreateFile :: !LPCTSTR !DWORD !DWORD !(Ptr SECURITY_ATTRIBUTES) !DWORD !DWORD !HANDLE !ST -> (!HANDLE,!ST) 
CreateFile lpFileName dwDesiredAccess dwShareMode NULL dwCreationDisposition  dwFlagsAndAttributes hTemplateFile st
	= (CreateFile lpFileName dwDesiredAccess dwShareMode 0 dwCreationDisposition  dwFlagsAndAttributes hTemplateFile, st)
where
	CreateFile :: !LPCTSTR !DWORD !DWORD !Int !DWORD !DWORD !HANDLE -> HANDLE
	CreateFile lpFileName dwDesiredAccess dwShareMode _ dwCreationDisposition  dwFlagsAndAttributes hTemplateFile
		= code {
			ccall CreateFileA@28 "PsIIIIII:I"
		};

UnmapViewOfFile :: !LPCVOID !ST -> (!BOOL,!ST)
UnmapViewOfFile lpcvoid st
	= (UnmapViewOfFile2 lpcvoid, st)
where
	UnmapViewOfFile2 :: !LPCVOID -> BOOL
	UnmapViewOfFile2 lpcvoid
		= code {
			ccall UnmapViewOfFile@8 "PI:I"
		}

CloseHandle :: !HANDLE !ST -> (!BOOL,!ST)
CloseHandle handle st
	= (CloseHandle handle,st)
where
	CloseHandle :: !HANDLE -> BOOL
	CloseHandle handle
		= code {
			ccall CloseHandle@4 "PI:I"
		}
	
GetFileSize :: !HFILE !(Ptr LPDWORD) !ST -> (!DWORD,!ST)
GetFileSize hfile NULL st
	= (GetFileSize hfile 0, st)
where
	GetFileSize :: !HFILE !LPDWORD -> DWORD
	GetFileSize hfile lpFileSizeHigh 
		= code {
			ccall GetFileSize@8 "PI:I"
		}
		
:: *MyString = MyString !*{#Char};

CreateSharedString :: !Int !Int !ST -> (!.{#Char},!ST)
CreateSharedString ptr size st
	# (MyString buffer)
		= CreateMySharedString ptr size
	= (buffer,st)
where
	CreateMySharedString :: !Int !Int -> MyString;
	CreateMySharedString _ _
		= code {
			ccall createsharedstring "II:I"
			fill_r e_memory_mapped_files_kMyString 0 1 0 0 0
			pop_b 1
		}
		
		
OpenProcess :: !DWORD !BOOL !DWORD !ST -> (!HANDLE,!ST)
OpenProcess dwDesiredAccess bInheritHandle dwProcessId st
	= (OpenProcess dwDesiredAccess bInheritHandle dwProcessId,st)
where 
	OpenProcess :: !DWORD !BOOL !DWORD -> HANDLE
	OpenProcess _ _ _
		= code {
			ccall OpenProcess@12 "PIII:I"
		};
