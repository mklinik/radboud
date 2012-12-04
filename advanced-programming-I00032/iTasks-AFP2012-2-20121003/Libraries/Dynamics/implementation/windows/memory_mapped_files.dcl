definition module memory_mapped_files

from StdOverloaded import class == (==)
from StdClass import class Eq, <>
from StdInt import bitor
from StdBool import instance == (Bool), not

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
:: PLHANDLE				:== Int
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
TRUE 			:== (<>) FALSE // RWS ???

SYNCHRONIZE					:== 0x00100000

STANDARD_RIGHTS_REQUIRED	:== 0x000F0000

PROCESS_ALL_ACCESS        	:== STANDARD_RIGHTS_REQUIRED bitor SYNCHRONIZE bitor 0xFFF;

:: *MyString = MyString !*{#Char};

:: *ST

initialState :: ST
CloseST :: !ST -> Bool

GetLastError :: Int
	
:: Ptr a 
	= NULL
	| Ptr a

GetCurrentDirectory :: String

CreateFileMapping :: !HANDLE !(Ptr SECURITY_ATTRIBUTES) !DWORD !DWORD !DWORD !(Ptr LPCTSTR) !ST -> (!HANDLE,!ST)
		

MapViewOfFile :: !HANDLE !DWORD !DWORD !DWORD !DWORD !ST -> (!Int,!ST)

DuplicateHandle :: !HANDLE !HANDLE !HANDLE !(Ptr PLHANDLE) !DWORD !BOOL !DWORD !ST -> (!BOOL,!ST)
GetCurrentProcess :: !ST -> (!HANDLE,!ST)

CreateFile :: !LPCTSTR !DWORD !DWORD !(Ptr SECURITY_ATTRIBUTES) !DWORD !DWORD !HANDLE !ST -> (!HANDLE,!ST) 

UnmapViewOfFile :: !LPCVOID !ST -> (!BOOL,!ST)

CloseHandle :: !HANDLE !ST -> (!BOOL,!ST)

GetFileSize :: !HFILE !(Ptr LPDWORD) !ST -> (!DWORD,!ST)
		
CreateSharedString :: !Int !Int !ST -> (!.{#Char},!ST)

OpenProcess :: !DWORD !BOOL !DWORD !ST -> (!HANDLE,!ST)