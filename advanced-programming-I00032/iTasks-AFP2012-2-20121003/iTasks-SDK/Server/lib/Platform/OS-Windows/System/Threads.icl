implementation module Threads

import _WinBase, _Pointer, StdArray, StdInt, StdClass, dynamic_string, _Unsafe, StdMisc
foreign export threadFunc

getCurrentThreadId :: !*World -> (!ThreadId, !*World)
getCurrentThreadId world = WinGetCurrentThreadId world

send :: !ThreadId !a !*World -> *(!Bool, !*World) | TC a
send tid msg world
	# msgStr = dynamic_to_string (dynamic msg)
	# res = mq_send_message tid msgStr
	= (not res, world)
where
	mq_send_message :: !DWORD !String -> Bool
	mq_send_message tid msg = code {
		.d 1 1 i
			jsr mqueue_send_message
		.o 0 1 b	
	}

receive :: !*World -> *(!Int, !Dynamic, !*World)
receive world
	# (sender, paramstr) = mq_recv_message
	= (sender, string_to_dynamic paramstr, world)
where
	mq_recv_message :: (!Int, !*String)
	mq_recv_message = code {
		.d 0 0
			jsr mqueue_receive_message
		.o 1 1 i
	}

fork :: !(*World -> *World) !*World -> (!ThreadId, !*World)
fork threadF world
	# threadFStr			= copy_to_string threadF
	# (heap, world)			= getProcessHeap world
	# s						= size threadFStr
	# (ptr, world)			= heapAlloc heap 0 (INT_SIZE * 4 + s) world
	# ptr					= writeInt ptr 0 thread_func_address
	# ptr					= writeInt ptr INT_SIZE 0
	# ptr					= writeInt ptr (INT_SIZE * 2) 0
	# ptr					= writeInt ptr (INT_SIZE * 3) s
	# ptr					= writeCharArray (ptr + INT_SIZE * 4) threadFStr
	# ptr					= ptr - INT_SIZE * 4
	# (handle, id, world)	= CreateThread 0 0 clean_new_thread_address ptr 0 world
	= (id, world)	

waitForThread :: !ThreadId !*World -> *World
waitForThread tid world
	# (handle, world)	= WinOpenThread SYNCHRONIZE False tid world
	# (r, world)		= waitForSingleObject handle INFINITE world
	| r <> WAIT_OBJECT_0 = abort "waitForThread error"
	= world

threadFunc :: !LPVOID -> DWORD
threadFunc ptr
	# s				= readInt ptr (INT_SIZE * 3)
	# threadFStr	= derefCharArray (ptr + INT_SIZE * 4) s
	# (threadF,_)	= copy_from_string {s` \\ s` <-: threadFStr}
	= appUnsafe (threadFunc` ptr threadF) 0
where
	threadFunc` :: !LPVOID !(*World -> *World) !*World -> *World
	threadFunc` ptr threadF world
		# (heap, world)	= getProcessHeap world
		# (ok, world)	= heapFree heap 0 ptr world
		// check ok
		# world			= threadF world
		= world

clean_new_thread_address :: Int
clean_new_thread_address = code {
	pushLc clean_new_thread
|	pushL clean_new_thread
}

thread_func_address :: Int
thread_func_address = code {
	pushLc threadFunc
|	pushL threadFunc
}
