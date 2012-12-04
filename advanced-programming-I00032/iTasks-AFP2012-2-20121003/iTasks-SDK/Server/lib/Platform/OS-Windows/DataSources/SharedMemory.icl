implementation module SharedMemory

import _WinBase, _Pointer, _Unsafe, StdInt, StdTuple, StdString, StdArray, StdBool, StdFunc, FilePath, SharedDataSource, dynamic_string
import StdMisc

/**
* |--------------------------------------------|
* | size | value ptr | version | observer list |
* |--------------------------------------------|
*         |                     |               |-------------------|
*         |                      -------------> | observer 0 | next |
*         V                                     |-------------------|
*        |----------------------|                             |
*        | dynamic value string |                              --> ...
*        |----------------------|                 
*        <-------- size -------->
*/
sharedMemory :: !a !*envC -> (!Shared a *envS, !*envC) | MemoryEnv envC & MemoryEnv envS
sharedMemory v world
	# (heap, world)	= getProcessHeap world
	# initStr		= copy_to_string v
	# sStr			= size initStr
	# (mutx, world)	= createMutexA NULL False NULL world
	// check ok
	# (iptr, world)	= heapAlloc heap 0 (INT_SIZE * 4) world
	# (vptr, world)	= heapAlloc heap 0 sStr world
	# vptr			= writeCharArray vptr initStr
	# iptr			= writeIntElemOffset iptr 0 sStr	// init size of dynamic string
	# iptr			= writeIntElemOffset iptr 1 vptr	// init pointer to dynamic string
	# iptr			= writeIntElemOffset iptr 2 0		// init version number
	# iptr			= writeIntElemOffset iptr 3 NULL	// init linked list of observers
	= (createBasicDataSource "sharedMemory" (toString iptr) (mkOps heap mutx iptr) id const, world)
where
	get str = fst (copy_from_string {c \\ c <-: str})
	putback v _ = copy_to_string v
	
	mkOps heap mutx ptr env =
		({ read			= read
		, write			= write
		, getVersion	= getVersion
		, lock			= lock
		, lockExcl		= lockExcl
		, unlock		= unlock
		, close			= close
		, addObserver	= addObserver
		}, env)
	where
		read world
			# (sStr,ptr)	= readIntElemOffsetP ptr 0
			# (vptr,ptr)	= readIntElemOffsetP ptr 1
			# (ver,ptr)		= readIntElemOffsetP ptr 2
			# str			= derefCharArray vptr sStr
			# world			= forceEvalPointer ptr world
			# world			= forceEval str world
			= (Ok (fst (copy_from_string {c \\ c <-: str}), ver), world)
			
		write b world
			# dstr			= copy_to_string b
			# (vptr,ptr)	= readIntElemOffsetP ptr 1
			# (ok, world)	= heapFree heap 0 vptr world
			| not ok = (Error "writing to shared memory: error freeing memory", world)
			# sStr			= size dstr
			# (vptr, world)	= heapAlloc heap 0 sStr world
			| vptr == NULL = (Error "writing to shared memory: error allocating memory", world)
			# vptr			= writeCharArray vptr dstr
			# ptr			= writeIntElemOffset ptr 0 sStr
			# ptr			= writeIntElemOffset ptr 1 vptr
			// increase version number
			# ver			= readIntElemOffset ptr 2
			# ptr			= writeIntElemOffset ptr 2 (inc ver)
			// notify observers and empty list
			# (wptr,ptr)	= readIntElemOffsetP ptr 3
			# world			= notifyObservers wptr world
			# ptr			= writeIntElemOffset ptr 3 NULL
			= (Ok Void, forceEvalPointer ptr world)
		where
			notifyObservers :: !Pointer !*env -> *env
			notifyObservers wptr world
				| wptr == NULL = world
				# obs 			= readIntElemOffset wptr 0
				# (_, world)	= setEvent obs world
				# next 			= readIntElemOffset wptr 1
				# world			= notifyObservers next world
				# (ok, world)	= heapFree heap 0 wptr world
				| not ok = abort "notifyWaiters: error freeing heap"
				= world

		getVersion world
			= (Ok (readIntElemOffset ptr 2), world)
		
		addObserver observer world
			# (nptr, world)	= heapAlloc heap 0 (INT_SIZE * 2) world
			# optr			= readIntElemOffset ptr 3
			# nptr			= writeIntElemOffset nptr 0 observer
			# nptr			= writeIntElemOffset nptr 1 optr
			# ptr			= writeIntElemOffset ptr 3 nptr
			= forceEvalPointer ptr world
			
		lock = lock`
		lockExcl = lock`
		lock` world
			# (r, world) = waitForSingleObject mutx INFINITE world
			| r <> WAIT_OBJECT_0 = abort "shared memory: error getting lock"
			= world
		
		unlock world
			# (ok, world) = releaseMutex mutx world
			| not ok = abort "shared memory: error releasing lock"
			= world
			
		close world = world
		
instance MemoryEnv World
where
	accMemory accFunc env
		# (a, mem) = accFunc 0
		= (a,env)