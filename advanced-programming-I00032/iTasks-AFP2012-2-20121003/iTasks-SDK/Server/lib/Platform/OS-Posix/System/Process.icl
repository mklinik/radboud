implementation module Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString
import StdMisc

//Data
import Maybe
import Void

//System
import FilePath
import File
import OSError
import _Pointer
import _Posix

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world //TODO: Use mCurrentDirectory argument
	//Check if path exists 
	# (ok,world)	= fileExists path world
	| not ok
		= (Error (1,"File " +++ path +++ " does not exist"),world)
	//Fork
	# (pid, world)			= fork world
	| pid == 0
		//Exec
		# (argv,args_memory)	= makeArgv [path:args]
		# (res,world)			= execvp (path +++ "\0") argv world
		= (exit 1 world)
	| pid > 0
		= (Ok {ProcessHandle| pid = pid}, world)
	| otherwise
		= getLastOSError world
where
	makeArgv :: [String] -> (!{#Pointer},!Pointer)
	makeArgv argv_list
		# args_size = argvLength argv_list 0
		  args_string = createArgsString args_size argv_list
		  args_memory = malloc args_size
		| args_memory == 0
			= abort "malloc failed"
		# args_memory = memcpy_string_to_pointer args_memory args_string args_size
		  argv = createArgv argv_list args_memory
		= (argv,args_memory)
	where
		argvLength [a:as] l
			= argvLength as (l+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4)))
		argvLength [] l
			= l

    	createArgsString args_size argv_list
			# s = createArray args_size '\0'
			= copyArgs argv_list 0 s
		where
			copyArgs [a:as] i s
				# s = copyChars 0 a i s
				= copyArgs as (i+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))) s
			copyArgs [] i s
				= s
    
			copyChars :: !Int !{#Char} !Int !*{#Char} -> *{#Char}
			copyChars ai a si s
				| ai<size a
					# s = {s & [si]=a.[ai]}
					= copyChars (ai+1) a (si+1) s
				= s

		createArgv argv_list args_memory
			# n_args = length argv_list
			# argv = createArray (n_args+1) 0;
			= fillArgv 0 argv_list argv args_memory 
		where
			fillArgv :: !Int ![{#Char}] !*{#Pointer} !Int -> *{#Pointer}
			fillArgv arg_n [a:as] argv args_memory
				# argv = {argv & [arg_n]=args_memory}
				  args_memory = args_memory + ((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))
				= fillArgv (arg_n+1) as argv args_memory
			fillArgv arg_n [] argv args_memory
				= {argv & [arg_n]=0}

checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status WNOHANG world //Non-blocking wait :)
	| ret == 0
		= (Ok Nothing, world)	
	| ret == pid	
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok (Just exitCode), world)
	| otherwise
		= getLastOSError world

waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
waitForProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status 0 world //Blocking wait
	| ret == pid
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok exitCode, world)
	| otherwise
		= getLastOSError world

	
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of 
		Ok handle	= waitForProcess handle world
		Error e		= (Error e, world)
