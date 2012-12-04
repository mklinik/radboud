implementation module PmCleanSystem

import StdEnv
import File
import FilePath
import Directory
import OSError
import Void

from Maybe import qualified ::Maybe(..)
from Process import qualified callProcess

import PmCompilerOptions
import PmPath
import PmTypes

import Platform
import StdPathname
//import thread_message
import UtilIO
import UtilStrictLists

import linkargs
import WriteOptionsFile

maketempdir :: Pathname -> Pathname
maketempdir startupdir = startupdir +++ "\\Temp"

:: CompilerProcessHandlesAndId = {
	compiler_thread_id :: !Int,
	compiler_thread_handle :: !Int,
	compiler_process_handle :: !Int
   }

:: CompilerProcessIds :== [CompilerProcessHandlesAndId]

NoCompilerProcessIds :: CompilerProcessIds
NoCompilerProcessIds = []

instance == CompileOrCheckSyntax
where
//	(==) :: !CompileOrCheckSyntax !CompileOrCheckSyntax -> Bool
	(==) SyntaxCheck SyntaxCheck
		=	True
	(==) Compilation Compilation
		=	True
	(==) _ _
		=	False
		
instance FileEnv LogEnv where
	accFiles :: !.(*Files -> (.x,*Files)) !*LogEnv -> (!.x,!*LogEnv)
	accFiles accfun logenv
		# (r, world) = accFiles accfun logenv.world
		= (r, { logenv & world = world })

	appFiles :: !.(*Files -> *Files) !*LogEnv -> *LogEnv
	appFiles appfun logenv
		= { logenv & world = appFiles appfun logenv.world }
		
ExitCleanCompiler :: !(!CompilingInfo,*World) -> (!CompilingInfo,*World)
/*
ExitCleanCompiler prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle), ps)
	# wm_number=get_message_number;
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("exit\0")
	| /*trace_tn ("ExitCleanCompiler "+++toString r+++"\n") &&*/ r==0
		= prog
		= (CompilingInfo NoCompiler,ps);
*/		
ExitCleanCompiler prog
	= prog

instance == CodeGenerateAsmOrCode
where
//	(==) :: !CodeGenerateAsmOrCode !CodeGenerateAsmOrCode -> Bool
	(==) AsmGeneration AsmGeneration
		=	True
	(==) CodeGeneration CodeGeneration
		=	True
	(==) _ _
		=	False
		
instance == CompilerMsg
where
	(==) CompilerOK CompilerOK = True
	(==) SyntaxError SyntaxError = True
	(==) GlobalError GlobalError = True
	(==) _ _ = False
	
mangleCompiler2 ccstring` startupdir
	# (ccstring`,rem)			= splitOptions ccstring`
	# (opts,opts`)				= splitOptions rem
	# (shortOK,ccstring)		= GetShortPathName (startupdir +++ "\\" +++ ccstring` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ ccstring`) +++ "'."
		= (False,line,"","","")
	# cocl = ccstring % (0, size ccstring - 2)
	# cocldir = RemoveFilename cocl
	= (True,cocl,cocldir,opts,opts`)

CompilePersistent ::
	!String !Bool !(WindowFun *LogEnv) !(WindowFun *LogEnv) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Bool !Bool !Bool !CompilerOptions !Pathname !CompilingInfo !*LogEnv
	-> (!CompilingInfo,!(!*LogEnv, !Pathname, !CompilerMsg))
CompilePersistent
	cocl` write_module_times errwin typewin compileOrCheckSyntax path paths projectHeapProfiling
	projectTimeProfiling projectEagerOrDynamic co=:{CompilerOptions | listTypes}
	startupdir cstate env

	# tooltempdir = maketempdir startupdir
	# (cocl_ok,cocl,cocl_dir,cocl_startup,options)	= mangleCompiler2 cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# env					= errwin [cocl] env
		= (cstate,(env,"",GlobalError))

	#	out_file_name		=  out_file_path tooltempdir dummy_slot
		errors_file_name	=  errors_file_path tooltempdir dummy_slot
	# cocl_arguments
		= [options]
		  ++ write_module_times_string
		  ++ CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
			projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	
	# (res, world) = 'Process'.callProcess cocl cocl_arguments ('Maybe'.Just cocl_dir) env.world
	# env = { env & world = world }
	# (compile_ok, exitcode,(cstate,env)) = case res of
		Ok exitcode	= (True, exitcode, (cstate,env))
		Error err	= (False, 0, (cstate,env)) 
			
//	# (compile_ok,exitcode,(cstate,env)) = compile_with_cache cocl cocl_dir cocl_startup cocl_arguments (cstate,env);
	| not compile_ok
  		# env = errwin ["Error: Unable to run compiler: "+++cocl +++ " :"+++toString (snd (fromError res))] env
  		= (cstate,(env,"",GlobalError))
	# (path,mess,env) = CompileHandleExitCode exitcode cocl tooltempdir dummy_slot  errwin typewin path listTypes env
	=	(cstate,(env,path,mess))
where
	dummy_slot = 0
	write_module_times_string = if write_module_times ["-wmt"] []

CompileBuildCommand :: !String !String !CompileOrCheckSyntax !Pathname !(List Pathname) !Bool !Bool !Bool
				!CompilerOptions -> [String]
CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
					projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	= MakeCompilerOptionsString
		compileOrCheckSyntax
		projectHeapProfiling
		projectTimeProfiling
		projectEagerOrDynamic
		co
		++
		[ path
		, "-P"
		, ConcatenatePath paths
		, "-RE"
		, errors_file_name
		, "-RO"
		, out_file_name
		]
		
CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *LogEnv) !(WindowFun *LogEnv) !Pathname
				!ListTypes !*LogEnv -> (!Pathname,!CompilerMsg,!*LogEnv)
CompileHandleExitCode exitcode cocl startupdir slot errwin typewin path
					listTypes ps
	# tooltempdir = startupdir
	#	out_file_name		=  out_file_path tooltempdir slot
		errors_file_name	=  errors_file_path tooltempdir slot
		((errors,errors_and_messages_not_empty,errors_and_messages),world)
			= ReadErrorsAndWarnings errors_file_name ps.world
	# ps = { ps & world = world }
	| exitcode <> 0 && not errors_and_messages_not_empty =	// werkt dit ook voor persistent versie?
		( ""
		, GlobalError
		, errwin (	[  "Error: Compiler crashed: "+++cocl
					: if (errors == CompilerOK) ["Unable to open Errors file"] []
					]) ps
		)
	#	abcpath			= MakeABCSystemPathname path
		ps				= (if errors_and_messages_not_empty (errwin (StrictListToList errors_and_messages)) id) ps
		errors			= case exitcode of
							0	-> CompilerOK
							_	-> errors
     = (abcpath,errors,ps)

out_file_path :: String Int -> String
out_file_path tooltempdir slot
	=	file_path tooltempdir "out" slot

errors_file_path :: String Int -> String
errors_file_path tooltempdir slot
	=	file_path tooltempdir "errors" slot
	
file_path :: String String Int -> String
file_path dir base_name slot
	=	dir +++ DirSeparatorString +++ base_name +++ (if (slot == 0) "" (toString slot))

ConcatenatePath :: (List Pathname) -> String
/* old version
ConcatenatePath Nil             = ""
ConcatenatePath (path :! rest ) = path +++ ";" +++ ConcatenatePath rest
*/
ConcatenatePath ss
	# s = createArray (sSize ss) ';'
	= sUpdate 0 s ss
where
	sSize Nil = 0
	sSize (string :! Nil) = size string
	sSize (string :! rest) = size string + 1 + sSize rest
	
	sUpdate i s Nil = s
	sUpdate i s (string :! Nil)
		# (_,s) = sU (size string) i 0 s string
		= s
	sUpdate i s (string :! rest)
		# (i,s) = sU (size string) i 0 s string
		# i = inc i
		= sUpdate i s rest
	
	sU l i j s h
		| j >= l = (i,s)
		# s = update s i h.[j]
		= sU l (inc i) (inc j) s h

CodeGen	::	!String !(WindowFun *LogEnv) !CodeGenerateAsmOrCode !Pathname !Bool
			!CodeGenOptions !Processor !ApplicationOptions !Pathname !*LogEnv
			-> (!Pathname,!Bool,!*LogEnv)
CodeGen cgen` wf genAsmOrCode path timeprofile cgo tp ao startupdir ps
	# tooltempdir = maketempdir startupdir
	# (cgen_ok,cgen,cgendir)		= mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps				= wf [cgen] ps
		= ("",False,ps)

	#	objpath				= MakeObjSystemPathname tp path
		path_without_suffix	= RemoveSuffix path
		args				= MakeCodeGenOptionsString genAsmOrCode timeprofile cgo
  							  ++ [path_without_suffix]

//TODO: restore error redirection to file
  		errorsfilename		= tooltempdir +++ DirSeparatorString +++ "errors"
  		
		(res, world) 		= 'Process'.callProcess cgen args ('Maybe'.Just cgendir) ps.world
		ps					= { ps & world = world }
	| isError res
		= (objpath,False,wf [  "Error: Unable to run code generator: "+++cgen
							] ps
		)
	# exit_code = fromOk res
	#	((_, errors_not_empty, error_text),world)	= ReadErrorsAndWarnings errorsfilename ps.world
	# ps											= { ps & world = world }
	# ps											= (if errors_not_empty 
														(wf (StrictListToList error_text)) 
														( if (exit_code <> 0)
															(wf ["Error: Code generator failed for '" +++ path +++ "' with exit code: "+++toString exit_code,(quoted_string path_without_suffix)])
															id
														)
													  ) ps
	=  (objpath,exit_code==0,ps)

:: StartedCodeGenerator = !{
	scg_thread_handle :: !Int,
	scg_std_error_handle :: !Int,
	scg_abc_path :: !{#Char},
	scg_path_without_suffix :: !{#Char},
	scg_errors_file_name :: !{#Char}
  }

mangleGenerator cgen` startupdir
	# (cgen`,opts)			= splitOptions cgen`
	# (shortOK,cgen)		= GetShortPathName (startupdir +++ "\\" +++ cgen` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ cgen`) +++ "'."
		= (False,line,"")
	# cgencom = cgen % (0, size cgen - 2) +++ opts
	# cgendir = RemoveFilename (cgen % (0, size cgen - 2))
	= (True,cgencom,cgendir)

MakeCodeGenOptionsString :: CodeGenerateAsmOrCode Bool CodeGenOptions -> [String]
MakeCodeGenOptionsString genAsmOrCode timeprofile {ci,cs}
	= checkindex ++ checkstack ++ genasm
where
	checkindex	| ci = ["-ci"]
	            	 = []
	checkstack	| cs = ["-os"]
					 = []
	genasm		| genAsmOrCode == AsmGeneration
					 = ["-a"]
					 = []

/* Links the given file:
*/

Link ::	!String !(WindowFun *LogEnv) !Pathname !ApplicationOptions
		!Pathname !(List Pathname) !(List Pathname) !(List Pathname) !Bool !Bool !Bool !Bool !String
		!Bool !String !Pathname !String !Processor !Bool !*LogEnv
		 -> (!*LogEnv,!Bool)
Link linker` winfun path
		applicationOptions=:{ss,hs,initial_heap_size,profiling,heap_size_multiple,o,memoryProfilingMinimumHeapSize=minheap}
		optionspathname library_file_names object_file_names static_libraries static gen_relocs gen_linkmap
		link_resources resource_path gen_dll dll_syms startupdir dynlstr _ use_64_bit_processor ps
	# tooltempdir = maketempdir startupdir
	# (ok,linker,linkerdir)		= mangleLinker linker` startupdir
	| not ok
		# ps					= winfun [linker] ps
		= (ps,False)
	# flags						= ApplicationOptionsToFlags applicationOptions
	# optdirpath				= RemoveFilename optionspathname
	# world						= ps.LogEnv.world
	# (exists, world)			= fileExists optdirpath world
	# (err, world)				= if exists (False, world)
									( case createDirectory optdirpath world of
										(Ok Void, world) = (False,world)
										(Error _, world) = (True, world)
									)
	# ps = { ps & world = world }
	| err = (winfun ["Linker error: Unable to access or create: "+++optdirpath] ps,False)
	# (options_file_ok,ps)		= accFiles (write_options_file optionspathname flags hs ss initial_heap_size heap_size_multiple minheap use_64_bit_processor) ps
	| not options_file_ok
		= (winfun ["Linker error: Could not write the options object file: "+++optionspathname] ps,False)
	# linkopts =
		{ exe_path					= path
		, res_path					= resource_path
		, open_console				= o <> NoConsole
		, static_link				= static
		, gen_relocs				= gen_relocs
		, gen_linkmap				= gen_linkmap
		, link_resources			= link_resources
		, object_paths				= optionspathname :! (RemoveDup object_file_names)
	  	, dynamic_libs				= RemoveDup library_file_names
	  	, static_libs				= RemoveDup static_libraries
	  	, stack_size				= ss
		, gen_dll					= gen_dll
		, dll_names					= dll_syms
		, dynamics_path				= startupdir +++. DirSeparatorString +++. dynlstr
	  	}
	# linkerpath					= RemoveFilename linker
//	# linkoptspath					= MakeFullPathname linkerpath "linkopts"
//	# linkerrspath					= MakeFullPathname linkerpath "linkerrs"
	# linkoptspath					= MakeFullPathname tooltempdir "linkopts"
	# linkerrspath					= MakeFullPathname tooltempdir "linkerrs"
	# (err,ps)						= accFiles (WriteLinkOpts linkoptspath linkopts) ps
	| isJust err
		= (winfun (fromJust err) ps,False)

	# linkopts = ["-I", linkoptspath, "-O", linkerrspath]
	
	# (res, world) = 'Process'.callProcess linker linkopts ('Maybe'.Just linkerdir) ps.world
	# ps = { ps & world = world }
	| isError res = (winfun ["Error: Unable to run linker: "+++linker] ps, False)
	# exit_code = fromOk res
	# link_ok = exit_code==0
	# ((err,link_errors),ps) = accFiles (ReadLinkErrors linkerrspath) ps
	| isJust err
		= (winfun (fromJust err) ps,False)
	# (errtext,errlines) = (link_errors, length link_errors)
	| errlines<>0
		= (winfun errtext ps,link_ok)
	=  (ps,link_ok)

mangleLinker linkstr` startupdir
	# (linkstr`,opts)		= splitOptions linkstr`
	# (shortOK,linkstr)		= GetShortPathName (startupdir +++ DirSeparatorString +++ linkstr` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ DirSeparatorString +++ linkstr`) +++ "'."
		= (False,line,"")
	# linkcom = linkstr % (0, size linkstr - 2) +++ opts
	# linkdir = RemoveFilename (linkstr % (0, size linkstr - 2))
	= (True,linkcom,linkdir)

splitOptions str
	| first_q >= len_str	= (str,"")
	= (first_str,last_str)
where
	first_str =  str%(0,dec first_q)
	last_str = str % (inc first_q, len_str)
	len_str = size str
	first_q			= FindQuoteChar str len_str 0
	FindQuoteChar str len pos	= FindChar ':' str len pos;
	FindChar	:: !Char !.String !.Int !Int -> Int;
	FindChar c line linelen pos
		| pos >= linelen		=  pos;
		| c ==  line.[pos]		=  pos;
								=  FindChar c line linelen (inc pos);

ReadErrorsAndWarnings :: !Pathname !*env -> ((!CompilerMsg, !Bool, !(List String)), !*env) | FileSystem env
ReadErrorsAndWarnings path env
	#	(opened,file,env)	= fopen path FReadText env
	| not opened
		= ((CompilerOK,False,Nil),env)
	#	(errors,errors_and_warnings_read,errlist,file`) = ReadErrorAndWarningMessages file
		(_,env) = fclose file` env
	= ((errors,errors_and_warnings_read,errlist),env)
	
Strip "" = ""
Strip s
	#! last = dec (size s)
	#! char = s.[last]
	| char == '\n' || char == '\r'
		= Strip (s % (0,dec last))
	= s

ReadErrorAndWarningMessages :: !*File -> (!CompilerMsg,!Bool,!List String,!*File)
ReadErrorAndWarningMessages file
	#!	(string, file1)					= freadline file
		(eof,file2)						= fend file1
	| eof
		#!	not_empty_or_newline 		= (size string)<>0 && string.[0]<>'\n'
		= (SyntaxError,not_empty_or_newline,Strip string :! Nil,file2)
	#	(path_error,_,errlist,file3) = ReadErrorAndWarningMessages file2
	= (path_error,True,Strip string:!errlist,file3)

MakeCompilerOptionsString :: !CompileOrCheckSyntax !Bool !Bool !Bool !CompilerOptions -> [String]
MakeCompilerOptionsString compileOrCheckSyntax projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
			{neverMemoryProfile, neverTimeProfile,sa,gw,gc,listTypes,attr,reuseUniqueNodes,fusion}
	= options
where 
	memoryProfileSwitch
		| (not neverMemoryProfile && projectMemoryProfiling)
		|| projectEagerOrDynamic
			= ["-desc"]
			= []
	timeProfileSwitch
		| not neverTimeProfile && projectTimeProfiling
			= ["-pt"]
			= []
	dynamicLinkSwitch
		| projectEagerOrDynamic
			= ["-exl","-dynamics"]
			= []
	strictness
		| sa
			= []
			= ["-sa"]
	warnings
		| gw
			= []
			= ["-w"]
	comments
		| gc
			= ["-d"]
			= []
	listtypes
		| listTypes == InferredTypes
			= ["-lt"]
		| listTypes == AllTypes
			= ["-lat"]
		| listTypes == StrictExportTypes
			= ["-lset"]
			= []
	show_attr
		| attr
			= []
			= ["-lattr"]
	checksyntax
		| compileOrCheckSyntax == SyntaxCheck
			= ["-c"]
			= []
	reuse
		| reuseUniqueNodes
			= ["-ou"]
			= []
	add_fusion_option l = if fusion (l ++ ["-fusion"]) l;
	
	options		= add_fusion_option (checksyntax ++ timeProfileSwitch ++ memoryProfileSwitch ++ dynamicLinkSwitch
									++ strictness ++ warnings ++ comments ++listtypes++show_attr++reuse)
/*
start_compile_with_cache :: String Int String String String CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache path slot directory startup_arguments arguments compiler_process_ids ps
	| slot<length compiler_process_ids
		# compiler_handles_and_id = compiler_process_ids !! slot
		= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	| r==0
		= (False,compiler_process_ids,ps)
	# compiler_handles_and_id = {compiler_thread_id=compiler_thread_id,compiler_thread_handle=compiler_thread_handle,compiler_process_handle=compiler_process_handle}
	# compiler_process_ids = compiler_process_ids++[compiler_handles_and_id]
	= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps
	
start_compile_with_cache2 :: {#.Char} CompilerProcessHandlesAndId {#.Char} {#.Char} CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache2 path {compiler_thread_id,compiler_thread_handle,compiler_process_handle} directory arguments compiler_process_ids ps
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	| r==0
		= (False,compiler_process_ids,ps)
	= (True,compiler_process_ids,ps)
	
int_to_hex v
	= {hex_char i \\ i<-[0..7]};
where
		hex_char i
			# h=(v>>((7-i)<<2)) bitand 15;
			= toChar (if (h<10) (toInt '0'+h) ((toInt 'A'-10)+h));
*/			
InitCompilingInfo :: *CompilingInfo
//InitCompilingInfo = NotCompiling
InitCompilingInfo = CompilingInfo NoCompiler

/*
compile_with_cache :: String String String String (CompilingInfo,*LogEnv) -> (!Bool,!Int,!(CompilingInfo,*LogEnv))
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo NoCompiler, ps)
//	# startup_arguments = ""
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	| r==0
		= (False,0,prog)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle;
	| ok
		# ci = CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle)
		= (ok,s,(ci,ps));
		= (ok,s,prog);
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle),ps)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	| ok
		= (ok,s,prog)
	= (ok,s,(CompilingInfo NoCompiler,ps))
compile_with_cache path directory startup_arguments arguments prog=:(NotCompiling,ps)
    # (res, world) = 'Process'.callProcess path [arguments] ('Maybe'.Just directory) ps.world
    # ps = { ps & world = world }
    | isError res = (False, 0,  (NotCompiling, ps))
    # exit_code = fromOk res
    = (True, exit_code, (NotCompiling, ps))
	
compile_with_cache2 :: {#.Char} {#.Char} {#.Char} Int Int Int -> (!Bool,!Int)
compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	| r==0
		= (False,0)
	# (r,a,s) =get_integers_from_thread_message wm_number compiler_thread_handle
	| r==0
		= (False,s)
	= (True,s)
*/