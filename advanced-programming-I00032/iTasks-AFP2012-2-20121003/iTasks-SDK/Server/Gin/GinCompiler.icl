implementation module GinCompiler

import StdFile
import iTasks
import IWorld

import GinSyntax
import GinAbstractSyntax
import GinParser

import GinCompilerLogParser
import GinConfig

import Text
import Error
import OSError

import FilePath
from File import instance toString FileError, readFile, writeFile
from Process import qualified callProcess
from Map import newMap

from PmCleanSystem import 
	::CompileOrCheckSyntax(..),
	::CodeGenerateAsmOrCode(..),
	::CompilerMsg(..), 
	::CompilerOptions(..), 
	::CompilerProcess,
	::CompilingInfo,
	::List,
	::ListTypes(..),
	::LogEnv(..),
	::Pathname, 
	::WindowFun,
	CodeGen,
	CompilePersistent,
	DefaultCompilerOptions,
	ExitCleanCompiler,
	InitCompilingInfo,
	Link,
	instance == CompilerMsg

from PmPath import 
	MakeABCSystemPathname,
	MakeObjSystemPathname
	
from PmTypes import
	:: ApplicationOptions(..),
	:: CodeGenOptions(..),
	:: Output(..),
	:: Processor,
	DefApplicationOptions,
	DefaultProcessor,
	DefCodeGenOptions
	
from linkargs import
	:: LinkInfo`(..),
	:: LPathname,
	ReadLinkOpts

import UtilStrictLists

from File import deleteFile

derive class iTask CompileResult

derive JSONEncode CompilingInfo, CompilerProcess
derive JSONDecode CompilingInfo, CompilerProcess

runCompiler :: !GModule !(AModule -> (String, FunctionMap, LineMap)) (String String GinConfig FunctionMap LineMap *IWorld -> (CompileResult a, *IWorld)) *IWorld -> (CompileResult a, *IWorld)
runCompiler gMod printfun compiler iworld=:{tmpDirectory}
//1. Load configuration
# (config,iworld) = accWorldIWorld ginLoadConfig iworld 
| isNothing config = (CompileGlobalError "Configuration not found", iworld)
# config = fromJust config
//2. Parse and transform GModule
# (st, iworld) = accWorldIWorld (gToAModule gMod config) iworld
# result = runParse st
| isParseError result = (CompilePathError (getParseError result), iworld)
# aMod = expandModule (getParseSuccess result)
//3. (Pretty-)print module
# (basename,iworld) = getUniqueBasename iworld
# (source,functionMap,lineMap) = printfun { AModule | aMod & name = basename }
//4. Write source code to temp icl file
# fullname = (filenameFromConfig config tmpDirectory basename "icl")
# (result, iworld) = accWorldIWorld (writeFile fullname source) iworld
| isError result = (CompileGlobalError ("Write icl file failed: " +++ toString (fromError result)), iworld)
//5. Call compiler function
# (result, iworld) = compiler source basename config functionMap lineMap iworld
//6. Delete temp icl file
# (deleted,iworld) = accWorldIWorld (deleteFile fullname) iworld
| isError deleted = (CompileGlobalError ("Failed to delete file " +++ fullname +++ ": " +++ snd (fromError deleted)), iworld)
= (result, iworld)

getUniqueBasename :: *IWorld -> (String, *IWorld)
getUniqueBasename iworld
# (mCounter, iworld) = loadValue key iworld
# counter = case mCounter of
	Just c = c + 1
	Nothing = 0
# iworld = storeValue key counter iworld
= (prefix +++ (toString counter), iworld)
where
	key = "gin-tempfile"
	prefix = "temp"

batchBuild :: !GModule *IWorld -> (CompileResult String, *IWorld)
batchBuild gMod iworld=:{tmpDirectory} = runCompiler gMod printfun build iworld
where
	printfun :: AModule -> (String, FunctionMap, LineMap)
	printfun aMod = (prettyPrintAModule POWriteDynamics aMod, newMap, newMap)

	build :: !String !String !GinConfig FunctionMap LineMap *IWorld -> (CompileResult String, *IWorld)
	build source basename config functionMap lineMap iworld
	# (res, iworld) = accWorldIWorld (readFile (config.iTasksPath </> "Server" </> "Gin" </> "project-template")) iworld
	| isError res = (CompileGlobalError ("Failed to read project template file: " +++ toString (fromError res)), iworld)
	# projectFile = replaceSubString "{UserPath}" config.userPath (replaceSubString "{Basename}" basename (fromOk res))
	# (res, iworld) = accWorldIWorld (writeFile (filenameFromConfig config tmpDirectory basename "prj") projectFile) iworld
	| isError res = (CompileGlobalError ("Failed to write project file: " +++ toString (fromError res)), iworld)
	# projectFile = filenameFromConfig config tmpDirectory basename "prj"
	# (res, iworld) = accWorldIWorld ('Process'.callProcess (config.cleanPath </> "CleanIDE.exe") ["--batch-build", projectFile] (Just config.cleanPath)) iworld
	# (deleted,iworld) = accWorldIWorld (deleteFile projectFile) iworld
	| isError deleted = (CompileGlobalError ("Failed to delete file " +++ projectFile +++ ": " +++ snd (fromError deleted)), iworld)
	| isError res = (CompileGlobalError ("Calling Clean IDE failed: " +++ snd (fromError res)), iworld)
	| fromOk res == 0
		# batchfile = (filenameFromConfig config tmpDirectory basename "bat")
		# (res, iworld) = accWorldIWorld ('Process'.callProcess batchfile [] Nothing) iworld
		| isError res = (CompileGlobalError ("Failed to run dynamic linker batch file: " +++ snd (fromError res)), iworld)
		# dynfile = filenameFromConfig config tmpDirectory basename "dyn"
	    = (CompileSuccess dynfile, iworld)
	# (res, iworld) = accWorldIWorld (readFile (filenameFromConfig config tmpDirectory basename "log")) iworld
	| isError res = (CompileGlobalError ("Read log file failed: " +++ toString (fromError res)), iworld)
	# log = fromOk res
	= (CompileGlobalError log, iworld)
	
syntaxCheck :: !GModule *IWorld -> (CompileResult Void, *IWorld)
syntaxCheck gMod iworld = runCompiler gMod syntaxCheckPrintAModule (compile SyntaxCheck) iworld

// --------------------------------------------------------------------------------
// Compiler interface
// --------------------------------------------------------------------------------

compile :: CompileOrCheckSyntax !String !String !GinConfig FunctionMap LineMap *IWorld -> (CompileResult Void, *IWorld)
compile compileOrCheckSyntax source basename config functionMap lineMap iworld=:{tmpDirectory}
# (mCompilingInfo, iworld) = loadCompilingInfo iworld
# compilingInfo = case mCompilingInfo of
	Just c = c
	Nothing = InitCompilingInfo
# ((compileResult, compilingInfo), iworld) = accWorldIWorld (compile` compilingInfo) iworld
# iworld = storeCompilingInfo compilingInfo iworld
# iworld = exitCompiler iworld //<- TODO: remove
= (compileResult, iworld)
where
	compile` :: CompilingInfo *World -> ((CompileResult Void, CompilingInfo), *World)
	compile` compilingInfo world
	//Take iTasks compiler if iTasks is in subdirectory of Clean distribution
	//Otherwise, fallback to compiler shipped with Clean distribution
	# compilerPath = 
		if (config.GinConfig.cleanPath </> "" == takeDirectory config.GinConfig.iTasksPath </> "")
			(dropDirectory config.GinConfig.iTasksPath </> "Compiler")
			("Tools" </> "Clean System")
	# env = { errors = [], world = world }
	# compilingInfo = InitCompilingInfo //<- TODO: remove
	# (compilingInfo, (env, _, compilerMsg)) = 
		CompilePersistent
				(compilerPath </> "CleanCompiler.exe" +++ " : -h 64M -dynamics -generics")
		        False                                      //Don't write module times
		        addError                                   //Error display function
		        (\_ x -> x)                                //Types display function
		        compileOrCheckSyntax
		        (basename +++ ".icl")
		        (ListToStrictList (searchPaths config tmpDirectory))
		        False                                      //No memory profiling
		        False                                      //No time profiling
		        True                                       //Eager or dynamic linking
		        { DefaultCompilerOptions & listTypes = NoTypes }
		        config.cleanPath
		        compilingInfo
		        env
	# log = join "\n" (map (join "\n") env.LogEnv.errors)
	| compilerMsg == GlobalError
		= ((CompileGlobalError ("Calling Clean compiler failed: " +++ log), compilingInfo), env.LogEnv.world)
	| compilerMsg == CompilerOK
		= ((CompileSuccess Void, compilingInfo), env.LogEnv.world)
	# errors = (findPathErrors (parseCleanCompilerLog log) functionMap lineMap)
	| isEmpty errors = ((CompileGlobalError log, compilingInfo), env.LogEnv.world)
	= ((CompilePathError errors, compilingInfo), env.LogEnv.world)

loadCompilingInfo :: *IWorld -> (Maybe CompilingInfo, *IWorld)
loadCompilingInfo iworld = loadValue compilerId iworld

storeCompilingInfo :: CompilingInfo *IWorld -> *IWorld
storeCompilingInfo compilingInfo iworld = storeValue compilerId compilingInfo iworld

compilerId :: String
compilerId = "gin-compiler"

deleteCompilingInfo :: *IWorld -> *IWorld
deleteCompilingInfo iworld = deleteValue compilerId iworld

exitCompiler :: *IWorld -> *IWorld
exitCompiler iworld
# (mCompilingInfo, iworld) = loadCompilingInfo iworld
| isNothing mCompilingInfo = iworld
# compilingInfo = fromJust mCompilingInfo
# (_, iworld) = accWorldIWorld (curry ExitCleanCompiler (fromJust mCompilingInfo)) iworld
= deleteCompilingInfo iworld

/*
// --------------------------------------------------------------------------------
// Code generator interface
// --------------------------------------------------------------------------------

codegen :: !String !GinConfig *IWorld -> (CompileResult Void, *IWorld) 
codegen basename config iworld=:{tmpDirectory} = accWorldIWorld codegen` iworld
where
	codegen` :: !*World -> (CompileResult Void, *World)	
	codegen` world
	# env = { errors = [], world = world }
	# abcfile = MakeABCSystemPathname (filenameFromConfig config tmpDirectory basename "icl")
	# (pathname, ok, env) = CodeGen
		("Tools" </> "Clean System" </> "CodeGenerator.exe")
		addError
		CodeGeneration
		abcfile
		False	//No time profiling
		DefCodeGenOptions
		DefaultProcessor
		ginApplicationOptions
		config.cleanPath
		env
	# log = join "\n" (map (join "\n") env.LogEnv.errors)
	| not ok
		= (CompileGlobalError ("Code generator error: " +++ log), env.LogEnv.world)
	# world = env.LogEnv.world
	# (_,world) = deleteFile abcfile world
	= (CompileSuccess Void, world)

// --------------------------------------------------------------------------------
// Linker interface
// --------------------------------------------------------------------------------

link :: String !GinConfig *IWorld -> (CompileResult Void, *IWorld) 
link basename config iworld=:{tmpDirectory} = accWorldIWorld (link` basename config) iworld
where
	link` :: !String !GinConfig *World -> (CompileResult Void, *World)	
	link` basename config world
	# ((linkinfo, ok, err), world) = accFiles (ReadLinkOpts (config.iTasksPath </> "Server" </> "Gin" </> "linkopts-template")) world
	| not ok = (CompileGlobalError ("Linker error: Failed to read linker options file: " +++ err), world)
	
	# env = { errors = [], world = world }
	# linkopts = MakeObjSystemPathname DefaultProcessor (filenameFromConfig config tmpDirectory (basename +++ "_options") "icl")
	# (env, ok) = Link
		("Tools" </> "Clean System" </> "StaticLinker.exe")
		addError
		(filenameFromConfig config tmpDirectory basename "exe")
		ginApplicationOptions
		linkopts										    // linker obtions file
		(dynamicLibs config tmpDirectory basename linkinfo)	// dynamic library file names
		(objectPaths config tmpDirectory basename linkinfo)	// object file names
		Nil													// static library file names
		False												// link statically
		False												// generate relocations
		False												// generate link map
		False												// link in resources
		""													// source of resources to link in
		False												// generate dll?
		""													// dll export symbols
		config.cleanPath									// startup directory
		("Tools" </> "Dynamics" </> "DynamicLinker.exe")
		DefaultProcessor
		False												// 64 bit target processor
		env
	//Delete object file and link opts
	# world = env.LogEnv.world
	# (_, world) = deleteFile (MakeObjSystemPathname DefaultProcessor (filenameFromConfig config tmpDirectory basename "obj")) world
	# (_, world) = deleteFile linkopts world
	# log = join "\n" (map (join "\n") env.LogEnv.errors)
	| not ok
		= (CompileGlobalError (log), world)
	= (CompileSuccess Void, world)

dynamicLibs :: !GinConfig !String !String !LinkInfo` -> List LPathname
dynamicLibs config tempPath basename linkinfo = Map (setLinkPaths config tempPath basename) linkinfo.dynamic_libs

objectPaths :: !GinConfig !String !String !LinkInfo` -> List LPathname
objectPaths config tempPath basename linkinfo = Map (setLinkPaths config tempPath basename) linkinfo.object_paths
	
setLinkPaths :: !GinConfig !String String !String -> String
setLinkPaths config tempPath basename haystack
# haystack = replaceSubString "{Project}" tempPath haystack
# haystack = replaceSubString "{ITasks}" config.iTasksPath haystack
# haystack = replaceSubString "{Application}" config.cleanPath haystack
# haystack = replaceSubString "{Serialization}" serializationModule haystack
= replaceSubString "{Basename}" basename haystack
*/

// --------------------------------------------------------------------------------
// Utility functions
// --------------------------------------------------------------------------------

failed :: (CompileResult a) -> Bool
failed (CompileSuccess _) = False
failed _ = True

convertFail :: (CompileResult a) -> (CompileResult b)
convertFail (CompileGlobalError msg) = CompileGlobalError msg
convertFail (CompilePathError paths) = CompilePathError paths

addError :: [String] LogEnv -> LogEnv
addError err env = 	/* trace_n (join "\n" err) */ { env & errors = env.errors ++ [err] }

accWorldIWorld :: (*World -> (b, *World)) *IWorld -> (b, *IWorld)
accWorldIWorld f iworld
# (r, world) = f iworld.IWorld.world
= (r, { IWorld | iworld & world = world })

appWorldIWorld :: (*World -> *World) *IWorld -> *IWorld
appWorldIWorld f iworld = { IWorld | iworld & world = f iworld.IWorld.world }
	
filenameFromConfig :: !GinConfig !String !String !String -> String
filenameFromConfig config tempPath basename extension = tempPath </> basename +++ "." +++ extension

// --------------------------------------------------------------------------------
// Project settings
// --------------------------------------------------------------------------------

ginApplicationOptions :: ApplicationOptions
ginApplicationOptions = { DefApplicationOptions & o = NoConsole}

searchPaths :: !GinConfig !String -> [String]
searchPaths config tempPath = 
	[ tempPath
	: config.userPath
	: map ((</>) config.iTasksPath) iTasksPaths
	++ map ((</>) config.cleanPath) cleanPaths
	]

iTasksPaths :: [String]
iTasksPaths = 
	[ "Server"
	, "Server\\API\\Common"
	, "Server\\API\\Core"
	, "Server\\API\\Extensions"
	, "Server\\API\\Extensions\\Admin"
	, "Server\\Framework"
	, "Server\\Framework\\Data"
	, "Server\\Framework\\Handlers"
	, "Server\\lib"
	, "Server\\lib\\Http"
	, "Server\\lib\\Platform\\OS-Independent"
	, "Server\\lib\\Platform\\OS-Independent\\Data"
	, "Server\\lib\\Platform\\OS-Independent\\Database"
	, "Server\\lib\\Platform\\OS-Independent\\Database\\SQL"
	, "Server\\lib\\Platform\\OS-Independent\\GUI"
	, "Server\\lib\\Platform\\OS-Independent\\Internet"
	, "Server\\lib\\Platform\\OS-Independent\\Internet\\HTTP"
	, "Server\\lib\\Platform\\OS-Independent\\Math"
	, "Server\\lib\\Platform\\OS-Independent\\Network"
	, "Server\\lib\\Platform\\OS-Independent\\System"
	, "Server\\lib\\Platform\\OS-Independent\\Test"
	, "Server\\lib\\Platform\\OS-Independent\\Text"
	, "Server\\lib\\Platform\\OS-Independent\\Text\\Encodings"
	, "Server\\lib\\Platform\\OS-Windows\\Data"
	, "Server\\lib\\Platform\\OS-Windows\\Network"
	, "Server\\lib\\Platform\\OS-Windows\\System"
	, "Server\\lib\\Platform\\OS-Windows\\Database"
	, "Server\\lib\\Platform\\OS-Windows\\Database\\SQL"
	, "Server\\lib\\Platform\\OS-Windows-32\\System"
	, "Server\\lib\\graph_copy"
	]

cleanPaths :: [String]
cleanPaths = 
	[ "Libraries\\StdEnv"
	, "Libraries\\StdLib"
	, "Libraries\\Generics"
	, "Libraries\\Dynamics"
	, "Libraries\\Dynamics\\general"
	, "Libraries\\Dynamics\\implementation"
	, "Libraries\\Dynamics\\implementation\\windows"
	, "Libraries\\Directory"
	, "Libraries\\TCPIP"
	]
