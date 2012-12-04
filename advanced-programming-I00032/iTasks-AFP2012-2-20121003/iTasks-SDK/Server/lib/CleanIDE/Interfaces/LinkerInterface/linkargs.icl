implementation module linkargs

import StdArray, StdBool, StdChar, StdFile, StdFunc, StdInt
import StdMaybe
import UtilOptions, UtilStrictLists

:: LPathname :== String

:: LinkInfo` =
	{ exe_path		:: !LPathname
	, res_path		:: !LPathname
	, open_console	:: !Bool
	, static_link	:: !Bool
	, gen_relocs	:: !Bool
	, gen_linkmap	:: !Bool
	, link_resources:: !Bool
	, object_paths	:: !List LPathname
	, dynamic_libs	:: !List LPathname
	, static_libs	:: !List LPathname
	, stack_size	:: !Int
	, gen_dll		:: !Bool
	, dll_names		:: !String
	, dynamics_path :: !String
	}

emptyLinkInfo` :: LinkInfo`
emptyLinkInfo` =
	{ exe_path		= "a.exe"
	, res_path		= ""
	, open_console	= True
	, static_link	= True
	, gen_relocs	= False
	, gen_linkmap	= False
	, link_resources= False
	, object_paths	= Nil
	, dynamic_libs	= Nil
	, static_libs	= Nil
	, stack_size	= 0x100000			// 1MB old linker default
	, gen_dll		= False
	, dll_names		= ""
	, dynamics_path = ""
	}
	

//-- Link Args Options File

LinkFileVersion :== "1.0"

WriteLinkOpts	:: !{#Char} !LinkInfo` !*Files -> (!Maybe [String], !*Files)
WriteLinkOpts	linkargsPath linkargs files
	#! (opened, file, files)	=	fopen linkargsPath FWriteText files
	| not opened
		=	(Just ["Fatal open link opts..."],files)
//		=	(Just ["Fatal open link opts: "+++.linkargsPath],files)
	#! options					=	WLO linkargs
	#! file						=	WriteOptionsFile LinkFileVersion options file
	# (closed,files)			=	fclose file files
	| not closed
		= (Just ["Fatal close link opts..."],files)
	= (Nothing,files)

ReadLinkOpts	:: !{#Char} !*Files -> ((!LinkInfo`, !Bool, !{#Char}),!*Files)
ReadLinkOpts linkargsPath ps
	#	(opened, file, ps)		= fopen linkargsPath FReadData ps
	| not opened
		= ((emptyLinkInfo`,False,"The file \"" +++  linkargsPath +++ "\" could not be opened."),ps)
	#	(version, file)			= ReadVersion file
	| version <> LinkFileVersion
		#	(_, ps)				= fclose file ps
		= ((emptyLinkInfo`,False,"The file \"" +++  linkargsPath +++ "\" has the wrong version."+++version+++"<<<"),ps)
	#!	(options, file)			= ReadOptionsFile file
		linkargs				= RLO options
		(closed, ps)			= fclose file ps
	| not closed
		// generate warning?
		=	((linkargs, True,"The file \"" +++ linkargsPath +++ "\" could not be closed."), ps)
	=	((linkargs, True,""), ps)
	
WLO :: !LinkInfo` -> [Option]
WLO prefs
	= PutOptions (LinkOptionsTable) prefs

RLO :: .[Option] -> LinkInfo`
RLO options
	# prefs = GetOptions LinkOptionsTable options emptyLinkInfo`
	= prefs

LinkOptionsTable :: OptionsTable LinkInfo`
LinkOptionsTable =
	{ SimpleOption "ExePath" (\a->a.exe_path) (\v a->{a & exe_path=v})
	, SimpleOption "ResPath" (\a->a.res_path) (\v a->{a & res_path=v})
	, SimpleOption "OpenConsole" (\a->if a.open_console "1" "0") (\v a->{a & open_console=(if (v=="1") True False)})
	, SimpleOption "StaticLink" (\a->if a.static_link "1" "0") (\v a->{a & static_link=(if (v=="1") True False)})
	, SimpleOption "GenRelocations" (\a->if a.gen_relocs "1" "0") (\v a->{a & gen_relocs=(if (v=="1") True False)})
	, SimpleOption "GenLinkMap" (\a->if a.gen_linkmap "1" "0") (\v a->{a & gen_linkmap=(if (v=="1") True False)})
	, SimpleOption "LinkResources" (\a->if a.link_resources "1" "0") (\v a->{a & link_resources=(if (v=="1") True False)})
	, ListOption "ObjectPaths" (PathOption) "" (\a->a.object_paths) (\v a->{a & object_paths=v})
	, ListOption "DynamicLibs" (PathOption) "" (\a->a.dynamic_libs) (\v a->{a & dynamic_libs=v})
	, ListOption "StaticLibs" (PathOption) "" (\a->a.static_libs) (\v a->{a & static_libs=v})
	, SimpleOption "StackSize" (\a->a.stack_size) (\v a->{a & stack_size=v})
	, SimpleOption "GenDLL" (\a->if a.gen_dll "1" "0") (\v a->{a & gen_dll=(if (v=="1") True False)})
	, SimpleOption "DLLSymbols" (\a->a.dll_names) (\v a->{a & dll_names=v})
	, SimpleOption "DynamicLinker" (\a->a.dynamics_path) (\v a->{a & dynamics_path=v})
	}

PathOption =SimpleOption "Path" id const

instance fromString Int where fromString s = toInt s

//-- link errors

ReadLinkErrors :: !String !*Files -> ((Maybe [String],[String]),*Files)
ReadLinkErrors errors_path files
	# (ok,file,files)	= fopen errors_path FReadText files
	| not ok
		= ((Just ["Fatal read link errors..."],[]),files)
	# (le,file)			= ReadErrorLines file
	# (ok,files)		= fclose file files
	| not ok
		= ((Just ["Fatal close link errors..."],[]),files)
	= ((Nothing,le),files)
where
	ReadErrorLines file
		#!	(string, file)					= freadline file
			(eof,file)						= fend file
			errline							= Strip string
		| eof
			| errline == ""
				= ([],file)
			= ([errline],file)
		#	(errlist,file)		= ReadErrorLines file
		= ([errline:errlist],file)

	Strip "" = ""
	Strip s
		#! last = dec (size s)
		#! char = s.[last]
		| char == '\n' || char == '\r'
			= Strip (s % (0,dec last))
		= s

WriteLinkErrors :: !String ![String] !*Files -> (Maybe [String],*Files)
WriteLinkErrors errors_path le files
	# (ok,file,files) = fopen errors_path FWriteText files
	| not ok = (Just ["Unable to open linkerrs"],files)
	# file = WriteErrorLines le file
	# (ok,files) = fclose file files
	| not ok = (Just ["Unable to close linkerrs"],files)
	= (Nothing,files)
where
	WriteErrorLines [] file = file
	WriteErrorLines [h:t] file
		# file = fwrites (h+++"\n") file
		= WriteErrorLines t file
