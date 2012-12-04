implementation module PmProject

import GenPrint, GenParse, GenUpdate, GenVisualize
import StdClass,StdBool, StdInt, StdString,StdArray, StdFunc, StdTuple, StdList
import StdMaybe
import PmPath, UtilStrictLists
from UtilDate import NoDate, :: DATE
import UtilNewlinesFile
import PmTypes
from StdSystem import dirseparator
import Platform
import UtilOptions, PmFiles

derive class iTask	Project, CodeGenOptions, ProjectDynamicInfo, ProjectStaticInfo, StaticLibInfo, List, LinkOptions, ApplicationOptions, InfListItem, UndefModule, UndefSymbol
derive class iTask	LinkMethod, Output, ModInfo, ABCLinkInfo, DATE, EditWdOptions, CompilerOptions, OptionalWindowPosAndSize, EditOptions, ListTypes, WindowPos_and_Size, NewlineConvention

derive bimap		Maybe, (,)

//--

::	Def_and_Imp				:== Bool;
DclMod					:== True;
IclMod					:== False;

::	WindowOpen_and_Closed	:== Bool;
WinOpen					:== True;
WinClosed				:== False;
	
::	Modification			:== Bool;
Modified				:== True;
Unmodified				:== False;

//	First element of InfList (if any) is the root module.

::	InfList		:==	List InfListItem
	
::	InfListItem	=
	{ mn	:: !Modulename		// module name
	, info	:: !ModInfo			// module info
	, src	:: !Bool			// src up to date?
	, abc	:: !Bool 			// abc up to date?
	}

::	InfUpdate 	:== InfListItem -> (!InfListItem, Bool)

PR_InitProject :: Project;
PR_InitProject =
	{ built				= True
	, saved				= True
	, exec				= True
	, execpath			= EmptyPathname
	, inflist			= Nil
	, codegenopt		= DefCodeGenOptions
	, code_gen_options_unchanged	= True
	, applicationopt	= DefApplicationOptions
//	, projectopt		= DefProjectOptions
	, linkOptions		= DefaultLinkOptions
	, prjpaths			= Nil
	, staticLibInfo		= DefStaticLibInfo
	, target			= ""
	, static_info		= EmptyStaticInfo
	, dynamic_info		= EmptyDynamicInfo
	, prec = Nothing
	, posl = Nothing
	}

PR_GetExecPath	:: !Project -> String
PR_GetExecPath {execpath} = execpath

PR_SetExecPath	:: !String !Project -> Project
PR_SetExecPath pth prj = {prj & execpath = pth}

DefStaticLibInfo =
	{ sLibs = Nil
	, sDcls = Nil
	, sDeps = Nil
	}

PR_ProjectSet :: !Project -> Bool;
PR_ProjectSet project=:{inflist=Nil}	=  False;
PR_ProjectSet project=:{inflist}		=  True;

PR_NewProject :: !String !EditWdOptions !CompilerOptions !CodeGenOptions !ApplicationOptions
				!(List String) !LinkOptions -> Project;
PR_NewProject main_module_file_name eo compilerOptions cgo ao prjpaths linkOptions =
	{ PR_InitProject
	& built			= False
	, saved			= False
	, exec			= False
	, execpath		= PlatformDependant	//MakeExecPathname main_module_file_name
						("{Project}"+++{dirseparator}+++modname+++".exe")		// Win
						("{Project}"+++{dirseparator}+++modname)				// Mac
	, inflist		=
		{ mn		= modname
		, info		=	{ dir		= "{Project}"//dirname
						, defeo	= eo
						, impeo	= eo
						, compilerOptions		= compilerOptions
						, defopen	= False
						, impopen = True
						, date	= NoDate
						, abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil}
						}
		, src		= True
		, abc		= True
		} :! Nil
	, codegenopt	= cgo
	, code_gen_options_unchanged	= True
	, applicationopt= ao
	, prjpaths		= if (StringOccurs dirname prjpaths) prjpaths (dirname:!prjpaths)
	, linkOptions	= linkOptions
	, staticLibInfo = DefStaticLibInfo
	, target		= "StdEnv"
	}
where 
	modname	= GetModuleName main_module_file_name;
	dirname	= RemoveFilename main_module_file_name;
	
//--

PR_SetBuilt	:: !(List Modulename) !.Project -> .Project;
PR_SetBuilt used project=:{inflist=Nil} = {Project | project & built = True};
PR_SetBuilt used prj=:{inflist=infl=:(root:!rest),saved}
	#! len		= LLength rest
	# used		= Map GetModuleName used
	# rest		= RemoveUnusedModules used rest
	# len`		= LLength rest
	# unchanged	= len == len`
	= {prj & built=True,saved=saved && unchanged,inflist= root:!rest}
where	
	{mn=rootmn,info} = root
	RemoveUnusedModules used list = FilterR member list
	where
		member {mn}	= StringOccurs mn used && rootmn <> mn
		
//--

PR_AddABCInfo :: !String !(List LinkObjFileName) !(List LinkLibraryName) !CompilerOptions !EditWdOptions !EditWdOptions !Project -> Project
PR_AddABCInfo mod_path dep_objects dep_libraries compilerOptions defeo impeo project=:{inflist=Nil}
//	# project = trace_n ("PR_Add: no root...") project
	= project
PR_AddABCInfo mod_path dep_objects dep_libraries compilerOptions defeo impeo project=:{inflist}
//	# project = trace_n ("PR_Add: adding "+++mod_path) project
	# inflist		= TryInsertInList mod_name mod_dir inflist
	# (inflist,_)	= UpdateList mod_name update inflist;
	= {project & saved=False,inflist=inflist, built=False}
where
	mod_name			= GetModuleName mod_path
	mod_dir				= RemoveFilename mod_path

	update infListItem=:{InfListItem | info}
		= (	{ InfListItem | infListItem
			& info.abcLinkInfo.linkObjFileNames		= dep_objects
			, info.abcLinkInfo.linkLibraryNames		= dep_libraries
			, info.dir								= mod_dir
			, info.compilerOptions					= compilerOptions
			}, True)

	TryInsertInList :: !String !String !InfList -> InfList
	TryInsertInList importermn importerdir Nil	// no root module...
		= Nil
	TryInsertInList importermn importerdir ((root=:{mn,info}):!rest)
		| importermn == mn	// updating root module...
			# root	= {InfListItem | root & info = {ModInfo | info & dir = importerdir}}
			= root :! rest
		# rest		= TryInsertImporter rest rest
		= root :! rest
	where
		TryInsertImporter ::	!InfList !InfList -> InfList
		TryInsertImporter Nil list
			# item =
				  {	mn		= importermn, 
					info	= { dir		= importerdir,
								compilerOptions	
										= compilerOptions,
								defeo	= defeo,
								impeo	= impeo,
								defopen	= False,
								impopen	= False,
								date	= NoDate,
								abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} },
					src		= True,
					abc		= True }
			= item :! list
		TryInsertImporter (({mn}):!rest) list
			| importermn<>mn
				= TryInsertImporter rest list
				= list
					

//--

PR_ClearDependencies :: !Project -> Project
PR_ClearDependencies project=:{inflist=Nil}
	= {project & saved = False, inflist = Nil, built = False}
PR_ClearDependencies project=:{inflist=il=:(root :! rest)}
	= {project & saved = False, inflist = root` :! Nil, built = False}
where
	root` =
		{ InfListItem
		| root
		& info =
			{ root.InfListItem.info
			& date = NoDate
			, abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil}}}
	
PR_SetRoot :: !String !EditWdOptions !CompilerOptions !Project -> Project;
PR_SetRoot root eo co project=:{inflist=Nil}
	= project;
PR_SetRoot newroot eo compilerOptions project=:{prjpaths}
	=	{project &	saved	= False,
					built	= False,
					exec	= False,
					inflist	= {	mn		= modname,
								info	= {	dir		= dirname,
											defeo	= eo,
											impeo	= eo,
											compilerOptions		= compilerOptions,
											defopen	= False,	/* nonsense, we don't know this! */
											impopen = True,
											date	= NoDate,
											abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} },
								src		= True,
								abc		= True
								} :! Nil,
					prjpaths= if (StringOccurs dirname prjpaths) prjpaths (dirname:!prjpaths) /* a bit iffy */
		  };
where 
	modname	= GetModuleName newroot;
	dirname	= RemoveFilename newroot;
	

PR_SetCompiled	:: !Modulename !Project -> Project;
PR_SetCompiled modname project=:{inflist}
	=  {project & inflist = inf`}
where 
	(inf`, _)	= UpdateList modname setcompiled inflist;
	
	setcompiled	:: !InfListItem -> (!InfListItem,!Bool);
	setcompiled itm=:{src,abc}
		= ({itm & src = True},True);
	
		
PR_SetCodeGenerated	:: !Modulename !Project -> Project;
PR_SetCodeGenerated modname project=:{inflist}
	= {project & inflist = inf`};
where 
	(inf`,_)	= UpdateList modname setcode inflist;
	
	setcode :: !InfListItem -> (!InfListItem, !Bool);
	setcode itm=:{InfListItem | src}
		= ({InfListItem | itm & abc = True}, True);
	
	
PR_SetSysCodeGenerated :: !Project -> Project;
PR_SetSysCodeGenerated project = {project & code_gen_options_unchanged = True};
	
PR_SetLinked	:: !Project -> Project;
PR_SetLinked project=:{inflist}
	=  {project & exec = True};
	
PR_SetSaved	:: !Project -> Project;
PR_SetSaved project = {Project | project & saved = True};
		
PR_SetCodeGenOptions	:: !CodeGenOptions !Project -> Project;
PR_SetCodeGenOptions options project=:{inflist,saved,codegenopt,code_gen_options_unchanged} =
	{ project
	& inflist						= infl`
	, saved							= saved && unchanged
	, code_gen_options_unchanged	= code_gen_options_unchanged && cg_unchanged
	, codegenopt					= options
	}
where 
	(infl`,_)				= infl`_;
	infl`_ | cg_unchanged	= (inflist,True);
							= P_MapR setcode inflist;
	unchanged				= cg_unchanged;
	cg_unchanged			= options == codegenopt;
	
	setcode :: !InfListItem -> (!InfListItem, !Bool);
	setcode itm=:{InfListItem | src}
		= ({InfListItem | itm & abc = False}, True);
	
	
PR_SetApplicationOptions :: !ApplicationOptions !Project -> Project;
PR_SetApplicationOptions options project=:{saved,exec,applicationopt}
	= {project & applicationopt = options, exec = exec && unchanged,saved=saved && unchanged};
	where 
		unchanged	= eqAppOpts options applicationopt;

eqAppOpts :: !ApplicationOptions !ApplicationOptions -> Bool
eqAppOpts ao1 ao2
	=	ao1.hs == ao2.hs &&
		ao1.ss == ao2.ss &&
		ao1.em == ao2.em &&
		ao1.heap_size_multiple == ao2.heap_size_multiple &&
		ao1.initial_heap_size == ao2.initial_heap_size &&
		ao1.set == ao2.set &&
		ao1.sgc == ao2.sgc &&
		ao1.pss == ao2.pss &&
		ao1.marking_collection == ao2.marking_collection &&
		ao1.o == ao2.o &&
		ao1.fn == ao2.fn &&
		ao1.fs == ao2.fs &&
		ao1.write_stderr_to_file == ao2.write_stderr_to_file &&
		ao1.memoryProfiling == ao2.memoryProfiling &&
		ao1.memoryProfilingMinimumHeapSize == ao2.memoryProfilingMinimumHeapSize &&
		ao1.profiling == ao2.profiling &&
		ao1.stack_traces == ao2.stack_traces &&
		ao1.standard_rte == ao2.standard_rte

// do we need to check resource linking flags???
eqLinkOpts :: !LinkOptions !LinkOptions -> Bool
eqLinkOpts lo1 lo2 =
	lo1.method == lo2.method &&
	lo1.generate_relocations == lo2.generate_relocations &&
	lo1.generate_link_map == lo2.generate_link_map &&
	lo1.link_resources == lo2.link_resources &&
	(if lo1.link_resources (lo1.resource_source == lo2.resource_source) True) &&
	EQStrings (SortStrings lo1.extraObjectModules) (SortStrings lo2.extraObjectModules) &&
	EQStrings (SortStrings lo1.libraries) (SortStrings lo2.libraries) &&
	lo1.generate_dll == lo2.generate_dll &&
	lo1.dll_export_list_name == lo2.dll_export_list_name

	
PR_SetLinkOptions	:: !Project !LinkOptions -> Project;
PR_SetLinkOptions project linkOptions
	| eqLinkOpts linkOptions project.Project.linkOptions
		=	{Project | project & linkOptions = linkOptions, saved = False};
	| otherwise
		=	{Project | project & linkOptions = linkOptions, exec = False, saved = False};

PR_GetLinkOptions	:: !Project -> LinkOptions;
PR_GetLinkOptions project
	=  project.Project.linkOptions;

PR_SetPaths	:: !Bool !(List String) !(List String) !Project -> Project;
PR_SetPaths def defs new project=:{Project | inflist=Nil} = project;
PR_SetPaths def defs new project=:{Project | built,inflist=infl=:((root=:{InfListItem | info={dir}}):!rest),prjpaths,saved}
	| def	= {Project | project &
							built				= built && olddirs,
							saved				= saved && olddirs,
							inflist				= inflist1 };
			= {Project | project &
							built 				= built && olddirs,
							saved				= saved && unchanged && olddirs,
							inflist				= inflist1,
							prjpaths			= prjpaths1 };
	where 
		(inflist1,olddirs)	= P_MapR SetDcl_and_Icl_and_ABCModified infl;
		unchanged			= EQStrings (SortStrings prjpaths) (SortStrings prjpaths1);
		prjpaths1 	| def	= prjpaths;
					| StringOccurs dir new
							= new;
							= dir:!new;
						
		SetDcl_and_Icl_and_ABCModified :: !InfListItem -> (!InfListItem,!Bool);
		SetDcl_and_Icl_and_ABCModified itm=:{InfListItem | info=minfo=:{dir}}
		| unchanged	= ({itm & src=False}, True);
					= ({itm & info = {minfo & dir="", date=NoDate}, src=False},False);
			where 
				unchanged	= StringOccurs dir defs || StringOccurs dir prjpaths1;
			
		
		
PR_GetCodeGenOptions :: !Project -> CodeGenOptions;
PR_GetCodeGenOptions project=:{codegenopt} =  codegenopt;

//PR_GetProcessor :: !Project -> Processor;
//PR_GetProcessor project=:{codegenopt={tp}} = tp;
	
PR_GetApplicationOptions :: !Project -> ApplicationOptions;
PR_GetApplicationOptions project=:{applicationopt} =  applicationopt;
	
PR_GetPaths	:: !Project -> List String;
PR_GetPaths project=:{Project | prjpaths} = prjpaths;

PR_GetRootModuleName :: !Project -> String
PR_GetRootModuleName p=:{inflist=Nil}
	= EmptyPathname
PR_GetRootModuleName p=:{inflist={mn}:!rest}
	= mn

PR_GetRootPathName	:: !Project -> (String,Project)
PR_GetRootPathName p=:{inflist=Nil}
	= (EmptyPathname,p)
PR_GetRootPathName p=:{inflist={mn,info={dir}}:!rest}
	| size dir==0
		= (EmptyPathname,p)
		= (MakeFullPathname dir (MakeImpPathname mn),p)

PR_GetRootPath	:: !Project -> String
PR_GetRootPath {inflist=Nil}
	= EmptyPathname;
PR_GetRootPath {inflist={mn,info={dir}}:!rest}
	| size dir==0
		= EmptyPathname;
		= dir;

PR_GetModulenames	:: !Bool !Def_and_Imp !Project -> (List String,Project)
PR_GetModulenames full def project=:{inflist}
	= (modnames,project)
where 
	(modnames,_)	= P_MapR GetModulenames inflist
	
	GetModulenames :: !InfListItem -> (!String ,!Bool)
	GetModulenames {mn,info={dir}}
		| full && def	= (MakeFullPathname dir (MakeDefPathname mn),True)
		| full			= (MakeFullPathname dir (MakeImpPathname mn),True)
						= (mn,True)

PR_GetOpenModulenames	:: !Project -> List String
PR_GetOpenModulenames project=:{inflist}
	= FlattenList modnames
where 
	(modnames,_)	= P_MapR GetModulenames inflist
	
	GetModulenames :: !InfListItem -> (List String,!Bool)
	GetModulenames {mn,info={dir,defopen,impopen}}
		| defopen && impopen	= ((defname :! impname :! Nil),True)
		| defopen				= ((defname :! Nil),True)
		| impopen				= ((impname :! Nil),True)
								= (Nil,True)
	where
		defname = MakeFullPathname dir (MakeDefPathname mn)
		impname = MakeFullPathname dir (MakeImpPathname mn)

PR_GetModuleStuff :: !Project -> List (Modulename,String,Modulename,String)
PR_GetModuleStuff project=:{inflist}
	= stuff
where 
	(stuff,_)	= P_MapR GetModulenames inflist
	
	GetModulenames :: !InfListItem -> ((!Modulename,!String,!Modulename,!String),!Bool)
	GetModulenames {mn,info={dir}}
						= ((MakeDefPathname mn,dir,MakeImpPathname mn,dir),True)

PR_Built :: !Project -> Bool;
PR_Built project=:{Project | built} =  built;
		
PR_SrcUpToDate :: !Modulename !Project -> Bool;
PR_SrcUpToDate modname project=:{inflist}
	# item = FindInList modname inflist
	| isNothing item
		= False
	# item = fromJust item
	= item.src
	
	
PR_ABCUpToDate	:: !Modulename !Project -> Bool;
PR_ABCUpToDate modname project=:{inflist}
	# item = FindInList modname inflist
	| isNothing item
		= False
	# item = fromJust item
	= item.abc
	
	
PR_SysUptoDate :: !Project -> Bool;
PR_SysUptoDate project=:{code_gen_options_unchanged} = code_gen_options_unchanged;
	
PR_ExecUpToDate	:: !Project -> Bool;		
PR_ExecUpToDate project=:{exec} = exec;

PR_Saved :: !Project -> Bool;
PR_Saved {Project | saved} = saved;

PR_AddRootModule ::	!Bool !CodeGenOptions !ApplicationOptions !(List String) !LinkOptions
					!Modulename !ModInfo -> Project;
PR_AddRootModule built cg ao prjs linkOptions mn info=:{dir}
  = { PR_InitProject
  	& built			= built && dir <> ""
	, saved			= False	// ???
	, exec			= True
	, execpath		= ""//MakeExecPathname (MakeFullPathname dir mn)
	, code_gen_options_unchanged
						= True
	, inflist			= root:!Nil
	, codegenopt		= cg
	, applicationopt	= ao
	, prjpaths		= prjs
	, linkOptions	= linkOptions
	, staticLibInfo = DefStaticLibInfo
	, target = ""
	};
where 
	root	= {	mn		= mn,info	= info,src		= True,abc		= True };
	
	
PR_AddModule :: !Modulename !ModInfo !Project -> Project;
PR_AddModule mn info=:{dir} project=:{built,inflist=root:!rest}
	= {project & built = built && dir<>"", inflist = root:!new:!rest};
where 
	new	= {	mn		= mn,
			info	= info,
			src		= True,
			abc		= True };
PR_AddModule mn info=:{dir} project=:{built,inflist=Nil}
	= {project & built = built && dir<>"", inflist = new:!Nil};
where 
	new	= {	mn		= mn,
			info	= info,
			src		= True,
			abc		= True };
	
PR_GetModuleInfo :: !Modulename !Project -> Maybe ModInfo
PR_GetModuleInfo mn {Project | inflist}
	# item = FindInList mn inflist
	| isNothing item
		= Nothing
	= Just (fromJust item).InfListItem.info
	
PR_UpdateModule :: !Modulename !(ModInfo -> ModInfo) !Project -> Project;
PR_UpdateModule mn update project=:{inflist,saved}
	= {project & inflist = infl`,saved = saved && unchanged};
where 
	(infl`,unchanged)	= UpdateList mn update` inflist;
	update` itm=:{InfListItem | info}	= ({InfListItem | itm & info = info`}, unchanged);
	where 
		info`		= update info;
		unchanged	= eqInfo info info`;

eqInfo :: !ModInfo !ModInfo -> Bool
eqInfo info1 info2
	=	info1.defeo.eo == info2.defeo.eo &&
		info1.impeo.eo == info2.impeo.eo &&
		eqCO info1.compilerOptions info2.compilerOptions &&
		info1.defeo.pos_size == info2.defeo.pos_size && 
		info1.impeo.pos_size == info2.impeo.pos_size &&
		info1.defopen == info2.defopen &&
		info1.impopen == info2.impopen

eqCO :: !CompilerOptions !CompilerOptions -> Bool
eqCO co1 co2
	=	co1.neverTimeProfile == co2.neverTimeProfile &&
		co1.neverMemoryProfile == co2.neverMemoryProfile &&
		co1.sa == co2.sa &&
		co1.CompilerOptions.listTypes == co2.CompilerOptions.listTypes &&
		co1.gw == co2.gw &&
		co1.bv == co2.bv &&
		co1.gc == co2.gc
							
PR_UpdateModules :: ![Modulename] !(ModInfo -> ModInfo) !Project -> Project
PR_UpdateModules mn update project
	= seq [(PR_UpdateModule m update) \\ m <- mn] project	// DvA quick hack, not very efficient!

//
//	Operations on tables
//

UpdateList :: !String InfUpdate !InfList -> (!InfList,!Bool)
UpdateList key update list = UpdateList2 key update list Nil
where
	UpdateList2 :: !String InfUpdate !InfList !InfList -> (!InfList,!Bool)
	UpdateList2 key update Nil acc
		=  (Reverse2 acc Nil,True)
	UpdateList2 key update ((first=:{mn,info}):!rest) acc
		| mn <> key
			= UpdateList2 key update rest (first:!acc)
		# (first,changed)	= update first
		= (Reverse2 acc (first :! rest), changed)

FindInList	:: !String !InfList -> Maybe InfListItem
FindInList key Nil									= Nothing
FindInList key ((itm=:{mn,info}):!rest)	| mn <> key	= FindInList key rest
													= Just itm

//--

SetProject :: !{#Char} !{#Char} !ProjectGlobalOptions -> Project
SetProject applicationDir projectDir
		{ pg_built
		, pg_codegen
		, pg_application
		, pg_projectPaths, pg_link, pg_mainModuleInfo={ModInfoAndName|name, info},pg_otherModules
		, pg_target
		, pg_staticLibInfo
		, pg_execpath
		, pg_static
		, pg_dynamic
//		, pg_generateDLL
//		, pg_exportedDLL
		, pg_precompile
		, pg_postlink
		}
	# paths			= ExpandPaths applicationDir projectDir  pg_projectPaths
	# linkOptions	= ExpandLinkOptionsPaths applicationDir projectDir pg_link
	# project		= PR_AddRootModule pg_built pg_codegen pg_application paths linkOptions name (ExpandModuleInfoPaths applicationDir projectDir info)
	# project		= addModules pg_otherModules project
	# staticLibInfo = ExpandStaticLibPaths applicationDir projectDir pg_staticLibInfo
	# project		= PR_SetStaticLibsInfo staticLibInfo project
	# project		= PR_SetTarget pg_target project
	# exepath		= ExpandPath applicationDir projectDir pg_execpath
	# project		= PR_SetExecPath exepath project
//	# project		= PR_SetGenDLL pg_generateDLL project
//	# project		= PR_SetExpDLL pg_exportedDLL project
	// default of used appopts in exe are ok isn't right :-(
	# pg_static		= FixStatic applicationDir projectDir pg_static
	# project		= {project & static_info = pg_static, dynamic_info = pg_dynamic}
	# project		= {project & prec = pg_precompile, posl = pg_postlink}
	= project
where
	addModules Nil project
		=	project
	addModules ({ModInfoAndName|name, info} :! t) project
		=	addModules t (PR_AddModule name (ExpandModuleInfoPaths applicationDir projectDir info) project)

	FixStatic ap pp si=:{stat_mods,stat_objs,stat_slibs,stat_dlibs,stat_paths} =
		{ si
		& stat_mods		= ExpandPaths ap pp stat_mods
		, stat_objs		= ExpandPaths ap pp stat_objs
		, stat_slibs	= ExpandPaths ap pp stat_slibs
		, stat_dlibs	= ExpandPaths ap pp stat_dlibs
		, stat_paths	= ExpandPaths ap pp stat_paths
		}

GetProject :: !{#Char} !{#Char} !Project -> ProjectGlobalOptions
GetProject applicationDir projectDir project
	=	{ pg_built				= PR_Built project
		, pg_codegen			= PR_GetCodeGenOptions project
		, pg_application		= PR_GetApplicationOptions project
		, pg_projectPaths		= projectPaths
		, pg_link				= linkOptions
		, pg_mainModuleInfo		= mainModuleInfo
		, pg_otherModules		= otherModules
		, pg_staticLibInfo		= staticLibInfo
		, pg_target				= target
		, pg_execpath			= exepath
		, pg_static				= project.static_info
		, pg_dynamic			= project.dynamic_info
		, pg_precompile			= project.prec
		, pg_postlink			= project.posl
		}
where
	exepath
		# xp	= PR_GetExecPath project
		# xp	= symPath applicationDir projectDir xp
		= xp
	mainModuleInfo		=	getModule mainModuleName
	mainModuleName		=	PR_GetRootModuleName project
	(otherModuleNames,project`)	=	PR_GetModulenames False IclMod project
	otherModules		=	Map getModule (Filter ((<>) mainModuleName) otherModuleNames)
	getModule name	
		# info = PR_GetModuleInfo name project
		# info = if (isJust info) (fromJust info) defaultModInfo
		# info = SubstituteModuleInfoPaths applicationDir projectDir info
		= {ModInfoAndName|name = name, info = info}
	linkOptions			=	SubstituteLinkOptionsPaths applicationDir projectDir (PR_GetLinkOptions project)
	projectPaths		=	SubstitutePaths applicationDir projectDir  (PR_GetPaths project)
	staticLibInfo		=	SubstituteStaticLibPaths applicationDir projectDir (PR_GetStaticLibsInfo project)
	target				=	PR_GetTarget project

	defaultModInfo :: ModInfo
	defaultModInfo	=
		{ dir		= EmptyPathname
		, compilerOptions		= DefaultCompilerOptions
		, defeo 	= {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize}
		, impeo	= {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize}
		, defopen = False
		, impopen = False
		, date	= NoDate
		, abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} 
		}
	where
		DefaultEditOptions =
			{ newlines = HostNativeNewlineConvention}
/*			{ tabs = 4
			, fontname = "Courier New"	//NonProportionalFontDef.fName
			, fontsize = 10				//NonProportionalFontDef.fSize
			, autoi = True
			, newlines = HostNativeNewlineConvention
			, showtabs = False
			, showlins = False
			, showsync = True
			} 
*/

//--

ExpandModuleInfoPaths :: {#Char} {#Char} ModInfo -> ModInfo
ExpandModuleInfoPaths applicationDir projectDir moduleInfo=:{dir}
	= {moduleInfo & dir = ExpandPath applicationDir projectDir dir}

ExpandLinkOptionsPaths :: {#Char} {#Char} LinkOptions -> LinkOptions
ExpandLinkOptionsPaths applicationDir projectDir linkOptions=:{extraObjectModules, libraries}
	=	{ linkOptions
		& extraObjectModules	= ExpandPaths applicationDir projectDir extraObjectModules
		, libraries				= ExpandPaths applicationDir projectDir libraries
		}

ExpandStaticLibPaths :: {#Char} {#Char} StaticLibInfo -> StaticLibInfo
ExpandStaticLibPaths applicationDir projectDir staticLibs=:{sLibs}
	=	{ staticLibs
		& sLibs = ExpandPaths applicationDir projectDir sLibs
		}

//ExpandPaths :: {#Char} {#Char} (List {#Char}) -> List {#Char}
ExpandPaths applicationDir projectDir list
	:== fulPaths applicationDir projectDir list

ExpandPath applicationDir projectDir path
	:== fulPath applicationDir projectDir path

SubstituteModuleInfoPaths :: {#Char} {#Char} ModInfo -> ModInfo
SubstituteModuleInfoPaths applicationDir projectDir info
	=	{info & dir = SubstitutePath applicationDir projectDir info.dir}

SubstituteLinkOptionsPaths :: {#Char} {#Char} LinkOptions -> LinkOptions
SubstituteLinkOptionsPaths applicationDir projectDir linkOptions=:{extraObjectModules, libraries}
	=	{linkOptions
		& extraObjectModules = SubstitutePaths applicationDir projectDir extraObjectModules
		, libraries = SubstitutePaths applicationDir projectDir libraries
		}

SubstituteStaticLibPaths :: {#Char} {#Char} StaticLibInfo -> StaticLibInfo
SubstituteStaticLibPaths applicationDir projectDir staticLibs=:{sLibs}
	=	{ staticLibs
		& sLibs = SubstitutePaths applicationDir projectDir sLibs
		}

//SubstitutePaths :: {#Char} {#Char} (List {#Char}) -> List {#Char}
SubstitutePaths applicationDir projectDir list
	:== symPaths applicationDir projectDir list

SubstitutePath applicationDir projectDir path
	:== symPath applicationDir projectDir path

//---

PR_GetABCLinkInfo	:: !Project -> ABCLinkInfo;
PR_GetABCLinkInfo project=:{inflist}
	#	allLinkInfoRecords	= map (\{InfListItem | info={abcLinkInfo}} -> abcLinkInfo) (StrictListToList inflist);
		oneLinkInfoRecord	= foldl mergeTwoRecords emptyRecord allLinkInfoRecords;
	= oneLinkInfoRecord;
where
		mergeTwoRecords { linkObjFileNames=linkObjFileNames1, linkLibraryNames=linkLibraryNames1}
						{ linkObjFileNames=linkObjFileNames2, linkLibraryNames=linkLibraryNames2}
			= { linkObjFileNames	= UnionStringList linkObjFileNames2 linkObjFileNames1,
				linkLibraryNames	= UnionStringList linkLibraryNames2 linkLibraryNames1};
		emptyRecord
			= { linkObjFileNames = Nil, linkLibraryNames	= Nil};

//---

PR_GetStaticLibsInfo :: !Project -> StaticLibInfo
PR_GetStaticLibsInfo {Project | staticLibInfo} = staticLibInfo

PR_SetStaticLibsInfo :: !StaticLibInfo !Project -> Project
PR_SetStaticLibsInfo staticLibInfo project
	= {Project | project & staticLibInfo = staticLibInfo}

PR_GetTarget :: !Project -> String
PR_GetTarget {Project | target} = target

PR_SetTarget :: !String !Project -> Project
PR_SetTarget target project
	| target == project.Project.target
		= project
		= {Project | project & target = target, exec = False, saved = False}

//--

SL_Add :: !String !StaticLibInfo -> StaticLibInfo
SL_Add pathname sl
	// load sDcls and sDeps info...
	// read some kind of libdef file...
	= {sl & sLibs = Append sl.sLibs pathname}

SL_Rem :: ![String] !String !String !StaticLibInfo -> StaticLibInfo
SL_Rem pathsel ap pp sl
	// remove sDcls and sDeps...
	= {sl & sLibs = seq
		[ RemoveStringFromList (fulPath ap pp s)
		\\ s <- pathsel
		] sl.sLibs}

SL_Libs :: !StaticLibInfo -> List String
SL_Libs sl=:{sLibs} = sLibs

SL_Dcls :: !StaticLibInfo -> List String
SL_Dcls sl=:{sDcls} = sDcls

SL_Deps :: !StaticLibInfo -> List String
SL_Deps sl=:{sDeps} = sDeps

SL_SetLibs :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetLibs lp sl = {sl & sLibs = lp}

SL_SetDcls :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDcls lp sl = {sl & sDcls = lp}

SL_SetDeps :: !(List String) !StaticLibInfo -> StaticLibInfo
SL_SetDeps lp sl = {sl & sDeps = lp}

//--

SaveProjectFile	:: !String !Project !String !*Files -> (!Bool, !*Files);
SaveProjectFile	projectPath project applicationDir files
	#! (opened, file, files)		=	fopen projectPath FWriteText files
	| not opened
		=	(False, files)
	#! projectDir			  	=	RemoveFilename projectPath
	#! projectGO				=	GetProject applicationDir projectDir project
	#! options				  	=	PutOptions ProjectTable projectGO
	#! file						=	WriteOptionsFile ProjectFileVersion options file
	=	fclose file files

ReadProjectFile	:: !String !String !*Files -> ((!Project, !Bool, !{#Char}),!*Files)
ReadProjectFile projectPath applicationDir ps
	#	(opened, file, ps)		= fopen projectPath FReadData ps
		emptyProject			= PR_InitProject
		projectName				= RemovePath projectPath
		projectDir				= RemoveFilename projectPath
	| not opened
		= ((emptyProject,False,"The file \"" +++  projectName +++ "\" could not be opened."),ps)
	#	(version, file)			= ReadVersion file
	| version == ""
		#	(_, ps)				= fclose file ps
		=	((emptyProject,False,"The file \"" +++  projectName +++ "\" is an old project and could not be opened."),ps)
	#!	(options, file)			= ReadOptionsFile file
		projectGO				= GetProject applicationDir projectDir emptyProject
		projectGO				= GetOptions ProjectTable options projectGO
		projectGO				= (if (version == "1.3")
									(\p->{p&pg_target="StdEnv"})
									(id)
								) projectGO	// DvA: need to set needs save flag for project;
/*
It's better to replace above with a dialog with popup of available environments.
*/
		unexpanded_exec_path	= projectGO.pg_execpath
		project					= SetProject applicationDir projectDir projectGO
		execpath				= PR_GetExecPath project
		(rootpath,project)		= PR_GetRootPathName project
		project					= PR_SetExecPath (if (size unexpanded_exec_path==0) (MakeExecPathname rootpath) execpath) project
		(closed, ps)			= fclose file ps
	| not closed
		// generate warning?
		=	((project, True,"The file \"" +++ projectName +++ "\" could not be closed."), ps)
	=	((project, True,""), ps)

getStaticInfo :: !Project -> (ProjectStaticInfo,Project)
getStaticInfo prj=:{static_info} = (static_info,prj)

setStaticInfo :: !.ProjectStaticInfo !.Project -> .Project
setStaticInfo inf prj = {prj & static_info = inf}

getDynamicInfo :: !Project -> (ProjectDynamicInfo,Project)
getDynamicInfo prj=:{dynamic_info} = (dynamic_info,prj)

setDynamicInfo :: !.ProjectDynamicInfo !.Project -> .Project
setDynamicInfo inf prj = {prj & dynamic_info = inf}

PR_SetPrecompile	:: !(Maybe String) !Project -> Project
PR_SetPrecompile prec prj = {prj & prec = prec}

PR_GetPrecompile	:: !Project -> (!Maybe String, !Project)
PR_GetPrecompile prj=:{prec} = (prec,prj)

PR_SetPostlink		:: !(Maybe String) !Project -> Project
PR_SetPostlink posl prj = {prj & posl = posl}

PR_GetPostlink		:: !Project -> (!Maybe String, !Project)
PR_GetPostlink prj=:{posl} = (posl,prj)
