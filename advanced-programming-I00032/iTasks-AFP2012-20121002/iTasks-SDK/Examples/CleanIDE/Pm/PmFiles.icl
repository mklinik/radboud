implementation module PmFiles

//	File I/O routines for the project.

import StdArray, StdFunc, StdInt
import UtilNewlinesFile, UtilOptions, UtilStrictLists
import PmProject
import UtilDate
from PmPath import convert_path_separators,convert_exec_path_separators_and_extension

ProjectFileVersion :== "1.4"

:: ProjectGlobalOptions =
	{ pg_built				:: !Bool
	, pg_codegen			:: CodeGenOptions
	, pg_application		:: ApplicationOptions
//	, pg_projectOptions		:: ProjectOptions
	, pg_link				:: LinkOptions
	, pg_projectPaths		:: List Pathname
	, pg_otherModules		:: List ModInfoAndName
	, pg_mainModuleInfo		:: ModInfoAndName
	, pg_staticLibInfo		:: StaticLibInfo
	, pg_target				:: String
	, pg_execpath			:: String
	, pg_static				:: !ProjectStaticInfo
	, pg_dynamic			:: !ProjectDynamicInfo
//	, pg_generateDLL		:: !Bool
//	, pg_exportedDLL		:: !String
	, pg_precompile			:: !Maybe String
	, pg_postlink			:: !Maybe String
	}

:: ProjectStaticInfo =
	{ stat_mods				:: !List Pathname
	, stat_objs				:: !List Pathname
	, stat_slibs			:: !List Pathname
	, stat_dlibs			:: !List Pathname
	, stat_paths			:: !List Pathname
	, stat_app_path			:: !Pathname
	, stat_prj_path			:: !Pathname
	}

EmptyStaticInfo :: ProjectStaticInfo
EmptyStaticInfo =
	{ stat_mods				= Nil
	, stat_objs				= Nil
	, stat_slibs			= Nil
	, stat_dlibs			= Nil
	, stat_paths			= Nil
	, stat_app_path			= ""
	, stat_prj_path			= ""
	}

:: ProjectDynamicInfo =
	{ dyn_syms				:: !List UndefSymbol
	, dyn_mods				:: !List UndefModule
	, dyn_objs				:: !List Pathname
	, dyn_slibs				:: !List Pathname
	, dyn_dlibs				:: !List Pathname
	, dyn_paths				:: !List Pathname
	}

EmptyDynamicInfo :: ProjectDynamicInfo
EmptyDynamicInfo =
	{ dyn_syms				= Nil
	, dyn_mods				= Nil
	, dyn_objs				= Nil
	, dyn_slibs				= Nil
	, dyn_dlibs				= Nil
	, dyn_paths				= Nil
	}


:: UndefSymbol =
	{ symbol_name	:: !String
	, path			:: !String
	}
 
:: UndefModule =
	{ module_name	:: !String
	, path			:: !String
	}

EmptyUndefSymbol	:: UndefSymbol
EmptyUndefSymbol =
	{ symbol_name	= ""
	, path			= ""
	}
 
EmptyUndefModule	:: UndefModule
EmptyUndefModule =
	{ module_name	= ""
	, path			= ""
	}

//--

ProjectGlobalOptionsTable :: OptionsTable ProjectGlobalOptions
ProjectGlobalOptionsTable =
	{	SimpleOption	"Built"									(\a->a.pg_built)			(\v a->{a & pg_built=v})
	,	SimpleOption	"Target"								(\a->a.pg_target)			(\v a->{a & pg_target=v})
	,	SimpleWithStringConversionOption convert_exec_path_separators_and_extension "Exec" (\a->a.pg_execpath) (\v a->{a & pg_execpath=v})
	,	GroupedOption	"CodeGen"		CodeGenOptionsTable		(\a->a.pg_codegen)			(\v a->{a & pg_codegen=v})
	,	GroupedOption	"Application"	ApplicationOptionsTable	(\a->a.pg_application)		(\v a->{a & pg_application=v})
//	,	GroupedOption	"Project"		ProjectOptionsTable		(\a->a.pg_projectOptions)	(\v a->{a & pg_projectOptions=v})
	,	GroupedOption	"Link"			LinkOptionsTable		(\a->a.pg_link)				(\v a->{a & pg_link=v})
	,	ListOption		"Paths"			PathName ""				(\a->a.pg_projectPaths)		(\v a->{a & pg_projectPaths=v})
	,	GroupedOption	"Static"		StaticLibsInfoTable		(\a->a.pg_staticLibInfo)	(\v a->{a & pg_staticLibInfo=v})
	,	SimpleOption	"Precompile"							(\a->unwrap a.pg_precompile)(\v a->{a & pg_precompile = wrap v})
	,	SimpleOption	"Postlink"								(\a->unwrap a.pg_postlink)	(\v a->{a & pg_postlink = wrap v})
	}
where
	// Making Precompile & Postlink 'List' options is probably prettier...
	unwrap Nothing = ""
	unwrap (Just s) = s
	
	wrap "" = Nothing
	wrap s = Just s

instance fromString Bool
where
	fromString "False"
		=	False
	fromString _ 
		=	True


ModInfoAndNameTable :: OptionsTable ModInfoAndName
ModInfoAndNameTable =
	{ SimpleOption	"Name"									(\a->a.ModInfoAndName.name)	(\v a->{ModInfoAndName|a & name=v})
	, SimpleWithStringConversionOption convert_path_separators "Dir" (\a->a.info.dir)	(\v a->{a & info.dir=v})
	, GroupedOption	"Compiler"		CompilerOptionsTable	(\a->a.info.compilerOptions)(\v a->{a & info.compilerOptions=v})
	, GroupedOption	"Dcl"			EditWdOptionsTable		(\a->a.info.defeo)			(\v a->{a & info.defeo=v})
	, SimpleOption	"DclOpen"								(\a->a.info.defopen)		(\v a->{a & info.defopen=v})
	, GroupedOption	"Icl"			EditWdOptionsTable		(\a->a.info.impeo)			(\v a->{a & info.impeo=v})
	, SimpleOption	"IclOpen"								(\a->a.info.impopen)		(\v a->{a & info.impopen=v})
	, SimpleOption	"LastModified"							(\a->a.info.date)			(\v a->{a & info.date=v})
	, ListOption	"NeededObjFiles" ObjectFile ""			(\a->a.info.abcLinkInfo.linkObjFileNames) (\v a->{a & info.abcLinkInfo.linkObjFileNames=v})
	, ListOption	"NeededLibraries" Library   ""			(\a->a.info.abcLinkInfo.linkLibraryNames) (\v a->{a & info.abcLinkInfo.linkLibraryNames=v}) 
	}

ModInfoAndNameEntry =
		GroupedOption "Module" ModInfoAndNameTable id const

ProjectTable :: OptionsTable ProjectGlobalOptions
ProjectTable =	// +++ order is important here
	{	GroupedOption	"Global"		ProjectGlobalOptionsTable	id							const
	,	GroupedOption	"MainModule"	ModInfoAndNameTable			(\a->a.pg_mainModuleInfo)	(\v a->{a & pg_mainModuleInfo=v})
	,	ListOption		"OtherModules"	ModInfoAndNameEntry 		{info=EmptyModInfo,name=""}	(\a->a.pg_otherModules)	(\v a->{a & pg_otherModules=v})
	,	GroupedOption	"Static"		StaticInfoTable				(\a->a.pg_static)			(\v a->{a & pg_static=v})
	,	GroupedOption	"Dynamic"		DynamicInfoTable			(\a->a.pg_dynamic)			(\v a->{a & pg_dynamic=v})
	}
where
	EmptyModInfo :: ModInfo
	EmptyModInfo = {	dir		= EmptyPathname,
					compilerOptions		= DefaultCompilerOptions,
					defeo 	= {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize},
					impeo	= {eo=DefaultEditOptions,pos_size=NoWindowPosAndSize},
					defopen = False,
					impopen = False,
					date	= NoDate,
					abcLinkInfo = {linkObjFileNames = Nil, linkLibraryNames = Nil} }
	DefaultEditOptions :: EditOptions;
	DefaultEditOptions =
		{	newlines	= HostNativeNewlineConvention
/*		{	tabs		= 4
		,	fontname	= "Courier"
		,	fontsize	= 9
		,	autoi		= True
		,	newlines	= HostNativeNewlineConvention
		,	showtabs	= False
		,	showlins	= False
		,	showsync	= True
*/		}


CompilerOptionsTable :: OptionsTable CompilerOptions
CompilerOptionsTable =
	{
		SimpleOption "NeverMemoryProfile"	(\a->a.neverMemoryProfile)	(\v a->{a & neverMemoryProfile=v}),
		SimpleOption "NeverTimeProfile"		(\a->a.neverTimeProfile)	(\v a->{a & neverTimeProfile=v}),
		SimpleOption "StrictnessAnalysis"	(\a->a.sa)					(\v a->{a & sa=v}),
		SimpleOption "ListTypes"			(\a->a.listTypes)			(\v a->{a & listTypes=v}),
		SimpleOption "ListAttributes"		(\a->a.attr)				(\v a->{a & attr=v}),
		SimpleOption "Warnings"				(\a->a.gw)					(\v a->{a & gw=v}),
		SimpleOption "Verbose"				(\a->a.bv)					(\v a->{a & bv=v}),
		SimpleOption "ReadableABC"			(\a->a.gc)					(\v a->{a & gc=v}),
		SimpleOption "ReuseUniqueNodes"		(\a->a.reuseUniqueNodes)	(\v a->{a & reuseUniqueNodes=v}),
		SimpleOption "Fusion"				(\a->a.fusion)				(\v a->{a & fusion=v})
	}

CodeGenOptionsTable :: OptionsTable CodeGenOptions
CodeGenOptionsTable	=
	{
		SimpleOption "CheckStacks"		(\a->a.cs)		(\v a->{a & cs=v}),
		SimpleOption "CheckIndexes" 	(\a->a.ci)		(\v a->{a & ci=v})
//		SimpleOption "KeepABC" 			(\a->a.kaf)		(\v a->{a & kaf=v}),
//		SimpleOption "TargetProcessor"	(\a->a.tp)		(\v a->{a & tp=v})
	}

instance fromString Int
where
	fromString s
		=	toInt s

ApplicationProfiletOptionsTable :: OptionsTable ApplicationOptions
ApplicationProfiletOptionsTable	=
	{
		SimpleOption "Memory"					(\a->a.memoryProfiling)					(\v a->{a & memoryProfiling=v}),
		SimpleOption "MemoryMinimumHeapSize"	(\a->a.memoryProfilingMinimumHeapSize)	(\v a->{a & memoryProfilingMinimumHeapSize=v}),
		SimpleOption "Time"						(\a->a.profiling)						(\v a->{a & profiling=v}),
		SimpleOption "Stack"					(\a->a.stack_traces)					(\v a->{a & stack_traces=v})
	}

ApplicationOutputOptionsTable :: OptionsTable ApplicationOptions
ApplicationOutputOptionsTable	=
	{
		SimpleOption "Output"		(\a->a.o)						(\v a->{a & o=v}),
		SimpleOption "Font"			(\a->a.fn)						(\v a->{a & fn=v}),
		SimpleOption "FontSize"		(\a->a.fs)						(\v a->{a & fs=v}),
		SimpleOption "WriteStdErr"	(\a->a.write_stderr_to_file)	(\v a->{a & write_stderr_to_file=v})
	}

ApplicationOptionsTable :: OptionsTable ApplicationOptions
ApplicationOptionsTable	=
	{
		SimpleOption "HeapSize"						(\a->a.hs)					(\v a->{a & hs=v}),
		SimpleOption "StackSize"					(\a->a.ss)					(\v a->{a & ss=v}),
		SimpleOption "ExtraMemory"					(\a->a.em)					(\v a->{a & em=v}),
		SimpleOption "IntialHeapSize"				(\a->a.initial_heap_size)	(\v a->{a & initial_heap_size=v}),
		SimpleOption "HeapSizeMultiplier"			(\a->a.heap_size_multiple)	(\v a->{a & heap_size_multiple=v}),
		SimpleOption "ShowExecutionTime"			(\a->a.set)					(\v a->{a & set=v}),
		SimpleOption "ShowGC"						(\a->a.sgc)					(\v a->{a & sgc=v}),
		SimpleOption "ShowStackSize"				(\a->a.pss)					(\v a->{a & pss=v}),
		SimpleOption "MarkingCollector"				(\a->a.marking_collection)	(\v a->{a & marking_collection=v}),
		SimpleOption "StandardRuntimeEnv"			(\a->a.standard_rte)		(\v a->{a & standard_rte=v}),
		GroupedOption "Profile"	ApplicationProfiletOptionsTable	id				const,
		GroupedOption "Output"	ApplicationOutputOptionsTable	id				const
	}
/*
ProjectOptionsTable :: OptionsTable ProjectOptions
ProjectOptionsTable =
	{
		SimpleOption "Verbose"	(\a->a.ProjectOptions.verbose)	(\v a->{ProjectOptions | a & verbose=v})
	}
*/

PathName :: OptionsTableEntry {#Char}
PathName
	=	SimpleWithStringConversionOption convert_path_separators "Path"	id const

ModuleName :: OptionsTableEntry {#Char}
ModuleName
	=	SimpleOption "Module" id const

ObjectFile :: OptionsTableEntry {#Char}
ObjectFile
	=	SimpleOption "ObjectFile" id const

Library :: OptionsTableEntry {#Char}
Library
	=	SimpleOption "Library" id const

StaticLibsInfoTable :: OptionsTable StaticLibInfo
StaticLibsInfoTable =
	{ ListOption	"StaticLibs"			PathName ""		(\a->SL_Libs a)				(\v a->SL_SetLibs v a)
	, ListOption	"StaticDcls"			ModuleName ""	(\a->SL_Dcls a)				(\v a->SL_SetDcls v a)
	, ListOption	"StaticDeps"			ModuleName ""	(\a->SL_Deps a)				(\v a->SL_SetDeps v a)
	}

LinkOptionsTable :: OptionsTable LinkOptions
LinkOptionsTable	=
	{ ListOption	"ExtraObjects"			PathName ""	(\a->a.extraObjectModules)		(\v a->{a & extraObjectModules=v})
	, ListOption	"ExtraLibraries"		PathName ""	(\a->a.libraries)				(\v a->{a & libraries=v})
	, SimpleOption	"LinkMethod"						(\a->a.method)					(\v a->{a & method = v})
	, SimpleOption	"GenerateRelocations"				(\a->a.generate_relocations)	(\v a->{a & generate_relocations = v})
	, SimpleOption	"GenerateLinkMap"					(\a->a.generate_link_map)		(\v a->{a & generate_link_map = v})
	, SimpleOption	"LinkResources"						(\a->a.link_resources)			(\v a->{a & link_resources = v})
	, SimpleOption	"ResourceSource"					(\a->a.resource_source)			(\v a->{a & resource_source = v})
	, SimpleOption	"GenerateDLL"						(\a->a.generate_dll)			(\v a->{a & generate_dll = v})
	, SimpleOption	"ExportedNames"						(\a->a.dll_export_list_name)	(\v a->{a & dll_export_list_name = v})
//	, SimpleOption	"AddCarbResource"					(\a->a.add_carb_resource)		(\v a->{a & add_carb_resource = v})
	}

EditWdOptionsTable :: OptionsTable EditWdOptions
EditWdOptionsTable	=
	{
		GroupedOption "Editor"			EditOptionsTable	(\a->a.eo)			(\v a->{a & eo=v}),
		OptionalGroupedOption "WindowPosition" is_no_position_and_size WindowPositionTable (\a->a.pos_size) (\v a->{a & pos_size=v})
	}

is_no_position_and_size NoWindowPosAndSize
	= True
is_no_position_and_size _
	= False

EditOptionsTable :: OptionsTable EditOptions
EditOptionsTable	=
	{}
/*	{ SimpleOption "TabSize"		(\a->a.EditOptions.tabs)		(\v a->{EditOptions | a & tabs=v})
	, SimpleOption "Font"			(\a->a.EditOptions.fontname)	(\v a->{EditOptions | a & fontname=v})
	, SimpleOption "FontSize"		(\a->a.EditOptions.fontsize)	(\v a->{EditOptions | a & fontsize=v})
	, SimpleOption "AutoIndent"	(\a->a.EditOptions.autoi)		(\v a->{EditOptions | a & autoi=v})
	, SimpleOption "ShowTabs"	(\a->a.EditOptions.showtabs)		(\v a->{EditOptions | a & showtabs=v})
	, SimpleOption "ShowLins"	(\a->a.EditOptions.showlins)		(\v a->{EditOptions | a & showlins=v})
	}
*/
WindowPositionTable :: OptionsTable OptionalWindowPosAndSize
WindowPositionTable	=
	{
		SimpleOption "X"		(\a->(getWindowPosAndSize a).posx)	(\v a->WindowPosAndSize {getWindowPosAndSize a & posx=v}),
		SimpleOption "Y"		(\a->(getWindowPosAndSize a).posy)	(\v a->WindowPosAndSize {getWindowPosAndSize a & posy=v}),
		SimpleOption "SizeX"	(\a->(getWindowPosAndSize a).sizex)	(\v a->WindowPosAndSize {getWindowPosAndSize a & sizex=v}),
		SimpleOption "SizeY"	(\a->(getWindowPosAndSize a).sizey)	(\v a->WindowPosAndSize {getWindowPosAndSize a & sizey=v})
	}

getWindowPosAndSize (WindowPosAndSize wps) = wps
getWindowPosAndSize NoWindowPosAndSize = DefWindowPos_and_Size

StaticInfoTable =
	{ ListOption	"Mods"	PathName ""	(\a->a.stat_mods)	(\v a->{a & stat_mods=v})
	, ListOption	"Objs"	PathName ""	(\a->a.stat_objs)	(\v a->{a & stat_objs=v})
	, ListOption	"Slib"	PathName ""	(\a->a.stat_slibs)	(\v a->{a & stat_slibs=v})
	, ListOption	"Dlib"	PathName ""	(\a->a.stat_dlibs)	(\v a->{a & stat_dlibs=v})
	, ListOption	"Pths"	PathName ""	(\a->a.stat_paths)	(\v a->{a & stat_paths=v})
	, SimpleOption	"AppP"			(\a->a.stat_app_path)	(\v a->{a & stat_app_path=v})
	, SimpleOption	"PrjP"			(\a->a.stat_prj_path)	(\v a->{a & stat_prj_path=v})
	}

DynamicInfoTable =
	{ ListOption	"Syms"	Usym esym	(\a->a.dyn_syms)	(\v a->{a & dyn_syms=v})
	, ListOption	"Mods"	Umod emod	(\a->a.dyn_mods)	(\v a->{a & dyn_mods=v})
	, ListOption	"Objs"	PathName ""	(\a->a.dyn_objs)	(\v a->{a & dyn_objs=v})
	, ListOption	"Slib"	PathName ""	(\a->a.dyn_slibs)	(\v a->{a & dyn_slibs=v})
	, ListOption	"Dlib"	PathName ""	(\a->a.dyn_dlibs)	(\v a->{a & dyn_dlibs=v})
	, ListOption	"Pths"	PathName ""	(\a->a.dyn_paths)	(\v a->{a & dyn_paths=v})
	}

esym = {symbol_name = "", path = ""}
emod = {module_name = "", path = ""}

Usym = GroupedOption "usym" USTable id const
Umod = GroupedOption "umod" UMTable id const

USTable =
	{ SimpleOption "name" (\a->a.symbol_name)		(\v a -> {a & symbol_name = v})
	, SimpleOption "path" (\a->a.UndefSymbol.path)	(\v a -> {UndefSymbol | a & path = v})
	}
 
UMTable =
	{ SimpleOption "name" (\a->a.module_name)		(\v a -> {a & module_name = v})
	, SimpleOption "path" (\a->a.UndefModule.path)	(\v a -> {UndefModule | a & path = v})
	}
