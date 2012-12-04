module main

import scanner, parse, postparse, check, type, trans, convertcases, utilities, convertDynamics

import StdEnv
// RWS ...
import frontend
// ... RWS

// MV ...
from type_io import openTclFile, closeTclFile
// ... MV

write_tcl_file yes no :== yes;

Start world
	# (std_io, world) = stdio world
	  (_, ms_out, world) = fopen "out" FWriteText world
	  (symbol_table,world) = init_identifiers newHeap world
	  (ms_out,world) = accFiles (
	  		\files -> 
				(let
				    (ms_paths, ms_files, ms_error) = converFileToListOfStrings "mainPrefs" files stderr
					ms = CommandLoop symbol_table { ms_io = std_io, ms_out = ms_out, ms_error = ms_error, ms_files = ms_files, ms_paths = ms_paths }
				in
					(ms.ms_out, ms.ms_files))) world
	= fclose ms_out world

CommandLoop symbol_heap ms=:{ms_io}
//	# (answer, ms_io)		= freadline (ms_io <<< "> ")
	# (answer, ms_io)		= ("c abstract",ms_io)
	  (command, argument)	= SplitAtLayoutChar (dropWhile isSpace (fromString answer))
	| command == []
		= CommandLoop symbol_heap { ms & ms_io = ms_io}
		# (ready, symbol_heap, ms) = DoCommand command argument symbol_heap { ms & ms_io = ms_io}
		| ready
			= ms
			= ms
//			= CommandLoop symbol_heap ms

::	MainStateDefs funs funtypes types conses classes instances members selectors =
	{	msd_funs		:: !funs
	,	msd_funtypes	:: !funtypes
	,	msd_types		:: !types
	,	msd_conses		:: !conses
	,	msd_classes		:: !classes
	,	msd_instances	:: !instances
	,	msd_members		:: !members
	,	msd_selectors	:: !selectors
	,	msd_genfuns		:: ![FunDef]
	}
	

::	*MainState =
	{	ms_io			:: !*File
	,	ms_error		:: !*File
	,	ms_out			:: !*File
	,	ms_paths		:: ![{#Char}]
	,	ms_files		:: !*Files
	}	

::	InterMod =
	{	inter_name					:: !String
	,	inter_modules				:: !{# String}
/*	,	inter_fun_defs				:: !{# FunDef}
	,	inter_icl_dcl_conversions	:: !Optional {# Index}
*/
	}

::	ModuleTree = ModuleNode !String !ModuleTree !ModuleTree | NoModules

containsModule name (ModuleNode inter_name left right)
	| inter_name == name
		= True
	| inter_name < name
		= containsModule name right
		= containsModule name left
containsModule name NoModules
	= False

addModule name mod tree=:(ModuleNode this_mod left right)
	| this_mod == name
		= tree
	| this_mod < name
		= ModuleNode this_mod left (addModule name mod right)
		= ModuleNode this_mod (addModule name mod left) right
addModule _ mod NoModules
	= ModuleNode mod NoModules NoModules

:: DclCache = {
	dcl_modules::!{#DclModule},
	cached_macros::!.{#.{#FunDef}},
	predef_symbols::!.PredefinedSymbols,
	hash_table::!.HashTable,
	heaps::!.Heaps
 };

::	Project =
	{	proj_main_module	:: !String
	,	proj_modules		:: !ModuleTree
	,	proj_cache			:: !.DclCache
	}

empty_cache :: *SymbolTable -> *DclCache
empty_cache symbol_heap
	# heaps = {hp_var_heap = newHeap, hp_expression_heap = newHeap, hp_type_heaps = {th_vars = newHeap, th_attrs = newHeap}, hp_generic_heap = newHeap}
	# (predef_symbols, hash_table) = buildPredefinedSymbols (newHashTable symbol_heap)
	= {dcl_modules={},cached_macros={},predef_symbols=predef_symbols,hash_table=hash_table,heaps=heaps}

DoCommand ['c':_] argument symbol_heap ms 
	# (file_name, rest_input) = SplitAtLayoutChar (dropWhile isSpace argument)
	  (opt_mod,dcl_cache,ms) = compileModule (toString file_name) (empty_cache symbol_heap) ms
	= (False, dcl_cache.hash_table.hte_symbol_heap, ms)

DoCommand ['m':_] argument symbol_heap ms 
	# (file_name, rest_input) = SplitAtLayoutChar (dropWhile isSpace argument)
	# mod_ident = toString file_name
	# dcl_cache=empty_cache symbol_heap
  	# (proj, ms) = makeProject { proj_main_module=mod_ident,
  									proj_modules=NoModules,
  									proj_cache=dcl_cache} ms
	= (False, proj.proj_cache.hash_table.hte_symbol_heap, ms)

DoCommand ['s':_] argument symbol_heap ms=:{ms_io, ms_files} 
	# (file_name, rest_input)	= SplitAtLayoutChar (dropWhile isSpace argument)
	  file_name 				= toString (file_name++['.icl'])
	  (ok,file,files)			= fopen file_name FReadText ms_files
	  (lines,file)				= freadlines file
	  (ok,files)				= fclose file files
	= (False, symbol_heap, {ms & ms_io = ms_io <<< ("file "+++file_name+++" "+++toString (length lines)+++" lines\n") <<< lines <<< "\n", ms_files = files})

DoCommand ['t':_] argument symbol_heap ms=:{ms_files, ms_io}
	# (file_names, ms_files, ms_io) = converFileToListOfStrings "testfiles" ms_files ms_io
	# (dcl_cache,ms) = foldSt check_module file_names ((empty_cache symbol_heap),{ ms & ms_files = ms_files, ms_io = ms_io })
	= (False, dcl_cache.hash_table.hte_symbol_heap, ms)
where
	check_module file_name (dcl_cache,ms)
		# ms = {ms & ms_io = ms.ms_io <<< "Compiling " <<< file_name <<< "\n"}
  		# (opt_mod, dcl_cache,ms) = compileModule file_name dcl_cache ms
		= case opt_mod of
			No
				-> (dcl_cache,{ ms & ms_io = ms.ms_io <<< file_name <<< " is not OK\n" })
			_
				-> (dcl_cache,ms)

DoCommand ['q':_] argument symbol_heap ms
	= (True, symbol_heap, ms)

DoCommand ['h':_] argument symbol_heap  ms=:{ms_io}
	= (False, symbol_heap, {ms & ms_io = ms_io <<< "No help available. Sorry.\n"})

DoCommand command argument symbol_heap  ms=:{ms_io}
	= (False, symbol_heap, {ms & ms_io = ms_io <<< toString command <<< "?\n"})

freadlines file
    |   sfend file
        =   ([],file)
	    #   (line, file)    = freadline file
   		#   (lines,file)    = freadlines file
        =   ([line:lines],file)

SplitAtLayoutChar [] = ([], [])
SplitAtLayoutChar [x:xs]
	| x == ' ' || x == '\t' || x == '\n'
		= ([], xs)
	| otherwise
		= ([x:word], rest_input)
where
	(word, rest_input) = SplitAtLayoutChar xs

compileModule :: String *DclCache *MainState -> *(!Optional InterMod,!*DclCache,!*MainState);
compileModule mod_ident dcl_cache ms
	# (mod_ident, hash_table) = putIdentInHashTable mod_ident IC_Module dcl_cache.hash_table
	  dcl_cache = {dcl_cache & hash_table=hash_table}
	= loadModule mod_ident.boxed_ident dcl_cache ms

dummyModTime :: {#Char} .f -> ({#Char}, .f)
dummyModTime _ f
	=	("", f)

loadModule :: Ident *DclCache *MainState -> *(!Optional InterMod,!*DclCache,!*MainState);
loadModule mod_ident {dcl_modules,cached_macros,predef_symbols,hash_table,heaps} ms=:{ms_files,ms_error,ms_io,ms_out,ms_paths}
// MV ...
	# (tcl_file,ms=:{ms_files,ms_error,ms_io,ms_out,ms_paths})
		= write_tcl_file (WrapopenTclFile ms) (No,ms);
// ... MV
	# (optional_syntax_tree,cached_cached_macros,cached_dcl_mods,_,main_dcl_module_n,predef_symbols, hash_table, ms_files, ms_error, ms_io, ms_out,tcl_file,heaps)
		= frontEndInterface { feo_dump_core = False, feo_strip_unused = False,feo_up_to_phase = FrontEndPhaseAll, feo_generics = False, feo_fusion = False} mod_ident {sp_locations = [], sp_paths = ms_paths} dcl_modules cached_macros No predef_symbols hash_table dummyModTime ms_files ms_error ms_io ms_out tcl_file heaps
// MV ...
	# (_,ms_files)
		= closeTclFile tcl_file ms_files 
// ... MV
	# ms = {ms & ms_files=ms_files, ms_error=ms_error,ms_io=ms_io,ms_out=ms_out}
	= case optional_syntax_tree of
		Yes {fe_icl={/*icl_functions,*/icl_used_module_numbers}, fe_dcls}
			# dcl_modules={{dcl_module \\ dcl_module<-:cached_dcl_mods} & [main_dcl_module_n].dcl_macro_conversions=No}
			# var_heap = remove_expanded_types_from_dcl_modules 0 dcl_modules icl_used_module_numbers heaps.hp_var_heap
			# heaps = {heaps & hp_var_heap = var_heap }
			->	(Yes (buildInterMod mod_ident icl_used_module_numbers fe_dcls),
					{dcl_modules=dcl_modules,cached_macros=cached_cached_macros,predef_symbols=predef_symbols,hash_table=hash_table,heaps=heaps}, ms)
		No
			->	(No, {dcl_modules=dcl_modules,cached_macros=cached_cached_macros,predef_symbols=predef_symbols,hash_table=hash_table,heaps=heaps},ms)
where
	WrapopenTclFile ms=:{ms_files}
		# (tcl_file,ms_files)
			= openTclFile True "test" ms_files
		= (tcl_file,{ms & ms_files = ms_files});

remove_expanded_types_from_dcl_modules :: Int {#DclModule} NumberSet *VarHeap -> *VarHeap
remove_expanded_types_from_dcl_modules module_n dcls used_module_numbers var_heap
	| module_n<size dcls
		| module_n==cPredefinedModuleIndex || not (inNumberSet module_n used_module_numbers)
			= remove_expanded_types_from_dcl_modules (module_n+1) dcls used_module_numbers var_heap
			# var_heap = remove_expanded_types_from_dcl_module 0 dcls.[module_n].dcl_functions var_heap
				with
					remove_expanded_types_from_dcl_module :: Int {#FunType} *VarHeap -> *VarHeap
					remove_expanded_types_from_dcl_module function_n dcl_functions var_heap
						| function_n<size dcl_functions
							# {ft_type_ptr} = dcl_functions.[function_n]
							# (ft_type,var_heap) = readPtr ft_type_ptr var_heap
							= case ft_type of
								VI_ExpandedType expandedType
									# var_heap = writePtr ft_type_ptr VI_Empty var_heap
									-> remove_expanded_types_from_dcl_module (function_n+1) dcl_functions var_heap
								_
									-> remove_expanded_types_from_dcl_module (function_n+1) dcl_functions var_heap
							= var_heap
			= remove_expanded_types_from_dcl_modules (module_n+1) dcls used_module_numbers var_heap
		= var_heap

choose_random_module random_n modules
	# n_modules = length modules;
	# module_n = toInt (random_n*toReal n_modules)
	# module_n = if (module_n<0) 0 (if (module_n>=n_modules) (n_modules-1) module_n)
	# r = find_and_remove_module 0 modules;
		with
		find_and_remove_module n [modjule:modules]
			| n==module_n
				= (modjule,modules);
				# (found_module,modules) = find_and_remove_module (n+1) modules;
				= (found_module,[modjule:modules]);
	= r;

//import MersenneTwister

makeProject :: *Project *MainState -> *(!*Project,!*MainState);
makeProject proj=:{proj_main_module,proj_cache} ms
	# (main_mod,dcl_cache,ms) = compileModule proj_main_module proj_cache ms
	# proj = {proj & proj_cache=dcl_cache}
	= case main_mod of
		Yes main_mod=:{inter_modules}
//			# random_numbers = genRandReal 100;
			# random_numbers = []
			# (proj_modules,proj,ms) = collect_modules [ mod \\ mod <-: inter_modules ] (ModuleNode main_mod.inter_name NoModules NoModules) random_numbers proj ms
			-> ({ proj & proj_modules = proj_modules }, ms)
		_
			-> (proj,ms)
where
	collect_modules :: [String] ModuleTree [Real] *Project *MainState -> *(!ModuleTree,!*Project,!*MainState);
	collect_modules [] collected_modules random_numbers proj ms
		= (collected_modules,proj,ms)
	collect_modules [id_name : modules] collected_modules random_numbers proj ms
//	collect_modules modules collected_modules [random_number:random_numbers] proj ms
//		# (id_name,modules) = choose_random_module random_number modules
		| id_name=="_predefined"
			= collect_modules modules collected_modules random_numbers proj ms
		| containsModule id_name collected_modules
			= collect_modules modules collected_modules random_numbers proj ms
			# ms = {ms & ms_io = ms.ms_io <<< "Compiling " <<< id_name <<< "\n"}
			# dcl_cache = proj.proj_cache
//			# dcl_cache = (empty_cache proj.proj_cache.hash_table.hte_symbol_heap)
			# (this_mod,dcl_cache,ms) = compileModule id_name dcl_cache ms
			# proj = {proj & proj_cache=dcl_cache}
			= case this_mod of
				Yes new_mod
					# collected_modules = addModule id_name new_mod.inter_name collected_modules
					# modules = modules ++ [ mod \\ mod <-: new_mod.inter_modules | not (containsModule mod collected_modules) && not (isMember mod modules)]
					-> collect_modules modules collected_modules random_numbers proj ms
				_
					# ms = {ms & ms_io = ms.ms_io <<< "Compiling " <<< id_name <<< " failed \n"}
					# proj = {proj & proj_cache=empty_cache proj.proj_cache.hash_table.hte_symbol_heap}
					-> collect_modules modules collected_modules random_numbers proj ms
//					-> (NoModules, ms)

buildInterMod name icl_used_module_numbers dcl_modules // fun_defs dcl_icl_conversions icl_dcl_conversions
	# used_dcl_modules = [modjule \\ modjule <-: dcl_modules & module_n<-[0..] | inNumberSet module_n icl_used_module_numbers ]
	=	{	inter_name					= name.id_name
		,	inter_modules				= { dcl_name.id_name \\ {dcl_name} <- used_dcl_modules }
/*
		,	inter_fun_defs				= fun_defs
		,	inter_icl_dcl_conversions	= icl_dcl_conversions
*/
		}

/* RWS			
showComponents :: !*{! Group} !Int !Bool !*{# FunDef} !*File  -> (!*{! Group}, !*{# FunDef},!*File)
showComponents comps comp_index show_types fun_defs file
	| comp_index >= size comps
		= (comps, fun_defs, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, file) = show_component comp.group_members show_types fun_defs (file <<< "component " <<< comp_index <<< '\n')
		= showComponents comps (inc comp_index) show_types fun_defs file
where
	show_component [] show_types fun_defs file
		= (fun_defs, file <<< '\n')
	show_component [fun:funs] show_types fun_defs file
		#! fun_def = fun_defs.[fun]
		| show_types
			= show_component funs show_types fun_defs (file <<< '\n' <<< fun_def)
			= show_component funs show_types fun_defs (file <<< fun_def)
//		= show_component funs show_types fun_defs (file <<< fun_def.fun_ident)

showComponents2 :: !{! Group} !Int !*{# FunDef} !{! ConsClasses} !*File  -> (!*{# FunDef},!*File)
showComponents2 comps comp_index fun_defs acc_args file
	| comp_index >= (size comps)
		= (fun_defs, file)
	# (fun_defs, file) = show_component comps.[comp_index].group_members fun_defs acc_args file
	= showComponents2 comps (inc comp_index) fun_defs acc_args file
where
	show_component [] fun_defs _ file
		= (fun_defs, file <<< '\n')
	show_component [fun:funs] fun_defs acc_args file
		#! fd = fun_defs.[fun]
		# file = show_accumulating_arguments acc_args.[fun].cc_args (file <<< fd.fun_ident <<< '.' <<< fun <<< " (")
		= show_component funs fun_defs acc_args (file <<< ") ")
	
	show_accumulating_arguments [ cc : ccs] file
		| cc == cPassive
			= show_accumulating_arguments ccs (file <<< 'p')
		| cc == cActive
			= show_accumulating_arguments ccs (file <<< 'c')
		| cc == cAccumulating
			= show_accumulating_arguments ccs (file <<< 'a')
			= show_accumulating_arguments ccs (file <<< '?')
	show_accumulating_arguments [] file
		= file


show_components comps fun_defs = map (show_component fun_defs) comps

show_component fun_defs [] = []
show_component fun_defs [fun:funs] = [fun_defs.[fun ---> fun] : show_component fun_defs funs]

showTypes :: !*{! Group} !Int !*{# FunDef} !*File  -> (!*{! Group}, !*{# FunDef},!*File)
showTypes comps comp_index fun_defs file
	| comp_index >= size comps
		= (comps, fun_defs, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, file) = show_types comp.group_members fun_defs (file <<< "component " <<< comp_index <<< '\n')
		= showTypes comps (inc comp_index) fun_defs file
where
	show_types [] fun_defs file
		= (fun_defs, file <<< '\n')
	show_types [fun:funs] fun_defs file
		#! fun_def = fun_defs.[fun]
		# properties = { form_properties = cAttributed bitor cAnnotated, form_attr_position = No }
		  (Yes ftype) = fun_def.fun_type
		= show_types funs fun_defs (file <<< fun_def.fun_ident <<< " :: " <:: (properties, ftype) <<< '\n' )
*/

converFileToListOfStrings file_name files error
	# (ok, file, files) = fopen file_name FReadText files
	| ok
		# (lines, file) = read_lines file
		= (lines, snd (fclose file files), error)
		= ([], files, error <<< "Could not open \"" <<< file_name <<< "\"\n")
where
	read_lines file
		# (line, file) = freadline file
		  last_char_index = size line - 1
		| last_char_index < 0
			= ([], file)
		| line.[last_char_index] == '\n'
			| last_char_index == 0 || line.[0] == '|'
				= read_lines file
				# (lines, file) = read_lines file
				= ([line % (0, last_char_index - 1) : lines ], file)
		// otherwise
			= ([line], file)
