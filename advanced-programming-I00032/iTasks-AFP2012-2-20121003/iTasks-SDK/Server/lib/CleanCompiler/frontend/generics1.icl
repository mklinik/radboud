//**************************************************************************************
// Generic programming features 
//**************************************************************************************

implementation module generics1

import StdEnv
import check
from checktypes import createMoreClassDictionaries
from transform import ::Group
import genericsupport

// Data types

:: FunDefs :== {#FunDef}
:: Modules :== {#CommonDefs}
:: DclModules :== {#DclModule}
:: Groups :== {!Group}

:: FunsAndGroups= ! {
		fg_fun_index :: !Index,
		fg_group_index :: !Index,
		fg_funs :: ![FunDef],
		fg_groups :: ![Group],
		fg_bimap_functions :: !BimapFunctions
	}

:: BimapFunctions = {
		bimap_id_function :: !FunctionIndexAndIdent,
		bimap_fromto_function :: !FunctionIndexAndIdent,
		bimap_tofrom_function :: !FunctionIndexAndIdent,
		bimap_to_function :: !FunctionIndexAndIdent,
		bimap_from_function :: !FunctionIndexAndIdent,
		bimap_arrow_function :: !FunctionIndexAndIdent,
		bimap_arrow_arg_id_function :: !FunctionIndexAndIdent,
		bimap_arrow_res_id_function :: !FunctionIndexAndIdent,
		bimap_from_Bimap_function :: !FunctionIndexAndIdent,
		bimap_PAIR_function :: !FunctionIndexAndIdent,
		bimap_EITHER_function :: !FunctionIndexAndIdent,
		bimap_OBJECT_function :: !FunctionIndexAndIdent,
		bimap_CONS_function :: !FunctionIndexAndIdent,
		bimap_FIELD_function :: !FunctionIndexAndIdent
	}

:: FunctionIndexAndIdent = {	
		fii_index :: !Index,
		fii_ident :: Ident
	}

:: *GenericState = 
	{ gs_modules :: !*Modules
	, gs_exprh :: !*ExpressionHeap
	, gs_genh :: !*GenericHeap
	, gs_varh :: !*VarHeap
	, gs_tvarh :: !*TypeVarHeap
	, gs_avarh :: !*AttrVarHeap 
	, gs_error :: !*ErrorAdmin
	, gs_symtab :: !*SymbolTable
	, gs_dcl_modules :: !*DclModules
	, gs_td_infos :: !*TypeDefInfos
	, gs_funs :: !*{#FunDef}
	, gs_groups :: {!Group}
	// non-unique, read only
	, gs_predefs :: !PredefinedSymbols
	, gs_main_module :: !Index
	, gs_used_modules :: !NumberSet
	}

// Exported functions

convertGenerics :: 
		!Int 					// index of the main dcl module
		!NumberSet				// set of used modules
		!{#CommonDefs} 			// common definitions of all modules
		!{!Group} 				// groups of functions
		!*{# FunDef} 			// functions
		!*TypeDefInfos 			// type definition information of all modules
		!*Heaps 				// all heaps
		!*HashTable 			// needed for what creating class dictionaries
		!*PredefinedSymbols 	// predefined symbols
		!u:{# DclModule}		// dcl modules
		!*ErrorAdmin 			// to report errors
	->  ( !{#CommonDefs}		// common definitions of all modules
		, !{!Group}				// groups of functions
		, !*{# FunDef}			// function definitions
		, ![IndexRange]			// index ranges of generated functions
		, !*TypeDefInfos		// type definition infos
		, !*Heaps				// all heaps
		, !*HashTable			// needed for creating class dictinaries
		, !*PredefinedSymbols	// predefined symbols	
		, !u:{# DclModule}		// dcl modules
		, !*ErrorAdmin			// to report errors
		)
convertGenerics main_dcl_module_n used_module_numbers modules groups funs td_infos heaps hash_table u_predefs dcl_modules error
	#! modules = {x \\ x <-: modules} 			// unique copy
	#! dcl_modules = { x \\ x <-: dcl_modules } 	// unique copy
	#! size_predefs = size u_predefs
	#! (predefs, u_predefs) = arrayCopyBegin u_predefs size_predefs // non-unique copy

	#! td_infos = clearTypeDefInfos td_infos
	#! (modules, heaps) = clearGenericDefs modules heaps

	# {hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}, hp_expression_heap} = heaps
	# gs = 
		{ gs_modules = modules
		, gs_symtab = hash_table.hte_symbol_heap
		, gs_dcl_modules = dcl_modules
		, gs_td_infos = td_infos
		, gs_exprh = hp_expression_heap	
		, gs_genh = hp_generic_heap	
		, gs_varh = hp_var_heap
		, gs_tvarh = th_vars
		, gs_avarh = th_attrs
		, gs_error = error	
		, gs_funs = funs
		, gs_groups = groups	
		, gs_predefs = predefs
		, gs_main_module = main_dcl_module_n
		, gs_used_modules = used_module_numbers
		} 

	# (generic_ranges, gs) = convert_generics gs

	#	{ 	gs_modules = modules, gs_symtab, gs_dcl_modules = dcl_modules, gs_td_infos = td_infos, 
			gs_genh = hp_generic_heap, gs_varh = hp_var_heap, gs_tvarh = th_vars, gs_avarh = th_attrs, 
			gs_exprh = hp_expression_heap,	
			gs_error = error, gs_funs = funs, gs_groups = groups,
			gs_predefs = predefs, gs_main_module = main_dcl_module_n, gs_used_modules = used_module_numbers} = gs
	#! hash_table = { hash_table & hte_symbol_heap = gs_symtab }
	#! heaps = 
		{ hp_expression_heap = hp_expression_heap
		, hp_var_heap = hp_var_heap
		, hp_generic_heap = hp_generic_heap
		, hp_type_heaps = { th_vars = th_vars, th_attrs = th_attrs }
		}
	= (modules, groups, funs, generic_ranges, td_infos, heaps, hash_table, u_predefs, dcl_modules, error)
where
	convert_generics :: !*GenericState -> (![IndexRange], !*GenericState)
	convert_generics gs		
		# (iso_range, bimap_functions, gs) = buildGenericRepresentations gs
		| not gs.gs_error.ea_ok = ([], gs)	

		# gs = buildClasses gs
		| not gs.gs_error.ea_ok = ([], gs)

		# (instance_range, gs) = convertGenericCases bimap_functions gs
		| not gs.gs_error.ea_ok = ([], gs)	

		#! gs = convertGenericTypeContexts gs

		= ([/*iso_range,*/instance_range], gs)

// clear stuff that might have been left over
// from compilation of other icl modules

clearTypeDefInfos :: !*{#*{#TypeDefInfo}} -> *{#*{#TypeDefInfo}}
clearTypeDefInfos td_infos
	= clear_modules 0 td_infos
where
	clear_modules n td_infos
		| n == size td_infos
			= td_infos
			#! (td_infos1, td_infos) = td_infos![n]
			#! td_infos1 = clear_td_infos 0 td_infos1
			#! td_infos = {td_infos & [n]=td_infos1}
			= clear_modules (inc n) td_infos 
			
	clear_td_infos n td_infos 			
		| n == size td_infos
			= td_infos
			#! (td_info, td_infos) = td_infos![n]
			#! td_infos = {td_infos & [n] = {td_info & tdi_gen_rep = No}}
			= clear_td_infos (inc n) td_infos 

clearGenericDefs :: !*{#CommonDefs} !*Heaps -> (!*{#CommonDefs},!*Heaps)
clearGenericDefs modules heaps
	= clear_module 0 modules  heaps
where	
	clear_module n modules heaps
		| n == size modules
			= (modules, heaps)
			#! ({com_generic_defs}, modules) = modules![n]
			#! (com_generic_defs, heaps) = updateArraySt clear_generic_def {x\\x<-:com_generic_defs} heaps 			
			#! modules = {modules & [n].com_generic_defs = com_generic_defs}
			= clear_module (inc n) modules heaps
			
	clear_generic_def generic_def=:{gen_info_ptr} heaps=:{hp_generic_heap}
		#! (gen_info, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		#! gen_info = { gen_info & gen_classes = createArray 32 [] }
		#! hp_generic_heap = writePtr gen_info_ptr gen_info hp_generic_heap
		= (generic_def, {heaps & hp_generic_heap = hp_generic_heap})
		
//	generic type representation

// generic representation is built for each type argument of
// generic cases of the current module
buildGenericRepresentations :: !*GenericState -> (!IndexRange,!BimapFunctions,!*GenericState)
buildGenericRepresentations gs=:{gs_main_module, gs_modules, gs_funs, gs_groups}
	#! (size_funs, gs_funs) = usize gs_funs
	#! size_groups = size gs_groups
	#! ({com_gencase_defs}, gs_modules) = gs_modules![gs_main_module]
	
	#! gs = { gs & gs_modules = gs_modules, gs_funs = gs_funs, gs_groups = gs_groups }
	
	# undefined_function_and_ident = {fii_index = -1,fii_ident = undef}
	  bimap_functions = {
				bimap_id_function = undefined_function_and_ident,
				bimap_fromto_function = undefined_function_and_ident,
				bimap_tofrom_function = undefined_function_and_ident,
				bimap_to_function = undefined_function_and_ident,
				bimap_from_function = undefined_function_and_ident,
		  		bimap_arrow_function = undefined_function_and_ident,
		  		bimap_arrow_arg_id_function = undefined_function_and_ident,
		  		bimap_arrow_res_id_function = undefined_function_and_ident,
		  		bimap_from_Bimap_function = undefined_function_and_ident,
		  		bimap_PAIR_function = undefined_function_and_ident,
		  		bimap_EITHER_function = undefined_function_and_ident,
		  		bimap_OBJECT_function = undefined_function_and_ident,
		  		bimap_CONS_function = undefined_function_and_ident,
		  		bimap_FIELD_function = undefined_function_and_ident
	  		}
	  funs_and_groups = {fg_fun_index=size_funs, fg_group_index=size_groups, fg_funs=[], fg_groups=[],fg_bimap_functions= bimap_functions}
	#! (funs_and_groups, gs)
		= foldArraySt build_generic_representation com_gencase_defs (funs_and_groups, gs)

	# {fg_fun_index,fg_funs=new_funs,fg_groups=new_groups,fg_bimap_functions} = funs_and_groups 
	# {gs_funs, gs_groups} = gs
	#! gs_funs = arrayPlusRevList gs_funs new_funs
	#! gs_groups = arrayPlusRevList gs_groups new_groups

	#! range = {ir_from = size_funs, ir_to = fg_fun_index}

	= (range, fg_bimap_functions, {gs & gs_funs = gs_funs, gs_groups = gs_groups})
where
	build_generic_representation
			{gc_type_cons=TypeConsSymb {type_index={glob_module,glob_object}, type_ident},gc_gcf,gc_pos} 
			(funs_and_groups, gs)
		# (type_def,gs) = gs!gs_modules.[glob_module].com_type_defs.[glob_object]
		# (td_info, gs) = gs!gs_td_infos.[glob_module,glob_object]
		= case gc_gcf of
			GCF gc_ident {gcf_body=GCB_FunIndex fun_index}
				-> case gs.gs_funs.[fun_index].fun_body of
					TransformedBody _ 
						// does not need a generic representation
						-> (funs_and_groups, gs)
					GeneratedBody	
						// needs a generic representation
						-> build_generic_type_rep type_def.td_rhs type_def.td_ident glob_module glob_object td_info gc_ident.id_name gc_pos funs_and_groups gs
			GCFS gcfs
				-> build_generic_type_rep type_def.td_rhs type_def.td_ident glob_module glob_object td_info "derive generic superclass" gc_pos funs_and_groups gs
	build_generic_representation _ st
		= st

	build_generic_type_rep td_rhs type_def_ident glob_module glob_object td_info g_ident_name gc_pos funs_and_groups gs
		= case td_rhs of
			SynType _
				#  gs_error = reportError g_ident_name gc_pos ("cannot derive a generic instance for a synonym type " +++ type_def_ident.id_name) gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})	
			AbstractType _
				#  gs_error = reportError g_ident_name gc_pos ("cannot derive a generic instance for an abstract type "  +++ type_def_ident.id_name) gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})	
			_
				-> case td_info.tdi_gen_rep of
					Yes _
						-> (funs_and_groups, gs)	// generic representation already built
					No
						# type_def_gi = {gi_module=glob_module,gi_index=glob_object}
						# (gen_type_rep, funs_and_groups, gs)
							= buildGenericTypeRep type_def_gi funs_and_groups gs
						# td_info = {td_info & tdi_gen_rep = Yes gen_type_rep}
						# gs = {gs & gs_td_infos.[glob_module,glob_object] = td_info}
						-> (funs_and_groups, gs)

:: ConsInfo = {ci_cons_info :: DefinedSymbol, ci_field_infos :: [DefinedSymbol]}

buildGenericTypeRep :: !GlobalIndex /*type def index*/ !FunsAndGroups !*GenericState ->	(!GenericTypeRep,!FunsAndGroups,!*GenericState)
buildGenericTypeRep type_index funs_and_groups
		gs=:{gs_modules, gs_predefs, gs_main_module, gs_error, gs_td_infos, gs_exprh, gs_varh, gs_genh, gs_avarh, gs_tvarh}
	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}

	# (type_def, gs_modules) = gs_modules![type_index.gi_module].com_type_defs.[type_index.gi_index]

	# (type_info, cons_infos, funs_and_groups, gs_modules, heaps, gs_error)
		= buildTypeDefInfo type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups gs_modules heaps gs_error

	# (atype, (gs_modules, gs_td_infos, heaps, gs_error)) 
		= buildStructType type_index type_info cons_infos gs_predefs (gs_modules, gs_td_infos, heaps, gs_error)
	
	# (from_fun_ds, funs_and_groups, heaps, gs_error)
		= buildConversionFrom type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error

	# (to_fun_ds, funs_and_groups, heaps, gs_error)
		= buildConversionTo type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error

	# (iso_fun_ds, funs_and_groups, heaps, gs_error)
		= buildConversionIso type_def from_fun_ds to_fun_ds gs_main_module gs_predefs funs_and_groups heaps gs_error

	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	# gs = {gs	& gs_modules = gs_modules
				, gs_td_infos = gs_td_infos
				, gs_error = gs_error
				, gs_avarh = th_attrs
				, gs_tvarh = th_vars
				, gs_varh = hp_var_heap
				, gs_genh = hp_generic_heap
				, gs_exprh = hp_expression_heap
		   }
	= ({gtr_type=atype,gtr_iso=iso_fun_ds,gtr_to=to_fun_ds,gtr_from=from_fun_ds}, funs_and_groups, gs)
	
//	the structure type

convertATypeToGenTypeStruct :: !Ident !Position !PredefinedSymbols !AType (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin) 
													   -> (GenTypeStruct, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
convertATypeToGenTypeStruct ident pos predefs type st
	= convert type st
where	
	convert {at_type=TA type_symb args, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=TAS type_symb args _, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=(CV tv) :@: args} st
		#! (args, st) = mapSt convert args st
		= (GTSAppVar tv args, st)
	convert {at_type=x --> y} st
		#! (x, st) = convert x st
		#! (y, st) = convert y st
		= (GTSArrow x y, st)
	convert {at_type=TV tv} st
		= (GTSVar tv, st)  
	convert {at_type=TB _} st
		= (GTSAppCons KindConst [], st)  
	convert {at_type=type} (modules, td_infos, heaps, error)
		# error = reportError ident.id_name pos ("can not build generic representation for this type", type) error
		= (GTSE, (modules, td_infos, heaps, error))

	convert_type_app {type_index} attr args (modules, td_infos, heaps, error)	
		# (type_def, modules) = modules![type_index.glob_module].com_type_defs.[type_index.glob_object]
		= case type_def.td_rhs of 
			SynType atype
				# (expanded_type, th) = expandSynonymType type_def attr args heaps.hp_type_heaps 
				-> convert {at_type = expanded_type, at_attribute = attr} 
					(modules, td_infos, {heaps & hp_type_heaps = th}, error) 
			_
				#! {pds_module, pds_def} = predefs.[PD_UnboxedArrayType]
				| 	type_index.glob_module == pds_module
					&& type_index.glob_object == pds_def
					&& (case args of [{at_type=TB _}] -> True; _ -> False)
					-> (GTSAppCons KindConst [], (modules, td_infos, heaps, error))
				| otherwise
					#! ({tdi_kinds}, td_infos) = td_infos ! [type_index.glob_module,type_index.glob_object]
					#! kind = if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds)
					#! (args, st) = mapSt  convert args (modules, td_infos, heaps, error)
					-> (GTSAppCons kind args, st)  

convert_bimap_AType_to_GenTypeStruct :: !AType !Position !PredefinedSymbols (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin) 
														 -> (GenTypeStruct, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
convert_bimap_AType_to_GenTypeStruct type pos predefs st
	= convert type st
where
	convert {at_type=TA type_symb args, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=TAS type_symb args _, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=(CV tv) :@: args} st
		#! (args, st) = mapSt convert args st
		= (GTSAppVar tv args, st)
	convert {at_type=x --> y} st
		#! (x, st) = convert x st
		#! (y, st) = convert y st
		= (GTSArrow x y, st)  
	convert {at_type=TV tv} st
		= (GTSVar tv, st)  
	convert {at_type=TB _} st
		= (GTSAppCons KindConst [], st)  
	convert {at_type=type} (modules, td_infos, heaps, error)
		# error = reportError predefined_idents.[PD_GenericBimap].id_name pos ("can not build generic representation for this type", type) error
		= (GTSE, (modules, td_infos, heaps, error))

	convert_type_app {type_index=type_index=:{glob_module,glob_object},type_arity} attr args (modules, td_infos, heaps, error)
		# (type_def, modules) = modules![glob_module].com_type_defs.[glob_object]
		= case type_def.td_rhs of 
			SynType atype
				# (expanded_type, th) = expandSynonymType type_def attr args heaps.hp_type_heaps
				-> convert {at_type = expanded_type, at_attribute = attr} 
					(modules, td_infos, {heaps & hp_type_heaps = th}, error) 
			AbstractType _
				#! {pds_module, pds_def} = predefs.[PD_UnboxedArrayType]
				| glob_module == pds_module && glob_object == pds_def
					&& (case args of [{at_type=TB _}] -> True; _ -> False)
					-> (GTSAppCons KindConst [], (modules, td_infos, heaps, error))
			RecordType _
				# {pds_module, pds_def} = predefs.[PD_TypeBimap]
				| glob_module == pds_module && glob_object == pds_def
					&& case args of [_,_] -> True; _ -> False
					#! (tdi_kinds,td_infos) = td_infos![glob_module,glob_object].tdi_kinds
					#! kind = if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds)
					#! (args, st) = convert_args args (modules, td_infos, heaps, error)
					-> (GTSAppBimap kind args, st)
			AlgType alts
				# n_args = length args
				| n_args>0 && type_arity==n_args
					# (can_generate_bimap_to_or_from,modules,heaps)
						= can_generate_bimap_to_or_from_for_this_type type_def glob_module alts modules heaps
					| can_generate_bimap_to_or_from
						#! (tdi_kinds,td_infos) = td_infos![glob_module,glob_object].tdi_kinds			
						#! (args, st) = convert_args args (modules, td_infos, heaps, error)
						-> (GTSAppConsSimpleType type_index (KindArrow tdi_kinds) args, st)
						-> 	convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
			_
				-> 	convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
	where
		convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
			#! (tdi_kinds,td_infos) = td_infos![glob_module,glob_object].tdi_kinds
			#! kind = if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds)
			#! (args, st) = convert_args args (modules, td_infos, heaps, error)
			= (GTSAppCons kind args, st)

	can_generate_bimap_to_or_from_for_this_type :: !CheckedTypeDef !Index ![DefinedSymbol] !*Modules !*Heaps -> (!Bool,!*Modules,!*Heaps)
	can_generate_bimap_to_or_from_for_this_type type_def=:{td_args} type_def_module_n alts modules heaps=:{hp_type_heaps}
		# th_vars = number_type_arguments td_args 0 hp_type_heaps.th_vars
		#! ok = check_constructors alts type_def_module_n modules th_vars
		# th_vars = remove_type_argument_numbers td_args th_vars
		# heaps = {heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}}
		= (ok,modules,heaps)
	where
		check_constructors :: ![DefinedSymbol] !Index !Modules !TypeVarHeap -> Bool
		check_constructors [{ds_index}:constructors] type_def_module_n modules th_vars
			# {cons_type,cons_exi_vars} = modules.[type_def_module_n].com_cons_defs.[ds_index]
			= isEmpty cons_exi_vars &&
			  isEmpty cons_type.st_context &&
			  check_constructor cons_type.st_args 0 th_vars &&
			  check_constructors constructors type_def_module_n modules th_vars
		check_constructors [] type_def_module_n modules th_vars
			= True

		check_constructor :: ![AType] !Int !TypeVarHeap -> Bool
		check_constructor [{at_type=TV {tv_info_ptr}}:atypes] used_type_vars th_vars
			= case sreadPtr tv_info_ptr th_vars of
				TVI_GenTypeVarNumber arg_n
					# arg_mask = 1<<arg_n
					| used_type_vars bitand arg_mask<>0
						-> False
						# used_type_vars = used_type_vars bitor arg_mask
						-> check_constructor atypes used_type_vars th_vars
		check_constructor [_:_] used_type_vars th_vars
			= False
		check_constructor [] used_type_vars th_vars
			= True

	convert_args args st
		= mapSt convert args st

// the structure type of a generic type can often be simplified
// because bimaps for types not containing generic variables are indentity bimaps
simplify_bimap_GenTypeStruct :: ![TypeVar] !GenTypeStruct !*Heaps -> (!GenTypeStruct, !*Heaps)
simplify_bimap_GenTypeStruct gvars type heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}} 
	#! th_vars = foldSt mark_type_var gvars th_vars
	#! (type, th_vars) = simplify type th_vars
	#! th_vars = foldSt clear_type_var gvars th_vars 
	= (type, { heaps & hp_type_heaps = { hp_type_heaps & th_vars = th_vars}})
where
	simplify t=:(GTSAppCons KindConst [])  st
		= (t, st)
	simplify (GTSAppCons kind=:(KindArrow kinds) args) st
		# formal_arity = length kinds
		# actual_arity = length args
		# contains_gen_vars = occurs_list args st
		| formal_arity == actual_arity && not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (args, st) = mapSt simplify args st
			= (GTSAppCons kind args, st)
	simplify (GTSAppConsSimpleType type_symbol_n kind args) st
		# contains_gen_vars = occurs_list args st
		| not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (args, st) = mapSt simplify args st
			= (GTSAppConsSimpleType type_symbol_n kind args, st)
	simplify t=:(GTSAppBimap KindConst [])  st
		= (t, st)
	simplify (GTSAppBimap kind=:(KindArrow kinds) args) st
		# formal_arity = length kinds
		# actual_arity = length args
		# contains_gen_vars = occurs_list args st
		| formal_arity == actual_arity && not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (args, st) = mapSt simplify args st
			= (GTSAppBimap kind args, st)
	simplify (GTSArrow x y) st
		# contains_gen_vars = occurs2 x y st
		| not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (x, st) = simplify x st
			# (y, st) = simplify y st
			= (GTSArrow x y, st)
	simplify (GTSAppVar tv args) st
		# (args, st) = mapSt simplify args st
		= (GTSAppVar tv args, st)	
	simplify t=:(GTSVar tv) st
		= (t, st)
	simplify (GTSPair x y) st
		# (x, st) = simplify x st
		# (y, st) = simplify y st
		= (GTSPair x y, st)
	simplify (GTSEither x y) st
		# (x, st) = simplify x st
		# (y, st) = simplify y st
		= (GTSEither x y, st)
	simplify (GTSCons cons_info_ds x) st
		# (x, st) = simplify x st
		= (GTSCons cons_info_ds x, st)
	simplify (GTSField field_info_ds x) st
		# (x, st) = simplify x st
		= (GTSField field_info_ds x, st)
	simplify (GTSObject type_info_ds x) st
		# (x, st) = simplify x st
		= (GTSObject type_info_ds x, st)
		
	occurs (GTSAppCons _ args) st 	= occurs_list args st
	occurs (GTSAppConsSimpleType _ _ args) st 	= occurs_list args st
	occurs (GTSAppBimap _ args) st 	= occurs_list args st
	occurs (GTSAppVar tv args) st 	= type_var_occurs tv st || occurs_list args st		
	occurs (GTSVar tv) st			= type_var_occurs tv st
	occurs (GTSArrow x y) st 		= occurs2 x y st
	occurs (GTSPair x y) st			= occurs2 x y st
	occurs (GTSEither x y) st		= occurs2 x y st
	occurs (GTSCons _ arg) st 		= occurs arg st	
	occurs (GTSField _ arg) st 		= occurs arg st	
	occurs (GTSObject _ arg) st 	= occurs arg st	
	occurs GTSE st 					= False

	occurs2 x y st
		= occurs x st || occurs y st

	occurs_list [] st
		= False
	occurs_list [x:xs] st 
		= occurs x st || occurs_list xs st

	type_var_occurs tv th_vars
		= case sreadPtr tv.tv_info_ptr th_vars of
			TVI_Empty = False
			TVI_Used = True

	mark_type_var tv=:{tv_info_ptr} th_vars 
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		= case tv_info of
			TVI_Empty = writePtr tv_info_ptr TVI_Used th_vars 
			_ = abort "type var is not empty"

	clear_type_var {tv_info_ptr} th_vars
		= writePtr tv_info_ptr TVI_Empty th_vars 

buildStructType ::
		!GlobalIndex				// type def global index
		!DefinedSymbol 				// type_info
		![ConsInfo]					// constructor and field info symbols
		!PredefinedSymbols
		(!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
	-> 	( !GenTypeStruct			// the structure type
		, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
		)
buildStructType {gi_module,gi_index} type_info cons_infos predefs (modules, td_infos, heaps, error)
	# (type_def=:{td_ident}, modules) = modules![gi_module].com_type_defs.[gi_index]	
	= build_type type_def type_info cons_infos (modules, td_infos, heaps, error)	
where
	build_type {td_rhs=AlgType alts, td_ident, td_pos} type_info cons_infos st
		# (cons_args, st) = zipWithSt (build_alt td_ident td_pos) alts cons_infos st
		# type = build_sum_type cons_args
		= (GTSObject type_info type, st)
	build_type
			{td_rhs=RecordType {rt_constructor}, td_ident, td_pos} 
			type_info  [{ci_cons_info, ci_field_infos}] 
			(modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[rt_constructor.ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)		
			# args = [GTSField fi arg \\ arg <- args & fi <- ci_field_infos]
			# prod_type = build_prod_type args		
			# type = GTSCons ci_cons_info prod_type
			= (GTSObject type_info type, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))
	build_type {td_rhs=SynType type,td_ident, td_pos} type_info cons_infos (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of a synonym type" error
		= (GTSE, (modules, td_infos, heaps, error))
	build_type td=:{td_rhs=(AbstractType _),td_ident, td_arity, td_args, td_pos} type_info cdis (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an abstract type" error
		= (GTSE, (modules, td_infos, heaps, error))

	build_alt td_ident td_pos cons_def_sym=:{ds_index} {ci_cons_info} (modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)	
			# prod_type = build_prod_type args
			= (GTSCons ci_cons_info prod_type, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))

	build_prod_type :: [GenTypeStruct] -> GenTypeStruct
	build_prod_type types 
		= listToBin build_pair build_unit types	
	where
		build_pair x y = GTSPair x y
		build_unit = GTSAppCons KindConst []	
		
	build_sum_type :: [GenTypeStruct] -> GenTypeStruct
	build_sum_type types
		= listToBin build_either build_void types
	where
		build_either x y = GTSEither x y
		build_void = abort "sanity check: no alternatives in a type\n"		

/*
// build a product of types
buildProductType :: ![AType] !PredefinedSymbols -> AType
buildProductType types predefs 
	= listToBin build_pair build_unit types
where
	build_pair x y = buildPredefTypeApp PD_TypePAIR [x, y] predefs	
	build_unit  = buildPredefTypeApp PD_TypeUNIT [] predefs

// build a sum of types		
buildSumType :: ![AType] !PredefinedSymbols -> AType
buildSumType types predefs 
	= listToBin build_either build_void types
where
	build_either x y = buildPredefTypeApp PD_TypeEITHER [x, y] predefs	
	build_void  = abort "sum of zero types\n"
*/

// build a binary representation of a list
listToBin :: (a a -> a) a [a] -> a 
listToBin bin tip [] = tip
listToBin bin tip [x] = x
listToBin bin tip xs
	# (l,r) = splitAt ((length xs) / 2) xs
	= bin (listToBin bin tip l) (listToBin bin tip r)

// build application of a predefined type constructor 
buildPredefTypeApp :: !Int [AType] !PredefinedSymbols -> AType
buildPredefTypeApp predef_index args predefs
	# {pds_module, pds_def} = predefs.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# global_index = {glob_module = pds_module, glob_object = pds_def}
	# type_symb = MakeTypeSymbIdent global_index pds_ident (length args) 		  
	= makeAType (TA type_symb args) TA_Multi

//	build type infos
buildTypeDefInfo :: 
		!Index 				// type def module
		!CheckedTypeDef		// the type definition
		!Index				// icl module
		!PredefinedSymbols		
		!FunsAndGroups !*Modules !*Heaps !*ErrorAdmin
	-> 	( DefinedSymbol	// type info
		, ![ConsInfo]
		, !FunsAndGroups, !*Modules, !*Heaps, !*ErrorAdmin)
buildTypeDefInfo td_module td=:{td_rhs = AlgType alts} main_module_index predefs funs_and_groups modules heaps error
	= buildTypeDefInfo1 td_module td alts [] main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td_module td=:{td_rhs = RecordType {rt_constructor, rt_fields}} main_module_index predefs funs_and_groups modules heaps error
	= buildTypeDefInfo1 td_module td [rt_constructor] [x\\x<-:rt_fields] main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td_module td=:{td_rhs = SynType type, td_ident, td_pos} main_module_index predefs funs_and_groups modules heaps error
	# error = reportError td_ident.id_name td_pos "cannot build constructor uinformation for a synonym type" error
	= buildTypeDefInfo1 td_module td [] [] main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td_module td=:{td_rhs = AbstractType _, td_ident, td_pos} main_module_index predefs funs_and_groups modules heaps error
	# error = reportError td_ident.id_name td_pos "cannot build constructor uinformation for an abstract type" error
	= buildTypeDefInfo1 td_module td [] [] main_module_index predefs funs_and_groups modules heaps error

buildTypeDefInfo1 td_module {td_ident, td_pos, td_arity} alts fields main_module_index predefs
				funs_and_groups=:{fg_fun_index=fun_index,fg_group_index=group_index,fg_funs=funs,fg_groups=groups} modules heaps error

	# num_conses = length alts
	# num_fields = length fields
	# new_group_index = inc group_index

	# type_def_dsc_index = fun_index
	# first_cons_dsc_index = fun_index + 1
	# cons_dsc_indexes = [first_cons_dsc_index .. first_cons_dsc_index + num_conses - 1]
	# first_field_dsc_index = first_cons_dsc_index + num_conses
	# field_dsc_indexes = [first_field_dsc_index .. first_field_dsc_index + num_fields - 1]
	# new_fun_index = first_field_dsc_index + num_fields

	# group = {group_members = [fun_index .. new_fun_index - 1]}
	# new_groups = [group:groups]
	
	# type_def_dsc_ds = {ds_arity=0, ds_ident=makeIdent("tdi_"+++td_ident.id_name), ds_index=type_def_dsc_index}
	# cons_dsc_dss = [ {ds_arity=0, ds_ident=makeIdent("cdi_"+++ds_ident.id_name), ds_index=i} \\ 
		{ds_ident} <- alts & i <- cons_dsc_indexes]
	# field_dsc_dss = [ {ds_arity=0, ds_ident=makeIdent("fdi_"+++fs_ident.id_name), ds_index=i} \\ 
		{fs_ident} <- fields & i <- field_dsc_indexes]

	# (type_def_dsc_fun, heaps) = build_type_def_dsc group_index cons_dsc_dss type_def_dsc_ds heaps	
	
	# (cons_dsc_funs, (modules, heaps)) = zipWithSt (build_cons_dsc group_index type_def_dsc_ds field_dsc_dss) cons_dsc_dss alts (modules, heaps)

	# (field_dsc_funs, (modules, heaps)) = zipWithSt (build_field_dsc group_index (hd cons_dsc_dss)) field_dsc_dss fields (modules, heaps)
	 
	// NOTE: reverse order (new functions are added at the head) 
	# new_funs = (reverse field_dsc_funs) ++ (reverse cons_dsc_funs) ++ [type_def_dsc_fun] ++ funs 

	# funs_and_groups = {funs_and_groups & fg_fun_index=new_fun_index, fg_group_index=new_group_index, fg_funs=new_funs, fg_groups=new_groups}

	# (type_info_ds, (funs_and_groups, heaps)) 
		= build_type_info type_def_dsc_ds (funs_and_groups, heaps)
	
	# (cons_info_dss, (funs_and_groups, heaps)) 
		= mapSt build_cons_info cons_dsc_dss (funs_and_groups, heaps)

	# (field_info_dss, (funs_and_groups, heaps)) 
		= mapSt build_field_info field_dsc_dss (funs_and_groups, heaps)

	# cons_infos = case (cons_info_dss, field_info_dss) of
		([cons_info_ds], field_infos) -> [{ci_cons_info = cons_info_ds, ci_field_infos = field_infos}] 	 
		(cons_info_dss, []) -> [{ci_cons_info=x,ci_field_infos=[]}\\x<-cons_info_dss]
		_ -> abort "generics.icl sanity check: fields in non-record type\n"

	= (type_info_ds, cons_infos, funs_and_groups, modules, heaps, error)
where
	build_type_def_dsc group_index cons_info_dss {ds_ident} heaps
		# td_name_expr = makeStringExpr td_ident.id_name
		# td_arity_expr = makeIntExpr td_arity
		# num_conses_expr = makeIntExpr (length alts)
		# (cons_info_exprs, heaps) = mapSt (\x st->buildFunApp main_module_index x [] st) cons_info_dss heaps
		# (td_conses_expr, heaps) = makeListExpr cons_info_exprs predefs heaps
		
		# (body_expr, heaps) = buildPredefConsApp PD_CGenericTypeDefDescriptor 
			[ td_name_expr
			, td_arity_expr
			, num_conses_expr 
			, td_conses_expr
			// TODO: module_name_expr
			] 
			predefs heaps

		# fun = makeFunction ds_ident group_index [] body_expr No main_module_index td_pos
		= (fun, heaps)

	build_cons_dsc group_index type_def_info_ds field_dsc_dss cons_info_ds cons_ds (modules, heaps)
		# ({cons_ident,cons_type,cons_priority,cons_number,cons_exi_vars}, modules)
			= modules! [td_module].com_cons_defs.[cons_ds.ds_index]  		
		# name_expr 			 = makeStringExpr cons_ident.id_name
		# arity_expr 			 = makeIntExpr cons_type.st_arity
		# (prio_expr, heaps)	 = make_prio_expr cons_priority heaps
		# (type_def_expr, heaps) = buildFunApp main_module_index type_def_info_ds [] heaps
		# (type_expr, heaps) 	 = make_type_expr cons_exi_vars cons_type heaps 			
		# (field_exprs, heaps)   = mapSt (\x st->buildFunApp main_module_index x [] st) field_dsc_dss heaps
		# (fields_expr, heaps)   =  makeListExpr field_exprs predefs heaps 
		# cons_index_expr		 = makeIntExpr cons_number
		# (body_expr, heaps) 
			= buildPredefConsApp PD_CGenericConsDescriptor 
				[ name_expr 
				, arity_expr
				, prio_expr
				, type_def_expr
				, type_expr
				, fields_expr 
				, cons_index_expr
				]  
				predefs heaps

		# fun = makeFunction cons_info_ds.ds_ident group_index [] body_expr No main_module_index td_pos		
		= (fun, (modules, heaps))
	where
		make_prio_expr NoPrio heaps
			= buildPredefConsApp PD_CGenConsNoPrio [] predefs heaps
		make_prio_expr (Prio assoc prio) heaps
			# assoc_predef = case assoc of
				NoAssoc 	-> PD_CGenConsAssocNone 
				LeftAssoc 	-> PD_CGenConsAssocLeft
				RightAssoc 	-> PD_CGenConsAssocRight
			# (assoc_expr, heaps) = buildPredefConsApp assoc_predef [] predefs heaps 	
			# prio_expr = makeIntExpr prio		
			= buildPredefConsApp PD_CGenConsPrio [assoc_expr, prio_expr] predefs heaps 

		make_type_expr [] {st_vars, st_args, st_result} heaps=:{hp_type_heaps=type_heaps=:{th_vars}}
			# (_,th_vars) = foldSt (\ {tv_info_ptr} (n, th_vars) -> (n+1, writePtr tv_info_ptr (TVI_GenTypeVarNumber n) th_vars)) st_vars (0,th_vars)
			# heaps = {heaps & hp_type_heaps={type_heaps & th_vars=th_vars}}
			# (arg_exprs, heaps) = mapSt make_expr1 st_args heaps
			# (result_expr, heaps) = make_expr1 st_result heaps
			# {hp_type_heaps=type_heaps=:{th_vars}} = heaps
			# th_vars = foldSt (\ {tv_info_ptr} th_vars -> writePtr tv_info_ptr TVI_Empty th_vars) st_vars th_vars
			# heaps = {heaps & hp_type_heaps={type_heaps & th_vars=th_vars}}
			= curry arg_exprs result_expr heaps
		where
			curry [] result_expr heaps 
				= (result_expr, heaps)
			curry [x:xs] result_expr heaps
				# (y, heaps) = curry xs result_expr heaps
				= make_arrow x y heaps
		
			make_expr1 :: !AType !*Heaps -> (!Expression, !*Heaps)
			make_expr1 {at_type} heaps = make_expr at_type heaps
		
			make_expr :: !Type !*Heaps -> (!Expression, !*Heaps)
			make_expr (TA type_symb arg_types) heaps
				# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
				# (type_cons, heaps) = make_type_cons type_symb.type_ident.id_name heaps 
				= make_apps type_cons arg_exprs heaps
			make_expr (TAS type_symb arg_types _) heaps
				# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
				# (type_cons, heaps) = make_type_cons type_symb.type_ident.id_name heaps 
				= make_apps type_cons arg_exprs heaps
			make_expr (x --> y) heaps
				# (x, heaps) = make_expr1 x heaps
				# (y, heaps) = make_expr1 y heaps				
				= make_arrow x y heaps
			make_expr TArrow heaps 
				= make_type_cons "(->)" heaps
			make_expr (TArrow1 type) heaps
				# (arg_expr, heaps) = make_expr1 type heaps 
				# (arrow_expr, heaps) = make_type_cons "(->)" heaps
				= make_app arrow_expr arg_expr heaps
			make_expr (CV {tv_info_ptr} :@: arg_types) heaps
				# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
				# (tv_expr, heaps) = make_type_var tv_info_ptr heaps
				= make_apps tv_expr arg_exprs heaps
			make_expr (TB bt) heaps
				= make_type_cons (toString bt) heaps	
			make_expr (TV {tv_info_ptr}) heaps 
				= make_type_var tv_info_ptr heaps 
			make_expr (GTV {tv_info_ptr}) heaps
				= make_type_var tv_info_ptr heaps 
			make_expr (TQV {tv_info_ptr}) heaps 
				= make_type_var tv_info_ptr heaps
			make_expr TE heaps
				= make_error_type_cons heaps
			make_expr (TFA _ _) heaps
				// error is reported in convertATypeToGenTypeStruct
				= make_error_type_cons heaps
			make_expr (TFAC _ _ _) heaps
				// error is reported in convertATypeToGenTypeStruct
				= make_error_type_cons heaps
			make_expr _ heaps
				= abort "type does not match\n"

			make_apps x [] heaps 
				= (x, heaps)
			make_apps x [y:ys] heaps
				# (z, heaps) = make_app x y heaps	
				= make_apps z ys heaps

			make_type_var tv_info_ptr heaps
				#! type_var_n = case sreadPtr tv_info_ptr heaps.hp_type_heaps.th_vars of
									TVI_GenTypeVarNumber n -> n
				= buildPredefConsApp PD_CGenTypeVar [makeIntExpr type_var_n] predefs heaps									

			make_arrow x y heaps = buildPredefConsApp PD_CGenTypeArrow [x, y] predefs heaps

			make_app x y heaps = buildPredefConsApp PD_CGenTypeApp [x, y] predefs heaps 	 

			make_error_type_cons heaps = make_type_cons "<error>" heaps
		make_type_expr [_:_] {st_vars, st_args, st_result} heaps
			// Error "cannot build a generic representation of an existential type" is reported in buildStructType
			= make_type_cons "<error>" heaps

	make_type_cons name heaps
		# name_expr = makeStringExpr name
		= buildPredefConsApp PD_CGenTypeCons [name_expr] predefs heaps

	build_field_dsc group_index cons_dsc_ds field_dsc_ds {fs_ident, fs_index} (modules, heaps)
		# name_expr = makeStringExpr fs_ident.id_name
		# ({sd_field_nr}, modules)	
			= modules! [td_module].com_selector_defs.[fs_index]  		
		# index_expr = makeIntExpr sd_field_nr
		# (cons_expr, heaps) = buildFunApp main_module_index cons_dsc_ds [] heaps				
		# (body_expr, heaps) 
			= buildPredefConsApp PD_CGenericFieldDescriptor 
				[ name_expr 
				, index_expr
				, cons_expr
				]  
				predefs heaps
		# fun = makeFunction field_dsc_ds.ds_ident group_index [] body_expr No main_module_index td_pos		
		= (fun, (modules, heaps))
		
	build_cons_info cons_dsc_ds (funs_and_groups, heaps)
		# ident = makeIdent ("g"+++cons_dsc_ds.ds_ident.id_name)	

		# (cons_dsc_expr, heaps) = buildFunApp main_module_index cons_dsc_ds [] heaps

		# (body_expr, heaps) 
			= buildPredefConsApp PD_GenericConsInfo [cons_dsc_expr] predefs heaps		

		# (def_sym, funs_and_groups) = buildFunAndGroup ident [] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, (funs_and_groups, heaps))

	build_field_info field_dsc_ds (funs_and_groups, heaps)
		# ident = makeIdent ("g"+++field_dsc_ds.ds_ident.id_name)	

		# (field_dsc_expr, heaps) = buildFunApp main_module_index field_dsc_ds [] heaps

		# (body_expr, heaps) 
			= buildPredefConsApp PD_GenericFieldInfo [field_dsc_expr] predefs heaps		

		# (def_sym, funs_and_groups) = buildFunAndGroup ident [] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, (funs_and_groups, heaps))

	build_type_info type_dsc_ds (funs_and_groups, heaps)
		# ident = makeIdent ("g"+++type_dsc_ds.ds_ident.id_name)	

		# (type_dsc_expr, heaps) = buildFunApp main_module_index type_dsc_ds [] heaps

		# (body_expr, heaps) 
			= buildPredefConsApp PD_GenericTypeInfo [type_dsc_expr] predefs heaps		

		# (def_sym, funs_and_groups) = buildFunAndGroup ident [] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, (funs_and_groups, heaps))

//	conversions functions

// buildConversionIso
buildConversionIso :: 
		!CheckedTypeDef		// the type definition
		!DefinedSymbol		// from fun
		!DefinedSymbol	 	// to fun
		!Index				// main module
		!PredefinedSymbols
		FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!DefinedSymbol,
		FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildConversionIso type_def=:{td_ident, td_pos} from_fun to_fun
		main_dcl_module_n predefs funs_and_groups heaps error
	#! (from_expr, heaps) 	= buildFunApp main_dcl_module_n from_fun [] heaps
	#! (to_expr, heaps) 	= buildFunApp main_dcl_module_n to_fun [] heaps	
	#! (iso_expr, heaps) 	= build_bimap_record to_expr from_expr predefs heaps
	
	#! ident = makeIdent ("iso" +++ td_ident.id_name)
	#! (def_sym, funs_and_groups) = buildFunAndGroup ident [] iso_expr No main_dcl_module_n td_pos funs_and_groups
	= (def_sym, funs_and_groups, heaps, error)

build_bimap_record to_expr from_expr predefs heaps 
	= buildPredefConsApp PD_ConsBimap [to_expr, from_expr] predefs heaps	

// conversion from type to generic
buildConversionTo ::
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbols
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> 	(!DefinedSymbol,
		 FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildConversionTo		
		type_def_mod 
		type_def=:{td_rhs, td_ident, td_index, td_pos} 
		main_module_index predefs funs_and_groups heaps error
	# (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 
	# (body_expr, heaps, error) = 
		build_expr_for_type_rhs type_def_mod td_index td_rhs arg_expr heaps error
	# fun_name = makeIdent ("toGeneric" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups) 
			= (buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) 
			= (buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
where
	// build conversion for type rhs
	build_expr_for_type_rhs :: 
			!Int 				// type def module
			!Int 				// type def index 
			!TypeRhs			// type def rhs 
			!Expression			// expression of the function argument variable   
			!*Heaps 
			!*ErrorAdmin
		-> 	( !Expression		// generated expression
			, !*Heaps	// state
			, !*ErrorAdmin)
 	build_expr_for_type_rhs type_def_mod type_def_index (AlgType def_symbols) arg_expr heaps error
		= build_expr_for_conses False type_def_mod type_def_index def_symbols arg_expr heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (RecordType {rt_constructor}) arg_expr heaps error		
		= build_expr_for_conses True type_def_mod type_def_index [rt_constructor] arg_expr  heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (AbstractType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for an abstract type" error
		= (EE, heaps, error)
	build_expr_for_type_rhs type_def_mod type_def_index (SynType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for a synonym type" error
		= (EE, heaps, error)

	// build conversion for constructors of a type def 	
	build_expr_for_conses is_record type_def_mod type_def_index cons_def_syms arg_expr heaps error
		# (case_alts, heaps, error)
			= build_exprs_for_conses is_record 0 (length cons_def_syms) type_def_mod cons_def_syms  heaps error
		# case_patterns = AlgebraicPatterns {glob_module = type_def_mod, glob_object = type_def_index} case_alts
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

	// build conversions for constructors 	
	build_exprs_for_conses :: !Bool !Int !Int !Int ![DefinedSymbol] !*Heaps !*ErrorAdmin
		-> ([AlgebraicPattern], !*Heaps, !*ErrorAdmin)
	build_exprs_for_conses is_record i n type_def_mod [] heaps error
		= ([], heaps, error)
	build_exprs_for_conses is_record i n type_def_mod [cons_def_sym:cons_def_syms] heaps error
		#! (alt, heaps, error) = build_expr_for_cons is_record i n type_def_mod cons_def_sym heaps error
		#! (alts, heaps, error) =  build_exprs_for_conses is_record (i+1) n type_def_mod cons_def_syms heaps error 		
		= ([alt:alts], heaps, error)

	// build conversion for a constructor	
	build_expr_for_cons :: !Bool !Int !Int !Int !DefinedSymbol !*Heaps !*ErrorAdmin 
		-> (AlgebraicPattern, !*Heaps, !*ErrorAdmin)
	build_expr_for_cons is_record i n type_def_mod cons_def_sym=:{ds_ident, ds_arity} heaps error	
		#! names = ["x" +++ toString (i+1) +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps 

		#! (arg_exprs, heaps) = build_fields is_record var_exprs heaps
			with
				build_fields False var_exprs heaps = (var_exprs, heaps)
				build_fields True var_exprs heaps = mapSdSt build_field var_exprs predefs heaps

		#! (expr, heaps) = build_prod arg_exprs predefs heaps
		#! (expr, heaps) = build_cons expr predefs heaps
		#! (expr, heaps) = build_sum i n expr predefs heaps
				
		#! (expr, heaps) = build_object expr predefs heaps
						
		#! alg_pattern = {
			ap_symbol = {glob_module = type_def_mod, glob_object = cons_def_sym},
			ap_vars = vars,
			ap_expr = expr,
			ap_position = NoPos
			}
		= (alg_pattern, heaps, error)	
	
	build_sum :: !Int !Int !Expression !PredefinedSymbols !*Heaps -> (!Expression, !*Heaps)
	build_sum i n expr predefs heaps
		| n == 0 	= abort "build sum of zero elements\n"
		| i >= n	= abort "error building sum"
		| n == 1 	= (expr, heaps)
		| i < (n/2) 
			# (expr, heaps) = build_sum i (n/2) expr predefs heaps
			= build_left expr predefs heaps
		| otherwise
			# (expr, heaps) = build_sum (i - (n/2)) (n - (n/2)) expr predefs heaps
			= build_right expr predefs heaps
				
	build_prod :: ![Expression] !PredefinedSymbols !*Heaps -> (!Expression, !*Heaps)
	build_prod [] predefs heaps = build_unit heaps
	where
		build_unit heaps = buildPredefConsApp PD_ConsUNIT [] predefs heaps 	
	build_prod [expr] predefs heaps = (expr, heaps)
	build_prod exprs predefs heaps
		# (lexprs, rexprs) = splitAt ((length exprs)/2) exprs  
		# (lexpr, heaps) = build_prod lexprs predefs heaps
		# (rexpr, heaps) = build_prod rexprs predefs heaps
		= build_pair lexpr rexpr predefs heaps
			
buildConversionFrom	::	
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbols
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!DefinedSymbol,
		 FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildConversionFrom
		type_def_mod 
		type_def=:{td_rhs, td_ident, td_index, td_pos} 
		main_module_index predefs funs_and_groups heaps error
	# (body_expr, arg_var, heaps, error) = 
		build_expr_for_type_rhs type_def_mod td_rhs heaps error
	# fun_name = makeIdent ("fromGeneric" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups) 
			= (buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) 
			= (buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
where
	// build expression for type def rhs
	build_expr_for_type_rhs :: 
			!Index				// type def module
			!TypeRhs			// type rhs
			!*Heaps !*ErrorAdmin	
		-> 	( !Expression		// body expresssion
			, !FreeVar
			, !*Heaps, !*ErrorAdmin)
	build_expr_for_type_rhs type_def_mod (AlgType def_symbols) heaps error
		#! (expr, var, heaps, error) = build_sum False type_def_mod def_symbols heaps error
		#! (expr, var, heaps) = build_case_object var expr predefs heaps
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (RecordType {rt_constructor}) heaps error				
		# (expr, var, heaps, error) = build_sum True type_def_mod [rt_constructor] heaps	error
		#! (expr, var, heaps) = build_case_object var expr predefs heaps
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (AbstractType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for an abstract type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	build_expr_for_type_rhs type_def_mod (SynType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for a synonym type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	
	// build expression for sums
	build_sum ::
			!Bool				// is record 
			!Index 
			![DefinedSymbol] 
			!*Heaps !*ErrorAdmin
		-> 	( !Expression
			, !FreeVar			// top variable
			, !*Heaps, !*ErrorAdmin)
	build_sum is_record type_def_mod [] heaps error
		= abort "algebraic type with no constructors!\n"
	build_sum is_record type_def_mod [def_symbol] heaps error
		#! (cons_app_expr, cons_arg_vars, heaps) = build_cons_app type_def_mod def_symbol heaps
		#! (prod_expr, var, heaps) = build_prod is_record cons_app_expr cons_arg_vars heaps 
		#! (alt_expr, var, heaps) = build_case_cons var prod_expr predefs heaps
		= (alt_expr, var, heaps, error)
	build_sum is_record type_def_mod def_symbols heaps error
		#! (left_def_syms, right_def_syms) = splitAt ((length def_symbols) /2) def_symbols
		#! (left_expr, left_var, heaps, error) 
			= build_sum is_record type_def_mod left_def_syms heaps error
		#! (right_expr, right_var, heaps, error)
			= build_sum is_record type_def_mod right_def_syms heaps error	
		#! (case_expr, var, heaps) = 
			build_case_either left_var left_expr right_var right_expr predefs heaps
		= (case_expr, var, heaps, error)
	
	// build expression for products
	build_prod :: 
			!Bool							// is record
			!Expression   					// result of the case on product
			![FreeVar] 						// list of variables of the constructor pattern
			!*Heaps
		-> 	( !Expression					// generated product
			, !FreeVar						// top variable
			, !*Heaps
			)
	build_prod is_record expr [] heaps
		= build_case_unit expr heaps	
	build_prod is_record expr [cons_arg_var] heaps
		| is_record
			= build_case_field cons_arg_var expr predefs heaps
			= (expr, cons_arg_var, heaps)
	build_prod is_record expr cons_arg_vars heaps
		#! (left_vars, right_vars) = splitAt ((length cons_arg_vars) /2) cons_arg_vars		 
		#! (expr, right_var, heaps) = build_prod is_record expr right_vars heaps
		#! (expr, left_var, heaps) = build_prod is_record expr left_vars heaps
		#! (case_expr, var, heaps) = build_case_pair left_var right_var expr predefs heaps
		= (case_expr, var, heaps) 
	
	// build constructor application expression
	build_cons_app :: !Index !DefinedSymbol !*Heaps 
		-> (!Expression, ![FreeVar], !*Heaps)
	build_cons_app cons_mod def_symbol=:{ds_arity} heaps
		#! names = ["x"  +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps 
		#! (expr, heaps) = buildConsApp cons_mod def_symbol var_exprs heaps
	 	= (expr, vars, heaps)

	build_case_unit body_expr heaps
		# unit_pat = buildPredefConsPattern PD_ConsUNIT [] body_expr predefs
		# {pds_module, pds_def} = predefs.[PD_TypeUNIT]
		# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [unit_pat]
		= build_case_expr case_patterns heaps

build_pair x y predefs heaps
	= buildPredefConsApp PD_ConsPAIR [x, y] predefs heaps

build_left x predefs heaps
	= buildPredefConsApp PD_ConsLEFT [x] predefs heaps

build_right x predefs heaps
	= buildPredefConsApp PD_ConsRIGHT [x] predefs heaps

build_object expr predefs heaps
	= buildPredefConsApp PD_ConsOBJECT [expr] predefs heaps						

build_cons expr predefs heaps
	= buildPredefConsApp PD_ConsCONS [expr] predefs heaps

build_field var_expr predefs heaps
	= buildPredefConsApp PD_ConsFIELD [var_expr] predefs heaps 

build_case_pair var1 var2 body_expr predefs heaps
	# pair_pat = buildPredefConsPattern PD_ConsPAIR [var1, var2] body_expr predefs	
	# {pds_module, pds_def} = predefs.[PD_TypePAIR]
	# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [pair_pat]	
	= build_case_expr case_patterns heaps

build_case_either left_var left_expr right_var right_expr predefs heaps
	# left_pat = buildPredefConsPattern PD_ConsLEFT [left_var] left_expr predefs
	# right_pat = buildPredefConsPattern PD_ConsRIGHT [right_var] right_expr predefs
	# {pds_module, pds_def} = predefs.[PD_TypeEITHER]
	# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [left_pat, right_pat]
	= build_case_expr case_patterns heaps

build_case_object var body_expr predefs heaps
	# pat = buildPredefConsPattern PD_ConsOBJECT [var] body_expr predefs	
	# {pds_module, pds_def} = predefs.[PD_TypeOBJECT]
	# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [pat]
	= build_case_expr case_patterns heaps

build_case_cons var body_expr predefs heaps
	# pat = buildPredefConsPattern PD_ConsCONS [var] body_expr predefs	
	# {pds_module, pds_def} = predefs.[PD_TypeCONS]
	# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [pat]
	= build_case_expr case_patterns heaps 

build_case_field var body_expr predefs heaps
	# pat = buildPredefConsPattern PD_ConsFIELD [var] body_expr predefs	
	# {pds_module, pds_def} = predefs.[PD_TypeFIELD]
	# case_patterns = AlgebraicPatterns {glob_module = pds_module, glob_object = pds_def} [pat]
	= build_case_expr case_patterns heaps 

// case with a variable as the selector expression
build_case_expr case_patterns heaps
	# (var_expr, var, heaps) = buildVarExpr "c" heaps
	# (case_expr, heaps) = buildCaseExpr var_expr case_patterns heaps
	= (case_expr, var, heaps)

// build kind indexed classes 

buildClasses :: !*GenericState -> *GenericState
buildClasses gs=:{gs_main_module}
	#! ({com_class_defs,com_member_defs},gs) = gs!gs_modules.[gs_main_module]
	#! num_classes = size com_class_defs
	#! num_members = size com_member_defs

	#! ((classes, members, new_num_classes, new_num_members), gs)
		= build_modules 0 ([], [], num_classes, num_members) gs

	# first_new_class_index = size com_class_defs

	// obtain common definitions again because com_gencase_defs are updated 
	#! (common_defs,gs) = gs!gs_modules.[gs_main_module]
	# common_defs = {common_defs & com_class_defs = arrayPlusRevList com_class_defs classes
								 , com_member_defs = arrayPlusRevList com_member_defs members}

	#! (common_defs, gs)
		= build_class_dictionaries first_new_class_index common_defs gs
	
	= {gs & gs_modules.[gs_main_module] = common_defs}  
where
	build_modules :: !Index (![ClassDef], ![MemberDef], !Int, !Int) !*GenericState
		-> ((![ClassDef], ![MemberDef], !Int, !Int), !*GenericState)
	build_modules module_index st gs=:{gs_modules,gs_used_modules}
		| module_index == size gs_modules
			= (st, gs)
		| not (inNumberSet module_index gs_used_modules)
			= build_modules (inc module_index) st gs		
			#! ({com_gencase_defs},gs_modules) = gs_modules![module_index] 
			#! (com_gencase_defs, st, gs) 
				= build_module module_index 0 {x\\x<-:com_gencase_defs} st {gs & gs_modules=gs_modules}
			#! gs = {gs & gs_modules.[module_index].com_gencase_defs = com_gencase_defs}
			= build_modules (inc module_index) st gs

	build_module module_index index com_gencase_defs st gs
		| index == size com_gencase_defs
			= (com_gencase_defs, st, gs)
			#! (gencase, com_gencase_defs) = com_gencase_defs ! [index]
			#! (gencase, st, gs) = on_gencase module_index index gencase st gs
			#! com_gencase_defs = {com_gencase_defs & [index] = gencase} 	
			= build_module module_index (inc index) com_gencase_defs st gs

	on_gencase :: !Index !Index
			!GenericCaseDef (![ClassDef], ![MemberDef], !Index, Index) !*GenericState
		-> (!GenericCaseDef,(![ClassDef], ![MemberDef], !Index, Index), !*GenericState)
	on_gencase module_index index
				gencase=:{gc_type_cons, gc_gcf=GCF gc_ident gcf=:{gcf_generic}} st gs=:{gs_modules, gs_td_infos}
		#! (gen_def, gs_modules) = gs_modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]
		#! (kind, gs_td_infos) = get_kind_of_type_cons gc_type_cons gs_td_infos

		// To generate all partially applied shorthand instances we need
		// classes for all partial applications of the gc_kind and for
		// all the argument kinds.
		// Additionally, we always need classes for base cases *, *->* and *->*->* 

		#! gs = {gs & gs_modules = gs_modules, gs_td_infos = gs_td_infos}
		#! subkinds = determine_subkinds kind 		
		#! kinds = 
			[ KindConst
			, KindArrow [KindConst]
			, KindArrow [KindConst, KindConst] 
			: subkinds]
		#! (st, gs) = build_classes_if_needed gen_def kinds st gs
		#! gencase = { gencase & gc_gcf = GCF gc_ident {gcf & gcf_kind = kind}}

		#! type_index = index_OBJECT_CONS_FIELD_type gencase.gc_type gs.gs_predefs
		| type_index>=0		
			# (GCF _ {gcf_body = GCB_FunIndex fun_index}) = gencase.gc_gcf
			  gen_info_ptr = gen_def.gen_info_ptr

			  fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons
			  ocf_index = {ocf_module=module_index,ocf_index=fun_index,ocf_ident=fun_ident}

			  (gen_info,generic_heap) = readPtr gen_info_ptr gs.gs_genh
			  gen_OBJECT_CONS_FIELD_indices = {gi\\gi<-:gen_info.gen_OBJECT_CONS_FIELD_indices}
			  gen_OBJECT_CONS_FIELD_indices = {gen_OBJECT_CONS_FIELD_indices & [type_index]=ocf_index}
			  gen_info = {gen_info & gen_OBJECT_CONS_FIELD_indices=gen_OBJECT_CONS_FIELD_indices}
			  generic_heap = writePtr gen_info_ptr gen_info generic_heap
			  gs = {gs & gs_genh=generic_heap}
			= (gencase, st, gs)

			= (gencase, st, gs)
	on_gencase module_index index
			gencase=:{gc_gcf=GCFS gcfs,gc_type_cons} st gs=:{gs_td_infos}
		# (kind, gs_td_infos) = get_kind_of_type_cons gc_type_cons gs_td_infos
		#! gs = {gs & gs_td_infos = gs_td_infos}
		# subkinds = determine_subkinds kind 		
		# kinds = 
			[ KindConst
			, KindArrow [KindConst]
			, KindArrow [KindConst, KindConst] 
			: subkinds]
		# (gcfs,st,gs) = build_classes_for_generic_superclasses_if_needed gcfs kind kinds st gs
		#! gencase = {gencase & gc_gcf = GCFS gcfs}
		= (gencase, st, gs)
	where
		build_classes_for_generic_superclasses_if_needed [!gcf=:{gcf_generic}:gcfs!] kind kinds st gs
			#! (gen_def,gs) = gs!gs_modules.[gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]
			# (st, gs) = build_classes_if_needed gen_def kinds st gs
			# gcf={gcf & gcf_kind = kind}
			# (gcfs,st,gs) = build_classes_for_generic_superclasses_if_needed gcfs kind kinds st gs
			= ([!gcf:gcfs!],st,gs)
		build_classes_for_generic_superclasses_if_needed [!!] kind kinds st gs
			= ([!!],st,gs)

	build_classes_if_needed gen_def kinds st gs
		= foldSt (build_class_if_needed gen_def) kinds (st, gs)

	build_class_if_needed :: !GenericDef !TypeKind ((![ClassDef], ![MemberDef], !Index, Index), *GenericState)
		-> ((![ClassDef], ![MemberDef], !Index, Index), *GenericState)		
	build_class_if_needed gen_def kind ((classes, members, class_index, member_index), gs=:{gs_main_module, gs_genh})
		#! (opt_class_info, gs_genh) = lookup_generic_class_info gen_def kind gs_genh
		#! gs = {gs & gs_genh = gs_genh}
		= case opt_class_info of
			No
				#! (class_def, member_def, gs=:{gs_genh}) 
					= buildClassAndMember gs_main_module class_index member_index kind gen_def gs 
				#! class_info = 
					{	gci_kind = kind
					,	gci_module = gs_main_module
					,	gci_class = class_index
					,	gci_member = member_index
					}
				#! gs_genh = add_generic_class_info gen_def class_info gs_genh
				#! gs = { gs & gs_genh = gs_genh }
				-> (([class_def:classes], [member_def:members], inc class_index, inc member_index), gs)
			Yes class_info	
				-> ((classes, members, class_index, member_index), gs)
	
	determine_subkinds KindConst 
		= [KindConst]
	determine_subkinds (KindArrow kinds) 
		= do_it kinds
	where
		do_it [] = [KindConst]
		do_it all_ks=:[k:ks] 
			#! this_kind = KindArrow all_ks
			#! left_subkinds = determine_subkinds k
			#! right_subkinds = do_it ks
			= [this_kind : left_subkinds ++ right_subkinds] 
				
	get_kind_of_type_cons :: !TypeCons !*TypeDefInfos -> (!TypeKind, !*TypeDefInfos)
	get_kind_of_type_cons (TypeConsBasic _) td_infos 
		= (KindConst, td_infos)
	get_kind_of_type_cons TypeConsArrow td_infos
		= (KindArrow [KindConst,KindConst], td_infos)
	get_kind_of_type_cons (TypeConsSymb {type_ident, type_index}) td_infos
		#! ({tdi_kinds}, td_infos) = td_infos ! [type_index.glob_module,type_index.glob_object]
		= (if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds), td_infos)
	get_kind_of_type_cons (TypeConsVar tv) td_infos
		= (KindConst, td_infos)

	lookup_generic_class_info {gen_info_ptr} kind hp_generic_heap
		#! ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		= (lookupGenericClassInfo kind gen_classes, hp_generic_heap)

	add_generic_class_info	{gen_info_ptr} class_info gs_genh	
		#! (gen_info=:{gen_classes}, gs_genh) = readPtr gen_info_ptr gs_genh
		#! gen_classes = addGenericClassInfo class_info gen_classes
		= writePtr gen_info_ptr {gen_info & gen_classes=gen_classes} gs_genh

	build_class_dictionaries :: !Int !CommonDefs !*GenericState -> (!CommonDefs, !*GenericState) 		
	build_class_dictionaries first_new_class_index common_defs  
			gs=:{gs_varh, gs_tvarh, gs_main_module, gs_symtab, gs_dcl_modules}
		#! class_defs = { x \\ x <-: common_defs.com_class_defs } // make unique copy
		#  type_defs = { x \\ x <-: common_defs.com_type_defs } // make unique copy
		#  cons_defs = { x \\ x <-: common_defs.com_cons_defs } // make unique copy
		#  selector_defs = { x \\ x <-: common_defs.com_selector_defs } // make unique copy
		#  (size_type_defs,type_defs) = usize type_defs 
		#! (new_type_defs, new_selector_defs, new_cons_defs,type_defs,selector_defs,cons_defs,class_defs, gs_dcl_modules, gs_tvarh, gs_varh, gs_symtab)
			= createMoreClassDictionaries
					first_new_class_index
					gs_main_module 
					size_type_defs
					(size common_defs.com_selector_defs) 
					(size common_defs.com_cons_defs) 
					type_defs selector_defs cons_defs class_defs 
					gs_dcl_modules gs_tvarh gs_varh gs_symtab

		#! common_defs = { common_defs & 
			com_class_defs = class_defs, 
			com_type_defs = arrayPlusList type_defs new_type_defs,
			com_selector_defs = arrayPlusList selector_defs new_selector_defs,
			com_cons_defs = arrayPlusList cons_defs new_cons_defs}

		# gs = { gs & gs_tvarh = gs_tvarh
					, gs_varh = gs_varh
					, gs_dcl_modules = gs_dcl_modules
					, gs_symtab = gs_symtab }
		= (common_defs, gs)

// limitations:
// - context restrictions on generic variables are not allowed
buildMemberType :: !GenericDef !TypeKind !TypeVar !*GenericState -> ( !SymbolType, !*GenericState)
buildMemberType gen_def=:{gen_ident,gen_pos,gen_type,gen_vars} kind class_var gs=:{gs_predefs}
	#! (gen_type, gs) = add_bimap_contexts gen_def gs 

	#! th = {th_vars = gs.gs_tvarh, th_attrs = gs.gs_avarh}
	#! (kind_indexed_st, gatvs, th, gs_error) 
		= buildKindIndexedType gen_type gen_vars kind gen_ident gen_pos th gs.gs_error

	#! (member_st, th, gs_error) 
		= replace_generic_vars_with_class_var kind_indexed_st gatvs th gs_error

	#! th = assertSymbolType member_st th // just paranoied about cleared variables
	#! th = assertSymbolType gen_type th
	
	# {th_vars, th_attrs} = th
	#! gs = {gs & gs_avarh = th_attrs, gs_tvarh = th_vars, gs_error = gs_error }
	= (member_st, gs)
where
	add_bimap_contexts 
			{gen_type=gen_type=:{st_vars, st_context}, gen_vars, gen_info_ptr} 
			gs=:{gs_predefs, gs_varh, gs_genh}
		#! ({gen_var_kinds}, gs_genh) = readPtr gen_info_ptr gs_genh	
		#! num_gen_vars = length gen_vars
		#! tvs = st_vars -- gen_vars
		#! kinds = drop num_gen_vars gen_var_kinds
		#! (bimap_contexts, gs_varh) = build_contexts tvs kinds gs_varh 
		
		#! gs = {gs & gs_varh = gs_varh, gs_genh = gs_genh}
		= ({gen_type & st_context = st_context ++ bimap_contexts}, gs)
	where
		build_contexts [] [] st 
			= ([], st)
		build_contexts [x:xs] [KindConst:kinds] st
			= build_contexts xs kinds st
		build_contexts [x:xs] [kind:kinds] st
			# (z, st) = build_context x kind st
			# (zs, st) = build_contexts xs kinds st
			= ([z:zs], st) 

		build_context tv kind gs_varh
			#! (var_info_ptr, gs_varh) = newPtr VI_Empty gs_varh
			#! {pds_module, pds_def} = gs_predefs . [PD_GenericBimap]
			#! pds_ident = predefined_idents . [PD_GenericBimap]
			# glob_def_sym = 
				{ glob_module = pds_module
				, glob_object = {ds_ident=pds_ident, ds_index=pds_def, ds_arity = 1}
				}
			# tc_class = TCGeneric 
				{ gtc_generic=glob_def_sym
				, gtc_kind = kind
				, gtc_class = {glob_module=NoIndex,glob_object={ds_ident=makeIdent "<no generic class>", ds_index=NoIndex, ds_arity=1}}
				, gtc_generic_dict = {gi_module=NoIndex, gi_index=NoIndex}
				}
			=({tc_class = tc_class, tc_types = [TV tv], tc_var = var_info_ptr}, gs_varh)	

	replace_generic_vars_with_class_var st atvs th error			
		#! th = subst_gvs atvs th
		#! (new_st, th) = applySubstInSymbolType st th
		= (new_st, th, error)
	where
		subst_gvs atvs th=:{th_vars, th_attrs}
			#! tvs = [atv_variable \\ {atv_variable} <- atvs ]
			#! avs = [av \\ {atv_attribute=TA_Var av} <- atvs ]
			
			# th_vars = foldSt subst_tv tvs th_vars

			// all generic vars get the same uniqueness variable
			# th_attrs = case avs of 
				[av:avs]	-> foldSt (subst_av av) avs th_attrs
				[] 			-> th_attrs

 			= { th & th_vars = th_vars, th_attrs = th_attrs }
		
		subst_tv {tv_info_ptr} th_vars
			= writePtr tv_info_ptr (TVI_Type (TV class_var)) th_vars

		subst_av av {av_info_ptr} th_attrs
			= writePtr av_info_ptr (AVI_Attr (TA_Var av)) th_attrs

buildClassAndMember :: Int Int Int TypeKind GenericDef *GenericState -> (ClassDef,MemberDef,*GenericState)
buildClassAndMember
		module_index class_index member_index kind
		gen_def=:{gen_ident, gen_pos}
		gs=:{gs_tvarh}
	# (class_var, gs_tvarh) = freshTypeVar (makeIdent "class_var") gs_tvarh
	#! (member_def, gs) 
		= build_class_member class_var {gs & gs_tvarh = gs_tvarh}	
	#! class_def = build_class class_var member_def
	= (class_def, member_def, gs)
where
	class_ident = genericIdentToClassIdent gen_def.gen_ident.id_name kind 
	member_ident = genericIdentToMemberIdent gen_def.gen_ident.id_name kind 
	class_ds = {ds_index = class_index, ds_ident = class_ident, ds_arity = 1}

	build_class_member class_var gs=:{gs_varh}
		#! (type_ptr, gs_varh) = newPtr VI_Empty gs_varh 
		#! (tc_var_ptr, gs_varh) = newPtr VI_Empty gs_varh 
		#! gs = {gs & gs_varh = gs_varh }
		#! type_context = 
			{	tc_class = TCClass {glob_module = module_index, glob_object=class_ds}
			,	tc_types = [TV class_var] 
			,	tc_var = tc_var_ptr 
			}
		#! (member_type, gs) 
			= buildMemberType gen_def kind class_var gs
		#! member_type = { member_type & st_context = [type_context : member_type.st_context] }
		#! member_def = {
			me_ident = member_ident, 
			me_class = {glob_module = module_index, glob_object = class_index},
			me_offset = 0,
			me_type = member_type,
			me_type_ptr = type_ptr,				// empty
			me_class_vars = [class_var], 		// the same variable as in the class
			me_pos = gen_pos,
			me_priority = NoPrio
			}
		= (member_def, gs)

	build_class class_var member_def=:{me_type}
		#! class_member = 
			{ ds_ident = member_ident
			, ds_index = member_index
			, ds_arity = me_type.st_arity
			}
		#! class_dictionary = 
			{ ds_ident = class_ident 
			, ds_arity = 0 
			, ds_index = NoIndex/*index in the type def table, filled in later*/ 
			}
		#! class_def = { 
			class_ident = class_ident, 
			class_arity = 1,  
			class_args = [class_var],
		    class_context = [], 
		    class_pos = gen_pos, 
		    class_members = createArray 1 class_member, 
		    class_cons_vars = 0, // dotted class variables
		    class_dictionary = class_dictionary
		    }
		= class_def

// Convert generic cases 

convertGenericCases :: !BimapFunctions !*GenericState -> (!IndexRange, !*GenericState)
convertGenericCases bimap_functions
		gs=:{gs_main_module, gs_used_modules, gs_predefs, gs_funs, gs_groups, gs_modules, gs_dcl_modules, gs_td_infos, 
			gs_avarh, gs_tvarh, gs_varh, gs_genh, gs_exprh,
			gs_error}

	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}	

	#! (first_fun_index, gs_funs) = usize gs_funs
	#! first_group_index = size gs_groups
	#! fun_info = {fg_fun_index=first_fun_index, fg_group_index=first_group_index, fg_funs=[], fg_groups=[], fg_bimap_functions=bimap_functions}

	#! (main_common_defs, gs_modules) = gs_modules ! [gs_main_module] 	
	#! main_module_instances = main_common_defs.com_instance_defs

	#! first_instance_index = size main_module_instances	
	#! instance_info = (first_instance_index, [])

	#! (gs_modules, gs_dcl_modules, (fun_info, instance_info, heaps, gs_error)) 
		= build_exported_main_instances_in_modules 0 gs_modules gs_dcl_modules (fun_info, instance_info, heaps, gs_error)

	#! first_main_instance_fun_index = fun_info.fg_fun_index

	#! (gs_modules, gs_dcl_modules, (fun_info, instance_info, gs_funs, gs_td_infos, heaps, gs_error)) 
		= build_main_instances_in_main_module gs_main_module gs_modules gs_dcl_modules (fun_info, instance_info, gs_funs, gs_td_infos, heaps, gs_error)

	#! first_shorthand_function_index = fun_info.fg_fun_index

	#! (gs_modules, gs_dcl_modules, (fun_info, instance_info, heaps, gs_error)) 
		= build_shorthand_instances_in_modules 0 gs_modules gs_dcl_modules (fun_info, instance_info, heaps, gs_error)
	
	#! {fg_fun_index, fg_funs=new_funs, fg_groups=new_groups} = fun_info
	#! gs_funs = arrayPlusRevList gs_funs new_funs
	#! gs_groups = arrayPlusRevList gs_groups new_groups

	#! (instance_index, new_instances) = instance_info
	#! com_instance_defs = arrayPlusRevList main_module_instances new_instances

	#! main_common_defs = {main_common_defs & com_instance_defs = com_instance_defs}	
	#! gs_modules = {gs_modules & [gs_main_module] = main_common_defs}
	
	#! instance_fun_range = {ir_from=first_main_instance_fun_index, ir_to=first_shorthand_function_index}

	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	# gs = {gs	& gs_modules = gs_modules
				, gs_dcl_modules = gs_dcl_modules
				, gs_td_infos = gs_td_infos
				, gs_funs = gs_funs
				, gs_groups = gs_groups
				, gs_error = gs_error
				, gs_avarh = th_attrs
				, gs_tvarh = th_vars
				, gs_varh = hp_var_heap
				, gs_genh = hp_generic_heap
				, gs_exprh = hp_expression_heap
		   }
	= (instance_fun_range, gs)
where
	build_exported_main_instances_in_modules :: !Index
			!*{#CommonDefs} !*{#DclModule} !(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
		-> (!*{#CommonDefs},!*{#DclModule},!(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
	build_exported_main_instances_in_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
		| not (inNumberSet module_index gs_used_modules) || module_index==gs_main_module
			= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
			#! (com_gencase_defs,modules) = modules![module_index].com_gencase_defs
			| size com_gencase_defs==0
				= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
			#! (dcl_functions,dcl_modules) = dcl_modules![module_index].dcl_functions
			#! (dcl_functions, modules, st)
				= build_exported_main_instances_in_module module_index com_gencase_defs {x\\x<-:dcl_functions} modules st  
			#! dcl_modules = {dcl_modules & [module_index].dcl_functions = dcl_functions} 
			= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
	where
		build_exported_main_instances_in_module module_index com_gencase_defs dcl_functions modules st
			= foldArraySt (build_exported_main_instance module_index) com_gencase_defs (dcl_functions, modules, st)

		build_exported_main_instance :: !Index !GenericCaseDef
				(!*{#FunType} ,!*Modules, !(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
			->	(!*{#FunType} ,!*Modules, !(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))					
		build_exported_main_instance module_index
				{gc_gcf=GCF gc_ident {gcf_body = GCB_FunIndex fun_index,gcf_kind,gcf_generic}, gc_type, gc_type_cons,gc_pos} 
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! has_generic_info = is_OBJECT_CONS_FIELD_type gc_type gs_predefs
			= build_exported_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
		build_exported_main_instance module_index
				{gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos}
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! has_generic_info = is_OBJECT_CONS_FIELD_type gc_type gs_predefs
			= build_exported_main_instances gcfs ins_type module_index gc_type_cons gc_pos has_generic_info
											dcl_functions modules st
		where
			build_exported_main_instances [!{gcf_body = GCB_FunIndex fun_index,gcf_generic,gcf_kind,gcf_gident}:gcfs!] ins_type module_index gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
				# (dcl_functions, modules, st)
					= build_exported_main_instance_ ins_type module_index gcf_gident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
				= build_exported_main_instances gcfs ins_type module_index gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
			build_exported_main_instances [!!] ins_type module_index gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
				= (dcl_functions, modules, st)

		build_exported_main_instance_ :: InstanceType Int Ident Int TypeKind GlobalIndex TypeCons Position Bool
				!*{#FunType} !*{#CommonDefs} !(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
			-> (!*{#FunType},!*{#CommonDefs},!(FunsAndGroups, !(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
		build_exported_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
								dcl_functions modules (fun_info, ins_info, heaps, error)
			#! (class_info, (modules, heaps)) = get_class_for_kind gcf_generic gcf_kind (modules, heaps)
			#! ({class_members}, modules) = modules![class_info.gci_module].com_class_defs.[class_info.gci_class]	
			#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_members.[0].ds_index]

			#! (fun_type, heaps, error)
				= determine_type_of_member_instance member_def ins_type heaps error

			#! fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

			| not has_generic_info
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type dcl_functions heaps

				# class_instance_member = {cim_ident=fun_ident,cim_arity=module_index,cim_index= -1-fun_index}
				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, modules, (fun_info, ins_info, heaps, error))

				# (fun_type_with_generic_info,type_heaps)
					= add_generic_info_to_type fun_type gs_predefs heaps.hp_type_heaps
				# heaps = {heaps & hp_type_heaps=type_heaps}

				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type_with_generic_info dcl_functions heaps

				#! ({ds_ident,ds_arity,ds_index}, fun_info, heaps)
					= build_instance_member_with_generic_info module_index gc_ident gc_pos gcf_kind fun_ident fun_index fun_type gs_predefs fun_info heaps
				# class_instance_member = {cim_ident=ds_ident,cim_arity=ds_arity,cim_index=ds_index}

				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, modules, (fun_info, ins_info, heaps, error))

	build_main_instances_in_main_module :: !Index
			!*{#CommonDefs} !*{#DclModule} !(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
		-> (!*{#CommonDefs},!*{#DclModule},!(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
	build_main_instances_in_main_module gs_main_module modules dcl_modules st
		#! (com_gencase_defs,modules) = modules![gs_main_module].com_gencase_defs
		| size com_gencase_defs==0
			= (modules,dcl_modules,st)
		#! (dcl_functions,dcl_modules) = dcl_modules![gs_main_module].dcl_functions
		#! (dcl_functions, modules, st)
			= foldArraySt (build_main_instance gs_main_module) com_gencase_defs ({x\\x<-:dcl_functions}, modules, st)
		#! dcl_modules = {dcl_modules & [gs_main_module].dcl_functions = dcl_functions} 
		= (modules,dcl_modules,st)
	where
		build_main_instance :: !Index !GenericCaseDef
				(!*{#FunType}, !*Modules, !(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
			->	(!*{#FunType}, !*Modules, !(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))					
		build_main_instance module_index
				{gc_gcf=GCF gc_ident {gcf_body = GCB_FunIndex fun_index,gcf_kind,gcf_generic}, gc_type, gc_type_cons,gc_pos} 
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! has_generic_info = is_OBJECT_CONS_FIELD_type gc_type gs_predefs
			= build_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
		build_main_instance module_index
				{gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos}
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! has_generic_info = is_OBJECT_CONS_FIELD_type gc_type gs_predefs
			= build_main_instances gcfs ins_type module_index gc_type_cons gc_pos has_generic_info dcl_functions modules st
		where
			build_main_instances [!{gcf_body = GCB_FunIndex fun_index,gcf_generic,gcf_kind,gcf_gident}:gcfs!] ins_type module_index gc_type_cons gc_pos has_generic_info
									dcl_functions modules st
				# (dcl_functions, modules, st)
					= build_main_instance_ ins_type module_index gcf_gident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
											dcl_functions modules st
				= build_main_instances gcfs ins_type module_index gc_type_cons gc_pos has_generic_info dcl_functions modules st
			build_main_instances [!!] ins_type module_index gc_type_cons gc_pos has_generic_info dcl_functions modules st
				= (dcl_functions, modules, st)

		build_main_instance_ :: InstanceType Int Ident Int TypeKind GlobalIndex TypeCons Position Bool
				!*{#FunType} !*{#CommonDefs} !(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
			-> (!*{#FunType},!*{#CommonDefs},!(FunsAndGroups, !(!Index, ![ClassInstance]), !*{#FunDef}, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
		build_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos has_generic_info
								dcl_functions modules (fun_info,ins_info,fun_defs,td_infos,heaps,error)
			#! (class_info, (modules, heaps)) = get_class_for_kind gcf_generic gcf_kind (modules, heaps)
			#! ({class_members}, modules) = modules![class_info.gci_module].com_class_defs.[class_info.gci_class]	
			#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_members.[0].ds_index]

			#! (fun_type, heaps, error)
				= determine_type_of_member_instance member_def ins_type heaps error

			#! fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

			| not has_generic_info
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type dcl_functions heaps

				#! (fun_info, fun_defs, td_infos, modules, heaps, error)
					= update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident has_generic_info gcf_generic fun_type
						fun_info fun_defs td_infos modules heaps error

				# class_instance_member = {cim_ident=fun_ident,cim_arity=module_index,cim_index= -1-fun_index}
				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, modules, (fun_info, ins_info, fun_defs, td_infos, heaps, error))

				# (fun_type_with_generic_info,type_heaps)
					= add_generic_info_to_type fun_type gs_predefs heaps.hp_type_heaps
				# heaps = {heaps & hp_type_heaps=type_heaps}

				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type_with_generic_info dcl_functions heaps

				#! (fun_info, fun_defs, td_infos, modules, heaps, error)
					= update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident has_generic_info gcf_generic fun_type_with_generic_info
						fun_info fun_defs td_infos modules heaps error

				#! ({ds_ident,ds_arity,ds_index}, fun_info, heaps)
					= build_instance_member_with_generic_info module_index gc_ident gc_pos gcf_kind fun_ident fun_index fun_type gs_predefs fun_info heaps
				# class_instance_member = {cim_ident=ds_ident,cim_arity=ds_arity,cim_index=ds_index}
	
				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, modules, (fun_info, ins_info, fun_defs, td_infos, heaps, error))

	instance_vars_from_type_cons (TypeConsVar tv)
		= [tv]
	instance_vars_from_type_cons _
		= []

	build_shorthand_instances_in_modules :: !Index
			!*{#CommonDefs} !*{#DclModule} (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
		-> (!*{#CommonDefs}, *{#DclModule},(FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
	build_shorthand_instances_in_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
		| not (inNumberSet module_index gs_used_modules)
			= build_shorthand_instances_in_modules (module_index+1) modules dcl_modules st
			#! (com_gencase_defs,modules) = modules![module_index].com_gencase_defs
			#! (modules, st)
				= build_shorthand_instances_in_module module_index com_gencase_defs modules st  
			= build_shorthand_instances_in_modules (module_index+1) modules dcl_modules st
	where
		build_shorthand_instances_in_module module_index com_gencase_defs modules st
			= foldArraySt (build_shorthand_instances module_index) com_gencase_defs (modules, st)

	build_shorthand_instances :: !Index !GenericCaseDef
			(!*Modules, (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
		->	(!*Modules, (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))					
	build_shorthand_instances module_index {gc_gcf=GCF gc_ident {gcf_kind=KindConst}} st	
		= st
	build_shorthand_instances module_index {gc_gcf=GCF gc_ident {gcf_kind=KindArrow kinds,gcf_generic,gcf_body},gc_type,gc_type_cons,gc_pos} st
		= build_shorthand_instance_for_kinds gc_ident kinds gcf_generic gcf_body gc_type gc_type_cons gc_pos module_index st
	build_shorthand_instances module_index {gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos} st
		= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
	where
		build_shorthand_instances_for_generic_superclasses [!{gcf_kind=KindConst}:gcfs!] module_index gc_type gc_type_cons gc_pos st
			= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
		build_shorthand_instances_for_generic_superclasses [!{gcf_kind=KindArrow kinds,gcf_generic,gcf_body,gcf_gident}:gcfs!] module_index gc_type gc_type_cons gc_pos st
			# st = build_shorthand_instance_for_kinds gcf_gident kinds gcf_generic gcf_body gc_type gc_type_cons gc_pos module_index st
			= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
		build_shorthand_instances_for_generic_superclasses [!!] module_index gc_type gc_type_cons gc_pos st
			= st

	build_shorthand_instance_for_kinds gc_ident kinds gcf_generic (GCB_FunIndex fun_index) gc_type gc_type_cons gc_pos module_index st
		= foldSt (build_shorthand_instance gc_ident kinds gcf_generic fun_index gc_type gc_type_cons gc_pos module_index) [1 .. length kinds] st

	build_shorthand_instance gc_ident kinds gcf_generic fun_index gc_type gc_type_cons gc_pos module_index num_args
			(modules, (fun_info, ins_info, heaps, error))

		#! (consumed_kinds, rest_kinds) = splitAt num_args kinds 		
		#! this_kind = case rest_kinds of
			[] -> KindConst
			_  -> KindArrow rest_kinds 

		#! (class_info, (modules, heaps)) = get_class_for_kind gcf_generic this_kind (modules, heaps)
		#! (arg_class_infos, (modules, heaps)) 
			= mapSt (get_class_for_kind gcf_generic) consumed_kinds (modules, heaps)
		#! ({class_members}, modules) = modules![class_info.gci_module].com_class_defs.[class_info.gci_class]	
		#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_members.[0].ds_index]

		#! (ins_type, heaps)
			= build_instance_type gc_type arg_class_infos heaps
		#! (fun_type, heaps, error)
			= determine_type_of_member_instance member_def ins_type heaps error

		# fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

		#! has_generic_info = is_OBJECT_CONS_FIELD_type gc_type gs_predefs

		#! (memfun_ds, fun_info, heaps) 
			= build_shorthand_instance_member module_index this_kind gcf_generic has_generic_info fun_index fun_ident gc_pos fun_type arg_class_infos fun_info heaps
		
		#! ins_info 
			= build_shorthand_class_instance this_kind class_info.gci_class gc_ident gc_pos memfun_ds ins_type ins_info

		= (modules, (fun_info, ins_info, heaps, error))
	where
		build_instance_type type class_infos heaps=:{hp_type_heaps=th=:{th_vars},hp_var_heap}
			#! arity = length class_infos
			#! type_var_names = [makeIdent ("a" +++ toString i) \\ i <- [1 .. arity]]
			#! (type_vars, th_vars) = mapSt freshTypeVar type_var_names th_vars 
			#! type_var_types = [TV tv \\ tv <- type_vars] 	
			#! new_type_args = [makeAType t TA_Multi \\ t <- type_var_types]
			
			#! type = fill_type_args type new_type_args	
			
			#! (contexts, hp_var_heap) 
				= zipWithSt build_context class_infos type_vars hp_var_heap
			
			#! ins_type = 
				{	it_vars	= type_vars
				,	it_types = [type]
				,	it_attr_vars = []
				,	it_context = contexts
				}
			= (ins_type, {heaps & hp_type_heaps = {th & th_vars = th_vars}, hp_var_heap = hp_var_heap})
		where
			fill_type_args (TA type_symb_ident=:{type_arity} type_args) new_type_args
				#! type_arity = type_arity + length new_type_args 
				#! type_args = type_args ++ new_type_args
				= TA {type_symb_ident & type_arity = type_arity} type_args 
			fill_type_args TArrow [arg_type, res_type]
				= arg_type --> res_type
			fill_type_args TArrow [arg_type]
				= TArrow1 arg_type	
			fill_type_args (TArrow1 arg_type) [res_type]
				= arg_type --> res_type	 
			fill_type_args type args
				= abort ("fill_type_args\n"---> ("fill_type_args", type, args)) 

			build_context {gci_class, gci_module, gci_kind} tv hp_var_heap
				# (var_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap			
				# type_context =		
					{	tc_class = TCClass
							{ glob_module=gci_module // the same as icl module
							, glob_object =
								{ ds_ident = genericIdentToClassIdent gc_ident.id_name gci_kind
								, ds_index = gci_class
								, ds_arity = 1
								}
							}
					,	tc_types = [TV tv]
					,	tc_var	 = var_info_ptr
					}
				= (type_context, hp_var_heap)	

		build_shorthand_instance_member module_index this_kind gcf_generic has_generic_info fun_index fun_ident gc_pos st class_infos fun_info heaps
			#! arg_var_names = ["x" +++ toString i \\ i <- [1..st.st_arity]]
			#! (arg_var_exprs, arg_vars, heaps) = buildVarExprs arg_var_names heaps
					
			#! (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
			#! heaps = {heaps & hp_expression_heap = hp_expression_heap}
			#! fun_name = genericIdentToMemberIdent gc_ident.id_name this_kind
	
			# (gen_exprs, heaps) = mapSt (build_generic_app gcf_generic gc_ident) class_infos heaps
	
			#! arg_exprs = gen_exprs ++ arg_var_exprs
			# (body_expr, heaps)
				= if has_generic_info
					(let (generic_info_expr, heaps2) = buildPredefConsApp PD_NoGenericInfo [] gs_predefs heaps			
					 in buildFunApp2 module_index fun_index fun_ident [generic_info_expr:arg_exprs] heaps2)
					(buildFunApp2 module_index fun_index fun_ident arg_exprs heaps)
	
			#! (st, heaps) = fresh_symbol_type st heaps

			#! (fun_ds, fun_info) 
				= buildFunAndGroup fun_name arg_vars body_expr (Yes st) gs_main_module gc_pos fun_info

			= (fun_ds, fun_info, heaps)
		where
			build_generic_app {gi_module, gi_index} gc_ident {gci_kind} heaps
				= buildGenericApp gi_module gi_index gc_ident gci_kind [] heaps

		build_shorthand_class_instance this_kind class_index gc_ident gc_pos {ds_ident,ds_arity,ds_index} ins_type (ins_index, instances)
			#! class_ident = genericIdentToClassIdent gc_ident.id_name this_kind
			#! ins = 
			 	{	ins_class_index = {gi_module=gs_main_module, gi_index=class_index}
			 	,	ins_class_ident = {ci_ident=Ident class_ident, ci_arity=1}
				,	ins_ident 	= class_ident
				,	ins_type 	= ins_type
				,	ins_member_types = []
				,	ins_members	= {{cim_ident=ds_ident,cim_arity=ds_arity,cim_index=ds_index}}
				,	ins_specials = SP_None
				,	ins_pos		= gc_pos
				}
			= (ins_index+1, [ins:instances])

	get_class_for_kind :: !GlobalIndex !TypeKind !(!*{#CommonDefs},!*Heaps) -> (!GenericClassInfo,!(!*{#CommonDefs},!*Heaps))
	get_class_for_kind {gi_module, gi_index} kind (modules,heaps=:{hp_generic_heap})
		#! ({gen_info_ptr}, modules) = modules![gi_module].com_generic_defs.[gi_index]
		#! ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		# (Yes class_info) = lookupGenericClassInfo kind gen_classes
		= (class_info, (modules, heaps))	

	determine_type_of_member_instance :: !MemberDef !InstanceType !*Heaps !*ErrorAdmin
		-> (!SymbolType, !*Heaps, !*ErrorAdmin)
	determine_type_of_member_instance {me_type, me_class_vars} ins_type heaps=:{hp_type_heaps, hp_var_heap} error
		#! (symbol_type, _, hp_type_heaps, _, error) 
			= determineTypeOfMemberInstance me_type me_class_vars ins_type SP_None hp_type_heaps No error
		#! (st_context, hp_var_heap) = initializeContextVariables symbol_type.st_context hp_var_heap
		#! hp_type_heaps = clearSymbolType me_type hp_type_heaps
		#! symbol_type = {symbol_type & st_context = st_context}
		#! heaps = {heaps & hp_type_heaps = hp_type_heaps, hp_var_heap = hp_var_heap}
		= (symbol_type, heaps, error)

	update_dcl_function :: !Index !Ident !SymbolType !*{#FunType} !*Heaps -> (!*{#FunType}, !*Heaps)
	update_dcl_function fun_index fun_ident symbol_type dcl_functions heaps 
		| fun_index < size dcl_functions
			#! (symbol_type, heaps) = fresh_symbol_type symbol_type heaps			
			#! (fun, dcl_functions) = dcl_functions![fun_index]
			#! fun = {fun	& ft_ident = fun_ident
							, ft_type = symbol_type
							, ft_arity = symbol_type.st_arity}
			#! dcl_functions = {dcl_functions & [fun_index] = fun}
			= (dcl_functions, heaps)
			= (dcl_functions, heaps)

	update_icl_function :: !Index !Ident !Position !TypeCons !Ident !Bool !GlobalIndex !SymbolType
			!FunsAndGroups !*{#FunDef} !*TypeDefInfos !*{#CommonDefs} !*Heaps !*ErrorAdmin 
		-> (!FunsAndGroups,!*{#FunDef},!*TypeDefInfos,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
	update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident has_generic_info gcf_generic st funs_and_groups fun_defs td_infos modules heaps error
		#! (st, heaps) = fresh_symbol_type st heaps
		#! (fun=:{fun_body, fun_arity}, fun_defs) = fun_defs![fun_index] 		
		= case fun_body of
			TransformedBody {tb_args,tb_rhs}	// user defined case
				| has_generic_info
					| fun_arity<>st.st_arity
						# error = reportError gc_ident.id_name gc_pos
									("incorrect arity "+++toString (fun_arity-1)+++", expected "+++toString (st.st_arity-1)) error
						-> (funs_and_groups, fun_defs, td_infos, modules, heaps, error)	
					#! fun = {fun & fun_ident = fun_ident, fun_type = Yes st}
					#! fun_defs = {fun_defs & [fun_index] = fun}
					-> (funs_and_groups, fun_defs, td_infos, modules, heaps, error)
					# fun_body = TransformedBody {tb_args = tl tb_args, tb_rhs = tb_rhs}
					| fun_arity-1<>st.st_arity
						# error = reportError gc_ident.id_name gc_pos
									("incorrect arity "+++toString (fun_arity-1)+++", expected "+++toString st.st_arity) error
						-> (funs_and_groups, fun_defs, td_infos, modules, heaps, error)	
					#! fun = {fun & fun_ident = fun_ident, fun_body = fun_body, fun_type = Yes st}
					#! fun_defs = {fun_defs & [fun_index] = fun}
					-> (funs_and_groups, fun_defs, td_infos, modules, heaps, error)
			GeneratedBody		// derived case
				#! (TransformedBody {tb_args, tb_rhs}, funs_and_groups, td_infos, modules, heaps, error)  
					= buildGenericCaseBody gs_main_module gc_pos gc_type_cons gc_ident has_generic_info gcf_generic st gs_predefs funs_and_groups td_infos modules heaps error
				# {fg_group_index,fg_groups} = funs_and_groups
				#! fun = makeFunction fun_ident fg_group_index tb_args tb_rhs (Yes st) gs_main_module gc_pos
				#! fun_defs = {fun_defs & [fun_index] = fun}
				# group = {group_members=[fun_index]}
				  funs_and_groups = {funs_and_groups & fg_group_index=fg_group_index+1,fg_groups=[group:fg_groups]}
				-> (funs_and_groups, fun_defs, td_infos, modules, heaps, error)

	build_class_instance :: Int Ident Position TypeKind ClassInstanceMember InstanceType !(!Int,![ClassInstance]) -> (!Int,![ClassInstance])
	build_class_instance class_index gc_ident gc_pos gc_kind class_instance_member ins_type (ins_index, instances)
		# class_ident = genericIdentToClassIdent gc_ident.id_name gc_kind
		#! ins =
		 	{	ins_class_index = {gi_module=gs_main_module, gi_index=class_index}
		 	,	ins_class_ident = {ci_ident=Ident class_ident, ci_arity=1}
			,	ins_ident 	= class_ident
			,	ins_type 	= ins_type
			,	ins_member_types = []
			,	ins_members	= {class_instance_member}
			,	ins_specials = SP_None
			,	ins_pos		= gc_pos
			}
		= (ins_index+1, [ins:instances])

	// Creates a function that just calls the generic case function, but with an extra NoGenericInfo argument
	build_instance_member_with_generic_info module_index gc_ident gc_pos gcf_kind fun_ident fun_index st predefs fun_info heaps		
		#! arg_var_names = ["x" +++ toString i \\ i <- [1..st.st_arity]]
		#! (arg_var_exprs, arg_vars, heaps) = buildVarExprs arg_var_names heaps

		# (generic_info_expr, heaps) = buildPredefConsApp PD_NoGenericInfo [] predefs heaps
		# arg_var_exprs = [generic_info_expr:arg_var_exprs]

		#! (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
		#! heaps = {heaps & hp_expression_heap = hp_expression_heap}
		#! expr = App 
			{ app_symb = 
				{ symb_ident=fun_ident
				, symb_kind=SK_Function {glob_module=module_index, glob_object=fun_index}
				}
			, app_args = arg_var_exprs
			, app_info_ptr = expr_info_ptr
			}		
		#! (st, heaps) = fresh_symbol_type st heaps
		#! memfun_name = genericIdentToMemberIdent gc_ident.id_name gcf_kind
		#! (fun_ds, fun_info) 
			= buildFunAndGroup memfun_name arg_vars expr (Yes st) gs_main_module gc_pos fun_info
		= (fun_ds, fun_info, heaps)

	fresh_symbol_type :: !SymbolType !*Heaps -> (!SymbolType, !*Heaps)	
	fresh_symbol_type st heaps=:{hp_type_heaps}
		# (fresh_st, hp_type_heaps) = freshSymbolType st hp_type_heaps
		= (fresh_st, {heaps & hp_type_heaps = hp_type_heaps})	

// add an argument for generic info at the beginning
add_generic_info_to_type :: !SymbolType !{#PredefinedSymbol} !*TypeHeaps -> (!SymbolType,!*TypeHeaps)
add_generic_info_to_type st=:{st_arity, st_args, st_args_strictness} predefs th=:{th_vars}
	#! {pds_module, pds_def} = predefs.[PD_GenericInfo]
	#! pds_ident = predefined_idents.[PD_GenericInfo]
	#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} pds_ident 0
	#! st = {st & st_args = [makeAType (TA type_symb []) TA_Multi : st_args]
				, st_arity = st_arity + 1
				, st_args_strictness = insert_n_lazy_values_at_beginning 1 st_args_strictness	 
			}
	= (st, {th & th_vars = th_vars})

index_OBJECT_CONS_FIELD_type :: !Type !{#PredefinedSymbol} -> Int
index_OBJECT_CONS_FIELD_type (TA {type_index={glob_module,glob_object}} []) predefs
	# {pds_module,pds_def} = predefs.[PD_TypeOBJECT]
	| glob_module==pds_module && pds_def==glob_object
		= 0
	# {pds_module,pds_def} = predefs.[PD_TypeCONS]
	| glob_module==pds_module && pds_def==glob_object
		= 1
	# {pds_module,pds_def} = predefs.[PD_TypeFIELD]
	| glob_module==pds_module && pds_def==glob_object
		= 2
		= -1
index_OBJECT_CONS_FIELD_type _ predefs
	= -1

is_OBJECT_CONS_FIELD_type :: !Type !{#PredefinedSymbol} -> Bool
is_OBJECT_CONS_FIELD_type (TA {type_index={glob_module,glob_object}} []) predefs
	# {pds_module,pds_def} = predefs.[PD_TypeOBJECT]
	| glob_module==pds_module && pds_def==glob_object
		= True
	# {pds_module,pds_def} = predefs.[PD_TypeCONS]
	| glob_module==pds_module && pds_def==glob_object
		= True
	# {pds_module,pds_def} = predefs.[PD_TypeFIELD]
	| glob_module==pds_module && pds_def==glob_object
		= True
		= False
is_OBJECT_CONS_FIELD_type _ predefs
	= False

buildGenericCaseBody :: 
		!Index					// current icl module
		!Position !TypeCons !Ident !Bool !GlobalIndex
		!SymbolType				// type of the instance function 
		!PredefinedSymbols
		!FunsAndGroups !*TypeDefInfos !*{#CommonDefs} !*Heaps !*ErrorAdmin 
	-> (!FunctionBody,
		!FunsAndGroups, !*TypeDefInfos,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
buildGenericCaseBody main_module_index gc_pos (TypeConsSymb {type_ident,type_index}) gc_ident has_generic_info gcf_generic st predefs
					funs_and_groups td_infos modules heaps error
	#! (gen_def, modules) = modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]
	#! (td_info=:{tdi_gen_rep}, td_infos) = td_infos![type_index.glob_module, type_index.glob_object]
	# (gen_type_rep=:{gtr_type}) = case tdi_gen_rep of
		Yes x -> x
		No -> abort "sanity check: no generic representation\n"

	#! (type_def=:{td_args, td_arity}, modules) = modules![type_index.glob_module].com_type_defs.[type_index.glob_object]
	#! (generated_arg_exprs, original_arg_exprs, arg_vars, heaps)
		= build_arg_vars gen_def td_args heaps

	# (arg_vars,heaps)
		= case has_generic_info of
			True
				# (generic_info_var, heaps) = build_generic_info_arg heaps
				#! arg_vars = [generic_info_var:arg_vars]
				-> (arg_vars,heaps)
			False
				-> (arg_vars,heaps)

	#! (specialized_expr, funs_and_groups, td_infos, heaps, error)
		= build_specialized_expr gc_pos gc_ident gcf_generic gtr_type td_args generated_arg_exprs gen_def.gen_info_ptr funs_and_groups td_infos heaps error

	#! (body_expr, funs_and_groups, modules, td_infos, heaps, error)
		= adapt_specialized_expr gc_pos gen_def gen_type_rep original_arg_exprs specialized_expr funs_and_groups modules td_infos heaps error

	= (TransformedBody {tb_args=arg_vars, tb_rhs=body_expr}, funs_and_groups, td_infos, modules, heaps, error)	
where
	build_generic_info_arg heaps=:{hp_var_heap}
		// generic arg is never referenced in the generated body
		#! (fv_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap
		#! fv = {fv_count = 0, fv_ident = makeIdent "geninfo", fv_info_ptr = fv_info_ptr, fv_def_level = NotALevel}	
		= (fv, {heaps & hp_var_heap = hp_var_heap})

	build_arg_vars {gen_ident, gen_vars, gen_type} td_args heaps 
		#! (generated_arg_exprs, generated_arg_vars, heaps) 
			= buildVarExprs 
				[ gen_ident.id_name +++ atv_variable.tv_ident.id_name \\ {atv_variable} <- td_args] 
				heaps	
		#! (original_arg_exprs, original_arg_vars, heaps) 
			= buildVarExprs 
				[ "x" +++ toString n \\ n <- [1 .. gen_type.st_arity]] 
				heaps	
		= (generated_arg_exprs, original_arg_exprs, generated_arg_vars ++ original_arg_vars, heaps)

	// generic function specialized to the generic representation of the type
	build_specialized_expr gc_pos gc_ident gcf_generic gtr_type td_args generated_arg_exprs gen_info_ptr funs_and_groups td_infos heaps error
		#! spec_env = [(atv_variable, TVI_Expr False expr) \\ {atv_variable} <- td_args & expr <- generated_arg_exprs]
		# generic_bimap = predefs.[PD_GenericBimap]
		| gcf_generic.gi_module==generic_bimap.pds_module && gcf_generic.gi_index==generic_bimap.pds_def

			// JvG: can probably make special version of simplify_bimap_GenTypeStruct that doesn't simplify if any var occurs, because all vars are passed
			# (gtr_type, heaps) = simplify_bimap_GenTypeStruct [atv_variable \\ {atv_variable} <- td_args] gtr_type heaps

			# (expr,funs_and_groups,heaps,error)
				= specialize_generic_bimap gcf_generic gtr_type spec_env gc_ident gc_pos main_module_index predefs funs_and_groups heaps error
			= (expr,funs_and_groups,td_infos,heaps,error)

			# ({gen_OBJECT_CONS_FIELD_indices},generic_heap) = readPtr gen_info_ptr heaps.hp_generic_heap
			  heaps = {heaps & hp_generic_heap=generic_heap}

			# (expr,td_infos,heaps,error)
				= specializeGeneric gcf_generic gtr_type spec_env gc_ident gc_pos gen_OBJECT_CONS_FIELD_indices main_module_index td_infos heaps error
			= (expr,funs_and_groups,td_infos,heaps,error)

	// adaptor that converts a function for the generic representation into a 
	// function for the type itself
	adapt_specialized_expr :: Position GenericDef GenericTypeRep [Expression] Expression
						!FunsAndGroups !*Modules !*TypeDefInfos !*Heaps !*ErrorAdmin
		-> (!Expression,!FunsAndGroups,!*Modules,!*TypeDefInfos,!*Heaps,!*ErrorAdmin)
	adapt_specialized_expr gc_pos {gen_type, gen_vars, gen_info_ptr} {gtr_iso,gtr_to,gtr_from} original_arg_exprs specialized_expr
			funs_and_groups modules td_infos heaps error
		#! (var_kinds, heaps) = get_var_kinds gen_info_ptr heaps		
		#! non_gen_var_kinds = drop (length gen_vars) var_kinds  
		
		#! non_gen_vars = gen_type.st_vars -- gen_vars	
		#! (gen_env, heaps) 
			= build_gen_env gtr_iso gtr_to gtr_from gen_vars heaps
		#! (non_gen_env, funs_and_groups, heaps)
			= build_non_gen_env non_gen_vars non_gen_var_kinds funs_and_groups heaps
		#! spec_env = gen_env ++ non_gen_env	
		#! curried_gen_type = curry_symbol_type gen_type

		#! (struct_gen_type, (modules, td_infos, heaps, error))
			= convert_bimap_AType_to_GenTypeStruct curried_gen_type gc_pos predefs (modules, td_infos, heaps, error)  

		#! (struct_gen_type, heaps) = simplify_bimap_GenTypeStruct gen_vars struct_gen_type heaps

		# bimap_gi = {gi_module=bimap_module,gi_index=bimap_index}
		#! (body_expr, funs_and_groups, modules, heaps, error)
			= adapt_with_specialized_generic_bimap bimap_gi struct_gen_type spec_env bimap_ident gc_pos original_arg_exprs specialized_expr main_module_index predefs 
						funs_and_groups modules heaps error

		= (body_expr, funs_and_groups, modules, td_infos, heaps, error)
	where
		{pds_module = bimap_module, pds_def=bimap_index} = predefs.[PD_GenericBimap]
		bimap_ident = predefined_idents.[PD_GenericBimap]
		
		get_var_kinds gen_info_ptr heaps=:{hp_generic_heap}
			#! ({gen_var_kinds}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
			= (gen_var_kinds, {heaps & hp_generic_heap = hp_generic_heap})
		
		curry_symbol_type {st_args, st_result}
			= foldr (\x y -> makeAType (x --> y) TA_Multi) st_result st_args 	
	
		build_gen_env :: !DefinedSymbol !DefinedSymbol !DefinedSymbol ![TypeVar] !*Heaps -> (![(!TypeVar, !TypeVarInfo)], !*Heaps)
		build_gen_env gtr_iso gtr_to gtr_from gen_vars heaps 
			= mapSt build_iso_expr gen_vars heaps
		where
			build_iso_expr gen_var heaps 
				= ((gen_var, TVI_Iso gtr_iso gtr_to gtr_from), heaps)

		build_non_gen_env :: ![TypeVar] ![TypeKind] FunsAndGroups !*Heaps -> (![(!TypeVar, !TypeVarInfo)], !FunsAndGroups, !*Heaps)
		build_non_gen_env non_gen_vars kinds funs_and_groups heaps
			= zipWithSt2 build_bimap_expr non_gen_vars kinds funs_and_groups heaps
		where
			// build application of generic bimap for a specific kind
			build_bimap_expr non_gen_var KindConst funs_and_groups heaps
				# (expr, funs_and_groups, heaps)
					= bimap_id_expression main_module_index predefs funs_and_groups heaps
				= ((non_gen_var, TVI_Expr True expr), funs_and_groups, heaps)
			build_bimap_expr non_gen_var kind funs_and_groups heaps
				#! (expr, heaps)
					= buildGenericApp bimap_module bimap_index bimap_ident kind [] heaps		
				= ((non_gen_var, TVI_Expr False expr), funs_and_groups, heaps)

buildGenericCaseBody main_module_index gc_pos gc_type_cons gc_ident has_generic_info gcf_generic st predefs funs_and_groups td_infos modules heaps error
	# error = reportError gc_ident.id_name gc_pos "cannot specialize to this type" error
	= (TransformedBody {tb_args=[], tb_rhs=EE}, funs_and_groups, td_infos, modules, heaps, error)

//  convert generic type contexts into normal type contexts

convertGenericTypeContexts :: !*GenericState -> *GenericState
convertGenericTypeContexts 
		gs=:{gs_main_module, gs_used_modules, gs_predefs, gs_funs, gs_modules, gs_dcl_modules, gs_error,
			gs_avarh, gs_tvarh, gs_exprh, gs_varh, gs_genh}
	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}	

	# (gs_funs, (gs_modules, heaps, gs_error)) = convert_functions 0 gs_funs (gs_modules, heaps, gs_error)

	# (gs_modules, gs_dcl_modules, (heaps, gs_error)) = convert_modules 0 gs_modules gs_dcl_modules (heaps, gs_error)

	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps

	 = {gs	& gs_funs = gs_funs
			, gs_modules = gs_modules
			, gs_dcl_modules = gs_dcl_modules
			, gs_error = gs_error
			, gs_avarh = th_attrs
			, gs_tvarh = th_vars
			, gs_varh = hp_var_heap
			, gs_genh = hp_generic_heap
			, gs_exprh = hp_expression_heap
		}
where
	convert_functions fun_index funs st
		| fun_index == size funs 
			= (funs, st)
			# (fun, funs) = funs ! [fun_index]
			# (fun, st) = convert_function fun st
 			# funs = {funs & [fun_index] = fun}
			= convert_functions (inc fun_index) funs st
	where
		convert_function :: !FunDef !(!*Modules, !*Heaps, !*ErrorAdmin) 
						-> (!FunDef,!(!*Modules, !*Heaps, !*ErrorAdmin))
		convert_function fun=:{fun_type=Yes symbol_type, fun_ident, fun_pos} st
			# (has_converted_context, symbol_type, st) = convert_contexts_in_symbol_type fun_ident fun_pos symbol_type st
			| has_converted_context
				# fun = {fun & fun_type = Yes symbol_type}
				= (fun, st)
				= (fun, st)		 
		convert_function fun st
			= (fun, st)

	convert_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
			# (modules, dcl_modules, st) = convert_module module_index modules dcl_modules st
			= convert_modules (inc module_index) modules dcl_modules st
	
	convert_module :: !Index !*Modules !*DclModules (!*Heaps,!*ErrorAdmin)
						 -> (!*Modules,!*DclModules,(!*Heaps,!*ErrorAdmin))
	convert_module module_index modules dcl_modules st
		| inNumberSet module_index gs_used_modules		 
			#! (common_defs, modules) = modules ! [module_index]
			#! (dcl_module=:{dcl_functions, dcl_common}, dcl_modules) = dcl_modules ! [module_index]
			
			#! (common_defs, modules, st) = convert_common_defs common_defs modules st
			#! (dcl_common, modules, st) = convert_common_defs dcl_common modules st
			#! (dcl_functions, modules, st) = convert_dcl_functions {x\\x<-:dcl_functions} modules st

			# dcl_modules = {dcl_modules & [module_index] = {dcl_module & dcl_functions = dcl_functions, dcl_common = dcl_common}}
			# modules = {modules & [module_index] = common_defs}
			= (modules, dcl_modules, st)
		| otherwise
			= (modules, dcl_modules, st)	
	
	convert_common_defs common_defs=:{com_class_defs,com_member_defs,com_instance_defs,com_cons_defs} modules (heaps, error)	
		# (com_class_defs, st) 
			= updateArraySt convert_class {x\\x<-:com_class_defs} (modules, heaps, error)
		# (com_member_defs, st)
			= updateArraySt convert_member {x\\x<-:com_member_defs} st
		# (com_instance_defs, st)
			= updateArraySt convert_instance {x\\x<-:com_instance_defs} st
		# (com_cons_defs, (modules, heaps, error))
			= updateArraySt convert_constructor {x\\x<-:com_cons_defs} st

		# common_defs = { common_defs
			& com_class_defs = com_class_defs
			, com_member_defs = com_member_defs
			, com_instance_defs = com_instance_defs
			, com_cons_defs = com_cons_defs
			}
		= (common_defs, modules, (heaps, error))
	where
		convert_class class_def=:{class_ident, class_pos, class_context} st
			# (ok, class_context, st) = convert_contexts class_ident class_pos class_context st
			| ok 
				# class_def={class_def & class_context = class_context}
				= (class_def, st) 	
				= (class_def, st) 	

		convert_member member_def=:{me_ident, me_pos, me_type} st
			# (ok, me_type, st) = convert_contexts_in_symbol_type me_ident me_pos me_type st
			| ok 
				# member_def={member_def & me_type = me_type}
				= (member_def, st) 	
				= (member_def, st) 	
									
		convert_instance ins=:{ins_type=ins_type=:{it_context}, ins_ident, ins_pos} st
			# (ok, it_context, st) = convert_contexts ins_ident ins_pos it_context st
			| ok 
				# ins={ins & ins_type = {ins_type & it_context = it_context}}
				= (ins, st)
				= (ins, st)

		convert_constructor cons=:{cons_ident,cons_pos,cons_type} st
			# (has_converted_context, cons_type, st) = convert_contexts_in_symbol_type cons_ident cons_pos cons_type st
			| has_converted_context
				= ({cons & cons_type=cons_type}, st)
				= (cons, st)

	convert_dcl_functions dcl_functions modules (heaps, error)
		# (dcl_functions, (modules, heaps, error)) 
			= updateArraySt convert_dcl_function dcl_functions (modules, heaps, error)	
		= (dcl_functions, modules, (heaps, error))
	where
		convert_dcl_function fun=:{ft_type, ft_ident, ft_pos} st
			# (ok, ft_type, st) = convert_contexts_in_symbol_type ft_ident ft_pos ft_type st
			| ok 
				# fun={fun & ft_type = ft_type}
				= (fun, st) 	
				= (fun, st) 	

	convert_contexts_in_symbol_type :: Ident Position !SymbolType !(!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
										 	-> (!Bool,!SymbolType,!(!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	convert_contexts_in_symbol_type fun_ident fun_pos symbol_type=:{st_context,st_args} st
		# (has_converted_context, st_context, st) = convert_contexts fun_ident fun_pos st_context st
		  (has_converted_arg, st_args, st) = convert_contexts_in_args fun_ident fun_pos st_args st
		| has_converted_context || has_converted_arg
			= (True,{symbol_type & st_context=st_context, st_args=st_args}, st)
			= (False,symbol_type, st)

	convert_contexts_in_args :: Ident Position ![AType] !(!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
									 -> (!Bool,![AType],!(!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	convert_contexts_in_args fun_ident fun_pos arg_args=:[arg=:{at_type=TFAC tvs t contexts}:args] st
		# (has_converted_context,contexts,st) = convert_contexts fun_ident fun_pos contexts st
 		# (has_converted_arg,args,st) = convert_contexts_in_args fun_ident fun_pos args st
		| has_converted_context || has_converted_arg
			= (True,[{arg & at_type=TFAC tvs t contexts}:args],st)
			= (False,arg_args,st)
	convert_contexts_in_args fun_ident fun_pos arg_args=:[arg:args] st
		# (has_converted_arg,args,st) = convert_contexts_in_args fun_ident fun_pos args st
		| has_converted_arg
			= (True,[arg:args],st)
			= (False,arg_args,st)
	convert_contexts_in_args fun_ident fun_pos [] st
		= (False,[],st)

	convert_contexts fun_name fun_pos [] st 
		= (False, [], st)
	convert_contexts fun_name fun_pos all_tcs=:[tc:tcs] st
		# (ok1, tc, st) = convert_context fun_name fun_pos tc st
		# (ok2, tcs, st) = convert_contexts fun_name fun_pos tcs st
		| ok1 || ok2
			= (True, [tc:tcs], st) 
			= (False, all_tcs, st)

	convert_context :: !Ident !Position !TypeContext (!*Modules, !*Heaps, !*ErrorAdmin)
		-> (!Bool, !TypeContext, (!*Modules, !*Heaps, !*ErrorAdmin))
	convert_context fun_name fun_pos tc=:{tc_class=TCGeneric gtc=:{gtc_generic, gtc_kind}} (modules, heaps=:{hp_generic_heap}, error)
		# ({gen_info_ptr}, modules) = modules![gtc_generic.glob_module].com_generic_defs.[gtc_generic.glob_object.ds_index]
		# ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap		
		# opt_class_info = lookupGenericClassInfo gtc_kind gen_classes
		# (tc_class, error) = case opt_class_info of 
			No
				# error = reportError fun_name.id_name fun_pos "no generic cases for this kind" error  
				-> (TCGeneric gtc, error)
			Yes class_info 
				# clazz = 
					{ glob_module = class_info.gci_module
					, glob_object = 
						{ ds_ident = genericIdentToClassIdent gtc_generic.glob_object.ds_ident.id_name gtc_kind 
						, ds_arity = 1
						, ds_index = class_info.gci_class
						}
					}
				// AA HACK: dummy dictionary
				#! {pds_module,pds_def} = gs_predefs.[PD_TypeGenericDict]
				# generic_dict = {gi_module=pds_module, gi_index=pds_def}
				-> (TCGeneric {gtc & gtc_class=clazz, gtc_generic_dict=generic_dict}, error)
		= (True, {tc & tc_class=tc_class}, (modules, {heaps & hp_generic_heap=hp_generic_heap}, error))
	convert_context fun_name fun_pos tc st 
		= (False, tc, st)

//  specialization

specializeGeneric ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		!{#OBJECT_CONS_FIELD_index}
		!Index 					// main_module index
		!*TypeDefInfos !*Heaps !*ErrorAdmin
	-> (!Expression,
		!*TypeDefInfos,!*Heaps,!*ErrorAdmin)
specializeGeneric gen_index type spec_env gen_ident gen_pos gen_OBJECT_CONS_FIELD_indices main_module_index td_infos heaps error
	#! heaps = set_tvs spec_env heaps
	#! (expr, (td_infos, heaps, error)) 
		= specialize type (td_infos, heaps, error)
	#! heaps = clear_tvs spec_env heaps
	= (expr, td_infos, heaps, error)
where
	specialize (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		= build_generic_app kind arg_exprs gen_index gen_ident st
	specialize (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		#! (expr, st) = specialize_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize (GTSVar tv) st
		= specialize_type_var tv st
	specialize (GTSArrow x y) st
		#! (x, st) = specialize x st
		#! (y, st) = specialize y st
		= build_generic_app (KindArrow [KindConst, KindConst]) [x,y] gen_index gen_ident st
	specialize (GTSPair x y) st
		#! (x, st) = specialize x st
		#! (y, st) = specialize y st
		= build_generic_app (KindArrow [KindConst, KindConst]) [x,y] gen_index gen_ident st
	specialize (GTSEither x y) st
		#! (x, st) = specialize x st
		#! (y, st) = specialize y st
		= build_generic_app (KindArrow [KindConst, KindConst]) [x,y] gen_index gen_ident st
	specialize (GTSCons cons_info_ds arg_type) st
		# (arg_expr, (td_infos, heaps, error)) = specialize arg_type st
		#! (generic_info_expr, heaps) = buildFunApp main_module_index cons_info_ds [] heaps		
		# gen_CONS_index = gen_OBJECT_CONS_FIELD_indices.[1]
		| gen_CONS_index.ocf_module>=0
			#! (expr, heaps)
				= buildFunApp2 gen_CONS_index.ocf_module gen_CONS_index.ocf_index gen_CONS_index.ocf_ident [generic_info_expr, arg_expr] heaps
			= (expr, (td_infos, heaps, error))
			// no instance for CONS, report error here ?
			#! (expr, heaps)
				= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident (KindArrow [KindConst]) [arg_expr] heaps
			= (expr, (td_infos, heaps, error))
	specialize (GTSField field_info_ds arg_type) st
		# (arg_expr, (td_infos, heaps, error)) = specialize arg_type st
		#! (generic_info_expr, heaps) = buildFunApp main_module_index field_info_ds [] heaps
		# gen_FIELD_index = gen_OBJECT_CONS_FIELD_indices.[2]
		| gen_FIELD_index.ocf_module>=0
			#! (expr, heaps)
				= buildFunApp2 gen_FIELD_index.ocf_module gen_FIELD_index.ocf_index gen_FIELD_index.ocf_ident [generic_info_expr, arg_expr] heaps
			= (expr, (td_infos, heaps, error))
			// no instance for FIELD, report error here ?
			#! (expr, heaps)
				= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident (KindArrow [KindConst]) [arg_expr] heaps
			= (expr, (td_infos, heaps, error))
	specialize (GTSObject type_info_ds arg_type) st
		# (arg_expr, (td_infos, heaps, error)) = specialize arg_type st
		#! (generic_info_expr, heaps) = buildFunApp main_module_index type_info_ds [] heaps
		# gen_OBJECT_index = gen_OBJECT_CONS_FIELD_indices.[0]
		| gen_OBJECT_index.ocf_module>=0
			#! (expr, heaps)
				= buildFunApp2 gen_OBJECT_index.ocf_module gen_OBJECT_index.ocf_index gen_OBJECT_index.ocf_ident [generic_info_expr, arg_expr] heaps
			= (expr, (td_infos, heaps, error))
			// no instance for OBJECT, report error here ?
			#! (expr, heaps)
				= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident (KindArrow [KindConst]) [arg_expr] heaps
			= (expr, (td_infos, heaps, error))
	specialize type (td_infos, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (td_infos, heaps, error))

	specialize_type_var tv=:{tv_info_ptr} (td_infos, heaps=:{hp_type_heaps=th=:{th_vars}}, error)		
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				-> (expr, (td_infos, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index iso_ds [] heaps
				-> (expr, (td_infos, heaps, error))

	build_generic_app kind arg_exprs gen_index gen_ident (td_infos, heaps, error)
		#! (expr, heaps)
			= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs heaps 
		= (expr, (td_infos, heaps, error))

specialize_generic_bimap ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		!Index 					// main_module index
		!PredefinedSymbols
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!Expression,
		!FunsAndGroups,!*Heaps,!*ErrorAdmin)
specialize_generic_bimap gen_index type spec_env gen_ident gen_pos main_module_index predefs funs_and_groups heaps error
	#! heaps = set_tvs spec_env heaps
	#! (expr, (funs_and_groups, heaps, error)) 
		= specialize type (funs_and_groups, heaps, error)
	#! heaps = clear_tvs spec_env heaps
	= (expr, funs_and_groups, heaps, error)
where
	specialize (GTSAppCons KindConst []) (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps)
			= bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, heaps, error))
	specialize (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		= build_generic_app kind arg_exprs gen_index gen_ident st
	specialize (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		#! (expr, st) = specialize_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize (GTSVar tv) st
		= specialize_type_var tv st
	specialize (GTSArrow x y) st=:(_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize y st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_arg_id_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize x st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_res_id_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
			#! (x, st) = specialize x st 
			#! (y, st) = specialize y st 
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
	specialize (GTSPair x y) st
		#! (x, st) = specialize x st 
		#! (y, st) = specialize y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_PAIR_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize (GTSEither x y) st
		#! (x, st) = specialize x st 
		#! (y, st) = specialize y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_EITHER_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize (GTSCons cons_info_ds arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_CONS_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize (GTSField field_info_ds arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_FIELD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize (GTSObject type_info_ds arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_OBJECT_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize GTSAppConsBimapKindConst (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps)
			= bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, heaps, error))
	specialize type (funs_and_groups, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, heaps, error))

	specialize_type_var tv=:{tv_info_ptr} (funs_and_groups, heaps=:{hp_type_heaps=th=:{th_vars}}, error)		
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				-> (expr, (funs_and_groups, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index iso_ds [] heaps
				-> (expr, (funs_and_groups, heaps, error))

	build_generic_app kind arg_exprs gen_index gen_ident (funs_and_groups, heaps, error)
		#! (expr, heaps)
			= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs heaps 
		= (expr, (funs_and_groups, heaps, error))

adapt_with_specialized_generic_bimap ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		![Expression]
		!Expression
		!Index 					// main_module index
		!PredefinedSymbols
		!FunsAndGroups !*Modules !*Heaps !*ErrorAdmin
	-> (!Expression,
		!FunsAndGroups,!*Modules,!*Heaps,!*ErrorAdmin)
adapt_with_specialized_generic_bimap gen_index type spec_env gen_ident gen_pos arg_exprs specialized_expr main_module_index predefs
		funs_and_groups modules heaps error
	#! heaps = set_tvs spec_env heaps
	#! (adapted_arg_exprs, arg_exprs, type, st)
		= adapt_args arg_exprs type (funs_and_groups, modules, heaps, error)
	#! (body_expr, (funs_and_groups, modules, heaps, error))
		= adapt_result arg_exprs type specialized_expr adapted_arg_exprs st
	# heaps = clear_tvs spec_env heaps
	= (body_expr, funs_and_groups, modules, heaps, error)
where
	adapt_args [arg_expr:arg_exprs] (GTSArrow arg_type args_type) st
		# (adapted_arg_expr,st)
		  	= adapt_arg arg_type arg_expr st
		  (adapted_arg_exprs,arg_exprs,args_type,st)
			= adapt_args arg_exprs args_type st
		= ([adapted_arg_expr:adapted_arg_exprs],arg_exprs,args_type,st)
	adapt_args arg_exprs args_type st
		= ([],arg_exprs,args_type,st)

	adapt_arg arg_type arg_expr st=:(_,_,heaps,_)
		| is_bimap_id arg_type heaps
			= (arg_expr,st)
			= specialize_to_with_arg arg_type arg_expr st

	adapt_result arg_exprs type specialized_expr adapted_arg_exprs st=:(_,_,heaps,_)
		| is_bimap_id type heaps
			= (build_body_expr specialized_expr adapted_arg_exprs arg_exprs,st)
			with
				build_body_expr specialized_expr [] []
					= specialized_expr
				build_body_expr specialized_expr [] original_arg_exprs
					= specialized_expr @ original_arg_exprs
				build_body_expr specialized_expr adapted_arg_exprs []
					= specialized_expr @ adapted_arg_exprs
				build_body_expr specialized_expr adapted_arg_exprs original_arg_exprs
					= specialized_expr @ (adapted_arg_exprs++original_arg_exprs)

			#! specialized_expr_with_adapted_args
				= case adapted_arg_exprs of
					[] -> specialized_expr
					_  -> specialized_expr @ adapted_arg_exprs
			= case arg_exprs of
				[]
					-> specialize_from_with_arg type specialized_expr_with_adapted_args st
				_
					# (adapted_expr,st)
						= specialize_from_with_arg type specialized_expr_with_adapted_args st
					-> (adapted_expr @ arg_exprs, st)

	specialize_to_with_arg (GTSVar tv=:{tv_info_ptr}) arg (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				# expr = build_map_to_expr expr predefs @ [arg]
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index to_ds [arg] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_to_with_arg (GTSAppConsSimpleType type_symbol_n kind arg_types) arg st
		= bimap_to_simple_type type_symbol_n kind arg_types arg st
	specialize_to_with_arg type arg st
		# (adaptor_expr,st)
			= specialize_to type st
		= (adaptor_expr @ [arg],st)

	specialize_from_with_arg (GTSVar tv=:{tv_info_ptr}) arg (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				# expr = build_map_from_expr expr predefs @ [arg]
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [arg] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_from_with_arg (GTSAppConsSimpleType type_symbol_n kind arg_types) arg st
		= bimap_from_simple_type type_symbol_n kind arg_types arg st
	specialize_from_with_arg type arg st
		# (adaptor_expr,st)
			= specialize_from type st
		= (adaptor_expr @ [arg],st)

	specialize_from (GTSArrow (GTSAppCons KindConst []) y) st
		= specialize_from_arrow_arg_id y st
	specialize_from (GTSArrow GTSAppConsBimapKindConst y) st
		= specialize_from_arrow_arg_id y st
	specialize_from (GTSArrow x (GTSAppCons KindConst [])) st
		= specialize_from_arrow_res_id x st
	specialize_from (GTSArrow x GTSAppConsBimapKindConst) st
		= specialize_from_arrow_res_id x st
	specialize_from (GTSArrow (GTSVar {tv_info_ptr=xp}) (GTSVar {tv_info_ptr=yp})) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (x_expr, th_vars) = readPtr xp th_vars
		  (y_expr, th_vars) = readPtr yp th_vars
		  heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression x_expr
			# (y,heaps) = build_map_from_tvi_expr y_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_from_arrow_arg_id_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
		| is_bimap_id_expression y_expr
			# (x,heaps) = build_map_to_tvi_expr x_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_from_arrow_res_id_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
			# (x,heaps) = build_map_to_tvi_expr x_expr main_module_index predefs heaps
			  (y,heaps) = build_map_from_tvi_expr y_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_from_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow (GTSVar {tv_info_ptr}) y) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		#! (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression expr
			# st = (funs_and_groups, modules, heaps, error)
			= specialize_from_arrow_arg_id y st
			# (x,heaps) = build_map_to_tvi_expr expr main_module_index predefs heaps
			  (y, (funs_and_groups, modules, heaps, error))
			  	= specialize_from y (funs_and_groups, modules, heaps, error)
			  (expr, funs_and_groups, heaps)
				= bimap_from_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow x (GTSVar {tv_info_ptr})) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		#! (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression expr
			# st = (funs_and_groups, modules, heaps, error)
			= specialize_from_arrow_res_id x st
			# (y,heaps) = build_map_from_tvi_expr expr main_module_index predefs heaps
			  (x, (funs_and_groups, modules, heaps, error))
			  	= specialize_to x (funs_and_groups, modules, heaps, error)
			  (expr, funs_and_groups, heaps)
				= bimap_from_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow x y) st
		#! (x, st) = specialize_to x st
		#! (y, st) = specialize_from y st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_from_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSVar tv=:{tv_info_ptr}) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				# from_expr = build_map_from_expr expr predefs
				-> (from_expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_from type=:(GTSAppBimap (KindArrow [KindConst,KindConst]) [arg1,arg2]) st
		# (arg1,st) = specialize arg1 st
		  (arg2,st) = specialize arg2 st
		  (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_from_Bimap_expression [arg1,arg2] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from type (funs_and_groups, modules, heaps, error)
		#! (bimap_expr, st)
			= specialize type (funs_and_groups, modules, heaps, error)
		# adaptor_expr = build_map_from_expr bimap_expr predefs
		= (adaptor_expr, st)

	specialize_from_arrow_arg_id y st
		#! (y, st) = specialize_from y st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_from_arrow_arg_id_expression [y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))

	specialize_from_arrow_res_id x st
		#! (x, st) = specialize_to x st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_from_arrow_res_id_expression [x] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))

	specialize_to (GTSVar tv=:{tv_info_ptr}) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				# from_expr = build_map_to_expr expr predefs
				-> (from_expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index to_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_to type (funs_and_groups, modules, heaps, error)
		#! (bimap_expr, st) 
			= specialize type (funs_and_groups, modules, heaps, error)
		# adaptor_expr = build_map_to_expr bimap_expr predefs
		= (adaptor_expr, st)

	specialize (GTSAppCons KindConst []) (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps)
			= bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize (GTSAppConsSimpleType _ kind arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize (GTSAppBimap kind arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = mapSt specialize arg_types st
		#! (expr, st) = specialize_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize (GTSVar tv) st
		= specialize_type_var tv st
	specialize (GTSArrow x y) st=:(_,_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_arg_id_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize x st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_res_id_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
			#! (x, st) = specialize x st
			#! (y, st) = specialize y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_arrow_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize GTSAppConsBimapKindConst (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps)
			= bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize type (funs_and_groups, modules, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, modules, heaps, error))

	specialize_type_var tv=:{tv_info_ptr} (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)		
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_Expr is_bimap_id expr
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso iso_ds to_ds from_ds
				# (expr,heaps) = buildFunApp main_module_index iso_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))

	build_generic_app kind arg_exprs gen_index gen_ident heaps
		= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs heaps 

	bimap_to_simple_type :: !(Global Index) !TypeKind ![GenTypeStruct] !Expression !*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
																  -> *(!Expression,!*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	bimap_to_simple_type global_type_def_index=:{glob_module} (KindArrow kinds) arg_types arg (funs_and_groups,modules,heaps,error)
		# (alts,constructors_arg_types,modules,heaps)
			= determine_constructors_arg_types global_type_def_index arg_types modules heaps
		# (alg_patterns,funs_and_groups,modules,heaps,error)
			= build_to_alg_patterns alts constructors_arg_types glob_module funs_and_groups modules heaps error
		= build_bimap_case global_type_def_index arg alg_patterns funs_and_groups modules heaps error
	where
		build_to_alg_patterns [cons_ds=:{ds_ident,ds_index,ds_arity}:alts] [constructor_arg_types:constructors_arg_types] type_module_n funs_and_groups modules heaps error
			# arg_names = ["x" +++ toString k \\ k <- [1..ds_arity]]
			# (var_exprs, vars, heaps) = buildVarExprs arg_names heaps
			# (args,(funs_and_groups,modules,heaps,error))
				= specialize_to_with_args constructor_arg_types var_exprs (funs_and_groups,modules,heaps,error)
			# (alg_pattern,heaps)
				= build_alg_pattern cons_ds vars args type_module_n heaps
			# (alg_patterns,funs_and_groups,modules,heaps,error)
				= build_to_alg_patterns alts constructors_arg_types type_module_n funs_and_groups modules heaps error
			= ([alg_pattern:alg_patterns],funs_and_groups,modules,heaps,error)
		build_to_alg_patterns [] [] type_module_n funs_and_groups modules heaps error
			= ([],funs_and_groups,modules,heaps,error)
	
		specialize_to_with_args [type:types] [arg:args] st=:(_,_,heaps,_)
			| is_bimap_id type heaps
				# (args,st)
					= specialize_to_with_args types args st
				= ([arg:args],st)
				# (arg,st)
					= specialize_to_with_arg type arg st
				# (args,st)
					= specialize_to_with_args types args st
				= ([arg:args],st)
		specialize_to_with_args [] [] st
			= ([],st)

	bimap_from_simple_type :: !(Global Index) !TypeKind ![GenTypeStruct] !Expression !*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
																	-> *(!Expression,!*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	bimap_from_simple_type global_type_def_index=:{glob_module} (KindArrow kinds) arg_types arg (funs_and_groups,modules,heaps,error)
		# (alts,constructors_arg_types,modules,heaps)
			= determine_constructors_arg_types global_type_def_index arg_types modules heaps
		# (alg_patterns,funs_and_groups,modules,heaps,error)
			= build_from_alg_patterns alts constructors_arg_types glob_module funs_and_groups modules heaps error
		= build_bimap_case global_type_def_index arg alg_patterns funs_and_groups modules heaps error
	where
		build_from_alg_patterns [cons_ds=:{ds_ident,ds_index,ds_arity}:alts] [constructor_arg_types:constructors_arg_types] type_module_n funs_and_groups modules heaps error
			# arg_names = ["x" +++ toString k \\ k <- [1..ds_arity]]
			# (var_exprs, vars, heaps) = buildVarExprs arg_names heaps
			# (args,(funs_and_groups,modules,heaps,error))
				= specialize_from_with_args constructor_arg_types var_exprs (funs_and_groups,modules,heaps,error)
			# (alg_pattern,heaps)
				= build_alg_pattern cons_ds vars args type_module_n heaps
			# (alg_patterns,funs_and_groups,modules,heaps,error)
				= build_from_alg_patterns alts constructors_arg_types type_module_n funs_and_groups modules heaps error
			= ([alg_pattern:alg_patterns],funs_and_groups,modules,heaps,error)
		build_from_alg_patterns [] [] type_module_n funs_and_groups modules heaps error
			= ([],funs_and_groups,modules,heaps,error)

		specialize_from_with_args [type:types] [arg:args] st=:(_,_,heaps,_)
			| is_bimap_id type heaps
				# (args,st)
					= specialize_from_with_args types args st
				= ([arg:args],st)
				# (arg,st)
					= specialize_from_with_arg type arg st
				# (args,st)
					= specialize_from_with_args types args st
				= ([arg:args],st)
		specialize_from_with_args [] [] st
			= ([],st)

	determine_constructors_arg_types :: !(Global Index) ![GenTypeStruct] !*Modules !*Heaps
								 -> (![DefinedSymbol],![[GenTypeStruct]],!*Modules,!*Heaps)
	determine_constructors_arg_types {glob_module,glob_object} arg_types modules heaps
		# ({td_args,td_rhs=AlgType alts},modules) = modules![glob_module].com_type_defs.[glob_object]

		# {hp_type_heaps} = heaps
		# th_vars = number_type_arguments td_args 0 hp_type_heaps.th_vars
		# arg_types_a = {!arg_type\\arg_type<-arg_types}
		# (constructors_arg_types,modules,th_vars)
			= compute_constructors_arg_types alts glob_module arg_types_a modules th_vars
		# th_vars = remove_type_argument_numbers td_args th_vars
		# heaps = {heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}}
		= (alts,constructors_arg_types,modules,heaps)
	where
		compute_constructors_arg_types :: ![DefinedSymbol] !Int !{!GenTypeStruct} !*Modules !*TypeVarHeap
														   -> (![[GenTypeStruct]],!*Modules,!*TypeVarHeap)
		compute_constructors_arg_types [cons_ds=:{ds_ident,ds_index}:alts] type_module_n arg_types_a modules th_vars
			# ({cons_type={st_args}},modules) = modules![type_module_n].com_cons_defs.[ds_index]
			# (constructor_arg_numbers,th_vars)
				= compute_constructor_arg_types st_args arg_types_a th_vars
			# (constructors_arg_numbers,modules,th_vars)
				= compute_constructors_arg_types alts type_module_n arg_types_a modules th_vars
			= ([constructor_arg_numbers:constructors_arg_numbers],modules,th_vars)
		compute_constructors_arg_types [] type_module_n arg_types_a modules th_vars
			= ([],modules,th_vars)

		compute_constructor_arg_types :: ![AType] !{!GenTypeStruct} !*TypeVarHeap -> (![GenTypeStruct],!*TypeVarHeap)
		compute_constructor_arg_types [{at_type=TV {tv_info_ptr}}:atypes] arg_types_a th_vars
			# (TVI_GenTypeVarNumber constructor_arg_number,th_vars)
				= readPtr tv_info_ptr th_vars
			#! constructor_arg_types = arg_types_a.[constructor_arg_number]
			# (constructors_arg_types,th_vars)
				= compute_constructor_arg_types atypes arg_types_a th_vars
			= ([constructor_arg_types:constructors_arg_types],th_vars);
		compute_constructor_arg_types [] arg_types_a th_vars
			= ([],th_vars)

	build_bimap_case :: !(Global Index) !.Expression ![AlgebraicPattern] !FunsAndGroups !*Modules !*Heaps !*ErrorAdmin
													   -> (!Expression,!(!FunsAndGroups,!*Modules,!*Heaps,!*ErrorAdmin))
	build_bimap_case global_type_def_index arg alg_patterns funs_and_groups modules heaps error
		# case_patterns = AlgebraicPatterns global_type_def_index alg_patterns
		# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
		# case_expr = Case {case_expr = arg, case_guards = case_patterns, case_default = No, case_ident = No,
							case_info_ptr = expr_info_ptr, case_explicit = True, case_default_pos = NoPos}
		# heaps = {heaps & hp_expression_heap = hp_expression_heap}
		= (case_expr, (funs_and_groups,modules,heaps,error))

	build_alg_pattern :: !DefinedSymbol ![FreeVar] ![Expression] !Int !*Heaps -> (!AlgebraicPattern,!*Heaps)
	build_alg_pattern cons_ds=:{ds_ident,ds_index} vars args type_module_n heaps
		# cons_symbol = {glob_module = type_module_n, glob_object = cons_ds}
		# cons_symb_ident = {symb_ident = ds_ident, symb_kind = SK_Constructor {glob_module = type_module_n,glob_object = ds_index}}

		# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
		# expr = App {app_symb = cons_symb_ident, app_args = args, app_info_ptr = expr_info_ptr} 

		#! alg_pattern = { ap_symbol = cons_symbol, ap_vars = vars, ap_expr = expr, ap_position = NoPos }
		# heaps = {heaps & hp_expression_heap = hp_expression_heap}
		= (alg_pattern,heaps)

is_bimap_id :: !GenTypeStruct !Heaps -> Bool
is_bimap_id (GTSAppCons KindConst []) heaps
	= True
is_bimap_id GTSAppConsBimapKindConst heaps
	= True
is_bimap_id (GTSVar {tv_info_ptr}) heaps
	= case sreadPtr tv_info_ptr heaps.hp_type_heaps.th_vars of
		TVI_Expr is_bimap_id expr
			-> is_bimap_id
		_
			-> False
is_bimap_id _ heaps
	= False

is_bimap_id_expression (TVI_Expr is_bimap_id _)
	= is_bimap_id
is_bimap_id_expression _
	= False

set_tvs spec_env heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}}
	#! th_vars = foldSt write_tv spec_env th_vars
		with write_tv ({tv_info_ptr}, tvi) th_vars
				= writePtr tv_info_ptr tvi th_vars		
	= {heaps & hp_type_heaps = {hp_type_heaps & th_vars = th_vars }}

clear_tvs spec_env heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}}
	#! th_vars = foldSt write_tv spec_env th_vars
		with write_tv ({tv_info_ptr}, _) th_vars
				= writePtr tv_info_ptr TVI_Empty th_vars		
	= {heaps & hp_type_heaps = {hp_type_heaps & th_vars = th_vars }}

number_type_arguments :: ![ATypeVar] !Int !*TypeVarHeap -> *TypeVarHeap
number_type_arguments [{atv_variable={tv_info_ptr}}:atype_vars] arg_n th_vars
	# th_vars = writePtr tv_info_ptr (TVI_GenTypeVarNumber arg_n) th_vars
	= number_type_arguments atype_vars (arg_n+1) th_vars
number_type_arguments [] arg_n th_vars
	= th_vars

remove_type_argument_numbers :: ![ATypeVar] !*TypeVarHeap -> *TypeVarHeap
remove_type_argument_numbers [{atv_variable={tv_info_ptr}}:atype_vars] th_vars
	# th_vars = writePtr tv_info_ptr TVI_Empty th_vars
	= remove_type_argument_numbers atype_vars th_vars
remove_type_argument_numbers [] th_vars
	= th_vars

build_bimap_with_calls map_id_index map_id_ident to_args from_args main_module_index predefs heaps
	# (map_to_expr,heaps) = buildFunApp2 main_module_index map_id_index map_id_ident to_args heaps
	  (map_from_expr,heaps) = buildFunApp2 main_module_index map_id_index map_id_ident from_args heaps
	= build_bimap_record map_to_expr map_from_expr predefs heaps	

build_var_with_bimap_selectors var_name predefs heaps
	# (bimap_var_expr,arg_var,heaps) = buildVarExpr var_name heaps 
	  to_arg_expr = build_map_to_expr bimap_var_expr predefs
	  from_arg_expr = build_map_from_expr bimap_var_expr predefs
	= (to_arg_expr,from_arg_expr,arg_var,heaps)

bimap_fromto_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_fromto_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/fromto from to f x = from (f (to x))
		# bimap_fromto_ident = makeIdent "bimap/fromto"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [from_var,to_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [to_expr @ [x_expr]]]
		  (bimap_fromto_index,funs_and_groups) = buildFunAndGroup2 bimap_fromto_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_fromto_function={fii_index=bimap_fromto_index,fii_ident=bimap_fromto_ident}}
		= (bimap_fromto_index,bimap_fromto_ident,funs_and_groups,heaps)

bimap_tofrom_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_tofrom_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/tofrom to from f x = from (f (to x))
		# bimap_tofrom_ident = makeIdent "bimap/tofrom"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [to_var,from_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [to_expr @ [x_expr]]]
		  (bimap_tofrom_index,funs_and_groups) = buildFunAndGroup2 bimap_tofrom_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_tofrom_function={fii_index=bimap_tofrom_index,fii_ident=bimap_tofrom_ident}}
		= (bimap_tofrom_index,bimap_tofrom_ident,funs_and_groups,heaps)

bimap_to_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_to_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/from to f x = f (to x)
		# bimap_to_ident = makeIdent "bimap/to"
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [to_var,f_var,x_var]
		  rhs_expr = f_expr @ [to_expr @ [x_expr]]
		  (bimap_to_index,funs_and_groups) = buildFunAndGroup2 bimap_to_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_to_function={fii_index=bimap_to_index,fii_ident=bimap_to_ident}}
		= (bimap_to_index,bimap_to_ident,funs_and_groups,heaps)

bimap_from_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_from_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/from from f x = from (f x)
		# bimap_from_ident = makeIdent "bimap/from"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [from_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [x_expr]]
		  (bimap_from_index,funs_and_groups) = buildFunAndGroup2 bimap_from_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_from_function={fii_index=bimap_from_index,fii_ident=bimap_from_ident}}
		= (bimap_from_index,bimap_from_ident,funs_and_groups,heaps)

bimap_id_expression main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_id_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident [] heaps
		= (expr,funs_and_groups,heaps)
		// bimap/id x = x
		# bimap_id_ident = makeIdent "bimap/id"
		  (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 
		  (bimap_id_index,funs_and_groups) = buildFunAndGroup2 bimap_id_ident [arg_var] arg_expr main_module_index funs_and_groups

		// bimap/c = {map_to = bimap/id, map_from = bimap/id}
		  bimap_c_ident = makeIdent "bimap/c"
		  (bimap_expr,heaps) = build_bimap_with_calls bimap_id_index bimap_id_ident [] [] main_module_index predefs heaps

		  (bimap_c_index,funs_and_groups) = buildFunAndGroup2 bimap_c_ident [] bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_id_function={fii_index=bimap_c_index,fii_ident=bimap_c_ident}}

		  (bimap_c_expr,heaps) = buildFunApp2 main_module_index bimap_c_index bimap_c_ident [] heaps
		= (bimap_c_expr,funs_and_groups,heaps)

bimap_arrow_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_arrow_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		# (bimap_tofrom_index,bimap_tofrom_ident,funs_and_groups,heaps)
			= bimap_tofrom_function main_module_index funs_and_groups heaps
		// bimap/arrow args res
		//	= {map_to = bimap/tofrom arg.map_from res.map_to, map_from = bimap/tofrom arg.map_to res.map_to}
		  bimap_arrow_ident = makeIdent "bimap/arrow"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (to_res_expr,from_res_expr,res_var,heaps) = build_var_with_bimap_selectors "res" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls bimap_tofrom_index bimap_tofrom_ident [from_arg_expr,to_res_expr] [to_arg_expr,from_res_expr] main_module_index predefs heaps

		  args = [arg_var,res_var]
		  (bimap_arrow_index,funs_and_groups) = buildFunAndGroup2 bimap_arrow_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_arrow_function={fii_index=bimap_arrow_index,fii_ident=bimap_arrow_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_arrow_index bimap_arrow_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_arrow_arg_id_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_arrow_arg_id_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		# (bimap_from_index,bimap_from_ident,funs_and_groups,heaps)
			= bimap_from_function main_module_index funs_and_groups heaps
		// bimap/arrow_arg_id res
		//	= {map_to = bimap/from res.map_to, map_from = bimap/from res.map_from }
		  bimap_arrow_arg_id_ident = makeIdent "bimap/arrow_arg_id"
		  (to_res_expr,from_res_expr,res_var,heaps) = build_var_with_bimap_selectors "res" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls bimap_from_index bimap_from_ident [to_res_expr] [from_res_expr] main_module_index predefs heaps

		  args = [res_var]
		  (bimap_arrow_arg_id_index,funs_and_groups) = buildFunAndGroup2 bimap_arrow_arg_id_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_arrow_arg_id_function={fii_index=bimap_arrow_arg_id_index,fii_ident=bimap_arrow_arg_id_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_arrow_arg_id_index bimap_arrow_arg_id_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_arrow_res_id_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_arrow_res_id_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		# (bimap_to_index,bimap_to_ident,funs_and_groups,heaps)
			= bimap_to_function main_module_index funs_and_groups heaps
		// bimap/arrow_res_id arg
		//	= {map_to = bimap/to arg.map_from, map_from = bimap/to arg.map_to }
		  bimap_arrow_res_id_ident = makeIdent "bimap/arrow_res_id"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls bimap_to_index bimap_to_ident [from_arg_expr] [to_arg_expr] main_module_index predefs heaps

		  args = [arg_var]
		  (bimap_arrow_res_id_index,funs_and_groups) = buildFunAndGroup2 bimap_arrow_res_id_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_arrow_res_id_function={fii_index=bimap_arrow_res_id_index,fii_ident=bimap_arrow_res_id_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_arrow_res_id_index bimap_arrow_res_id_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_from_Bimap_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_from_Bimap_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		# (bimap_fromto_index,bimap_fromto_ident,funs_and_groups,heaps)
			= bimap_fromto_function main_module_index funs_and_groups heaps

		// bimap/from_Bimap arg res f
		//	= {map_to = bimap/fromto res.map_from arg.map_to f.map_to, map_from = bimap/fromto arg.map_from res.map_to f.map_from}
		  bimap_from_Bimap_ident = makeIdent "bimap/from_Bimap"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (to_res_expr,from_res_expr,res_var,heaps) = build_var_with_bimap_selectors "res" predefs heaps
		  (to_f_expr,from_f_expr,f_var,heaps) = build_var_with_bimap_selectors "f" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls bimap_fromto_index bimap_fromto_ident
		  							[from_res_expr,to_arg_expr,to_f_expr] [from_arg_expr,to_res_expr,from_f_expr] main_module_index predefs heaps

		  args = [arg_var,res_var,f_var]
		  (bimap_from_Bimap_index,funs_and_groups) = buildFunAndGroup2 bimap_from_Bimap_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_from_Bimap_function={fii_index=bimap_from_Bimap_index,fii_ident=bimap_from_Bimap_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_from_Bimap_index bimap_from_Bimap_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_PAIR_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_PAIR_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		/*
		bimap/PAIR x y
			= {map_to = map/PAIR x.map_to y.map_to, map_from = map/PAIR x.map_from y.map_from}
		where
			map/PAIR fx fy (PAIR x y) = PAIR (fx x) (fy y)
		*/
		# map_PAIR_ident = makeIdent "map/PAIR"
		  (fx_expr,fx_var,heaps) = buildVarExpr "fx" heaps 
		  (fy_expr,fy_var,heaps) = buildVarExpr "fy" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps
		  (y_expr,y_var,heaps) = buildVarExpr "y" heaps

		  (object_expr,heaps) = build_pair (fx_expr @ [x_expr]) (fy_expr @ [y_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_pair x_var y_var object_expr predefs heaps
		  args = [fx_var,fy_var,c_var]
		  (map_PAIR_index,funs_and_groups) = buildFunAndGroup2 map_PAIR_ident args case_expr main_module_index funs_and_groups

		  bimap_PAIR_ident = makeIdent "bimap/PAIR"
		  (to_x_expr,from_x_expr,x_var,heaps) = build_var_with_bimap_selectors "x" predefs heaps
		  (to_y_expr,from_y_expr,y_var,heaps) = build_var_with_bimap_selectors "y" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls map_PAIR_index map_PAIR_ident [to_x_expr,to_y_expr] [from_x_expr,from_y_expr] main_module_index predefs heaps

		  args = [x_var,y_var]
		  (bimap_PAIR_index,funs_and_groups) = buildFunAndGroup2 bimap_PAIR_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_PAIR_function={fii_index=bimap_PAIR_index,fii_ident=bimap_PAIR_ident}}

		  (bimap_PAIR_expr,heaps) = buildFunApp2 main_module_index bimap_PAIR_index bimap_PAIR_ident arg_exprs heaps
		= (bimap_PAIR_expr,funs_and_groups,heaps)

bimap_EITHER_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_EITHER_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		/*
		bimap/EITHER l r
			= {map_to = map/EITHER l.map_to r.map_to, map_from = map/EITHER l.map_from r.map_from}
		where
			map/EITHER lf rf (LEFT l)  = LEFT  (lf l)
			map/EITHER lf rf (RIGHT r) = RIGHT (rf r)
		*/
		# map_EITHER_ident = makeIdent "map/EITHER"
		  (lf_expr,lf_var,heaps) = buildVarExpr "lf" heaps 
		  (rf_expr,rf_var,heaps) = buildVarExpr "rf" heaps 
		  (l_expr,l_var,heaps) = buildVarExpr "l" heaps
		  (r_expr,r_var,heaps) = buildVarExpr "r" heaps

		  (left_expr,heaps) = build_left (lf_expr @ [l_expr]) predefs heaps
		  (right_expr,heaps) = build_right (rf_expr @ [r_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_either l_var left_expr r_var right_expr predefs heaps

		  args = [lf_var,rf_var,c_var]
		  (map_EITHER_index,funs_and_groups) = buildFunAndGroup2 map_EITHER_ident args case_expr main_module_index funs_and_groups

		  bimap_EITHER_ident = makeIdent "bimap/EITHER"
		  (to_l_expr,from_l_expr,l_var,heaps) = build_var_with_bimap_selectors "l" predefs heaps
		  (to_r_expr,from_r_expr,r_var,heaps) = build_var_with_bimap_selectors "r" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls map_EITHER_index map_EITHER_ident [to_l_expr,to_r_expr] [from_l_expr,from_r_expr] main_module_index predefs heaps

		  args = [l_var,r_var]
		  (bimap_EITHER_index,funs_and_groups) = buildFunAndGroup2 bimap_EITHER_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_EITHER_function={fii_index=bimap_EITHER_index,fii_ident=bimap_EITHER_ident}}

		  (bimap_EITHER_expr,heaps) = buildFunApp2 main_module_index bimap_EITHER_index bimap_EITHER_ident arg_exprs heaps
		= (bimap_EITHER_expr,funs_and_groups,heaps)

bimap_OBJECT_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_OBJECT_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		/*
		bimap/OBJECT arg
			= {map_to = map/OBJECT arg.map_to, map_from = map/OBJECT arg.map_from}
		where
			map/OBJECT f (OBJECT x) = OBJECT (f x)
		*/
		# map_OBJECT_ident = makeIdent "map/OBJECT"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (object_expr,heaps) = build_object (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_object x_var object_expr predefs heaps
		  args = [f_var,c_var]
		  (map_OBJECT_index,funs_and_groups) = buildFunAndGroup2 map_OBJECT_ident args case_expr main_module_index funs_and_groups

		  bimap_OBJECT_ident = makeIdent "bimap/OBJECT"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls map_OBJECT_index map_OBJECT_ident [to_arg_expr] [from_arg_expr] main_module_index predefs heaps

		  args = [arg_var]
		  (bimap_OBJECT_index,funs_and_groups) = buildFunAndGroup2 bimap_OBJECT_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_OBJECT_function={fii_index=bimap_OBJECT_index,fii_ident=bimap_OBJECT_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_OBJECT_index bimap_OBJECT_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_CONS_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_CONS_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		/*
		bimap/CONS arg
			= {map_to = map/CONS arg.map_to, map_from = map/CONS arg.map_from}
		where
			map/CONS f (CONS x) = CONS (f x)
		*/
		# map_CONS_ident = makeIdent "map/CONS"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (cons_expr,heaps) = build_cons (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_cons x_var cons_expr predefs heaps
		  args = [f_var,c_var]
		  (map_CONS_index,funs_and_groups) = buildFunAndGroup2 map_CONS_ident args case_expr main_module_index funs_and_groups

		  bimap_CONS_ident = makeIdent "bimap/CONS"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls map_CONS_index map_CONS_ident [to_arg_expr] [from_arg_expr] main_module_index predefs heaps

		  args = [arg_var]
		  (bimap_CONS_index,funs_and_groups) = buildFunAndGroup2 bimap_CONS_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_CONS_function={fii_index=bimap_CONS_index,fii_ident=bimap_CONS_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_CONS_index bimap_CONS_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_FIELD_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_FIELD_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		/*
		bimap/FIELD arg
			= {map_to = map/FIELD arg.map_to, map_from = map/FIELD arg.map_from}
		where
			map/FIELD f (FIELD x) = FIELD (f x)
		*/
		# map_FIELD_ident = makeIdent "map/FIELD"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (field_expr,heaps) = build_field (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_field x_var field_expr predefs heaps
		  args = [f_var,c_var]
		  (map_FIELD_index,funs_and_groups) = buildFunAndGroup2 map_FIELD_ident args case_expr main_module_index funs_and_groups

		  bimap_FIELD_ident = makeIdent "bimap/FIELD"
		  (to_arg_expr,from_arg_expr,arg_var,heaps) = build_var_with_bimap_selectors "arg" predefs heaps
		  (bimap_expr,heaps) = build_bimap_with_calls map_FIELD_index map_FIELD_ident [to_arg_expr] [from_arg_expr] main_module_index predefs heaps

		  args = [arg_var]
		  (bimap_FIELD_index,funs_and_groups) = buildFunAndGroup2 bimap_FIELD_ident args bimap_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_FIELD_function={fii_index=bimap_FIELD_index,fii_ident=bimap_FIELD_ident}}

		  (bimap_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_FIELD_index bimap_FIELD_ident arg_exprs heaps
		= (bimap_arrow_expr,funs_and_groups,heaps)

bimap_from_arrow_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_fromto_index,bimap_fromto_ident,funs_and_groups,heaps)
		= bimap_tofrom_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_fromto_index bimap_fromto_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

bimap_from_arrow_res_id_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_to_index,bimap_to_ident,funs_and_groups,heaps)
		= bimap_to_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_to_index bimap_to_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

bimap_from_arrow_arg_id_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_from_index,bimap_from_ident,funs_and_groups,heaps)
		= bimap_from_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_from_index bimap_from_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

//	kind indexing of generic types

// kind indexing:
// t_{*} a1 ... an 			= t a1 ... an 
// t_{k->l} a1 ... an		= forall b1...bn.(t_k b1 ... bn) -> (t_l (a1 b1) ... (an bn)) 
buildKindIndexedType :: 
		!SymbolType			// symbol type to kind-index
		![TypeVar]			// generic type variables
		!TypeKind			// kind index
		!Ident				// name for debugging
		!Position 			// position for debugging
		!*TypeHeaps			// type heaps
		!*ErrorAdmin		
	-> 	( !SymbolType		// instantiated type
		, ![ATypeVar]		// fresh generic type variables
		, !*TypeHeaps		// type heaps
		, !*ErrorAdmin	
		)
buildKindIndexedType st gtvs kind ident pos th error
	#! th = clearSymbolType st th
	#! (fresh_st, fresh_gtvs, th) = fresh_generic_type st gtvs th	

	#! (gatvs, th) = collectAttrsOfTypeVarsInSymbolType fresh_gtvs fresh_st th

	#! (kind_indexed_st, _, th, error) = build_symbol_type fresh_st gatvs kind 1 th error	
	 	 
	#! th = clearSymbolType kind_indexed_st th
	#! th = clearSymbolType st th				// paranoja
	= (kind_indexed_st, gatvs, th, error)
where
	fresh_generic_type st gtvs th
		# (fresh_st, th) = freshSymbolType st th
		# fresh_gtvs = take (length gtvs) fresh_st.st_vars		
		= (fresh_st, fresh_gtvs, th)

	build_symbol_type ::
			 !SymbolType 	// generic type, 
			 ![ATypeVar]	// attributed generic variables
			 !TypeKind 		// kind to specialize to 
			 !Int 			// current order (in the sense of the order of the kind)
			 !*TypeHeaps !*ErrorAdmin
		-> ( !SymbolType	// new generic type
			, ![ATypeVar]	// fresh copies of generic variables created for the 
							// generic arguments
			, !*TypeHeaps, !*ErrorAdmin)
	build_symbol_type st gatvs KindConst order th error	
		= (st, [], th, error)
	build_symbol_type st gatvs (KindArrow kinds) order th error
		| order > 2
			# error = reportError ident.id_name pos "kinds of order higher then 2 are not supported" error
			= (st, [], th, error)
		
		# (arg_sts, arg_gatvss, th, error) 
			= build_args st gatvs order kinds th error 

		# (body_st, th) 
			= build_body st gatvs (transpose arg_gatvss) th 

		# num_added_args = length kinds
		# new_st = 
			{ st_vars = removeDup (
					foldr (++) body_st.st_vars [st_vars \\ {st_vars}<-arg_sts])
			, st_attr_vars = removeDup (
					foldr (++) body_st.st_attr_vars [st_attr_vars \\ {st_attr_vars}<-arg_sts])
			, st_args = [st_result \\ {st_result}<-arg_sts] ++ body_st.st_args
			, st_result = body_st.st_result 
			, st_arity = body_st.st_arity + num_added_args
			, st_context = removeDup(
				foldr (++) body_st.st_context [st_context \\ {st_context} <- arg_sts])
			, st_attr_env = removeDup(
				foldr (++) body_st.st_attr_env [st_attr_env \\ {st_attr_env} <- arg_sts])
			, st_args_strictness = insert_n_lazy_values_at_beginning num_added_args body_st.st_args_strictness	 
			}
		= (new_st, flatten arg_gatvss, th, error)

	build_args st gatvs order kinds th error
		# (arg_sts_and_gatvss, (_,th,error)) 
			= mapSt (build_arg st gatvs order) kinds (1,th,error)
		# (arg_sts, arg_gatvss) = unzip arg_sts_and_gatvss
		= (arg_sts, arg_gatvss, th, error)

	build_arg :: 
			!SymbolType 		// current part of the generic type
			![ATypeVar]			// generic type variables with their attrs
			!Int 				// order
			!TypeKind			// kind corrseponding to the arg
			( !Int				// the argument number
			, !*TypeHeaps, !*ErrorAdmin)				
		->  ( (!SymbolType, [ATypeVar]) // fresh symbol type and generic variables 
			, 	( !Int			// incremented argument number
				, !*TypeHeaps, !*ErrorAdmin))
	build_arg st gatvs order kind (arg_num, th, error)
		#! th = clearSymbolType st th
		#! (fresh_gatvs, th) = mapSt subst_gatv gatvs th
		#! (new_st, th) = applySubstInSymbolType st th
		
		#! (new_st, forall_atvs, th, error) 
			= build_symbol_type new_st fresh_gatvs kind (inc order) th error	
		#! (curry_st, th)	
			= curryGenericArgType1 new_st ("cur" +++ toString order +++ postfix) th 	

		#! curry_st = adjust_forall curry_st forall_atvs

		= ((curry_st, fresh_gatvs), (inc arg_num, th, error))
	where
		postfix = toString arg_num

		subst_gatv atv=:{atv_attribute, atv_variable} th=:{th_attrs, th_vars}			
			# (tv, th_vars) = subst_gtv atv_variable th_vars 
			# (attr, th_attrs) = subst_attr atv_attribute th_attrs 			
			=	( {atv & atv_variable = tv, atv_attribute = attr}
			 	, {th & th_vars = th_vars, th_attrs = th_attrs}
			 	)	

		// generic type var is replaced with a fresh one
		subst_gtv {tv_info_ptr, tv_ident} th_vars 
			# (tv, th_vars) = freshTypeVar (postfixIdent tv_ident.id_name postfix) th_vars	
			= (tv, writePtr tv_info_ptr (TVI_Type (TV tv)) th_vars)

		subst_attr (TA_Var {av_ident, av_info_ptr}) th_attrs 
			# (av, th_attrs) = freshAttrVar (postfixIdent av_ident.id_name postfix) th_attrs
			= (TA_Var av, writePtr av_info_ptr (AVI_Attr (TA_Var av)) th_attrs)

		subst_attr TA_Multi th = (TA_Multi, th)
		subst_attr TA_Unique th = (TA_Unique, th)

		adjust_forall curry_st [] = curry_st
		adjust_forall curry_st=:{st_result} forall_atvs 
			#! st_result = {st_result & at_type = TFA forall_atvs st_result.at_type}
		 	= 	{ curry_st 
				& st_result = st_result
				, st_attr_vars 
					= curry_st.st_attr_vars -- [av \\ {atv_attribute=TA_Var av} <- forall_atvs]
				, st_vars 
					= curry_st.st_vars -- [atv_variable \\ {atv_variable} <- forall_atvs]
				}

	build_body :: 
			!SymbolType 
			![ATypeVar]
			![[ATypeVar]]
			!*TypeHeaps
		->	(!SymbolType, !*TypeHeaps)
	build_body st gatvs arg_gatvss  th
		# th = clearSymbolType st th
		# th = fold2St subst_gatv gatvs arg_gatvss th
		# (st, th) = applySubstInSymbolType st th 
		//# st = add_propagating_inequalities st gatvs arg_gatvss 
		= (st, th)
	where
		subst_gatv gatv=:{atv_variable} arg_gatvs th=:{th_vars}
			#! type_args = [ makeAType (TV atv_variable) atv_attribute 
							\\ {atv_variable, atv_attribute} <- arg_gatvs]
			#! type = (CV atv_variable) :@: type_args
			#! th_vars = writePtr atv_variable.tv_info_ptr (TVI_Type type) th_vars
			= {th & th_vars = th_vars}
			
		add_propagating_inequalities st gatvs arg_gatvss
			# inequalities = zipWith make_inequalities gatvs arg_gatvss 
			= {st & st_attr_env = st.st_attr_env ++ flatten inequalities}
		where
			make_inequalities gatv arg_gatvs 
				= filterOptionals (map (make_inequality gatv) arg_gatvs)
			make_inequality {atv_attribute=TA_Var x} {atv_attribute=TA_Var y} 
				= Yes {ai_offered = x, ai_demanded = y}	// offered <= demanded = outer<=inner = x<=y
			make_inequality _ _
				= No

reportError name pos msg error=:{ea_file} 
	# ea_file = ea_file <<< "Error " <<< (stringPosition name pos) <<< ":" <<< msg <<< '\n'
	= { error & ea_file = ea_file , ea_ok = False }

reportWarning name pos msg error=:{ea_file}
	# ea_file = ea_file <<< "Warning " <<< (newPosition name pos) <<< ":" <<< msg <<< '\n'
	= { error & ea_file = ea_file }
	
//	Type Helpers

makeAType :: !Type !TypeAttribute -> AType
makeAType type attr = {	at_attribute = attr, at_type = type }

makeATypeVar :: !TypeVar !TypeAttribute -> ATypeVar
makeATypeVar tv attr = {atv_variable = tv, atv_attribute = attr}

//----------------------------------------------------------------------------------------
// folding of a AType, depth first 
//----------------------------------------------------------------------------------------

class foldType t :: (Type  .st -> .st) (AType  .st -> .st) t .st -> .st

instance foldType [a] | foldType a where
	foldType on_type on_atype types st 
		= foldSt (foldType on_type on_atype) types st

instance foldType (a,b) | foldType a & foldType b where
	foldType on_type on_atype (x,y) st 
		= foldType on_type on_atype y (foldType on_type on_atype x st)

instance foldType Type where
	foldType on_type on_atype type st
		# st = fold_type type st
		= on_type type st 
	where
		fold_type (TA type_symb args) st = foldType on_type on_atype args st
		fold_type (TAS type_symb args _) st = foldType on_type on_atype args st
		fold_type (l --> r) st = foldType on_type on_atype (l,r) st
		fold_type (TArrow) st = st
		fold_type (TArrow1 t) st = foldType on_type on_atype t st
		fold_type (_ :@: args) st = foldType on_type on_atype args st
		fold_type (TB _) st = st
		fold_type (TFA tvs type) st = foldType on_type on_atype type st
		fold_type (GTV _) st = st
		fold_type (TV _) st = st		
		fold_type t st = abort "foldType: does not match\n" ---> ("type", t)

instance foldType AType where
	foldType on_type on_atype atype=:{at_type} st 
		# st = foldType on_type on_atype at_type st
		= on_atype atype st 

instance foldType TypeContext where
	foldType on_type on_atype {tc_types} st
		= foldType on_type on_atype tc_types st 

//----------------------------------------------------------------------------------------
// mapping of a AType, depth first 
//----------------------------------------------------------------------------------------
class mapTypeSt type :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type before recursion
	(AType -> u:(.st -> u:(AType, .st))) 		// called on each attributed type before recursion
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type after recursion
	(AType -> u:(.st -> u:(AType, .st))) 		// called on each attributed type after recursion	
	type .st -> u:(type, .st)

mapTypeBeforeSt :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type before recursion
	(AType  -> u:(.st -> u:(AType, .st))) 		// called on each attributed type before recursion
	type .st -> u:(type, .st) | mapTypeSt type
mapTypeBeforeSt on_type_before on_atype_before type st
	= mapTypeSt on_type_before on_atype_before idSt idSt type st
	
mapTypeAfterSt :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type after recursion
	(AType  -> u:(.st -> u:(AType, .st))) 		// called on each attributed type after recursion
	type .st -> u:(type, .st) | mapTypeSt type
mapTypeAfterSt on_type_after on_atype_after type st
	= mapTypeSt idSt idSt on_type_after on_atype_after type st

instance mapTypeSt [a] | mapTypeSt a where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
		= mapSt (mapTypeSt on_type_before on_atype_before on_type_after on_atype_after) type st

instance mapTypeSt (a, b) | mapTypeSt a & mapTypeSt b where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after (x, y) st
		#! (x1, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after x st
		#! (y1, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after y st
		= ((x1,y1), st)

instance mapTypeSt Type where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
		#! (type1, st) = on_type_before type st
		#! (type2, st) = map_type type1 st
		#! (type3, st) = on_type_after type2 st
		= (type3, st)
	where
		map_type (TA type_symb_ident args) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (TA type_symb_ident args, st)
		map_type (TAS type_symb_ident args strictness) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (TAS type_symb_ident args strictness, st)
		map_type (l --> r) st 
			#! ((l,r), st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after (l,r) st
			= (l --> r, st)
		map_type TArrow st 	= (TArrow, st)
		map_type (TArrow1 t) st 
			#! (t, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after t st
			= (TArrow1 t, st)
		map_type (cv :@: args) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (cv :@: args, st)
		map_type t=:(TB _) st = (t, st)	
		map_type (TFA tvs type) st 
			#! (type, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
			= (TFA tvs type, st)
		map_type t=:(GTV _) st = (t, st)	
		map_type t=:(TV _) st = (t, st)	
		map_type t st
			= abort "mapTypeSt: type does not match\n" ---> ("type", t)

instance mapTypeSt AType where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after atype st
		#! (atype, st) = on_atype_before atype st 
		#! (at_type, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after atype.at_type st
		= on_atype_after {atype & at_type = at_type} st

instance mapTypeSt TypeContext where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after tc=:{tc_types} st
		#! (tc_types, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after tc_types st
		= ({tc&tc_types=tc_types}, st)

// allocate fresh type variable
freshTypeVar :: !Ident  !*TypeVarHeap -> (!TypeVar, !*TypeVarHeap) 
freshTypeVar name th_vars 
	# (info_ptr, th_vars) = newPtr TVI_Empty th_vars
	= ({tv_ident = name, tv_info_ptr = info_ptr}, th_vars)

// allocate fresh attribute variable
freshAttrVar :: !Ident !*AttrVarHeap -> (!AttributeVar, !*AttrVarHeap)
freshAttrVar name th_attrs
	# (info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
	= ({av_ident = name, av_info_ptr = info_ptr}, th_attrs)

// take a fresh copy of a SymbolType	  
freshSymbolType :: 
		!SymbolType			// symbol type to take fresh 
		!*TypeHeaps			// variable storage
	->  ( !SymbolType		// fresh symbol type
		, !*TypeHeaps		// variable storage
		) 
freshSymbolType st th=:{th_vars, th_attrs}
	#! (fresh_st_vars, th_vars) = mapSt subst_type_var st.st_vars th_vars
	#! (fresh_st_attr_vars, th_attrs) = mapSt subst_attr_var st.st_attr_vars th_attrs
	#! th = {th & th_vars = th_vars, th_attrs = th_attrs}

	#! (fresh_st_args, th) 		= fresh_type st.st_args th
	#! (fresh_st_result, th) 	= fresh_type st.st_result th	
	#! (fresh_st_context, th) 	= fresh_type st.st_context th	
	#! (fresh_st_attr_env, th) 	= mapSt fresh_ineq st.st_attr_env th		
			
	#! fresh_st = 
		{ st
		& st_args = fresh_st_args
		, st_result = fresh_st_result
		, st_context = fresh_st_context
		, st_attr_env = fresh_st_attr_env
		, st_vars = fresh_st_vars
		, st_attr_vars = fresh_st_attr_vars 
		}

	#! th = clearSymbolType fresh_st th
	#! th = clearSymbolType st th

	#! th = assertSymbolType fresh_st th
	#! th = assertSymbolType st th

	= (fresh_st, th)
where
	subst_type_var :: !TypeVar !*TypeVarHeap -> (!TypeVar, !*TypeVarHeap)
	subst_type_var tv=:{tv_info_ptr} th_vars
		# (new_ptr, th_vars) = newPtr TVI_Empty th_vars  
		= ({tv & tv_info_ptr=new_ptr}, writePtr tv_info_ptr (TVI_TypeVar new_ptr) th_vars)

	subst_attr_var :: !AttributeVar !*AttrVarHeap -> (!AttributeVar, !*AttrVarHeap)
	subst_attr_var av=:{av_info_ptr} th_attrs
		# (new_ptr, th_attrs) = newPtr AVI_Empty th_attrs  
		= ({av & av_info_ptr = new_ptr}, writePtr av_info_ptr (AVI_AttrVar new_ptr) th_attrs)			

	fresh_type :: type !*TypeHeaps -> (type, !*TypeHeaps) | mapTypeSt type 
	fresh_type t st = mapTypeBeforeSt on_type on_atype t st
		
	on_type (TV tv) th
		#! (tv, th) = on_type_var tv th	
		= (TV tv, th)		 			 
	on_type (GTV tv) th
		#! (tv, th) = on_type_var tv th	
		= (GTV tv, th)
	on_type (CV tv=:{tv_info_ptr} :@: args) th=:{th_vars}
		#! (tv, th) = on_type_var tv th	
		= (CV tv :@: args, th)
	on_type (TFA atvs type) th
		#! (fresh_atvs, th) = mapSt subst_atv atvs th
		// the variables in the type will be substituted by
		// the recursive call of mapType 
		= (TFA fresh_atvs type, th)
	where
		subst_atv atv=:{atv_variable, atv_attribute}  th=:{th_vars, th_attrs} 
			#! (atv_variable, th_vars) = subst_type_var atv_variable th_vars 
			# (atv_attribute, th_attrs) = subst_attr atv_attribute th_attrs
			=	( {atv & atv_variable = atv_variable, atv_attribute = atv_attribute}
				, {th & th_vars = th_vars, th_attrs = th_attrs})
		subst_attr (TA_Var av=:{av_info_ptr}) th_attrs
			# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
			= case av_info of
				AVI_Empty
					# (av, th_attrs) = subst_attr_var av th_attrs
					-> (TA_Var av, th_attrs)
				AVI_AttrVar av_info_ptr
					-> (TA_Var {av & av_info_ptr = av_info_ptr}, th_attrs)						   
		subst_attr TA_Unique th_attrs 
			= (TA_Unique, th_attrs)
		subst_attr TA_Multi th_attrs 
			= (TA_Multi, th_attrs)
	on_type type th 
		= (type, th)

	on_atype atype=:{at_attribute=TA_Var av} th
		#! (fresh_av, th) = on_attr_var av th 
		= ({atype & at_attribute=TA_Var fresh_av}, th)
	on_atype atype th 
		= (atype, th)

	fresh_ineq :: !AttrInequality !*TypeHeaps -> (!AttrInequality, !*TypeHeaps)
	fresh_ineq 	ai=:{ai_demanded,ai_offered} th
		#! (ai_demanded, th) = on_attr_var ai_demanded th
		#! (ai_offered, th) = on_attr_var ai_offered th
		= ({ai & ai_demanded = ai_demanded, ai_offered = ai_offered}, th)
		
	on_type_var tv=:{tv_info_ptr} th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		#! tv = case tv_info of
			TVI_TypeVar new_ptr -> {tv & tv_info_ptr = new_ptr} 
			_ 					-> abort ("freshSymbolType, invalid tv_info\n" ---> tv_info)
		= (tv, {th & th_vars = th_vars}) 			 

	on_attr_var av=:{av_info_ptr} th=:{th_attrs}
		#! (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		#! av = case av_info of
			AVI_AttrVar new_ptr -> {av & av_info_ptr = new_ptr} 			 
					//---> ("fresh attr var", av.av_ident, ptrToInt av_info_ptr, ptrToInt new_ptr)			
			_  -> abort ("freshSymbolType, invalid av_info\n" ---> av_info)
		= ( av, {th & th_attrs = th_attrs}) 			 

assertSymbolType :: !SymbolType !*TypeHeaps -> *TypeHeaps
assertSymbolType {st_args, st_result, st_context} th
	= foldType on_type on_atype ((st_args, st_result), st_context) th	
where
	on_type :: !Type !*TypeHeaps -> *TypeHeaps
	on_type (TV tv) th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
		#! th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Empty 	-> th
			_ 			-> (abort "TV  tv_info not empty\n") --->(tv, tv_info)	
	on_type (CV tv :@: _) th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
		#! th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Empty 	-> th
			_ 			-> (abort "CV tv_info not empty\n") --->(tv, tv_info)			
	on_type (TFA atvs type) th=:{th_attrs, th_vars}		
		#! th_attrs = foldSt on_av [av \\ {atv_attribute=TA_Var av} <- atvs] th_attrs
		#! th_vars = foldSt on_tv [atv_variable\\{atv_variable} <- atvs] th_vars
		= {th & th_attrs = th_attrs, th_vars = th_vars }
	where 		
		on_av av th_attrs 
			#! (av_info, th_attrs) = readPtr av.av_info_ptr th_attrs
			= case av_info of
			AVI_Empty	-> th_attrs
			_ ->  (abort "TFA av_info not empty\n") --->(av, av_info)
		on_tv tv th_vars
			#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
			= case tv_info of
				TVI_Empty 	-> th_vars
				_ 			-> (abort "TFA tv_info not empty\n") --->(tv, tv_info)					
	on_type _ th = th
		
	on_atype :: !AType !*TypeHeaps -> *TypeHeaps
	on_atype {at_attribute=TA_Var av} th=:{th_attrs}
		#! (av_info, th_attrs) = readPtr av.av_info_ptr th_attrs
		#! th = {th & th_attrs = th_attrs}
		= case av_info of
			AVI_Empty	-> th
			_ ->  (abort "av_info not empty\n") --->(av, av_info)
	on_atype _ th = th

				
// build curried type out of SymbolType
buildCurriedType :: ![AType] !AType !TypeAttribute ![AttrInequality] ![AttributeVar] !String !Int !*AttrVarHeap 
	-> (!AType, ![AttrInequality], ![AttributeVar], !Int, !*AttrVarHeap)
buildCurriedType [] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs 
	= (type, attr_env, attr_vars, attr_store, th_attrs)
buildCurriedType [at=:{at_attribute}] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# atype = makeAType (at --> type) cum_attr
	= (atype, attr_env, attr_vars, attr_store, th_attrs)
buildCurriedType [at=:{at_attribute}:ats] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# (next_cum_attr, new_attr_env, attr_vars, attr_store, th_attrs) = combine_attributes at_attribute cum_attr attr_env attr_vars attr_store th_attrs
	  (res_type, attr_env, attr_vars, attr_store, th_attrs) = buildCurriedType ats type next_cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# atype = makeAType (at --> res_type) cum_attr  
	= (atype, attr_env, attr_vars, attr_store, th_attrs)
where
	combine_attributes TA_Unique cum_attr attr_env attr_vars attr_store th_attrs
		= (TA_Unique, attr_env, attr_vars, attr_store, th_attrs)
	combine_attributes (TA_Var attr_var) (TA_Var cum_attr_var) attr_env attr_vars attr_store th_attrs
		#! (new_attr_var, th_attrs) 
			= freshAttrVar (makeIdent (attr_var_name +++ toString attr_store)) th_attrs	
		# attr_env = 
			[	{ ai_demanded = cum_attr_var,ai_offered = new_attr_var }
			, 	{ ai_demanded = attr_var, ai_offered = new_attr_var }
			: 	attr_env
			]
		= (	TA_Var new_attr_var, attr_env, [new_attr_var:attr_vars], inc attr_store, th_attrs)
	combine_attributes (TA_Var _) cum_attr attr_env attr_vars attr_store th_attrs
		= (cum_attr, attr_env, attr_vars, attr_store, th_attrs)
	combine_attributes _ (TA_Var cum_attr_var) attr_env attr_vars attr_store th_attrs
		#! (new_attr_var, th_attrs) 
			= freshAttrVar (makeIdent (attr_var_name +++ toString attr_store)) th_attrs		
		# attr_env = [	{ ai_demanded = cum_attr_var,ai_offered = new_attr_var }: attr_env]
		= (	TA_Var new_attr_var, attr_env, [new_attr_var:attr_vars], inc attr_store, th_attrs)
	combine_attributes _ cum_attr attr_env attr_vars attr_store th_attrs
		= (cum_attr, attr_env, attr_vars, attr_store, th_attrs)

// Build curried type out of symbol type.
// Starts with TA_Multi cumulative attribute.
// This is the weakest requirement,
// since we do not know how the generic argument will be used
// in the instance functions. It depends on the instance type. 
curryGenericArgType :: !SymbolType !String !*TypeHeaps 
	-> (!SymbolType, !*TypeHeaps)
curryGenericArgType  st=:{st_args, st_result, st_attr_env, st_attr_vars} attr_var_name th=:{th_attrs}
		
	#! (atype, attr_env, attr_vars, attr_store, th_attrs) 
		= buildCurriedType st_args st_result TA_Multi st_attr_env st_attr_vars attr_var_name 1 th_attrs

	# curried_st = 
		{ st 
		& st_args = []
		, st_arity = 0
		, st_result = atype
		, st_attr_env = attr_env
		, st_attr_vars = attr_vars
		}
	= (curried_st, {th & th_attrs = th_attrs})	
		//---> ("curryGenericArgType", st, curried_st)

curryGenericArgType1 :: !SymbolType !String !*TypeHeaps 
	-> (!SymbolType, !*TypeHeaps)
curryGenericArgType1  st=:{st_args, st_result, st_attr_env, st_attr_vars} attr_var_name th=:{th_attrs}
	# (atype, attr_vars, av_num, th_attrs) = curry st_args st_result 1 th_attrs
	# curried_st = {st & st_args = [], st_arity = 0, st_result = atype, st_attr_vars = attr_vars}
	= (curried_st, {th & th_attrs = th_attrs})	
where
	// outermost closure gets TA_Multi attribute
	curry [] res av_num th_attrs
		= (res, [], av_num, th_attrs)
	curry [arg:args] res av_num th_attrs
		#! (res, avs, av_num, th_attrs) = curry1 args res av_num th_attrs
 		#! atype = makeAType (arg --> res) TA_Multi
		= (atype, avs, av_num, th_attrs)
		
	// inner closures get TA_Var attributes	
	curry1 [] res av_num th_attrs
		= (res, [], av_num, th_attrs)	 	
	curry1 [arg:args] res av_num th_attrs
		#! (res, avs, av_num, th_attrs) = curry1 args res av_num th_attrs
		#! (av, th_attrs) = freshAttrVar (makeIdent (attr_var_name +++ toString av_num)) th_attrs
 		#! atype = makeAType (arg --> res) (TA_Var av)
		= (atype, [av:avs], inc av_num, th_attrs)

// write empty value in the variable heaps 

clearType t th 
	= foldType clear_type clear_atype t th
where
	clear_type (TV tv) th = clear_type_var tv th	
	clear_type (GTV tv) th = clear_type_var tv th
	clear_type (CV tv :@: _) th = clear_type_var tv th
	clear_type (TFA atvs type) th
		#! th = foldSt clear_attr [atv_attribute \\ {atv_attribute} <- atvs] th
		#! th = foldSt clear_type_var [atv_variable \\ {atv_variable} <- atvs] th
		= th
	clear_type _ th = th

	clear_atype {at_attribute} th 
		= clear_attr at_attribute th

	clear_attr (TA_Var av) th = clear_attr_var av th
	clear_attr (TA_RootVar av) th = clear_attr_var av th
	clear_attr _ th = th
		
	clear_type_var {tv_info_ptr} th=:{th_vars} 
		= {th & th_vars = writePtr tv_info_ptr TVI_Empty th_vars} 

	clear_attr_var {av_info_ptr} th=:{th_attrs} 
		= {th & th_attrs = writePtr av_info_ptr AVI_Empty th_attrs} 

clearSymbolType st th
	// clears not only st_vars and st_attrs, but also TFA variables
	= clearType ((st.st_result, st.st_args), st.st_context) th

// collect variables

collectTypeVarsAndAttrVars ::
		!type 
		!*TypeHeaps
	-> 	(![TypeVar]
		,![AttributeVar]
		,!*TypeHeaps
		)
	| foldType type	 
collectTypeVarsAndAttrVars type th
	#! th = clearType type th
	#! (tvs, avs, th) = foldType collect_type_var collect_attr type ([], [], th)
	#! th = clearType type th
	= (tvs, avs, th)
where
	collect_type_var (TV tv) st = add_type_var tv st
	collect_type_var (GTV tv) st = add_type_var tv st
	collect_type_var (CV tv :@: _) st = add_type_var tv st
	collect_type_var (TFA forall_atvs type) (tvs, avs, th_vars) 
		#! forall_tvs = [atv_variable\\{atv_variable}<-forall_atvs]
		#! forall_avs = [av \\ {atv_attribute=TA_Var av}<-forall_atvs]
		= (tvs -- forall_tvs, avs -- forall_avs, th_vars)
				//---> ("collectTypeVarsAndAttrVars TFA", tvs, forall_tvs, tvs -- forall_tvs)
	collect_type_var t st = st
		
	add_type_var tv (tvs, avs, th=:{th_vars})
		# (was_used, th_vars) = markTypeVarUsed tv th_vars
		# th = {th & th_vars = th_vars}
		| was_used 
			= (tvs, avs, th)
				//---> ("collectTypeVarsAndAttrVars: TV was used", tv)
			= ([tv:tvs], avs, th)
				//---> ("collectTypeVarsAndAttrVars: TV was not used", tv)
	
	collect_attr {at_attribute} st = collect_attr_var at_attribute st
	
	collect_attr_var (TA_Var av) st = add_attr_var av st
	collect_attr_var (TA_RootVar av) st = add_attr_var av st
	collect_attr_var _ st = st
				
	add_attr_var av (atvs, avs, th=:{th_attrs})		
		# (was_used, th_attrs) = markAttrVarUsed av th_attrs
		# th = {th & th_attrs = th_attrs}
		| was_used 
			= (atvs, avs, th)
			= (atvs, [av:avs], th)

collectTypeVars type th
	# (tvs, _, th) = collectTypeVarsAndAttrVars type th
	= (tvs, th)
collectAttrVars type th 
	# (_, avs, th) = collectTypeVarsAndAttrVars type th
	= (avs, th)

collectAttrsOfTypeVars :: ![TypeVar] type !*TypeHeaps -> (![ATypeVar], !*TypeHeaps) | foldType type
collectAttrsOfTypeVars tvs type th
	#! (th=:{th_vars}) = clearType type th
	
	# th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Used h) tvs th_vars 
	
	#! (atvs, th_vars) = foldType on_type on_atype type ([], th_vars)

	# th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Empty h) tvs th_vars 

 	#! th = clearType type {th & th_vars= th_vars}
	= (atvs, th)
where
	on_type type st = st

	on_atype {at_type=TV tv, at_attribute} st = on_type_var tv at_attribute st				 	 
	on_atype {at_type=GTV tv, at_attribute} st = on_type_var tv at_attribute st				 	 
	on_atype {at_type=(CV tv :@: _), at_attribute} st = on_type_var tv at_attribute st
	//??? TFA -- seems that it is not needed
 	on_atype _ st = st 	

	on_type_var tv=:{tv_info_ptr} attr (atvs, th_vars)	
	 	#! (tvi, th_vars) = readPtr tv_info_ptr th_vars
	 	= case tvi of
	 		TVI_Used
	 			# th_vars = writePtr tv_info_ptr TVI_Empty th_vars
	 			-> ([makeATypeVar tv attr : atvs], th_vars)
	 		TVI_Empty 
	 			-> (atvs, th_vars) 

collectAttrsOfTypeVarsInSymbolType tvs {st_args, st_result} th
 	= collectAttrsOfTypeVars tvs [st_result:st_args] th  

// marks empty type vars used,
// returns whether the type var was already used	 	  
markTypeVarUsed tv=:{tv_info_ptr} th_vars
	# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
	= case tv_info of
		TVI_Empty -> (False, writePtr tv_info_ptr TVI_Used th_vars)
		TVI_Used  -> (True, th_vars)
		_ -> (abort "markTypeVarUsed: wrong tv_info ") ---> (tv, tv_info)

// marks empty attr vars  used
// returns whether the attr var was already used		
markAttrVarUsed {av_info_ptr} th_attrs
	# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
	= case av_info of
		AVI_Empty -> (False, writePtr av_info_ptr AVI_Used th_attrs)
		AVI_Used  -> (True, th_attrs)

simplifyTypeApp :: !Type ![AType] -> Type
simplifyTypeApp (TA type_cons=:{type_arity} cons_args) type_args
	= TA { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args)
simplifyTypeApp (TAS type_cons=:{type_arity} cons_args strictness) type_args
	= TAS { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args) strictness
simplifyTypeApp (CV tv :@: type_args1) type_args2 = CV tv :@: (type_args1 ++ type_args2)
simplifyTypeApp TArrow [type1, type2] = type1 --> type2
simplifyTypeApp TArrow [type] = TArrow1 type
simplifyTypeApp (TArrow1 type1) [type2] = type1 --> type2
simplifyTypeApp (TV tv) type_args = CV tv :@: type_args
simplifyTypeApp (TB _) type_args = TE
simplifyTypeApp (TArrow1 _) type_args = TE
		
// substitutions

// Uninitialized variables are not substituted, but left intact
//
// This behaviour is needed for kind indexing generic types,
// where generic variables are substituted and non-generic variables
// are not
//
applySubst :: !type !*TypeHeaps -> (!type, !*TypeHeaps) | mapTypeSt type 
applySubst type th
	= mapTypeAfterSt on_type on_atype type th
where
	on_type type=:(TV {tv_info_ptr}) th=:{th_vars}
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars 
		# th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Type t -> (t, th)
			TVI_Empty -> (type, th) 
	on_type (GTV _) th 
		= abort "GTV"
	on_type type=:(CV {tv_info_ptr} :@: args) th=:{th_vars}
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars 
		# th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Type t -> (simplifyTypeApp t args, th)
			TVI_Empty  -> (type, th) 

	//on_type type=:(TFA atvs t) th=:{th_vars}
	//	= abort "applySubst TFA" 

	on_type type th
		= (type, th)

	on_atype atype=:{at_attribute} th=:{th_attrs}	
		# (at_attribute, th_attrs) = subst_attr at_attribute th_attrs
		= ({atype & at_attribute = at_attribute}, {th & th_attrs = th_attrs})

	subst_attr attr=:(TA_Var {av_info_ptr}) th_attrs
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		= case av_info of
			AVI_Attr a -> (a, th_attrs)
			AVI_Empty -> (attr, th_attrs) 
	subst_attr (TA_RootVar {av_info_ptr}) th_attrs
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		= case av_info of
			AVI_Attr a -> (a, th_attrs)
	subst_attr TA_Multi th = (TA_Multi, th)
	subst_attr TA_Unique th = (TA_Unique, th)

applySubstInSymbolType st=:{st_args, st_result, st_attr_env, st_context} th
	#! (new_st_args, th) 	= applySubst st.st_args th
	#! (new_st_result, th) 	= applySubst st.st_result th	
	#! (new_st_context, th) 	= applySubst st.st_context th	
	#! (new_st_attr_env, th)	= mapSt subst_ineq st.st_attr_env th		
	
	#! th = clear_type_vars st.st_vars th
	#! th = clear_attr_vars st.st_attr_vars th
		
	#! (new_st_vars, new_st_attr_vars, th) 
		= collectTypeVarsAndAttrVars ((new_st_args,new_st_result), new_st_context) th

	#! new_st = 
		{ st
		& st_args = new_st_args
		, st_result = new_st_result
		, st_context = new_st_context
		, st_attr_env = new_st_attr_env
		, st_vars = new_st_vars
		, st_attr_vars = new_st_attr_vars 
		}
		
	#! th = clearSymbolType st th	

	#! th = assertSymbolType new_st th
	#! th = assertSymbolType st th
		
	= (new_st, th)
		//---> ("applySubstInSymbolType", new_st)
where 
	subst_ineq 	ai=:{ai_demanded,ai_offered} th
		# (ai_demanded, th) = subst_attr_var ai_demanded th
		# (ai_offered, th) = subst_attr_var ai_offered th
		= ({ai & ai_demanded = ai_demanded, ai_offered = ai_offered}, th)
	subst_attr_var  av=:{av_info_ptr} th=:{th_attrs}
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
		# th = {th & th_attrs = th_attrs}
		= case av_info of
			AVI_Attr (TA_Var av1) -> (av1, th)
			AVI_Attr _ -> (av, th)
			AVI_Empty -> (av, th)
	clear_type_vars tvs th=:{th_vars}
		#! th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Empty h) tvs th_vars
		= {th & th_vars = th_vars}
	clear_attr_vars avs th=:{th_attrs}
		#! th_attrs = foldSt (\{av_info_ptr} h->writePtr av_info_ptr AVI_Empty h) avs th_attrs
		= {th & th_attrs = th_attrs}				

expandSynonymType :: !CheckedTypeDef !TypeAttribute ![AType] !*TypeHeaps -> (!Type, !*TypeHeaps)
expandSynonymType {td_rhs=SynType {at_type}, td_args, td_attribute} ta_attr ta_args th
	#! th_attrs = bind_attribute td_attribute ta_attr th.th_attrs
	#! th = fold2St bind_type_and_attr td_args ta_args { th & th_attrs = th_attrs }
	#! (at_type, th) = applySubst at_type th
	#! th_attrs = clear_attribute td_attribute th.th_attrs
	#! th = foldSt clear_type_and_attr td_args { th & th_attrs = th_attrs }
	= (at_type, th)   
where
	bind_type_and_attr {atv_attribute, atv_variable={tv_info_ptr}} {at_type,at_attribute} type_heaps=:{th_vars,th_attrs}
		= { type_heaps &	th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type),
							th_attrs = bind_attribute atv_attribute at_attribute th_attrs }
		
	bind_attribute (TA_Var {av_info_ptr}) attr th_attrs
		= th_attrs <:= (av_info_ptr, AVI_Attr attr)
	bind_attribute _ _ th_attrs
		= th_attrs

	clear_type_and_attr {atv_attribute, atv_variable={tv_info_ptr}} type_heaps=:{th_vars,th_attrs}
		= { type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Empty), th_attrs = clear_attribute atv_attribute th_attrs }
		
	clear_attribute (TA_Var {av_info_ptr}) th_attrs
		= th_attrs <:= (av_info_ptr, AVI_Empty)
	clear_attribute _ th_attrs
		= th_attrs
expandSynonymType td ta_attr ta_args th = abort "expanding not a synonym type\n" 

//	Function Helpers

makeFunction :: !Ident !Index ![FreeVar] !Expression !(Optional SymbolType) !Index !Position -> FunDef
makeFunction ident group_index arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos	
	#! (arg_vars, local_vars, free_vars) = collectVars body_expr arg_vars	
	| not (isEmpty free_vars)
		= abort "makeFunction: free_vars is not empty\n"
	=	{ fun_ident = ident
		, fun_arity = length arg_vars
		, fun_priority = NoPrio
		, fun_body = TransformedBody {tb_args = arg_vars, tb_rhs = body_expr }
		, fun_type = opt_sym_type
		, fun_pos = fun_pos
		, fun_kind  = FK_Function cNameNotLocationDependent
		, fun_lifted = 0
		, fun_info = 
			{ fi_calls = collectCalls main_dcl_module_n body_expr
			, fi_group_index = group_index
			, fi_def_level = NotALevel
			, fi_free_vars =  []
			, fi_local_vars = local_vars
			, fi_dynamics = []
			, fi_properties = 0
			}	
		}

buildFunAndGroup :: !Ident ![FreeVar] !Expression !(Optional SymbolType) !Index !Position !FunsAndGroups -> (!DefinedSymbol, FunsAndGroups)
buildFunAndGroup 
		ident arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos 
		funs_and_groups=:{fg_fun_index,fg_group_index,fg_funs,fg_groups}
	# fun = makeFunction ident fg_group_index arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos
	# group = {group_members = [fg_fun_index]}
	# def_sym = {ds_ident=ident, ds_arity=fun.fun_arity, ds_index=fg_fun_index}
	  funs_and_groups = {funs_and_groups & fg_fun_index=fg_fun_index+1, fg_group_index=fg_group_index+1, fg_funs=[fun:fg_funs], fg_groups=[group:fg_groups]}
	= (def_sym, funs_and_groups)

buildFunAndGroup2 :: !Ident ![FreeVar] !Expression !Index !FunsAndGroups -> (!Index, !FunsAndGroups)
buildFunAndGroup2 ident arg_vars body_expr main_dcl_module_n funs_and_groups=:{fg_fun_index,fg_group_index,fg_funs,fg_groups}
	# fun = makeFunction ident fg_group_index arg_vars body_expr No main_dcl_module_n NoPos
	  group = {group_members = [fg_fun_index]}
	  funs_and_groups = {funs_and_groups & fg_fun_index=fg_fun_index+1, fg_group_index=fg_group_index+1, fg_funs=[fun:fg_funs], fg_groups=[group:fg_groups]}
	= (fg_fun_index, funs_and_groups)
	
//	Expr Helpers

// Primitive expressions

makeIntExpr :: Int -> Expression
makeIntExpr value = BasicExpr (BVI (toString value))

makeStringExpr :: String -> Expression
makeStringExpr str
	=  BasicExpr (BVS (adjust_string str))
where
	adjust_string str
		= { ch \\ ch <- ['\"'] ++ adjust_chars [ch \\ ch <-: str] ++ ['\"'] }
	adjust_chars [] = []
	adjust_chars ['\\':cs] 	= ['\\','\\' : adjust_chars cs]
	adjust_chars [c:cs] 	= [c : adjust_chars cs]
		
makeListExpr :: [Expression] !PredefinedSymbols !*Heaps -> (Expression, !*Heaps)
makeListExpr [] predefs heaps
	= buildPredefConsApp PD_NilSymbol [] predefs heaps
makeListExpr [expr:exprs] predefs heaps 
	# (list_expr, heaps) = makeListExpr exprs predefs heaps 
	= buildPredefConsApp PD_ConsSymbol [expr, list_expr] predefs heaps

buildConsApp :: !Index DefinedSymbol ![Expression] !*Heaps 
	-> (!Expression, !*Heaps) 
buildConsApp cons_mod {ds_ident, ds_index, ds_arity} arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# cons_glob = {glob_module = cons_mod, glob_object = ds_index}
	# expr = App {
		app_symb = {
			symb_ident = ds_ident, 
			symb_kind = SK_Constructor cons_glob
			}, 
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = { heaps & hp_expression_heap = hp_expression_heap } 
	= (expr, heaps)	

buildFunApp :: !Index !DefinedSymbol ![Expression] !*Heaps -> (!Expression, !*Heaps) 
buildFunApp fun_mod {ds_ident, ds_index} arg_exprs heaps
	= buildFunApp2 fun_mod ds_index ds_ident arg_exprs heaps

buildFunApp2 :: !Index !Index !Ident ![Expression] !*Heaps -> (!Expression, !*Heaps) 
buildFunApp2 fun_mod ds_index ds_ident arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# fun_glob = {glob_module = fun_mod, glob_object = ds_index}
	# expr = App {
		app_symb = {symb_ident = ds_ident, symb_kind = SK_Function fun_glob},
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (expr, heaps)	

buildPredefFunApp :: !Int [Expression] !PredefinedSymbols !*Heaps
	-> (!Expression, !*Heaps)
buildPredefFunApp predef_index args predefs heaps
	# {pds_module, pds_def} = predefs.[predef_index]
 	= buildFunApp2 pds_module pds_def predefined_idents.[predef_index] args heaps

buildGenericApp :: !Index !Index !Ident !TypeKind ![Expression] !*Heaps
	-> (!Expression, !*Heaps)
buildGenericApp gen_module gen_index gen_ident kind arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# glob_index = {glob_module = gen_module, glob_object = gen_index}
	# expr = App {
		app_symb = {symb_ident = gen_ident, symb_kind = SK_Generic glob_index kind}, 
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (expr, heaps)	

buildPredefConsApp :: !Int [Expression] !PredefinedSymbols !*Heaps
	-> (!Expression, !*Heaps)
buildPredefConsApp predef_index args predefs heaps=:{hp_expression_heap}
	# {pds_module, pds_def} = predefs.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# global_index = {glob_module = pds_module, glob_object = pds_def}
	# symb_ident = 
		{ symb_ident = pds_ident 
		, symb_kind = SK_Constructor global_index
		}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# app = App {app_symb = symb_ident, app_args = args, app_info_ptr = expr_info_ptr} 
	= (app, {heaps & hp_expression_heap = hp_expression_heap})

buildPredefConsPattern :: !Int ![FreeVar] !Expression !PredefinedSymbols
	-> AlgebraicPattern
buildPredefConsPattern predef_index vars expr predefs
	# {pds_module, pds_def} = predefs.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# cons_def_symbol = {
		ds_ident = pds_ident,
		ds_arity = length vars,
		ds_index = pds_def
		}
	# pattern = {
		ap_symbol = {glob_module = pds_module, glob_object = cons_def_symbol},
		ap_vars = vars,
		ap_expr = expr,
		ap_position = NoPos		
		}
	= pattern

buildCaseExpr :: Expression CasePatterns !*Heaps 
	-> (!Expression, !*Heaps)
buildCaseExpr case_arg case_alts heaps=:{hp_expression_heap}	
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# expr = Case 
		{ case_expr = case_arg
		, case_guards = case_alts
		, case_default = No
		, case_ident = No
		, case_info_ptr = expr_info_ptr
		, case_explicit = False
		, case_default_pos = NoPos 
		}
	# heaps = { heaps & hp_expression_heap = hp_expression_heap}	
	= (expr, heaps)

build_map_from_tvi_expr (TVI_Expr is_bimap_id bimap_expr) main_module_index predefs heaps
	= (buildRecordSelectionExpr bimap_expr PD_map_from 1 predefs, heaps)
build_map_from_tvi_expr (TVI_Iso iso_ds to_ds from_ds) main_module_index predefs heaps
	= buildFunApp main_module_index from_ds [] heaps

build_map_from_expr bimap_expr predefs
	= buildRecordSelectionExpr bimap_expr PD_map_from 1 predefs

build_map_to_tvi_expr (TVI_Expr is_bimap_id bimap_expr) main_module_index predefs heaps
	= (buildRecordSelectionExpr bimap_expr PD_map_to 0 predefs, heaps)
build_map_to_tvi_expr (TVI_Iso iso_ds to_ds from_ds) main_module_index predefs heaps
	= buildFunApp main_module_index to_ds [] heaps

build_map_to_expr bimap_expr predefs
	= buildRecordSelectionExpr bimap_expr PD_map_to 0 predefs

buildRecordSelectionExpr :: !Expression !Index !Int !PredefinedSymbols -> Expression
buildRecordSelectionExpr record_expr predef_field field_n predefs 
	# {pds_module, pds_def} = predefs . [predef_field]
	# pds_ident = predefined_idents . [predef_field]
	# selector = { 
		glob_module = pds_module, 
		glob_object = {ds_ident = pds_ident, ds_index = pds_def, ds_arity = 1}}
	= Selection NormalSelector record_expr [RecordSelection selector field_n]

// variables

// build a new variable and an expression associated with it
buildVarExpr :: 
		!String 			// variable name
		!*Heaps	
	-> (!Expression 		// variable expression
		, !FreeVar 			// variable
		, !*Heaps
		)
buildVarExpr name heaps=:{hp_var_heap, hp_expression_heap} 
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# (var_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap
	# var_ident = makeIdent name
	# var = Var {var_ident = var_ident, var_expr_ptr = expr_info_ptr, var_info_ptr = var_info_ptr } 
	# hp_var_heap = writePtr var_info_ptr (VI_Expression var) hp_var_heap
	# heaps = { heaps & hp_var_heap = hp_var_heap, hp_expression_heap = hp_expression_heap }
	# fv = {fv_count = 1/* if 0, trans crashes*/, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel} 
	= (var, fv, heaps)

buildVarExprs [] heaps = ([], [], heaps)
buildVarExprs [x:xs] heaps
	# (y, z, heaps) = buildVarExpr x heaps
	# (ys, zs, heaps) = buildVarExprs xs heaps
	= ([y:ys], [z:zs], heaps)

// recursion over expressions

//-----------------------------------------------------------------------------
// fold expression applies a function to each node of an expression
// recursively:
// first apply the function, then recurse
//-----------------------------------------------------------------------------
foldExpr :: 
		(Expression -> .st -> .st)  	// function to apply at each node
		Expression 						// expression to run throuh
		.st 							// state
	-> 
		.st								// updated state 
foldExpr f expr=:(Var _) st 
	= f expr st
foldExpr f expr=:(App {app_args}) st 
	# st = f expr st
	= foldSt (foldExpr f) app_args st
foldExpr f expr=:(expr1 @ exprs) st
	# st = f expr st
	= foldSt (foldExpr f) [expr1:exprs] st	
foldExpr f expr=:(Let {let_lazy_binds, let_strict_binds, let_expr}) st
	# st = f expr st
	# st = foldSt (fold_let_binds f) let_strict_binds st 
	# st = foldSt (fold_let_binds f) let_lazy_binds st 
	= foldExpr f let_expr st 
where
	fold_let_binds f {lb_src} st = foldExpr f lb_src st 
foldExpr f expr=:(Case {case_expr,case_guards,case_default}) st
	# st = f expr st
	# st = foldExpr f case_expr st
	# st = fold_guards f case_guards st 
	# st = foldOptional (foldExpr f) case_default st
	= st
where
	fold_guards f (AlgebraicPatterns gi aps) st = foldSt (foldExpr f) [ap_expr\\{ap_expr}<-aps] st
	fold_guards f (BasicPatterns gi bps) st = foldSt (foldExpr f) [bp_expr\\{bp_expr}<-bps] st
	fold_guards f (DynamicPatterns dps) st = foldSt (foldExpr f) [dp_rhs\\{dp_rhs}<-dps] st
	fold_guards f NoPattern st = st
foldExpr f expr=:(Selection _ expr1 _) st
	# st = f expr st
  	= foldExpr f expr1 st 	
foldExpr f expr=:(Update expr1 sels expr2) st
	# st = f expr st
	# st = foldExpr f expr1 st 
	# st = foldSt (fold_sel f) sels st 
	# st = foldExpr f expr2 st 
	= st
where
	fold_sel f (RecordSelection _ _) st = st
	fold_sel f (ArraySelection _ _ expr) st = foldExpr f expr st
	fold_sel f (DictionarySelection _ _ _ expr) st = foldExpr f expr st
foldExpr f expr=:(RecordUpdate _ expr1 binds) st
	# st = f expr st
	# st = foldExpr f expr1 st 
	# st = foldSt (foldExpr f) [bind_src\\{bind_src}<-binds] st
	= st
foldExpr f expr=:(TupleSelect _ _ expr1) st 
	# st = f expr st
	= foldExpr f expr1 st
foldExpr f expr=:(BasicExpr _) st
	= f expr st	
foldExpr f expr=:(Conditional {if_cond,if_then,if_else}) st
	# st = f expr st
	# st = foldExpr f if_cond st	
	# st = foldExpr f if_then st	
	# st = foldOptional (foldExpr f) if_else st	
	= st
foldExpr f expr=:(MatchExpr _ expr1) st 
	# st = f expr st
	= foldExpr f expr1 st
foldExpr f expr=:(DynamicExpr {dyn_expr}) st 
	# st = f expr st
	= foldExpr f dyn_expr st
foldExpr f EE st 
	= st
foldExpr f expr st 
	= abort "generic.icl: foldExpr does not match\n"

// needed for collectCalls
instance == FunCall where (==) (FunCall x _) (FunCall y _) = x == y

// collect function calls made in the expression
collectCalls :: !Index !Expression -> 	[FunCall]
collectCalls current_module expr = removeDup (foldExpr get_call expr [])
where
	get_call (App {app_symb={symb_kind=SK_Function {glob_module,glob_object}, symb_ident}}) indexes
		| glob_module == current_module
			= [FunCall glob_object NotALevel : indexes]
				//---> ("collect call ", symb_ident, glob_object)
			= indexes
				//---> ("do not collect call ", symb_ident, glob_module, glob_object)
	get_call _ indexes = indexes

// collects variables and computes the refernce counts
collectVars :: 
		!Expression 	// expression to collect variables in
		![FreeVar] 		// function argument variables
	-> (  ![FreeVar]	// argument variables (with updated ref count)
		, ![FreeVar]	// local variables
		, ![FreeVar]	// free_variables
		)
collectVars expr arg_vars  
	# arg_vars = [ {v & fv_count = 0} \\ v <- arg_vars]
	= foldExpr collect_vars expr (arg_vars, [], [])
where
	collect_vars (Var {var_ident, var_info_ptr}) (arg_vars, local_vars, free_vars)
		# var = {fv_ident = var_ident, fv_count = 1, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel}
		# (added, arg_vars) = add_var var arg_vars
		| added 
			= (arg_vars, local_vars, free_vars)
		# (added, local_vars) = add_var var local_vars
		| added 
			= (arg_vars, local_vars, free_vars)
		# (added, free_vars) = add_var var free_vars
		| added 
			= (arg_vars, local_vars, free_vars)				
		= (arg_vars, local_vars, [var:free_vars])
	where
		add_var var [] = (False, [])
		add_var var [v=:{fv_count,fv_info_ptr}:vs]
			| var.fv_info_ptr == fv_info_ptr
				= (True, [{v&fv_count = inc fv_count}:vs])
				# (added, vs) = add_var var vs
				= (added, [v:vs])	
	collect_vars (Let {let_lazy_binds, let_strict_binds}) (arg_vars, local_vars, free_vars)
		# vars = [{lb_dst&fv_count=0} \\ {lb_dst} <- (let_lazy_binds ++ let_strict_binds)]
		# (local_vars, free_vars) = foldSt add_local_var vars (local_vars, free_vars) 
		= (arg_vars, local_vars, free_vars)
	collect_vars (Case {case_guards}) (arg_vars, local_vars, free_vars)
		# vars = [{v&fv_count=0} \\ v <- collect case_guards]
		# (local_vars, free_vars) = foldSt add_local_var vars (local_vars, free_vars) 
		= (arg_vars, local_vars, free_vars) 
	where
		collect (AlgebraicPatterns _ aps) = flatten [ap_vars\\{ap_vars}<-aps]
		collect (BasicPatterns _ bps) = []
		collect (DynamicPatterns dps) = [dp_var \\ {dp_var}<-dps]
		collect NoPattern = []
	collect_vars expr st = st		

	add_local_var var (local_vars, []) = ([var:local_vars], [])
	add_local_var var (local_vars, free_vars=:[fv:fvs])
		| var.fv_info_ptr == fv.fv_info_ptr 
			= ([fv:local_vars], fvs)
			# (local_vars, fvs1) = add_local_var var (local_vars, fvs)
			= (local_vars, [fv:fvs1])

// Array helpers

//updateArraySt :: (a .st -> (a, .st)) *{a} .st -> (*{a}, .st) 
updateArraySt f xs st
	:== map_array 0 xs st
where
	map_array n xs st
		| n == size xs
			= (xs, st)
			# (x, xs) = xs![n]	
			# (x, st) = f x st			
			= map_array (inc n) {xs&[n]=x} st

//foldArraySt :: (a .st -> .st) {a} .st -> .st 
foldArraySt f xs st
	:== fold_array 0 xs st
where
	fold_array n xs st
		| n == size xs
			= st	
			# st = f xs.[n] st
			= fold_array (inc n) xs st

//	General Helpers

idSt x st = (x, st)

(--) infixl 5 :: u:[a] .[a] -> u:[a] | Eq a
(--) x y = removeMembers x y 

// should actually be in the standard library
transpose []             = []
transpose [[] : xss]     = transpose xss
transpose [[x:xs] : xss] = 
	[[x : [hd l \\ l <- xss]] : transpose [xs : [ tl l \\  l <- xss]]]

foldOptional f No st = st
foldOptional f (Yes x) st = f x st

filterOptionals [] = []
filterOptionals [No : xs] 	= filterOptionals xs
filterOptionals [Yes x : xs] = [x : filterOptionals xs]

zipWith f [] [] = []
zipWith f [x:xs] [y:ys] = [f x y : zipWith f xs ys]
zipWith f _ _ = abort "zipWith: lists of different length\n"

zipWithSt f l1 l2 st
	:== zipWithSt l1 l2 st
where
	zipWithSt [] [] st 
		= ([], st)
	zipWithSt [x:xs] [y:ys] st
		# (z, st) = f x y st
		# (zs, st) = zipWithSt xs ys st
		= ([z:zs], st) 

zipWithSt2 f l1 l2 st1 st2
	:== zipWithSt2 l1 l2 st1 st2
where
	zipWithSt2 [] [] st1 st2
		= ([], st1, st2)
	zipWithSt2 [x:xs] [y:ys] st1 st2
		# (z, st1, st2) = f x y st1 st2
		# (zs, st1, st2) = zipWithSt2 xs ys st1 st2
		= ([z:zs], st1, st2)

mapSdSt f l sd s :== map_sd_st l s
where
	map_sd_st [x : xs] s
	 	# (x, s) = f x sd s
		  (xs, s) = map_sd_st xs s
		#! s = s
		= ([x : xs], s)
	map_sd_st [] s
		#! s = s
	 	= ([], s)
