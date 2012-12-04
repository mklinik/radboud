implementation module trans

import StdEnv

import syntax, transform, checksupport, StdCompare, check, utilities, unitype, typesupport, type
import classify, partition

SwitchCaseFusion			fuse dont_fuse :== fuse
SwitchGeneratedFusion		fuse dont_fuse :== fuse
SwitchFunctionFusion		fuse dont_fuse :== fuse
SwitchConstructorFusion		fuse fuse_generic_constructors dont_fuse :== fuse_generic_constructors
SwitchRnfConstructorFusion  rnf  linear	   :== rnf
SwitchCurriedFusion			fuse xtra dont_fuse :== fuse 
SwitchExtraCurriedFusion	fuse macro	   :== fuse//(fuse && macro)//fuse
SwitchTrivialFusion			fuse dont_fuse :== fuse
SwitchUnusedFusion			fuse dont_fuse :== fuse
SwitchTransformConstants	tran dont_tran :== tran
SwitchSpecialFusion			fuse dont_fuse :== fuse
SwitchArityChecks			check dont_check :== check
SwitchAutoFoldCaseInCase	fold dont	   :== fold
SwitchAutoFoldAppInCase		fold dont	   :== fold
SwitchAlwaysIntroduceCaseFunction yes no   :== no
SwitchNonRecFusion			fuse dont_fuse :== dont_fuse
SwitchHOFusion				fuse dont_fuse :== fuse
SwitchHOFusion`				fuse dont_fuse :== fuse
SwitchStrictPossiblyAddLet  strict lazy    :== lazy//strict

(-!->) infix
(-!->) a b :== a  // ---> b
(<-!-) infix
(<-!-) a b :== a  // <--- b

fromYes (Yes x) = x

is_SK_Function_or_SK_LocalMacroFunction (SK_Function _) = True
is_SK_Function_or_SK_LocalMacroFunction (SK_LocalMacroFunction _) = True
is_SK_Function_or_SK_LocalMacroFunction _ = False

undeff :== -1

empty_atype = { at_attribute = TA_Multi, at_type = TE }

get_producer_symbol (PR_Curried symbol arity)
	= (symbol,arity)
get_producer_symbol (PR_Function symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_GeneratedFunction symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_Constructor symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_CurriedFunction symbol arity _)
	= (symbol,arity)

// Extended variable info accessors...

readVarInfo :: VarInfoPtr *VarHeap -> (VarInfo, !*VarHeap)
readVarInfo var_info_ptr var_heap
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_Extended _ original_var_info	-> (original_var_info, var_heap)
		_								-> (var_info, var_heap)

readExtendedVarInfo :: VarInfoPtr *VarHeap -> (ExtendedVarInfo, !*VarHeap)
readExtendedVarInfo var_info_ptr var_heap
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_Extended extensions _	-> (extensions, var_heap)
		_							-> abort "Error in compiler: 'readExtendedVarInfo' failed in module trans.\n"

writeVarInfo :: VarInfoPtr VarInfo *VarHeap -> *VarHeap
writeVarInfo var_info_ptr new_var_info var_heap
	# (old_var_info, var_heap) = readPtr var_info_ptr var_heap
	= case old_var_info of
		VI_Extended extensions _	-> writePtr var_info_ptr (VI_Extended extensions new_var_info) var_heap
		_							-> writePtr var_info_ptr new_var_info var_heap

setExtendedVarInfo :: !VarInfoPtr !ExtendedVarInfo !*VarHeap -> *VarHeap
setExtendedVarInfo var_info_ptr extension var_heap
	# (old_var_info, var_heap) = readPtr var_info_ptr var_heap
	= case old_var_info of
		VI_Extended _ original_var_info	-> writePtr var_info_ptr (VI_Extended extension original_var_info) var_heap
		_								-> writePtr var_info_ptr (VI_Extended extension old_var_info) var_heap

// Extended expression info accessors...

readExprInfo :: !ExprInfoPtr !*ExpressionHeap -> (!ExprInfo,!*ExpressionHeap)
readExprInfo expr_info_ptr symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended _ ei	-> (ei, symbol_heap)
		_					-> (expr_info, symbol_heap)

writeExprInfo :: !ExprInfoPtr !ExprInfo !*ExpressionHeap -> *ExpressionHeap
writeExprInfo expr_info_ptr new_expr_info symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended extensions _	-> writePtr expr_info_ptr (EI_Extended extensions new_expr_info) symbol_heap
		_							-> writePtr expr_info_ptr new_expr_info symbol_heap

app_EEI_ActiveCase transformer expr_info_ptr expr_heap
	# (expr_info, expr_heap) = readPtr expr_info_ptr expr_heap
	= case expr_info of
		(EI_Extended (EEI_ActiveCase aci) original_expr_info)
			-> writePtr expr_info_ptr (EI_Extended (EEI_ActiveCase (transformer aci)) original_expr_info) expr_heap
		_	-> expr_heap

set_aci_free_vars_info_case unbound_variables case_info_ptr expr_heap
	= app_EEI_ActiveCase (\aci -> { aci & aci_free_vars=Yes unbound_variables }) case_info_ptr expr_heap

remove_aci_free_vars_info case_info_ptr expr_heap
	= app_EEI_ActiveCase (\aci->{aci & aci_free_vars = No }) case_info_ptr expr_heap

cleanup_attributes expr_info_ptr symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended _ expr_info -> writePtr expr_info_ptr expr_info symbol_heap
		_ -> symbol_heap

//	TRANSFORM

::	*TransformInfo =
	{	ti_fun_defs				:: !*{# FunDef}
	,	ti_instances 			:: !*{! InstanceInfo }
	,	ti_cons_args 			:: !*{! ConsClasses}
	,	ti_new_functions 		:: ![FunctionInfoPtr]
	,	ti_fun_heap				:: !*FunctionHeap
	,	ti_var_heap				:: !*VarHeap
	,	ti_symbol_heap			:: !*ExpressionHeap
	,	ti_type_heaps			:: !*TypeHeaps
	,	ti_type_def_infos		:: !*TypeDefInfos
	,	ti_next_fun_nr			:: !Index
	,	ti_cleanup_info			:: !CleanupInfo
	,	ti_recursion_introduced	:: !Optional RI
	,	ti_error_file			:: !*File
	,	ti_predef_symbols		:: !*PredefinedSymbols
	}

:: RI = { ri_fun_index :: !Int, ri_fun_ptr :: !FunctionInfoPtr}

::	ReadOnlyTI = 
	{	ro_imported_funs	:: !{# {# FunType} }
	,	ro_common_defs		:: !{# CommonDefs }
// the following four are used when possibly generating functions for cases...
	,	ro_root_case_mode		:: !RootCaseMode
	,	ro_tfi					:: !TransformFunctionInfo
	,	ro_main_dcl_module_n 	:: !Int
	,	ro_transform_fusion		:: !Bool			// fusion switch
	,	ro_StdStrictLists_module_n :: !Int
	,	ro_StdGeneric_module_n	:: !Int
	}

::	TransformFunctionInfo =
	{	tfi_root				:: !SymbIdent		// original function
	,	tfi_case				:: !SymbIdent		// original function or possibly generated case
	,	tfi_args				:: ![FreeVar]		// args of above
	,	tfi_vars				:: ![FreeVar]		// strict variables
	,	tfi_geni				:: !(!Int,!Int)
	,	tfi_orig				:: !SymbIdent		// original consumer
	}

::	RootCaseMode = NotRootCase | RootCase | RootCaseOfZombie

::	CopyState = {
		cs_var_heap				:: !.VarHeap,
		cs_symbol_heap			:: !.ExpressionHeap,
		cs_opt_type_heaps		:: !.Optional .TypeHeaps,
		cs_cleanup_info			:: ![ExprInfoPtr]
	}

::	CopyInfo = { ci_handle_aci_free_vars	:: !AciFreeVarsHandleMode }

:: AciFreeVarsHandleMode = LeaveAciFreeVars | RemoveAciFreeVars | SubstituteAciFreeVars

neverMatchingCase (Yes ident)
	# ident = ident -!-> ("neverMatchingCase",ident)
	= FailExpr ident
neverMatchingCase _ 
	# ident = {id_name = "neverMatchingCase", id_info = nilPtr}  -!-> "neverMatchingCase without ident\n"
	= FailExpr ident
/*
	= Case { case_expr = EE, case_guards = NoPattern, case_default = No, case_ident = ident, case_info_ptr = nilPtr, 
// RWS ...
						case_explicit = False,
				//		case_explicit = True,	// DvA better?
// ... RWS
						case_default_pos = NoPos }
*/

store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns var_heap
	= fold2St store_type_info_of_alg_pattern ct_cons_types patterns var_heap
	where
		store_type_info_of_alg_pattern var_types {ap_vars} var_heap
			= fold2St store_type_info_of_pattern_var var_types ap_vars var_heap

		store_type_info_of_pattern_var var_type {fv_info_ptr} var_heap
			= setExtendedVarInfo fv_info_ptr (EVI_VarType var_type) var_heap

class transform a :: !a !ReadOnlyTI !*TransformInfo -> (!a, !*TransformInfo)

instance transform Expression
where
	transform expr=:(App app=:{app_symb,app_args}) ro ti
		# (app_args, ti) = transform app_args ro ti
		= transformApplication { app & app_args = app_args } [] ro ti
	transform appl_expr=:(expr @ exprs) ro ti
		# (expr, ti) = transform expr ro ti
		  (exprs, ti) = transform exprs ro ti
		= case expr of
			App app
				-> transformApplication app exprs ro ti
			_
				-> (expr @ exprs, ti)
	transform (Let lad=:{let_strict_binds, let_lazy_binds, let_expr}) ro ti
		# ti = store_type_info_of_bindings_in_heap lad ti
		  (let_strict_binds, ti) = transform let_strict_binds ro ti
		  (let_lazy_binds, ti) = transform let_lazy_binds ro ti
		  (let_expr, ti) = transform let_expr ro ti
		  lad = { lad & let_lazy_binds = let_lazy_binds, let_strict_binds = let_strict_binds, let_expr = let_expr}
//		  ti = check_type_info lad ti
		= (Let lad, ti)
	  where
		store_type_info_of_bindings_in_heap {let_strict_binds, let_lazy_binds,let_info_ptr} ti
			# let_binds = let_strict_binds ++ let_lazy_binds
			  (EI_LetType var_types, ti_symbol_heap) = readExprInfo let_info_ptr ti.ti_symbol_heap
			  ti_var_heap = foldSt store_type_info_let_bind (zip2 var_types let_binds) ti.ti_var_heap
			= {ti & ti_symbol_heap = ti_symbol_heap, ti_var_heap = ti_var_heap}
		store_type_info_let_bind (var_type, {lb_dst={fv_info_ptr}}) var_heap
			= setExtendedVarInfo fv_info_ptr (EVI_VarType var_type) var_heap
/*
		check_type_info {let_strict_binds,let_lazy_binds,let_info_ptr} ti
			# (EI_LetType var_types, ti_symbol_heap) = readExprInfo let_info_ptr ti.ti_symbol_heap
			= { ti & ti_symbol_heap = ti_symbol_heap }
				//  ---> ("check_type_info_of_bindings_in_heap",let_strict_binds,let_lazy_binds,var_types)
*/
	transform (Case kees) ro ti
		# ti = store_type_info_of_patterns_in_heap kees ti
		= transformCase kees ro ti
	  where
		store_type_info_of_patterns_in_heap {case_guards,case_info_ptr} ti
			= case case_guards of
				AlgebraicPatterns _ patterns
					# (EI_CaseType {ct_cons_types},ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
					  ti_var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns ti.ti_var_heap
					-> { ti & ti_symbol_heap = ti_symbol_heap, ti_var_heap = ti_var_heap }
				BasicPatterns _ _
					-> ti // no variables occur
				OverloadedListPatterns _ _ patterns
					# (EI_CaseType {ct_cons_types},ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
					  ti_var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns ti.ti_var_heap
					-> { ti & ti_symbol_heap = ti_symbol_heap, ti_var_heap = ti_var_heap }
				NoPattern
					-> ti

	transform (Selection opt_type expr selectors) ro ti
		# (expr, ti) = transform expr ro ti
		= transformSelection opt_type selectors expr ro ti
	transform (Update expr1 selectors expr2) ro ti
		# (expr1,ti) = transform expr1 ro ti
		# (selectors,ti) = transform_expressions_in_selectors selectors ti
			with
				transform_expressions_in_selectors [selection=:RecordSelection _ _ : selections] ti
					# (selections,ti) = transform_expressions_in_selectors selections ti
					= ([selection:selections],ti)
				transform_expressions_in_selectors [ArraySelection ds ep expr : selections] ti
					# (expr,ti) = transform expr ro ti
					# (selections,ti) = transform_expressions_in_selectors selections ti
					= ([ArraySelection ds ep expr:selections],ti)
				transform_expressions_in_selectors [DictionarySelection bv dictionary_selections ep expr : selections] ti
					# (expr,ti) = transform expr ro ti
					# (dictionary_selections,ti) = transform_expressions_in_selectors dictionary_selections ti
					# (selections,ti) = transform_expressions_in_selectors selections ti
					= ([DictionarySelection bv dictionary_selections ep expr:selections],ti)
				transform_expressions_in_selectors [] ti
					= ([],ti)
		# (expr2,ti) = transform expr2 ro ti
		= (Update expr1 selectors expr2,ti)
	transform (RecordUpdate cons_symbol expr exprs) ro ti
		# (expr,ti) = transform expr ro ti
		# (exprs,ti) = transform_fields exprs ro ti
		=(RecordUpdate cons_symbol expr exprs,ti)
	where	
		transform_fields [] ro ti
			= ([],ti)
		transform_fields [bind=:{bind_src} : fields] ro ti
			# (bind_src,ti) = transform bind_src ro ti
			# (fields,ti) = transform_fields fields ro ti
			= ([{bind & bind_src=bind_src} : fields],ti)
	transform (TupleSelect a1 arg_nr expr) ro ti
		# (expr,ti) = transform expr ro ti
		= (TupleSelect a1 arg_nr expr,ti)
	transform (MatchExpr a1 expr) ro ti
		# (expr,ti) = transform expr ro ti
		= (MatchExpr a1 expr,ti)
	transform (DynamicExpr dynamic_expr) ro ti
		# (dynamic_expr, ti) = transform dynamic_expr ro ti
		= (DynamicExpr dynamic_expr, ti)
	transform (DictionariesFunction dictionaries expr expr_type) ro ti
		# (expr,ti) = transform expr ro ti
		= (DictionariesFunction dictionaries expr expr_type,ti)
	transform expr ro ti
		= (expr, ti)

instance transform DynamicExpr where
	transform dyn=:{dyn_expr} ro ti
		# (dyn_expr, ti) = transform dyn_expr ro ti
		= ({dyn & dyn_expr = dyn_expr}, ti)

transformCase this_case=:{case_expr,case_guards,case_default,case_ident,case_info_ptr} ro ti
	| SwitchCaseFusion (not ro.ro_transform_fusion) True
		= skip_over this_case ro ti
	| isNilPtr case_info_ptr			// encountered neverMatchingCase?!
		= skip_over this_case ro ti
	# (case_info, ti_symbol_heap) = readPtr case_info_ptr ti.ti_symbol_heap
	  ti = { ti & ti_symbol_heap=ti_symbol_heap }
	  (result_expr, ti)	= case case_info of
							EI_Extended (EEI_ActiveCase aci) _
								| is_variable case_expr
									-> skip_over this_case ro ti
								-> case ro.ro_root_case_mode of
									NotRootCase
										-> transform_active_non_root_case this_case aci ro ti
									_
										-> transform_active_root_case aci this_case ro ti
							_
								-> skip_over this_case ro ti
	  ti = { ti & ti_symbol_heap = remove_aci_free_vars_info case_info_ptr ti.ti_symbol_heap }
	# final_expr = removeNeverMatchingSubcases result_expr ro
	= (final_expr, ti) // ---> ("transformCase",result_expr,final_expr)
where
	is_variable (Var _) = True
	is_variable _ 		= False

skip_over this_case=:{case_expr,case_guards,case_default} ro ti
	# ro_lost_root = { ro & ro_root_case_mode = NotRootCase }
	  (new_case_expr, ti) = transform case_expr ro_lost_root ti
	  (new_case_guards, ti) = transform case_guards ro_lost_root ti
	  (new_case_default, ti) = transform case_default ro_lost_root ti
	= (Case { this_case & case_expr=new_case_expr, case_guards=new_case_guards, case_default=new_case_default }, ti)

free_vars_to_bound_vars free_vars
	= [Var {var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = nilPtr} \\ {fv_ident,fv_info_ptr} <- free_vars]

transform_active_root_case aci this_case=:{case_expr = Case case_in_case} ro ti
	= lift_case case_in_case this_case ro ti
where
	lift_case nested_case=:{case_guards,case_default} outer_case ro ti
		| isNilPtr nested_case.case_info_ptr	// neverMatchingCase ?!
			= skip_over outer_case ro ti
		# default_exists = case case_default of
							Yes _	-> True
							No		-> False
		  (case_guards, ti) = lift_patterns default_exists case_guards nested_case.case_info_ptr outer_case ro ti
		  (case_default, ti) = lift_default case_default outer_case ro ti
		  (EI_CaseType outer_case_type, ti_symbol_heap) = readExprInfo outer_case.case_info_ptr ti.ti_symbol_heap
		// the result type of the nested case becomes the result type of the outer case
		  ti_symbol_heap = overwrite_result_type nested_case.case_info_ptr outer_case_type.ct_result_type ti_symbol_heap
		// after this transformation the aci_free_vars information doesn't hold anymore
		  ti_symbol_heap = remove_aci_free_vars_info nested_case.case_info_ptr ti_symbol_heap
		  ti = { ti & ti_symbol_heap = ti_symbol_heap }
		= (Case {nested_case & case_guards = case_guards, case_default = case_default}, ti)
	  where
		overwrite_result_type case_info_ptr new_result_type ti_symbol_heap
			#! (EI_CaseType case_type, ti_symbol_heap)	= readExprInfo case_info_ptr ti_symbol_heap
			= writeExprInfo case_info_ptr (EI_CaseType { case_type & ct_result_type = new_result_type}) ti_symbol_heap

	lift_patterns default_exists (AlgebraicPatterns type case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ ap_expr \\ {ap_expr} <- case_guards ]
		  (EI_CaseType {ct_cons_types,ct_result_type},symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types case_guards ti.ti_var_heap
		  ti = {ti & ti_symbol_heap=symbol_heap,ti_var_heap=var_heap}
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (AlgebraicPatterns type [ { case_guard & ap_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)
	lift_patterns default_exists (BasicPatterns basic_type case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ bp_expr \\ {bp_expr} <- case_guards ]
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (BasicPatterns basic_type [ { case_guard & bp_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)
	lift_patterns default_exists (OverloadedListPatterns type decons_expr case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ ap_expr \\ {ap_expr} <- case_guards ]
		  (EI_CaseType {ct_cons_types},symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types case_guards ti.ti_var_heap
		  ti = {ti & ti_symbol_heap=symbol_heap,ti_var_heap=var_heap}
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (OverloadedListPatterns type decons_expr [ { case_guard & ap_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)

	lift_patterns_2 False [guard_expr] outer_case ro ti
		// if no default pattern exists, then the outer case expression does not have to be copied for the last pattern
		# (guard_expr, ti)						= possiblyFoldOuterCase True guard_expr outer_case ro ti
		= ([guard_expr], ti)
	lift_patterns_2 default_exists [guard_expr : guard_exprs] outer_case ro ti
		# (guard_expr, ti)						= possiblyFoldOuterCase False guard_expr outer_case ro ti
		  (guard_exprs, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= ([guard_expr : guard_exprs], ti)
	lift_patterns_2 _ [] _ _ ti
		= ([], ti)
		
	lift_default (Yes default_expr) outer_case ro ti
		# (default_expr, ti)					= possiblyFoldOuterCase True default_expr outer_case ro ti
		= (Yes default_expr, ti)
	lift_default No _ _ ti
		= (No, ti)

	possiblyFoldOuterCase final guard_expr outer_case ro ti
		| SwitchAutoFoldCaseInCase (isFoldExpression guard_expr ti.ti_fun_defs ti.ti_cons_args) False // otherwise GOTO next alternative
			| False -!-> ("possiblyFoldOuterCase","Case",bef < 0 || act < 0,ro.ro_tfi.tfi_args,aci.aci_params) = undef
			| bef < 0 || act < 0
				= possiblyFoldOuterCase` final guard_expr outer_case ro ti	//abort "possiblyFoldOuterCase: unexpected!\n"
			= transformApplication { app_symb = folder, app_args = folder_args, app_info_ptr = nilPtr } [] ro ti
		= possiblyFoldOuterCase` final guard_expr outer_case ro ti
	where
		isFoldExpression (App app)	ti_fun_defs ti_cons_args = isFoldSymbol app.app_symb.symb_kind
			where
				isFoldSymbol (SK_Function {glob_module,glob_object})
					| glob_module==ro.ro_StdStrictLists_module_n
						# type_arity = ro.ro_imported_funs.[glob_module].[glob_object].ft_type.st_arity
						| type_arity==0 || (type_arity==2 && case app.app_args of [_:_] -> True; _ -> False)
							= False
							= True
					| glob_module==ro.ro_main_dcl_module_n && glob_object>=size ti_cons_args &&
						(ti_fun_defs.[glob_object].fun_info.fi_properties bitand FI_IsUnboxedListOfRecordsConsOrNil<>0) &&
							(case ti_fun_defs.[glob_object].fun_type of
								Yes type ->(type.st_arity==0 || (type.st_arity==2 && case app.app_args of [_:_] -> True; _ -> False)))
							= False						
						= True
				isFoldSymbol (SK_LocalMacroFunction _)	= True
				isFoldSymbol (SK_GeneratedFunction _ _)	= True
				isFoldSymbol _							= False
		isFoldExpression (Var _)	ti_fun_defs ti_cons_args = True
//		isFoldExpression (Case _)	ti_fun_defs ti_cons_args = True
		isFoldExpression _			ti_fun_defs ti_cons_args = False

		ro_tfi = ro.ro_tfi

		(bef,act) = ro_tfi.tfi_geni
		new_f_a_before	= take bef ro_tfi.tfi_args
		new_f_a_after	= drop (bef+act) ro_tfi.tfi_args
		
		f_a_before	= new_f_a_before	//| new_f_a_before <> old_f_a_before	= abort "!!!"
		f_a_after	= new_f_a_after

		folder		= ro_tfi.tfi_orig
		folder_args = f_a_before` ++ [guard_expr:f_a_after`]
		old_f_a_before	= takeWhile (\e -> not (isMember e aci.aci_params)) ro_tfi.tfi_args
		old_f_a_help	= dropWhile (\e -> not (isMember e aci.aci_params)) ro_tfi.tfi_args
		old_f_a_after	= dropWhile (\e -> isMember e aci.aci_params) old_f_a_help
		f_a_before`	= free_vars_to_bound_vars f_a_before
		f_a_after`	= free_vars_to_bound_vars f_a_after

		isMember x [hd:tl] = hd.fv_info_ptr==x.fv_info_ptr || isMember x tl
		isMember x []	= False

	possiblyFoldOuterCase` final guard_expr outer_case ro ti
		| final
			# new_case = {outer_case & case_expr = guard_expr}
			= transformCase new_case ro ti // ---> ("possiblyFoldOuterCase`",Case new_case)
		# cs = {cs_var_heap = ti.ti_var_heap, cs_symbol_heap = ti.ti_symbol_heap, cs_opt_type_heaps = No, cs_cleanup_info=ti.ti_cleanup_info}
		  (outer_guards, cs=:{cs_cleanup_info})	= copy outer_case.case_guards {ci_handle_aci_free_vars = LeaveAciFreeVars} cs
		  (expr_info, ti_symbol_heap)			= readPtr outer_case.case_info_ptr cs.cs_symbol_heap
		  (new_info_ptr, ti_symbol_heap)		= newPtr expr_info ti_symbol_heap
		  new_cleanup_info 						= case expr_info of
		  		EI_Extended _ _
		  			-> [new_info_ptr:cs_cleanup_info]
		  		_ 	-> cs_cleanup_info
		  ti = { ti & ti_var_heap = cs.cs_var_heap, ti_symbol_heap = ti_symbol_heap, ti_cleanup_info=new_cleanup_info }
		  new_case								= { outer_case & case_expr = guard_expr, case_guards=outer_guards, case_info_ptr=new_info_ptr }
		= transformCase new_case ro ti // ---> ("possiblyFoldOuterCase`",Case new_case)

transform_active_root_case aci this_case=:{case_expr = case_expr=:(App app=:{app_symb,app_args}),case_guards,case_default,case_explicit,case_ident} ro ti
	= case app_symb.symb_kind of
		SK_Constructor cons_index
			// currently only active cases are matched at runtime (multimatch problem)
			# aci_linearity_of_patterns = aci.aci_linearity_of_patterns
			  (may_be_match_expr, ti) = match_and_instantiate aci_linearity_of_patterns cons_index app_args case_guards case_default ro ti
			-> expr_or_never_matching_case may_be_match_expr case_ident ti
		SK_Function {glob_module,glob_object}
			| glob_module==ro.ro_StdStrictLists_module_n &&
				(let type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
				 in (type.st_arity==0 || (type.st_arity==2 && case app_args of [_:_] -> True; _ -> False)))
				# type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
				-> trans_case_of_overloaded_nil_or_cons type ti
			| glob_module==ro.ro_main_dcl_module_n && glob_object>=size ti.ti_cons_args &&
				(ti.ti_fun_defs.[glob_object].fun_info.fi_properties bitand FI_IsUnboxedListOfRecordsConsOrNil)<>0 &&
				(case ti.ti_fun_defs.[glob_object].fun_type of
					Yes type ->(type.st_arity==0 || (type.st_arity==2 && case app_args of [_:_] -> True; _ -> False)))
				# (Yes type,ti) = ti!ti_fun_defs.[glob_object].fun_type
				-> trans_case_of_overloaded_nil_or_cons type ti
		// otherwise it's a function application
		_
			# {aci_params,aci_opt_unfolder} = aci
			-> case aci_opt_unfolder of
				No
					-> skip_over this_case ro ti									-!-> ("transform_active_root_case","No opt unfolder")
				Yes unfolder
					| not (equal app_symb.symb_kind unfolder.symb_kind)
						// in this case a third function could be fused in
						-> possiblyFoldOuterCase this_case ro ti					-!-> ("transform_active_root_case","Diff opt unfolder",unfolder,app_symb)
					# variables = [ Var {var_ident=fv_ident, var_info_ptr=fv_info_ptr, var_expr_ptr=nilPtr}
									\\ {fv_ident, fv_info_ptr} <- ro.ro_tfi.tfi_args ]
					  (app_symb, ti)
						= case ro.ro_root_case_mode -!-> ("transform_active_root_case","Yes opt unfolder",unfolder) of
							RootCaseOfZombie
								# (recursion_introduced,ti) = ti!ti_recursion_introduced
								  (ro_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _}) = ro.ro_tfi.tfi_case
								-> case recursion_introduced of
									No
										# (ti_next_fun_nr, ti) = ti!ti_next_fun_nr
										  ri = {ri_fun_index=ti_next_fun_nr, ri_fun_ptr=fun_info_ptr}
										-> ({ro_fun & symb_kind=SK_GeneratedFunction fun_info_ptr ti_next_fun_nr},
										    {ti & ti_next_fun_nr = inc ti_next_fun_nr, ti_recursion_introduced = Yes ri})
											-!-> ("Recursion","RootCaseOfZombie",ti_next_fun_nr,recursion_introduced)
									Yes {ri_fun_index,ri_fun_ptr}
										| ri_fun_ptr==fun_info_ptr
											-> ({ro_fun & symb_kind=SK_GeneratedFunction fun_info_ptr ri_fun_index},ti)
							RootCase
								-> (ro.ro_tfi.tfi_root,{ti & ti_recursion_introduced = No})
									-!-> ("Recursion","RootCase",ro.ro_tfi.tfi_root)
					  app_args1 = replace_arg [ fv_info_ptr \\ {fv_info_ptr}<-aci_params ] app_args variables
					  (app_args2, ti) = transform app_args1 { ro & ro_root_case_mode = NotRootCase } ti
					-> (App {app_symb=app_symb, app_args=app_args2, app_info_ptr=nilPtr}, ti)
where
	possiblyFoldOuterCase this_case ro ti
		| SwitchAutoFoldAppInCase True False
			| False -!-> ("possiblyFoldOuterCase","App",bef < 0 || act < 0,ro.ro_tfi.tfi_args,aci.aci_params) = undef
			| bef < 0 || act < 0
				= skip_over this_case ro ti	//abort "possiblyFoldOuterCase: unexpected!\n"
			= transformApplication { app_symb = folder, app_args = folder_args, app_info_ptr = nilPtr } [] ro ti
		= skip_over this_case ro ti
	where
		ro_tfi = ro.ro_tfi
		
		(bef,act)	= ro_tfi.tfi_geni
		new_f_a_before	= take bef ro_tfi.tfi_args
		new_f_a_after	= drop (bef+act) ro_tfi.tfi_args
		
		f_a_before	= new_f_a_before
		f_a_after	= new_f_a_after

		folder		= ro_tfi.tfi_orig
		folder_args = f_a_before` ++ [case_expr:f_a_after`]
		old_f_a_before	= takeWhile (\e -> not (isMember e aci.aci_params)) ro_tfi.tfi_args
		old_f_a_help	= dropWhile (\e -> not (isMember e aci.aci_params)) ro_tfi.tfi_args
		old_f_a_after	= dropWhile (\e -> isMember e aci.aci_params) old_f_a_help
		f_a_before`	= free_vars_to_bound_vars f_a_before
		f_a_after`	= free_vars_to_bound_vars f_a_after

		isMember x [hd:tl] = hd.fv_info_ptr==x.fv_info_ptr || isMember x tl
		isMember x []	= False

	equal (SK_Function glob_index1) (SK_Function glob_index2)
		= glob_index1==glob_index2
	equal (SK_LocalMacroFunction glob_index1) (SK_LocalMacroFunction glob_index2)
		= glob_index1==glob_index2
	equal (SK_GeneratedFunction _ index1) (SK_GeneratedFunction _ index2)
		= index1==index2
	equal _ _
		= False

	replace_arg [] _ f
		= f
	replace_arg producer_vars=:[fv_info_ptr:_] act_pars form_pars=:[h_form_pars=:(Var {var_info_ptr}):t_form_pars]
		| fv_info_ptr<>var_info_ptr
			= [h_form_pars:replace_arg producer_vars act_pars t_form_pars]
		= replacement producer_vars act_pars form_pars
	  where
		replacement producer_vars [] form_pars
			= form_pars
		replacement producer_vars _ []
			= []
		replacement producer_vars [h_act_pars:t_act_pars] [form_par=:(Var {var_info_ptr}):form_pars]
			| isMember var_info_ptr producer_vars
				= [h_act_pars:replacement producer_vars t_act_pars form_pars]
			= replacement producer_vars t_act_pars form_pars

	match_and_instantiate linearities cons_index app_args (AlgebraicPatterns _ algebraicPatterns) case_default ro ti
		= match_and_instantiate_algebraic_type linearities cons_index app_args algebraicPatterns case_default ro ti
		where
			match_and_instantiate_algebraic_type [linearity:linearities] cons_index app_args
												 [{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] case_default ro ti
				| cons_index.glob_module == glob_module && cons_index.glob_object == ds_index
					# {cons_type} = ro.ro_common_defs.[glob_module].com_cons_defs.[ds_index]
					= instantiate linearity app_args ap_vars ap_expr cons_type.st_args_strictness cons_type.st_args ti
				= match_and_instantiate_algebraic_type linearities cons_index app_args guards case_default ro ti
			match_and_instantiate_algebraic_type _ cons_index app_args [] case_default ro ti
				= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	match_and_instantiate linearities cons_index app_args (OverloadedListPatterns (OverloadedList _ _ _ _) _ algebraicPatterns) case_default ro ti
		= match_and_instantiate_overloaded_list linearities cons_index app_args algebraicPatterns case_default ro ti
		where
			match_and_instantiate_overloaded_list [linearity:linearities] cons_index=:{glob_module=cons_glob_module,glob_object=cons_ds_index} app_args 
									[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
									case_default ro ti
				| equal_list_contructor glob_module ds_index cons_glob_module cons_ds_index
					# {cons_type} = ro.ro_common_defs.[cons_glob_module].com_cons_defs.[cons_ds_index]
					= instantiate linearity app_args ap_vars ap_expr cons_type.st_args_strictness cons_type.st_args ti
					= match_and_instantiate_overloaded_list linearities cons_index app_args guards case_default ro ti
					where
						equal_list_contructor glob_module ds_index cons_glob_module cons_ds_index
							| glob_module==cPredefinedModuleIndex && cons_glob_module==cPredefinedModuleIndex
								# index=ds_index+FirstConstructorPredefinedSymbolIndex
								# cons_index=cons_ds_index+FirstConstructorPredefinedSymbolIndex
								| index==PD_OverloadedConsSymbol
									= cons_index==PD_ConsSymbol || cons_index==PD_StrictConsSymbol || cons_index==PD_TailStrictConsSymbol || cons_index==PD_StrictTailStrictConsSymbol
								| index==PD_OverloadedNilSymbol
									= cons_index==PD_NilSymbol || cons_index==PD_StrictNilSymbol || cons_index==PD_TailStrictNilSymbol || cons_index==PD_StrictTailStrictNilSymbol
									= abort "equal_list_contructor"
			match_and_instantiate_overloaded_list _ cons_index app_args [] case_default ro ti
				= transform case_default { ro & ro_root_case_mode = NotRootCase } ti

	trans_case_of_overloaded_nil_or_cons type ti
		| type.st_arity==0
			# (may_be_match_expr, ti) = match_and_instantiate_overloaded_nil case_guards case_default ro ti
			= expr_or_never_matching_case may_be_match_expr case_ident ti
			# aci_linearity_of_patterns = aci.aci_linearity_of_patterns
			  (may_be_match_expr, ti) = match_and_instantiate_overloaded_cons type aci_linearity_of_patterns app_args case_guards case_default ro ti
			= expr_or_never_matching_case may_be_match_expr case_ident ti
	where
		match_and_instantiate_overloaded_nil (OverloadedListPatterns _ _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_nil algebraicPatterns case_default ro ti
		match_and_instantiate_overloaded_nil (AlgebraicPatterns _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_nil algebraicPatterns case_default ro ti
	
		match_and_instantiate_nil [{ap_symbol={glob_module,glob_object={ds_index}},ap_expr} : guards] case_default ro ti
			| glob_module==cPredefinedModuleIndex
				# index=ds_index+FirstConstructorPredefinedSymbolIndex
				| index==PD_NilSymbol || index==PD_StrictNilSymbol || index==PD_TailStrictNilSymbol || index==PD_StrictTailStrictNilSymbol ||
				  index==PD_OverloadedNilSymbol || index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol
					= instantiate [] [] [] ap_expr NotStrict [] ti
					= match_and_instantiate_nil guards case_default ro ti
		match_and_instantiate_nil [] case_default ro ti
			= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	
		match_and_instantiate_overloaded_cons cons_function_type linearities app_args (AlgebraicPatterns _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_overloaded_cons_boxed_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_boxed_match [linearity:linearities] app_args
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_ConsSymbol || index==PD_StrictConsSymbol || index==PD_TailStrictConsSymbol || index==PD_StrictTailStrictConsSymbol
							# {cons_type} = ro.ro_common_defs.[glob_module].com_cons_defs.[ds_index]
							= instantiate linearity app_args ap_vars ap_expr cons_type.st_args_strictness cons_type.st_args ti
		//				| index==PD_NilSymbol || index==PD_StrictNilSymbol || index==PD_TailStrictNilSymbol || index==PD_StrictTailStrictNilSymbol
							= match_and_instantiate_overloaded_cons_boxed_match linearities app_args guards case_default ro ti
		//					= abort "match_and_instantiate_overloaded_cons_boxed_match"
				match_and_instantiate_overloaded_cons_boxed_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
		match_and_instantiate_overloaded_cons cons_function_type linearities app_args (OverloadedListPatterns _ _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_overloaded_match [linearity:linearities] app_args 
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_UnboxedConsSymbol || index==PD_UnboxedTailStrictConsSymbol || index==PD_OverloadedConsSymbol
							= instantiate linearity app_args ap_vars ap_expr cons_function_type.st_args_strictness cons_function_type.st_args ti
		//				| index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol || index==PD_OverloadedNilSymbol
							= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
		//					= abort "match_and_instantiate_overloaded_cons_overloaded_match"
				match_and_instantiate_overloaded_cons_overloaded_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	
		/*
		match_and_instantiate_overloaded_cons linearities app_args (OverloadedListPatterns _ (App {app_args=[],app_symb={symb_kind=SK_Function {glob_module=decons_module,glob_object=deconsindex}}}) algebraicPatterns) case_default ro ti
			= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_overloaded_match [linearity:linearities] app_args 
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_UnboxedConsSymbol || index==PD_UnboxedTailStrictConsSymbol || index==PD_OverloadedConsSymbol
							# (argument_types,strictness) = case ro.ro_imported_funs.[decons_module].[deconsindex].ft_type.st_result.at_type of
													TA _ args=:[arg1,arg2] -> (args,NotStrict)
													TAS _ args=:[arg1,arg2] strictness -> (args,strictness)
							= instantiate linearity app_args ap_vars ap_expr strictness argument_types ti
						| index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol || index==PD_OverloadedNilSymbol
							= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
							= abort "match_and_instantiate_overloaded_cons_overloaded_match"
				match_and_instantiate_overloaded_cons_overloaded_match [linearity:linearities] app_args [guard : guards] case_default ro ti
					= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
				match_and_instantiate_overloaded_cons_overloaded_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
		*/

	instantiate linearity app_args ap_vars ap_expr cons_type_args_strictness cons_type_args ti
		# zipped_ap_vars_and_args = zip2 ap_vars app_args
		  (body_strictness,ti_fun_defs,ti_fun_heap) = body_strict ap_expr ap_vars ro ti.ti_fun_defs ti.ti_fun_heap
		  ti = {ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap}
		  unfoldables = [ (arg_is_strict i body_strictness || ((not (arg_is_strict i cons_type_args_strictness))) && linear) || in_normal_form app_arg
		  				 \\ linear <- linearity & app_arg <- app_args & i <- [0..]]
		  unfoldable_args = filterWith unfoldables zipped_ap_vars_and_args
		  not_unfoldable = map not unfoldables
		  ti_var_heap = foldSt (\({fv_info_ptr}, arg) -> writeVarInfo fv_info_ptr (VI_Expression arg)) unfoldable_args ti.ti_var_heap
		  (new_expr, ti_symbol_heap) = possibly_add_let zipped_ap_vars_and_args ap_expr not_unfoldable cons_type_args ro ti.ti_symbol_heap cons_type_args_strictness
		  copy_state = { cs_var_heap = ti_var_heap, cs_symbol_heap = ti_symbol_heap, cs_opt_type_heaps = No,cs_cleanup_info=ti.ti_cleanup_info }
		  (unfolded_expr, copy_state) = copy new_expr {ci_handle_aci_free_vars = LeaveAciFreeVars} copy_state
		  ti = { ti & ti_var_heap = copy_state.cs_var_heap, ti_symbol_heap = copy_state.cs_symbol_heap,ti_cleanup_info=copy_state.cs_cleanup_info }
		  (final_expr, ti) = transform unfolded_expr { ro & ro_root_case_mode = NotRootCase } ti
//		| False ---> ("instantiate",app_args,ap_vars,ap_expr,final_expr,unfoldables) = undef
		= (Yes final_expr, ti)
	where
		body_strict (Var v) ap_vars ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			# is = [i \\ i <- [0..] & var <- ap_vars | v.var_info_ptr == var.fv_info_ptr]
			= case is of
				[]		-> (lazy_args,fun_defs,fun_heap)
				[i:_]	-> (add_strictness i lazy_args,fun_defs,fun_heap)
		body_strict (App app) ap_vars ro fun_defs fun_heap
			# (is,fun_defs,fun_heap) = app_indices app ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			= (seq (map add_strictness is) lazy_args, fun_defs,fun_heap)
		body_strict _ _ ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			= (lazy_args,fun_defs,fun_heap)
		
		app_indices {app_symb,app_args} ro fun_defs fun_heap
			# ({st_args_strictness,st_arity},fun_defs,fun_heap)	= get_producer_type app_symb ro fun_defs fun_heap
			| length app_args == st_arity
				= find_indices st_args_strictness 0 app_args ro fun_defs fun_heap
				= ([],fun_defs,fun_heap)
		where
			find_indices st_args_strictness i [] ro fun_defs fun_heap
				= ([],fun_defs,fun_heap)
			find_indices st_args_strictness i [e:es] ro fun_defs fun_heap
				# (is,fun_defs,fun_heap)	= find_index st_args_strictness i e ro fun_defs fun_heap
				# (iss,fun_defs,fun_heap)	= find_indices st_args_strictness (i+1) es ro fun_defs fun_heap
				= (is++iss,fun_defs,fun_heap)

			find_index st_args_strictness i e ro fun_defs fun_heap
				| arg_is_strict i st_args_strictness
					= case e of
						Var v	-> ([i \\ i <- [0..] & var <- ap_vars | v.var_info_ptr == var.fv_info_ptr],fun_defs,fun_heap)
						App	a	-> app_indices a ro fun_defs fun_heap
						_		-> ([],fun_defs,fun_heap)
				= ([],fun_defs,fun_heap)
		
	expr_or_never_matching_case (Yes match_expr) case_ident ti
		= (match_expr, ti)
	expr_or_never_matching_case No case_ident ti
		= (neverMatchingCase never_ident, ti) <-!- ("transform_active_root_case:App:neverMatchingCase",never_ident)
		where
			never_ident = case ro.ro_root_case_mode of
							NotRootCase -> case_ident
							_ -> Yes ro.ro_tfi.tfi_case.symb_ident

transform_active_root_case aci this_case=:{case_expr = (BasicExpr basic_value),case_guards,case_default} ro ti
	// currently only active cases are matched at runtime (multimatch problem)
	# basicPatterns = getBasicPatterns case_guards
	  may_be_match_pattern = dropWhile (\{bp_value} -> bp_value<>basic_value) basicPatterns
	| isEmpty may_be_match_pattern
		= case case_default of
			Yes default_expr-> transform default_expr { ro & ro_root_case_mode = NotRootCase } ti
			No				-> (neverMatchingCase never_ident, ti) <-!- ("transform_active_root_case:BasicExpr:neverMatchingCase",never_ident)
					with
						never_ident = case ro.ro_root_case_mode of
							NotRootCase -> this_case.case_ident
							_ -> Yes ro.ro_tfi.tfi_case.symb_ident
	= transform (hd may_be_match_pattern).bp_expr { ro & ro_root_case_mode = NotRootCase } ti
where
	getBasicPatterns (BasicPatterns _ basicPatterns)
		= basicPatterns

transform_active_root_case aci this_case=:{case_expr = (Let lad)} ro ti
	# ro_not_root = { ro & ro_root_case_mode = NotRootCase }
	  (new_let_strict_binds, ti) = transform lad.let_strict_binds ro_not_root ti
	  (new_let_lazy_binds, ti) = transform lad.let_lazy_binds ro_not_root ti
	  (new_let_expr, ti) = transform (Case { this_case & case_expr = lad.let_expr }) ro ti
	= (Let { lad & let_expr = new_let_expr, let_strict_binds = new_let_strict_binds, let_lazy_binds = new_let_lazy_binds }, ti)

transform_active_root_case aci this_case ro ti
	= skip_over this_case ro ti
	
in_normal_form (Var _)			= True
in_normal_form (BasicExpr _)	= True
in_normal_form _				= False

filterWith [True:t2] [h1:t1]
	= [h1:filterWith t2 t1]
filterWith [False:t2] [h1:t1]
	= filterWith t2 t1
filterWith _ _
	= []

possibly_add_let [] ap_expr _ _ _ ti_symbol_heap cons_type_args_strictness
	= (ap_expr, ti_symbol_heap)
possibly_add_let zipped_ap_vars_and_args ap_expr not_unfoldable cons_type_args ro ti_symbol_heap cons_type_args_strictness
	# let_type = filterWith not_unfoldable cons_type_args
	  (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti_symbol_heap
	= SwitchStrictPossiblyAddLet
		(let
			strict_binds	= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									& i <- [0..]
									| n && arg_is_strict i cons_type_args_strictness
									]
			lazy_binds		= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									& i <- [0..]
									| n && not (arg_is_strict i cons_type_args_strictness)
									]
		 in
		 	case (strict_binds,lazy_binds) of
		 		([],[])
		 			->	ap_expr
		 		_
		 			->	Let
						{	let_strict_binds	= strict_binds
						,	let_lazy_binds		= lazy_binds
						,	let_expr			= ap_expr
						,	let_info_ptr		= new_info_ptr
						,	let_expr_position	= NoPos
						}
	  , ti_symbol_heap
	  ) 
	  (let
			lazy_binds		= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									| n
									]
	   in
			case lazy_binds of
				[]
					-> ap_expr
				_
					-> Let
		  			 	{	let_strict_binds	= []
						,	let_lazy_binds		= lazy_binds
						,	let_expr			= ap_expr
						,	let_info_ptr		= new_info_ptr
						,	let_expr_position	= NoPos
						}
	  , ti_symbol_heap
	  )

free_variables_of_expression expr ti
	# ti_var_heap = clearVariables expr ti.ti_var_heap
	  fvi = {fvi_var_heap = ti_var_heap, fvi_expr_heap = ti.ti_symbol_heap, fvi_variables = [], fvi_expr_ptrs = ti.ti_cleanup_info}
	  {fvi_var_heap, fvi_expr_heap, fvi_variables, fvi_expr_ptrs} = freeVariables expr fvi
	  ti = {ti & ti_var_heap = fvi_var_heap, ti_symbol_heap = fvi_expr_heap, ti_cleanup_info = fvi_expr_ptrs}
	 = (fvi_variables,ti)

transform_active_non_root_case :: !Case !ActiveCaseInfo !ReadOnlyTI !*TransformInfo -> *(!Expression, !*TransformInfo)
transform_active_non_root_case kees=:{case_info_ptr,case_expr = App {app_symb}} aci=:{aci_free_vars} ro ti=:{ti_recursion_introduced=old_ti_recursion_introduced}
	| not aci.aci_safe
		= skip_over kees ro ti
	| is_safe_producer app_symb.symb_kind ro ti.ti_fun_heap ti.ti_cons_args
		// determine free variables	
		# (free_vars,ti) = free_variables_of_expression (Case {kees & case_expr=EE}) ti	
		// search function definition and consumer arguments
		  (outer_fun_def, outer_cons_args, ti_cons_args, ti_fun_defs, ti_fun_heap)
				= get_fun_def_and_cons_args ro.ro_tfi.tfi_root.symb_kind ti.ti_cons_args ti.ti_fun_defs ti.ti_fun_heap
		  outer_arguments
		  		= case outer_fun_def.fun_body of
								TransformedBody {tb_args} 	-> tb_args
								Expanding args				-> args
		  outer_info_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-outer_arguments]
		  free_var_info_ptrs = [ var_info_ptr \\ {var_info_ptr}<-free_vars ]
		  used_mask = [isMember fv_info_ptr free_var_info_ptrs \\ {fv_info_ptr}<-outer_arguments]
		  arguments_from_outer_fun = [ outer_argument \\ outer_argument<-outer_arguments & used<-used_mask | used ]
		  lifted_arguments
		  		= [ { fv_def_level = undeff, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_count = undeff}
								\\ {var_ident, var_info_ptr} <- free_vars | not (isMember var_info_ptr outer_info_ptrs)]
		  all_args = lifted_arguments++arguments_from_outer_fun
		| SwitchArityChecks (1+length all_args > 32) False
			# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
			| ro.ro_transform_fusion
				# ti = { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Case Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
				= skip_over kees ro ti
			= skip_over kees ro ti
		# (fun_info_ptr, ti_fun_heap) = newPtr FI_Empty ti_fun_heap
		  fun_ident = { id_name = ro.ro_tfi.tfi_root.symb_ident.id_name+++"_case", id_info = nilPtr }
		  fun_ident = { symb_ident = fun_ident, symb_kind=SK_GeneratedFunction fun_info_ptr undeff }
		# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap }
//				---> ("lifted arguments",[fv_ident\\{fv_ident}<-lifted_arguments],outer_arguments,
//					'\n',kees.case_expr,kees.case_guards,kees.case_default)
	  	# fun_index = ti.ti_next_fun_nr
	  	# ti = { ti & ti_next_fun_nr = fun_index + 1 }
		// JvG: why are dictionaries not the first arguments ?
		# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie, ro_tfi.tfi_case = fun_ident, ro_tfi.tfi_args = all_args }
	  	= generate_case_function_with_pattern_argument fun_index case_info_ptr (Case kees) outer_fun_def outer_cons_args used_mask fun_ident all_args ti

transform_active_non_root_case kees=:{case_info_ptr} aci=:{aci_free_vars} ro ti=:{ti_recursion_introduced=old_ti_recursion_introduced}
	| not aci.aci_safe
		= skip_over kees ro ti
	// determine free variables
	# (free_vars,ti) = free_variables_of_expression (Case kees) ti	
	// search function definition and consumer arguments
	  (outer_fun_def, outer_cons_args, ti_cons_args, ti_fun_defs, ti_fun_heap)
			= get_fun_def_and_cons_args ro.ro_tfi.tfi_root.symb_kind ti.ti_cons_args ti.ti_fun_defs ti.ti_fun_heap
	  outer_arguments
	  		= case outer_fun_def.fun_body of
							TransformedBody {tb_args} 	-> tb_args
							Expanding args				-> args
	  outer_info_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-outer_arguments]
	  free_var_info_ptrs = [ var_info_ptr \\ {var_info_ptr}<-free_vars ]
	  used_mask = [isMember fv_info_ptr free_var_info_ptrs \\ {fv_info_ptr}<-outer_arguments]
	  arguments_from_outer_fun = [ outer_argument \\ outer_argument<-outer_arguments & used<-used_mask | used ]
	  lifted_arguments
	  		= [ { fv_def_level = undeff, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_count = undeff}
							\\ {var_ident, var_info_ptr} <- free_vars | not (isMember var_info_ptr outer_info_ptrs)]
	  all_args = lifted_arguments++arguments_from_outer_fun
	| SwitchArityChecks (length all_args > 32) False
		# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
		| ro.ro_transform_fusion
			#  ti	= { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Case Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
			= skip_over kees ro ti
		= skip_over kees ro ti
	# (fun_info_ptr, ti_fun_heap) = newPtr FI_Empty ti_fun_heap
	  fun_ident = { id_name = ro.ro_tfi.tfi_root.symb_ident.id_name+++"_case", id_info = nilPtr }
	  fun_ident = { symb_ident = fun_ident, symb_kind=SK_GeneratedFunction fun_info_ptr undeff }
					<-!- ("<<<transformCaseFunction",fun_ident)
	| SwitchAlwaysIntroduceCaseFunction True False
		# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap }
	  	# fun_index = ti.ti_next_fun_nr
	  	# ti = { ti & ti_next_fun_nr = fun_index + 1 }
		# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie , ro_tfi.tfi_case = fun_ident, ro_tfi.tfi_args = all_args }
	  	= generate_case_function fun_index case_info_ptr (Case kees) outer_fun_def outer_cons_args used_mask new_ro ti
	# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie, ro_tfi.tfi_case = fun_ident, ro_tfi.tfi_args = all_args, ro_tfi.tfi_geni = (-1,-1) }
	  ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
	  (new_expr, ti)
	  		= transformCase kees new_ro ti
	  (ti_recursion_introduced, ti) = ti!ti_recursion_introduced
	  			<-!- ("transformCaseFunction>>>",fun_ident)
	  ti = { ti & ti_recursion_introduced = old_ti_recursion_introduced }
	= case ti_recursion_introduced of
		Yes {ri_fun_index}
			-> generate_case_function ri_fun_index case_info_ptr new_expr outer_fun_def outer_cons_args used_mask new_ro ti
		No	-> (new_expr, ti)

generate_case_function :: !Int !ExprInfoPtr !Expression FunDef .ConsClasses [.Bool] !.ReadOnlyTI !*TransformInfo -> (!Expression,!*TransformInfo)
generate_case_function fun_index case_info_ptr new_expr outer_fun_def outer_cons_args used_mask
					{ro_tfi={tfi_case=tfi_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _},tfi_args}} ti
	# fun_arity								= length tfi_args
	# ti = arity_warning "generate_case_function" tfi_fun.symb_ident fun_index fun_arity ti
	  (Yes {st_args,st_attr_env})			= outer_fun_def.fun_type
	  types_from_outer_fun					= [ st_arg \\ st_arg <- st_args & used <- used_mask | used ]
	  nr_of_lifted_vars						= fun_arity-(length types_from_outer_fun)
	  (lifted_types, ti_var_heap)			= get_types_of_local_vars (take nr_of_lifted_vars tfi_args) ti.ti_var_heap
	  (EI_CaseType {ct_result_type}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
	  (form_vars, ti_var_heap)				= mapSt bind_to_fresh_expr_var tfi_args ti_var_heap

	  arg_types								= lifted_types++types_from_outer_fun

	# ti = {ti & ti_var_heap = ti_var_heap, ti_symbol_heap = ti_symbol_heap}
	# (fun_type,ti)							= determine_case_function_type fun_arity ct_result_type arg_types st_attr_env ti

	  // unfold...
	  cs =		{ cs_var_heap				= ti.ti_var_heap
	  			, cs_symbol_heap			= ti.ti_symbol_heap
				, cs_opt_type_heaps			= Yes ti.ti_type_heaps
	  			, cs_cleanup_info			= ti.ti_cleanup_info
	  			}
	  (copied_expr, cs)
			= copy new_expr {ci_handle_aci_free_vars = SubstituteAciFreeVars} cs
	  {cs_var_heap=ti_var_heap, cs_symbol_heap=ti_symbol_heap, cs_cleanup_info=ti_cleanup_info, cs_opt_type_heaps = Yes ti_type_heaps}
	  		= cs
	  // generated function...
	  fun_def =	{ fun_ident					= tfi_fun.symb_ident
				, fun_arity					= fun_arity
				, fun_priority				= NoPrio
				, fun_body					= TransformedBody { tb_args = form_vars, tb_rhs = copied_expr}
				, fun_type					= Yes fun_type
				, fun_pos					= NoPos
				, fun_kind					= FK_Function cNameNotLocationDependent
				, fun_lifted				= undeff
				, fun_info = 	{	fi_calls		= []
								,	fi_group_index	= outer_fun_def.fun_info.fi_group_index
								,	fi_def_level	= NotALevel
								,	fi_free_vars	= []
								,	fi_local_vars	= []
								,	fi_dynamics		= []
								,	fi_properties	= outer_fun_def.fun_info.fi_properties
								}	
				}
	# cc_args_from_outer_fun		= [ cons_arg \\ cons_arg <- outer_cons_args.cc_args & used <- used_mask | used ]
	  cc_linear_bits_from_outer_fun	= [ cons_arg \\ cons_arg <- outer_cons_args.cc_linear_bits & used <- used_mask | used ]
	  new_cons_args =
				{ cc_size			= fun_arity
				, cc_args			= repeatn nr_of_lifted_vars CPassive ++ cc_args_from_outer_fun
				, cc_linear_bits	= repeatn nr_of_lifted_vars    False ++ cc_linear_bits_from_outer_fun
				, cc_producer		= False
				}
	  gf =		{ gf_fun_def		= fun_def
				, gf_instance_info	= II_Empty
				, gf_cons_args		= new_cons_args
				, gf_fun_index		= fun_index
				}
	  ti_fun_heap = writePtr fun_info_ptr (FI_Function gf) ti.ti_fun_heap
	  ti = { ti & ti_new_functions	= [fun_info_ptr:ti.ti_new_functions]
	  			, ti_var_heap		= ti_var_heap
	  			, ti_fun_heap		= ti_fun_heap
	  			, ti_symbol_heap	= ti_symbol_heap
	  			, ti_type_heaps		= ti_type_heaps
	  			, ti_cleanup_info	= ti_cleanup_info 
	  			}
	  app_symb = {tfi_fun & symb_kind = SK_GeneratedFunction fun_info_ptr fun_index}
	  app_args = free_vars_to_bound_vars tfi_args
	= ( App {app_symb = app_symb, app_args = app_args, app_info_ptr = nilPtr}, ti)


generate_case_function_with_pattern_argument :: !Int !ExprInfoPtr !Expression FunDef .ConsClasses [.Bool] !SymbIdent ![FreeVar] !*TransformInfo
	-> (!Expression,!*TransformInfo)
generate_case_function_with_pattern_argument fun_index case_info_ptr 
					case_expr=:(Case kees=:{case_expr=old_case_expr}) outer_fun_def outer_cons_args used_mask
					ro_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _} ro_fun_args ti
	# fun_arity								= length ro_fun_args
	# ti = arity_warning "generate_case_function" ro_fun.symb_ident fun_index fun_arity ti
	  (Yes {st_args,st_attr_env})			= outer_fun_def.fun_type
	  types_from_outer_fun					= [ st_arg \\ st_arg <- st_args & used <- used_mask | used ]
	  nr_of_lifted_vars						= fun_arity-(length types_from_outer_fun)
	  (lifted_types, ti_var_heap)			= get_types_of_local_vars (take nr_of_lifted_vars ro_fun_args) ti.ti_var_heap
	  (EI_CaseType {ct_result_type,ct_pattern_type}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
	  (form_vars, ti_var_heap)				= mapSt bind_to_fresh_expr_var ro_fun_args ti_var_heap

	  arg_types								= lifted_types++types_from_outer_fun

	  ti = {ti & ti_var_heap = ti_var_heap, ti_symbol_heap = ti_symbol_heap}
	  (fun_type,ti)							= determine_case_function_type fun_arity ct_result_type [ct_pattern_type:arg_types] st_attr_env ti

	  // unfold...
	  cs =		{ cs_var_heap				= ti.ti_var_heap
	  			, cs_symbol_heap			= ti.ti_symbol_heap
				, cs_opt_type_heaps			= Yes ti.ti_type_heaps
	  			, cs_cleanup_info			= ti.ti_cleanup_info
	  			}
	  (Case copied_kees, cs)
			= copy (Case {kees & case_expr=EE}) {ci_handle_aci_free_vars = SubstituteAciFreeVars} cs
	  {cs_var_heap=ti_var_heap, cs_symbol_heap=ti_symbol_heap, cs_cleanup_info=ti_cleanup_info, cs_opt_type_heaps = Yes ti_type_heaps}
	  		= cs

	  (new_info_ptr, ti_var_heap) = newPtr VI_Empty ti_var_heap
	  var_id = {id_name = "_x", id_info = nilPtr}
	  case_free_var = {fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}
	  case_var = Var {var_ident = var_id, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr}
	  copied_expr = Case {copied_kees & case_expr=case_var}
	  form_vars = [case_free_var:form_vars]
	  fun_arity = fun_arity+1
	  // generated function...
	  fun_def =	{ fun_ident					= ro_fun.symb_ident
				, fun_arity					= fun_arity
				, fun_priority				= NoPrio
				, fun_body					= TransformedBody { tb_args = form_vars, tb_rhs = copied_expr}
				, fun_type					= Yes fun_type
				, fun_pos					= NoPos
				, fun_kind					= FK_Function cNameNotLocationDependent
				, fun_lifted				= undeff
				, fun_info = 	{	fi_calls		= []
								,	fi_group_index	= outer_fun_def.fun_info.fi_group_index
								,	fi_def_level	= NotALevel
								,	fi_free_vars	= []
								,	fi_local_vars	= []
								,	fi_dynamics		= []
								,	fi_properties	= outer_fun_def.fun_info.fi_properties
								}	
				}
	# cc_args_from_outer_fun		= [ cons_arg \\ cons_arg <- outer_cons_args.cc_args & used <- used_mask | used ]
	  cc_linear_bits_from_outer_fun	= [ cons_arg \\ cons_arg <- outer_cons_args.cc_linear_bits & used <- used_mask | used ]
	  new_cons_args =
	  			{ cc_size			= fun_arity
	  			, cc_args			= [CActive : repeatn nr_of_lifted_vars CPassive ++ cc_args_from_outer_fun]
	  			, cc_linear_bits	= [True    : repeatn nr_of_lifted_vars    False ++ cc_linear_bits_from_outer_fun]
	  			, cc_producer		= False
	  			}
	  gf =		{ gf_fun_def		= fun_def
	  			, gf_instance_info	= II_Empty
	  			, gf_cons_args		= new_cons_args
	  			, gf_fun_index		= fun_index
	  			}
	  ti_fun_heap = writePtr fun_info_ptr (FI_Function gf) ti.ti_fun_heap
	  ti = { ti & ti_new_functions	= [fun_info_ptr:ti.ti_new_functions]
	  			, ti_var_heap		= ti_var_heap
	  			, ti_fun_heap		= ti_fun_heap
	  			, ti_symbol_heap	= ti_symbol_heap
	  			, ti_type_heaps		= ti_type_heaps
	  			, ti_cleanup_info	= ti_cleanup_info 
	  			}
	  app_symb = { ro_fun & symb_kind = SK_GeneratedFunction fun_info_ptr fun_index}
	  app_args = [old_case_expr : free_vars_to_bound_vars ro_fun_args]
	= (App {app_symb = app_symb, app_args = app_args, app_info_ptr = nilPtr}, ti)

get_types_of_local_vars n_vars var_heap
	= mapSt get_type_of_local_var n_vars var_heap
where
	get_type_of_local_var {fv_info_ptr} var_heap
		# (EVI_VarType a_type, var_heap)	= readExtendedVarInfo fv_info_ptr var_heap
		= (a_type, var_heap)

determine_case_function_type fun_arity ct_result_type arg_types st_attr_env ti=:{ti_type_heaps}
	# (type_variables, th_vars)				= getTypeVars [ct_result_type:arg_types] ti_type_heaps.th_vars
	  (fresh_type_vars, th_vars)			= mapSt bind_to_fresh_type_variable type_variables th_vars
	  ti_type_heaps							= { ti_type_heaps & th_vars = th_vars }
	  (fresh_arg_types, ti_type_heaps)		= substitute arg_types ti_type_heaps
	  (fresh_result_type, ti_type_heaps)	= substitute ct_result_type ti_type_heaps
	  fun_type =
	  			{ st_vars					= fresh_type_vars
	  			, st_args					= fresh_arg_types
	  			, st_arity					= fun_arity
	  			, st_args_strictness		= NotStrict
	  			, st_result					= fresh_result_type
	  			, st_context				= []
	  			, st_attr_vars				= []
	  			, st_attr_env				= []
	  			}
	  ti									= { ti & ti_type_heaps = ti_type_heaps }
	= (fun_type,ti)

removeNeverMatchingSubcases :: Expression !.ReadOnlyTI -> Expression
removeNeverMatchingSubcases keesExpr=:(Case kees) ro
	// remove those case guards whose right hand side is a never matching case
	| is_never_matching_case keesExpr
		= keesExpr
	# {case_guards, case_default} = kees
	  filtered_default = get_filtered_default case_default
	= case case_guards of
		AlgebraicPatterns i alg_patterns
			| not (any (is_never_matching_case o get_alg_rhs) alg_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_alg_rhs) alg_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident <-!- ("removeNeverMatchingSubcases:AlgebraicPatterns:neverMatchingCase",never_ident)
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = AlgebraicPatterns i filtered_case_guards, case_default = filtered_default }
		BasicPatterns bt basic_patterns
			| not (any (is_never_matching_case o get_basic_rhs) basic_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_basic_rhs) basic_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident <-!- ("removeNeverMatchingSubcases:BasicPatterns:neverMatchingCase",never_ident)
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = BasicPatterns bt filtered_case_guards, case_default = filtered_default }
		OverloadedListPatterns i decons_expr alg_patterns
			| not (any (is_never_matching_case o get_alg_rhs) alg_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_alg_rhs) alg_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident <-!- ("removeNeverMatchingSubcases:OverloadedListPatterns:neverMatchingCase",never_ident)
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = OverloadedListPatterns i decons_expr filtered_case_guards, case_default = filtered_default }
		_	-> abort "removeNeverMatchingSubcases does not match"
where
	get_filtered_default y=:(Yes c_default)
		| is_never_matching_case c_default
			= No
		= y
	get_filtered_default no
		= no
	has_become_never_matching No [] = True
	has_become_never_matching _ _ = False
	is_default_only (Yes _) [] = True
	is_default_only _ _ = False
	is_never_matching_case (Case {case_guards = NoPattern, case_default = No })
		= True
	is_never_matching_case _
		= False
	get_alg_rhs {ap_expr} = ap_expr
	get_basic_rhs {bp_expr} = bp_expr
	is_never_matching_default No
		= False
	is_never_matching_default (Yes expr)
		= is_never_matching_case expr
	never_ident = case ro.ro_root_case_mode of
		NotRootCase -> kees.case_ident
		_ -> Yes ro.ro_tfi.tfi_case.symb_ident
removeNeverMatchingSubcases expr ro
	= expr

	
instance transform LetBind
where
	transform bind=:{lb_src} ro ti
		# (lb_src, ti) = transform lb_src ro ti
		= ({ bind & lb_src = lb_src }, ti)

instance transform BasicPattern
where
	transform pattern=:{bp_expr} ro ti
		# (bp_expr, ti) = transform bp_expr ro ti
		= ({ pattern & bp_expr = bp_expr }, ti)

instance transform AlgebraicPattern
where
	transform pattern=:{ap_expr} ro ti
		# (ap_expr, ti) = transform ap_expr ro ti
		= ({ pattern & ap_expr = ap_expr }, ti)

instance transform CasePatterns
where
	transform (AlgebraicPatterns type patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		= (AlgebraicPatterns type patterns, ti)
	transform (BasicPatterns type patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		= (BasicPatterns type patterns, ti)
	transform (OverloadedListPatterns type=:(OverloadedList _ _ _ _) decons_expr patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		# (decons_expr, ti) = transform decons_expr ro ti
		= (OverloadedListPatterns type decons_expr patterns, ti)
	transform (OverloadedListPatterns type decons_expr patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		# (decons_expr, ti) = transform decons_expr ro ti
		= (OverloadedListPatterns type decons_expr patterns, ti)
	transform NoPattern ro ti
		= (NoPattern, ti)
	transform _ ro ti
		= abort "transform CasePatterns does not match"

instance transform (Optional a) | transform a
where
	transform (Yes x) ro ti
		# (x, ti) = transform x ro ti
		= (Yes x, ti)
	transform no ro ti
		= (no, ti)

instance transform [a] | transform a
where
	transform [x : xs]  ro ti
		# (x, ti) = transform x ro ti
		  (xs, ti) = transform xs ro ti
		= ([x : xs], ti)
	transform [] ro ti
		= ([], ti)

//@ tryToFindInstance: 

cIsANewFunction		:== True
cIsNotANewFunction	:== False

tryToFindInstance :: !{! Producer} !InstanceInfo !*(Heap FunctionInfo) -> *(!Bool, !FunctionInfoPtr, !InstanceInfo, !.FunctionHeap)
tryToFindInstance new_prods II_Empty fun_heap
	# (fun_def_ptr, fun_heap) = newPtr FI_Empty fun_heap
	= (cIsANewFunction, fun_def_ptr, II_Node new_prods fun_def_ptr II_Empty II_Empty, fun_heap)
tryToFindInstance new_prods instances=:(II_Node prods fun_def_ptr left right) fun_heap
	| size new_prods > size prods
		# (is_new, new_fun_def_ptr, right, fun_heap) = tryToFindInstance new_prods right fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
	| size new_prods < size prods
		# (is_new, new_fun_def_ptr, left, fun_heap) = tryToFindInstance new_prods left fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
	# cmp = compareProducers new_prods prods
	| cmp == Equal
		= (cIsNotANewFunction, fun_def_ptr, instances, fun_heap)
	| cmp == Greater
		# (is_new, new_fun_def_ptr, right, fun_heap) = tryToFindInstance new_prods right fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
		# (is_new, new_fun_def_ptr, left, fun_heap) = tryToFindInstance new_prods left fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)

compareProducers prods1 prods2
	#! nr_of_prods = size prods1
	= compare_producers 0 nr_of_prods prods1 prods2
where
	compare_producers prod_index nr_of_prods prods1 prods2
		| prod_index == nr_of_prods
			= Equal
		# cmp = prods1.[prod_index] =< prods2.[prod_index]
		| cmp == Equal
			= compare_producers (inc prod_index) nr_of_prods prods1 prods2
		= cmp

instance =< Bool
where
	(=<) True True = Equal
	(=<) True False = Smaller
	(=<) False True = Greater
	(=<) False False = Equal
	
instance =< Producer
where
	(=<) pr1 pr2
		| equal_constructor pr1 pr2
			= compare_constructor_arguments  pr1 pr2
		| less_constructor pr1 pr2
			= Smaller
			= Greater
	where
		compare_constructor_arguments (PR_Function _ _ index1) (PR_Function _ _ index2)
			= index1 =< index2
		compare_constructor_arguments (PR_GeneratedFunction _ _ index1) (PR_GeneratedFunction _ _ index2)
			= index1 =< index2
		compare_constructor_arguments 	(PR_Class app1 lifted_vars_with_types1 t1) 
										(PR_Class app2 lifted_vars_with_types2 t2) 
//			= app1.app_args =< app2.app_args
			# cmp = smallerOrEqual t1 t2
			| cmp<>Equal
				= cmp
			= compare_types lifted_vars_with_types1 lifted_vars_with_types2
		compare_constructor_arguments (PR_Curried symb_ident1 _) (PR_Curried symb_ident2 _)
			= symb_ident1 =< symb_ident2
		compare_constructor_arguments PR_Empty PR_Empty
			= Equal
		compare_constructor_arguments PR_Unused PR_Unused
			= Equal
		compare_constructor_arguments (PR_Constructor symb_ident1 _ _) (PR_Constructor symb_ident2 _ _)
			= symb_ident1 =< symb_ident2
		compare_constructor_arguments (PR_CurriedFunction symb_ident1 _ _) (PR_CurriedFunction symb_ident2 _ _)
			= symb_ident1 =< symb_ident2
			
		compare_types [(_, type1):types1] [(_, type2):types2]
			# cmp = smallerOrEqual type1 type2
			| cmp<>Equal
				= cmp
			= compare_types types1 types2
		compare_types [] [] = Equal
		compare_types [] _ = Smaller
		compare_types _ [] = Greater
		
/*
 *	UNIQUENESS STUFF...
 */

create_fresh_type_vars :: !Int !*TypeVarHeap -> (!{!TypeVar}, !*TypeVarHeap)
create_fresh_type_vars nr_of_all_type_vars th_vars
	# fresh_array = createArray  nr_of_all_type_vars {tv_ident = {id_name="",id_info=nilPtr}, tv_info_ptr=nilPtr}
	= iFoldSt allocate_fresh_type_var 0 nr_of_all_type_vars (fresh_array,th_vars)
where
	allocate_fresh_type_var i (array, th_vars)
		# (new_tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
		  tv = { tv_ident = { id_name = "a"+++toString i, id_info = nilPtr }, tv_info_ptr=new_tv_info_ptr }
		= ({array & [i] = tv}, th_vars)

create_fresh_attr_vars :: !{!CoercionTree} !Int !*AttrVarHeap -> (!{!TypeAttribute}, !.AttrVarHeap)
create_fresh_attr_vars demanded nr_of_attr_vars th_attrs
	# fresh_array = createArray nr_of_attr_vars TA_None
	= iFoldSt (allocate_fresh_attr_var demanded) 0 nr_of_attr_vars (fresh_array, th_attrs)
where
	allocate_fresh_attr_var demanded i (attr_var_array, th_attrs)
		= case demanded.[i] of
			CT_Unique
				-> ({ attr_var_array & [i] = TA_Unique}, th_attrs)
			CT_NonUnique
				-> ({ attr_var_array & [i] = TA_Multi}, th_attrs)
			_
				# (new_info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
				-> ({ attr_var_array & [i] = TA_Var { av_ident = NewAttrVarId i, av_info_ptr = new_info_ptr }}, th_attrs)

coercionsToAttrEnv :: !{!TypeAttribute} !Coercions -> [AttrInequality]
coercionsToAttrEnv attr_vars {coer_demanded, coer_offered}
	= flatten [ [ {ai_offered = toAttrVar attr_vars.[offered],
					ai_demanded = toAttrVar attr_vars.[demanded] }
				\\ offered <- fst (flattenCoercionTree offered_tree) ]
			  \\ offered_tree<-:coer_offered & demanded<-[0..] ]
  where
	toAttrVar (TA_Var av) = av

substitute_attr_inequality {ai_offered, ai_demanded} th_attrs
	#! ac_offered = pointer_to_int ai_offered th_attrs
	   ac_demanded = pointer_to_int ai_demanded th_attrs
	= ({ ac_offered = ac_offered, ac_demanded = ac_demanded }, th_attrs)
  where
	pointer_to_int {av_info_ptr} th_attrs
		# (AVI_Attr (TA_TempVar i)) = sreadPtr av_info_ptr th_attrs
		= i

new_inequality {ac_offered, ac_demanded} coercions
	= newInequality ac_offered ac_demanded coercions

:: UniquenessRequirement =
	{	ur_offered		:: !AType
	,	ur_demanded		:: !AType
	,	ur_attr_ineqs	:: ![AttrCoercion]
	}

:: ATypesWithStrictness = {ats_types::![AType],ats_strictness::!StrictnessList};

compute_args_strictness new_arg_types_array = compute_args_strictness 0 0 NotStrict 0 new_arg_types_array
  	where
  		compute_args_strictness strictness_index strictness strictness_list array_index new_arg_types_array
  			| array_index==size new_arg_types_array
  				| strictness==0
  					= strictness_list
  					= append_strictness strictness strictness_list
  				# {ats_types,ats_strictness} = new_arg_types_array.[array_index]
  				# (strictness_index,strictness) = add_strictness_for_arguments ats_types 0 strictness_index strictness strictness_list
  					with
  						add_strictness_for_arguments [] ats_strictness_index strictness_index strictness strictness_list
  							= (strictness_index,strictness)
  						add_strictness_for_arguments [_:ats_types] ats_strictness_index strictness_index strictness strictness_list
  							| arg_is_strict ats_strictness_index ats_strictness
  								# (strictness_index,strictness,strictness_list) = add_next_strict strictness_index strictness strictness_list
  								= add_strictness_for_arguments ats_types (ats_strictness_index+1) strictness_index strictness strictness_list
  								# (strictness_index,strictness,strictness_list) = add_next_not_strict strictness_index strictness strictness_list
  								= add_strictness_for_arguments ats_types (ats_strictness_index+1) strictness_index strictness strictness_list
  				= compute_args_strictness strictness_index strictness strictness_list (array_index+1) new_arg_types_array
	
/*
 * GENERATE FUSED FUNCTION
 */

:: *DetermineArgsState =
	{ das_vars						:: ![FreeVar]
	, das_arg_types					:: !*{#ATypesWithStrictness}
	, das_next_attr_nr				:: !Int
	, das_new_linear_bits			:: ![Bool]
	, das_new_cons_args				:: ![ConsClass]
	, das_uniqueness_requirements	:: ![UniquenessRequirement]
	, das_AVI_Attr_TA_TempVar_info_ptrs	:: ![[AttributeVar]]
	, das_subst						:: !*{!Type}
	, das_type_heaps				:: !*TypeHeaps
	, das_fun_defs					:: !*{#FunDef}
	, das_fun_heap					:: !*FunctionHeap
	, das_var_heap					:: !*VarHeap
	, das_cons_args					:: !*{!ConsClasses}
	, das_predef					:: !*PredefinedSymbols
	}

generateFunction :: !SymbIdent !FunDef ![ConsClass] ![Bool] !{! Producer} !FunctionInfoPtr !ReadOnlyTI !Int !*TransformInfo -> (!Index, !Int, !*TransformInfo)
generateFunction app_symb fd=:{fun_body = TransformedBody {tb_args,tb_rhs},fun_info = {fi_group_index}} 
				 cc_args cc_linear_bits prods fun_def_ptr ro n_extra
				 ti=:{ti_var_heap,ti_next_fun_nr,ti_new_functions,ti_fun_heap,ti_symbol_heap,ti_fun_defs,
				 		ti_type_heaps,ti_cons_args,ti_cleanup_info, ti_type_def_infos}
//	| False--->("generating new function",fd.fun_ident.id_name,"->",ti_next_fun_nr,prods,tb_args)		= undef
/*
	| False-!->("generating new function",fd.fun_ident.id_name,"->",ti_next_fun_nr)		= undef
	| False-!->("with type",fd.fun_type)												= undef
	| False-!->("producers:",II_Node prods nilPtr II_Empty II_Empty,("cc_args",cc_args,("cc_linear_bits",cc_linear_bits)))		= undef
	| False-!->("body:",tb_args, tb_rhs)												= undef
*/
	#!(fi_group_index, ti_cons_args, ti_fun_defs, ti_fun_heap)
			= max_group_index 0 prods ro.ro_main_dcl_module_n fi_group_index ti_fun_defs ti_fun_heap ti_cons_args

	# (Yes consumer_symbol_type) = fd.fun_type
	  consumer_symbol_type		= strip_universal_quantor consumer_symbol_type
	  (sound_consumer_symbol_type, (ti_type_heaps, ti_type_def_infos))
	  		= add_propagation_attributes` ro.ro_common_defs consumer_symbol_type (ti_type_heaps, ti_type_def_infos)
	  (function_producer_types, ti_fun_defs, ti_fun_heap)
	  		= iFoldSt (accum_function_producer_type prods ro) 0 (size prods) 
	  				([], ti_fun_defs, ti_fun_heap)
	  function_producer_types	= mapOpt strip_universal_quantor function_producer_types
	  (opt_sound_function_producer_types, (ti_type_heaps, ti_type_def_infos))
	  		= mapSt (add_propagation_attributes ro.ro_common_defs) function_producer_types (ti_type_heaps, ti_type_def_infos)
	  (opt_sound_function_producer_types, ti_type_heaps)
	  		= mapSt copy_opt_symbol_type opt_sound_function_producer_types ti_type_heaps

	  sound_function_producer_types		// nog even voor determine args....
	  		= [x \\ Yes x <- opt_sound_function_producer_types]

	# {st_attr_vars,st_args,st_args_strictness,st_result,st_attr_env}
	  		= sound_consumer_symbol_type

	  (class_types, ti_fun_defs, ti_fun_heap)
	  		= iFoldSt (accum_class_type prods ro) 0 (size prods) 
	  				([], ti_fun_defs, ti_fun_heap)
	  (type_vars_in_class_types, th_vars)
	  		= mapSt getTypeVars class_types ti_type_heaps.th_vars

	  all_involved_types
	  		= class_types ++ (flatten (map (\{st_args, st_result}-> [st_result:st_args]) 
					  					[sound_consumer_symbol_type:sound_function_producer_types]))
//	| False ---> ("all_involved_types",app_symb,all_involved_types)	= undef
	# (propagating_cons_vars, th_vars)
	  		= collectPropagatingConsVars all_involved_types th_vars
	  all_type_vars
	  		=   flatten [st_vars \\ {st_vars} <- [sound_consumer_symbol_type:sound_function_producer_types]]
	  		  ++flatten type_vars_in_class_types
//	| False -!-> ("all_type_vars",all_type_vars)	= undef
	# (nr_of_all_type_vars, th_vars)
	  		=  foldSt bind_to_temp_type_var all_type_vars (0, th_vars)
	  subst = createArray nr_of_all_type_vars TE
	  (next_attr_nr, th_attrs)
	  		= bind_to_temp_attr_vars st_attr_vars (FirstAttrVar, ti_type_heaps.th_attrs)
	  // remember the st_attr_vars, because the AVI_Attr (TA_TempVar _)'s must be removed before unfold,
	  // because types in Cases and Lets should not use TA_TempVar's
	  das_AVI_Attr_TA_TempVar_info_ptrs = [st_attr_vars]
	  ti_type_heaps = {ti_type_heaps & th_attrs = th_attrs, th_vars = th_vars}
//	| False-!->("before substitute", st_args, "->", st_result)		= undef
	# ((st_args,st_result), ti_type_heaps)
	  		= substitute (st_args,st_result) ti_type_heaps
//	| False-!->("after substitute", st_args, "->", st_result)		= undef
// determine args...
	# das = { das_vars						= []
			, das_arg_types					= st_args_array st_args st_args_strictness
			, das_next_attr_nr				= next_attr_nr
			, das_new_linear_bits			= []
			, das_new_cons_args				= []
			, das_uniqueness_requirements	= []
			, das_AVI_Attr_TA_TempVar_info_ptrs = das_AVI_Attr_TA_TempVar_info_ptrs
			, das_subst						= subst
			, das_type_heaps				= ti_type_heaps
			, das_fun_defs					= ti_fun_defs
			, das_fun_heap					= ti_fun_heap
			, das_var_heap					= ti_var_heap
			, das_cons_args					= ti_cons_args
			, das_predef					= ti.ti_predef_symbols
			}
	# das		= determine_args cc_linear_bits cc_args 0 prods opt_sound_function_producer_types tb_args ro das
	  uvar		= [arg \\ prod <-: prods & arg <- tb_args | isUnused prod]
	  				with
	  					isUnused PR_Unused = True
	  					isUnused _ = False
	  
	  new_fun_args				= das.das_vars
	  new_arg_types_array		= das.das_arg_types
	  next_attr_nr				= das.das_next_attr_nr
	  new_linear_bits			= das.das_new_linear_bits
	  new_cons_args				= das.das_new_cons_args
	  uniqueness_requirements	= das.das_uniqueness_requirements
	  das_AVI_Attr_TA_TempVar_info_ptrs = das.das_AVI_Attr_TA_TempVar_info_ptrs
	  subst						= das.das_subst
	  ti_type_heaps				= das.das_type_heaps
	  ti_fun_defs				= das.das_fun_defs
	  ti_fun_heap				= das.das_fun_heap
	  ti_var_heap				= das.das_var_heap
	  ti_cons_args				= das.das_cons_args
	  ti_predef_symbols			= das.das_predef
	  
	  new_fun_arity
	  		= length new_fun_args
	| SwitchArityChecks (new_fun_arity > 32) False
		# new_gen_fd =
			{	gf_fun_def			= fd
			,	gf_instance_info	= II_Empty
			,	gf_cons_args		= {cc_args = [], cc_size = 0, cc_linear_bits=[], cc_producer = False}
			,	gf_fun_index		= -1
			}
		# ti_fun_heap 	= ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd)
		# ti = { ti & ti_type_heaps = ti_type_heaps, ti_symbol_heap = ti_symbol_heap, ti_fun_defs = ti_fun_defs
				, ti_fun_heap = ti_fun_heap, ti_var_heap = ti_var_heap, ti_cons_args = ti_cons_args, ti_type_def_infos = ti_type_def_infos
				, ti_predef_symbols = ti_predef_symbols }
		| ro.ro_transform_fusion
			#  ti = { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Function Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
			= (-1,new_fun_arity,ti)
		= (-1,new_fun_arity,ti)
	# new_arg_types = flatten [ ats_types \\ {ats_types}<-:new_arg_types_array ]
	  
	  new_args_strictness = compute_args_strictness new_arg_types_array
	  	  	  
	  cons_vars
	  		= createArray (inc (BITINDEX nr_of_all_type_vars)) 0
	  (cons_vars, th_vars)
			= foldSt set_cons_var_bit propagating_cons_vars (cons_vars, ti_type_heaps.th_vars)
//	| False--->("subst before", [el\\el<-:subst], "cons_vars", [el\\el<-:cons_vars])		= undef
	# ti_type_heaps = { ti_type_heaps & th_vars = th_vars }

	# (subst, next_attr_nr, ti_type_heaps, ti_type_def_infos)
	  		= liftSubstitution subst ro.ro_common_defs cons_vars next_attr_nr ti_type_heaps ti_type_def_infos
//	| False--->("subst after lifting", [el\\el<-:subst])		= undef

	# (consumer_attr_inequalities, th_attrs)
			= mapSt substitute_attr_inequality st_attr_env ti_type_heaps.th_attrs
	  ti_type_heaps
	  		= { ti_type_heaps & th_attrs = th_attrs }
	  
	  coercions
	  		= { coer_offered	= {{ CT_Empty \\ i <- [0 .. next_attr_nr - 1] } & [AttrMulti] = CT_NonUnique }
	  		  , coer_demanded	= {{ CT_Empty \\ i <- [0 .. next_attr_nr - 1] } & [AttrUni] = CT_Unique } 
	  		  }
	  coercions 
	  		= foldSt new_inequality consumer_attr_inequalities coercions
	  coercions 
	  		= foldSt (\{ur_attr_ineqs} coercions -> foldSt new_inequality ur_attr_ineqs coercions)
		  			uniqueness_requirements coercions
	  (subst, coercions, ti_type_def_infos, ti_type_heaps)
	  		= foldSt (coerce_types ro.ro_common_defs cons_vars) uniqueness_requirements
	  				(subst, coercions, ti_type_def_infos, ti_type_heaps)
	# ([st_result:new_arg_types], (coercions, subst, ti_type_heaps, ti_type_def_infos))
	  		= mapSt (expand_type ro.ro_common_defs cons_vars) [st_result:new_arg_types]
	  				(coercions, subst, ti_type_heaps, ti_type_def_infos)
//	| False-!->("unified type", new_arg_types, "->", st_result)		= undef

	# (fresh_type_vars_array,ti_type_heaps)
	  		= accTypeVarHeap (create_fresh_type_vars nr_of_all_type_vars) ti_type_heaps
	  (attr_partition, demanded) 
	  		= partitionateAttributes coercions.coer_offered coercions.coer_demanded
	  		// to eliminate circles in the attribute inequalities graph that was built during "determine_args"
	  (fresh_attr_vars, ti_type_heaps)
	  		= accAttrVarHeap (create_fresh_attr_vars demanded (size demanded)) ti_type_heaps
	  		// the attribute variables stored in the "demanded" graph are represented as integers: 
	  		// prepare to replace them by pointers
	  ((fresh_arg_types, fresh_result_type), used_attr_vars) 
	  		= replaceIntegers (new_arg_types, st_result) (fresh_type_vars_array, fresh_attr_vars, attr_partition)
	  				 (createArray (size demanded) False)
			// replace the integer-attribute-variables with pointer-attribute-variables or TA_Unique or TA_Multi
	  final_coercions
	  		= removeUnusedAttrVars demanded [i \\ i<-[0..size used_attr_vars-1] | not used_attr_vars.[i]]
			// the attribute inequalities graph may have contained unused attribute variables.

 	  (all_attr_vars2, ti_type_heaps)
 	  		= accAttrVarHeap (getAttrVars (fresh_arg_types, fresh_result_type)) ti_type_heaps
	  all_attr_vars
	  		= [ attr_var \\ TA_Var attr_var <- [fresh_attr_vars.[i] \\ i<-[0..size used_attr_vars-1] | used_attr_vars.[i]]]
 	# (all_fresh_type_vars, ti_type_heaps)
 	  		= accTypeVarHeap (getTypeVars (fresh_arg_types, fresh_result_type)) ti_type_heaps
	  new_fun_type
	  		= Yes
	  			{ st_vars		= all_fresh_type_vars
	  			, st_args		= fresh_arg_types
	  			, st_args_strictness=new_args_strictness
	  			, st_arity		= new_fun_arity
	  			, st_result		= fresh_result_type
	  			, st_context	= []
	  			, st_attr_vars	= all_attr_vars
	  			, st_attr_env	= coercionsToAttrEnv fresh_attr_vars final_coercions 
				}
/* DvA... STRICT_LET
	  // DvA: moet hier rekening houden met strictness dwz alleen safe args expanderen en rest in stricte let genereren...
	  (tb_rhs,ti_symbol_heap,strict_free_vars) = case let_bindings of
	  			([],[],_,_)
	  				-> (tb_rhs,ti_symbol_heap,[])
	  			(s,l,st,lt)
					# let_type = st++lt
					# (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti_symbol_heap
	  				# new_expr = Let
	  						{ let_strict_binds	= s
	  						, let_lazy_binds	= l
	  						, let_expr			= tb_rhs
	  						, let_info_ptr		= new_info_ptr
	  						, let_expr_position	= NoPos
	  						}
	  				# strict_free_vars = [lb_dst \\ {lb_dst} <- s]
	  				-> (new_expr,ti_symbol_heap,strict_free_vars)
...DvA */
	  new_fd_expanding 
	  		= { fd & fun_body = Expanding new_fun_args, fun_arity = new_fun_arity,fun_type=new_fun_type, 
	  					fun_info.fi_group_index = fi_group_index
/* DvA... STRICT_LET
					,fun_info.fi_free_vars = strict_free_vars++fd.fun_info.fi_free_vars
...DvA */
	  					}
	  new_fd_cons_args
//	  		= {cc_args = new_cons_args, cc_size = length new_cons_args, cc_linear_bits=new_linear_bits, cc_producer = False}
	  		= {cc_args = repeatn (length new_cons_args) CPassive, cc_size = length new_cons_args, cc_linear_bits=new_linear_bits, cc_producer = False}
	  new_gen_fd
	  		= { gf_fun_def = new_fd_expanding,	gf_instance_info = II_Empty, gf_fun_index = ti_next_fun_nr,
				 gf_cons_args = new_fd_cons_args }
	  ti_fun_heap = ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd)
	  (subst, _)
	  		= iFoldSt (replace_integers_in_substitution (fresh_type_vars_array, fresh_attr_vars, attr_partition))
	  				0 nr_of_all_type_vars (subst, createArray (size demanded) False)
	  		// replace the integer-attribute-variables with pointer-attribute-variables or TA_Unique or TA_Multi in subst
	  (_, th_vars)
	  		= foldSt (\{tv_info_ptr} (i, th_vars) 
	  					-> case subst.[i] of
	  						TE
	  							-> (i+1, writePtr tv_info_ptr (TVI_Type (TV fresh_type_vars_array.[i])) th_vars)
	  						_
	  							-> (i+1, writePtr tv_info_ptr (TVI_Type subst.[i]) th_vars))
	  				all_type_vars (0, ti_type_heaps.th_vars)
	  // remove the AVI_Attr (TA_TempVar _)'s before unfold, because types in Cases and Lets should not use TA_TempVar's
	  th_attrs = remove_TA_TempVars_in_info_ptrs das_AVI_Attr_TA_TempVar_info_ptrs ti_type_heaps.th_attrs
	  cs 	=	{ cs_var_heap				= ti_var_heap
	  			, cs_symbol_heap			= ti_symbol_heap
				, cs_opt_type_heaps			= Yes { ti_type_heaps & th_vars=th_vars, th_attrs=th_attrs }
	  			, cs_cleanup_info			= ti_cleanup_info
	  			}
//	| False ---> ("before unfold:", tb_rhs) = undef
	# (tb_rhs, {cs_var_heap=var_heap,cs_symbol_heap,cs_opt_type_heaps=Yes ti_type_heaps, cs_cleanup_info})
			= copy tb_rhs {ci_handle_aci_free_vars = RemoveAciFreeVars} cs
//	| False ---> ("unfolded:", tb_rhs) = undef
	# var_heap						= fold2St store_arg_type_info new_fun_args fresh_arg_types var_heap
		with
			store_arg_type_info {fv_info_ptr} a_type ti_var_heap
				= setExtendedVarInfo fv_info_ptr (EVI_VarType a_type) ti_var_heap
	# ro_fun= { symb_ident = fd.fun_ident, symb_kind = SK_GeneratedFunction fun_def_ptr ti_next_fun_nr }
	# ro_root_case_mode = case tb_rhs of 
	  						Case _
	  							-> RootCase
	  						_	-> NotRootCase
	
	# (args1,resto,restn,var_heap) = take1 tb_args new_fun_args var_heap
		with
			take1 [o:os] [n:ns] var_heap
				# (vi,var_heap) = readVarInfo o.fv_info_ptr var_heap
				# eq = case vi of
					VI_Variable _ fip	-> fip == n.fv_info_ptr
					_					-> False
				| eq
					# (ts,os,ns,var_heap)	= take1 os ns var_heap
					= ([o:ts],os,ns,var_heap)
					= ([],[o:os],[n:ns],var_heap)
			take1 os ns var_heap = ([],os,ns,var_heap)
	# (args2o,args2n,resto,restn,var_heap) = take2 resto restn var_heap
		with
			take2 [] [] var_heap = ([],[],[],[],var_heap)
			take2 os ns var_heap
				# (os`,var_heap) = extend os var_heap 
				# os`` = map fst os`
				# ns`` = map (\{fv_info_ptr}->fv_info_ptr) ns
				# condO = \(o,_) -> not (isMember o ns``)
				# condN = \{fv_info_ptr} -> not (isMember fv_info_ptr os``)
				# (ao`,ro`) = (takeWhile condO os`, dropWhile condO os`)
				# (an,rn) = (takeWhile condN ns, dropWhile condN ns)
				# ao = shrink ao`
				# ro = shrink ro`
				= (ao,an,ro,rn,var_heap)
			where
				extend os uvh	= seqList (map ext os) uvh
				ext o uvh
					# (vi,uvh) = readVarInfo o.fv_info_ptr uvh
					= case vi of
						VI_Variable _ fip	-> ((fip,o),uvh)
						_					-> ((nilPtr,o),uvh)
				shrink as = map snd as

				isMember x [hd:tl]
					| isNilPtr x	= False
					| isNilPtr hd	= isMember x tl
					= hd==x || isMember x tl
				isMember x []	= False

	# (args3,resto,restn,var_heap) = take1 resto restn var_heap
		with
			take1 [o:os] [n:ns] var_heap
				# (vi,var_heap) = readVarInfo o.fv_info_ptr var_heap
				# eq = case vi of
					VI_Variable _ fip	-> fip == n.fv_info_ptr
					_					-> False
				| eq
					# (ts,os,ns,var_heap)	= take1 os ns var_heap
					= ([o:ts],os,ns,var_heap)
					= ([],[o:os],[n:ns],var_heap)
			take1 os ns var_heap = ([],os,ns,var_heap)
/*			take1 [] [] = ([],[],[])
			take1 [o:os] [n:ns]
				| o.fv_info_ptr == n.fv_info_ptr
					# (ts,os,ns)	= take1 os ns
					= ([o:ts],os,ns)
					= ([],[o:os],[n:ns])
*/
	| False -!-> ("genFun",(tb_args,new_fun_args),args1,(args2o,args2n),args3,(resto,restn)) = undef
	| not (isEmpty resto)	= abort "genFun:resto"
	| not (isEmpty restn)	= abort "genFun:restn"

	# tfi = {	tfi_root = ro_fun,
				tfi_case = ro_fun,
				tfi_orig = app_symb,
				tfi_args = new_fun_args,
				tfi_vars = uvar ++ [arg \\ arg <- new_fun_args & i <- [0..] | arg_is_strict i new_args_strictness],
										// evt ++ verwijderde stricte arg...
				tfi_geni = (length args1,length args2n)
			 }
	# ro = { ro & ro_root_case_mode = ro_root_case_mode, ro_tfi=tfi}
				// ---> ("genfun uvars",uvar,[arg \\ arg <- new_fun_args & i <- [0..] | arg_is_strict i new_args_strictness])
//	| False ---> ("transform generated function:",ti_next_fun_nr,ro_root_case_mode)		= undef
//	| False ---> ("transforming new function:",ti_next_fun_nr,tb_rhs)		= undef
//	| False -!-> ("transforming new function:",tb_rhs)		= undef
	# ti
			= { ti & ti_var_heap = var_heap, ti_fun_heap = ti_fun_heap, ti_symbol_heap = cs_symbol_heap,
	  			ti_next_fun_nr = inc ti_next_fun_nr, ti_type_def_infos = ti_type_def_infos,
	  			ti_new_functions = [fun_def_ptr : ti_new_functions], ti_fun_defs = ti_fun_defs,
	  			ti_type_heaps = ti_type_heaps, ti_cleanup_info = cs_cleanup_info,
	  			ti_cons_args = ti_cons_args,
	  			ti_predef_symbols = ti_predef_symbols }
	# ti = arity_warning "generateFunction" fd.fun_ident.id_name ti_next_fun_nr new_fun_arity ti

	# (tb_rhs,ti)		= case n_extra of
							0	-> (tb_rhs,ti)
							_
								# act_args = map f2b (reverse (take n_extra (reverse new_fun_args)))
									with
										f2b { fv_ident, fv_info_ptr }
											= Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = nilPtr }
								-> add_args_to_fun_body act_args fresh_result_type tb_rhs ro ti

	  (new_fun_rhs, ti)
			= transform tb_rhs ro ti
	  new_fd
	  		= { new_fd_expanding & fun_body = TransformedBody {tb_args = new_fun_args, tb_rhs = new_fun_rhs} }
//	| False ---> ("generated function", new_fd)		= undef

	# new_gen_fd = { new_gen_fd & gf_fun_def = new_fd, gf_cons_args = new_fd_cons_args}
	# ti = { ti & ti_fun_heap 	= ti.ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd) }
	= (ti_next_fun_nr, new_fun_arity, ti)
where
	st_args_array :: ![AType] !StrictnessList -> .{#ATypesWithStrictness}
	st_args_array st_args args_strictness
		# strict1=Strict 1
		= { {ats_types=[el],ats_strictness=if (arg_is_strict i args_strictness) strict1 NotStrict} \\ i<-[0..] & el <- st_args }

	is_dictionary :: !.AType !{#{#.TypeDefInfo}} -> Bool
	is_dictionary {at_type=TA {type_index} _} es_td_infos
		#! td_infos_of_module=es_td_infos.[type_index.glob_module]
		= type_index.glob_object>=size td_infos_of_module || td_infos_of_module.[type_index.glob_object].tdi_group_nr==(-1)
	is_dictionary _ es_td_infos
		= False

	set_cons_var_bit :: !.TypeVar !*(!*{#.Int},!u:(Heap TypeVarInfo)) -> (!.{#Int},!v:(Heap TypeVarInfo)), [u <= v]
	set_cons_var_bit {tv_info_ptr} (cons_vars, th_vars)
		# (TVI_Type (TempV i), th_vars) = readPtr tv_info_ptr th_vars 
		= (set_bit i cons_vars, th_vars)

	copy_opt_symbol_type :: !(Optional .SymbolType) !*TypeHeaps -> (!(Optional .SymbolType),!.TypeHeaps)
	copy_opt_symbol_type No ti_type_heaps
		= (No, ti_type_heaps)
	copy_opt_symbol_type (Yes symbol_type=:{st_vars, st_attr_vars, st_args, st_result, st_attr_env})
				ti_type_heaps=:{th_vars, th_attrs}
		# (fresh_st_vars, th_vars)
				= mapSt bind_to_fresh_type_variable st_vars th_vars
		  (fresh_st_attr_vars, th_attrs)
				= mapSt bind_to_fresh_attr_variable st_attr_vars th_attrs
		  ([fresh_st_result:fresh_st_args], ti_type_heaps)
		  		= substitute [st_result:st_args] { ti_type_heaps & th_vars = th_vars, th_attrs = th_attrs }
		  (fresh_st_attr_env, ti_type_heaps)
		  		= substitute st_attr_env ti_type_heaps
		= (Yes { symbol_type & st_vars = fresh_st_vars, st_attr_vars = fresh_st_attr_vars, st_args = fresh_st_args,
				st_result = fresh_st_result, st_attr_env = fresh_st_attr_env}, ti_type_heaps)

	add_propagation_attributes :: !{#.CommonDefs} !(Optional .SymbolType) !*(!*TypeHeaps,!*{#*{#.TypeDefInfo}})
											  -> (!(Optional .SymbolType),! (!.TypeHeaps,! {#.{# TypeDefInfo}}))
	add_propagation_attributes common_defs No state
		= (No, state)
	add_propagation_attributes common_defs (Yes st) state
		# (st, state)	= add_propagation_attributes` common_defs st state
		= (Yes st, state)

	add_propagation_attributes` :: !{#.CommonDefs} !.SymbolType !*(!*TypeHeaps,!*{#*{#.TypeDefInfo}})
											   -> (!.SymbolType,! (!.TypeHeaps,! {#.{# TypeDefInfo}}))
	add_propagation_attributes` common_defs st=:{st_args, st_result, st_attr_env, st_attr_vars}
				(type_heaps, type_def_infos)
		# ps	=	{ prop_type_heaps	= type_heaps
					, prop_td_infos		= type_def_infos
					, prop_attr_vars	= st_attr_vars
					, prop_attr_env		= st_attr_env
					, prop_error		= No 
					}
		# ([sound_st_result:sound_st_args], ps)
			  	= mapSt (add_propagation_attributes_to_atype common_defs) [st_result:st_args] ps
		  sound_symbol_type = {st & st_args	= sound_st_args
								  , st_result = sound_st_result
								  , st_attr_env = ps.prop_attr_env
								  , st_attr_vars = ps.prop_attr_vars
								}
		  state = (ps.prop_type_heaps, ps.prop_td_infos)
		= (sound_symbol_type, state)

	add_propagation_attributes_to_atype :: !{#.CommonDefs} !.AType !*PropState -> (!AType,!.PropState)
	add_propagation_attributes_to_atype modules type ps
		| is_dictionary type ps.prop_td_infos
			= (type, ps)
		# (type, prop_class, ps) = addPropagationAttributesToAType modules type ps
		= (type, ps)

	accum_class_type :: !{!.Producer} !.ReadOnlyTI !.Int !(!u:[v:AType],!.b,!.c) -> (!w:[x:AType],!.b,!.c), [u <= w,v <= x]
	accum_class_type prods ro i (type_accu, ti_fun_defs, ti_fun_heap)
		= case prods.[i] of
			PR_Class _ _ class_type
				-> ([{empty_atype & at_type = class_type}  : type_accu ], ti_fun_defs, ti_fun_heap)
			_
				-> (type_accu, ti_fun_defs, ti_fun_heap)

	accum_function_producer_type :: !{!.Producer} !.ReadOnlyTI !.Int !*(!u:[v:(Optional .SymbolType)],!*{#.FunDef},!*(Heap FunctionInfo)) -> (!w:[x:(Optional SymbolType)],!.{#FunDef},!.(Heap FunctionInfo)), [u <= w,v <= x]
	accum_function_producer_type prods ro i (type_accu, ti_fun_defs, ti_fun_heap)
		= case prods.[size prods-i-1] of
			PR_Empty
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Class _ _ class_type
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Unused
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			producer
				# (symbol,_) = get_producer_symbol producer
				  (symbol_type, ti_fun_defs, ti_fun_heap)
						= get_producer_type symbol ro ti_fun_defs ti_fun_heap
				-> ([Yes symbol_type:type_accu], ti_fun_defs, ti_fun_heap)

	collectPropagatingConsVars :: ![AType] !*(Heap TypeVarInfo) -> (!.[TypeVar],!.(Heap TypeVarInfo))
	collectPropagatingConsVars type th_vars
		# th_vars = performOnTypeVars initializeToTVI_Empty type th_vars
		= performOnTypeVars collect_unencountered_cons_var type ([], th_vars)
	  where
		collect_unencountered_cons_var :: !.TypeAttribute !u:TypeVar !*(!v:[w:TypeVar],!*(Heap TypeVarInfo)) -> (!x:[y:TypeVar],!.(Heap TypeVarInfo)), [v <= x,w u <= y]
		collect_unencountered_cons_var TA_MultiOfPropagatingConsVar tv=:{tv_info_ptr} (cons_var_accu, th_vars)
			# (tvi, th_vars) = readPtr tv_info_ptr th_vars
			= case tvi of
				TVI_Empty
					-> ([tv:cons_var_accu], writePtr tv_info_ptr TVI_Used th_vars)
				TVI_Used
					-> (cons_var_accu, th_vars)
		collect_unencountered_cons_var _ _ state
			= state

	replace_integers_in_substitution :: (!{!.TypeVar},!{!.TypeAttribute},!{#.Int}) !.Int !*(!*{!Type},!*{#.Bool}) -> (!.{!Type},!.{#Bool})
	replace_integers_in_substitution replace_input i (subst, used)
		# (subst_i, subst) = subst![i]
		  (subst_i, used)
		  		= replaceIntegers subst_i replace_input used
		= ({ subst & [i] = subst_i }, used)

	coerce_types common_defs cons_vars {ur_offered, ur_demanded} (subst, coercions, ti_type_def_infos, ti_type_heaps)
		# (opt_error_info, subst, coercions, ti_type_def_infos, ti_type_heaps)
				= determineAttributeCoercions ur_offered ur_demanded True
						subst coercions common_defs cons_vars ti_type_def_infos ti_type_heaps
		= case opt_error_info of
			Yes _
				-> abort "Error in compiler: determineAttributeCoercions failed in module trans"
			No
				-> (subst, coercions, ti_type_def_infos, ti_type_heaps)

	expand_type :: !{#.CommonDefs} !{#.Int} !.AType !*(!*Coercions,!u:{!.Type},!*TypeHeaps,!*{#*{#.TypeDefInfo}}) -> (!AType,!(!.Coercions,!v:{!Type},!.TypeHeaps,!{#.{#TypeDefInfo}})), [u <= v]
	expand_type ro_common_defs cons_vars atype (coercions, subst, ti_type_heaps, ti_type_def_infos)
		| is_dictionary atype ti_type_def_infos
			# (_, atype, subst) = arraySubst atype subst
			= (atype, (coercions, subst, ti_type_heaps, ti_type_def_infos))
		# es = {es_type_heaps = ti_type_heaps, es_td_infos = ti_type_def_infos}
		  (_, btype, (subst, es))
		  		= expandType ro_common_defs cons_vars atype (subst, es)
 		  {es_type_heaps = ti_type_heaps, es_td_infos = ti_type_def_infos}
				= es
		# cs = {crc_type_heaps = ti_type_heaps, crc_coercions = coercions, crc_td_infos = ti_type_def_infos}
		  (_, cs)
		  		= coerce PositiveSign ro_common_defs cons_vars [] btype btype cs
		  { crc_type_heaps = ti_type_heaps, crc_coercions = coercions, crc_td_infos = ti_type_def_infos }
		  		= cs
		= (btype, (coercions, subst, ti_type_heaps, ti_type_def_infos))

// get_producer_type retrieves the type of symbol
get_producer_type :: !SymbIdent !.ReadOnlyTI !*{#FunDef} !*FunctionHeap -> (!SymbolType,!*{#FunDef},!*FunctionHeap)
get_producer_type {symb_kind=SK_Function {glob_module, glob_object}} ro fun_defs fun_heap
	| glob_module == ro.ro_main_dcl_module_n
		# ({fun_type=Yes symbol_type, fun_info={fi_properties}}, fun_defs) = fun_defs![glob_object]
		|  fi_properties bitand FI_HasTypeSpec <> 0
			# (_, symbol_type) = removeAnnotations symbol_type
			= (symbol_type, fun_defs, fun_heap)
			= (symbol_type, fun_defs, fun_heap)
		# {ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
		  (_, ft_type=:{st_args,st_args_strictness}) = removeAnnotations ft_type
		  new_st_args = addTypesOfDictionaries ro.ro_common_defs ft_type.st_context st_args
		  new_st_arity = length new_st_args
		  new_st_args_strictness = insert_n_strictness_values_at_beginning (new_st_arity-length st_args) st_args_strictness
		= ({ft_type & st_args = new_st_args, st_args_strictness = new_st_args_strictness, st_arity = new_st_arity, st_context = [] }, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_LocalMacroFunction glob_object} ro fun_defs fun_heap
	# ({fun_type=Yes symbol_type}, fun_defs) = fun_defs![glob_object]
	= (symbol_type, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_GeneratedFunction fun_ptr _} ro fun_defs fun_heap
	# (FI_Function {gf_fun_def={fun_type=Yes symbol_type}}, fun_heap) = readPtr fun_ptr fun_heap
	= (symbol_type, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_Constructor {glob_module, glob_object}} ro fun_defs fun_heap
	# cons_defs = ro.ro_common_defs.[glob_module].com_cons_defs
	# {cons_type} = cons_defs.[glob_object]
	# (_,cons_type) = removeAnnotations cons_type	// necessary???
	= (cons_type, fun_defs, fun_heap)

//@ determine_args
determine_args
	:: ![Bool] ![ConsClass] !Index !{!Producer} ![Optional SymbolType] ![FreeVar] !ReadOnlyTI !*DetermineArgsState
	-> *DetermineArgsState
determine_args _ [] prod_index producers prod_atypes forms _ das=:{das_var_heap}
	# (vars, das_var_heap)	= new_variables forms das_var_heap
	= {das & das_vars = vars, das_var_heap = das_var_heap}
where
	new_variables [] var_heap
		= ([], var_heap)
	new_variables [form=:{fv_ident,fv_info_ptr}:forms] var_heap
		# (vars, var_heap) = new_variables forms var_heap
		  (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
		= ([{ form & fv_info_ptr = new_info_ptr } : vars], writeVarInfo fv_info_ptr (VI_Variable fv_ident new_info_ptr) var_heap)

determine_args [linear_bit : linear_bits] [cons_arg : cons_args] prod_index producers [prod_atype:prod_atypes]
				[form : forms] input das
	# das = determine_args linear_bits cons_args (inc prod_index) producers prod_atypes forms input das
//	# producer	= if (cons_arg == CActive) (producers.[prod_index]) PR_Empty
	# producer	= case cons_arg of
					CActive			-> producers.[prod_index]
					CUnusedStrict	-> producers.[prod_index]
					CUnusedLazy		-> producers.[prod_index]
					_				-> PR_Empty
	= determine_arg producer prod_atype form prod_index ((linear_bit,cons_arg), input) das

determine_arg
	:: !Producer .(Optional SymbolType) !FreeVar .Int !(!(!Bool,!ConsClass),!ReadOnlyTI) !*DetermineArgsState
	-> *DetermineArgsState

determine_arg PR_Empty _ form=:{fv_ident,fv_info_ptr} _ ((linear_bit,cons_arg), _) das=:{das_var_heap}
	# (new_info_ptr, das_var_heap)	= newPtr VI_Empty das_var_heap
	# das_var_heap					= writeVarInfo fv_info_ptr (VI_Variable fv_ident new_info_ptr) das_var_heap
	=	{ das 
		& das_vars				= [{ form & fv_info_ptr = new_info_ptr } : das.das_vars ]
		, das_new_linear_bits	= [ linear_bit : das.das_new_linear_bits ]
		, das_new_cons_args		= [ cons_arg : das.das_new_cons_args ]
		, das_var_heap			= das_var_heap
		}

determine_arg PR_Unused _ form prod_index (_,ro) das=:{das_var_heap}
	# no_arg_type				= { ats_types= [], ats_strictness = NotStrict }
	= {das & das_arg_types.[prod_index] = no_arg_type}

determine_arg (PR_Class class_app free_vars_and_types class_type) _ {fv_info_ptr} prod_index (_,ro)
			  das=:{das_arg_types, das_subst, das_type_heaps, das_predef}
	# (ws_arg_type, das_arg_types) = das_arg_types![prod_index]
	# {ats_types=[arg_type:_]} = ws_arg_type
	  (int_class_type, das_type_heaps)
	  		= substitute class_type das_type_heaps
	  class_atype = { empty_atype & at_type = int_class_type }
	  type_input
	  		= { ti_common_defs = ro.ro_common_defs
	  		  , ti_functions = ro.ro_imported_funs
			  ,	ti_main_dcl_module_n = ro.ro_main_dcl_module_n
			  , ti_expand_newtypes = True
			  }
	// AA: Dummy generic dictionary does not unify with corresponding class dictionary.
	// Make it unify
	# ({pds_module,pds_def},das_predef) = das_predef![PD_TypeGenericDict]
	# genericGlobalIndex	= {glob_module = pds_module, glob_object = pds_def}
	# (succ, das_subst, das_type_heaps)
		//AA: = unify class_atype arg_type type_input das_subst das_type_heaps
		= unify_dict class_atype arg_type type_input das_subst das_type_heaps
		with
			unify_dict class_atype=:{at_type=TA type_symb1 args1} arg_type=:{at_type=TA type_symb2 args2} 
				| type_symb1 == type_symb2 
					= unify class_atype arg_type
				// FIXME: check indexes, not names. Need predefs for that. 	
//				| type_symb1.type_ident.id_name == "GenericDict"
				| type_symb1.type_index == genericGlobalIndex
					= unify {class_atype & at_type = TA type_symb2 args1} arg_type	
//				| type_symb2.type_ident.id_name == "GenericDict"
				| type_symb2.type_index == genericGlobalIndex
					= unify class_atype {arg_type & at_type = TA type_symb1 args2} 	  				
			unify_dict class_atype arg_type 
				= unify class_atype arg_type
	| not succ
		= abort ("sanity check nr 93 in module trans failed\n"--->(class_atype,"\n", arg_type))
	# (free_vars_and_types,das_type_heaps) = mapSt subFVT free_vars_and_types das_type_heaps
		with
			subFVT (fv,ty) th
				# (ty`,th`)		= substitute ty th
				= ((fv,ty`),th`)

	# ws_ats_types = [ { empty_atype & at_type = at_type } \\ (_, at_type) <- free_vars_and_types]
	# ws_arg_type` = {ats_types= ws_ats_types, ats_strictness = first_n_strict (length free_vars_and_types) }

	= {das
		& das_vars = mapAppend (\({var_info_ptr,var_ident}, _)
						-> { fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0 })
				  			  free_vars_and_types das.das_vars
		, das_arg_types			= {das_arg_types & [prod_index] = ws_arg_type` }
		, das_new_linear_bits	= mapAppend (\_ -> True) free_vars_and_types das.das_new_linear_bits
		, das_new_cons_args		= mapAppend (\_ -> CActive) free_vars_and_types das.das_new_cons_args
		, das_subst				= das_subst
		, das_type_heaps		= das_type_heaps
		, das_var_heap			= writeVarInfo fv_info_ptr (VI_Dictionary class_app.app_symb class_app.app_args class_type) das.das_var_heap
		, das_predef			= das_predef
		}

determine_arg producer (Yes {st_args, st_args_strictness, st_result, st_attr_vars, st_context, st_attr_env, st_arity})
				{fv_info_ptr,fv_ident} prod_index ((linear_bit, _),ro)
				das=:{das_subst,das_type_heaps,das_fun_defs,das_fun_heap,das_var_heap,das_cons_args,das_arg_types,das_next_attr_nr,das_AVI_Attr_TA_TempVar_info_ptrs}

	# {th_vars, th_attrs}		= das_type_heaps
	# (symbol,symbol_arity)		= get_producer_symbol producer
	  curried					= case producer of
	  									PR_Curried _ _ -> True
	  									PR_CurriedFunction _ _ _  -> True
	  									_ -> False;
	#! size_fun_defs			= size das_fun_defs

	# ({cc_args, cc_linear_bits}, das_fun_heap, das_cons_args)
			= calc_cons_args curried symbol.symb_kind symbol_arity das_cons_args linear_bit size_fun_defs das_fun_heap
	  ({ats_types=[arg_type:_],ats_strictness}, das_arg_types)
	  		= das_arg_types![prod_index]

	  (das_next_attr_nr, th_attrs)
	  		= bind_to_temp_attr_vars st_attr_vars (das_next_attr_nr, th_attrs)
	  // remember the st_attr_vars, because the AVI_Attr (TA_TempVar _)'s must be removed before unfold,
	  // because types in Cases and Lets should not use TA_TempVar's
	  das_AVI_Attr_TA_TempVar_info_ptrs = [st_attr_vars:das_AVI_Attr_TA_TempVar_info_ptrs]
	  		// prepare for substitute calls
	  ((st_args, st_result), das_type_heaps)
	  		= substitute (st_args, st_result) { das_type_heaps & th_vars = th_vars, th_attrs = th_attrs }
	  nr_of_applied_args = symbol_arity
	  (application_type, attr_env, das_next_attr_nr)
	  		= build_application_type st_arity (length st_context) st_result st_args nr_of_applied_args [] das_next_attr_nr
	  type_input
	  		= { ti_common_defs			= ro.ro_common_defs
	  		  , ti_functions			= ro.ro_imported_funs
			  ,	ti_main_dcl_module_n	= ro.ro_main_dcl_module_n
			  , ti_expand_newtypes = True
			  }
	# (succ, das_subst, das_type_heaps)
	  		= unify application_type arg_type type_input das_subst das_type_heaps
	| not succ
		= abort "Error in compiler: unification in module trans failed\n"
	# (attr_inequalities, das_type_heaps)
			= accAttrVarHeap (mapSt substitute_attr_inequality st_attr_env) das_type_heaps
	  new_uniqueness_requirement
	  		= { ur_offered		= application_type
	  		  , ur_demanded		= arg_type
//	  		  , ur_attr_ineqs	= attr_inequalities 
	  		  , ur_attr_ineqs	= attr_inequalities ++ attr_env
	  		  }
	  (expr_to_unfold,form_vars,das_fun_defs,das_fun_heap,das_var_heap)
		= make_producer_expression_and_args producer das.das_vars das_fun_defs das_fun_heap das_var_heap
/* DvA... STRICT_LET
	  (expr_to_unfold, das_var_heap, let_bindings)
			= case arg_type.at_annotation of
				AN_Strict
					# (new_info_ptr_l, das_var_heap) = newPtr VI_Empty das_var_heap
	  				# free_var_l = { fv_ident = { id_name = "free_l", id_info = nilPtr }, fv_info_ptr = new_info_ptr_l, fv_count = 0, fv_def_level = NotALevel }
			  		# act_var_l = Var { var_ident = { id_name = "act_l", id_info = nilPtr }, var_info_ptr = new_info_ptr_l, var_expr_ptr = nilPtr }

					# bind = {lb_dst = fv, lb_src = act_var_l, lb_position = NoPos}

					# das_var_heap = writeVarInfo new_info_ptr_l expr_to_unfold das_var_heap

					# let_bindings = case let_bindings of
									(s,l,st,lt) -> ([bind:s],l,[arg_type:st],lt)
					-> (VI_Empty, das_var_heap, let_bindings)
				_	-> (expr_to_unfold,das_var_heap,let_bindings)
...DvA */
	# das_arg_types = { das_arg_types & [prod_index] = {ats_types=take nr_of_applied_args st_args,ats_strictness=st_args_strictness} }
	=	{ das 
		& das_vars						= form_vars
		, das_arg_types					= das_arg_types 
		, das_next_attr_nr				= das_next_attr_nr
		, das_new_linear_bits			= cc_linear_bits ++ das.das_new_linear_bits
		, das_new_cons_args				= cc_args ++ das.das_new_cons_args
		, das_uniqueness_requirements	= [new_uniqueness_requirement:das.das_uniqueness_requirements]
		, das_AVI_Attr_TA_TempVar_info_ptrs = das_AVI_Attr_TA_TempVar_info_ptrs
		, das_subst						= das_subst
		, das_type_heaps				= das_type_heaps
		, das_fun_defs					= das_fun_defs
		, das_fun_heap					= das_fun_heap
		, das_var_heap					= writeVarInfo fv_info_ptr expr_to_unfold das_var_heap
		, das_cons_args					= das_cons_args
		}
where
	make_producer_expression_and_args (PR_Constructor symbol=:{symb_kind=SK_Constructor {glob_module}} arity _) das_vars das_fun_defs das_fun_heap das_var_heap
		# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
		= (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Curried symbol=:{symb_kind=SK_Function {glob_module}} arity) das_vars das_fun_defs das_fun_heap das_var_heap
		| glob_module <> ro.ro_main_dcl_module_n
			# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
			= (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Curried symbol=:{symb_kind} arity) das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
			_
				# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
				-> (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Function symbol=:{symb_kind} arity _) das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Body symbol tb (take arity form_vars), form_vars, das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_GeneratedFunction symbol=:{symb_kind} arity _) das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Body symbol tb (take arity form_vars), form_vars, das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_CurriedFunction symbol=:{symb_kind} arity _) das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				  expr = App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}
				-> (VI_ExpressionOrBody expr symbol tb (take arity form_vars), form_vars, das_fun_defs,das_fun_heap,das_var_heap)

	build_n_anonymous_var_args arity das_vars das_var_heap
 		# var_names = repeatn arity {id_name = "_x", id_info = nilPtr}
		= build_var_args (/*reverse*/ var_names) das_vars [] das_var_heap

	build_n_named_var_args arity tb_args das_vars das_var_heap
		# var_names = take arity [fv_ident \\ {fv_ident}<-tb_args]
		= build_var_args (reverse var_names) das_vars [] das_var_heap
		
	build_var_args [] form_vars act_vars var_heap
		= (form_vars, act_vars, var_heap)
	build_var_args [new_name:new_names] form_vars act_vars var_heap
		# (info_ptr, var_heap) = newPtr VI_Empty var_heap
		  form_var = { fv_ident = new_name, fv_info_ptr = info_ptr, fv_count = 0, fv_def_level = NotALevel }
		  act_var = { var_ident = new_name, var_info_ptr = info_ptr, var_expr_ptr = nilPtr }
		= build_var_args new_names [form_var : form_vars] [Var act_var : act_vars] var_heap

	calc_cons_args curried symb_kind symbol_arity ti_cons_args linear_bit size_fun_defs fun_heap
		# (cons_size, ti_cons_args) = usize ti_cons_args
		# (opt_cons_classes, fun_heap, ti_cons_args)
				= case symb_kind of
					SK_Function {glob_module, glob_object}
						| glob_module == ro.ro_main_dcl_module_n && glob_object < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![glob_object]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						-> (No, fun_heap, ti_cons_args)
					SK_LocalMacroFunction glob_object
						| glob_object < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![glob_object]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						-> (No, fun_heap, ti_cons_args)
					SK_GeneratedFunction fun_ptr fun_index
						| fun_index < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![fun_index]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						| fun_index < size_fun_defs
							-> abort "sanity check failed in module trans"
						# (FI_Function {gf_cons_args}, fun_heap) = readPtr fun_ptr fun_heap
						-> (Yes gf_cons_args, fun_heap, ti_cons_args)
					SK_Constructor _
						-> (No, fun_heap, ti_cons_args)
		= case opt_cons_classes of
			Yes cons_classes
				# cc_args = copy_classes symbol_arity cons_classes.cc_args
				-> ({ cc_size			= symbol_arity
					, cc_args			= cc_args
					, cc_linear_bits	= if curried
											(repeatn symbol_arity linear_bit)
											(take symbol_arity cons_classes.cc_linear_bits)
					, cc_producer		= False
					}
		  			, fun_heap, ti_cons_args)
			No
				-> ({ cc_size			= symbol_arity
					, cc_args			= repeatn symbol_arity CPassive
					, cc_linear_bits	= repeatn symbol_arity linear_bit
					, cc_producer		= False
					}
					, fun_heap, ti_cons_args)

	copy_classes 0 _ = []
	copy_classes n [cc:ccs]
		= case cc of
			CUnusedStrict	-> [CActive:copy_classes (dec n) ccs]
			CUnusedLazy		-> [CActive:copy_classes (dec n) ccs]
			cc				-> [cc:copy_classes (dec n) ccs]

/*
	build_application_type st_arity nr_context_args st_result st_args nr_of_applied_args
		| st_arity+nr_context_args==nr_of_applied_args
			= st_result
		| nr_of_applied_args<nr_context_args
			= abort "sanity check nr 234 failed in module trans"
		# (applied_args, unapplied_args) = splitAt (nr_of_applied_args-nr_context_args) st_args
		  attr_approx = if (any has_unique_attribute applied_args) TA_Unique TA_Multi
		= foldr (\atype1 atype2->{at_attribute=attr_approx, at_type=atype1-->atype2})
				st_result unapplied_args
	  where
		has_unique_attribute {at_attribute=TA_Unique} = True
		has_unique_attribute _ = False
*/
	build_application_type st_arity nr_context_args st_result st_args nr_of_applied_args attr_env attr_store
		| st_arity+nr_context_args==nr_of_applied_args
			= (st_result, attr_env, attr_store)
		| nr_of_applied_args<nr_context_args
			= abort "sanity check nr 234 failed in module trans"
		# req_arity	= nr_of_applied_args - nr_context_args

		= currySymbolType st_args st_arity st_result attr_env req_arity attr_store
/*
		# (type`,attr_env`,attr_store`)
			= currySymbolType st_args st_arity st_result attr_env req_arity attr_store
		# (applied_args, unapplied_args) = splitAt req_arity st_args
		  attr_approx = if (any has_unique_attribute applied_args) TA_Unique TA_Multi			// DvA: should be var instead of multi...
		# type = foldr (\atype1 atype2->{at_attribute=attr_approx, at_type=atype1-->atype2})
				st_result unapplied_args
		| False ---> ("build",type,type`) = undef
//		= (type, attr_env, attr_store)
		= (type`, attr_env`, attr_store`)
	  where
		has_unique_attribute {at_attribute=TA_Unique} = True
		has_unique_attribute _ = False
*/

// DvA: from type.icl...
currySymbolType tst_args tst_arity tst_result tst_attr_env req_arity ts_attr_store
	| tst_arity == req_arity
		= (tst_result, tst_attr_env, ts_attr_store)
	# (tst_args, rest_args, is_unique)			= split_args req_arity tst_args 
	| is_unique
		# (type, _, _)							= buildCurriedType rest_args tst_result TA_Unique [] 0
		= (type, tst_attr_env, ts_attr_store)
		# tst_attr_env							= build_attr_env ts_attr_store tst_args tst_attr_env
		# (type, tst_attr_env, ts_attr_store)	= buildCurriedType rest_args tst_result (TA_TempVar ts_attr_store)
		  												tst_attr_env (inc ts_attr_store)
		= (type, tst_attr_env, ts_attr_store)
where
	split_args 0 args = ([], args, False)
	split_args n [atype=:{at_attribute} : args]
		# (left, right, is_unique) = split_args (dec n) args
		= ([ atype : left ], right, is_unique || attr_is_unique at_attribute)
	
	attr_is_unique TA_Unique = True
	attr_is_unique _ = False
	
	build_attr_env cum_attr_var [] attr_env
		= attr_env
	build_attr_env cum_attr_var [{at_attribute=(TA_TempVar attr_var)} : args] attr_env
		# attr_env = [{ ac_demanded = attr_var, ac_offered = cum_attr_var } : attr_env]
		= build_attr_env cum_attr_var args attr_env
	build_attr_env cum_attr_var [_ : args] attr_env
		= build_attr_env cum_attr_var args attr_env

buildCurriedType [] type cum_attr attr_env attr_store
	= (type, attr_env, attr_store)
buildCurriedType [at=:{at_attribute}:ats] type cum_attr attr_env attr_store
	# (next_cum_attr, attr_env, attr_store) = combine_attributes at_attribute cum_attr attr_env attr_store
	  (res_type, attr_env, attr_store) = buildCurriedType ats type next_cum_attr attr_env attr_store
	= ({at_attribute = cum_attr , at_type = at --> res_type }, attr_env, attr_store)
where
	combine_attributes TA_Unique cum_attr attr_env attr_store
		= (TA_Unique, attr_env, attr_store)
	combine_attributes (TA_TempVar attr_var) (TA_TempVar cum_attr_var) attr_env attr_store
		# attr_env =
			[{ ac_demanded = cum_attr_var,ac_offered = attr_store }
			,{ ac_demanded = attr_var,ac_offered = attr_store }
			:attr_env]
		= (TA_TempVar attr_store, attr_env, inc attr_store)
	combine_attributes (TA_TempVar _) cum_attr attr_env attr_store
		= (cum_attr, attr_env, attr_store)
	combine_attributes _ (TA_TempVar cum_attr_var) attr_env attr_store
		# attr_env = [{ ac_demanded = cum_attr_var,ac_offered = attr_store }:attr_env]
		= (TA_TempVar attr_store, attr_env, inc attr_store)
	combine_attributes _ cum_attr attr_env attr_store
		= (cum_attr, attr_env, attr_store)

freshAttrVar attr_var th_attrs
	# (new_info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
	= ({ av_ident = NewAttrVarId attr_var, av_info_ptr = new_info_ptr }, th_attrs)


//@ max_group_index

max_group_index
	:: !Int !{!Producer} Index Index *{#FunDef} *FunctionHeap *{!ConsClasses}
	-> (Index,*{!ConsClasses},*{#FunDef},*FunctionHeap)
max_group_index prod_index producers ro_main_dcl_module_n current_max fun_defs fun_heap cons_args
	| prod_index == size producers
		= (current_max, cons_args, fun_defs, fun_heap)
		# (current_max, cons_args, fun_defs, fun_heap)
			= max_group_index_of_producer producers.[prod_index] current_max fun_defs fun_heap cons_args
		= max_group_index (inc prod_index) producers ro_main_dcl_module_n current_max fun_defs fun_heap cons_args
where
	max_group_index_of_producer PR_Empty current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer PR_Unused current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Class {app_args} _ _) current_max fun_defs fun_heap cons_args
		= foldSt (foldrExprSt max_group_index_of_member) app_args (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried {symb_kind=SK_Function {glob_object=fun_index, glob_module}} _) current_max fun_defs fun_heap cons_args
		| glob_module<>ro_main_dcl_module_n
			= (current_max, cons_args, fun_defs, fun_heap)
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried {symb_kind=SK_LocalMacroFunction fun_index} _) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried { symb_kind = SK_GeneratedFunction fun_ptr fun_index} _) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Function _ _ fun_index) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_GeneratedFunction { symb_kind = SK_GeneratedFunction fun_ptr fun_index} _ _)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Constructor symb _ args) current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_CurriedFunction {symb_kind = SK_GeneratedFunction fun_ptr fun_index} _ _)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_CurriedFunction _ _ fun_index)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)

	max_group_index_of_member
				(App {app_symb = {symb_ident, symb_kind = SK_Function { glob_object = fun_index, glob_module = mod_index}}}) 
				(current_max, cons_args, fun_defs, fun_heap)
		| mod_index == ro_main_dcl_module_n
			# (size_args, cons_args) = usize cons_args
			| fun_index < size_args
				# ({fun_info = {fi_group_index}},fun_defs) = fun_defs![fun_index]
				= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
			= (current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member
				(App {app_symb = {symb_ident, symb_kind = SK_LocalMacroFunction fun_index}})
				(current_max, cons_args, fun_defs, fun_heap)
		# (size_args, cons_args) = usize cons_args
		| fun_index < size_args
			# ({fun_info = {fi_group_index}}, fun_defs) = fun_defs![fun_index]
			= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member
				(App {app_symb = {symb_kind = SK_GeneratedFunction fun_ptr _}})
				(current_max, cons_args, fun_defs, fun_heap)
		# (FI_Function {gf_fun_def={fun_info = {fi_group_index}}}, fun_heap) = readPtr fun_ptr fun_heap
		= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member _ (current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)

	max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		# (fun_def,fun_defs) = fun_defs![fun_index]
		= (max fun_def.fun_info.fi_group_index current_max, fun_defs)

	max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		# (fun_size, fun_defs)	= usize fun_defs
		| fun_index < fun_size
			# ({fun_info},fun_defs) = fun_defs![fun_index] 
			= (max fun_info.fi_group_index current_max, fun_defs, fun_heap)
			# (FI_Function generated_function, fun_heap) = readPtr fun_ptr fun_heap
			= (max generated_function.gf_fun_def.fun_info.fi_group_index current_max, fun_defs, fun_heap)

//@ replaceIntegers

class replaceIntegers a :: !a !({!TypeVar}, !{!TypeAttribute}, !AttributePartition) !*{#Bool} -> (!a, !.{#Bool})
	// get rid of all those TempV and TA_Var things

instance replaceIntegers (a, b) | replaceIntegers a & replaceIntegers b where
	replaceIntegers (a, b) input used
		# (a, used) = replaceIntegers a input used
		  (b, used) = replaceIntegers b input used
		= ((a, b), used)

instance replaceIntegers [a] | replaceIntegers a where
	replaceIntegers [] input used
		= ([], used)
	replaceIntegers [h:t] input used
		# (h, used) = replaceIntegers h input used
		  (t, used) = replaceIntegers t input used
		= ([h:t], used)

instance replaceIntegers TypeAttribute where
	replaceIntegers (TA_TempVar i) (_, attributes, attr_partition) used
		# index = attr_partition.[i]
		  attribute = attributes.[index]
		= case attribute of
			TA_Var _
				-> (attribute, {used & [index] = True})
			_
				-> (attribute, used)
	replaceIntegers ta _ used
		= (ta, used)

instance replaceIntegers Type where
	replaceIntegers (TA type_symb_ident args) input used
		# (args, used) = replaceIntegers args input used
		= (TA type_symb_ident args, used)
	replaceIntegers (TAS type_symb_ident args strictness) input used
		# (args, used) = replaceIntegers args input used
		= (TAS type_symb_ident args strictness, used)
	replaceIntegers (a --> b) input used
		# (a, used) = replaceIntegers a input used
		  (b, used) = replaceIntegers b input used
		= (a --> b, used)
	replaceIntegers (consvar :@: args) input=:(fresh_type_vars, _, _) used
		# (TempCV i) = consvar
		  (args, used) = replaceIntegers args input used
		= (CV fresh_type_vars.[i] :@: args, used)
	replaceIntegers (TempV i) (fresh_type_vars, _, _) used
		= (TV fresh_type_vars.[i], used)
	replaceIntegers type input used
		= (type, used)

instance replaceIntegers AType where
	replaceIntegers atype=:{at_attribute, at_type} input used
		# (at_attribute, used) = replaceIntegers at_attribute input used
		  (at_type, used) = replaceIntegers at_type input used
		= ({atype & at_attribute = at_attribute, at_type = at_type}, used)

// Variable binding...

bind_to_fresh_expr_var {fv_ident, fv_info_ptr} var_heap
	# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
	  form_var = { fv_ident = fv_ident, fv_info_ptr = new_info_ptr, fv_count = undeff, fv_def_level = NotALevel }
	  act_var = { var_ident = fv_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
	= (form_var, writeVarInfo fv_info_ptr (VI_Expression (Var act_var)) var_heap)

bind_to_fresh_type_variable {tv_ident, tv_info_ptr} th_vars
	# (new_tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
	  tv = {tv_ident=tv_ident, tv_info_ptr=new_tv_info_ptr}
	= (tv, writePtr tv_info_ptr (TVI_Type (TV tv)) th_vars)

bind_to_fresh_attr_variable {av_ident, av_info_ptr} th_attrs
	# (new_av_info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
	  av = { av_ident=av_ident, av_info_ptr=new_av_info_ptr }
	= (av, writePtr av_info_ptr (AVI_Attr (TA_Var av)) th_attrs)

bind_to_temp_type_var {tv_info_ptr} (next_type_var_nr, th_vars)
	= (next_type_var_nr+1, writePtr tv_info_ptr (TVI_Type (TempV next_type_var_nr)) th_vars)

bind_to_temp_attr_vars :: [AttributeVar] *(Int,*AttrVarHeap) -> (!Int,!*AttrVarHeap)
bind_to_temp_attr_vars attr_vars next_attr_var_n_and_attrs
	= foldSt bind_to_temp_attr_var attr_vars next_attr_var_n_and_attrs
where
	bind_to_temp_attr_var {av_info_ptr} (next_attr_var_nr, th_attrs)
		= (next_attr_var_nr+1, writePtr av_info_ptr (AVI_Attr (TA_TempVar next_attr_var_nr)) th_attrs)

remove_TA_TempVars_in_info_ptrs [hAVI_Attr_TA_TempVar_info_ptrs:tAVI_Attr_TA_TempVar_info_ptrs] attrs
	# attrs = remove_TA_TempVars_in_info_ptr_list hAVI_Attr_TA_TempVar_info_ptrs attrs
	= remove_TA_TempVars_in_info_ptrs tAVI_Attr_TA_TempVar_info_ptrs attrs
where
	remove_TA_TempVars_in_info_ptr_list [{av_info_ptr}:tAVI_Attr_TA_TempVar_info_ptrs] attrs
		= case readPtr av_info_ptr attrs of
			(AVI_Attr (TA_TempVar _),attrs)
				// use TA_Multi as in cleanUpTypeAttribute
				# attrs = writePtr av_info_ptr (AVI_Attr TA_Multi) attrs
				-> remove_TA_TempVars_in_info_ptr_list tAVI_Attr_TA_TempVar_info_ptrs attrs
			(_,attrs)
				-> remove_TA_TempVars_in_info_ptr_list tAVI_Attr_TA_TempVar_info_ptrs attrs
	remove_TA_TempVars_in_info_ptr_list [] attrs
		= attrs
remove_TA_TempVars_in_info_ptrs [] attrs
	= attrs

transformFunctionApplication :: !FunDef !InstanceInfo !ConsClasses !App ![Expression] !ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
transformFunctionApplication fun_def instances cc=:{cc_size, cc_args, cc_linear_bits} app=:{app_symb,app_args} extra_args ro ti
	# (app_args, extra_args) = complete_application fun_def.fun_arity app_args extra_args
//	| False -!-> ("transformFunctionApplication",app_symb,app_args,extra_args,fun_def.fun_arity,cc_size) = undef
	| expanding_consumer
	 	= (build_application { app & app_args = app_args } extra_args, ti)
	| cc_size == 0
		# {fun_body=fun_body=:TransformedBody {tb_rhs}, fun_kind} = fun_def
		| SwitchTransformConstants (ro.ro_transform_fusion && is_not_caf fun_kind && is_sexy_body tb_rhs) False
			= transform_trivial_function app app_args extra_args ro ti
		= (build_application { app & app_args = app_args } extra_args, ti)
	| cc_size >= 0
		# is_applied_to_macro_fun = fun_def.fun_info.fi_properties bitand FI_IsMacroFun <> 0
	  	# consumer_is_curried = cc_size <> length app_args
		# non_rec_consumer
			= (fun_def.fun_info.fi_properties bitand FI_IsNonRecursive) <> 0
		# safe_args
			= isEmpty [arg \\ arg <- app_args & cc_arg <- cc_args | unsafe cc_arg && non_var arg]
						with
							unsafe CAccumulating			= True
							unsafe CVarOfMultimatchCase		= True
							unsafe _						= False
							
							non_var (Var _)					= False
							non_var _						= True
	  	# ok_non_rec_consumer	= non_rec_consumer && safe_args
	  	#! (producers, new_args, strict_let_binds, ti)
	  		= determineProducers is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer fun_def.fun_type cc_linear_bits cc_args app_args 0 (createArray cc_size PR_Empty) ro ti
		#! (arity_changed,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,n_extra,ti)
			= determineCurriedProducersInExtraArgs new_args extra_args is_applied_to_macro_fun producers cc_args cc_linear_bits fun_def ro ti
	  	| containsProducer cc_size producers || arity_changed
	  		# (is_new, fun_def_ptr, instances, ti_fun_heap) = tryToFindInstance producers instances ti.ti_fun_heap
	  		| is_new
	  			# ti							= update_instance_info app_symb.symb_kind instances { ti & ti_fun_heap = ti_fun_heap }
	  			# (fun_index, fun_arity, ti)	= generateFunction app_symb fun_def cc_args cc_linear_bits producers fun_def_ptr ro n_extra ti
				| fun_index == (-1)
					= (build_application { app & app_args = app_args } extra_args, ti) // ---> ("failed instance")
	  			# app_symb = { app_symb & symb_kind = SK_GeneratedFunction fun_def_ptr fun_index }
				# (app_args, extra_args) = complete_application fun_arity new_args extra_args
	  			
//	  			# (FI_Function {gf_fun_def},ti_fun_heap) = readPtr fun_def_ptr ti.ti_fun_heap
//	  			# ti = {ti & ti_fun_heap = ti_fun_heap} ---> ("generated",fun_def_ptr,gf_fun_def)
	  			
	  			# (expr,ti) = transformApplication { app & app_symb = app_symb, app_args = app_args } extra_args ro ti
	  			= possiblyAddStrictLetBinds expr strict_let_binds ti
  			# (FI_Function {gf_fun_index, gf_fun_def}, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
			# ti = {ti & ti_fun_heap = ti_fun_heap}
			| gf_fun_index == (-1)
				= (build_application { app & app_args = app_args } extra_args, ti) // ---> ("known failed instance")
			# app_symb` = { app_symb & symb_kind = SK_GeneratedFunction fun_def_ptr gf_fun_index }
			  (app_args, extra_args) = complete_application gf_fun_def.fun_arity new_args extra_args
			  (expr,ti) = transformApplication {app & app_symb = app_symb`, app_args = app_args} extra_args ro ti
  			= possiblyAddStrictLetBinds expr strict_let_binds ti
		| SwitchTrivialFusion ro.ro_transform_fusion False
			= transform_trivial_function app app_args extra_args ro ti
		= (build_application { app & app_args = app_args } extra_args, ti)
	= (build_application { app & app_args = app_args } extra_args, ti)
where
	expanding_consumer = case fun_def.fun_body of
								Expanding _	-> True
								_			-> False

	is_not_caf FK_Caf	= False
	is_not_caf _		= True

	possiblyAddStrictLetBinds expr strict_lets ti
		# (strict_let_binds,let_type) = unzip strict_lets
		= case strict_let_binds of
			[]	-> (expr,ti)
			_
				# (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti.ti_symbol_heap
				  ti = {ti & ti_symbol_heap = ti_symbol_heap}
				-> (Let 	{	let_strict_binds	= strict_let_binds
							,	let_lazy_binds		= []
							,	let_expr			= expr
							,	let_info_ptr		= new_info_ptr
							,	let_expr_position	= NoPos
							},ti) // ---> "added strict_let_binds"

	transform_trivial_function :: !.App ![.Expression] ![.Expression] !.ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
	transform_trivial_function app=:{app_symb} app_args extra_args ro ti
		# (fun_def,ti_fun_defs,ti_fun_heap)		= get_fun_def app_symb.symb_kind ro.ro_main_dcl_module_n ti.ti_fun_defs ti.ti_fun_heap
		# {fun_body=fun_body=:TransformedBody {tb_args,tb_rhs},fun_type} = fun_def
		# (opt_expr, ti_fun_defs, ti_fun_heap, ti_type_heaps, ti_cons_args)
												= is_trivial_body tb_args tb_rhs app_args fun_type ro ti_fun_defs ti_fun_heap ti.ti_type_heaps ti.ti_cons_args
		# ti									= { ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_type_heaps = ti_type_heaps, ti_cons_args = ti_cons_args }
		= case opt_expr of
			No		
					-> (build_application { app & app_symb = app_symb, app_args = app_args } extra_args, ti)
			(Yes tb_rhs)
				| isEmpty extra_args
					-> (tb_rhs, ti)
					-> (tb_rhs @ extra_args, ti)

	update_instance_info :: !.SymbKind !.InstanceInfo !*TransformInfo -> *TransformInfo
	update_instance_info (SK_Function {glob_object}) instances ti=:{ti_instances}
		 = { ti & ti_instances = { ti_instances & [glob_object] = instances } }
	update_instance_info (SK_LocalMacroFunction glob_object) instances ti=:{ti_instances}
		 = { ti & ti_instances = { ti_instances & [glob_object] = instances } }
	update_instance_info (SK_GeneratedFunction fun_def_ptr fun_index) instances ti=:{ti_fun_heap, ti_instances}
		| fun_index < size ti_instances
			= { ti & ti_instances = { ti_instances & [fun_index] = instances } }
		# (FI_Function fun_info, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
		= { ti & ti_fun_heap = ti_fun_heap <:= (fun_def_ptr, FI_Function { fun_info & gf_instance_info = instances })}

	complete_application :: !.Int !.[Expression] !.[Expression] -> (!.[Expression],![Expression])
	complete_application form_arity args extra_args
		= (take form_arity all_args,drop form_arity all_args)
	where
		all_args = args ++ extra_args

	build_application :: !.App ![.Expression] -> Expression
	build_application app []
		= App app
	build_application app extra_args
		= App app @ extra_args
		
is_cons_or_decons_of_UList_or_UTSList glob_object glob_module imported_funs
	:== let  type = imported_funs.[glob_module].[glob_object].ft_type;
		  in type.st_arity>0 && not (isEmpty type.st_context);

determineCurriedProducersInExtraArgs :: ![Expression] ![Expression] !Bool !{!.Producer} ![Int] ![Bool] !FunDef !ReadOnlyTI !*TransformInfo -> *(!Bool,![Expression],![Expression],!{!Producer},![Int],![Bool],!FunDef,!Int,!*TransformInfo)
determineCurriedProducersInExtraArgs new_args [] is_applied_to_macro_fun producers cc_args cc_linear_bits fun_def ro ti
	= (False,new_args,[],producers,cc_args,cc_linear_bits,fun_def,0,ti)
determineCurriedProducersInExtraArgs new_args extra_args is_applied_to_macro_fun producers cc_args cc_linear_bits fun_def ro ti
	| not (SwitchExtraCurriedFusion ro.ro_transform_fusion is_applied_to_macro_fun)
		= (False,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,0,ti)
	# n_extra_args													= length extra_args
	# {fun_type = Yes symbol_type=:{st_args,st_result,st_arity}}	= fun_def
	# (ok,new_args_types,new_result_type)							= get_new_args_types_from_result_type st_result n_extra_args
	| not ok
		= (False,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,0,ti)
	# symbol_type	= {symbol_type & st_result=new_result_type,st_args=st_args++new_args_types,st_arity=st_arity+n_extra_args}
	# fun_def							= {fun_def & fun_type=Yes symbol_type}
	# (form_args,var_heap)				= create_new_args n_extra_args ti.ti_var_heap
	# ti								= {ti & ti_var_heap=var_heap}
	# fun_def							= case fun_def.fun_body of
											TransformedBody tb
												-> {fun_def & fun_body=TransformedBody 
													{tb & tb_args = add_args_to_fun_args form_args tb.tb_args
													}}
	# new_producers						= arrayPlusList producers [PR_Empty \\ i<-[0..n_extra_args-1]]
	# new_cc_args						= cc_args ++ [CPassive \\ i<-[0..n_extra_args-1]]
	# new_cc_linear_bits				= cc_linear_bits ++ [True \\ i<-[0..n_extra_args-1]]
	= (True,new_args++extra_args,[],new_producers,new_cc_args,new_cc_linear_bits,fun_def,n_extra_args,ti)
where
	get_new_args_types_from_result_type type 0
		= (True,[],type)
	get_new_args_types_from_result_type {at_type=a-->b} n
		# (ok,args_types,result_type) = get_new_args_types_from_result_type b (n-1)
		= (ok,[a:args_types],result_type)
	get_new_args_types_from_result_type type _
		= (False,[],type)

	create_new_args n_new_args var_heap
		| n_new_args==0
			= ([], var_heap)            
		# new_name				= { id_name = "_a", id_info = nilPtr }
		  (info_ptr, var_heap)	= newPtr VI_Empty var_heap
		  form_var				= { fv_ident = new_name, fv_info_ptr = info_ptr, fv_count = 0, fv_def_level = NotALevel }
		  (form_vars,var_heap)	= create_new_args (n_new_args-1) var_heap
		= ([form_var : form_vars],var_heap)

add_args_to_fun_args form_args tb_args
	= tb_args ++ form_args

add_args_to_fun_body act_args new_result_type tb_rhs ro ti
		= add_arguments tb_rhs act_args new_result_type ro ti
where
	add_arguments (App app=:{app_symb,app_args}) extra_args new_result_type ro ti
		# (form_arity,fun_defs,fun_heap) = get_arity app_symb ro ti.ti_fun_defs ti.ti_fun_heap
		# ti = {ti & ti_fun_defs=fun_defs,ti_fun_heap=fun_heap}
		# ar_diff = form_arity - length app_args
		| length extra_args <= ar_diff
			= (App {app & app_args = app_args ++ extra_args }, ti)
			= (App {app & app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
	add_arguments (Case kees=:{case_guards,case_default,case_info_ptr}) extra_args new_result_type ro ti
		# (case_default, ti)	= add_arguments_opt case_default extra_args new_result_type ro ti
		# (case_guards, ti)		= add_arguments_guards case_guards extra_args new_result_type ro ti
		# ti_symbol_heap		= overwrite_result_type case_info_ptr new_result_type ti.ti_symbol_heap
		# ti					= {ti & ti_symbol_heap = ti_symbol_heap}
		= (Case {kees & case_guards = case_guards, case_default = case_default}, ti)
	where
		overwrite_result_type case_info_ptr new_result_type ti_symbol_heap
			#! (EI_CaseType case_type, ti_symbol_heap)	= readExprInfo case_info_ptr ti_symbol_heap
			= writeExprInfo case_info_ptr (EI_CaseType { case_type & ct_result_type = new_result_type}) ti_symbol_heap
	add_arguments (Let lad=:{let_expr}) extra_args new_result_type ro ti
		# (let_expr, ti)		= add_arguments let_expr extra_args new_result_type ro ti
		= (Let {lad & let_expr = let_expr}, ti)
	add_arguments (expr1 @ expr2) extra_args _ ro ti
		= (expr1 @ (expr2++extra_args),ti)
	add_arguments expr extra_args _ ro ti
		= (expr @ extra_args,ti) // ---> ("????",expr)
	
	add_arguments_opt No _ _ ro ti = (No,ti)
	add_arguments_opt (Yes expr) extra_args new_result_type ro ti
		# (expr, ti)	= add_arguments expr extra_args new_result_type ro ti
		= (Yes expr,ti)
	
	add_arguments_guards (AlgebraicPatterns gindex apats) extra_args new_result_type ro ti
		# (apats, ti)	= add_arguments_apats apats extra_args new_result_type ro ti
		= (AlgebraicPatterns gindex apats, ti)
	add_arguments_guards (BasicPatterns btype bpats) extra_args new_result_type ro ti
		# (bpats, ti)	= add_arguments_bpats bpats extra_args new_result_type ro ti
		= (BasicPatterns btype bpats, ti)
	add_arguments_guards (DynamicPatterns dpats) extra_args new_result_type ro ti
		# (dpats, ti)	= add_arguments_dpats dpats extra_args new_result_type ro ti
		= (DynamicPatterns dpats, ti)
	add_arguments_guards (OverloadedListPatterns type decons_expr apats) extra_args new_result_type ro ti
		# (apats, ti)	= add_arguments_apats apats extra_args new_result_type ro ti
		= (OverloadedListPatterns type decons_expr apats, ti)
	add_arguments_guards NoPattern extra_args _ ro ti
		= (NoPattern, ti)
	
	add_arguments_apats [] extra_args _ ro ti = ([],ti)
	add_arguments_apats [ap=:{ap_expr}:aps] extra_args new_result_type ro ti
		# (ap_expr, ti)	= add_arguments ap_expr extra_args new_result_type ro ti
		# (aps, ti)		= add_arguments_apats aps extra_args new_result_type ro ti
		= ([{ap & ap_expr = ap_expr}:aps],ti)

	add_arguments_bpats [] extra_args _ ro ti = ([],ti)
	add_arguments_bpats [bp=:{bp_expr}:bps] extra_args new_result_type ro ti
		# (bp_expr, ti)	= add_arguments bp_expr extra_args new_result_type ro ti
		# (bps, ti)		= add_arguments_bpats bps extra_args new_result_type ro ti
		= ([{bp & bp_expr = bp_expr}:bps],ti)

	add_arguments_dpats [] extra_args _ ro ti = ([],ti)
	add_arguments_dpats [dp=:{dp_rhs}:dps] extra_args new_result_type ro ti
		# (dp_rhs, ti)	= add_arguments dp_rhs extra_args new_result_type ro ti
		# (dps, ti)	= add_arguments_dpats dps extra_args new_result_type ro ti
		= ([{dp & dp_rhs = dp_rhs}:dps],ti)

	get_arity {symb_kind=SK_Function {glob_module, glob_object}} ro fun_defs fun_heap
		| glob_module == ro.ro_main_dcl_module_n
			# (fun_arity, fun_defs) = fun_defs![glob_object].fun_arity
			= (fun_arity, fun_defs, fun_heap)
		# {ft_arity,ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
		= (ft_arity + length ft_type.st_context, fun_defs, fun_heap)
	get_arity {symb_kind=SK_LocalMacroFunction glob_object} ro fun_defs fun_heap
		# (fun_arity, fun_defs) = fun_defs![glob_object].fun_arity
		= (fun_arity, fun_defs, fun_heap)
	get_arity {symb_kind=SK_GeneratedFunction fun_ptr _} ro fun_defs fun_heap
		# (FI_Function {gf_fun_def={fun_arity}}, fun_heap) = readPtr fun_ptr fun_heap
		= (fun_arity, fun_defs, fun_heap)
	get_arity {symb_kind=SK_Constructor {glob_module, glob_object}} ro fun_defs fun_heap
		# arity = ro.ro_common_defs.[glob_module].com_cons_defs.[glob_object].cons_type.st_arity
		= (arity, fun_defs, fun_heap)

//@ is_trivial_body

:: *MatchState =
	{ tvar_map			:: ![(TypeVar,TypeVar)]
	, ms_type_heaps		:: !*TypeHeaps
	, ms_common_defs	:: !{# CommonDefs}
	}

is_trivial_body :: ![FreeVar] !Expression ![Expression] !(Optional SymbolType) !.ReadOnlyTI
							 !*{#FunDef} !*FunctionHeap !*TypeHeaps !*{!ConsClasses}
	-> (!Optional Expression,!*{#FunDef},!*FunctionHeap,!*TypeHeaps,!*{!ConsClasses})
is_trivial_body [fv] (Var bv) [arg] type ro fun_defs fun_heap type_heaps cons_args
	= if (fv.fv_info_ptr == bv.var_info_ptr)
			(Yes arg, fun_defs, fun_heap, type_heaps, cons_args)
			(No, fun_defs, fun_heap, type_heaps , cons_args)
is_trivial_body args (App app) f_args type ro fun_defs fun_heap type_heaps cons_args
	| not (is_safe_producer app.app_symb.symb_kind ro fun_heap cons_args)
		= (No,fun_defs,fun_heap,type_heaps,cons_args)
	# (type`,fun_defs,fun_heap)	= get_producer_type app.app_symb ro fun_defs fun_heap
	# match = match_args (length f_args) info args app.app_args []
	= case match of
		Yes perm
			# (match, type_heaps) = match_types type type` perm ro.ro_common_defs type_heaps
			| match
				# f_args = permute_args f_args (take (length f_args) perm)
				-> (Yes (App {app & app_args = f_args}),fun_defs,fun_heap,type_heaps,cons_args)
				-> (No,fun_defs,fun_heap,type_heaps,cons_args)
		_	-> (No,fun_defs,fun_heap,type_heaps,cons_args)
where
	info :: {!VarInfoPtr}
	info = {v.fv_info_ptr \\ v <- args}
	
	match_args 0 _ [] [] accu
		= Yes (reverse accu)
	match_args 0 info [fv:fvs] [Var bv:bvs] accu
		| fv.fv_info_ptr == bv.var_info_ptr 
			# index = lookup bv.var_info_ptr info
			= match_args 0 info fvs bvs [index:accu]
			= No
	match_args n info [fv:fvs] [Var bv:bvs] accu
		# index = lookup bv.var_info_ptr info
		= match_args (dec n) info fvs bvs [index:accu]
	match_args _ _ _ _ _ = No
	
	lookup x d = lookup 0 x d
	where
		lookup i x d
			| d.[i] == x
				= i
				= lookup (inc i) x d
	
	permute_args args perm = [args!!p \\ p <- perm]
		
	match_types type type` perm common_defs type_heaps
		| not_ok_perm perm
			= (False,type_heaps)
		= case type of
			No			-> (True,type_heaps)
			Yes type	-> match_types type type` perm common_defs type_heaps
	where
		not_ok_perm perm = length perm <> size info
		
		match_types type type` perm common_defs type_heaps
			| not (match_strictness` (dec type.st_arity) type.st_args_strictness type`.st_args_strictness perm)
				= (False,type_heaps)
			= (True,type_heaps)
/*			# (ok,args,res)	= make_args (type`.st_arity) type.st_args type.st_result
			| not ok = (False,type_heaps)
			# args` = permute_args args perm
			# ms = {tvar_map=[], ms_type_heaps = type_heaps,ms_common_defs=common_defs}
			# (match_ok,ms)	= match_arg_types args type`.st_args ms
			| not match_ok = (False,ms.ms_type_heaps)
			# (match_ok,ms)	= match_res_type res type`.st_result ms
			| not match_ok = (False,ms.ms_type_heaps)
			| type.st_context <> [] || type`.st_context <> []
				= (False,ms.ms_type_heaps)
			= (True,ms.ms_type_heaps)

		where
			make_args n as r
				# l = length as
				| n < l		= (False,as,r)
				| n == l	= (True,as,r)
				= move_args (n-l) as r []
			move_args 0 as r accu	= (True,as++(reverse accu),r)
			move_args n as {at_type = a-->r} accu = move_args (dec n) as r [a:accu]
			move_args _ as r accu = (False,as,r)
*/
		match_strictness` i s1 s2 p
			| i < 0 = True
			= arg_is_strict (p!!i) s1 == arg_is_strict i s2 && match_strictness (dec i) s1 s2

	match_strictness i s1 s2
		| i < 0 = True
		= arg_is_strict i s1 == arg_is_strict i s2 && match_strictness (dec i) s1 s2
	
	match_arg_types [] [] ms
		= (True,ms)
	match_arg_types [arg:args] [arg`:args`] ms
		# (type_ok,ms)	= match_type arg.at_type arg.at_attribute arg`.at_type arg`.at_attribute ms
		| not type_ok = (False,ms)
		= match_arg_types args args` ms
	match_arg_types _ _ ms
		= (False,ms)
	
	match_res_type res res` ms
		= match_type res.at_type res.at_attribute res`.at_type res`.at_attribute ms

	match_type (TA tsid types) _ (TA tsid` types`) _ ms
		| tsid == tsid`
			= match_arg_types types types` ms
	match_type (TAS tsid types strictl) _ (TAS tsid` types` strictl`) _ ms
		| tsid == tsid`
			| not (match_strictness (dec (length types)) strictl strictl`) = (False,ms)
			= match_arg_types types types` ms
	match_type (arg --> res) _ (arg` --> res`) _ ms
		# (type_ok,ms)	= match_type arg.at_type arg.at_attribute arg`.at_type arg`.at_attribute ms
		| not type_ok = (False,ms)
		= match_type res.at_type res.at_attribute res`.at_type res`.at_attribute ms
	match_type (TB bt) _ (TB bt`) _ ms
		= (bt==bt`,ms)
	match_type (TV tv) _ (TV tv`) _ ms
		= match_tvar tv tv` ms
	match_type t1 a1 t2 a2 ms
		# type_heaps	= ms.ms_type_heaps
		# (succ1,t1,type_heaps)	= tryToExpand t1 a1 ms.ms_common_defs type_heaps
		# (succ2,t2,type_heaps)	= tryToExpand t2 a2 ms.ms_common_defs type_heaps
		# ms = { ms & ms_type_heaps = type_heaps }
		| succ1 || succ2 = match_type t1 a1 t2 a2 ms
		= (False,ms)

	match_tvar x y ms
		# (r,tvar_map)	= match_tvar x y ms.tvar_map
		= (r, {ms & tvar_map = tvar_map})
	where
		match_tvar x y [] = (True,[(x,y)])
		match_tvar x y ms=:[(x`,y`):t]
			| x == x`	= (y==y`, ms)
			# (res,t)	= match_tvar x y t
			= (res,[(x`,y`):t])
is_trivial_body args rhs f_args type ro fun_defs fun_heap type_heaps cons_args
	= (No,fun_defs,fun_heap,type_heaps,cons_args)

is_safe_producer (SK_GeneratedFunction fun_ptr _) ro fun_heap cons_args
	# (FI_Function {gf_cons_args={cc_producer}}) = sreadPtr fun_ptr fun_heap
	= cc_producer
is_safe_producer (SK_LocalMacroFunction glob_object) ro fun_heap cons_args
	= cons_args.[glob_object].cc_producer
is_safe_producer (SK_Function {glob_module, glob_object}) ro fun_heap cons_args
	# max_index = size cons_args
	| glob_module <> ro.ro_main_dcl_module_n || glob_object >= max_index
		= False
		= cons_args.[glob_object].cc_producer
is_safe_producer (SK_Constructor {glob_module}) ro fun_heap cons_args
	= SwitchConstructorFusion True (glob_module==ro.ro_StdGeneric_module_n) False

//@ transformApplication
transformApplication :: !App ![Expression] !ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
transformApplication app=:{app_symb=symb=:{symb_kind}, app_args} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs}
	| is_SK_Function_or_SK_LocalMacroFunction symb_kind // otherwise GOTO next alternative	
		# gi
			= case symb_kind of
				SK_Function global_index -> global_index
				SK_LocalMacroFunction index -> { glob_module = ro.ro_main_dcl_module_n, glob_object = index }
		# { glob_module, glob_object } = gi
		| glob_module == ro.ro_main_dcl_module_n
			| glob_object < size ti_cons_args
				#  (cons_class,ti_cons_args) = ti_cons_args![glob_object]
				   (instances, ti_instances) = ti_instances![glob_object]
				   (fun_def, ti_fun_defs) = ti_fun_defs![glob_object]
				   ti = { ti & ti_instances = ti_instances, ti_fun_defs = ti_fun_defs, ti_cons_args = ti_cons_args }
				= transformFunctionApplication fun_def instances cons_class app extra_args ro ti
			// It seems as if we have an array function 
				| isEmpty extra_args
					= (App app, ti)
					= (App { app & app_args = app_args ++ extra_args}, ti)

		| glob_module==ro.ro_StdStrictLists_module_n && is_cons_or_decons_of_UList_or_UTSList glob_object glob_module ro.ro_imported_funs && (not (isEmpty app_args))
//			&& True ---> ("transformApplication "+++toString symb.symb_ident)
			# {ft_type} = ro.ro_imported_funs.[glob_module].[glob_object] // type of cons instance of instance List [#] a | U(TS)List a
			# [{tc_class=TCClass {glob_module,glob_object={ds_index}}}:_] = ft_type.st_context			
			# member_n=find_member_n 0 symb.symb_ident.id_name ro.ro_common_defs.[glob_module].com_class_defs.[ds_index].class_members
			# cons_u_member_index=ro.ro_common_defs.[glob_module].com_class_defs.[ds_index].class_members.[member_n].ds_index
			# {me_ident,me_offset}=ro.ro_common_defs.[glob_module].com_member_defs.[cons_u_member_index]
			# select_symb= {glob_module=glob_module,glob_object={ds_ident=me_ident,ds_index=cons_u_member_index,ds_arity=1}}
			# [first_arg:other_app_args] = app_args;
			# args=other_app_args++extra_args
			| isEmpty args
				= select_member first_arg select_symb me_offset ti
				# (expr,ti) = select_member first_arg select_symb me_offset ti
				= case expr of
					App app
						-> transformApplication app args ro ti
					_
						-> (expr @ args,ti)
		// This function is imported
		| SwitchSpecialFusion
				(not (isEmpty app_args) )
				False
			// Check imported overloaded function application for specials...
			# {ft_specials}						= ro.ro_imported_funs.[glob_module].[glob_object]
			# specials							= case ft_specials of
													FSP_ContextTypes s	-> s
													_ -> []
			| not (isEmpty specials)
				# (ei,ti_symbol_heap)			= mapSt readAppInfo app_args ti.ti_symbol_heap
					with
						readAppInfo :: !Expression !*ExpressionHeap -> (!ExprInfo,!*ExpressionHeap)
						readAppInfo (App {app_info_ptr}) heap
							| isNilPtr app_info_ptr
								= (EI_Empty,heap)
							= readPtr app_info_ptr heap
						readAppInfo _ heap = (EI_Empty,heap)
				# ti							= {ti & ti_symbol_heap = ti_symbol_heap}
				# context						= ro.ro_imported_funs.[glob_module].[glob_object].ft_type.st_context
				# insts							= resolveContext context ei
				# (num_special_args,special_gi)	= findInstInSpecials insts specials
				| foundSpecial special_gi
					= build_application {app & app_symb.symb_kind = SK_Function special_gi} (drop num_special_args app_args) extra_args special_gi ti
				= build_application app app_args extra_args gi ti
			= build_application app app_args extra_args gi ti
		= build_application app app_args extra_args gi ti
	where
		build_application :: !.App ![.Expression] ![.Expression] !(Global .Int) !*TransformInfo -> (!Expression,!*TransformInfo)
		build_application app app_args extra_args {glob_module,glob_object} ti
			| isEmpty extra_args
				= (App {app & app_args = app_args}, ti)
			# {ft_arity,ft_type}	= ro.ro_imported_funs.[glob_module].[glob_object]
			  form_arity			= ft_arity + length ft_type.st_context
			  ar_diff				= form_arity - length app_args
			  nr_of_extra_args		= length extra_args
			| nr_of_extra_args <= ar_diff
				= (App {app  &  app_args = app_args ++ extra_args }, ti)
				= (App {app  &  app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
/*		
		build_special_application app app_args extra_args {glob_module,glob_object} ro ti
			| isEmpty extra_args
				= (App {app & app_args = app_args}, ti)
			# {ft_arity,ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
			  form_arity = ft_arity + length ft_type.st_context
			  ar_diff = form_arity - length app_args
			  nr_of_extra_args = length extra_args
			| nr_of_extra_args <= ar_diff
				= (App {app  &  app_args = app_args ++ extra_args }, ti)
				= (App {app  &  app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
*/
		find_member_n :: !Int !String !{#.DefinedSymbol} -> Int
		find_member_n i member_string a
			| i<size a
				| a.[i].ds_ident.id_name % (0,size member_string-1)==member_string
					= i
					= find_member_n (i+1) member_string a

		select_member :: !.Expression !(Global .DefinedSymbol) !.Int !*TransformInfo -> *(!Expression,!*TransformInfo)
		select_member exp=:(App {app_symb={symb_kind=SK_Constructor _},app_args,app_info_ptr}) select_symb me_offset ti=:{ti_symbol_heap}
			| not (isNilPtr app_info_ptr)
				# (ei,ti_symbol_heap)	= readPtr app_info_ptr ti_symbol_heap
				# ti = {ti & ti_symbol_heap = ti_symbol_heap}
				= case ei of
					(EI_DictionaryType _)	-> (app_args !! me_offset,ti)
					_						-> (Selection NormalSelector exp [RecordSelection select_symb me_offset],ti)
		select_member exp select_symb me_offset ti
			= (Selection NormalSelector exp [RecordSelection select_symb me_offset],ti)

// XXX linear_bits field has to be added for generated functions
transformApplication app=:{app_symb={symb_ident,symb_kind = SK_GeneratedFunction fun_def_ptr fun_index}} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs,ti_fun_heap}
	| fun_index < size ti_cons_args
		#  (cons_class, ti_cons_args) = ti_cons_args![fun_index]
		   (instances, ti_instances) = ti_instances![fun_index]
		   (fun_def, ti_fun_defs) = ti_fun_defs![fun_index]
		   ti = { ti & ti_instances = ti_instances, ti_fun_defs = ti_fun_defs, ti_cons_args = ti_cons_args }
		= transformFunctionApplication fun_def instances cons_class app extra_args ro ti
	# (FI_Function {gf_fun_def,gf_instance_info,gf_cons_args}, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
	  ti = { ti & ti_fun_heap = ti_fun_heap }
	= transformFunctionApplication gf_fun_def gf_instance_info gf_cons_args app extra_args ro ti
transformApplication app [] ro ti
	= (App app, ti)
transformApplication app=:{app_symb={symb_ident,symb_kind = SK_Constructor cons_index},app_args} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs,ti_fun_heap}
	# {cons_type}			= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
	# (app_args,extra_args)	= complete_application cons_type.st_arity app_args extra_args
	= (build_application { app & app_args = app_args } extra_args, ti)
where
	complete_application :: !.Int ![Expression] ![Expression] -> (![Expression],![Expression])
	complete_application form_arity args []
		= (args, [])
	complete_application form_arity args extra_args
		# arity_diff = min (form_arity - length args) (length extra_args)
		= (args ++ take arity_diff extra_args, drop arity_diff extra_args)

	build_application :: !.App ![.Expression] -> Expression
	build_application app []
		= App app
	build_application app extra_args
		= App app @ extra_args
transformApplication app extra_args ro ti
	= (App app @ extra_args, ti)

transformSelection :: SelectorKind [Selection] Expression ReadOnlyTI *TransformInfo -> (!Expression,!*TransformInfo)
transformSelection NormalSelector s=:[RecordSelection _ field_index : selectors] 
					app=:(App appi=:{app_symb={symb_kind= SK_Constructor _ }, app_args, app_info_ptr})
					ro ti=:{ti_symbol_heap}
	| isNilPtr app_info_ptr
		// urgh: now reevaluates cnf for each nested strict selector :-(
		| cnf_app_args appi ro
			= transformSelection NormalSelector selectors (app_args !! field_index) ro ti
		= (Selection NormalSelector app s, ti)
	# (app_info, ti_symbol_heap) = readPtr app_info_ptr ti_symbol_heap
	  ti = { ti & ti_symbol_heap = ti_symbol_heap }
	= case app_info of
		EI_DictionaryType _
			-> transformSelection NormalSelector selectors (app_args !! field_index) ro ti
		_
			// urgh: now reevaluates cnf for each nested strict selector :-(
			| cnf_app_args appi ro
				-> transformSelection NormalSelector selectors (app_args !! field_index) ro ti
			-> (Selection NormalSelector app s, ti)
where
	cnf_args [] index strictness ro = True
	cnf_args [arg:args] index strictness ro
		| arg_is_strict index strictness
			= case arg of
				BasicExpr _	-> cnf_args args (inc index) strictness ro
				App app		-> cnf_app_args app ro
				_			-> False
		= cnf_args args (inc index) strictness ro
	
	cnf_app_args {app_symb=symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} ro
		# {cons_type}		= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
		= cnf_args app_args 0 cons_type.st_args_strictness ro
	cnf_app_args {app_symb=symb=:{symb_kind}, app_args} ro
		= False
transformSelection NormalSelector s=:[RecordSelection _ field_index : selectors] 
					app=:(App appi=:{app_symb=app_symb=:{symb_kind}, app_args, app_info_ptr})
					ro ti
	| isOKSymbol symb_kind && isEmpty app_args
		# (fun_def,ti_fun_defs,ti_fun_heap)		= get_fun_def symb_kind ro.ro_main_dcl_module_n ti.ti_fun_defs ti.ti_fun_heap
		# ti = {ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap}
		# {fun_body,fun_type,fun_kind}			= fun_def
		| is_not_caf fun_kind
			= case fun_body of
				TransformedBody {tb_rhs}	-> case tb_rhs of
					App app						-> transformSelection NormalSelector s tb_rhs ro ti
					_							-> (Selection NormalSelector app s, ti)
			= (Selection NormalSelector app s, ti)
where
	isOKSymbol (SK_Function {glob_module})	= glob_module == ro.ro_main_dcl_module_n
	isOKSymbol (SK_LocalMacroFunction _)	= True
	isOKSymbol (SK_GeneratedFunction _ _)	= True
	isOKSymbol _							= False
	
	is_not_caf FK_Caf	= False
	is_not_caf _		= True
transformSelection NormalSelector [] expr ro ti
	= (expr, ti)
transformSelection selector_kind selectors expr ro ti
	= (Selection selector_kind expr selectors, ti)

//@	determineProducers: finds all legal producers in the argument list.
// This version finds FIRST legal producer in argument list...

// XXX store linear_bits and cc_args together ?

determineProducers :: !Bool !Bool !Bool !(Optional SymbolType) ![Bool] ![Int] ![Expression] !Int *{!Producer} !ReadOnlyTI !*TransformInfo -> *(!*{!Producer},![Expression],![(LetBind,AType)],!*TransformInfo);
determineProducers _ _ _ _ _ _ [] _ producers _ ti
	= (producers, [], [], ti)
determineProducers is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer fun_type [linear_bit : linear_bits] [ cons_arg : cons_args ] [ arg : args ] prod_index producers ro ti
 	| cons_arg == CActive
		# (producers, new_arg, ti) = determine_producer is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit arg [] prod_index producers ro ti
		| isProducer producers.[prod_index]
			= (producers, new_arg++args, [], ti)
		#! (producers, new_args, lb, ti) = determineProducers is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer fun_type linear_bits cons_args args (inc prod_index) producers ro ti
		= (producers, new_arg++new_args, lb, ti)
	| SwitchUnusedFusion
		(	ro.ro_transform_fusion 
		&&	cons_arg == CUnusedStrict 
		&&	isStrictArg fun_type prod_index
		) False
		# producers = { producers & [prod_index] = PR_Unused }
		# (lb,ti) = case isStrictVarOrSimpleExpression arg of
						True	-> ([],ti)
						_		# (info_ptr, ti_var_heap)	= newPtr VI_Empty ti.ti_var_heap
								  ti						= {ti & ti_var_heap = ti_var_heap}
								  lb =	{lb_dst=
								  			{ fv_ident = { id_name = "dummy_for_strict_unused", id_info = nilPtr }
								  			, fv_info_ptr = info_ptr
								  			, fv_count = 0
								  			, fv_def_level = NotALevel 
								  			}
								  		,lb_src=arg
								  		,lb_position=NoPos
								  		}
								-> ([(lb,getArgType fun_type prod_index)],ti)
		  
		= (producers, args, lb, ti)	 // ---> ("UnusedStrict",lb,arg,fun_type)
	| SwitchUnusedFusion
		(	ro.ro_transform_fusion 
		&&	cons_arg == CUnusedStrict 
		&&	not (isStrictArg fun_type prod_index)
		&&	isStrictVar arg
		) False
		# producers = { producers & [prod_index] = PR_Unused }
		= (producers, args, [], ti)	 // ---> ("UnusedMixed",arg,fun_type)
	| SwitchUnusedFusion (ro.ro_transform_fusion && cons_arg == CUnusedLazy) False
		# producers = { producers & [prod_index] = PR_Unused }
		= (producers, args, [], ti)	 // ---> ("UnusedLazy",arg,fun_type)
	#! (producers, new_args, lb, ti) = determineProducers is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer fun_type linear_bits cons_args args (inc prod_index) producers ro ti
	= (producers, [arg : new_args], lb, ti)
where
	isProducer PR_Empty	= False
	isProducer _		= True
	
	isStrictArg No _ = False
	isStrictArg (Yes {st_args_strictness}) index = arg_is_strict index st_args_strictness

	getArgType (Yes {st_args}) index = st_args!!index

	isStrictVar (Var bv) = not (isEmpty [fv \\ fv <- ro.ro_tfi.tfi_vars | fv.fv_info_ptr == bv.var_info_ptr])
	isStrictVar _ = False

	isStrictVarOrSimpleExpression (Var bv)
		= not (isEmpty [fv \\ fv <- ro.ro_tfi.tfi_vars | fv.fv_info_ptr == bv.var_info_ptr])
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_Constructor _},app_args=[]})
		= True
	isStrictVarOrSimpleExpression (BasicExpr _)
		= True
	isStrictVarOrSimpleExpression _
		= False

	determine_producer is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit arg=:(App app=:{app_info_ptr}) new_args prod_index producers ro ti
		| isNilPtr app_info_ptr
			= determineProducer app EI_Empty is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit new_args prod_index producers ro ti
		# (app_info, ti_symbol_heap) = readPtr app_info_ptr ti.ti_symbol_heap
		# ti = { ti & ti_symbol_heap = ti_symbol_heap }
		= determineProducer app app_info is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit new_args prod_index producers ro ti
	determine_producer _ _ _ _ arg new_args _ producers _ ti
		= (producers, [arg : new_args], ti)

determineProducer :: App ExprInfo Bool Bool Bool Bool [Expression] Int *{!Producer} ReadOnlyTI *TransformInfo -> *(!*{!Producer},![Expression],!*TransformInfo)
determineProducer app=:{app_symb = symb=:{symb_kind = SK_Constructor _}, app_args} (EI_DictionaryType type) _ _ _ _
				  new_args prod_index producers _ ti=:{ti_var_heap,ti_predef_symbols}
	# (normalise_symbol,ti_predef_symbols) = ti_predef_symbols![PD_Dyn_normalise]
	# (app_args, (new_vars_and_types, free_vars, ti_var_heap))
			= renewVariables app_args normalise_symbol ti_var_heap
	# prod	= PR_Class { app & app_args = app_args } new_vars_and_types type
	= ( {producers & [prod_index] = prod}
	  , free_vars++new_args
	  , {ti & ti_var_heap=ti_var_heap, ti_predef_symbols=ti_predef_symbols}
	  )
determineProducer app=:{app_symb = symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} _ _ _ _ linear_bit
				  new_args prod_index producers ro ti
	# {cons_type}								= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
	  rnf										= rnf_args app_args 0 cons_type.st_args_strictness ro
	| SwitchConstructorFusion
		(ro.ro_transform_fusion && SwitchRnfConstructorFusion (linear_bit || rnf) linear_bit)
		(ro.ro_transform_fusion && cons_index.glob_module==ro.ro_StdGeneric_module_n && (linear_bit || rnf))
		False
		# producers = {producers & [prod_index] = PR_Constructor symb (length app_args) app_args }
		= (producers, app_args ++ new_args, ti)
	= ( producers, [App app : new_args ], ti)
where
	rnf_args [] index strictness ro
		= True
	rnf_args [arg:args] index strictness ro
		| arg_is_strict index strictness
			= case arg of
				BasicExpr _	-> rnf_args args (inc index) strictness ro
				App app		-> rnf_app_args app args index strictness ro
				_			-> False
			= rnf_args args (inc index) strictness ro

	rnf_app_args {app_symb=symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} args index strictness ro
		# {cons_type}		= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
		| rnf_args app_args 0 cons_type.st_args_strictness ro
			= rnf_args args (inc index) strictness ro
			= False
	// what else is rnf => curried apps
	rnf_app_args {app_symb=symb=:{symb_kind}, app_args} args index strictness ro
		= False
determineProducer app=:{app_symb = symb=:{ symb_kind = SK_GeneratedFunction fun_ptr fun_index}, app_args} _ is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit
				  new_args prod_index producers ro ti
	# (FI_Function {gf_cons_args={cc_producer},gf_fun_def={fun_body, fun_arity, fun_type, fun_info}}, ti_fun_heap)
					= readPtr fun_ptr ti.ti_fun_heap
	  ti = { ti & ti_fun_heap=ti_fun_heap }
	# n_app_args = length app_args
	| n_app_args<>fun_arity
		| is_applied_to_macro_fun
			= ({producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		| SwitchCurriedFusion ro.ro_transform_fusion cc_producer False
			# (is_good_producer,ti)
				= SwitchGeneratedFusion
					(function_is_good_producer fun_body fun_type linear_bit ro ti)
					(False,ti)
			| cc_producer && is_good_producer
				= ({producers & [prod_index] = PR_CurriedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
			= ({producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		= (producers, [App app : new_args], ti)
	# (is_good_producer,ti)
		= SwitchGeneratedFusion
			(function_is_good_producer fun_body fun_type linear_bit ro ti)
			(False,ti)
	| cc_producer && is_good_producer
		= ({producers & [prod_index] = PR_GeneratedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
	# not_expanding_producer
		= case fun_body of
			Expanding _
				-> False
			_
				-> True //cc_producer
	| SwitchHOFusion
    	((not consumer_is_curried && not_expanding_producer) && is_applied_to_macro_fun && linear_bit && is_higher_order_function fun_type)
    	False
		= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
    | SwitchHOFusion`
    	((not consumer_is_curried && not_expanding_producer) && ok_non_rec_consumer && linear_bit && is_higher_order_function fun_type)
    	False
		= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
	# non_rec_producer = (fun_info.fi_properties bitand FI_IsNonRecursive) <> 0
	# ok_non_rec
		= case fun_body of
			Expanding _
				-> False
			(TransformedBody {tb_rhs})
				-> ro.ro_transform_fusion && not_expanding_producer && is_sexy_body tb_rhs && ok_non_rec_consumer && non_rec_producer//is_good_producer
	| SwitchNonRecFusion ok_non_rec False
		= ({producers & [prod_index] = PR_GeneratedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
	= (producers, [App app : new_args ], ti)
determineProducer app=:{app_symb = symb=:{symb_kind}, app_args} _ is_applied_to_macro_fun consumer_is_curried ok_non_rec_consumer linear_bit
				  new_args prod_index producers ro ti
	| is_SK_Function_or_SK_LocalMacroFunction symb_kind
		# { glob_module, glob_object }
			= case symb_kind of
				SK_Function global_index -> global_index
				SK_LocalMacroFunction index -> { glob_module = ro.ro_main_dcl_module_n, glob_object = index }
		# (fun_arity, ti) = get_fun_arity glob_module glob_object ro ti
		  n_app_args = length app_args
		| n_app_args<>fun_arity
			| is_applied_to_macro_fun
				= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
			# ({cc_producer},ti) = ti!ti_cons_args.[glob_object]
			| SwitchCurriedFusion ro.ro_transform_fusion cc_producer False
				# ({fun_body,fun_type,fun_info}, ti) = ti!ti_fun_defs.[glob_object]
				# (is_good_producer,ti)
					= SwitchFunctionFusion
						(function_is_good_producer fun_body fun_type linear_bit ro ti)
						(False,ti)
				#! max_index = size ti.ti_cons_args
				| glob_module==ro.ro_main_dcl_module_n && glob_object < max_index &&
				  is_good_producer && cc_producer && not consumer_is_curried
					= ({producers & [prod_index] = PR_CurriedFunction symb n_app_args glob_object}, app_args ++ new_args, ti)
				= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
			= (producers, [App app : new_args], ti)
		#! max_index = size ti.ti_cons_args
		| glob_module <> ro.ro_main_dcl_module_n || glob_object >= max_index /* Sjaak, to skip array functions */
			= (producers, [App app : new_args ], ti)
					-!-> ("Produce2cc_array",symb.symb_ident,if (glob_module <> ro.ro_main_dcl_module_n) "foreign" "array")
		# ({fun_body,fun_type,fun_info}, ti) = ti!ti_fun_defs.[glob_object]
		# (is_good_producer,ti)
			= SwitchFunctionFusion
				(function_is_good_producer fun_body fun_type linear_bit ro ti)
				(False,ti)
		  {cc_producer} = ti.ti_cons_args.[glob_object]
		| is_good_producer && cc_producer && not consumer_is_curried
			= ({ producers & [prod_index] = PR_Function symb n_app_args glob_object}, app_args ++ new_args, ti)
	    # not_expanding_producer
			= case fun_body of
				Expanding _
					-> False
				_
					-> True // cc_producer
		| (not consumer_is_curried && not_expanding_producer) && is_applied_to_macro_fun && linear_bit && is_higher_order_function fun_type
			= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		# non_rec_producer = (fun_info.fi_properties bitand FI_IsNonRecursive) <> 0
		# ok_non_rec
			= case fun_body of
				Expanding _
					-> False
				(TransformedBody {tb_rhs})
					-> ro.ro_transform_fusion && not_expanding_producer && is_sexy_body tb_rhs && ok_non_rec_consumer && non_rec_producer//&& is_good_producer
		| SwitchNonRecFusion ok_non_rec False
			= ({producers & [prod_index] = PR_Function symb n_app_args glob_object}, app_args ++ new_args, ti)
		= (producers, [App app : new_args], ti)
	= (producers, [App app : new_args], ti)
where
	get_max_index ti=:{ti_cons_args}
		#! (max_index, ti_cons_args)	= usize ti_cons_args
		= (max_index, {ti & ti_cons_args = ti_cons_args})

	get_fun_arity glob_module glob_object ro ti
		| glob_module <> ro.ro_main_dcl_module_n
			# {st_arity, st_context} = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
			= (st_arity+length st_context, ti)
		// for imported functions you have to add ft_arity and length st_context, but for unimported
		// functions fun_arity alone is sufficient
		= ti!ti_fun_defs.[glob_object].fun_arity

function_is_good_producer (Expanding _) fun_type linear_bit ro ti
	= (False,ti)
function_is_good_producer (TransformedBody {tb_rhs}) fun_type linear_bit ro ti
	| ro.ro_transform_fusion
		| linear_bit && is_sexy_body tb_rhs
			= (True,ti)
			= function_may_be_copied fun_type tb_rhs ro ti
		= (False,ti)
where
	function_may_be_copied (Yes {st_args_strictness}) rhs ro ti
		| is_not_strict st_args_strictness
			= expression_may_be_copied rhs ro ti
			= (False,ti)
	function_may_be_copied No rhs ro ti
		= expression_may_be_copied rhs ro ti

	// to optimize bimap
	expression_may_be_copied (Var _) ro ti
		= (True,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_Constructor cons_index}, app_args}) ro ti
		# cons_type = ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object].cons_type
		| cons_index.glob_module==ro.ro_StdGeneric_module_n && is_not_strict cons_type.st_args_strictness
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_Function {glob_object,glob_module}}, app_args}) ro ti
		| glob_module <> ro.ro_main_dcl_module_n
			# fun_type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
			| length app_args < fun_type.st_arity+length fun_type.st_context
				= expressions_may_be_copied app_args ro ti
				= (False,ti)
			# (fun_arity,ti) = ti!ti_fun_defs.[glob_object].fun_arity
			| length app_args < fun_arity
				= expressions_may_be_copied app_args ro ti
				= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_LocalMacroFunction glob_object}, app_args}) ro ti
		# (fun_arity,ti) = ti!ti_fun_defs.[glob_object].fun_arity
		| length app_args < fun_arity
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_GeneratedFunction fun_ptr _}, app_args}) ro ti
		# (FI_Function {gf_fun_def={fun_arity}}) = sreadPtr fun_ptr ti.ti_fun_heap
		| length app_args < fun_arity
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (Selection NormalSelector (Var _) [RecordSelection {glob_module,glob_object={ds_index}} _]) ro ti
		# selector_type = ro.ro_common_defs.[glob_module].com_selector_defs.[ds_index].sd_type
		| glob_module==ro.ro_StdGeneric_module_n && is_not_strict selector_type.st_args_strictness
			= (True,ti)
			= (False,ti)
	expression_may_be_copied _ ro ti
		= (False,ti)
	
	expressions_may_be_copied [expr:exprs] ro ti
		# (ok,ti) = expression_may_be_copied expr ro ti
		| ok
			= expressions_may_be_copied exprs ro ti
			= (False,ti)
	expressions_may_be_copied [] ro ti
		= (True,ti)

// when two function bodies have fusion with each other this only leads into satisfaction if one body
// fulfills the following sexyness property
// DvA: now that we have producer requirements we can integrate this condition there...
is_sexy_body (AnyCodeExpr _ _ _) = False	
is_sexy_body (ABCCodeExpr _ _) = False	
is_sexy_body (Let {let_strict_binds}) = isEmpty let_strict_binds	
	// currently a producer's body must not be a let with strict bindings. The code sharing elimination algorithm assumes that
	// all strict let bindings are on the top level expression (see "convertCasesOfFunctionsIntoPatterns"). This assumption
	// could otherwise be violated during fusion.
	// -> Here is place for optimisation: Either the fusion algorithm or the code sharing elimination algorithm could be
	// extended to generate new functions when a strict let ends up during fusion in a non top level position (MW)
is_sexy_body _ = True	

is_higher_order_function (Yes {st_result={at_type=_ --> _}})
        = True
is_higher_order_function _
        = False

containsProducer prod_index producers
	| prod_index == 0
		= False
		#! prod_index = dec prod_index
		= is_a_producer producers.[prod_index] || containsProducer prod_index producers
where
	is_a_producer PR_Empty	= False
	is_a_producer _ 		= True

:: *RenewState :== (![(BoundVar, Type)], ![Expression], !*VarHeap)

renewVariables :: ![Expression] !PredefinedSymbol !*VarHeap -> (![Expression], !RenewState)
renewVariables exprs normalise_symbol var_heap
	# (exprs, (new_vars, free_vars, var_heap))
			= mapSt map_expr_st exprs ([], [], var_heap)
	  var_heap = foldSt (\ expr var_heap
	  						-> case expr of
	  							Var {var_info_ptr}	-> writeVarInfo var_info_ptr VI_Empty var_heap
	  							_					-> var_heap
	  					) free_vars var_heap
	= (exprs, (new_vars, free_vars, var_heap))
  where
	map_expr_st (Var var=:{var_info_ptr, var_ident}) (new_vars_accu, free_vars_accu, var_heap)
		# (var_info, var_heap) = readPtr var_info_ptr var_heap
		= case var_info of
			VI_Extended _ (VI_Forward new_var)
				-> (Var new_var, (new_vars_accu, free_vars_accu, var_heap))
			VI_Extended evi=:(EVI_VarType var_type) _
				# (new_var, var_heap)
					= allocate_and_bind_new_var var_ident var_info_ptr evi var_heap
				-> (Var new_var, ([(new_var, var_type.at_type) : new_vars_accu], [Var var:free_vars_accu], var_heap))
	map_expr_st expr=:(App app=:{app_symb={symb_kind=SK_Function {glob_object,glob_module},symb_ident},app_args}) (new_vars_accu, free_vars_accu, var_heap)
		| glob_module==normalise_symbol.pds_module && glob_object==normalise_symbol.pds_def
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			  new_var = { var_ident = symb_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
			= (Var new_var, ([(new_var, TE) : new_vars_accu], [expr:free_vars_accu], var_heap))
	map_expr_st expr=:(App app=:{app_args}) st
		# (app_args, st) = mapSt map_expr_st app_args st
		= (App { app & app_args = app_args }, st)
	map_expr_st (Let lad=:{let_lazy_binds, let_strict_binds, let_expr}) st
		# (lazy_free_vars, st)
				= mapSt (\{lb_dst} st -> preprocess_local_var lb_dst st) let_lazy_binds st
		  (strict_free_vars, st)
				= mapSt (\{lb_dst} st -> preprocess_local_var lb_dst st) let_strict_binds st
		  (lazy_rhss, st)
		  		= mapSt (\{lb_src} st -> map_expr_st lb_src st) let_lazy_binds st
		  (strict_rhss, st)
		  		= mapSt (\{lb_src} st -> map_expr_st lb_src st) let_strict_binds st
		  (let_expr, st)
		  		= map_expr_st let_expr st
		  st	= foldSt (\{lb_dst} st -> postprocess_local_var lb_dst st) let_lazy_binds st
		  st	= foldSt (\{lb_dst} st -> postprocess_local_var lb_dst st) let_strict_binds st
		  expr	= Let { lad
		  			& let_lazy_binds	= add_let_binds lazy_free_vars lazy_rhss let_lazy_binds
		  			, let_strict_binds	= add_let_binds strict_free_vars strict_rhss let_strict_binds
		  			, let_expr			= let_expr
					}
		= (expr, st)
	map_expr_st (Selection a expr b) st
		# (expr, st) = map_expr_st expr st
		= (Selection a expr b, st)
	map_expr_st expr=:(BasicExpr _) st 
		= (expr, st)

	preprocess_local_var :: !FreeVar !RenewState -> (!FreeVar, !RenewState)
	preprocess_local_var fv=:{fv_ident, fv_info_ptr} (new_vars_accu, free_vars_accu, var_heap)
//		# (VI_Extended evi _, var_heap)
//				= readPtr fv_info_ptr var_heap
		# (evi, var_heap)
				= readExtendedVarInfo fv_info_ptr var_heap
		  (new_var, var_heap)
				= allocate_and_bind_new_var fv_ident fv_info_ptr evi var_heap
		= ( { fv & fv_info_ptr = new_var.var_info_ptr }
		  , (new_vars_accu, free_vars_accu, var_heap))

	allocate_and_bind_new_var var_ident var_info_ptr evi var_heap
		# (new_info_ptr, var_heap) = newPtr (VI_Extended evi VI_Empty) var_heap
		  new_var = { var_ident = var_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
		  var_heap = writeVarInfo var_info_ptr (VI_Forward new_var) var_heap
		= (new_var, var_heap)

	postprocess_local_var :: !FreeVar !RenewState -> RenewState
	postprocess_local_var {fv_info_ptr} (a, b, var_heap)
		= (a, b, writeVarInfo fv_info_ptr VI_Empty var_heap)

foldrExprSt f expr st :== foldr_expr_st expr st
  where
	foldr_expr_st expr=:(Var _) st
		= f expr st
	foldr_expr_st app=:(App {app_args}) st
		= f app (foldSt foldr_expr_st app_args st)
	foldr_expr_st lad=:(Let {let_lazy_binds, let_strict_binds, let_expr}) st
		# st
		  		= foldSt (\{lb_src} st -> foldr_expr_st lb_src st) let_lazy_binds st
		  st
		  		= foldSt (\{lb_src} st -> foldr_expr_st lb_src st) let_strict_binds st
		  st
		  		= f let_expr st
		= f lad st
	foldr_expr_st sel=:(Selection a expr b) st
		= f sel (foldr_expr_st expr st)
		
	// AA:	
	foldr_expr_st expr=:(BasicExpr _) st 
		= f expr st

add_let_binds :: [FreeVar] [Expression] [LetBind] -> [LetBind]
add_let_binds free_vars rhss original_binds
	= [{ original_bind & lb_dst = lb_dst, lb_src = lb_src}
		\\ lb_dst <- free_vars & lb_src <- rhss & original_bind <- original_binds]

//@	transformGroups

transformGroups :: !CleanupInfo !Int !Int !Int !Int !*{!Component} !*{#FunDef} !*{!.ConsClasses} !{# CommonDefs}  !{# {# FunType} }
		!*ImportedTypes !*TypeDefInfos !*VarHeap !*TypeHeaps !*ExpressionHeap !Bool !*File !*PredefinedSymbols
			-> (!*{!Component}, !*{#FunDef}, !*ImportedTypes, !ImportedConstructors, !*VarHeap, !*TypeHeaps, !*ExpressionHeap, !*File, !*PredefinedSymbols)
transformGroups cleanup_info main_dcl_module_n ro_StdStrictLists_module_n def_min def_max groups fun_defs cons_args common_defs imported_funs
		imported_types type_def_infos var_heap type_heaps symbol_heap compile_with_fusion error predef_symbols
	#! nr_of_funs = size fun_defs
	# initial_ti =	{ ti_fun_defs		= fun_defs
					, ti_instances		= createArray nr_of_funs II_Empty
					, ti_cons_args		= cons_args
					, ti_new_functions	= []
					, ti_fun_heap		= newHeap
					, ti_var_heap		= var_heap
					, ti_symbol_heap	= symbol_heap
					, ti_type_heaps		= type_heaps
					, ti_type_def_infos	= type_def_infos
					, ti_next_fun_nr	= nr_of_funs
					, ti_cleanup_info	= cleanup_info
					, ti_recursion_introduced	= No
					, ti_error_file		= error
					, ti_predef_symbols	= predef_symbols }

	# groups = [group \\ group <-: groups]
	# (groups, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti)
		= transform_groups 0 groups [] common_defs imported_funs imported_types [] [] initial_ti
	# groups = {group \\ group <- reverse groups}

	  {ti_fun_defs,ti_new_functions,ti_var_heap,ti_symbol_heap,ti_fun_heap,ti_next_fun_nr,ti_type_heaps,ti_cleanup_info} = ti
	# (fun_defs, imported_types, collected_imports, type_heaps, var_heap) 
			= foldSt (expand_abstract_syn_types_in_function_type common_defs) (reverse fun_indices_with_abs_syn_types)
					(ti_fun_defs, imported_types, collected_imports, ti_type_heaps, ti_var_heap)

	  (groups, new_fun_defs, imported_types, collected_imports, type_heaps, var_heap) 
	  		= foldSt (add_new_function_to_group common_defs ti_fun_heap) ti_new_functions
	  				(groups, [], imported_types, collected_imports, type_heaps, var_heap)
	  symbol_heap = foldSt cleanup_attributes ti.ti_cleanup_info ti.ti_symbol_heap
	  fun_defs = { fundef \\ fundef <- [ fundef \\ fundef <-: fun_defs ] ++ new_fun_defs }
	= (groups, fun_defs, imported_types, collected_imports,	var_heap, type_heaps, symbol_heap, ti.ti_error_file, ti.ti_predef_symbols)
where
	transform_groups :: !Int ![Component] !u:[Component] !{#CommonDefs} !{#{#FunType}} !*{#{#CheckedTypeDef}} ![(Global Int)] !v:[Int] !*TransformInfo -> *(!w:[Component],!.{#{#CheckedTypeDef}},![(Global Int)],!x:[Int],!*TransformInfo), [u <= w,v <= x]
	transform_groups group_nr [group:groups] acc_groups common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
		# {component_members} = group
		# (ti_fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti_type_heaps, ti_var_heap) 
				= convert_function_types component_members common_defs
						(ti.ti_fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti.ti_type_heaps, ti.ti_var_heap)
		# ti = { ti & ti_fun_defs = ti_fun_defs, ti_type_heaps = ti_type_heaps, ti_var_heap = ti_var_heap }
		# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups ti
		= transform_groups group_nr groups acc_groups common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
	transform_groups group_nr [] acc_groups common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
		= (acc_groups, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti)

	convert_function_types (ComponentMember member members) common_defs s
		# s = convert_function_type common_defs member s
		= convert_function_types members common_defs s
	convert_function_types NoComponentMembers common_defs s
		= s

	transform_groups_again :: !Int ![Component] ![Component] !{#CommonDefs} !{#{#FunType}} !*TransformInfo -> *(![Component],!*TransformInfo)
	transform_groups_again group_nr [group:groups] acc_groups common_defs imported_funs ti
		# {component_members} = group
		# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups ti
		= transform_groups_again group_nr groups acc_groups common_defs imported_funs ti
	transform_groups_again group_nr [] acc_groups common_defs imported_funs ti
		= (acc_groups, ti)

	transform_group :: !{#CommonDefs} !{#{#FunType}} !Int !ComponentMembers !u:[Component] !*TransformInfo -> *(!Int,!u:[Component],!*TransformInfo)
	transform_group common_defs imported_funs group_nr component_members acc_groups ti
		// assign group_nr to component_members
		# ti = assign_groups component_members group_nr ti

		# (before,ti) = ti!ti_next_fun_nr
		// transform component_members
		# ti = transform_functions component_members common_defs imported_funs ti
		// partitionate group: need to know added functions for this...
		# (after,ti) = ti!ti_next_fun_nr

		| not (compile_with_fusion || after > before)
			= (inc group_nr,[{component_members=component_members}:acc_groups],ti)

		# (ti_new_functions,ti) = ti!ti_new_functions

		# (new_functions_in_component,ti_fun_heap)
			= determine_new_functions_in_component (after-before) ti_new_functions before after ti.ti_fun_heap
		# ti = {ti & ti_fun_heap=ti_fun_heap}
		# (new_groups,ti) = partition_group group_nr (append_ComponentMembers component_members new_functions_in_component) ti
		// reanalyse consumers
		# (cleanup,ti_fun_defs,ti_var_heap,ti_symbol_heap,ti_fun_heap,ti_cons_args,same)
				= reanalyseGroups common_defs imported_funs main_dcl_module_n ro_StdStrictLists_module_n
					new_groups
					ti.ti_fun_defs ti.ti_var_heap ti.ti_symbol_heap ti.ti_fun_heap ti.ti_cons_args
		# ti = {ti 
				& ti_cleanup_info = cleanup ++ ti.ti_cleanup_info
				, ti_fun_defs = ti_fun_defs
				, ti_var_heap = ti_var_heap
				, ti_symbol_heap = ti_symbol_heap
				, ti_fun_heap = ti_fun_heap
				, ti_cons_args = ti_cons_args
				}
		// if wanted reapply transform_group to all found groups
		| after>before || length new_groups > 1 || not same
			= transform_groups` common_defs imported_funs group_nr new_groups acc_groups ti
		// producer annotation for finished components!
		# ti = reannotate_producers group_nr component_members ti
		= (inc group_nr,(reverse new_groups)++acc_groups,ti)
	where
		transform_groups` :: !{#CommonDefs} !{#{#FunType}} !Int ![Component] !u:[Component] !*TransformInfo -> *(!Int,!u:[Component],!*TransformInfo)
		transform_groups` common_defs imported_funs group_nr [] acc_groups ti
			= (group_nr, acc_groups, ti)
		transform_groups` common_defs imported_funs group_nr [{component_members}:groups] acc_groups ti
			# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups ti
			= transform_groups` common_defs imported_funs group_nr groups acc_groups ti
	
		changed_group_classification [] ti
			= (False,ti)
		changed_group_classification [fun:funs] ti
			= (False,ti)

		assign_groups :: !ComponentMembers !Int !*TransformInfo -> *TransformInfo
		assign_groups (ComponentMember member members) group_nr ti
			# ti = {ti & ti_fun_defs.[member].fun_info.fi_group_index = group_nr}
			= assign_groups members group_nr ti
		assign_groups (GeneratedComponentMember member fun_ptr members) group_nr ti=:{ti_fun_heap}
			# (FI_Function gf=:{gf_fun_def=fd}, ti_fun_heap) = readPtr fun_ptr ti_fun_heap
			# fd = {fd & fun_info.fi_group_index = group_nr}
			# ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def=fd}) ti_fun_heap
			# ti = {ti & ti_fun_heap=ti_fun_heap}
			= assign_groups members group_nr ti
		assign_groups NoComponentMembers group_nr ti
			= ti
		
		partition_group :: !.Int !ComponentMembers !*TransformInfo -> *(![Component],!*TransformInfo)
		partition_group group_nr component_members ti
			# {ti_fun_defs=fun_defs, ti_fun_heap=fun_heap, ti_next_fun_nr=max_fun_nr,
			   ti_predef_symbols=predef_symbols, ti_var_heap=var_heap, ti_symbol_heap=expression_heap, ti_error_file} = ti
			# next_group = group_nr
			# error_admin = {ea_file = ti_error_file, ea_loc = [], ea_ok = True }
			# (_,groups,fun_defs,fun_heap,predef_symbols,var_heap,expression_heap,error_admin)
				= partitionateFunctions`` max_fun_nr next_group fun_defs component_members main_dcl_module_n def_min def_max fun_heap predef_symbols var_heap expression_heap error_admin
			# ti = { ti	& ti_fun_defs		= fun_defs
						, ti_fun_heap		= fun_heap
						, ti_predef_symbols	= predef_symbols
						, ti_var_heap		= var_heap
						, ti_symbol_heap	= expression_heap
						, ti_error_file		= error_admin.ea_file }
			= (groups,ti)

		transform_functions :: !ComponentMembers !{#CommonDefs} !{#{#FunType}} !*TransformInfo -> *TransformInfo
		transform_functions (ComponentMember member members) common_defs imported_funs ti
			# (fun_def, ti) = ti!ti_fun_defs.[member]
			  fun_symb = {symb_ident=fun_def.fun_ident, symb_kind=SK_Function {glob_object=member, glob_module=main_dcl_module_n}}
			  (fun_body,ti)
				= transform_function fun_def.fun_type fun_def.fun_body fun_symb common_defs imported_funs ti
			  fun_def = {fun_def & fun_body=fun_body}
			  ti = {ti & ti_fun_defs.[member] = fun_def}
			= transform_functions members common_defs imported_funs ti
		transform_functions (GeneratedComponentMember member fun_ptr members) common_defs imported_funs ti
			# (FI_Function gf=:{gf_fun_def},ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
			  fun_symb = {symb_ident=gf_fun_def.fun_ident, symb_kind=SK_GeneratedFunction fun_ptr member }
			  ti = {ti & ti_fun_heap = ti_fun_heap}
			  (fun_body,ti)
				= transform_function gf_fun_def.fun_type gf_fun_def.fun_body fun_symb common_defs imported_funs ti
			  gf_fun_def = {gf_fun_def & fun_body=fun_body}
			  ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def=gf_fun_def}) ti.ti_fun_heap
			  ti = {ti & ti_fun_heap = ti_fun_heap}
			= transform_functions members common_defs imported_funs ti
		transform_functions NoComponentMembers common_defs imported_funs ti
			= ti

		transform_function :: !(Optional SymbolType) !FunctionBody !SymbIdent !{#CommonDefs} !{#{#FunType}} !*TransformInfo -> (!FunctionBody,!*TransformInfo)
		transform_function (Yes {st_args,st_args_strictness}) (TransformedBody tb) fun_symb common_defs imported_funs ti
			# (ro_StdGeneric_module_n,ti) = ti!ti_predef_symbols.[PD_StdGeneric].pds_def
			  ti_var_heap					= fold2St store_arg_type_info tb.tb_args st_args ti.ti_var_heap
			  tfi =	{ tfi_root				= fun_symb
					, tfi_case				= fun_symb
					, tfi_orig				= fun_symb
					, tfi_args				= tb.tb_args
					, tfi_vars				= [arg \\ arg <- tb.tb_args & i <- [0..] | arg_is_strict i st_args_strictness]
					, tfi_geni				= (-1,-1)
					}
			  ro =	{ ro_imported_funs				= imported_funs
					, ro_common_defs 				= common_defs
					, ro_root_case_mode				= get_root_case_mode tb
					, ro_tfi						= tfi
					, ro_main_dcl_module_n			= main_dcl_module_n
					, ro_transform_fusion			= compile_with_fusion
					, ro_StdStrictLists_module_n	= ro_StdStrictLists_module_n
					, ro_StdGeneric_module_n		= ro_StdGeneric_module_n
					}
			  ti = {ti & ti_var_heap = ti_var_heap}
		  
			  (fun_rhs, ti)						= transform tb.tb_rhs ro ti
			= (TransformedBody {tb & tb_rhs = fun_rhs},ti)
		  where
			store_arg_type_info {fv_info_ptr} a_type ti_var_heap
				= setExtendedVarInfo fv_info_ptr (EVI_VarType a_type) ti_var_heap
			
			fun_def_to_symb_ident fun_index fsize {fun_ident}
				| fun_index < fsize
				= { symb_ident=fun_ident, symb_kind=SK_Function {glob_object=fun_index, glob_module=main_dcl_module_n } }

			get_root_case_mode {tb_rhs=Case _}	= RootCase
			get_root_case_mode _ 				= NotRootCase

	reannotate_producers group_nr component_members ti
		// determine if safe group
		# (safe,ti) = safe_producers group_nr component_members component_members main_dcl_module_n ti
		| safe
			// if safe mark all members as safe
			= mark_producers_safe component_members ti
		= ti

	safe_producers :: Int ComponentMembers ComponentMembers Int *TransformInfo -> *(!Bool,!*TransformInfo)
	safe_producers group_nr component_members (ComponentMember fun funs) main_dcl_module_n ti
		// look for occurrence of component_members in safe argument position of fun RHS
		// i.e. linearity ok && ...
		# (fun_def,fun_defs) = (ti.ti_fun_defs)![fun]
		  {fun_body = TransformedBody {tb_rhs}} = fun_def
		  prs =	{ prs_group				= component_members
				, prs_cons_args 		= ti.ti_cons_args
				, prs_main_dcl_module_n	= main_dcl_module_n
				, prs_fun_heap			= ti.ti_fun_heap
				, prs_fun_defs			= fun_defs
				, prs_group_index		= group_nr }
		# (safe,prs) = producerRequirements tb_rhs prs
		#! ti = {ti & ti_fun_defs = prs.prs_fun_defs, ti_fun_heap = prs.prs_fun_heap, ti_cons_args = prs.prs_cons_args}
		// put back prs info into ti?
		| safe
			= safe_producers group_nr component_members funs main_dcl_module_n ti
			= (False,ti)
	safe_producers group_nr component_members (GeneratedComponentMember fun fun_ptr funs) main_dcl_module_n ti
		# (FI_Function {gf_fun_def}, ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
		  ti = {ti & ti_fun_heap=ti_fun_heap}
		  {fun_body = TransformedBody {tb_rhs}} = gf_fun_def
		  prs =	{ prs_group				= component_members
				, prs_cons_args 		= ti.ti_cons_args
				, prs_main_dcl_module_n	= main_dcl_module_n
				, prs_fun_heap			= ti.ti_fun_heap
				, prs_fun_defs			= ti.ti_fun_defs
				, prs_group_index		= group_nr }
		  (safe,prs) = producerRequirements tb_rhs prs
		#! ti = {ti & ti_fun_defs = prs.prs_fun_defs, ti_fun_heap = prs.prs_fun_heap, ti_cons_args = prs.prs_cons_args}
		| safe
			= safe_producers group_nr component_members funs main_dcl_module_n ti
			= (False,ti)
	safe_producers group_nr component_members NoComponentMembers main_dcl_module_n ti
		= (True,ti)

	mark_producers_safe (ComponentMember member members) ti
		# ti = {ti & ti_cons_args.[member].cc_producer = pIsSafe}
		= mark_producers_safe members ti
	mark_producers_safe (GeneratedComponentMember member fun_ptr members) ti
		# (FI_Function gf,ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
		  ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_cons_args.cc_producer = pIsSafe}) ti_fun_heap
		  ti = {ti & ti_fun_heap = ti_fun_heap}
		= mark_producers_safe members ti
	mark_producers_safe NoComponentMembers ti
		= ti

	add_new_function_to_group ::  !{# CommonDefs} !FunctionHeap  !FunctionInfoPtr
				  !(!*{!Component}, ![FunDef], !*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
				-> (!*{!Component}, ![FunDef], !*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
	add_new_function_to_group common_defs fun_heap fun_ptr (groups, fun_defs, imported_types, collected_imports, type_heaps, var_heap)
		# (FI_Function {gf_fun_def,gf_fun_index}) = sreadPtr fun_ptr fun_heap
		  {fun_type = Yes ft=:{st_args,st_result}, fun_info = {fi_group_index,fi_properties}} = gf_fun_def
		  ets =
		  	{ ets_type_defs							= imported_types
		  	, ets_collected_conses					= collected_imports
		  	, ets_type_heaps						= type_heaps
		  	, ets_var_heap							= var_heap
		  	, ets_main_dcl_module_n					= main_dcl_module_n
		  	, ets_contains_unexpanded_abs_syn_type	= False
		  	}
		#! (_,(st_result,st_args), {ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap})
		  		= expandSynTypes (if (fi_properties bitand FI_HasTypeSpec == 0) (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask) ExpandAbstractSynTypesMask) common_defs (st_result,st_args) ets
		# ft = { ft &  st_result = st_result, st_args = st_args }
		| fi_group_index >= size groups
			= abort ("add_new_function_to_group "+++ toString fi_group_index+++ "," +++ toString (size groups) +++ "," +++ toString gf_fun_index)

		# (group, groups) = groups![fi_group_index]
		| not (isComponentMember gf_fun_index group.component_members)
			= abort ("add_new_function_to_group INSANE!\n" +++ toString gf_fun_index +++ "," +++ toString fi_group_index)
		# groups = {groups & [fi_group_index] = group}
		# gf_fun_def = {gf_fun_def & fun_type = Yes ft}
		= (groups, [gf_fun_def : fun_defs], ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
	where
		isComponentMember index (ComponentMember member members)
			= index==member || isComponentMember index members
		isComponentMember index (GeneratedComponentMember member _ members)
			= index==member || isComponentMember index members
		isComponentMember index NoComponentMembers
			= False

	convert_function_type common_defs fun_index (fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, type_heaps, var_heap)
		# (fun_def=:{fun_type = Yes fun_type, fun_info = {fi_properties}}, fun_defs)
					= fun_defs![fun_index]
		  rem_annot	= fi_properties bitand FI_HasTypeSpec == 0
		  (fun_type,contains_unexpanded_abs_syn_type,imported_types, collected_imports, type_heaps, var_heap)
		  		= convertSymbolType_ (if rem_annot RemoveAnnotationsMask 0) common_defs fun_type main_dcl_module_n imported_types collected_imports type_heaps var_heap
		# fun_def	= { fun_def & fun_type = Yes fun_type }
		  fun_defs	= { fun_defs & [fun_index] = fun_def }
		| contains_unexpanded_abs_syn_type
			= (fun_defs, imported_types, collected_imports, [fun_index : fun_indices_with_abs_syn_types], type_heaps, var_heap)
			= (fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, type_heaps, var_heap)

	expand_abstract_syn_types_in_function_type :: !{#.CommonDefs} !.Int !*(!*{#FunDef},!*{#{#CheckedTypeDef}},![(Global .Int)],!*TypeHeaps,!*(Heap VarInfo)) -> (!*{#FunDef},!.{#{#CheckedTypeDef}},![(Global Int)],!.TypeHeaps,!.(Heap VarInfo))
	expand_abstract_syn_types_in_function_type common_defs fun_index (fun_defs, imported_types, collected_imports, type_heaps, var_heap)
		# (fun_def=:{fun_type = Yes fun_type, fun_info = {fi_properties}}, fun_defs)
					= fun_defs![fun_index]
		  rem_annot	= fi_properties bitand FI_HasTypeSpec == 0
		  (fun_type,contains_unexpanded_abs_syn_type,imported_types, collected_imports, type_heaps, var_heap)
	  		= convertSymbolType_ (if rem_annot (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask) ExpandAbstractSynTypesMask) common_defs fun_type main_dcl_module_n imported_types collected_imports type_heaps var_heap
	  	  fun_def = { fun_def & fun_type = Yes fun_type}
	  	  fun_defs = { fun_defs & [fun_index] = fun_def }
		= (fun_defs, imported_types, collected_imports, type_heaps, var_heap)

	append_ComponentMembers :: !ComponentMembers !ComponentMembers -> ComponentMembers
	append_ComponentMembers (ComponentMember member members) component_members_to_append
		= ComponentMember member (append_ComponentMembers members component_members_to_append)
	append_ComponentMembers (GeneratedComponentMember member fun_ptr members) component_members_to_append
		= GeneratedComponentMember member fun_ptr (append_ComponentMembers members component_members_to_append)
	append_ComponentMembers NoComponentMembers component_members_to_append
		= component_members_to_append
	
	determine_new_functions_in_component :: !Int ![FunctionInfoPtr] !Int !Int !*FunctionHeap -> (ComponentMembers,!*FunctionHeap)
	determine_new_functions_in_component 0 new_functions before after fun_heap
			= (NoComponentMembers,fun_heap)
	determine_new_functions_in_component n_functions [fun_ptr:new_functions] before after fun_heap
		# (FI_Function {gf_fun_index},fun_heap) = readPtr fun_ptr fun_heap
		| gf_fun_index>=before && gf_fun_index<after
			# (members,fun_heap) = determine_new_functions_in_component (n_functions-1) new_functions before after fun_heap
			= (GeneratedComponentMember gf_fun_index fun_ptr members,fun_heap)	

RemoveAnnotationsMask:==1
ExpandAbstractSynTypesMask:==2
DontCollectImportedConstructors:==4

convertSymbolType :: !Bool !{# CommonDefs} !SymbolType !Int !*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap 
	-> (!SymbolType, !*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
convertSymbolType rem_annots common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	# (st, ets_contains_unexpanded_abs_syn_type,ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
		= convertSymbolType_  (if rem_annots (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask) ExpandAbstractSynTypesMask) common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	= (st, ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)

convertSymbolTypeWithoutCollectingImportedConstructors :: !Bool !{# CommonDefs} !SymbolType !Int !*ImportedTypes !*TypeHeaps !*VarHeap 
	-> (!SymbolType, !*ImportedTypes, !*TypeHeaps, !*VarHeap)
convertSymbolTypeWithoutCollectingImportedConstructors rem_annots common_defs st main_dcl_module_n imported_types type_heaps var_heap
	# (st, ets_contains_unexpanded_abs_syn_type,ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
		= convertSymbolType_  (if rem_annots (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask bitor DontCollectImportedConstructors) (ExpandAbstractSynTypesMask bitor DontCollectImportedConstructors)) common_defs st main_dcl_module_n imported_types [] type_heaps var_heap
	= (st, ets_type_defs, ets_type_heaps, ets_var_heap)

convertSymbolType_ :: !Int !{# CommonDefs} !SymbolType !Int !*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap 
	-> (!SymbolType, !Bool,!*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
convertSymbolType_  rem_annots common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	# ets	=	{ ets_type_defs			= imported_types
				, ets_collected_conses	= collected_imports
				, ets_type_heaps		= type_heaps
				, ets_var_heap			= var_heap
				, ets_main_dcl_module_n	= main_dcl_module_n 
				, ets_contains_unexpanded_abs_syn_type = False
				}
	# {st_args,st_result,st_context,st_args_strictness} = st
	#! (_,(st_args,st_result), ets)		= expandSynTypes rem_annots common_defs (st_args,st_result) ets
	# new_st_args						= addTypesOfDictionaries common_defs st_context st_args
	  new_st_arity						= length new_st_args
	  st	=	{ st
	  			& st_args				= new_st_args
	  			, st_result				= st_result
	  			, st_arity				= new_st_arity
	  			, st_args_strictness	= insert_n_strictness_values_at_beginning (new_st_arity-length st_args) st_args_strictness
	  			, st_context			= []
	  			}
	# {ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap,ets_contains_unexpanded_abs_syn_type} = ets
	= (st, ets_contains_unexpanded_abs_syn_type, ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)

addTypesOfDictionaries :: !{#CommonDefs} ![TypeContext] ![AType] -> [AType]
addTypesOfDictionaries common_defs type_contexts type_args
	= mapAppend (add_types_of_dictionary common_defs) type_contexts type_args
where
	add_types_of_dictionary common_defs {tc_class = TCGeneric {gtc_generic_dict={gi_module,gi_index}}, tc_types}
		#! generict_dict_ident = predefined_idents.[PD_TypeGenericDict]
		/*
			AA HACK:
			Generic classes are always generated locally, 
			and therefore the their dictionaries are also generated localy. 
			Overloaded functions in DCL modules can have generic context restrictions, i.e. they will 
			get generic class dictionaries. 
			Problem: DCL function types refer to ICL type defs of dictionaries.
			Solution: plug a dummy dictinary type, defined in StdGeneric.
			It is possible because all generic class have one class argument and one member.
		*/
		# dict_type_symb = MakeTypeSymbIdent {glob_object = gi_index, glob_module = gi_module} generict_dict_ident 1
		# type_arg = {at_attribute = TA_Multi, at_type=hd tc_types}
		= {at_attribute = TA_Multi, at_type = TA dict_type_symb [type_arg]}

	add_types_of_dictionary common_defs {tc_class = TCClass {glob_module, glob_object={ds_index,ds_ident}}, tc_types}
		# {class_arity, class_dictionary={ds_ident,ds_index}, class_cons_vars}
				= common_defs.[glob_module].com_class_defs.[ds_index]
		  dict_type_symb
		  		= MakeTypeSymbIdent {glob_object = ds_index, glob_module = glob_module} ds_ident class_arity
		  (dict_args,_) = mapSt (\type class_cons_vars
								-> let at_attribute = if (class_cons_vars bitand 1<>0) TA_MultiOfPropagatingConsVar TA_Multi
							   		in ({at_attribute = at_attribute, at_type = type}, class_cons_vars>>1)
						   	) tc_types class_cons_vars
		= {at_attribute = TA_Multi, /* at_annotation = AN_Strict, */ at_type = TA dict_type_symb dict_args}

::	ExpandTypeState =
	{	ets_type_defs			:: !.{#{#CheckedTypeDef}}
	,	ets_collected_conses	:: !ImportedConstructors
	,	ets_type_heaps			:: !.TypeHeaps
	,	ets_var_heap			:: !.VarHeap
	,	ets_main_dcl_module_n :: !Int
	,	ets_contains_unexpanded_abs_syn_type :: !Bool
	}

class expandSynTypes a :: !Int !{# CommonDefs} !a !*ExpandTypeState -> (!Bool,!a, !*ExpandTypeState)

instance expandSynTypes Type
where
	expandSynTypes rem_annots common_defs type=:(arg_type --> res_type) ets
		# (changed,(arg_type, res_type), ets) = expandSynTypes rem_annots common_defs (arg_type, res_type) ets
		| changed
			= (True,arg_type --> res_type, ets)
			= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(TB _) ets
		= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(cons_var :@: types) ets
		# (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
		| changed
			= (True,cons_var :@: types, ets)
			= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(TA type_symb types) ets
		= expand_syn_types_in_TA rem_annots common_defs type TA_Multi ets
	expandSynTypes rem_annots common_defs type=:(TAS type_symb types _) ets
		= expand_syn_types_in_TA rem_annots common_defs type TA_Multi ets
	expandSynTypes rem_annots common_defs tfa_type=:(TFA vars type) ets
		# (changed,type, ets) = expandSynTypes rem_annots common_defs type ets
		| changed
			= (True,TFA vars type, ets)
			= (False,tfa_type, ets)
	expandSynTypes rem_annots common_defs type ets
		= (False,type, ets)

instance expandSynTypes [a] | expandSynTypes a
where
	expandSynTypes rem_annots common_defs [] ets
		= (False,[],ets)
	expandSynTypes rem_annots common_defs t=:[type:types] ets
		#! (changed_type,type,ets)		= expandSynTypes rem_annots common_defs type ets
		   (changed_types,types,ets)	= expandSynTypes rem_annots common_defs types ets
		| changed_type || changed_types
			= (True,[type:types],ets)
			= (False,t,ets)

instance expandSynTypes (a,b) | expandSynTypes a & expandSynTypes b
where
	expandSynTypes rem_annots common_defs (type1,type2) ets
		#! (changed_type1,type1,ets) = expandSynTypes rem_annots common_defs type1 ets
		   (changed_type2,type2,ets) = expandSynTypes rem_annots common_defs type2 ets
		= (changed_type1 || changed_type2,(type1,type2),ets)

instance expandSynTypes AType
where
	expandSynTypes rem_annots common_defs atype ets
		= expand_syn_types_in_a_type rem_annots common_defs atype ets
	where
		expand_syn_types_in_a_type :: !.Int !{#.CommonDefs} !.AType !*ExpandTypeState -> (!.Bool,!AType,!.ExpandTypeState)
		expand_syn_types_in_a_type rem_annots common_defs atype=:{at_type = at_type=: TA type_symb types,at_attribute} ets
			# (changed,at_type, ets) = expand_syn_types_in_TA rem_annots common_defs at_type at_attribute ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)
		expand_syn_types_in_a_type rem_annots common_defs atype=:{at_type = at_type=: TAS type_symb types _,at_attribute} ets
			# (changed,at_type, ets) = expand_syn_types_in_TA rem_annots common_defs at_type at_attribute ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)
		expand_syn_types_in_a_type rem_annots common_defs atype ets
			# (changed,at_type, ets) = expandSynTypes rem_annots common_defs atype.at_type ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)

expand_syn_types_in_TA :: !.Int !{#.CommonDefs} !.Type !.TypeAttribute !*ExpandTypeState -> (!Bool,!Type,!.ExpandTypeState)
expand_syn_types_in_TA rem_annots common_defs ta_type attribute ets=:{ets_type_defs}
	# (glob_object,glob_module,types)	= case ta_type of
		(TA type_symb=:{type_index={glob_object,glob_module},type_ident} types)				-> (glob_object,glob_module,types)
		(TAS type_symb=:{type_index={glob_object,glob_module},type_ident} types strictness)	-> (glob_object,glob_module,types)
	# ({td_rhs,td_ident,td_args,td_attribute},ets_type_defs) = ets_type_defs![glob_module].[glob_object]
	  ets = { ets & ets_type_defs = ets_type_defs }
	= case td_rhs of
		SynType rhs_type
			# (type,ets_type_heaps) = bind_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets.ets_type_heaps
			# (_,type,ets) = expandSynTypes rem_annots common_defs type { ets & ets_type_heaps = ets_type_heaps }
			-> (True,type,ets)
		AbstractSynType _ rhs_type
			| (rem_annots bitand ExpandAbstractSynTypesMask)<>0
				# (type,ets_type_heaps) = bind_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets.ets_type_heaps
				# (_,type,ets) = expandSynTypes rem_annots common_defs type { ets & ets_type_heaps = ets_type_heaps }
				-> (True,type,ets)

				# ets = {ets & ets_contains_unexpanded_abs_syn_type=True }
				#! (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
				# ta_type = if changed
								( case ta_type of
									TA  type_symb _				-> TA  type_symb types
									TAS type_symb _ strictness	-> TAS type_symb types strictness
								) ta_type
				| glob_module == ets.ets_main_dcl_module_n
					-> (changed,ta_type, ets)
					-> (changed,ta_type, collect_imported_constructors common_defs glob_module td_rhs ets)
		NewType {ds_index}
			# {cons_type={st_args=[arg_type:_]}} = common_defs.[glob_module].com_cons_defs.[ds_index];
			# (type,ets_type_heaps) = bind_and_substitute_before_expand types td_args td_attribute arg_type rem_annots attribute ets.ets_type_heaps
			# (_,type,ets) = expandSynTypes rem_annots common_defs type { ets & ets_type_heaps = ets_type_heaps }
			-> (True,type,ets)
		_
			#! (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
			# ta_type = if changed
							( case ta_type of
								TA  type_symb _				-> TA  type_symb types
								TAS type_symb _ strictness	-> TAS type_symb types strictness
							) ta_type
			| glob_module == ets.ets_main_dcl_module_n || (rem_annots bitand DontCollectImportedConstructors)<>0
				-> (changed,ta_type, ets)
				-> (changed,ta_type, collect_imported_constructors common_defs glob_module td_rhs ets)
where
	bind_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets_type_heaps
		# ets_type_heaps = bind_attr td_attribute attribute ets_type_heaps
		  ets_type_heaps = fold2St bind_var_and_attr td_args types ets_type_heaps
		= substitute_rhs rem_annots rhs_type.at_type ets_type_heaps
		where
			bind_var_and_attr {	atv_attribute = TA_Var {av_info_ptr},  atv_variable = {tv_info_ptr} } {at_attribute,at_type} type_heaps=:{th_vars,th_attrs}
				= { type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type), th_attrs = th_attrs <:= (av_info_ptr, AVI_Attr at_attribute) }
			bind_var_and_attr { atv_variable  = {tv_info_ptr}} {at_type} type_heaps=:{th_vars}
				= { type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type) }
	
			bind_attr (TA_Var {av_info_ptr}) attribute type_heaps=:{th_attrs}
				= { type_heaps & th_attrs = th_attrs <:= (av_info_ptr, AVI_Attr attribute) }
			bind_attr _ attribute type_heaps
				= type_heaps
	
			substitute_rhs rem_annots rhs_type type_heaps
				| (rem_annots bitand RemoveAnnotationsMask)<>0
					# (_, rhs_type) = removeAnnotations rhs_type
				  	= substitute rhs_type type_heaps
				  	= substitute rhs_type type_heaps

	collect_imported_constructors :: !{#.CommonDefs} !.Int !.TypeRhs !*ExpandTypeState -> .ExpandTypeState
	collect_imported_constructors common_defs mod_index (RecordType {rt_constructor}) ets=:{ets_collected_conses,ets_var_heap}
		# (ets_collected_conses, ets_var_heap)
				= collect_imported_constructor mod_index common_defs.[mod_index].com_cons_defs rt_constructor (ets_collected_conses, ets_var_heap)
		= { ets & ets_collected_conses = ets_collected_conses, ets_var_heap = ets_var_heap }
	collect_imported_constructors common_defs mod_index (AlgType constructors) ets=:{ets_collected_conses,ets_var_heap}
		# (ets_collected_conses, ets_var_heap) 
				= foldSt (collect_imported_constructor mod_index common_defs.[mod_index].com_cons_defs) constructors (ets_collected_conses, ets_var_heap)
		= { ets & ets_collected_conses = ets_collected_conses, ets_var_heap = ets_var_heap }
	collect_imported_constructors common_defs mod_index _ ets
		= ets
	
	collect_imported_constructor :: !.Int !{#.ConsDef} !.DefinedSymbol !*(!u:[v:(Global .Int)],!*(Heap VarInfo)) -> (!w:[x:(Global Int)],!.(Heap VarInfo)), [u <= w,v <= x]
	collect_imported_constructor mod_index cons_defs {ds_index} (collected_conses, var_heap)
		# {cons_type_ptr} = cons_defs.[ds_index]
		  (type_info, var_heap) = readVarInfo cons_type_ptr var_heap
		| has_been_collected type_info
			= (collected_conses, var_heap)
			= ([{ glob_module = mod_index, glob_object = ds_index } : collected_conses ], writeVarInfo cons_type_ptr VI_Used var_heap)
	where
		has_been_collected VI_Used				= True
		has_been_collected (VI_ExpandedType _)	= True
		has_been_collected _					= False

//@	freeVariables

class clearVariables expr :: !expr !*VarHeap -> *VarHeap

instance clearVariables [a] | clearVariables a
where
	clearVariables list fvi
		= foldSt clearVariables list fvi

instance clearVariables LetBind
where
	clearVariables {lb_src} fvi
		= clearVariables lb_src fvi

instance clearVariables (Bind a b) | clearVariables a
where
	clearVariables {bind_src} fvi
		= clearVariables bind_src fvi

instance clearVariables (Optional a) | clearVariables a
where
	clearVariables (Yes x) fvi
		= clearVariables x fvi
	clearVariables No fvi
		= fvi

instance clearVariables BoundVar
where
	clearVariables bound_var=:{var_info_ptr} var_heap
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_UsedVar _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_LocalVar				-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Empty				-> var_heap
			VI_Expression _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Dictionary _ _ _		-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Variable _ _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_AccVar _ _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_ExpressionOrBody _ _	_ _
				-> writeVarInfo var_info_ptr VI_Empty var_heap

instance clearVariables Expression
where
	clearVariables (Var var) fvi
		= clearVariables var fvi
	clearVariables (App {app_args}) fvi
		= clearVariables app_args fvi
	clearVariables (fun @ args) fvi
		= clearVariables args (clearVariables fun fvi)
	clearVariables (Let {let_strict_binds,let_lazy_binds,let_expr}) fvi
		# fvi = clearVariables let_strict_binds fvi
		  fvi = clearVariables let_lazy_binds fvi
		  fvi = clearVariables let_expr fvi
		= fvi
	clearVariables (Case {case_expr,case_guards,case_default}) fvi
		# fvi = clearVariables case_expr fvi
		  fvi = clearVariables case_guards fvi
		  fvi = clearVariables case_default fvi
		= fvi
	clearVariables (Selection _ expr selectors) fvi
		= clearVariables expr (clearVariables selectors fvi)
	clearVariables (Update expr1 selectors expr2) fvi
		= clearVariables expr2 (clearVariables selectors (clearVariables expr1 fvi))
	clearVariables (RecordUpdate cons_symbol expression expressions) fvi
		= clearVariables expression (clearVariables expressions fvi)
	clearVariables (TupleSelect _ arg_nr expr) fvi
		= clearVariables expr fvi
	clearVariables (MatchExpr _ expr) fvi
		= clearVariables expr fvi
	clearVariables EE fvi
		= fvi
	clearVariables _ fvi
		= fvi

instance clearVariables CasePatterns
where
	clearVariables (AlgebraicPatterns _ alg_patterns) fvi
		= foldSt clearVariables alg_patterns fvi
	clearVariables (BasicPatterns _ basic_patterns) fvi
		= foldSt clearVariables basic_patterns fvi
	clearVariables (OverloadedListPatterns _ _ alg_patterns) fvi
		= foldSt clearVariables alg_patterns fvi

instance clearVariables BasicPattern
where
	clearVariables {bp_expr} fvi
		= clearVariables bp_expr fvi

instance clearVariables AlgebraicPattern
where
	clearVariables {ap_vars, ap_expr} fvi
		= clearVariables ap_expr fvi
		
instance clearVariables Selection
where
	clearVariables (RecordSelection _ _) fvi
		= fvi
	clearVariables (ArraySelection _ _ expr) fvi
		= clearVariables expr fvi
	clearVariables (DictionarySelection dict_var selections _ expr) fvi
		= clearVariables dict_var (clearVariables selections (clearVariables expr fvi))
	
////////////////

::	FreeVarInfo =
	{	fvi_var_heap	:: !.VarHeap
	,	fvi_expr_heap	:: !.ExpressionHeap
	,	fvi_variables	:: ![BoundVar]
	,	fvi_expr_ptrs	:: ![ExprInfoPtr]
	}

class freeVariables expr ::  !expr !*FreeVarInfo -> *FreeVarInfo

instance freeVariables [a] | freeVariables a
where
	freeVariables list fvi
		= foldSt freeVariables list fvi

instance freeVariables LetBind
where
	freeVariables {lb_src} fvi
		= freeVariables lb_src fvi

instance freeVariables (Bind a b) | freeVariables a
where
	freeVariables {bind_src} fvi
		= freeVariables bind_src fvi

instance freeVariables (Optional a) | freeVariables a
where
	freeVariables (Yes x) fvi
		= freeVariables x fvi
	freeVariables No fvi
		= fvi

instance freeVariables BoundVar
where
	freeVariables bound_var=:{var_info_ptr} fvi=:{fvi_var_heap, fvi_variables}
		# (var_info, fvi_var_heap) = readVarInfo var_info_ptr fvi_var_heap
		  (fvi_variables, fvi_var_heap) = adjust_var_info bound_var var_info fvi_variables fvi_var_heap
		= {fvi & fvi_variables = fvi_variables, fvi_var_heap = fvi_var_heap }
	where
		adjust_var_info _ (VI_UsedVar _) fvi_variables fvi_var_heap
			= (fvi_variables, fvi_var_heap)
		adjust_var_info bound_var=:{var_ident} _ fvi_variables fvi_var_heap
			= ([bound_var : fvi_variables], writeVarInfo var_info_ptr (VI_UsedVar var_ident) fvi_var_heap)

instance freeVariables Expression
where
	freeVariables (Var var) fvi
		= freeVariables var fvi
	freeVariables (App {app_args}) fvi
		= freeVariables app_args fvi
	freeVariables (fun @ args) fvi
		= freeVariables args (freeVariables fun fvi)
	freeVariables (Let {let_strict_binds,let_lazy_binds,let_expr,let_info_ptr}) fvi=:{fvi_variables = global_variables}
		# let_binds = let_strict_binds ++ let_lazy_binds
		  (removed_variables, fvi_var_heap) = removeVariables global_variables fvi.fvi_var_heap
		  fvi = freeVariables let_binds { fvi & fvi_variables = [], fvi_var_heap = fvi_var_heap }
		  {fvi_expr_heap, fvi_variables, fvi_var_heap, fvi_expr_ptrs} = freeVariables let_expr fvi
		  (fvi_variables, fvi_var_heap) = removeLocalVariables [lb_dst \\ {lb_dst} <- let_binds] fvi_variables [] fvi_var_heap		
		  (unbound_variables, fvi_var_heap) = determineGlobalVariables fvi_variables fvi_var_heap
		  (fvi_variables, fvi_var_heap) = restoreVariables removed_variables fvi_variables fvi_var_heap
		  (let_info, fvi_expr_heap) = readPtr let_info_ptr fvi_expr_heap
		= { fvi & fvi_variables = fvi_variables
		  , fvi_var_heap = fvi_var_heap
		  , fvi_expr_heap = fvi_expr_heap
		  , fvi_expr_ptrs = [let_info_ptr : fvi_expr_ptrs]
		  }
	freeVariables (Case kees) fvi
		= freeVariablesOfCase kees fvi
	where
		freeVariablesOfCase {case_expr,case_guards,case_default, case_info_ptr} fvi=:{fvi_variables, fvi_var_heap}
			# (removed_variables, fvi_var_heap) = removeVariables fvi_variables fvi_var_heap
			  fvi = free_variables_of_guards case_guards { fvi & fvi_variables = [], fvi_var_heap = fvi_var_heap }
			  {fvi_expr_heap, fvi_variables, fvi_var_heap, fvi_expr_ptrs} = freeVariables case_default fvi
			  (unbound_variables, fvi_var_heap) = determineGlobalVariables fvi_variables fvi_var_heap
			  (fvi_variables, fvi_var_heap) = restoreVariables removed_variables fvi_variables fvi_var_heap
			  (case_info, fvi_expr_heap) = readPtr case_info_ptr fvi_expr_heap
			= freeVariables case_expr { fvi & fvi_variables = fvi_variables, fvi_var_heap = fvi_var_heap,
					fvi_expr_heap = set_aci_free_vars_info_case unbound_variables case_info_ptr fvi_expr_heap,
					fvi_expr_ptrs = [case_info_ptr : fvi_expr_ptrs] }
		where
			free_variables_of_guards (AlgebraicPatterns _ alg_patterns) fvi
				= foldSt free_variables_of_alg_pattern alg_patterns fvi
			free_variables_of_guards (BasicPatterns _ basic_patterns) fvi
				= foldSt free_variables_of_basic_pattern basic_patterns fvi
			where
				free_variables_of_basic_pattern {bp_expr} fvi
					= freeVariables bp_expr fvi
			free_variables_of_guards (OverloadedListPatterns _ _ alg_patterns) fvi
				= foldSt free_variables_of_alg_pattern alg_patterns fvi
		
			free_variables_of_alg_pattern {ap_vars, ap_expr} fvi=:{fvi_variables}
				# fvi = freeVariables ap_expr { fvi & fvi_variables = [] }
				  (fvi_variables, fvi_var_heap) = removeLocalVariables ap_vars fvi.fvi_variables fvi_variables fvi.fvi_var_heap
				= { fvi & fvi_var_heap = fvi_var_heap, fvi_variables = fvi_variables }
		
	freeVariables (Selection _ expr selectors) fvi
		= freeVariables selectors (freeVariables expr fvi)
	freeVariables (Update expr1 selectors expr2) fvi
		= freeVariables expr2 (freeVariables selectors (freeVariables expr1 fvi))
	freeVariables (RecordUpdate cons_symbol expression expressions) fvi
		= freeVariables expressions (freeVariables expression fvi)
	freeVariables (TupleSelect _ arg_nr expr) fvi
		= freeVariables expr fvi
	freeVariables (MatchExpr _ expr) fvi
		= freeVariables expr fvi
	freeVariables EE fvi
		= fvi
	freeVariables _ fvi
		= fvi

instance freeVariables Selection
where
	freeVariables (RecordSelection _ _) fvi
		= fvi
	freeVariables (ArraySelection _ _ expr) fvi
		= freeVariables expr fvi
	freeVariables (DictionarySelection dict_var selections _ expr) fvi
		= freeVariables dict_var (freeVariables selections (freeVariables expr fvi))
	
removeVariables global_variables var_heap
	= foldSt remove_variable global_variables ([], var_heap)
where
	remove_variable v=:{var_info_ptr} (removed_variables, var_heap)
		# (VI_UsedVar used_var, var_heap) = readVarInfo var_info_ptr var_heap
		= ([(v, used_var) : removed_variables], writeVarInfo var_info_ptr VI_Empty var_heap)

restoreVariables removed_variables global_variables var_heap
	= foldSt restore_variable removed_variables (global_variables, var_heap)
where
	restore_variable (v=:{var_info_ptr}, var_id) (restored_variables, var_heap)
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_UsedVar _
				-> (restored_variables, var_heap)
			_
				-> ([ v : restored_variables ], writeVarInfo var_info_ptr (VI_UsedVar var_id) var_heap)

determineGlobalVariables global_variables var_heap
	= foldSt determine_global_variable global_variables ([], var_heap)
where		
	determine_global_variable {var_info_ptr} (global_variables, var_heap)
		# (VI_UsedVar v_name, var_heap) = readVarInfo var_info_ptr var_heap
		= ([{var_ident = v_name, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr} : global_variables], var_heap)

removeLocalVariables local_variables all_variables global_variables var_heap
	# var_heap = foldSt mark_local_var local_variables var_heap
	= foldSt filter_local_var all_variables (global_variables, var_heap)
where
	mark_local_var {fv_info_ptr} var_heap
		= writeVarInfo fv_info_ptr VI_LocalVar var_heap

	filter_local_var v=:{var_info_ptr} (global_vars, var_heap)
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_LocalVar
				-> (global_vars, var_heap)
			_
				-> ([ v : global_vars ], var_heap)

//@ fun_def & cons_arg getters...

get_fun_def :: !SymbKind !Int !u:{#FunDef} !*FunctionHeap -> (!FunDef, !u:{#FunDef}, !*FunctionHeap)
get_fun_def (SK_Function {glob_module, glob_object}) main_dcl_module_n fun_defs fun_heap
	| glob_module<>main_dcl_module_n
		= abort "sanity check 2 failed in module trans"
	# (fun_def, fun_defs) = fun_defs![glob_object]
	= (fun_def, fun_defs, fun_heap)
get_fun_def (SK_LocalMacroFunction glob_object) main_dcl_module_n fun_defs fun_heap
	# (fun_def, fun_defs) = fun_defs![glob_object]
	= (fun_def, fun_defs, fun_heap)
get_fun_def (SK_GeneratedFunction fun_ptr _) main_dcl_module_n fun_defs fun_heap
	# (FI_Function {gf_fun_def}, fun_heap) = readPtr fun_ptr fun_heap
	= (gf_fun_def, fun_defs, fun_heap)

get_fun_def_and_cons_args :: !SymbKind !v:{!ConsClasses} !u:{#FunDef} !*FunctionHeap
		    -> (!FunDef, !ConsClasses, !v:{!ConsClasses},!u:{#FunDef},!*FunctionHeap)
get_fun_def_and_cons_args (SK_Function {glob_object}) cons_args fun_defs fun_heap
//	| glob_object >= size fun_defs
//		= abort "get_fun_def_and_cons_args:SK_Function"
	# (fun_def, fun_defs) = fun_defs![glob_object]
	# (fun_args, cons_args) = cons_args![glob_object]
	= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
get_fun_def_and_cons_args (SK_LocalMacroFunction glob_object) cons_args fun_defs fun_heap
//	| glob_object >= size fun_defs
//		= abort "get_fun_def_and_cons_args:SK_LocalMacroFunction"
	# (fun_def, fun_defs) = fun_defs![glob_object]
	# (fun_args, cons_args) = cons_args![glob_object]
	= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
get_fun_def_and_cons_args (SK_GeneratedFunction fun_info_ptr fun_index) cons_args fun_defs fun_heap
	| fun_index < size fun_defs
		# (fun_def, fun_defs) = fun_defs![fun_index]
//		| fun_index >= size cons_args
//			= abort "get_fun_def_and_cons_args:cons_args"
		# (fun_args, cons_args) = cons_args![fun_index]
		= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
	# (FI_Function {gf_fun_def, gf_cons_args}, fun_heap) = readPtr fun_info_ptr fun_heap
	= (gf_fun_def, gf_cons_args, cons_args, fun_defs, fun_heap)

//@ <<<
/*
instance <<< Group where
	(<<<) file {group_members}
	 = file <<< "Group: " <<< group_members
*/

instance <<< RootCaseMode where
	(<<<) file mode = case mode of NotRootCase -> file <<< "NotRootCase"; RootCase -> file <<< "RootCase"; RootCaseOfZombie -> file <<< "RootCaseOfZombie";

/*
instance <<< InstanceInfo
where
	(<<<) file (II_Node prods _ left right) = file <<< left <<< prods <<< right 
	(<<<) file II_Empty = file 

instance <<< Producer
where
	(<<<) file (PR_Function symbol _ index)
		= file <<< "(F)" <<< symbol.symb_ident
	(<<<) file (PR_GeneratedFunction symbol _ index)
		= file <<< "(G)" <<< symbol.symb_ident <<< index
	(<<<) file PR_Empty = file <<< 'E'
	(<<<) file (PR_Class app vars type) = file <<< "(Class(" <<< App app<<<","<<< type <<< "))"
	(<<<) file (PR_Curried {symb_ident, symb_kind} _) = file <<< "(Curried)" <<< symb_ident <<< symb_kind
	(<<<) file _ = file
*/
instance <<< Producer where
	(<<<) file PR_Empty
		= file <<< "(E)"
	(<<<) file PR_Unused
		= file <<< "(U)"
	(<<<) file (PR_Function ident int index)
		= file <<< "(F:" <<< ident <<< ")"
	(<<<) file (PR_Class app binds type)
		= file <<< "(O::" <<< app.app_symb <<< ")"
	(<<<) file (PR_Constructor ident int exprl)
		= file <<< "(C:" <<< ident <<< ")"
	(<<<) file (PR_GeneratedFunction ident int index)
		= file <<< "(G:" <<< ident <<< ")"
	(<<<) file (PR_Curried ident int)
		= file <<< "(P:" <<< ident <<< ")"
	(<<<) file (PR_CurriedFunction ident int index)
		= file <<< "(CF:" <<< ident <<< ")"

instance <<< {!a} | <<< a
where
	(<<<) file array
		# file = file <<< "{"
		= showBody 0 (size array) array file
	where
		showBody i m a f
			| i >= m	= f <<< "}"
						= showBody (inc i) m a (f <<< a.[i] <<< ", ")

instance <<< SymbKind
where
	(<<<) file SK_Unknown = file <<< "(SK_Unknown)"
	(<<<) file (SK_Function gi) = file <<< "(SK_Function)" <<< gi
	(<<<) file (SK_IclMacro gi) = file <<< "(SK_IclMacro)" <<< gi
	(<<<) file (SK_LocalMacroFunction gi) = file <<< "(SK_LocalMacroFunction)" <<< gi
	(<<<) file (SK_DclMacro gi) = file <<< "(SK_DclMacro)" <<< gi
	(<<<) file (SK_LocalDclMacroFunction gi) = file <<< "(SK_LocalDclMacroFunction)" <<< gi
	(<<<) file (SK_OverloadedFunction gi) = file <<< "(SK_OverloadedFunction)" <<< gi
	(<<<) file (SK_GeneratedFunction _ gi) = file <<< "(SK_GeneratedFunction)" <<< gi
	(<<<) file (SK_Constructor gi) = file <<< "(SK_Constructor)" <<< gi
	(<<<) file (SK_Generic gi tk) = file <<< "(SK_Constructor)" <<< gi
	(<<<) file SK_TypeCode = file <<< "(SK_TypeCode)"
	(<<<) file _ = file <<< "(SK_UNKNOWN)"
	
instance <<< ConsClasses
where
	(<<<) file {cc_args,cc_linear_bits,cc_producer} = file <<< cc_args <<< cc_linear_bits <<< cc_producer
	
instance <<< InstanceInfo
  where
	(<<<) file ii = (write_ii ii (file <<< "[")) <<< "]"
	  where
		write_ii II_Empty file
			= file
		write_ii (II_Node producers _ l r) file
			# file = write_ii l file <<< "("
			  file = foldSt (\pr file -> file<<<pr<<<",") [el \\ el<-:producers] file
			= write_ii r (file<<<")")

instance <<< (Ptr a)
where
	(<<<) file p = file <<< ptrToInt p

instance <<< SymbIdent
where
	(<<<) file symb=:{symb_kind = SK_Function symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_LocalMacroFunction symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_GeneratedFunction _ symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_OverloadedFunction symb_index }
		= file <<< symb.symb_ident <<<  "[o]@" <<< symb_index
	(<<<) file symb
		= file <<< symb.symb_ident 
/*
instance <<< {!Type}
where
	(<<<) file subst
		= file <<< "{"<<<[s\\s<-:subst] <<< "}\n"
*/
// SPECIAL...
instance <<< Specials
where
	(<<<) file spec = case spec of
		(SP_ParsedSubstitutions 	_)	-> file <<< "SP_ParsedSubstitutions"
		(SP_Substitutions 		 	_)	-> file <<< "SP_Substitutions"
		(SP_ContextTypes			l)	-> file <<< "(SP_ContextTypes: " <<< l <<< ")"
		(SP_TypeOffset				_)	-> file <<< "SP_TypeOffset"
		SP_None							-> file <<< "SP_None"

instance <<< Special
where
	(<<<) file {spec_index,spec_types,spec_vars,spec_attrs}
		= file <<< "spec_index" <<< spec_index <<< "spec_types" <<< spec_types <<< "spec_vars" <<< spec_vars <<< "spec_attrs" <<< spec_attrs

instance <<< ExprInfo
where
	(<<<) file EI_Empty = file <<< "EI_Empty"
	(<<<) file (EI_DictionaryType t) = file <<< "<EI_DictionaryType: " <<< t <<< ">"
//	(<<<) file (EI_Instance symb exprs) = file <<< symb <<< exprs
//	(<<<) file (EI_Selection sels var_ptr exprs) = file <<< sels <<< var_ptr <<< exprs
//	(<<<) file (EI_Context exprs) = file <<< exprs
	(<<<) file _ = file <<< "EI_Other"

instance <<< TypeContext
where
	(<<<) file co = file <<< co.tc_class <<< " " <<< co.tc_types <<< " <" <<< co.tc_var <<< '>'

resolveContext :: ![TypeContext] ![ExprInfo] -> [[Type]]
resolveContext [tc:tcs] [EI_DictionaryType t:eis]
	= minimiseContext tc t ++ resolveContext tcs eis
resolveContext _ _ = []

minimiseContext {tc_class = TCClass gds} (TA ti ts)
	# tc_index = {glob_module = gds.glob_module, glob_object = gds.glob_object.ds_index}
	| tc_index == ti.type_index
		= [[at_type \\ {at_type} <- ts]]
		= []
minimiseContext _ _ = []

findInstInSpecials :: ![[.Type]] ![.Special] -> .(!Int,!(Global Int))
findInstInSpecials insts []
	= (0,{glob_object= -1,glob_module = -1})
findInstInSpecials insts [{spec_types,spec_index}:specials]
	| matchTypes insts spec_types
		= (length spec_types, spec_index)
	= findInstInSpecials insts specials

matchTypes [] [] = True
matchTypes [l:ls] [r:rs]
	= l == r && matchTypes ls rs
matchTypes _ _ = False

foundSpecial {glob_object= -1,glob_module = -1}	= False
foundSpecial _ = True	

// ...SPECIAL

arity_warning msg symb_ident fun_index fun_arity ti
	| fun_arity <= 32
		= ti
	= {ti & ti_error_file = ti.ti_error_file <<< "Warning: Arity > 32 " <<< msg <<< " " <<< fun_arity <<< " " <<< symb_ident <<< "@" <<< fun_index <<< "\n"}

strip_universal_quantor :: SymbolType -> SymbolType
strip_universal_quantor st=:{st_vars,st_args,st_result}
	# (st_result,st_vars)	= strip st_result st_vars
	# (st_args,st_vars)		= mapSt strip st_args st_vars
	= {st & st_vars = st_vars, st_args = st_args, st_result = st_result}
where
	strip :: AType [TypeVar] -> (AType,[TypeVar])
	strip atype=:{at_type = TFA vars type} tvs
		= ({atype & at_type = type}, map (\{atv_variable}->atv_variable) vars ++ tvs)
	strip atype=:{at_type = TFAC vars type contexts} tvs
		= ({atype & at_type = type}, map (\{atv_variable}->atv_variable) vars ++ tvs)
	strip atype tvs
		= (atype,tvs)

mapOpt f [Yes a:x]	= [Yes (f a):mapOpt f x]
mapOpt f [No:x]		= [No:mapOpt f x]
mapOpt f [] 		= []

class copy a :: !a !CopyInfo !*CopyState -> (!a, !*CopyState)

instance copy Expression
where
	copy (Var var) ci cs
		= copyVariable var ci cs
	copy (App app) ci cs
		# (app, cs) = copy app ci cs
		= (App app, cs)
	copy (expr @ exprs) ci cs
		# ((expr,exprs), cs) = copy (expr,exprs) ci cs
		= (expr @ exprs, cs)
	copy (Let lad) ci cs
		# (lad, cs) = copy lad ci cs
		= (Let lad, cs)
	copy (Case case_expr) ci cs
		# (case_expr, cs) = copy case_expr ci cs
		= (Case case_expr, cs)
	copy (Selection selector_kind=:NormalSelector (Var var) selectors=:[RecordSelection _ field_n]) ci cs	
		# (var_info,var_heap) = readVarInfo var.var_info_ptr cs.cs_var_heap
		  cs = {cs & cs_var_heap=var_heap}
		= case var_info of
			VI_Expression expr
				-> (Selection selector_kind expr selectors, cs)
			VI_Variable var_ident var_info_ptr
			 	# (var_expr_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
				  expr = Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr}
				-> (Selection selector_kind expr selectors, {cs & cs_symbol_heap = cs_symbol_heap})
			VI_Dictionary app_symb app_args class_type
				# (expr,cs) = copy_dictionary_variable app_symb app_args class_type ci cs
				-> (Selection selector_kind expr selectors, cs)
			VI_Body fun_ident {tb_args, tb_rhs} new_aci_params
				# tb_args_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-tb_args ] 
				  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
				  cs_var_heap = bind_vars tb_args_ptrs new_aci_params cs_var_heap
				  cs = { cs & cs_var_heap = cs_var_heap }
				-> case tb_rhs of
					App {app_symb={symb_kind=SK_Constructor _},app_args}
						# (expr,cs) = copy (app_args!!field_n) ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						-> (expr, {cs & cs_var_heap = cs_var_heap})
					_
						# (expr,cs) = copy tb_rhs ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						-> (Selection selector_kind expr selectors, {cs & cs_var_heap = cs_var_heap})
			VI_ExpressionOrBody expr _ _ _
				-> (Selection selector_kind expr selectors, cs)
			_
				-> (Selection selector_kind (Var var) selectors, cs)
	copy (Selection selector_kind expr selectors) ci cs
		# ((expr, selectors), cs) = copy (expr, selectors) ci cs
		= (Selection selector_kind expr selectors, cs)
	copy (Update expr1 selectors expr2) ci cs
		# (((expr1, expr2), selectors), cs) = copy ((expr1, expr2), selectors) ci cs
		= (Update expr1 selectors expr2, cs)
	copy (RecordUpdate cons_symbol expression expressions) ci cs
		# ((expression, expressions), cs) = copy (expression, expressions) ci cs
		= (RecordUpdate cons_symbol expression expressions, cs)
	copy (TupleSelect symbol argn_nr expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (TupleSelect symbol argn_nr expr, cs)
	copy (MatchExpr cons_ident expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (MatchExpr cons_ident expr, cs)
	copy (DynamicExpr expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (DynamicExpr expr, cs)
	copy (TypeSignature type_function expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (TypeSignature type_function expr, cs)
	copy expr ci cs
		= (expr, cs)

copyVariable :: !BoundVar CopyInfo !*CopyState -> (!Expression, !*CopyState)
copyVariable var=:{var_info_ptr} ci cs
	# (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
	  cs = {cs & cs_var_heap=var_heap}
	= case var_info of
		VI_Expression expr
			-> (expr, cs)
		VI_Variable var_ident var_info_ptr
		 	# (var_expr_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
			-> (Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr}, { cs & cs_symbol_heap = cs_symbol_heap})
		VI_Body fun_ident _ vars
			-> (App {	app_symb = fun_ident,
						app_args = [ Var { var_ident=fv_ident, var_info_ptr=fv_info_ptr, var_expr_ptr=nilPtr }
									\\ {fv_ident,fv_info_ptr}<-vars],
						app_info_ptr = nilPtr }, cs)
		VI_Dictionary app_symb app_args class_type
			-> copy_dictionary_variable app_symb app_args class_type ci cs
		VI_ExpressionOrBody expr _ _ _
			-> (expr, cs)
		_
			-> (Var var, cs)

copy_dictionary_variable app_symb app_args class_type ci cs
	# (new_class_type, cs_opt_type_heaps) = substitute_class_types class_type cs.cs_opt_type_heaps
	  (new_info_ptr, cs_symbol_heap) = newPtr (EI_DictionaryType new_class_type) cs.cs_symbol_heap
	  app = App { app_symb = app_symb, app_args = app_args, app_info_ptr = new_info_ptr }
	  cs = { cs & cs_opt_type_heaps = cs_opt_type_heaps, cs_symbol_heap = cs_symbol_heap }
	= copy app ci cs
  where
	substitute_class_types class_types No
		= (class_types, No)
	substitute_class_types class_types (Yes type_heaps)
		# (new_class_types, type_heaps) = substitute class_types type_heaps
		= (new_class_types, Yes type_heaps)

instance copy DynamicExpr
where
	copy expr=:{dyn_expr, dyn_info_ptr} ci cs=:{cs_symbol_heap}
		# (dyn_info, cs_symbol_heap) = readPtr dyn_info_ptr cs_symbol_heap
		# (new_dyn_info_ptr, cs_symbol_heap) = newPtr dyn_info cs_symbol_heap
		# (dyn_expr, cs) = copy dyn_expr ci {cs & cs_symbol_heap=cs_symbol_heap}
		= ({ expr & dyn_expr = dyn_expr, dyn_info_ptr = new_dyn_info_ptr }, cs)

instance copy Selection
where
	copy (ArraySelection array_select expr_ptr index_expr) ci cs=:{cs_symbol_heap}
		# (new_ptr, cs_symbol_heap) = newPtr EI_Empty cs_symbol_heap
		  (index_expr, cs) = copy index_expr ci { cs & cs_symbol_heap = cs_symbol_heap}
		= (ArraySelection array_select new_ptr index_expr, cs)
	copy (DictionarySelection var selectors expr_ptr index_expr) ci cs=:{cs_symbol_heap}
		# (new_ptr, cs_symbol_heap) = newPtr EI_Empty cs_symbol_heap
		  (index_expr, cs) = copy index_expr ci { cs & cs_symbol_heap = cs_symbol_heap}
		  (var_expr, cs) = copyVariable var ci cs
		= case var_expr of 
			App {app_symb={symb_kind= SK_Constructor _ }, app_args}
				# [RecordSelection _ field_index:_] = selectors
				  (App { app_symb = {symb_ident, symb_kind = SK_Function array_select}}) =  app_args !! field_index
				-> (ArraySelection { array_select & glob_object = { ds_ident = symb_ident, ds_arity = 2, ds_index = array_select.glob_object}}
							new_ptr index_expr, cs)
			Var var
				-> (DictionarySelection var selectors new_ptr index_expr, cs)
	copy record_selection ci cs
		= (record_selection, cs)

instance copy FreeVar
where
	copy fv=:{fv_info_ptr,fv_ident} ci cs=:{cs_var_heap}
		# (new_info_ptr, cs_var_heap) = newPtr VI_Empty cs_var_heap
		= ({ fv & fv_info_ptr = new_info_ptr }, { cs & cs_var_heap = writePtr fv_info_ptr (VI_Variable fv_ident new_info_ptr) cs_var_heap })

instance copy App
where
	copy app=:{app_symb={symb_kind}, app_args, app_info_ptr} ci cs
		= case symb_kind of
			SK_Function {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_IclMacro macro_index
				-> copy_function_app app ci cs
			SK_DclMacro {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_OverloadedFunction {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_Generic {glob_module,glob_object} kind
				-> copy_function_app app ci cs
			SK_LocalMacroFunction local_macro_function_n
				-> copy_function_app app ci cs
			SK_LocalDclMacroFunction {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_Constructor _
				| not (isNilPtr app_info_ptr)
					# (app_info, cs_symbol_heap) = readPtr app_info_ptr cs.cs_symbol_heap
					  (new_app_info, cs_opt_type_heaps) = substitute_EI_DictionaryType app_info cs.cs_opt_type_heaps
					  (new_info_ptr, cs_symbol_heap) = newPtr new_app_info cs_symbol_heap
					  cs={ cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps }
					  (app_args, cs) = copy app_args ci cs
					-> ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, cs) 
					# (app_args, cs) = copy app_args ci cs
					-> ({ app & app_args = app_args}, cs)
			_
				# (app_args, cs) = copy app_args ci cs
				-> ({ app & app_args = app_args, app_info_ptr = nilPtr}, cs) 
	where
		copy_function_app app=:{app_args, app_info_ptr} ci cs
			# (new_info_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
			# cs={ cs & cs_symbol_heap = cs_symbol_heap }
			# (app_args, cs) = copy app_args ci cs
			= ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, cs) 

		substitute_EI_DictionaryType (EI_DictionaryType class_type) (Yes type_heaps)
			# (new_class_type, type_heaps) = substitute class_type type_heaps
			= (EI_DictionaryType new_class_type, Yes type_heaps)
		substitute_EI_DictionaryType x opt_type_heaps
			= (x, opt_type_heaps)

instance copy LetBind
where
	copy bind=:{lb_src} ci cs
		# (lb_src, cs) = copy lb_src ci cs
		= ({ bind & lb_src = lb_src }, cs)

instance copy (Bind a b) | copy a
where
	copy bind=:{bind_src} ci cs
		# (bind_src, cs) = copy bind_src ci cs
		= ({ bind & bind_src = bind_src }, cs)

instance copy Case
where
	copy kees=:{case_expr,case_guards,case_default,case_info_ptr} ci cs=:{cs_cleanup_info}
		# (old_case_info, cs_symbol_heap) = readPtr case_info_ptr cs.cs_symbol_heap
		  (new_case_info, cs_opt_type_heaps) = substitute_let_or_case_type old_case_info cs.cs_opt_type_heaps
		  (new_info_ptr, cs_symbol_heap) = newPtr new_case_info cs_symbol_heap
		  cs_cleanup_info = case old_case_info of
								EI_Extended _ _	-> [new_info_ptr:cs_cleanup_info]
								_				-> cs_cleanup_info
		  cs = { cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps, cs_cleanup_info=cs_cleanup_info }
		  ((case_guards,case_default), cs) = copy (case_guards,case_default) ci cs
		  (case_expr, cs) = update_active_case_info_and_copy case_expr new_info_ptr cs
		= ({ kees & case_expr = case_expr,case_guards = case_guards, case_default = case_default, case_info_ptr = new_info_ptr}, cs)
	where
		update_active_case_info_and_copy case_expr=:(Var {var_info_ptr}) case_info_ptr cs
			# (case_info, cs_symbol_heap) = readPtr case_info_ptr cs.cs_symbol_heap
			  cs = { cs & cs_symbol_heap = cs_symbol_heap }
			= case case_info of
				EI_Extended (EEI_ActiveCase aci=:{aci_free_vars}) ei
					# (new_aci_free_vars, cs) = case ci.ci_handle_aci_free_vars of
													LeaveAciFreeVars
														-> (aci_free_vars, cs)
													RemoveAciFreeVars
														-> (No, cs)
													SubstituteAciFreeVars
														-> case aci_free_vars of
																No		-> (No, cs)
																Yes fvs	# (fvs_subst, cs) = mapSt copyBoundVar fvs cs
																		-> (Yes fvs_subst, cs)
					  (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
					  cs = {cs & cs_var_heap=var_heap}
					-> case var_info of
						VI_Body fun_ident {tb_args, tb_rhs} new_aci_params
							# tb_args_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-tb_args ] 
							  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
							  cs_var_heap = bind_vars tb_args_ptrs new_aci_params cs_var_heap
							  (tb_rhs, cs) = copy tb_rhs ci { cs & cs_var_heap = cs_var_heap }
							  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
							  new_aci = { aci & aci_params = new_aci_params, aci_opt_unfolder = Yes fun_ident, aci_free_vars = new_aci_free_vars }
							  new_eei = (EI_Extended (EEI_ActiveCase new_aci) ei)
							  cs_symbol_heap = writePtr case_info_ptr new_eei cs.cs_symbol_heap
							-> (tb_rhs, { cs & cs_var_heap = cs_var_heap, cs_symbol_heap = cs_symbol_heap })
						_	# new_eei = EI_Extended (EEI_ActiveCase { aci & aci_free_vars = new_aci_free_vars }) ei
							  cs_symbol_heap = writePtr case_info_ptr new_eei cs.cs_symbol_heap
							-> copy case_expr ci { cs & cs_symbol_heap = cs_symbol_heap }
				_	-> copy case_expr ci cs
		update_active_case_info_and_copy (Var var=:{var_info_ptr} @ exprs) case_info_ptr cs
			# (exprs,cs) = copy exprs ci cs
			| is_var_list exprs
				# (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
				  cs = {cs & cs_var_heap=var_heap}
				= case var_info of
					VI_ExpressionOrBody _ fun_ident {tb_args, tb_rhs} new_aci_params
						# tb_args_ptrs = [fv_info_ptr \\ {fv_info_ptr}<-tb_args] 
						  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
						  (extra_exprs,cs_var_heap) = bind_variables tb_args_ptrs new_aci_params exprs cs_var_heap
						  cs = {cs & cs_var_heap = cs_var_heap}
						  (expr,cs) = copy tb_rhs ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						  cs = {cs & cs_var_heap = cs_var_heap}
						-> case extra_exprs of
							[]
								-> (expr,cs)
							extra_exprs
								-> (expr @ extra_exprs, cs)
						where
							bind_variables :: [VarInfoPtr] [FreeVar] [Expression] *VarHeap -> (![Expression],!*VarHeap)
							bind_variables [fv_info_ptr:arg_ptrs] [{fv_ident=name, fv_info_ptr=info_ptr}:new_aci_params] exprs var_heap
								# (exprs,var_heap) = bind_variables arg_ptrs new_aci_params exprs var_heap
								# var_heap = writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap
								= (exprs,var_heap)
							bind_variables arg_ptrs=:[_:_] [] exprs var_heap
								= bind_variables_for_exprs arg_ptrs exprs var_heap
							bind_variables [] [] exprs var_heap
								= (exprs,var_heap)

							bind_variables_for_exprs :: [VarInfoPtr] [Expression] *VarHeap -> (![Expression],!*VarHeap)
							bind_variables_for_exprs [fv_info_ptr:arg_ptrs] [Var {var_ident=name, var_info_ptr=info_ptr}:exprs] var_heap
								# (exprs,var_heap) = bind_variables_for_exprs arg_ptrs exprs var_heap
								# var_heap = writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap
								= (exprs,var_heap)
							bind_variables_for_exprs [] exprs var_heap
								= (exprs,var_heap)
					_
						# (expr,cs) = copyVariable var ci cs
						-> (expr @ exprs, cs)
				# (expr,cs) = copyVariable var ci cs
				= (expr @ exprs, cs)
			where
				is_var_list [Var _:exprs] = is_var_list exprs
				is_var_list [_ : _] = False
				is_var_list [] = True
		update_active_case_info_and_copy case_expr _ cs
			= copy case_expr ci cs

		copyBoundVar {var_info_ptr} cs
			# (VI_Expression (Var act_var), cs_var_heap) = readPtr var_info_ptr cs.cs_var_heap
			= (act_var, { cs & cs_var_heap = cs_var_heap })

bind_vars dest_info_ptrs src_free_vars var_heap
	= fold2St bind dest_info_ptrs src_free_vars var_heap
where
	bind fv_info_ptr {fv_ident=name, fv_info_ptr=info_ptr} var_heap
		= writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap

instance copy Let
where
	copy lad=:{let_strict_binds, let_lazy_binds, let_expr, let_info_ptr} ci cs
		# (let_strict_binds, cs) = copy_bound_vars let_strict_binds cs
		# (let_lazy_binds, cs) = copy_bound_vars let_lazy_binds cs
		# (let_strict_binds, cs) = copy let_strict_binds ci cs
		# (let_lazy_binds, cs) = copy let_lazy_binds ci cs
		# (let_expr, cs) = copy let_expr ci cs
		  (old_let_info, cs_symbol_heap) = readPtr let_info_ptr cs.cs_symbol_heap
		  (new_let_info, cs_opt_type_heaps) = substitute_let_or_case_type old_let_info cs.cs_opt_type_heaps
		  (new_info_ptr, cs_symbol_heap) = newPtr new_let_info cs_symbol_heap
		= ({lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr, let_info_ptr = new_info_ptr},
			{ cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps })
		where
			copy_bound_vars [bind=:{lb_dst} : binds] cs
				# (lb_dst, cs) = copy lb_dst ci cs
				  (binds, cs) = copy_bound_vars binds cs
				= ([ {bind & lb_dst = lb_dst} : binds ], cs)
			copy_bound_vars [] cs
				= ([], cs)

substitute_let_or_case_type	expr_info No
	= (expr_info, No)
substitute_let_or_case_type	(EI_Extended extensions expr_info) yes_type_heaps
	# (new_expr_info, yes_type_heaps) = substitute_let_or_case_type expr_info yes_type_heaps
	= (EI_Extended extensions new_expr_info, yes_type_heaps)
substitute_let_or_case_type	(EI_CaseType case_type) (Yes type_heaps)
	# (new_case_type, type_heaps) = substitute case_type type_heaps
	= (EI_CaseType new_case_type, Yes type_heaps)
substitute_let_or_case_type	(EI_LetType let_type) (Yes type_heaps)
	# (new_let_type, type_heaps) = substitute let_type type_heaps
	= (EI_LetType new_let_type, Yes type_heaps)

instance copy CasePatterns
where
	copy (AlgebraicPatterns type patterns) ci cs
		# (patterns, cs) = copy patterns ci cs
		= (AlgebraicPatterns type patterns, cs)
	copy (BasicPatterns type patterns) ci cs
		# (patterns, cs) = copy patterns ci cs
		= (BasicPatterns type patterns, cs)
	copy (OverloadedListPatterns type decons_expr patterns) ci cs
		# (patterns, cs) = copy patterns ci cs
		# (decons_expr, cs) = copy decons_expr ci cs
		= (OverloadedListPatterns type decons_expr patterns, cs)
	copy (NewTypePatterns type patterns) ci cs
		# (patterns, cs) = copy patterns ci cs
		= (NewTypePatterns type patterns, cs)
	copy (DynamicPatterns patterns) ci cs
		# (patterns, cs) = copy patterns ci cs
		= (DynamicPatterns patterns, cs)

instance copy AlgebraicPattern
where
	copy guard=:{ap_vars,ap_expr} ci cs
		# (ap_vars, cs) = copy ap_vars ci cs
		  (ap_expr, cs) = copy ap_expr ci cs
		= ({ guard & ap_vars = ap_vars, ap_expr = ap_expr }, cs)

instance copy BasicPattern
where
	copy guard=:{bp_expr} ci cs
		# (bp_expr, cs) = copy bp_expr ci cs
		= ({ guard & bp_expr = bp_expr }, cs)

instance copy DynamicPattern
where
	copy guard=:{dp_var,dp_rhs} ci cs
		# (dp_var, cs) = copy dp_var ci cs
		  (dp_rhs, cs) = copy dp_rhs ci cs
		= ({ guard & dp_var = dp_var, dp_rhs = dp_rhs }, cs)

instance copy [a] | copy a
where
	copy l ci cs
		= map_st l cs
		where
			map_st [x : xs] s
			 	# (x, s) = copy x ci s
				  (xs, s) = map_st xs s
				#! s = s
				= ([x : xs], s)
			map_st [] s
			 	= ([], s)

instance copy (a,b) | copy a & copy b
where
	copy (a,b) ci cs
		# (a,cs) = copy a ci cs
		# (b,cs) = copy b ci cs
		= ((a,b),cs)

instance copy (Optional a) | copy a
where
	copy (Yes x) ci cs
		# (x, cs) = copy x ci cs
		= (Yes x, cs)
	copy no ci cs
		= (no, cs)
