/*
	module owner: Ronny Wichers Schreur
*/
implementation module typereify

import syntax
import typesupport

typeFunName :: Ident -> {#Char}
typeFunName {id_name}
	=	"TD;" +++ id_name

class makeTypeFun a :: Ident Position SymbolType *VarHeap *SymbolTable
	-> (a, *VarHeap, *SymbolTable)

instance makeTypeFun FunDef where
	makeTypeFun ident position symbol_type var_heap symbol_table	
		=	(function, var_heap, symbol_table)
		where
			function =
				{	fun_ident = ident
				,	fun_arity = 1
				,	fun_priority = NoPrio
				,	fun_body = GeneratedBody
				,	fun_type = Yes symbol_type
				,	fun_pos = position
				,	fun_kind = FK_Function False
				,	fun_lifted = 0
				,	fun_info = EmptyFunInfo
				}

instance makeTypeFun FunType where
	makeTypeFun ident position symbol_type var_heap symbol_table
		# (entry, symbol_table)
			=	readPtr ident.id_info symbol_table
		# entry
			=	{ entry & ste_kind = STE_DclFunction}
		# symbol_table
			=	writePtr ident.id_info entry symbol_table
		# (ft_type_ptr, var_heap)
			=	newPtr VI_Empty var_heap
		=	({	ft_ident = ident
			,	ft_arity = 1
			,	ft_priority = NoPrio
			,	ft_type = symbol_type
			,	ft_pos = position
			,	ft_specials = FSP_None
			,	ft_type_ptr	= ft_type_ptr
			}, var_heap, symbol_table)

add_dcl_type_fun_types :: TypeSymbIdent Int *{#DclModule} *VarHeap *SymbolTable
										-> (*{#DclModule},*VarHeap,*SymbolTable)
add_dcl_type_fun_types ctTypeDefSymb n_cached_dcls dcl_mods var_heap symbols
	# (n, dcl_mods) = usize dcl_mods
	= add_type_fun_types n_cached_dcls n ctTypeDefSymb dcl_mods var_heap symbols
	where
		add_type_fun_types :: Int Int TypeSymbIdent *{#DclModule} *VarHeap *SymbolTable
												-> (*{#DclModule},*VarHeap,*SymbolTable)
		add_type_fun_types module_n n ctTypeDefSymb dcl_mods var_heap symbols
			| module_n >= n
				=	(dcl_mods, var_heap, symbols)
			| module_n == cPredefinedModuleIndex
				=	add_type_fun_types (module_n+1) n ctTypeDefSymb dcl_mods var_heap symbols
				# (dcl_mod, dcl_mods) = dcl_mods![module_n]
				# (dcl_mod, var_heap, symbols)
					=	add_fun_types_of_dcl_module ctTypeDefSymb dcl_mod var_heap symbols
				# dcl_mods = {dcl_mods & [module_n] = dcl_mod}
				=	add_type_fun_types (module_n+1) n ctTypeDefSymb dcl_mods var_heap symbols

add_fun_types_of_dcl_module :: TypeSymbIdent DclModule *VarHeap *SymbolTable
										 -> (DclModule,*VarHeap,*SymbolTable)
add_fun_types_of_dcl_module ctTypeDefSymb dcl_mod=:{dcl_functions, dcl_common={com_type_defs}} var_heap symbols

//	| trace_tn ("add_fun_types_of_dcl_module "+++toString dcl_mod.dcl_name+++" "+++toString (size com_type_defs))

	# n_functions = size dcl_functions
	  (type_funs, com_type_defs, var_heap, symbols)
		=	addTypeFunctionsA n_functions ctTypeDefSymb {def \\ def <-: com_type_defs} var_heap symbols
	  dcl_functions = {function \\ function <- [e \\ e <-: dcl_functions] ++ type_funs}
	  dcl_type_funs = {ir_from = n_functions, ir_to = size dcl_functions}
	  dcl_mod = { dcl_mod	&	dcl_functions = dcl_functions
							,	dcl_common.com_type_defs = com_type_defs
							,	dcl_type_funs = dcl_type_funs
							}
	= (dcl_mod, var_heap, symbols)

getCTTypeDefSymb predefs
	# ({pds_module, pds_def}, predefs) = predefs![PD_CTTypeDef]
	# ident = predefined_idents.[PD_CTTypeDef]
	# type_symb = {MakeNewTypeSymbIdent ident 0 & type_index.glob_module = pds_module, type_index.glob_object = pds_def}
	= (type_symb, predefs)

addDclTypeFunctions :: !Int !*{#DclModule} !*PredefinedSymbols !*VarHeap !*SymbolTable
						-> (!*{#DclModule},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)
addDclTypeFunctions nr_cached_dcls dcl_modules predefs var_heap symbols
	# (ctTypeDefSymb, predefs) = getCTTypeDefSymb predefs
	# (dcl_modules, var_heap, symbols)
		=	add_dcl_type_fun_types ctTypeDefSymb nr_cached_dcls dcl_modules var_heap symbols
	= (dcl_modules, predefs, var_heap, symbols)

addIclTypeFunctions :: !Int !Int !*{#FunDef} !*{#CheckedTypeDef} !*{#ClassDef} !*PredefinedSymbols !*VarHeap !*SymbolTable
				 -> (!IndexRange,!*{#FunDef},!*{#CheckedTypeDef},!*{#ClassDef},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)
addIclTypeFunctions n_dcl_type_defs n_dcl_class_defs icl_functions icl_type_defs icl_class_defs predefs var_heap symbol_table
	# (ctTypeDefSymb, predefs) = getCTTypeDefSymb predefs
	  (n_functions_before, icl_functions) = usize icl_functions

	# (type_fun_index,rev_type_funs,icl_type_defs,var_heap,symbol_table)
		= add_td_funs_for_exported_types 0 n_functions_before ctTypeDefSymb n_dcl_type_defs [] icl_type_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_class_defs,var_heap,symbol_table)
		= add_td_funs_for_exported_classes 0 type_fun_index ctTypeDefSymb n_dcl_class_defs rev_type_funs icl_class_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_type_defs,var_heap,symbol_table)
		= add_td_funs_for_not_exported_types (n_dcl_type_defs+n_dcl_class_defs) type_fun_index ctTypeDefSymb rev_type_funs icl_type_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_class_defs,var_heap,symbol_table)
		= add_td_funs_for_not_exported_classes n_dcl_class_defs type_fun_index ctTypeDefSymb rev_type_funs icl_class_defs var_heap symbol_table				

	  icl_functions = {function \\ function <- [e \\ e <-: icl_functions] ++ reverse rev_type_funs}
	  (n_functions_after, icl_functions) = usize icl_functions
	  type_fun_range = {ir_from=n_functions_before,ir_to=n_functions_after}
	= (type_fun_range,icl_functions,icl_type_defs,icl_class_defs,predefs,var_heap,symbol_table)
where
	add_td_funs_for_exported_types :: Int Int TypeSymbIdent Int [FunDef]  *{#CheckedTypeDef}  *VarHeap  *SymbolTable
													  -> (!Int,![FunDef],!*{#CheckedTypeDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_exported_types dcl_type_index type_fun_index ct_type_def n_dcl_type_defs rev_type_fun_defs icl_type_defs var_heap symbol_table
		| dcl_type_index<n_dcl_type_defs
			# icl_type_index = dcl_type_index
			  (type_def,icl_type_defs) = icl_type_defs![icl_type_index]
			  (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
			  icl_type_defs = {icl_type_defs & [icl_type_index].td_fun_index = type_fun_index}
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_exported_types (dcl_type_index+1) (type_fun_index+1) ct_type_def n_dcl_type_defs rev_type_fun_defs icl_type_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_type_defs,var_heap,symbol_table)

	add_td_funs_for_exported_classes :: Int Int TypeSymbIdent Int [FunDef]  *{#ClassDef}  *VarHeap  *SymbolTable
														-> (!Int,![FunDef],!*{#ClassDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_exported_classes dcl_class_index type_fun_index ct_type_def n_dcl_class_defs rev_type_fun_defs icl_class_defs var_heap symbol_table
		| dcl_class_index<n_dcl_class_defs
			# icl_type_index = dcl_class_index
			  (class_def,icl_class_defs) = icl_class_defs![icl_type_index]
			  (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index (class_def.class_ident.id_name+++";") class_def.class_pos ct_type_def var_heap symbol_table
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_exported_classes (dcl_class_index+1) (type_fun_index+1) ct_type_def n_dcl_class_defs rev_type_fun_defs icl_class_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_class_defs,var_heap,symbol_table)

	add_td_funs_for_not_exported_types :: Int Int TypeSymbIdent [FunDef] *{#CheckedTypeDef}  *VarHeap  *SymbolTable
													 -> (!Int,![FunDef],!*{#CheckedTypeDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_not_exported_types icl_type_index type_fun_index ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
		| icl_type_index<size icl_type_defs
			# (type_def,icl_type_defs) = icl_type_defs![icl_type_index]
			| type_def.td_fun_index==NoIndex
				# (type_fun_def, var_heap, symbol_table)
					= add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
				  icl_type_defs = {icl_type_defs & [icl_type_index].td_fun_index = type_fun_index}
				  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
				= add_td_funs_for_not_exported_types (icl_type_index+1) (type_fun_index+1) ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
				= add_td_funs_for_not_exported_types (icl_type_index+1) type_fun_index ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_type_defs,var_heap,symbol_table)

	add_td_funs_for_not_exported_classes :: Int Int TypeSymbIdent [FunDef]  *{#ClassDef}  *VarHeap  *SymbolTable
														-> (!Int,![FunDef],!*{#ClassDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_not_exported_classes icl_class_index type_fun_index ct_type_def rev_type_fun_defs icl_class_defs var_heap symbol_table
		| icl_class_index<size icl_class_defs
			# (class_def,icl_class_defs) = icl_class_defs![icl_class_index]
			# (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index (class_def.class_ident.id_name+++";") class_def.class_pos ct_type_def var_heap symbol_table
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_not_exported_classes (icl_class_index+1) (type_fun_index+1) ct_type_def rev_type_fun_defs icl_class_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_class_defs,var_heap,symbol_table)

getSymbol :: Index ((Global Index) -> SymbKind) *PredefinedSymbols -> (SymbIdent, !*PredefinedSymbols)
getSymbol index symb_kind predef_symbols
	# ({pds_module, pds_def}, predef_symbols) = predef_symbols![index]
	# pds_ident = predefined_idents.[index]
	  symbol = { symb_ident = pds_ident, symb_kind = symb_kind { glob_module = pds_module, glob_object = pds_def} }
	= (symbol, predef_symbols)

predefFunction :: Index *PredefinedSymbols -> (SymbIdent, !*PredefinedSymbols)
predefFunction cons_index predefs
	=	getSymbol cons_index SK_Function predefs

predefConstructor :: Index *PredefinedSymbols -> (SymbIdent, !*PredefinedSymbols)
predefConstructor cons_index predefs
	=	getSymbol cons_index SK_Constructor predefs

predefRecordConstructor :: Index {#CommonDefs} *PredefinedSymbols -> (SymbIdent, !*PredefinedSymbols)
predefRecordConstructor record_type_index common_defs predefs
	# ({pds_module=pds_module1, pds_def=pds_def1}, predefs)
		=	predefs![record_type_index]
	# {TypeDef | td_rhs=RecordType {rt_constructor,rt_fields}} = common_defs.[pds_module1].com_type_defs.[pds_def1]
	# record_cons_symb_ident
		= { SymbIdent |
			symb_ident	= rt_constructor.ds_ident
		,	symb_kind 	= SK_Constructor {glob_module = pds_module1, glob_object = rt_constructor.ds_index} 
		}
	=	(record_cons_symb_ident, predefs)

:: BuildTypeFunState =
	!{	bs_predefs :: !.PredefinedSymbols
	,	bs_type_heaps :: .TypeHeaps
	,	bs_var_heap :: .VarHeap
	}

buildTypeFunctions :: !Int !*{#FunDef} !{#CommonDefs}
	*PredefinedSymbols *VarHeap *TypeHeaps
	-> (*{#FunDef}, *PredefinedSymbols, *VarHeap, *TypeHeaps)
buildTypeFunctions main icl_functions common_defs predefs var_heap type_heaps
	# bs_state =
		{	bs_predefs = predefs
		,	bs_var_heap = var_heap
		,	bs_type_heaps = type_heaps
		}
	# type_defs
		=	common_defs.[main].com_type_defs
	# (type_funs, bs_state)
		=	build 0 (size type_defs) type_defs icl_functions bs_state 
	=	(type_funs, bs_state.bs_predefs, bs_state.bs_var_heap, 
			bs_state.bs_type_heaps)
	where
		build i n type_defs functions bs_state
			| i < n
				# info =
					{	ri_main = main
					,	ri_common_defs = common_defs
					,	ri_type_var_num = 0
					}
				# (functions, bs_state)
					=	buildTypeFunction type_defs.[i] functions info bs_state
				=	build (i+1) n type_defs functions bs_state
			// otherwise
				=	(functions, bs_state)

buildTypeFunction :: CheckedTypeDef *{#FunDef} Info *BuildTypeFunState
	-> (*{#FunDef}, *BuildTypeFunState)
buildTypeFunction type_def=:{td_fun_index, td_args} functions info bs_state
	| td_fun_index == NoIndex
		=	(functions, bs_state)
	// otherwise
		# (rhs, bs_state)
			=	numberTypeVarsBeforeRiefy td_args (reify type_def) info bs_state
		# (new_info_ptr, bs_var_heap) = newPtr VI_Empty bs_state.bs_var_heap
		# bs_state
			=	{bs_state & bs_var_heap=bs_var_heap}
	  	# var_id
	  		=	{id_name = "_x", id_info = nilPtr}
	  	  lhs_free_var
	  	  	=	{fv_def_level = NotALevel, fv_ident = var_id,
	  	  			fv_info_ptr = new_info_ptr, fv_count = 0}
		# body
			=	{tb_args = [lhs_free_var], tb_rhs = rhs}
		# functions
			=	{functions & [td_fun_index].fun_body=TransformedBody body}
		=	(functions, bs_state)

numberTypeVarsBeforeRiefy :: a Riefier Info *BuildTypeFunState
	-> (Expression, *BuildTypeFunState) | numberTypeVars a
numberTypeVarsBeforeRiefy vars riefier info bs_state
	# bs_type_heaps
		=	bs_state.bs_type_heaps
	# (ri_type_var_num, th_vars)
		=	numberTypeVars vars (info.ri_type_var_num, bs_type_heaps.th_vars)
	# bs_type_heaps
		=	{bs_type_heaps & th_vars = th_vars}
	# bs_state
		=	{bs_state & bs_type_heaps = bs_type_heaps}
	# (expr, bs_state)
		=	riefier {info & ri_type_var_num=ri_type_var_num} bs_state
	=	(expr, bs_state)
	
class numberTypeVars a :: a (!Int, !*TypeVarHeap) -> (!Int, !*TypeVarHeap)

instance numberTypeVars [a] | numberTypeVars a where
	numberTypeVars l h
		=	foldSt numberTypeVars l h

instance numberTypeVars ATypeVar where
	numberTypeVars {atv_variable} h
		=	numberTypeVars atv_variable h

instance numberTypeVars TypeVar where
	numberTypeVars {tv_info_ptr} (n, h)
		=	(n+1, writePtr tv_info_ptr (TVI_Reify n) h)

addTypeFunctionsA :: Int TypeSymbIdent *{#CheckedTypeDef} *VarHeap *SymbolTable
							  -> ([a], *{#CheckedTypeDef},*VarHeap,*SymbolTable) | makeTypeFun a
addTypeFunctionsA type_fun_index ct_type_def type_defs var_heap symbol_table
	# (n, type_defs) = usize type_defs
	= add_td_funs_acc 0 n type_fun_index ct_type_def type_defs [] var_heap symbol_table
where
	add_td_funs_acc :: Int Int Int TypeSymbIdent *{#CheckedTypeDef} [a] *VarHeap *SymbolTable
										   -> ([a], *{#CheckedTypeDef}, *VarHeap,*SymbolTable) | makeTypeFun a
	add_td_funs_acc i n type_fun_index ct_type_def type_defs rev_type_fun_defs var_heap symbol_table
		| i >= n
			= (reverse rev_type_fun_defs, type_defs, var_heap, symbol_table)
			# (type_def, type_defs) = type_defs![i]
			  (type_fun_def, var_heap, symbol_table)
				=	add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
			  type_defs = {type_defs & [i].td_fun_index = type_fun_index}
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_acc (i+1) n (type_fun_index+1) ct_type_def type_defs rev_type_fun_defs var_heap symbol_table

add_td_fun_def :: Int {#Char} Position TypeSymbIdent *VarHeap  *SymbolTable
											 -> (!a,!*VarHeap,!*SymbolTable) | makeTypeFun a
add_td_fun_def type_fun_index type_name pos ct_type_def var_heap symbol_table
	#	entry =	{	ste_kind		= STE_Empty
				,	ste_index		= type_fun_index
				,	ste_def_level	= -1
				,	ste_previous	= EmptySymbolTableEntry
				}
	# (fun_ident, symbol_table)
		=	newPtr entry symbol_table
	# type_fun_ident = {id_name="TD;"+++type_name, id_info=fun_ident}

	# result_type = TA ct_type_def []
	# symbol_type =
			{	st_vars = []
			,	st_args = [{at_attribute= TA_None, at_type = TB BT_Bool}]
			,	st_args_strictness = NotStrict
			,	st_arity = 1
			,	st_result = {at_attribute = TA_None, at_type = result_type}
			,	st_context = []
			,	st_attr_vars = []
			,	st_attr_env = []
			}

	=	makeTypeFun type_fun_ident pos symbol_type var_heap symbol_table

:: BMonad a :== *BuildTypeFunState -> *(a, *BuildTypeFunState)

apply :: Expression Expression -> Expression
apply (App app=:{app_args}) a
	=	App {app & app_args = app_args ++ [a]}
apply f a
	=	f @ [a]

lift symb
	=	return (App {app_symb = symb, app_args = [], app_info_ptr = nilPtr})

cons :: Index Info *BuildTypeFunState
			-> *(Expression, *BuildTypeFunState)
cons cons_index info bs=:{bs_predefs}
	# (symbol, bs_predefs)
		=	getSymbol cons_index SK_Constructor bs_predefs
	=	lift symbol {bs & bs_predefs=bs_predefs}

record :: Index  Info *BuildTypeFunState
			-> *(Expression, *BuildTypeFunState)
record type_index info=:{ri_common_defs} bs=:{bs_predefs}
	# (symbol, bs_predefs)
		=	predefRecordConstructor type_index ri_common_defs bs_predefs
	=	lift symbol {bs & bs_predefs=bs_predefs}

quote :: {#Char} -> {#Char}
quote string
	=	"\"" +++ string +++ "\""

(o`) infixr  9
(o`) f g info x :== g info (f info x)

function :: Index Info  *BuildTypeFunState
			-> *(Expression, *BuildTypeFunState)
function fun_index info bs=:{bs_predefs}
	# (symbol, bs_predefs)
		=	getSymbol fun_index SK_Function bs_predefs
	=	lift symbol {bs & bs_predefs=bs_predefs}

(`) infixl 9
(`) f a info state
	# (rf, state)
		=	f info state
	# (ra, state)
		=	reify a info state
	=	(apply rf ra, state)

:: Info =
	{	ri_main :: !Int
	,	ri_common_defs :: !{#CommonDefs}
	,	ri_type_var_num :: !Int
	}

:: Riefier :== Info -> BMonad Expression
class reify a ::  a -> Riefier

instance reify [a] | reify a where
	reify []
		=	cons PD_NilSymbol
	reify [h:t]
		=	cons PD_ConsSymbol ` h ` t

instance reify Int where
	reify int
		=	basic (BVInt int)

instance reify Bool where
	reify bool
		=	basic (BVB bool)

instance reify {#Char} where
	reify string
		=	basic (BVS string)

instance reify CheckedTypeDef where
	reify {td_ident, td_arity, td_attribute, td_rhs}
		| is_dictionary td_ident.id_name
			// not implemented for dictionaries
			=	record PD_CTTypeDef ` quote td_ident.id_name ` td_arity ` False ` (SynType 	{at_attribute=TA_None,at_type=TE})
			=	record PD_CTTypeDef ` quote td_ident.id_name ` td_arity ` is_unq_attribute td_attribute ` td_rhs
	where
		is_dictionary s
			= size s>0 && s.[size s-1]==';';
	
		is_unq_attribute (TA_Var _)
			=	False
		is_unq_attribute TA_Unique
			=	True

instance reify TypeRhs where
	reify (AlgType constructors)
		=	cons PD_CTAlgType ` get constructors
		where
			get constructors info=:{ri_main, ri_common_defs} state
				=	reify [(ds_index,common_defs.[ds_index]) \\ {ds_index} <- constructors] info state
				where
					common_defs
						=	ri_common_defs.[ri_main].com_cons_defs
	reify (RecordType record_type)
		=	reify record_type
	reify (SynType _)
		=	cons PD_CTSynType

instance reify (Int, ConsDef) where
	reify (cons_index, {cons_ident, cons_type, cons_exi_vars})
		=	numberTypeVarsBeforeRiefy cons_exi_vars
			(record PD_CTConsDef
				` (function PD__CTToCons ` consSymbol cons_ident cons_index)
				` cons_type.st_args ` length cons_exi_vars)
		where
			consSymbol cons_ident cons_index info=:{ri_main} state
				# cons_symb =
					{	symb_ident = cons_ident
					,	symb_kind = SK_Constructor { glob_module = ri_main, glob_object = cons_index}
					}
				=	reify cons_symb info state

instance reify RecordType where
	reify {rt_fields} // +++ constructor ??? +++ is_boxed
		=	cons PD_CTRecordType ` [field \\ field <-: rt_fields]

instance reify FieldSymbol where
	reify {fs_index}
		=	selector fs_index
		where
			selector fs_index info=:{ri_main,ri_common_defs} st
				=	(numberTypeVarsBeforeRiefy vars
					(record PD_CTFieldDef
						` quote def.sd_ident.id_name
						` length (def.sd_exi_vars)
						` def.sd_type.st_result))
					info st
			where
				def
					=	ri_common_defs.[ri_main]
									.com_selector_defs.[fs_index]
				vars
					=	[atv_variable \\ {atv_variable} <- def.sd_exi_vars]
					++	def.sd_type.st_vars

instance reify AType where
	reify {at_type}
		=	reify at_type

instance reify Riefier where
	reify x
		=	x

instance reify Type where
	reify type=:(TA symb args)
		=	reifyApp symb args
	reify type=:(TAS symb args _)
		=	reifyApp symb args
	reify (TV var)
		=	reify var
	reify (TQV var)
		=	reify var
	reify (a :@: args)
		=	foldl` reifyApply (reify a) args
	reify TArrow
		=	cons PD_Dyn_TypeCons ` function PD_Dyn_TypeCodeConstructor_Arrow
	reify (TArrow1 a)
		=	cons PD_Dyn_TypeApp `
				(cons PD_Dyn_TypeCons ` function PD_Dyn_TypeCodeConstructor_Arrow) ` a
	reify (a --> b)
		=	cons PD_Dyn_TypeApp ` (cons PD_Dyn_TypeApp `
				(cons PD_Dyn_TypeCons ` function PD_Dyn_TypeCodeConstructor_Arrow) ` a) ` b
	reify (TB basic_type)
		=	reify basic_type
	reify (TFA vars type)
		=	numberTypeVarsBeforeRiefy vars (reify type)
	reify t
		=	undef //  <<- ("reify", t)

reifyApp :: TypeSymbIdent [AType] Info *BuildTypeFunState
	-> (Expression, *BuildTypeFunState)
reifyApp symb args info=:{ri_common_defs} bs_state=:{bs_type_heaps}
	# (expanded, expanded_type, bs_type_heaps)
		=	expandTypeSynonym ri_common_defs symb args bs_type_heaps
	# bs_state
		=	{bs_state & bs_type_heaps=bs_type_heaps}
	| expanded
		=	reify expanded_type info bs_state
	// otherwise
		=	foldl` reifyApply (reify symb) args info bs_state

foldl` op r l = foldl r l // crashes if it's a macro
	where
		foldl r []		= r
		foldl r [a:x]	= foldl (op r a) x

reifyApply a h
	=	cons PD_Dyn_TypeApp ` a ` h

instance reify ConsVariable where
	reify (CV var)
		=	reify var

instance reify TypeVar where
	reify {tv_info_ptr, tv_ident}
		=	cons PD_Dyn_TypeVar ` typeVarNum tv_info_ptr
		where
			typeVarNum tv_info_ptr info bs=:{bs_type_heaps}
				# (tv_info, th_vars)
					=	readPtr tv_info_ptr bs_type_heaps.th_vars
				# tv_num
					=	case tv_info of
							TVI_Reify tv_num
								->	tv_num
							_
								->	abort "typeVar" // <<- (tv_ident.id_name, tv_info)
				# bs_type_heaps
					=	{bs_type_heaps & th_vars = th_vars}
				=	reify tv_num info {bs & bs_type_heaps = bs_type_heaps}

instance reify BasicType where
	reify (BT_String string_type)
		=	reify string_type
	reify basic_type
		=	cons PD_Dyn_TypeCons ` function (predef basic_type)
		where
			predef BT_Int
				=	PD_Dyn_TypeCodeConstructorInt
			predef BT_Char
				=	PD_Dyn_TypeCodeConstructorChar
			predef BT_Real
				=	PD_Dyn_TypeCodeConstructorReal
			predef BT_Bool
				=	PD_Dyn_TypeCodeConstructorBool
			predef BT_Dynamic
				=	PD_Dyn_TypeCodeConstructorDynamic
			predef BT_File
				=	PD_Dyn_TypeCodeConstructorFile
			predef BT_World
				=	PD_Dyn_TypeCodeConstructorWorld

instance reify SymbIdent where
	reify symb
		=	reify {app_symb = symb, app_args = [], app_info_ptr = nilPtr}

instance reify TypeSymbIdent where
	reify symb
		=	cons PD_Dyn_TypeCons ` reifyTypeIdent symb
		where
			reifyTypeIdent {type_index} info=:{ri_common_defs} st
				=	reify (toTypeCodeConstructor type_index ri_common_defs) info st

instance reify GlobalTCType where
	reify (GTT_PredefTypeConstructor {glob_object=type_index})
		| PD_Arity2TupleTypeIndex <= type_index && type_index <= PD_Arity32TupleTypeIndex
			# arity
				=	type_index - PD_Arity2TupleTypeIndex + 2
			=	function PD_Dyn_TypeCodeConstructor_Tuple ` arity
		// otherwise
			# predef_type_index
				=	type_index + FirstTypePredefinedSymbolIndex
			=	function (predefinedTypeConstructor predef_type_index)
	reify (GTT_Constructor type_fun)
		=	function PD_Dyn__to_TypeCodeConstructor ` type_fun

predefinedTypeConstructor predef_type_index
	| predef_type_index == PD_ListType
		=	PD_Dyn_TypeCodeConstructor_List
	| predef_type_index == PD_StrictListType
		=	PD_Dyn_TypeCodeConstructor_StrictList
	| predef_type_index == PD_UnboxedListType
		=	PD_Dyn_TypeCodeConstructor_UnboxedList
	| predef_type_index == PD_TailStrictListType
		=	PD_Dyn_TypeCodeConstructor_TailStrictList
	| predef_type_index == PD_StrictTailStrictListType
		=	PD_Dyn_TypeCodeConstructor_StrictTailStrictList
	| predef_type_index == PD_UnboxedTailStrictListType
		=	PD_Dyn_TypeCodeConstructor_UnboxedTailStrictList
	| predef_type_index == PD_LazyArrayType
		=	PD_Dyn_TypeCodeConstructor_LazyArray
	| predef_type_index == PD_StrictArrayType
		=	PD_Dyn_TypeCodeConstructor_StrictArray
	| predef_type_index == PD_UnboxedArrayType
		=	PD_Dyn_TypeCodeConstructor_UnboxedArray
	// otherwise
		=	fatal "predefinedType" "TC code from predef"

instance reify App where
	reify app
		=	reify (App app)

instance reify Expression where
	reify expr
		=	\x -> return expr

basic :: BasicValue -> Riefier
basic value
	=	\x -> return (BasicExpr value)

// copied and adopted from overloading
toTypeCodeConstructor type=:{glob_object=type_index, glob_module=module_index} common_defs
	| module_index == cPredefinedModuleIndex
		= GTT_PredefTypeConstructor type
	// otherwise
		# type
			=	common_defs.[module_index].com_type_defs.[type_index]
		# td_fun_index
			=	type.td_fun_index
		// sanity check ...
		| td_fun_index == NoIndex
			=	fatal "toTypeCodeConstructor" ("no function (" +++ type.td_ident.id_name +++ ")")
		// ... sanity check
		# type_fun
			=	{	symb_ident = {type.td_ident & id_info = nilPtr} // this is wrong but let's give it a try
				,	symb_kind = SK_Function {glob_module = module_index, glob_object = td_fun_index}
				}
		= GTT_Constructor type_fun

fatal :: {#Char} {#Char} -> .a
fatal function_name message
	=	abort ("typereflection, " +++ function_name +++ ": " +++ message)

expandTypeSynonym :: {#CommonDefs} TypeSymbIdent [AType] *TypeHeaps
	-> (Bool, Type, *TypeHeaps)
expandTypeSynonym defs cons_id type_args type_heaps
	# {type_ident,type_index={glob_object,glob_module}}
		=	cons_id
	# {td_ident,td_rhs,td_args,td_attribute}
		=	defs.[glob_module].com_type_defs.[glob_object]
	= case td_rhs of
		SynType {at_type}
			# (expanded_type, type_heaps)
				=	substituteType td_attribute TA_Multi td_args type_args
															at_type type_heaps
			-> (True, expanded_type, type_heaps)
		_
			-> (False, undef, type_heaps)

sanityCheckTypeFunctions :: !Int !CommonDefs !{#DclModule} !{#FunDef}
	->	Bool
sanityCheckTypeFunctions main_dcl icl_common dcl_mods fun_defs
	=	checkType {def.fun_ident.id_name \\ def <-: fun_defs} icl_common
	&&	all checkDcl [dcl \\ dcl <-: dcl_mods]
	&&	compareTypes icl_common dcl_mods.[main_dcl].dcl_common
	where
		checkDcl :: DclModule -> Bool
		checkDcl {dcl_functions, dcl_common}
			=	checkType {f.ft_ident.id_name \\ f <-: dcl_functions} dcl_common

class checkType a :: {{#Char}} a -> Bool

instance checkType CommonDefs where
	checkType names {com_type_defs}
		=	checkType names com_type_defs

instance checkType (a e) | Array a e & checkType e where
	checkType names a
		=	all (checkType names) [e \\ e <-: a]

instance checkType (TypeDef a) where
	checkType names {td_ident, td_fun_index}
		| td_fun_index == NoIndex
			=	True
		| names.[td_fun_index] == typeFunName td_ident
			=	True
		// otherwise
			=	False // ->> (names.[td_fun_index], "<>", typeFunName td_ident)

class compareTypes a :: a a -> Bool

instance compareTypes CommonDefs where
	compareTypes a b
		=	compareTypes a.com_type_defs b.com_type_defs

instance compareTypes (a e) | Array a e & compareTypes e where
	compareTypes a b
		=	and [compareTypes ea eb \\ ea <-: a & eb <-: b]

instance compareTypes (TypeDef a) where
	compareTypes a b
		| a.td_fun_index == b.td_fun_index
			=	True
		// otherwise
			=	False // ->> (a.td_ident.id_name, a.td_fun_index, "<>",
						// 		b.td_ident.id_name, b.td_fun_index)
