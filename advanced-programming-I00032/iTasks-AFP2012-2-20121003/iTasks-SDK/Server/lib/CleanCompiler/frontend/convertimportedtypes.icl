implementation module convertimportedtypes

import syntax, trans

cDontRemoveAnnotations :== False

convertDclModule :: !Int !{# DclModule} !{# CommonDefs} !*{#{# CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps
	-> (!*{#{# CheckedTypeDef}}, !ImportedConstructors, !*VarHeap, !*TypeHeaps)
convertDclModule main_dcl_module_n dcl_mods common_defs imported_types imported_conses var_heap type_heaps
	# {dcl_functions,dcl_common=dcl_common=:{com_type_defs,com_cons_defs,com_selector_defs},dcl_has_macro_conversions} = dcl_mods.[main_dcl_module_n]
	| dcl_has_macro_conversions
		#!(icl_type_defs, imported_types) = imported_types![main_dcl_module_n]
		  common_defs = { common \\ common <-: common_defs }
		  common_defs = { common_defs & [main_dcl_module_n] = dcl_common }
		  types_and_heaps = convert_dcl_functions dcl_functions common_defs ( { imported_types & [main_dcl_module_n] = com_type_defs }, imported_conses, var_heap, type_heaps)
		  types_and_heaps = convertConstructorTypes com_cons_defs main_dcl_module_n common_defs types_and_heaps
		  (imported_types, imported_conses, var_heap, type_heaps) = convertSelectorTypes com_selector_defs main_dcl_module_n common_defs types_and_heaps
		= ({ imported_types & [main_dcl_module_n] = icl_type_defs}, imported_conses, var_heap, type_heaps)
		= (imported_types, imported_conses, var_heap, type_heaps)
where
	convert_dcl_functions dcl_functions common_defs types_and_heaps
		= iFoldSt (convert_dcl_function dcl_functions common_defs) 0 (size dcl_functions) types_and_heaps

	convert_dcl_function dcl_functions common_defs dcl_index (imported_types, imported_conses, var_heap, type_heaps)
		#!{ft_type, ft_type_ptr, ft_ident} = dcl_functions.[dcl_index]
		  (ft_type, imported_types, imported_conses, type_heaps, var_heap)
		  	= convertSymbolType cDontRemoveAnnotations common_defs ft_type main_dcl_module_n imported_types imported_conses type_heaps var_heap
		= (imported_types, imported_conses, var_heap <:= (ft_type_ptr, VI_ExpandedType ft_type), type_heaps)

convertConstructorTypes cons_defs main_dcl_module_n common_defs types_and_heaps
	= iFoldSt (convert_constructor_type common_defs cons_defs) 0 (size cons_defs) types_and_heaps
where
	convert_constructor_type common_defs cons_defs cons_index (imported_types, imported_conses, var_heap, type_heaps)  
		#!{cons_type_ptr, cons_type, cons_ident} = cons_defs.[cons_index]
		  (cons_type, imported_types, imported_conses, type_heaps, var_heap)
				= convertSymbolType cDontRemoveAnnotations common_defs cons_type main_dcl_module_n imported_types imported_conses type_heaps var_heap
		= (imported_types, imported_conses, var_heap <:= (cons_type_ptr, VI_ExpandedType cons_type), type_heaps)

convertSelectorTypes selector_defs main_dcl_module_n common_defs types_and_heaps
	= iFoldSt (convert_selector_type common_defs selector_defs) 0 (size selector_defs) types_and_heaps
where
	convert_selector_type common_defs selector_defs sel_index (imported_types, imported_conses, var_heap, type_heaps)  
		#!{sd_type_ptr, sd_type, sd_ident} = selector_defs.[sel_index]
		  (sd_type, imported_types, imported_conses, type_heaps, var_heap)
				= convertSymbolType cDontRemoveAnnotations common_defs sd_type main_dcl_module_n imported_types imported_conses type_heaps var_heap
		= (imported_types, imported_conses, var_heap <:= (sd_type_ptr, VI_ExpandedType sd_type), type_heaps)

convertIclModule :: !Int !{# CommonDefs} !*{#{# CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps
	-> (!*{#{# CheckedTypeDef}}, !ImportedConstructors, !*VarHeap, !*TypeHeaps)
convertIclModule main_dcl_module_n common_defs imported_types imported_conses var_heap type_heaps
	#! types_and_heaps = convertConstructorTypes common_defs.[main_dcl_module_n].com_cons_defs main_dcl_module_n common_defs (imported_types, imported_conses, var_heap, type_heaps)
	= convertSelectorTypes common_defs.[main_dcl_module_n].com_selector_defs main_dcl_module_n common_defs types_and_heaps

convertImportedTypeSpecifications :: !Int !{# DclModule}  !{# {# FunType} } !{# CommonDefs} !ImportedConstructors !ImportedFunctions
	!*{# {#CheckedTypeDef}} !*TypeHeaps !*VarHeap -> (!*{#{#CheckedTypeDef}}, !*TypeHeaps, !*VarHeap)
convertImportedTypeSpecifications main_dcl_module_n dcl_mods dcl_functions common_defs imported_conses imported_functions imported_types type_heaps var_heap
	# {dcl_common={com_type_defs},dcl_has_macro_conversions} = dcl_mods.[main_dcl_module_n]
	| dcl_has_macro_conversions
		# abstract_type_indexes = iFoldSt (determine_abstract_type com_type_defs) 0 (size com_type_defs) []
		| isEmpty abstract_type_indexes
			= convert_imported_type_specs dcl_functions common_defs imported_conses imported_functions imported_types type_heaps var_heap
			#!(icl_type_defs, imported_types) = imported_types![main_dcl_module_n]
			  type_defs = foldSt insert_abstract_type abstract_type_indexes { icl_type_def \\ icl_type_def <-: icl_type_defs }
			  (imported_types, type_heaps, var_heap)
			  		= convert_imported_type_specs dcl_functions common_defs imported_conses imported_functions
						{ imported_types & [main_dcl_module_n] = type_defs } type_heaps var_heap
			= ({ imported_types & [main_dcl_module_n] = icl_type_defs }, type_heaps, var_heap)
		= convert_imported_type_specs dcl_functions common_defs imported_conses imported_functions imported_types type_heaps var_heap
where
	determine_abstract_type dcl_type_defs type_index abstract_type_indexes
		# {td_rhs} = dcl_type_defs.[type_index]
		= case td_rhs of
			AbstractType _
				-> [type_index : abstract_type_indexes]
			_
				-> abstract_type_indexes
					
	insert_abstract_type type_index type_defs
		# icl_index=type_index
		# (type_def, type_defs) = type_defs![icl_index]
		= { type_defs & [icl_index] = { type_def & td_rhs = AbstractType cAllBitsClear }}

	convert_imported_type_specs dcl_functions common_defs imported_conses imported_functions imported_types type_heaps var_heap
		# (imported_types, imported_conses, type_heaps, var_heap)
				= foldSt (convert_imported_function dcl_functions common_defs) imported_functions (imported_types, imported_conses, type_heaps, var_heap)
		= convert_imported_constructors common_defs imported_conses imported_types type_heaps var_heap

	convert_imported_function dcl_functions common_defs {glob_object,glob_module} (imported_types, imported_conses, type_heaps, var_heap)
		#!{ft_type_ptr,ft_type,ft_ident} = dcl_functions.[glob_module].[glob_object]
		  (ft_type, imported_types, imported_conses, type_heaps, var_heap)
				= convertSymbolType cDontRemoveAnnotations common_defs ft_type main_dcl_module_n imported_types imported_conses type_heaps var_heap
		= (imported_types, imported_conses, type_heaps, var_heap <:= (ft_type_ptr, VI_ExpandedType ft_type))
			
	convert_imported_constructors common_defs [] imported_types type_heaps var_heap
		= (imported_types, type_heaps, var_heap)
	convert_imported_constructors common_defs [ {glob_module, glob_object} : conses ] imported_types type_heaps var_heap 
		#!{com_cons_defs,com_selector_defs} = common_defs.[glob_module]
		  {cons_type_ptr,cons_type,cons_type_index,cons_ident} = common_defs.[glob_module].com_cons_defs.[glob_object]
		  (cons_type, imported_types, conses, type_heaps, var_heap)
		  		= convertSymbolType cDontRemoveAnnotations common_defs cons_type main_dcl_module_n imported_types conses type_heaps var_heap
		  var_heap = var_heap <:= (cons_type_ptr, VI_ExpandedType cons_type)
		  ({td_rhs}, imported_types) = imported_types![glob_module].[cons_type_index]
				//---> ("convert_imported_constructors", cons_ident, cons_type)
		= case td_rhs of
			RecordType {rt_fields}
				# (imported_types, conses, type_heaps, var_heap)
						= iFoldSt (convert_type_of_imported_field glob_module com_selector_defs rt_fields) 0 (size rt_fields)
							(imported_types, conses, type_heaps, var_heap)
				-> convert_imported_constructors common_defs conses imported_types type_heaps var_heap
			_
				-> convert_imported_constructors common_defs conses imported_types type_heaps var_heap
		where
			convert_type_of_imported_field module_index selector_defs fields field_index (imported_types, conses, type_heaps, var_heap)
				#!field_index = fields.[field_index].fs_index
				  {sd_type_ptr,sd_type,sd_ident} = selector_defs.[field_index]
				  (sd_type, imported_types, conses, type_heaps, var_heap)
				  		= convertSymbolType cDontRemoveAnnotations common_defs sd_type main_dcl_module_n imported_types conses type_heaps var_heap
				= (imported_types, conses, type_heaps, var_heap <:= (sd_type_ptr, VI_ExpandedType sd_type))
