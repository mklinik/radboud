implementation module genericsupport

import syntax, checksupport

getGenericClassInfo ::
	!(Global Index)
	!TypeKind
	!{#CommonDefs}		
	!*GenericHeap
	-> 		
	( Optional GenericClassInfo
	, !*GenericHeap					
	)
getGenericClassInfo {glob_module, glob_object} kind modules generic_heap
	#! (gen_def=:{gen_info_ptr}) = modules.[glob_module].com_generic_defs.[glob_object]
	#! ({gen_classes}, generic_heap) = readPtr gen_info_ptr generic_heap
	#! opt_class_info = lookupGenericClassInfo kind gen_classes
	= (opt_class_info, generic_heap)

getGenericMember :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)
getGenericMember gen kind modules generic_heap
	# (opt_class_info, generic_heap) = getGenericClassInfo  gen kind modules generic_heap
	= case opt_class_info of
		No -> (No, generic_heap) 
		Yes {gci_module, gci_member}
			#! member_glob = {glob_module = gci_module, glob_object = gci_member}
			-> (Yes member_glob, generic_heap)

getGenericClass :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)
getGenericClass gen kind modules generic_heap
	# (opt_class_info, generic_heap) = getGenericClassInfo  gen kind modules generic_heap
	= case opt_class_info of
		No -> (No, generic_heap) 
		Yes {gci_module, gci_class}
			#! class_glob = {glob_module = gci_module, glob_object = gci_class}
			-> (Yes class_glob, generic_heap)

lookupGenericClassInfo :: !TypeKind !GenericClassInfos	-> (Optional GenericClassInfo)
lookupGenericClassInfo kind class_infos
	#! hash_index = case kind of
		KindConst -> 0
		KindArrow kinds -> length kinds
	= lookup kind class_infos.[hash_index] 		
where
	lookup kind [] = No
	lookup kind [gci:gcis]
		| gci.gci_kind == kind 	= Yes gci
								= lookup kind gcis

addGenericClassInfo :: !GenericClassInfo !GenericClassInfos -> GenericClassInfos
addGenericClassInfo class_info=:{gci_kind} class_infos
	#! hash_index = case gci_kind of
		KindConst -> 0
		KindArrow kinds -> length kinds
	#! (class_infos1, class_infos) = class_infos ! [hash_index]
	#! class_infos1 = [class_info:class_infos1]	
	= {{x\\x<-:class_infos} & [hash_index] = class_infos1 }

//****************************************************************************************
//	Ident Helpers
//****************************************************************************************
makeIdent :: !String -> Ident
makeIdent str = {id_name = str, id_info = nilPtr} 

postfixIdent :: !String !String -> Ident
postfixIdent id_name postfix = makeIdent (id_name +++ postfix)

genericIdentToClassIdent :: !String !TypeKind -> Ident
genericIdentToClassIdent id_name kind
	= postfixIdent id_name ("_" +++ kind_to_short_string kind) 

kind_to_short_string :: !TypeKind -> {#Char}
kind_to_short_string KindConst = "s"
kind_to_short_string (KindArrow kinds) = kinds_to_str  kinds +++ "s"
where
	kinds_to_str [] = ""
	kinds_to_str [KindConst:ks] = "s" +++ kinds_to_str ks
	kinds_to_str [k:ks] = "o" +++ (kind_to_short_string k) +++ "c" +++ kinds_to_str ks	

genericIdentToMemberIdent :: !String !TypeKind -> Ident
genericIdentToMemberIdent id_name kind
	= genericIdentToClassIdent id_name kind

genericIdentToFunIdent :: !String !TypeCons -> Ident
genericIdentToFunIdent id_name type_cons
	= postfixIdent id_name ("_" +++ type_cons_to_str type_cons)
where
	type_cons_to_str (TypeConsSymb {type_ident}) = toString type_ident
	type_cons_to_str (TypeConsBasic bt) = toString bt
	type_cons_to_str TypeConsArrow = "ARROW"
	type_cons_to_str (TypeConsVar tv) = tv.tv_ident.id_name
		 	