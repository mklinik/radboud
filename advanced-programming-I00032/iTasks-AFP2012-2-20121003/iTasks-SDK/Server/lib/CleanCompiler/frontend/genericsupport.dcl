definition module genericsupport

import syntax, checksupport

lookupGenericClassInfo :: 
		!TypeKind 
		!GenericClassInfos	
	-> 	(Optional GenericClassInfo)

addGenericClassInfo :: 
		!GenericClassInfo 
		!GenericClassInfos 
	->	GenericClassInfos

getGenericClassInfo ::
	!(Global Index)
	!TypeKind
	!{#CommonDefs}		
	!*GenericHeap
	-> 		
	( Optional GenericClassInfo
	, !*GenericHeap					
	)
getGenericMember :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)

getGenericClass :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)


//****************************************************************************************
//	Ident Helpers
//****************************************************************************************
makeIdent 					:: !String -> Ident
postfixIdent 				:: !String !String -> Ident
genericIdentToClassIdent 	:: !String !TypeKind -> Ident
genericIdentToMemberIdent 	:: !String !TypeKind -> Ident
genericIdentToFunIdent 		:: !String !TypeCons -> Ident
kind_to_short_string :: !TypeKind -> {#Char}
