implementation module StdDynamicTypes

import StdEnv
import StdMaybe
import DefaultElem
import EnDecode

:: DummyModuleID	= DummyModuleID
	
// Type 
:: LibraryInstanceTypeReference 
	= LIT_TypeReference !LibRef !TIO_TypeReference		// fst Int is index in cs_library_instances
	
:: LibRef
	= LibRef !Int										// library instance in main dynamic
	
ENDECODE_MAYBE_SIZE	:== 1;
ENDECODE_JUST	 	:== 0;
ENDECODE_NOTHING	:== 1;

instance EnDecode (Maybe m) | EnDecode m
where
	to_size (Just i)
		= ENDECODE_MAYBE_SIZE + to_size i
	to_size Nothing
		= ENDECODE_MAYBE_SIZE
	
	to_string x=:(Just i) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_JUST }
		# (offset,buffer)
			= to_string i (offset + ENDECODE_MAYBE_SIZE) buffer
		= (offset,buffer)

	to_string Nothing offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_NOTHING }
		# offset
			= offset + ENDECODE_MAYBE_SIZE
		= (offset,buffer)
	
	from_string offset buffer	
		# disk_type_reference_id
			= toInt (buffer.[offset])
		| disk_type_reference_id == ENDECODE_JUST
			#! (x,offset)
				= from_string (offset + ENDECODE_MAYBE_SIZE) buffer
			#! x = Just x
			= (x, offset)
		| disk_type_reference_id == ENDECODE_NOTHING
			#! x = Nothing
			# offset
				= offset + ENDECODE_MAYBE_SIZE;
			= (x, offset)

::  TIO_TypeReference
	= {
		tio_type_without_definition  :: !Maybe String
	,   tio_tr_module_n    			 :: !Int
	,   tio_tr_type_def_n  			 :: !Int
	}

instance EnDecode TIO_TypeReference
where
	to_size {tio_type_without_definition,tio_tr_module_n,tio_tr_type_def_n}
		= to_size tio_type_without_definition + to_size tio_tr_module_n + to_size tio_tr_type_def_n

	to_string {tio_type_without_definition,tio_tr_module_n,tio_tr_type_def_n} offset buffer
		# (offset,buffer)
			= to_string tio_type_without_definition offset buffer
		# (offset,buffer)
			= to_string tio_tr_module_n offset buffer
		# (offset,buffer)
			= to_string tio_tr_type_def_n offset buffer
		= (offset,buffer)

	from_string offset buffer
		#! (tio_type_without_definition,offset)
			= from_string offset buffer
		#! (tio_tr_module_n,offset)
			= from_string offset buffer
		#! (tio_tr_type_def_n,offset)
			= from_string offset buffer
			
		#! di
			= { default_elem &
				tio_type_without_definition		= tio_type_without_definition
			,	tio_tr_module_n					= tio_tr_module_n
			,	tio_tr_type_def_n				= tio_tr_type_def_n
			}
		= (di,offset)
			
instance DefaultElem TIO_TypeReference
where
	default_elem 
		= {	
			tio_type_without_definition  = Nothing
		,   tio_tr_module_n    			 = 0
		,   tio_tr_type_def_n  			 = 0
		}
	
instance DefaultElem LibRef
where
	default_elem
		= LibRef (-1)
		
instance DefaultElem LibraryInstanceTypeReference
where
	default_elem
		= LIT_TypeReference default_elem default_elem
		
//COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES yes no :== yes // also change graph_to_string.c recompile DynamicLinker and dumpDynamic

//IS_COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES 	 :== COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES True False

FILE_IDENTIFICATION md5 normal :== md5

IS_NORMAL_FILE_IDENTIFICATION :== FILE_IDENTIFICATION False True

NAME_PREFIXES yes no :== no

SHARING_ACROSS_CONVERSIONS yes no :== no

IS_SHARING_ACROSS_CONVERSIONS :== SHARING_ACROSS_CONVERSIONS True False

:: TypeDef`
	= {
		td_id	:: !Int // !TypeConstructorID
	,	td_uid	:: !UniversalTypeID
	,	name	:: !String
	,	arity	:: !Int
	,	args	:: ![Int]
	, 	rhs		:: TypeRhs`
	}

instance DefaultElem TypeDef`
where 
	default_elem
		= {
			td_id	= default_elem
		,	td_uid	= default_elem
		,	name	= default_elem
		,	arity	= default_elem
		,	args	= default_elem
		, 	rhs		= default_elem
		}
		
instance EnDecode TypeDef`
where
	to_size {td_id,td_uid,name,arity,args,rhs}
		= to_size td_id + to_size td_uid + to_size name + to_size arity + to_size args + to_size rhs

	to_string {td_id,td_uid,name,arity,args,rhs} offset buffer
		# (offset,buffer)
			= to_string td_id offset buffer
		# (offset,buffer)
			= to_string td_uid offset buffer
		# (offset,buffer)
			= to_string name offset buffer
		# (offset,buffer)
			= to_string arity offset buffer
		# (offset,buffer)
			= to_string args offset buffer
		# (offset,buffer)
			= to_string rhs offset buffer
		= (offset,buffer)

	from_string offset buffer
		#! (td_id,offset)
			= from_string offset buffer
		#! (td_uid,offset)
			= from_string offset buffer
		#! (name,offset)
			= from_string offset buffer
		#! (arity,offset)
			= from_string offset buffer
		#! (args,offset)
			= from_string offset buffer
		#! (rhs,offset)
			= from_string offset buffer
			
		#! di
			= { default_elem &
				td_id	= td_id
			,	td_uid	= td_uid
			,	name	= name
			,	arity	= arity
			,	args	= args
			,	rhs		= rhs
			}
		= (di,offset)

:: UniversalTypeID
	= {
		uti_id			:: !String				// library.{typ}; most significant
	,	uti_type_ref	:: !TIO_TypeReference
	};

instance DefaultElem UniversalTypeID
where
	default_elem
		= {
			uti_id			= default_elem
		,	uti_type_ref	= default_elem
		};

instance EnDecode UniversalTypeID
where
	to_size {uti_id,uti_type_ref}
		= to_size uti_id + to_size uti_type_ref

	to_string {uti_id,uti_type_ref} offset buffer
		# (offset,buffer)
			= to_string uti_id offset buffer
		# (offset,buffer)
			= to_string uti_type_ref offset buffer
		= (offset,buffer)

	from_string offset buffer
		#! (uti_id,offset)
			= from_string offset buffer
		#! (uti_type_ref,offset)
			= from_string offset buffer
			
		#! di
			= { default_elem &
				uti_id			= uti_id
			,	uti_type_ref	= uti_type_ref
			}
		= (di,offset)
	
:: TypeRhs` 
	= AlgType` [Constructors`]
	| RecordType` [Field`] StrictnessList` [Int]
	| Empty`
		
instance DefaultElem TypeRhs`
where 
	default_elem
		= Empty`
	
ENDECODE_TYPERHS`_SIZE		:== 1

ENDECODE_ALGTYPE`		 	:== 0
ENDECODE_RECORD`			:== 1
ENDECODE_EMPTY`				:== 2
	
instance EnDecode TypeRhs`
where
	to_size (AlgType` constructors)
		= ENDECODE_TYPERHS`_SIZE + to_size constructors
	to_size (RecordType` fields strictness_list addresses)
		= ENDECODE_TYPERHS`_SIZE + to_size fields + to_size strictness_list + to_size addresses
	to_size Empty`
		= ENDECODE_TYPERHS`_SIZE
		
	to_string x=:(AlgType` constructors) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_ALGTYPE` }
		# (offset,buffer)
			= to_string constructors (offset + ENDECODE_TYPERHS`_SIZE) buffer
		= (offset,buffer)	
	to_string x=:(RecordType` fields strictness_list addresses) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_RECORD` }
		# (offset,buffer)
			= to_string fields (offset + ENDECODE_TYPERHS`_SIZE) buffer
		# (offset,buffer)
			= to_string strictness_list offset buffer
		# (offset,buffer)
			= to_string addresses offset buffer
		= (offset,buffer)	
	to_string Empty` offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_EMPTY` }
		# offset
			= offset + ENDECODE_TYPERHS`_SIZE
		= (offset,buffer)
		
	from_string offset buffer
		# type_rhs_kind
			= toInt (buffer.[offset])
		| type_rhs_kind == ENDECODE_ALGTYPE`
			#! (x,offset)
				= from_string (offset + ENDECODE_TYPERHS`_SIZE) buffer
			#! x = AlgType` x
			= (x, offset)
		| type_rhs_kind == ENDECODE_RECORD`
			#! (fields,offset)
				= from_string (offset + ENDECODE_TYPERHS`_SIZE) buffer
			#! (strictness_list,offset)
				= from_string offset buffer
			#! (addresses,offset)
				= from_string offset buffer
			#! x = RecordType` fields strictness_list addresses
			= (x, offset)

		| type_rhs_kind == ENDECODE_EMPTY`
			#! x = Empty`
			# offset = offset + ENDECODE_TYPERHS`_SIZE
			= (x, offset)
			
:: Constructors`
	= Constructor` !String [Type`] StrictnessList` [Int]
	
instance DefaultElem Constructors`
where
	default_elem
		= Constructor` default_elem default_elem default_elem default_elem
		
ENDECODE_CONSTRUCTORS`_SIZE	:== 1

ENDECODE_CONSTRUCTOR`		:== 0

instance EnDecode Constructors`
where
	to_size (Constructor` name types strictness_list addresses)
		= ENDECODE_CONSTRUCTORS`_SIZE + to_size name + to_size types + to_size strictness_list + to_size addresses
		
	to_string x=:(Constructor` name types strictness_list addresses) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_CONSTRUCTOR` }
		# (offset,buffer)
			= to_string name (offset + ENDECODE_CONSTRUCTORS`_SIZE) buffer
		# (offset,buffer)
			= to_string types offset buffer
		# (offset,buffer)
			= to_string strictness_list offset buffer
		# (offset,buffer)
			= to_string addresses offset buffer
		= (offset,buffer)
		
	from_string offset buffer
		#! constructor_kind
			= toInt (buffer.[offset])
		| constructor_kind == ENDECODE_CONSTRUCTOR`
			#! (name,offset)
				= from_string (offset + ENDECODE_CONSTRUCTORS`_SIZE) buffer
			#! (types,offset)
				= from_string offset buffer
			#! (strictness_list,offset)
				= from_string offset buffer
			#! (addresses,offset)
				= from_string offset buffer
			#! x = Constructor` name types strictness_list addresses
			= (x, offset)
	
:: Type`
	= TypeApp` TypeConstructorID [Type`] StrictnessList`
	| FuncApp` Type` Type`
	| TypeVar` !Int
	| EmptyType`
	
::	StrictnessList` 
	= NotStrict` 
	| Strict` !Int 
	| StrictList` !Int StrictnessList`
	
	
instance DefaultElem StrictnessList`
where
	default_elem
		= NotStrict`

ENDECODE_STRICTNESSLIST`_SIZE	:== 1
ENDECODE_NOTSTRICT`				:== 0
ENDECODE_STRICT`				:== 1
ENDECODE_STRICTLIST`			:== 2

instance EnDecode StrictnessList`
where
	to_size NotStrict`
		= ENDECODE_STRICTNESSLIST`_SIZE
	to_size (Strict` strictness)
		= ENDECODE_STRICTNESSLIST`_SIZE + to_size strictness
	to_size (StrictList` strictness next)
		= ENDECODE_STRICTNESSLIST`_SIZE + to_size strictness + to_size next
	
	to_string NotStrict` offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_NOTSTRICT` }
		# offset 
			= offset + ENDECODE_STRICTNESSLIST`_SIZE
		= (offset,buffer)
	to_string (Strict` strictness) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_STRICT` }
		# (offset,buffer)
			= to_string strictness (offset + ENDECODE_STRICTNESSLIST`_SIZE) buffer
		= (offset,buffer)
	
	to_string (StrictList` strictness next) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_STRICTLIST` }
		# (offset,buffer)
			= to_string strictness (offset + ENDECODE_STRICTNESSLIST`_SIZE) buffer
		# (offset,buffer)
			= to_string next offset buffer
		= (offset,buffer)
		
	from_string offset buffer
		# type_kind
			= toInt (buffer.[offset])
		| type_kind == ENDECODE_NOTSTRICT`
			#! x = NotStrict`
			#! offset = offset + ENDECODE_STRICTNESSLIST`_SIZE
		 	= (x,offset)
		| type_kind == ENDECODE_STRICT`
			#! (strictness,offset)
				= from_string (offset + ENDECODE_STRICTNESSLIST`_SIZE) buffer
			#! x = Strict` strictness
			= (x, offset)
		| type_kind == ENDECODE_STRICTLIST`
			#! (strictness,offset)
				= from_string (offset + ENDECODE_STRICTNESSLIST`_SIZE) buffer
			#! (next,offset)
				= from_string offset buffer
			#! x = StrictList` strictness next
			= (x, offset)

instance DefaultElem Type`
where
	default_elem
		= EmptyType`

ENDECODE_TYPE`_SIZE		:== 1

ENDECODE_TYPEAPP`		:== 0
ENDECODE_TYPEVAR`	 	:== 1
ENDECODE_FUNCAPP`		:== 2
ENDECODE_EMPTYTYPE`		:== 3

instance EnDecode Type`
where 
	to_size (TypeApp` type_id types strictness)
		= ENDECODE_TYPE`_SIZE + to_size type_id + to_size types + to_size strictness
	to_size (TypeVar` type_var)
		= ENDECODE_TYPE`_SIZE + to_size type_var
	to_size (FuncApp` type1 type2)
		= ENDECODE_TYPE`_SIZE + to_size type1 + to_size type2
	to_size EmptyType` 
		= ENDECODE_TYPE`_SIZE

	to_string x=:(FuncApp` type1 type2) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_FUNCAPP` }
		# (offset,buffer)
			= to_string type1 (offset + ENDECODE_TYPE`_SIZE) buffer
		# (offset,buffer)
			= to_string type2 offset buffer
		= (offset,buffer)				
	to_string x=:(TypeApp` type_id types strictness) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_TYPEAPP` }
		# (offset,buffer)
			= to_string type_id (offset + ENDECODE_TYPE`_SIZE) buffer
		# (offset,buffer)
			= to_string types offset buffer
		# (offset,buffer)
			= to_string strictness offset buffer
		= (offset,buffer)		
	to_string x=:(TypeVar` type_var) offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_TYPEVAR` }
		# (offset,buffer)
			= to_string type_var (offset + ENDECODE_TYPE`_SIZE) buffer
		= (offset,buffer)		
	to_string x=:EmptyType` offset buffer 	
		# buffer
			= { buffer & [offset] = toChar ENDECODE_EMPTYTYPE` }
		# offset
			= offset + ENDECODE_TYPE`_SIZE
		= (offset,buffer)		
		
	from_string offset buffer	
		# type_kind
			= toInt (buffer.[offset])
		| type_kind == ENDECODE_FUNCAPP`
			#! (type1,offset)
				= from_string (offset + ENDECODE_TYPE`_SIZE) buffer
			#! (type2,offset)
				= from_string offset buffer
			#! x = FuncApp` type1 type2
			= (x, offset)
		| type_kind == ENDECODE_TYPEAPP`
			#! (type_id,offset)
				= from_string (offset + ENDECODE_TYPE`_SIZE) buffer
			#! (types,offset)
				= from_string offset buffer
			#! (strictness,offset)
				= from_string offset buffer
			#! x = TypeApp` type_id types strictness
			= (x, offset)
		| type_kind == ENDECODE_TYPEVAR`
			#! (type_var,offset)
				= from_string (offset + ENDECODE_TYPE`_SIZE) buffer
			#! x = TypeVar` type_var
			= (x, offset)
		| type_kind == ENDECODE_EMPTYTYPE`
			#! x = EmptyType`
			#! offset = offset + ENDECODE_TYPE`_SIZE
		 	= (x,offset)
		 	
:: Field`
	= Field` !String !Type`
	| EmptyField`
	
instance DefaultElem Field`
where
	default_elem
		= EmptyField`
		
ENDECODE_FIELD`_SIZE		:== 1
ENDECODE_FIELD`				:== 0
ENDECODE_EMPTYFIELD`		:== 2

instance EnDecode Field`
where
	to_size (Field` field_name type)
		= ENDECODE_FIELD`_SIZE + to_size field_name + to_size type
	to_size EmptyField`
		= ENDECODE_FIELD`_SIZE
		
	to_string x=:(Field` field_name type) offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_FIELD` }
		# (offset,buffer)
			= to_string field_name (offset + ENDECODE_FIELD`_SIZE) buffer
		# (offset,buffer)
			= to_string type offset buffer
		= (offset,buffer)	
	to_string EmptyField` offset buffer
		# buffer
			= { buffer & [offset] = toChar ENDECODE_EMPTYFIELD` }
		# offset
			= offset + ENDECODE_FIELD`_SIZE
		= (offset,buffer)

	from_string offset buffer
		# type_rhs_kind
			= toInt (buffer.[offset])
		| type_rhs_kind == ENDECODE_FIELD`
			#! (field_name,offset)
				= from_string (offset + ENDECODE_FIELD`_SIZE) buffer
			#! (type,offset)
				= from_string offset buffer
			#! x = Field` field_name type
			= (x, offset)
		| type_rhs_kind == ENDECODE_EMPTYFIELD`
			#! x = EmptyField`
			# offset = offset + ENDECODE_FIELD`_SIZE
			= (x, offset)
	

				
instance DefaultElem [e]
where
	default_elem
		= []
		
:: PredefType
	= PT_Empty
	| PT_Int
	| PT_Char
	| PT_Real
	| PT_Bool
	| PT_Dynamic
	| PT_File
	| PT_World
	| PT__Arrow
	| PT__List
	| PT__StrictList
	| PT__UnboxedList
	| PT__TailStrictList
	| PT__StrictTailStrictList
	| PT__UnboxedTailStrictList
	| PT__Tuple !Int
	| PT__LazyArray
	| PT__StrictArray
	| PT__UnboxedArray

	
INT_INDEX						:== 0
CHAR_INDEX						:== 1
REAL_INDEX						:== 2
BOOL_INDEX						:== 3
DYNAMIC_INDEX					:== 4
FILE_INDEX						:== 5
WORLD_INDEX						:== 6
ARROW_INDEX						:== 7
LIST_INDEX						:== 8
STRICTLIST_INDEX				:== 9
UNBOXEDLIST_INDEX				:== 10
TAILSTRICTLIST_INDEX			:== 11
STRICTTAILSTRICTLIST_INDEX		:== 12
UNBOXEDTAILSTRICTLIST_INDEX		:== 13
TUPLE2_INDEX					:== 14
TUPLE3_INDEX					:== 15
TUPLE4_INDEX					:== 16
TUPLE5_INDEX					:== 17
TUPLE6_INDEX					:== 18
TUPLE7_INDEX					:== 19
TUPLE8_INDEX					:== 20
TUPLE9_INDEX					:== 21
TUPLE10_INDEX					:== 22
TUPLE11_INDEX					:== 23
TUPLE12_INDEX					:== 24
TUPLE13_INDEX					:== 25
TUPLE14_INDEX					:== 26
TUPLE15_INDEX					:== 27
TUPLE16_INDEX					:== 28
TUPLE17_INDEX					:== 29
TUPLE18_INDEX					:== 30
TUPLE19_INDEX					:== 31
TUPLE20_INDEX					:== 32
TUPLE21_INDEX					:== 33
TUPLE22_INDEX					:== 34
TUPLE23_INDEX					:== 35
TUPLE24_INDEX					:== 36
TUPLE25_INDEX					:== 37
TUPLE26_INDEX					:== 38
TUPLE27_INDEX					:== 39
TUPLE28_INDEX					:== 40
TUPLE29_INDEX					:== 41
TUPLE30_INDEX					:== 42
TUPLE31_INDEX					:== 43
TUPLE32_INDEX					:== 44
LAZYARRAY_INDEX					:== 45
STRICTARRAY_INDEX				:== 46
UNBOXED_ARRAY_INDEX				:== 47

N_PREDEFINED_INDICES			:== 48

INDEX_TO_PREDEFINED_TYPE_STRING :: {(String,PredefType)}
INDEX_TO_PREDEFINED_TYPE_STRING
	=: create_string_indices
where 
	create_string_indices :: {(String,PredefType)}
	create_string_indices 
		# string = createArray N_PREDEFINED_INDICES ("",PT_Empty)
		# string
			= { string &
				[INT_INDEX]						= ("Int",PT_Int)
			,	[CHAR_INDEX]					= ("Char",PT_Char)
			,	[REAL_INDEX]					= ("Real",PT_Real)
			,	[BOOL_INDEX]					= ("Bool",PT_Bool)
			,	[DYNAMIC_INDEX]					= ("Dynamic",PT_Dynamic)
			,	[FILE_INDEX]					= ("File",PT_File)
			,	[WORLD_INDEX]					= ("World",PT_World)
			,	[ARROW_INDEX]					= ("->",PT__Arrow)
			,	[LIST_INDEX]					= ("_List",PT__List)
			,	[STRICTLIST_INDEX]				= ("_!List",PT__StrictList)
			,	[UNBOXEDLIST_INDEX]				= ("_#List",PT__UnboxedList)
			,	[TAILSTRICTLIST_INDEX]			= ("_List!",PT__TailStrictList)
			,	[STRICTTAILSTRICTLIST_INDEX]	= ("_!List!",PT__StrictTailStrictList)
			,	[UNBOXEDTAILSTRICTLIST_INDEX]	= ("_#List!",PT__UnboxedTailStrictList)
			,	[LAZYARRAY_INDEX]				= ("_Array",PT__LazyArray)
			,	[STRICTARRAY_INDEX]				= ("_!Array",PT__StrictArray)
			,	[UNBOXED_ARRAY_INDEX]			= ("_#Array",PT__UnboxedArray)
			}
		# string		
			= {string &
				[tuple_index] = ("_Tuple" +++ toString (tuple_index - TUPLE2_INDEX + 2),PT__Tuple (tuple_index - TUPLE2_INDEX + 2)) \\ tuple_index <- [TUPLE2_INDEX..TUPLE32_INDEX]
			}
		= string		