definition module StdDynamicTypes

from StdMaybe import :: Maybe
from DefaultElem import class DefaultElem
from EnDecode import class EnDecode

:: DummyModuleID	= DummyModuleID

// Type 
:: LibraryInstanceTypeReference 
	= LIT_TypeReference !LibRef !TIO_TypeReference		// fst Int is index in cs_library_instances
	
:: LibRef
	= LibRef !Int										// library instance in main dynamic

instance EnDecode (Maybe m) | EnDecode m
	
::  TIO_TypeReference
	= {
		tio_type_without_definition  :: !Maybe String
	,   tio_tr_module_n    			 :: !Int
	,   tio_tr_type_def_n  			 :: !Int
	}

instance DefaultElem TIO_TypeReference

instance DefaultElem LibRef

instance DefaultElem LibraryInstanceTypeReference

//COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES yes no :== yes; // also change graph_to_string.c; recompile DynamicLinker and dumpDynamic

//IS_COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES 	 :== COLLECT_AND_RENUMBER_EXTERNAL_TYPE_REFERENCES True False;

FILE_IDENTIFICATION md5 normal :== md5;

IS_NORMAL_FILE_IDENTIFICATION :== FILE_IDENTIFICATION False True;

NAME_PREFIXES yes no :== no;

SHARING_ACROSS_CONVERSIONS yes no :== no;

IS_SHARING_ACROSS_CONVERSIONS :== SHARING_ACROSS_CONVERSIONS True False;

// Clean Object Type file (COT)
COT_SUPPORT yes no :== no;

COMPILE_FOR_COT_SUPPORT :== COT_SUPPORT True False;

:: TypeDef`							// TIO_TypeDef
	= {
		td_id	:: !Int //TypeConstructorID
	,	td_uid	:: !UniversalTypeID
	,	name	:: !String
	,	arity	:: !Int
	,	args	:: ![Int]
	, 	rhs		:: TypeRhs`
	};
	
instance DefaultElem TypeDef`
instance EnDecode TypeDef`
	
:: UniversalTypeID
	= {
		uti_id			:: !String				// library.{typ}; most significant
	,	uti_type_ref	:: !TIO_TypeReference
	};
		
instance DefaultElem UniversalTypeID
instance EnDecode UniversalTypeID
	
:: TypeConstructorID				// TIO_TypeSymbIdent
	:== Int
	
:: TypeRhs` 
	= AlgType` [Constructors`]
	| RecordType` [Field`] StrictnessList` [Int]
	| Empty`
		
:: Constructors`
	= Constructor` !String [Type`] StrictnessList` [Int]
		
:: Type`
	= TypeApp` TypeConstructorID [Type`] StrictnessList`
	| FuncApp` Type` Type`
	| TypeVar` !Int
	| EmptyType`

::	StrictnessList` 
	= NotStrict` 
	| Strict` !Int 
	| StrictList` !Int StrictnessList`

:: Field`
	= Field` !String !Type`
	| EmptyField`	

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
