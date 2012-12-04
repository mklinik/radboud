definition module predef

import syntax, hashtable

::	PredefinedSymbols	:== {# PredefinedSymbol}

::	PredefinedSymbol = {
		pds_module	:: !Index,
		pds_def		:: !Index
	}

init_identifiers :: !*SymbolTable !*World -> (!*SymbolTable,!*World)

predefined_idents :: {!Ident}

buildPredefinedSymbols :: !*HashTable -> (!.PredefinedSymbols,!*HashTable)

buildPredefinedModule :: !Bool !*PredefinedSymbols -> (!ScannedModule, !.PredefinedSymbols)

cPredefinedModuleIndex :== 1

PD_StringTypeIndex :== 0
PD_Arity2TupleTypeIndex :== 8
PD_Arity32TupleTypeIndex :== 38

/* identifiers not present the hastable */

PD_PredefinedModule			:== 0

FirstTypePredefinedSymbolIndex:==PD_StringType; // to compute index in com_type_defs

PD_StringType				:== 1

PD_ListType :== 2
PD_StrictListType :== 3
PD_UnboxedListType :== 4
PD_TailStrictListType :== 5
PD_StrictTailStrictListType :== 6
PD_UnboxedTailStrictListType :== 7
PD_OverloadedListType :== 8

PD_Arity2TupleType			:== 9
PD_Arity32TupleType			:== 39

PD_LazyArrayType			:== 40
PD_StrictArrayType			:== 41
PD_UnboxedArrayType			:== 42

// constructors:

FirstConstructorPredefinedSymbolIndex :== PD_ConsSymbol; // to compute index in com_cons_defs

PD_ConsSymbol :== 43
PD_StrictConsSymbol :== 44
PD_UnboxedConsSymbol :== 45
PD_TailStrictConsSymbol :== 46
PD_StrictTailStrictConsSymbol :== 47
PD_UnboxedTailStrictConsSymbol :== 48
PD_OverloadedConsSymbol :== 49

PD_NilSymbol :== 50
PD_StrictNilSymbol :== 51
PD_UnboxedNilSymbol :== 52
PD_TailStrictNilSymbol :== 53
PD_StrictTailStrictNilSymbol :== 54
PD_UnboxedTailStrictNilSymbol :== 55
PD_OverloadedNilSymbol :== 56

PD_Arity2TupleSymbol		:== 57
PD_Arity32TupleSymbol		:== 87

// end constructors

PD_TypeVar_a0				:== 88
PD_TypeVar_a31				:== 119

/* identifiers present in the hashtable */

PD_StdArray					:== 120
PD_StdEnum					:== 121
PD_StdBool					:== 122

PD_AndOp					:== 123
PD_OrOp						:== 124

/* Array functions */

PD_ArrayClass				:== 125

PD_CreateArrayFun			:== 126
PD__CreateArrayFun			:== 127
PD_ArraySelectFun			:== 128
PD_UnqArraySelectFun		:== 129
PD_ArrayUpdateFun			:== 130
PD_ArrayReplaceFun			:== 131
PD_ArraySizeFun				:== 132
PD_UnqArraySizeFun			:== 133

/* Enum/Comprehension functions */

PD_SmallerFun				:== 134
PD_LessOrEqualFun			:== 135
PD_IncFun					:== 136
PD_SubFun					:== 137
PD_From						:== 138
PD_FromThen					:== 139
PD_FromTo					:== 140
PD_FromThenTo				:== 141

/* StdMisc */
PD_StdMisc					:== 142
PD_abort					:== 143
PD_undef					:== 144

PD_Start					:== 145

PD_DummyForStrictAliasFun	:== 146

PD_StdStrictLists:==147

PD_cons:==148
PD_decons:==149

PD_cons_u:==150
PD_decons_u:==151

PD_cons_uts:==152
PD_decons_uts:==153

PD_nil:==154
PD_nil_u:==155
PD_nil_uts:==156

PD_ListClass :== 157
PD_UListClass :== 158
PD_UTSListClass :== 159

/* Dynamics */

// TC class
PD_TypeCodeMember			:== 160
PD_TypeCodeClass			:== 161
// dynamic module
PD_StdDynamic				:== 162
// dynamic type
PD_Dyn_DynamicTemp				:== 163
// type code (type)
PD_Dyn_TypeCode					:== 164
// unification (type)
PD_Dyn_UnificationEnvironment	:== 165
// type code (expressions)
PD_Dyn_TypeScheme			:== 166
PD_Dyn_TypeApp				:== 167
PD_Dyn_TypeVar				:== 168
PD_Dyn_TypeCons				:== 169
PD_Dyn_TypeUnique			:== 170
PD_Dyn__TypeFixedVar		:== 171
// unification (expressions)
PD_Dyn_initial_unification_environment	:== 172
PD_Dyn_bind_global_type_pattern_var		:== 173
PD_Dyn_unify							:== 174
PD_Dyn_normalise						:== 175
// predefined type code constructor (expressions)
PD_Dyn_TypeCodeConstructorInt						:==	176
PD_Dyn_TypeCodeConstructorChar						:== 177
PD_Dyn_TypeCodeConstructorReal						:== 178
PD_Dyn_TypeCodeConstructorBool						:== 179
PD_Dyn_TypeCodeConstructorDynamic					:== 180
PD_Dyn_TypeCodeConstructorFile						:== 181
PD_Dyn_TypeCodeConstructorWorld						:== 182
PD_Dyn_TypeCodeConstructor_Arrow					:== 183
PD_Dyn_TypeCodeConstructor_List						:== 184
PD_Dyn_TypeCodeConstructor_StrictList				:== 185
PD_Dyn_TypeCodeConstructor_UnboxedList				:== 186
PD_Dyn_TypeCodeConstructor_TailStrictList			:== 187
PD_Dyn_TypeCodeConstructor_StrictTailStrictList		:== 188		
PD_Dyn_TypeCodeConstructor_UnboxedTailStrictList	:== 189
PD_Dyn_TypeCodeConstructor_Tuple					:== 190
PD_Dyn_TypeCodeConstructor_LazyArray				:== 191
PD_Dyn_TypeCodeConstructor_StrictArray				:== 192
PD_Dyn_TypeCodeConstructor_UnboxedArray				:== 193

/* Generics */
PD_StdGeneric				:== 194
// Generics types
PD_TypeBimap				:== 195
PD_TypeUNIT					:== 196
PD_TypeEITHER				:== 197
PD_TypePAIR					:== 198
// for constructor info
PD_TypeCONS					:== 199
PD_TypeFIELD				:== 200
PD_TypeOBJECT				:== 201
PD_GenericInfo				:== 202
PD_TGenericConsDescriptor 	:== 203
PD_TGenericFieldDescriptor 	:== 204
PD_TGenericTypeDefDescriptor :== 205
PD_TGenConsPrio				:== 206
PD_TGenConsAssoc			:== 207
PD_TGenType					:== 208

PD_TypeGenericDict 			:== 209
// Generics fields
PD_map_to					:== 210
PD_map_from					:== 211
// Generics expressions
PD_ConsBimap				:== 212
PD_ConsUNIT					:== 213
PD_ConsLEFT					:== 214
PD_ConsRIGHT				:== 215
PD_ConsPAIR					:== 216
// for constructor info
PD_ConsCONS					:== 217
PD_ConsFIELD				:== 218
PD_ConsOBJECT				:== 219
PD_NoGenericInfo			:== 220
PD_GenericConsInfo			:== 221
PD_GenericFieldInfo			:== 222
PD_GenericTypeInfo			:== 223
PD_CGenericConsDescriptor 	:== 224
PD_CGenericFieldDescriptor 	:== 225
PD_CGenericTypeDefDescriptor :== 226
PD_CGenConsNoPrio			:== 227
PD_CGenConsPrio				:== 228
PD_CGenConsAssocNone		:== 229
PD_CGenConsAssocLeft		:== 230
PD_CGenConsAssocRight		:== 231
PD_CGenTypeCons				:== 232
PD_CGenTypeVar				:== 233
PD_CGenTypeArrow			:== 234
PD_CGenTypeApp				:== 235

PD_bimapId					:== 236
PD_GenericBimap				:== 237

PD_FromS					:== 238
PD_FromTS					:== 239
PD_FromSTS					:== 240
PD_FromU					:== 241
PD_FromUTS					:== 242
PD_FromO					:== 243

PD_FromThenS				:== 244
PD_FromThenTS				:== 245
PD_FromThenSTS				:== 246
PD_FromThenU				:== 247
PD_FromThenUTS				:== 248
PD_FromThenO				:== 249

PD_FromToS					:== 250
PD_FromToTS					:== 251
PD_FromToSTS				:== 252
PD_FromToU					:== 253
PD_FromToUTS				:== 254
PD_FromToO					:== 255

PD_FromThenToS				:== 256
PD_FromThenToTS				:== 257
PD_FromThenToSTS			:== 258
PD_FromThenToU				:== 259
PD_FromThenToUTS			:== 260
PD_FromThenToO				:== 261

/* Clean Type introspection */
PD_CleanTypes				:== 262
PD_CTTypeDef				:== 263
PD_CTAlgType				:== 264
PD_CTRecordType				:== 265
PD_CTSynType				:== 266
PD_CTPredefined				:== 267
PD_CTConsDef				:== 268
PD__CTToCons				:== 269
PD_CTFieldDef				:== 270

PD_Dyn__to_TypeCodeConstructor	:== 271

PD_NrOfPredefSymbols		:== 272

GetTupleConsIndex tup_arity :== PD_Arity2TupleSymbol + tup_arity - 2
GetTupleTypeIndex tup_arity :== PD_Arity2TupleType + tup_arity - 2

// changes requires recompile of {static,dynamic}-linker plus all dynamics ever made
UnderscoreSystemDynamicModule_String	:== "_SystemDynamic"	

// List-type
PD_ListType_String				:== "_List"
PD_ConsSymbol_String			:== "_Cons"
PD_NilSymbol_String				:== "_Nil"

// Array-type
PD_UnboxedArray_String			:== "_#Array"

DynamicRepresentation_String			:== "DynamicTemp" // "_DynamicTemp"		
