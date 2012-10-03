/*
	Wrap Clean nodes (for debugging purposes).

	Version 1.0.5
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module Wrap

import	StdOverloaded, StdMisc, StdArray, StdClass, StdInt

::	WrappedDescriptorId = {descriptorId :: !Int}

instance toString WrappedDescriptorId where
	toString :: !WrappedDescriptorId -> {#Char}
	toString {descriptorId}
		=	descriptorIDtoString descriptorId
	where
		descriptorIDtoString :: !Int -> {#Char}
		descriptorIDtoString id
			=	code
			{
			.d 0 1 i
				jsr DtoAC
			.o 1 0
			}

instance toString WrappedDescriptor where
	toString WrappedDescriptorCons
		=	"[:]"
	toString WrappedDescriptorNil
		=	"[]"
	toString WrappedDescriptorTuple
		=	"(,..,)"
	toString (WrappedDescriptorOther descriptorId)
		=	toString descriptorId
	toString WrappedDescriptorUnknown
		=	"<?>"

instance wrap WrappedArg where
	wrap x
		=	{arg = wrap x}

instance wrap (WrappedNode a) | wrap a where
	wrap x
		=	mapWrapNode wrapArg (shallowWrap x)
		where
			wrapArg :: !UnwrappedArg -> a | wrap a
			wrapArg x
				=	wrap x.node

mapWrapNode :: (a -> b) (WrappedNode a) -> WrappedNode b
mapWrapNode f (WrappedArray a)
	=	WrappedArray (mapArgs f a)
mapWrapNode f (WrappedRecord desc fields)
	=	WrappedRecord desc (mapArgs f fields)
mapWrapNode f (WrappedUnboxedList desc fields)
	=	WrappedUnboxedList desc (mapArgs f fields)
mapWrapNode f (WrappedUnboxedRecordList desc fields)
	=	WrappedUnboxedRecordList desc (mapArgs f fields)
mapWrapNode f (WrappedOther desc args)
	=	WrappedOther desc (mapArgs f args)
mapWrapNode _ (WrappedInt i)
	=	WrappedInt i
mapWrapNode _ (WrappedChar c)
	=	WrappedChar c
mapWrapNode _ (WrappedBool b)
	=	WrappedBool b
mapWrapNode _ (WrappedReal r)
	=	WrappedReal r
mapWrapNode _ (WrappedFile f)
	=	WrappedFile f
mapWrapNode _ (WrappedString s)
	=	WrappedString s
mapWrapNode _ (WrappedIntArray ia)
	=	WrappedIntArray ia
mapWrapNode _ (WrappedBoolArray ba)
	=	WrappedBoolArray ba
mapWrapNode _ (WrappedRealArray ra)
	=	WrappedRealArray ra
mapWrapNode _ (WrappedFileArray fa)
	=	WrappedFileArray fa

mapArgs :: (a -> b) {!a} -> {!b}
mapArgs f a
	=	{f e \\ e <-: a}

deepWrap :: !.a -> DeeplyWrappedNode
deepWrap node
	=	wrap node

shallowWrap :: !.a -> ShallowlyWrappedNode
shallowWrap node
	=	code
	{
					| A: <node> <result>
					| B:
		eq_desc	BOOL 0 0
		jmp_false	not_a_bool
		pushB_a	0
		pop_a	1
		fill_r	e_Wrap_kWrappedBool 0 1 0 0 0
		pop_b	1
	.d 1 0
		rtn
	:not_a_bool

		pushD_a 0
		eq_desc_b INT 0
		jmp_false	not_an_int

		pushI_a	0
		pop_a	1
		fill_r	e_Wrap_kWrappedInt 0 1 0 0 0
		pop_b	1
	.d 1 0
		rtn
	:not_an_int

		eq_desc	CHAR 0 0
		jmp_false	not_a_char

		pushC_a	0
		pop_a	1
		fill_r	e_Wrap_kWrappedChar 0 1 0 0 0
		pop_b	1
	.d 1 0
		rtn
	:not_a_char

		eq_desc	REAL 0 0
		jmp_false	not_a_real

		pushR_a	0
		pop_a	1
		fill_r	e_Wrap_kWrappedReal 0 2 0 0 0
		pop_b	2
	.d 1 0
		rtn
	:not_a_real

		eq_desc	FILE 0 0
		jmp_false	not_a_file

		pushF_a	0
		pop_a	1
		fill_r	e_Wrap_kWrappedFile 0 2 0 0 0
		pop_b	2
	.d 1 0
		rtn
	:not_a_file

		eq_desc	ARRAY 1 0
		jmp_true	wrap_array

		eq_desc	_ARRAY_ 0 0
		jmp_true	wrap__array

		eq_desc	_STRING_ 0 0
		jmp_true	wrap__string

		is_record	0
|	.d 2 0
		jmp_true	wrap_record

		get_node_arity	0
					| B: <n>
		eqI_b	0 0
		jmp_true	wrap_no_args

	:wrap_args
		push_a	0
		push_b	0
		push_b	0
		repl_args_b
					| A: <arg_1 .. arg_n> <node> <result>
		push_b	0
		create_array_	_ 1 0
					| A: <_{args}> <arg_1 .. arg_n> <node> <result>
		pushI	0

	:wrap_args_loop
					| A: <_{args}> <arg_(i+1) .. arg_n> <node> <result>
					| B: <i> <n>
		| wrap arg
		push_a	1
		build_r e_Wrap_rUnwrappedArg 1 0 0 0
		updatepop_a	0 1
		update_a	0 2
		pop_a	1

		| update i-th element of _args array with wrapped arg
		push_b	0
		update	_ 1 0

		| increment index
		incI

		push_b	0
		push_b	2
		eqI
		jmp_false	wrap_args_loop

		pop_b	2
					| A: <_{args}> <node> <result>
					| B:
|	.d 3 0
		jmp	wrap_descriptor

	:wrap_no_args
					| A: <node> <result>
					| B: <0>
		create_array_	_ 1 0

|	.o 3 0
	:wrap_descriptor
					| A: <_{args}> <node> <result>
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1
					| A: <node> <_{args}> <result>
		eq_nulldesc	_Tuple 0
		jmp_false	not_a_tuple
		build	e_Wrap_dWrappedDescriptorTuple 0 _hnf
|	.d 4 0
		jmp	wrap_other
	:not_a_tuple

		eq_nulldesc	_Cons 0
		jmp_false	not_a_cons
		build	e_Wrap_dWrappedDescriptorCons 0 _hnf
		jmp	wrap_other
	:not_a_cons

		eq_desc	_Nil 0 0
		jmp_false	not_a_nil
		build	e_Wrap_dWrappedDescriptorNil 0 _hnf
		jmp	wrap_other
	:not_a_nil

					| A: <node> <_{args}> <result>
		pushD_a	0
		build_r	e_Wrap_rWrappedDescriptorId 0 1 0 0
		pop_b	1

		build_r	e_Wrap_kWrappedDescriptorOther 1 0 0 0
		update_a	0 1
		pop_a	1
|	.o 4 0
	:wrap_other
					| A: <descriptor> <node> <_{args}> <result>
		update_a	0 1
		pop_a	1
					| A: <descriptor> <_{args}> <result>
		fill_r	e_Wrap_kWrappedOther 2 0 2 0 0
		pop_a	2
					| A: <result>
	.d 1 0
		rtn

|	.o 2 0
	| constructors with strict arguments are also represented by records
	:wrap_record
		pushI	0
		pushD_a	0
					| A: <node> <result>
					| B: <desc> <return>
		push_t_r_args

	:wrap_record_fields
					| A: <afield_1 .. afield_m> <result>
					| B: <l> <bfield_1 .. bfield_n> <desc> <return>
					| (l: points to record layout,
					|	desc: record descriptor
					|	return: return selector)

		| determine record  kind
		push_b	0
		push_r_arg_t

		| test if it's a constructor with strict arguments
		eqC_b	'd' 0
		jmp_true	is_constructor

		| test if it's a unboxed list
		eqC_b	'l' 0
		jmp_true	is_unboxed_list

		jmp	is_record

	:is_constructor
		pop_b	1
		| increment <l> past the 'c'
		incI
		pushC	'c'
		jmp	count_fields

	:is_unboxed_list
		pop_b	1
		| increment <l> past the 'l'
		incI

		| test if it's an unboxed list of records
		push_b	0
		push_r_arg_t
		eqC_b	'R' 0
		jmp_true	is_unboxed_list_records

		pop_b	1
		pushC	'l'
		jmp	count_fields

	:is_unboxed_list_records
		pop_b	1
		| increment <l> past the 'R'
		incI
		pushC	'u'
		jmp	count_fields

	:is_record
		pop_b	1
		pushC	'r'
		jmp	count_fields

	:count_fields
		push_b	1

	:count_fields_loop
					| A: <afield_1 .. afield_m> <result>
					| B: <p> <record_kind> <l> <bfield_1 .. bfield_n> <desc> <return> 
					| (p=l+offset)
		push_b	0
		push_r_arg_t
		eqI_b	0 0
		jmp_true	end_count_record_fields
		pop_b	1

		| increment <p>
		incI		

		jmp	count_fields_loop

	:end_count_record_fields
		pop_b	1
		push_b	0
		update_b	3 1
		subI
					| A: <afield_1 .. afield_m> <result>
					| B: <n+m> <record_kind> <l> <bfield_1 .. bfield_n> <desc> <return>
		create_array_	_ 1 0
		pushI	0
		push_b	2
		update_b	2 3
		update_b	1 2
		update_b	0 1
		pop_b	1

	:wrap_fields_loop
					| A: <_{fields}> <afield_ .. afield_m> <result>
					| B: <p> <i> <record_kind> <bfield_ .. bfield_n> <desc> <return>
		push_b	0
		push_r_arg_t
		eqI_b	0 0
		jmp_true	end_wrap_record_fields
		eqC_b	'i' 0
		jmp_true	wrap_int_field
		eqC_b	'c' 0
		jmp_true	wrap_char_field
		eqC_b	'r' 0
		jmp_true	wrap_real_field
		eqC_b	'b' 0
		jmp_true	wrap_bool_field
		eqC_b	'f' 0
		jmp_true	wrap_file_field
		eqC_b	'a' 0
		jmp_true	wrap_graph_field
		print_sc	"Wrap.shallowWrap: unimplemented record field type\n"
		halt

	:wrap_int_field
		pop_b	1

		| create and fill int node
		create
		fillI_b	3 0
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1

		update_b	2 3
		update_b	1 2
		update_b	0 1
		pop_b	1

		jmp	wrap_field

	:wrap_char_field
		pop_b	1

		| create and fill char node
		create
		fillC_b	3 0
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1

		update_b	2 3
		update_b	1 2
		update_b	0 1
		pop_b	1

		jmp	wrap_field

	:wrap_bool_field
		pop_b	1

		| create and fill bool node
		create
		fillB_b	3 0
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1

		update_b	2 3
		update_b	1 2
		update_b	0 1
		pop_b	1

		jmp	wrap_field


	:wrap_real_field
		pop_b	1

		| create and fill real node
		create
		fillR_b	3 0
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1

		update_b	2 4
		update_b	1 3
		update_b	0 2
		pop_b	2

		jmp	wrap_field

	:wrap_file_field
		pop_b	1

		| create and fill file node
		create
		fillF_b	3 0
		push_a	1
		update_a	1 2
		update_a	0 1
		pop_a	1

		update_b	2 4
		update_b	1 3
		update_b	0 2
		pop_b	2

		jmp	wrap_field

	:wrap_graph_field
		pop_b	1
		jmp	wrap_field

	:wrap_field
					| A: <_{fields}> <field> <afield_ .. afield_m>
		| wrap field
		push_a	1
		build_r e_Wrap_rUnwrappedArg 1 0 0 0
		updatepop_a	0 1
		update_a	0 2
		pop_a	1

		| update i-th element of _fields array with wrapped field
		push_b	1
		update _ 1 0
					| A: <_{fields}> <afield_ .. afield_m> <result>
					| B: <p> <i> <record_kind> <bfield_ .. bfield_n> <desc> <return>

		| increment index
		push_b	1
		incI
		update_b	0 2
		pop_b	1

		| increment pointer in layout string
		incI

		jmp	wrap_fields_loop

	:end_wrap_record_fields
					| A: <_{fields}> <result>
					| B: <i=0> <p> <i> <record_kind> <desc> <return>
		pop_b	3
					| A: <_{fields}> <result>
					| B: <record_kind> <desc> <return>

		push_b	1
		update_b	1 2
		update_b	0 1
		pop_b	1
					| B: <desc> <record_kind> <return>

		| create WrappedDescriptorOther node
		build_r	e_Wrap_rWrappedDescriptorId 0 1 0 0
		pop_b	1
		build_r	e_Wrap_kWrappedDescriptorOther 1 0 0 0
		update_a	0 1
		pop_a 1
					| A: <descriptor> <{fields}> <result>
					| B: <record_kind> <return>

		eqC_b	'r' 0
		jmp_true fill_record_result
		eqC_b	'c' 0
		jmp_true fill_constructor_result
		eqC_b	'l' 0
		jmp_true fill_unboxed_list_result
		eqC_b	'u' 0
		jmp_true fill_unboxed_list_records_result

		print_sc	"Wrap.shallowWrap: (record fields) unknown record kind\n"
		halt

	:fill_record_result
		pop_b	1
		| fill result node for record
		fill_r	e_Wrap_kWrappedRecord 2 0 2 0 0
		pop_a	2
		jmp	wrapped_record_return_to_caller

	:fill_constructor_result
		pop_b	1
		| fill result node for constructor

		fill_r	e_Wrap_kWrappedOther 2 0 2 0 0
		pop_a	2
		jmp	wrapped_record_return_to_caller

	:fill_unboxed_list_result
		pop_b	1
		| fill result node for constructor

		fill_r	e_Wrap_kWrappedUnboxedList 2 0 2 0 0
		pop_a	2
		jmp	wrapped_record_return_to_caller

	:fill_unboxed_list_records_result
		pop_b	1
		| fill result node for constructor

		fill_r	e_Wrap_kWrappedUnboxedRecordList 2 0 2 0 0
		pop_a	2
		jmp	wrapped_record_return_to_caller

	:wrapped_record_return_to_caller
					| A: <result>
					| B: <return>
		| return to caller (determined by the return selector)
		eqI_b	0 0
		jmp_true	wrap_record_return_node
		eqI_b	1 0
		jmp_true	wrap_record_array_return
		print_sc	"Wrap.shallowWrap: (record fields) unknown return selector\n"
		halt

	:wrap_record_return_node
					| A: <result>
					| B: <return>
		pop_b	1
	.d 1 0
					| A: <result>
					| B:
		rtn

	:wrap_array
					| A: <array> <result>
		| replace ARRAY by _ARRAY_
		pushA_a	0
		update_a	0 1
		pop_a		1

	:wrap__array
					| A: <_array> <result>
		eq_desc	_STRING_ 0 0
		jmp_false	not_a_string

	:wrap__string
		| fill result node
		fill_r	e_Wrap_kWrappedString 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
		rtn
	:not_a_string
	
		| push array element descriptor
		push_r_args_b	0 0 2 2 1
					| A: <_array> <result>
					| B: <desc>
		push_b	0
		eq_desc_b	BOOL 0
		jmp_true	wrap_bool_array
		push_b	0
		eq_desc_b	INT 0
		jmp_true	wrap_int_array
		push_b	0
		eq_desc_b	REAL 0
		jmp_true	wrap_real_array
		push_b	0
		eq_desc_b	FILE 0
		jmp_true	wrap_file_array

		pushI	0
		push_a	0
		push_arraysize	_ 0 1
					| A: <_array> <result>
					| B: <n> <i> <desc>
		push_b	2
		update_b	2 3
		update_b	1 2
		update_b	0 1
		pop_b	1
					| B: <desc> <n> <i>
		pushI	0
		eqI
					| B: <n> <i>
		jmp_false	wrap_record_array

		push_b	0
		create_array_	_ 1 0
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>
	.d 3 2 i i
		jmp	wrap_array_test

	.o 3 2 i i
	:wrap_array_elements
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>

		| wrap element
		push_b	1
		push_a	1
		select	_ 1 0

		build_r e_Wrap_rUnwrappedArg 1 0 0 0
		updatepop_a	0 1
					| A: <element> <_wrapped_array> <_array> <result>
					| B: <n> <i>
		| update i-th element of _wrapped_array with wrapped element
		push_a	1
		push_b	1
		update	_ 1 0
		update_a	0 1
		pop_a	1
					| B: <n> <i>
		| increment index
		push_b	1
		incI
		update_b	0 2

		| decrement n
		pop_b	1
		decI

	.o 3 2 i i
	:wrap_array_test
					| B: <n> <i>
		eqI_b	0 0
	.d 3 2 i i
		jmp_false	wrap_array_elements
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>
		pop_b	2
		update_a	0 1
		pop_a	1
					| A: <_wrapped_array> <result>
					| B:
		| fill result node
		fill_r	e_Wrap_kWrappedArray 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
					| B:
		rtn


	:wrap_bool_array
					| A: <_array> <result>
					| B: <desc>
		pop_b	1

		| fill result node
		fill_r	e_Wrap_kWrappedBoolArray 1 0 1 0 0
		pop_a	1
    .d 1 0
                    | A: <result>
                    | B:
        rtn

	:wrap_int_array
					| A: <_array> <result>
					| B: <desc>
		pop_b	1

		| fill result node
		fill_r	e_Wrap_kWrappedIntArray 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
					| B:
		rtn

	:wrap_real_array
					| A: <_array> <result>
					| B: <desc>
		pop_b	1

		| fill result node
		fill_r	e_Wrap_kWrappedRealArray 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
					| B:
		rtn

	:wrap_file_array
					| A: <_array> <result>
					| B: <desc>
		pop_b	1

		| fill result node
		fill_r	e_Wrap_kWrappedFileArray 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
					| B:
		rtn

	:wrap_record_array
					| A: <_array> <result>
					| B: <n> <i>
		push_b	0
		create_array_	_ 1 0
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>

		jmp	wrap_record_array_test

	:wrap_record_array_loop
					| B: <n> <i>
		pushI	1
		| push record element descriptor
		push_r_args_b	1 0 2 2 1
					| B: <desc> <return> <n> <i>

		| create result node for wrap_record_fields
		create

		| push fields from i-th array element
		push_b	3
		push_a	2
		push_a_r_args
					| A: <afield_1 .. afield_m> <elresult> <_wrapped_array> <_array> <result>
					| B: <l> <bfieldb_1 .. bfield_n> <desc> <return> <n> <i>
		| wrap record element
		jmp	wrap_record_fields
	:wrap_record_array_return
					| A: <element> <_wrapped_array> <_array> <result>
					| B: <return> <n> <i>
		pop_b	1
					| A: <element> <_wrapped_array> <_array> <result>
					| B: <n> <i> 
		| update i-th of _wrapped_array with wrapped record element
		push_a	1
		push_b	1
		update	_ 1 0
		update_a	0 1
		pop_a	1
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>
		| increment index
		push_b	1
		incI
		update_b	0 2
		pop_b	1

		| decrement n
		decI

	:wrap_record_array_test
		eqI_b	0 0
		jmp_false	wrap_record_array_loop
					| A: <_wrapped_array> <_array> <result>
					| B: <n> <i>
		pop_b	2
					| B:

		update_a	0 1
		pop_a	1
					| A: <_wrapped_array> <result>
		| fill result node
		fill_r	e_Wrap_kWrappedArray 1 0 1 0 0
		pop_a	1
	.d 1 0
					| A: <result>
					| B:
		rtn
	}
