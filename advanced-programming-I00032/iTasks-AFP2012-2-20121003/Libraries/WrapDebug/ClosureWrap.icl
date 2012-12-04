/*
	Wrap Clean nodes (for debugging purposes).

	Version 1.0.5
	Arjen van Weelden
	arjenw@cs.ru.nl
	Ronny Wichers Schreur
	ronny@cs.ru.nl
*/
implementation module ClosureWrap

import StdEnv
import Wrap

// check whether all functions have descriptors
closureHasDescriptor :: Bool
closureHasDescriptor
	=:	use_ea_entry_f && has_descriptor f
	where
		f
			=>	[]
		// make sure an ea entry is generated for f
		use_ea_entry_f
			=	const True [f]
		// test whether f has a descriptor, because f has an ea entry the
		// compiler will have generated a null descriptor in case the
		// real descriptor for f is not needed (memory profiling is off
		// and dynamics are disabled)
		has_descriptor :: a -> Bool
		has_descriptor _
			=	code
			{								|A| closure |B|
				pushI		8				|B| 8
				pushD_a		0				|B| n_entry | 8
				subI						|B| n_entry-8
				push_b_a	0				|A| n_entry-8 | closure
				pop_b		1				|B|
				pushD_a		0				|B| [n_entry-8]=desc
				pop_a		2				|A| 
				pushI		4294967295		| (1<<32)-1
				and%
				pushI		0				|B| 0 | desc
				eqI							|B| desc == 0
				notB
			}

closureDeepWrapNode :: .a -> ClosureDeeplyWrappedNode
closureDeepWrapNode node
	=	wrap node

instance wrap ClosureWrappedArg where
	wrap x
		=	{closurearg = wrap x}

instance wrap (ClosureWrappedNode a) | wrap a where
	wrap x
		=	mapWrapClosureNode wrapArg (closureShallowWrapNode x)
		where
			wrapArg :: !UnwrappedArg -> a | wrap a
			wrapArg arg
				=	wrap arg.node

mapWrapClosureNode :: (a -> b) (ClosureWrappedNode a) -> ClosureWrappedNode b
mapWrapClosureNode f (Closure desc args unboxed)
	=	Closure desc {f e \\ e <-: args} unboxed
mapWrapClosureNode f (NotAClosure node)
	=	NotAClosure (mapWrapNode f node)

closureShallowWrapNode :: .a -> ClosureShallowlyWrappedNode
closureShallowWrapNode node = code {
	pushI		2				|B| 2
	pushD_a		0				|B| node-desc | 2
	and%						|B| node-desc bitand 2
	pushI		2				|B| 2 | node-desc bitand 2
	eqI							|B| 2 == node-desc bitand 2
	jmp_false	is_a_closure	|B|

	create						|A| <new> | node | result
	push_a 1					|A| node | <new> | node | result
	update_a 1 2				|A| node | <new> | arg | result
	updatepop_a 0 1				|A| node | <new> | result
.d 2 0
	jsr e_Wrap_sshallowWrap		|A| WNode node | result
.o 1 0
	fill_r e_ClosureWrap_kNotAClosure 1 0 1 0 0
	pop_a 1						|A| WNotAClos (WNode node)
	.d 1 0
	rtn

:is_a_closure
	pushI		4				|B| 4
	pushD_a		0				|B| n-entry | 4
	subI						|B| n-entry - 4
	push_b_a	0				|A| n-entry-4 | node | result
	pop_b		1				|B|
	pushD_a		0				|B| [n-entry-4]=arity
	pop_a		1				|A| node | result
	pushI		4294967295		| (1<<32)-1
	and%

	pushI		0				|B| 0 | arity
	push_b		1				|B| arity | 0 | arity
	ltI							|B| arity < 0 | arity
	jmp_false	pos_arity		|B| arity

	pop_b		1				|B|
	pushI		1				|B| 1=arity

|.d 2 1 i
:pos_arity
	pushI		8				|B| 8 | arity
	push_b		1				|B| arity | 8 | arity
	shiftrU						|B| (arity / 256)=unboxed | arity
	pushI		255				|B| 255 | unboxed | arity
	push_b		2				|B| arity | 255 | unboxed | arity
	and%						|B| arity bitand 255 | unboxed | arity
	update_b	1 2				|B| (arity bitand 255) | unboxed | unboxed
	subI						|B| (arity  bitand 255 - unboxed)=boxed | unboxed

	pushI		8				|B| 8 | boxed | unboxed
	pushD_a		0				|B| n-entry | 8 | boxed | unboxed
	subI						|B| n-entry - 8 | boxed | unboxed
	push_b_a	0				|A| n-entry-8 | node | result
	pop_b		1				|B| boxed | unboxed
	pushD_a		0				|B| [n-entry-8] | boxed | unboxed
	pop_a		1				|A| node | result
	pushI		4294967295		| (1<<32)-1
	and%
	pushI		2				|B| 2 | [n-entry-8] | boxed | unboxed
	addI						|B| ([n-entry-8] + 2)=desc | boxed | unboxed

	| test for null descriptor
	pushI		2				|B| 2 | desc | boxed | unboxed
	push_b		1				|B| desc | 2 | desc | boxed | unboxed
	eqI							|B| desc == 2 | desc | boxed | unboxed
	jmp_true	no_descriptor	|B| desc | boxed | unboxed

	| even if the descriptor is not null, it's not guaranteed that there
	| is a descriptor, we make a pessimistic estimate here
.d 0 0
	jsr e_ClosureWrap_sclosureHasDescriptor
.o 0 1 b
	jmp_false no_descriptor

|.o 2 3 iii
:has_descriptor
	build_r	e_Wrap_rWrappedDescriptorId 0 1 0 0	|A| wdescid | node | result
	build_r	e_Wrap_kWrappedDescriptorOther 1 0 0 0 |A| wdesc | wdescid | {args} | wrapNodeStrict node | result
	updatepop_a	0 1				|A| wdesc | {args} | wrapNodeStrict node | result
	pop_b	1					|B| boxed | unboxed

	jmp	wrap_args

:no_descriptor
	pop_b		1				|B| boxed | unboxed
	buildh e_Wrap_dWrappedDescriptorUnknown 0

	jmp	wrap_args

:wrap_args
	push_b		0				|B| boxed | boxed | unboxed
	create_array_	_ 1 0		|A| {(WrappedNode * boxed)=args} | wdesc | node | result ||B| boxed | unboxed
	pushI		0				|B| 0  | boxed | unboxed

|.o 3 4 iiii
:args_loop						|A| {args} | wdesc| node | result ||B| i | desc | boxed | unboxed
	push_b		1				|B| boxed | i | boxed | unboxed
	push_b		1				|B| i | boxed | i | boxed | unboxed
	ltI							|B| i < boxed | i | boxed | unboxed
	jmp_false	args_done		|B| i | boxed | unboxed

	pushI		32
	pushI		4294967296		| 1<<32
	shiftr%						| (1<<32)>>32
	pushI		2
	addI						| 2 if 32 bits, 3 if 64 bits
	push_b		1				|B| i | 2 | i | boxed | unboxed
	incI						|B| i + 1 | 2 | i | boxed | unboxed
	shiftl%						|B| i * 4 + 4 | i | boxed | unboxed
	push_a_b	2				|B| addr node | i * 4 + 4 | i  | boxed | unboxed
	addI						|B| addr node + i * 4 + 4 | i | boxed | unboxed
	push_b_a	0				|A| node+i*4+4 | {args} | wdesc | node | result
	pop_b		1				|B| i | boxed | unboxed
	pushD_a		0				|B| [node+i*4+4] | i | boxed | unboxed
	pop_a		1				|A| {args} | wdesc | node | result
	push_b_a	0				|A| [node+i*4+4] | {args} | wdesc node | result
	pop_b		1				|B| i | boxed | unboxed
	
	build_r e_Wrap_rUnwrappedArg 1 0 0 0
	updatepop_a	0 1
	| build e_Wrap_tUnwrappedArg 1 e_Wrap_cUnwrappedArg
	
	push_a		1				|A| {args} | wrappedNode | {args} | wdesc | node | result
	update_a	1 2				|A| {args} | wrappedNode | wrappedNode | wdesc | node | result
	updatepop_a	0 1				|A| {args} | wrappedNode | wdesc | node | result
	push_b		0				|B| i | i | boxed | unboxed
	update		_ 1 0			|A| {args & [i] = wrappedNode} | wdesc | node | result ||B| i | boxed | unboxed
	incI						|B| i + 1 | boxed | unboxed
|.d 3 4 iiii
	jmp			args_loop

|.o 3 4 iiii
:args_done						|A| {args} | wdesc | node | result ||B| i | boxed | unboxed
	pop_b		2				|B| unboxed
	update_a	0 2				|A| {args} | wdesc | {args} | result
	pop_a		1				|A| wdesc | {args} | result

	fill_r  e_ClosureWrap_kClosure 2 1 2 0 0 |A| WDescId desc | {args}  | WClosure wdesc {args} unboxed
	pop_b	1					|B|
	pop_a	2					|A| WOther wdesc {args} unboxed 
	}

