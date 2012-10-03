
.globl	copy_graph_to_string
.globl	remove_forwarding_pointers_from_graph
.globl	collect_1l
.globl	end_heap

	.text

.globl	__copy__graph__to__string

__copy__graph__to__string:
	pushl	%ecx

	movl	end_heap,%ebx
	addl	$32,%ebx
	pushl	%ebx
	pushl	%edi
	pushl	%ecx
	call	copy_graph_to_string
	addl	$12,%esp

	movl	(%esp),%ecx
	pushl	%eax

	movl	end_heap,%ebx
	addl	$32,%ebx
	pushl	%ebx
	pushl	%ecx
	call	remove_forwarding_pointers_from_graph
	addl	$8,%esp

	popl	%ecx

	testl	%ecx,%ecx
	jne		__copy__graph__to__string_1

	popl	%ecx

	movl	end_heap,%ebp
	addl	$4-32+32,%ebp
	call	collect_1l
	jmp		__copy__graph__to__string

__copy__graph__to__string_1:
	addl	$4,%esp

	movl	4(%ecx),%eax
	addl	$8+3,%eax
	andl	$-4,%eax
	addl	%eax,%edi
	
	ret

