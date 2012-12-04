
.globl	collect_1l
.globl	end_heap

.globl	copy_string_to_graph
.globl	remove_forwarding_pointers_from_string

	.text

.globl	__copy__string__to__graph

__copy__string__to__graph:
	pushl	%ecx

	subl	$4,%esp

	pushl	%esp
	movl	end_heap,%ebx
	addl	$32,%ebx
	pushl	%ebx
	pushl	%edi
	pushl	%ecx
	call	copy_string_to_graph
	addl	$16,%esp

	testl	$1,%eax
	je		__copy__string__to__graph_1

	movl	4(%esp),%ecx
	andl	$-4,%eax

	pushl	%eax
	pushl	%ecx
	call	remove_forwarding_pointers_from_string
	addl	$8,%esp

	popl	%ebp
	popl	%ecx
	
	subl	$32,%ebp
	call	collect_1l
	jmp		__copy__string__to__graph

__copy__string__to__graph_1:
	movl	(%esp),%edi
	movl	%eax,%ecx
	addl	$8,%esp
	ret
