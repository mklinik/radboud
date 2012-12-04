

	.globl	copy_graph_to_string
	.globl	remove_forwarding_pointers_from_graph
	.globl	collect_1

	.text

	.intel_syntax noprefix

	.globl	__copy__graph__to__string

__copy__graph__to__string:
	push	rcx

	mov	r12,rsi
	mov	r13,rdi

	mov	rdi,rcx
	mov	rsi,r13
	lea	rdx,[r13+r15*8]

	mov	rbp,rsp
	and	rsp,-8
	call	copy_graph_to_string
	mov	rsp,rbp
	mov	rcx,qword ptr [rsp]
	push	rax

	mov	rdi,rcx
	lea	rsi,[r13+r15*8]

	mov	rbp,rsp
	and	rsp,-8
	call	remove_forwarding_pointers_from_graph
	mov	rsp,rbp
	pop	rcx

	mov	rsi,r12
	mov	rdi,r13

	test	rcx,rcx
	jne	__copy__graph__to__string_1

	pop	rcx

	lea	rbx,1[r15]
	sub	r15,rbx
	call	collect_1
	add	r15,rbx
	jmp	__copy__graph__to__string

__copy__graph__to__string_1:
	add	rsp,8

	mov	rax,qword ptr 8[rcx]
	add	rax,16+7
	and	rax,-8
	add	rdi,rax
	sar	rax,3
	sub	r15,rax
	ret
