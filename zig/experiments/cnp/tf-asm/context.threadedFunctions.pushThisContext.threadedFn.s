; symbol: _context.threadedFunctions.pushThisContext.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_context.threadedFunctions.pushThisContext.threadedFn:
    7434:	ff c3 00 d1	sub	sp, sp, #0x30
    7438:	f4 4f 01 a9	stp	x20, x19, [sp, #0x10]
    743c:	fd 7b 02 a9	stp	x29, x30, [sp, #0x20]
    7440:	fd 83 00 91	add	x29, sp, #0x20
    7444:	89 fc 70 d3	lsr	x9, x4, #48
    7448:	09 03 00 b4	cbz	x9, 0x74a8
    744c:	e8 03 01 aa	mov	x8, x1
    7450:	89 bc 40 f2	ands	x9, x4, #0xffffffffffff
    7454:	e0 01 00 54	b.eq	0x7490
    7458:	f3 03 00 aa	mov	x19, x0
    745c:	e0 03 00 91	mov	x0, sp
    7460:	e1 03 03 aa	mov	x1, x3
    7464:	f4 03 02 aa	mov	x20, x2
    7468:	e2 03 08 aa	mov	x2, x8
    746c:	e3 03 09 aa	mov	x3, x9
    7470:	00 00 00 94	bl	_context.push
    7474:	e1 0f 40 a9	ldp	x1, x3, [sp]
    7478:	68 e0 00 91	add	x8, x3, #0x38
    747c:	65 02 5f f8	ldur	x5, [x19, #-0x10]
    7480:	04 b1 7d 92	and	x4, x8, #0xfffffffffff8
    7484:	e0 03 13 aa	mov	x0, x19
    7488:	e2 03 14 aa	mov	x2, x20
    748c:	03 00 00 14	b	0x7498
    7490:	05 00 5f f8	ldur	x5, [x0, #-0x10]
    7494:	e1 03 08 aa	mov	x1, x8
    7498:	fd 7b 42 a9	ldp	x29, x30, [sp, #0x20]
    749c:	f4 4f 41 a9	ldp	x20, x19, [sp, #0x10]
    74a0:	ff c3 00 91	add	sp, sp, #0x30
    74a4:	a0 00 1f d6	br	x5
    74a8:	00 00 00 90	adrp	x0, ___anon_8320@PAGE
    74ac:	00 00 00 91	add	x0, x0, ___anon_8320@PAGEOFF
    74b0:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    74b4:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    74b8:	61 01 80 52	mov	w1, #0xb
    74bc:	00 00 00 94	bl	_debug.defaultPanic
