; symbol: _controlWords.dup.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.dup.threadedFn:
    597c:	ff 43 01 d1	sub	sp, sp, #0x50
    5980:	f6 57 02 a9	stp	x22, x21, [sp, #0x20]
    5984:	f4 4f 03 a9	stp	x20, x19, [sp, #0x30]
    5988:	fd 7b 04 a9	stp	x29, x30, [sp, #0x40]
    598c:	fd 03 01 91	add	x29, sp, #0x40
    5990:	e8 03 01 aa	mov	x8, x1
    5994:	15 85 5f f8	ldr	x21, [x8], #-0x8
    5998:	1f 21 7d f2	tst	x8, #0xff8
    599c:	20 01 00 54	b.eq	0x59c0
    59a0:	15 01 00 f9	str	x21, [x8]
    59a4:	05 04 41 f8	ldr	x5, [x0], #0x10
    59a8:	e1 03 08 aa	mov	x1, x8
    59ac:	fd 7b 44 a9	ldp	x29, x30, [sp, #0x40]
    59b0:	f4 4f 43 a9	ldp	x20, x19, [sp, #0x30]
    59b4:	f6 57 42 a9	ldp	x22, x21, [sp, #0x20]
    59b8:	ff 43 01 91	add	sp, sp, #0x50
    59bc:	a0 00 1f d6	br	x5
    59c0:	f3 03 00 aa	mov	x19, x0
    59c4:	e0 23 00 91	add	x0, sp, #0x8
    59c8:	f4 03 02 aa	mov	x20, x2
    59cc:	e2 03 03 aa	mov	x2, x3
    59d0:	e3 03 04 aa	mov	x3, x4
    59d4:	00 00 00 94	bl	_process.Stack.spillStack
    59d8:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    59dc:	35 8c 1f f8	str	x21, [x1, #-0x8]!
    59e0:	65 06 41 f8	ldr	x5, [x19], #0x10
    59e4:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    59e8:	e0 03 13 aa	mov	x0, x19
    59ec:	e2 03 14 aa	mov	x2, x20
    59f0:	ef ff ff 17	b	0x59ac
