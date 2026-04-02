; symbol: _primitives.BlockClosure.threadedFns.returnLocalClosure.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.returnLocalClosure.threadedFn:
    8784:	ff 83 01 d1	sub	sp, sp, #0x60
    8788:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    878c:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    8790:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    8794:	fd 43 01 91	add	x29, sp, #0x50
    8798:	e8 03 03 aa	mov	x8, x3
    879c:	f3 03 02 aa	mov	x19, x2
    87a0:	e2 03 01 aa	mov	x2, x1
    87a4:	f5 03 00 aa	mov	x21, x0
    87a8:	14 80 00 91	add	x20, x0, #0x20
    87ac:	89 fc 70 d3	lsr	x9, x4, #48
    87b0:	89 01 00 b5	cbnz	x9, 0x87e0
    87b4:	0d 00 00 14	b	0x87e8
    87b8:	e0 83 00 91	add	x0, sp, #0x20
    87bc:	e1 03 08 aa	mov	x1, x8
    87c0:	00 00 00 94	bl	_context.push
    87c4:	e2 23 42 a9	ldp	x2, x8, [sp, #0x20]
    87c8:	09 e1 00 91	add	x9, x8, #0x38
    87cc:	24 b1 7d 92	and	x4, x9, #0xfffffffffff8
    87d0:	a9 0a 40 f9	ldr	x9, [x21, #0x10]
    87d4:	14 25 02 a9	stp	x20, x9, [x8, #0x20]
    87d8:	89 fc 70 d3	lsr	x9, x4, #48
    87dc:	69 00 00 b4	cbz	x9, 0x87e8
    87e0:	83 bc 40 92	and	x3, x4, #0xffffffffffff
    87e4:	a3 fe ff b5	cbnz	x3, 0x87b8
    87e8:	a9 02 40 f9	ldr	x9, [x21]
    87ec:	2a 1d 40 92	and	x10, x9, #0xff
    87f0:	5f a5 02 f1	cmp	x10, #0xa9
    87f4:	01 04 00 54	b.ne	0x8874
    87f8:	29 fd 48 93	asr	x9, x9, #8
    87fc:	3f fd 03 f1	cmp	x9, #0xff
    8800:	a8 03 00 54	b.hi	0x8874
    8804:	41 20 00 d1	sub	x1, x2, #0x8
    8808:	3f 20 7d f2	tst	x1, #0xff8
    880c:	61 01 00 54	b.ne	0x8838
    8810:	e0 23 00 91	add	x0, sp, #0x8
    8814:	e1 03 02 aa	mov	x1, x2
    8818:	e2 03 08 aa	mov	x2, x8
    881c:	e3 03 04 aa	mov	x3, x4
    8820:	00 00 00 94	bl	_process.Stack.spillStack
    8824:	e2 a3 40 a9	ldp	x2, x8, [sp, #0x8]
    8828:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    882c:	89 fc 70 d3	lsr	x9, x4, #48
    8830:	89 fd ff b5	cbnz	x9, 0x87e0
    8834:	ed ff ff 17	b	0x87e8
    8838:	0a dd 78 d3	lsl	x10, x8, #8
    883c:	4a b1 75 92	and	x10, x10, #0xfffffffffff800
    8840:	29 01 0a aa	orr	x9, x9, x10
    8844:	2a 01 80 52	mov	w10, #0x9
    8848:	49 21 09 aa	orr	x9, x10, x9, lsl #8
    884c:	29 00 00 f9	str	x9, [x1]
    8850:	a5 0a 40 f9	ldr	x5, [x21, #0x10]
    8854:	e0 03 14 aa	mov	x0, x20
    8858:	e2 03 13 aa	mov	x2, x19
    885c:	e3 03 08 aa	mov	x3, x8
    8860:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    8864:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    8868:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    886c:	ff 83 01 91	add	sp, sp, #0x60
    8870:	a0 00 1f d6	br	x5
    8874:	00 00 00 90	adrp	x0, ___anon_9243@PAGE
    8878:	00 00 00 91	add	x0, x0, ___anon_9243@PAGEOFF
    887c:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8880:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8884:	41 03 80 52	mov	w1, #0x1a
    8888:	00 00 00 94	bl	_debug.defaultPanic
