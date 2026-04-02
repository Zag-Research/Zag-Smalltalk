; symbol: _primitives.SmallInteger.*.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.SmallInteger.*.inlinePrimitive:
    7624:	ff 83 00 d1	sub	sp, sp, #0x20
    7628:	fd 7b 01 a9	stp	x29, x30, [sp, #0x10]
    762c:	fd 43 00 91	add	x29, sp, #0x10
    7630:	28 04 40 f9	ldr	x8, [x1, #0x8]
    7634:	09 1d 40 92	and	x9, x8, #0xff
    7638:	3f a5 02 f1	cmp	x9, #0xa9
    763c:	c1 01 00 54	b.ne	0x7674
    7640:	29 00 40 f9	ldr	x9, [x1]
    7644:	2a 1d 40 92	and	x10, x9, #0xff
    7648:	5f a5 02 f1	cmp	x10, #0xa9
    764c:	41 01 00 54	b.ne	0x7674
    7650:	0a dd 78 92	and	x10, x8, #0xffffffffffffff00
    7654:	29 fd 48 93	asr	x9, x9, #8
    7658:	48 7d 09 9b	mul	x8, x10, x9
    765c:	49 7d 49 9b	smulh	x9, x10, x9
    7660:	3f fd 88 eb	cmp	x9, x8, asr #63
    7664:	e9 07 9f 1a	cset	w9, ne
    7668:	e9 23 00 39	strb	w9, [sp, #0x8]
    766c:	e9 1f 00 39	strb	w9, [sp, #0x7]
    7670:	a0 00 00 54	b.eq	0x7684
    7674:	05 04 41 f8	ldr	x5, [x0], #0x10
    7678:	fd 7b 41 a9	ldp	x29, x30, [sp, #0x10]
    767c:	ff 83 00 91	add	sp, sp, #0x20
    7680:	a0 00 1f d6	br	x5
    7684:	29 15 80 52	mov	w9, #0xa9
    7688:	08 01 09 aa	orr	x8, x8, x9
    768c:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7690:	05 10 40 f9	ldr	x5, [x0, #0x20]
    7694:	00 c0 00 91	add	x0, x0, #0x30
    7698:	f8 ff ff 17	b	0x7678
