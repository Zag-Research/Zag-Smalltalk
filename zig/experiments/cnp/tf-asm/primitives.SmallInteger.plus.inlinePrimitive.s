; symbol: _primitives.SmallInteger.+.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.SmallInteger.+.inlinePrimitive:
    755c:	ff 83 00 d1	sub	sp, sp, #0x20
    7560:	fd 7b 01 a9	stp	x29, x30, [sp, #0x10]
    7564:	fd 43 00 91	add	x29, sp, #0x10
    7568:	28 04 40 f9	ldr	x8, [x1, #0x8]
    756c:	09 1d 40 92	and	x9, x8, #0xff
    7570:	3f a5 02 f1	cmp	x9, #0xa9
    7574:	61 01 00 54	b.ne	0x75a0
    7578:	29 00 40 f9	ldr	x9, [x1]
    757c:	2a 1d 40 92	and	x10, x9, #0xff
    7580:	5f a5 02 f1	cmp	x10, #0xa9
    7584:	e1 00 00 54	b.ne	0x75a0
    7588:	29 dd 78 92	and	x9, x9, #0xffffffffffffff00
    758c:	08 01 09 ab	adds	x8, x8, x9
    7590:	e9 77 9f 1a	cset	w9, vs
    7594:	e9 23 00 39	strb	w9, [sp, #0x8]
    7598:	e9 1f 00 39	strb	w9, [sp, #0x7]
    759c:	a7 00 00 54	b.vc	0x75b0
    75a0:	05 04 41 f8	ldr	x5, [x0], #0x10
    75a4:	fd 7b 41 a9	ldp	x29, x30, [sp, #0x10]
    75a8:	ff 83 00 91	add	sp, sp, #0x20
    75ac:	a0 00 1f d6	br	x5
    75b0:	28 8c 00 f8	str	x8, [x1, #0x8]!
    75b4:	05 10 40 f9	ldr	x5, [x0, #0x20]
    75b8:	00 c0 00 91	add	x0, x0, #0x30
    75bc:	fa ff ff 17	b	0x75a4
