; symbol: _primitives.SmallInteger.-.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.SmallInteger.-.inlinePrimitive:
    75c0:	ff 83 00 d1	sub	sp, sp, #0x20
    75c4:	fd 7b 01 a9	stp	x29, x30, [sp, #0x10]
    75c8:	fd 43 00 91	add	x29, sp, #0x10
    75cc:	28 04 40 f9	ldr	x8, [x1, #0x8]
    75d0:	09 1d 40 92	and	x9, x8, #0xff
    75d4:	3f a5 02 f1	cmp	x9, #0xa9
    75d8:	61 01 00 54	b.ne	0x7604
    75dc:	29 00 40 f9	ldr	x9, [x1]
    75e0:	2a 1d 40 92	and	x10, x9, #0xff
    75e4:	5f a5 02 f1	cmp	x10, #0xa9
    75e8:	e1 00 00 54	b.ne	0x7604
    75ec:	29 dd 78 92	and	x9, x9, #0xffffffffffffff00
    75f0:	08 01 09 eb	subs	x8, x8, x9
    75f4:	e9 77 9f 1a	cset	w9, vs
    75f8:	e9 23 00 39	strb	w9, [sp, #0x8]
    75fc:	e9 1f 00 39	strb	w9, [sp, #0x7]
    7600:	a7 00 00 54	b.vc	0x7614
    7604:	05 04 41 f8	ldr	x5, [x0], #0x10
    7608:	fd 7b 41 a9	ldp	x29, x30, [sp, #0x10]
    760c:	ff 83 00 91	add	sp, sp, #0x20
    7610:	a0 00 1f d6	br	x5
    7614:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7618:	05 10 40 f9	ldr	x5, [x0, #0x20]
    761c:	00 c0 00 91	add	x0, x0, #0x30
    7620:	fa ff ff 17	b	0x7608
