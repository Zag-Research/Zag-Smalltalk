; symbol: _primitives.Float.<=.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.Float.<=.inlinePrimitive:
    7e70:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    7e74:	fd 03 00 91	mov	x29, sp
    7e78:	28 04 40 f9	ldr	x8, [x1, #0x8]
    7e7c:	1f 05 7f f2	tst	x8, #0x6
    7e80:	60 02 00 54	b.eq	0x7ecc
    7e84:	08 09 00 d1	sub	x8, x8, #0x2
    7e88:	08 11 c8 93	ror	x8, x8, #0x4
    7e8c:	00 01 67 9e	fmov	d0, x8
    7e90:	28 00 40 f9	ldr	x8, [x1]
    7e94:	1f 05 7f f2	tst	x8, #0x6
    7e98:	00 03 00 54	b.eq	0x7ef8
    7e9c:	08 09 00 d1	sub	x8, x8, #0x2
    7ea0:	08 11 c8 93	ror	x8, x8, #0x4
    7ea4:	01 01 67 9e	fmov	d1, x8
    7ea8:	00 20 61 1e	fcmp	d0, d1
    7eac:	28 0c 80 52	mov	w8, #0x61
    7eb0:	29 0d 80 52	mov	w9, #0x69
    7eb4:	28 91 88 9a	csel	x8, x9, x8, ls
    7eb8:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7ebc:	05 10 40 f9	ldr	x5, [x0, #0x20]
    7ec0:	00 c0 00 91	add	x0, x0, #0x30
    7ec4:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    7ec8:	a0 00 1f d6	br	x5
    7ecc:	09 01 40 92	and	x9, x8, #0x1
    7ed0:	1f 01 00 f1	cmp	x8, #0x0
    7ed4:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    7ed8:	e1 01 00 54	b.ne	0x7f14
    7edc:	09 01 40 79	ldrh	w9, [x8]
    7ee0:	3f 85 00 f1	cmp	x9, #0x21
    7ee4:	81 01 00 54	b.ne	0x7f14
    7ee8:	00 05 40 fd	ldr	d0, [x8, #0x8]
    7eec:	28 00 40 f9	ldr	x8, [x1]
    7ef0:	1f 05 7f f2	tst	x8, #0x6
    7ef4:	41 fd ff 54	b.ne	0x7e9c
    7ef8:	e8 00 00 b4	cbz	x8, 0x7f14
    7efc:	c8 00 00 37	tbnz	w8, #0x0, 0x7f14
    7f00:	09 01 40 79	ldrh	w9, [x8]
    7f04:	3f 85 00 f1	cmp	x9, #0x21
    7f08:	61 00 00 54	b.ne	0x7f14
    7f0c:	01 05 40 fd	ldr	d1, [x8, #0x8]
    7f10:	e6 ff ff 17	b	0x7ea8
    7f14:	05 04 41 f8	ldr	x5, [x0], #0x10
    7f18:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    7f1c:	a0 00 1f d6	br	x5
