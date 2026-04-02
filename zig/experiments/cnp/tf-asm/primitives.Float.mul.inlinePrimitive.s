; symbol: _primitives.Float.*.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.Float.*.inlinePrimitive:
    7d30:	28 04 40 f9	ldr	x8, [x1, #0x8]
    7d34:	1f 05 7f f2	tst	x8, #0x6
    7d38:	60 03 00 54	b.eq	0x7da4
    7d3c:	08 09 00 d1	sub	x8, x8, #0x2
    7d40:	08 11 c8 93	ror	x8, x8, #0x4
    7d44:	00 01 67 9e	fmov	d0, x8
    7d48:	28 00 40 f9	ldr	x8, [x1]
    7d4c:	1f 05 7f f2	tst	x8, #0x6
    7d50:	00 04 00 54	b.eq	0x7dd0
    7d54:	08 09 00 d1	sub	x8, x8, #0x2
    7d58:	08 11 c8 93	ror	x8, x8, #0x4
    7d5c:	01 01 67 9e	fmov	d1, x8
    7d60:	ff 03 02 d1	sub	sp, sp, #0x80
    7d64:	e9 23 06 6d	stp	d9, d8, [sp, #0x60]
    7d68:	fd 7b 07 a9	stp	x29, x30, [sp, #0x70]
    7d6c:	fd c3 01 91	add	x29, sp, #0x70
    7d70:	00 08 61 1e	fmul	d0, d0, d1
    7d74:	08 00 66 9e	fmov	x8, d0
    7d78:	08 f1 c8 93	ror	x8, x8, #0x3c
    7d7c:	08 09 00 91	add	x8, x8, #0x2
    7d80:	1f 05 7f f2	tst	x8, #0x6
    7d84:	80 03 00 54	b.eq	0x7df4
    7d88:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7d8c:	05 10 40 f9	ldr	x5, [x0, #0x20]
    7d90:	00 c0 00 91	add	x0, x0, #0x30
    7d94:	fd 7b 47 a9	ldp	x29, x30, [sp, #0x70]
    7d98:	e9 23 46 6d	ldp	d9, d8, [sp, #0x60]
    7d9c:	ff 03 02 91	add	sp, sp, #0x80
    7da0:	a0 00 1f d6	br	x5
    7da4:	09 01 40 92	and	x9, x8, #0x1
    7da8:	1f 01 00 f1	cmp	x8, #0x0
    7dac:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    7db0:	e1 01 00 54	b.ne	0x7dec
    7db4:	09 01 40 79	ldrh	w9, [x8]
    7db8:	3f 85 00 f1	cmp	x9, #0x21
    7dbc:	81 01 00 54	b.ne	0x7dec
    7dc0:	00 05 40 fd	ldr	d0, [x8, #0x8]
    7dc4:	28 00 40 f9	ldr	x8, [x1]
    7dc8:	1f 05 7f f2	tst	x8, #0x6
    7dcc:	41 fc ff 54	b.ne	0x7d54
    7dd0:	e8 00 00 b4	cbz	x8, 0x7dec
    7dd4:	c8 00 00 37	tbnz	w8, #0x0, 0x7dec
    7dd8:	09 01 40 79	ldrh	w9, [x8]
    7ddc:	3f 85 00 f1	cmp	x9, #0x21
    7de0:	61 00 00 54	b.ne	0x7dec
    7de4:	01 05 40 fd	ldr	d1, [x8, #0x8]
    7de8:	de ff ff 17	b	0x7d60
    7dec:	05 04 41 f8	ldr	x5, [x0], #0x10
    7df0:	a0 00 1f d6	br	x5
    7df4:	00 20 60 1e	fcmp	d0, d0
    7df8:	66 03 00 54	b.vs	0x7e64
    7dfc:	08 00 66 9e	fmov	x8, d0
    7e00:	08 f9 40 92	and	x8, x8, #0x7fffffffffffffff
    7e04:	09 fe ef d2	mov	x9, #0x7ff0000000000000
    7e08:	1f 01 09 eb	cmp	x8, x9
    7e0c:	01 01 00 54	b.ne	0x7e2c
    7e10:	08 00 00 90	adrp	x8, _object.inMemory.pInfMemObject@PAGE
    7e14:	08 01 00 91	add	x8, x8, _object.inMemory.pInfMemObject@PAGEOFF
    7e18:	08 20 60 1e	fcmp	d0, #0.0
    7e1c:	09 00 00 90	adrp	x9, _object.inMemory.nInfMemObject@PAGE
    7e20:	29 01 00 91	add	x9, x9, _object.inMemory.nInfMemObject@PAGEOFF
    7e24:	08 c1 89 9a	csel	x8, x8, x9, gt
    7e28:	d8 ff ff 17	b	0x7d88
    7e2c:	e0 07 02 a9	stp	x0, x1, [sp, #0x20]
    7e30:	e0 c3 00 91	add	x0, sp, #0x30
    7e34:	e4 0b 01 a9	stp	x4, x2, [sp, #0x10]
    7e38:	e2 03 03 aa	mov	x2, x3
    7e3c:	04 00 80 52	mov	w4, #0x0
    7e40:	e3 07 00 f9	str	x3, [sp, #0x8]
    7e44:	08 40 60 1e	fmov	d8, d0
    7e48:	00 00 00 94	bl	_process.Stack.alloc__anon_9146
    7e4c:	e0 07 42 a9	ldp	x0, x1, [sp, #0x20]
    7e50:	e4 0b 41 a9	ldp	x4, x2, [sp, #0x10]
    7e54:	e3 07 40 f9	ldr	x3, [sp, #0x8]
    7e58:	e8 1b 40 f9	ldr	x8, [sp, #0x30]
    7e5c:	08 05 00 fd	str	d8, [x8, #0x8]
    7e60:	ca ff ff 17	b	0x7d88
    7e64:	08 00 00 90	adrp	x8, _object.inMemory.nanMemObject@PAGE
    7e68:	08 01 00 91	add	x8, x8, _object.inMemory.nanMemObject@PAGEOFF
    7e6c:	c7 ff ff 17	b	0x7d88
