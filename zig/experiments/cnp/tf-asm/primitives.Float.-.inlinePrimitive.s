; symbol: _primitives.Float.-.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.Float.-.inlinePrimitive:
    7bf0:	28 04 40 f9	ldr	x8, [x1, #0x8]
    7bf4:	1f 05 7f f2	tst	x8, #0x6
    7bf8:	60 03 00 54	b.eq	0x7c64
    7bfc:	08 09 00 d1	sub	x8, x8, #0x2
    7c00:	08 11 c8 93	ror	x8, x8, #0x4
    7c04:	00 01 67 9e	fmov	d0, x8
    7c08:	28 00 40 f9	ldr	x8, [x1]
    7c0c:	1f 05 7f f2	tst	x8, #0x6
    7c10:	00 04 00 54	b.eq	0x7c90
    7c14:	08 09 00 d1	sub	x8, x8, #0x2
    7c18:	08 11 c8 93	ror	x8, x8, #0x4
    7c1c:	01 01 67 9e	fmov	d1, x8
    7c20:	ff 03 02 d1	sub	sp, sp, #0x80
    7c24:	e9 23 06 6d	stp	d9, d8, [sp, #0x60]
    7c28:	fd 7b 07 a9	stp	x29, x30, [sp, #0x70]
    7c2c:	fd c3 01 91	add	x29, sp, #0x70
    7c30:	00 38 61 1e	fsub	d0, d0, d1
    7c34:	08 00 66 9e	fmov	x8, d0
    7c38:	08 f1 c8 93	ror	x8, x8, #0x3c
    7c3c:	08 09 00 91	add	x8, x8, #0x2
    7c40:	1f 05 7f f2	tst	x8, #0x6
    7c44:	80 03 00 54	b.eq	0x7cb4
    7c48:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7c4c:	05 10 40 f9	ldr	x5, [x0, #0x20]
    7c50:	00 c0 00 91	add	x0, x0, #0x30
    7c54:	fd 7b 47 a9	ldp	x29, x30, [sp, #0x70]
    7c58:	e9 23 46 6d	ldp	d9, d8, [sp, #0x60]
    7c5c:	ff 03 02 91	add	sp, sp, #0x80
    7c60:	a0 00 1f d6	br	x5
    7c64:	09 01 40 92	and	x9, x8, #0x1
    7c68:	1f 01 00 f1	cmp	x8, #0x0
    7c6c:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    7c70:	e1 01 00 54	b.ne	0x7cac
    7c74:	09 01 40 79	ldrh	w9, [x8]
    7c78:	3f 85 00 f1	cmp	x9, #0x21
    7c7c:	81 01 00 54	b.ne	0x7cac
    7c80:	00 05 40 fd	ldr	d0, [x8, #0x8]
    7c84:	28 00 40 f9	ldr	x8, [x1]
    7c88:	1f 05 7f f2	tst	x8, #0x6
    7c8c:	41 fc ff 54	b.ne	0x7c14
    7c90:	e8 00 00 b4	cbz	x8, 0x7cac
    7c94:	c8 00 00 37	tbnz	w8, #0x0, 0x7cac
    7c98:	09 01 40 79	ldrh	w9, [x8]
    7c9c:	3f 85 00 f1	cmp	x9, #0x21
    7ca0:	61 00 00 54	b.ne	0x7cac
    7ca4:	01 05 40 fd	ldr	d1, [x8, #0x8]
    7ca8:	de ff ff 17	b	0x7c20
    7cac:	05 04 41 f8	ldr	x5, [x0], #0x10
    7cb0:	a0 00 1f d6	br	x5
    7cb4:	00 20 60 1e	fcmp	d0, d0
    7cb8:	66 03 00 54	b.vs	0x7d24
    7cbc:	08 00 66 9e	fmov	x8, d0
    7cc0:	08 f9 40 92	and	x8, x8, #0x7fffffffffffffff
    7cc4:	09 fe ef d2	mov	x9, #0x7ff0000000000000
    7cc8:	1f 01 09 eb	cmp	x8, x9
    7ccc:	01 01 00 54	b.ne	0x7cec
    7cd0:	08 00 00 90	adrp	x8, _object.inMemory.pInfMemObject@PAGE
    7cd4:	08 01 00 91	add	x8, x8, _object.inMemory.pInfMemObject@PAGEOFF
    7cd8:	08 20 60 1e	fcmp	d0, #0.0
    7cdc:	09 00 00 90	adrp	x9, _object.inMemory.nInfMemObject@PAGE
    7ce0:	29 01 00 91	add	x9, x9, _object.inMemory.nInfMemObject@PAGEOFF
    7ce4:	08 c1 89 9a	csel	x8, x8, x9, gt
    7ce8:	d8 ff ff 17	b	0x7c48
    7cec:	e0 07 02 a9	stp	x0, x1, [sp, #0x20]
    7cf0:	e0 c3 00 91	add	x0, sp, #0x30
    7cf4:	e4 0b 01 a9	stp	x4, x2, [sp, #0x10]
    7cf8:	e2 03 03 aa	mov	x2, x3
    7cfc:	04 00 80 52	mov	w4, #0x0
    7d00:	e3 07 00 f9	str	x3, [sp, #0x8]
    7d04:	08 40 60 1e	fmov	d8, d0
    7d08:	00 00 00 94	bl	_process.Stack.alloc__anon_9146
    7d0c:	e0 07 42 a9	ldp	x0, x1, [sp, #0x20]
    7d10:	e4 0b 41 a9	ldp	x4, x2, [sp, #0x10]
    7d14:	e3 07 40 f9	ldr	x3, [sp, #0x8]
    7d18:	e8 1b 40 f9	ldr	x8, [sp, #0x30]
    7d1c:	08 05 00 fd	str	d8, [x8, #0x8]
    7d20:	ca ff ff 17	b	0x7c48
    7d24:	08 00 00 90	adrp	x8, _object.inMemory.nanMemObject@PAGE
    7d28:	08 01 00 91	add	x8, x8, _object.inMemory.nanMemObject@PAGEOFF
    7d2c:	c7 ff ff 17	b	0x7c48
