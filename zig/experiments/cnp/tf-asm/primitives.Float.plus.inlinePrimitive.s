; symbol: _primitives.Float.+.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.Float.+.inlinePrimitive:
    7ab0:	28 04 40 f9	ldr	x8, [x1, #0x8]
    7ab4:	1f 05 7f f2	tst	x8, #0x6
    7ab8:	60 03 00 54	b.eq	0x7b24
    7abc:	08 09 00 d1	sub	x8, x8, #0x2
    7ac0:	08 11 c8 93	ror	x8, x8, #0x4
    7ac4:	00 01 67 9e	fmov	d0, x8
    7ac8:	28 00 40 f9	ldr	x8, [x1]
    7acc:	1f 05 7f f2	tst	x8, #0x6
    7ad0:	00 04 00 54	b.eq	0x7b50
    7ad4:	08 09 00 d1	sub	x8, x8, #0x2
    7ad8:	08 11 c8 93	ror	x8, x8, #0x4
    7adc:	01 01 67 9e	fmov	d1, x8
    7ae0:	ff 03 02 d1	sub	sp, sp, #0x80
    7ae4:	e9 23 06 6d	stp	d9, d8, [sp, #0x60]
    7ae8:	fd 7b 07 a9	stp	x29, x30, [sp, #0x70]
    7aec:	fd c3 01 91	add	x29, sp, #0x70
    7af0:	00 28 61 1e	fadd	d0, d0, d1
    7af4:	08 00 66 9e	fmov	x8, d0
    7af8:	08 f1 c8 93	ror	x8, x8, #0x3c
    7afc:	08 09 00 91	add	x8, x8, #0x2
    7b00:	1f 05 7f f2	tst	x8, #0x6
    7b04:	80 03 00 54	b.eq	0x7b74
    7b08:	28 8c 00 f8	str	x8, [x1, #0x8]!
    7b0c:	05 10 40 f9	ldr	x5, [x0, #0x20]
    7b10:	00 c0 00 91	add	x0, x0, #0x30
    7b14:	fd 7b 47 a9	ldp	x29, x30, [sp, #0x70]
    7b18:	e9 23 46 6d	ldp	d9, d8, [sp, #0x60]
    7b1c:	ff 03 02 91	add	sp, sp, #0x80
    7b20:	a0 00 1f d6	br	x5
    7b24:	09 01 40 92	and	x9, x8, #0x1
    7b28:	1f 01 00 f1	cmp	x8, #0x0
    7b2c:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    7b30:	e1 01 00 54	b.ne	0x7b6c
    7b34:	09 01 40 79	ldrh	w9, [x8]
    7b38:	3f 85 00 f1	cmp	x9, #0x21
    7b3c:	81 01 00 54	b.ne	0x7b6c
    7b40:	00 05 40 fd	ldr	d0, [x8, #0x8]
    7b44:	28 00 40 f9	ldr	x8, [x1]
    7b48:	1f 05 7f f2	tst	x8, #0x6
    7b4c:	41 fc ff 54	b.ne	0x7ad4
    7b50:	e8 00 00 b4	cbz	x8, 0x7b6c
    7b54:	c8 00 00 37	tbnz	w8, #0x0, 0x7b6c
    7b58:	09 01 40 79	ldrh	w9, [x8]
    7b5c:	3f 85 00 f1	cmp	x9, #0x21
    7b60:	61 00 00 54	b.ne	0x7b6c
    7b64:	01 05 40 fd	ldr	d1, [x8, #0x8]
    7b68:	de ff ff 17	b	0x7ae0
    7b6c:	05 04 41 f8	ldr	x5, [x0], #0x10
    7b70:	a0 00 1f d6	br	x5
    7b74:	00 20 60 1e	fcmp	d0, d0
    7b78:	66 03 00 54	b.vs	0x7be4
    7b7c:	08 00 66 9e	fmov	x8, d0
    7b80:	08 f9 40 92	and	x8, x8, #0x7fffffffffffffff
    7b84:	09 fe ef d2	mov	x9, #0x7ff0000000000000
    7b88:	1f 01 09 eb	cmp	x8, x9
    7b8c:	01 01 00 54	b.ne	0x7bac
    7b90:	08 00 00 90	adrp	x8, _object.inMemory.pInfMemObject@PAGE
    7b94:	08 01 00 91	add	x8, x8, _object.inMemory.pInfMemObject@PAGEOFF
    7b98:	08 20 60 1e	fcmp	d0, #0.0
    7b9c:	09 00 00 90	adrp	x9, _object.inMemory.nInfMemObject@PAGE
    7ba0:	29 01 00 91	add	x9, x9, _object.inMemory.nInfMemObject@PAGEOFF
    7ba4:	08 c1 89 9a	csel	x8, x8, x9, gt
    7ba8:	d8 ff ff 17	b	0x7b08
    7bac:	e0 07 02 a9	stp	x0, x1, [sp, #0x20]
    7bb0:	e0 c3 00 91	add	x0, sp, #0x30
    7bb4:	e4 0b 01 a9	stp	x4, x2, [sp, #0x10]
    7bb8:	e2 03 03 aa	mov	x2, x3
    7bbc:	04 00 80 52	mov	w4, #0x0
    7bc0:	e3 07 00 f9	str	x3, [sp, #0x8]
    7bc4:	08 40 60 1e	fmov	d8, d0
    7bc8:	00 00 00 94	bl	_process.Stack.alloc__anon_9146
    7bcc:	e0 07 42 a9	ldp	x0, x1, [sp, #0x20]
    7bd0:	e4 0b 41 a9	ldp	x4, x2, [sp, #0x10]
    7bd4:	e3 07 40 f9	ldr	x3, [sp, #0x8]
    7bd8:	e8 1b 40 f9	ldr	x8, [sp, #0x30]
    7bdc:	08 05 00 fd	str	d8, [x8, #0x8]
    7be0:	ca ff ff 17	b	0x7b08
    7be4:	08 00 00 90	adrp	x8, _object.inMemory.nanMemObject@PAGE
    7be8:	08 01 00 91	add	x8, x8, _object.inMemory.nanMemObject@PAGEOFF
    7bec:	c7 ff ff 17	b	0x7b08
