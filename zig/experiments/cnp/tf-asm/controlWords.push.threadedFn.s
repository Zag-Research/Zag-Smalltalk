; symbol: _controlWords.push.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.push.threadedFn:
    5be8:	ff 83 01 d1	sub	sp, sp, #0x60
    5bec:	f8 5f 02 a9	stp	x24, x23, [sp, #0x20]
    5bf0:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    5bf4:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    5bf8:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    5bfc:	fd 43 01 91	add	x29, sp, #0x50
    5c00:	f5 03 04 aa	mov	x21, x4
    5c04:	f6 03 03 aa	mov	x22, x3
    5c08:	f3 03 02 aa	mov	x19, x2
    5c0c:	f7 03 01 aa	mov	x23, x1
    5c10:	f4 03 00 aa	mov	x20, x0
    5c14:	08 00 40 f9	ldr	x8, [x0]
    5c18:	68 01 78 37	tbnz	w8, #0xf, 0x5c44
    5c1c:	a9 fe 70 d3	lsr	x9, x21, #48
    5c20:	09 02 00 b4	cbz	x9, 0x5c60
    5c24:	ea ce 74 92	and	x10, x23, #0xfffffffffffff000
    5c28:	29 01 0a aa	orr	x9, x9, x10
    5c2c:	a9 01 00 b4	cbz	x9, 0x5c60
    5c30:	0a fd 50 d3	lsr	x10, x8, #16
    5c34:	20 0d 2a cb	sub	x0, x9, w10, uxtb #3
    5c38:	18 fd 58 d3	lsr	x24, x8, #24
    5c3c:	d8 01 00 b5	cbnz	x24, 0x5c74
    5c40:	14 00 00 14	b	0x5c90
    5c44:	a9 fe 70 d3	lsr	x9, x21, #48
    5c48:	c9 00 00 b4	cbz	x9, 0x5c60
    5c4c:	e1 22 00 d1	sub	x1, x23, #0x8
    5c50:	3f 20 7d f2	tst	x1, #0xff8
    5c54:	00 05 00 54	b.eq	0x5cf4
    5c58:	3f 00 00 f9	str	xzr, [x1]
    5c5c:	12 00 00 14	b	0x5ca4
    5c60:	a9 be 40 92	and	x9, x21, #0xffffffffffff
    5c64:	0a 39 48 d3	ubfx	x10, x8, #8, #7
    5c68:	20 0d 0a 8b	add	x0, x9, x10, lsl #3
    5c6c:	18 fd 58 d3	lsr	x24, x8, #24
    5c70:	18 01 00 b4	cbz	x24, 0x5c90
    5c74:	08 27 40 92	and	x8, x24, #0x3ff
    5c78:	00 78 68 f8	ldr	x0, [x0, x8, lsl #3]
    5c7c:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12019
    5c80:	08 ff 4a d3	lsr	x8, x24, #10
    5c84:	1f ff 0f f1	cmp	x24, #0x3ff
    5c88:	f8 03 08 aa	mov	x24, x8
    5c8c:	48 ff ff 54	b.hi	0x5c74
    5c90:	18 00 40 f9	ldr	x24, [x0]
    5c94:	e1 22 00 d1	sub	x1, x23, #0x8
    5c98:	3f 20 7d f2	tst	x1, #0xff8
    5c9c:	a0 01 00 54	b.eq	0x5cd0
    5ca0:	38 00 00 f9	str	x24, [x1]
    5ca4:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5ca8:	80 82 00 91	add	x0, x20, #0x20
    5cac:	e2 03 13 aa	mov	x2, x19
    5cb0:	e3 03 16 aa	mov	x3, x22
    5cb4:	e4 03 15 aa	mov	x4, x21
    5cb8:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    5cbc:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    5cc0:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    5cc4:	f8 5f 42 a9	ldp	x24, x23, [sp, #0x20]
    5cc8:	ff 83 01 91	add	sp, sp, #0x60
    5ccc:	a0 00 1f d6	br	x5
    5cd0:	e0 23 00 91	add	x0, sp, #0x8
    5cd4:	e1 03 17 aa	mov	x1, x23
    5cd8:	e2 03 16 aa	mov	x2, x22
    5cdc:	e3 03 15 aa	mov	x3, x21
    5ce0:	00 00 00 94	bl	_process.Stack.spillStack
    5ce4:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    5ce8:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    5cec:	38 8c 1f f8	str	x24, [x1, #-0x8]!
    5cf0:	09 00 00 14	b	0x5d14
    5cf4:	e0 23 00 91	add	x0, sp, #0x8
    5cf8:	e1 03 17 aa	mov	x1, x23
    5cfc:	e2 03 16 aa	mov	x2, x22
    5d00:	e3 03 15 aa	mov	x3, x21
    5d04:	00 00 00 94	bl	_process.Stack.spillStack
    5d08:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    5d0c:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    5d10:	3f 8c 1f f8	str	xzr, [x1, #-0x8]!
    5d14:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5d18:	80 82 00 91	add	x0, x20, #0x20
    5d1c:	e2 03 13 aa	mov	x2, x19
    5d20:	e6 ff ff 17	b	0x5cb8
