; symbol: _primitives.BlockClosure.threadedFns.value.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.value.threadedFn:
    8bfc:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    8c00:	fd 03 00 91	mov	x29, sp
    8c04:	28 00 40 f9	ldr	x8, [x1]
    8c08:	09 09 00 12	and	w9, w8, #0x7
    8c0c:	3f 05 00 71	cmp	w9, #0x1
    8c10:	40 01 00 54	b.eq	0x8c38
    8c14:	3f 01 00 71	cmp	w9, #0x0
    8c18:	04 09 40 fa	ccmp	x8, #0x0, #0x4, eq
    8c1c:	61 01 00 54	b.ne	0x8c48
    8c20:	00 00 00 90	adrp	x0, ___anon_9381@PAGE
    8c24:	00 00 00 91	add	x0, x0, ___anon_9381@PAGEOFF
    8c28:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8c2c:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8c30:	61 02 80 52	mov	w1, #0x13
    8c34:	00 00 00 94	bl	_debug.defaultPanic
    8c38:	09 1d 03 53	ubfx	w9, w8, #3, #5
    8c3c:	3f 0d 00 71	cmp	w9, #0x3
    8c40:	a0 00 00 54	b.eq	0x8c54
    8c44:	f7 ff ff 17	b	0x8c20
    8c48:	09 01 40 79	ldrh	w9, [x8]
    8c4c:	3f 0d 00 71	cmp	w9, #0x3
    8c50:	81 fe ff 54	b.ne	0x8c20
    8c54:	1f 41 40 f1	cmp	x8, #0x10, lsl #12
    8c58:	23 03 00 54	b.lo	0x8cbc
    8c5c:	09 fd 50 d3	lsr	x9, x8, #16
    8c60:	2a 01 01 ca	eor	x10, x9, x1
    8c64:	5f fd 3f f1	cmp	x10, #0xfff
    8c68:	a8 02 00 54	b.hi	0x8cbc
    8c6c:	23 05 40 f9	ldr	x3, [x9, #0x8]
    8c70:	23 03 00 b4	cbz	x3, 0x8cd4
    8c74:	2a 01 40 f9	ldr	x10, [x9]
    8c78:	4a 21 7d 92	and	x10, x10, #0xff8
    8c7c:	29 0d 0a 8b	add	x9, x9, x10, lsl #3
    8c80:	21 01 01 91	add	x1, x9, #0x40
    8c84:	29 00 03 ca	eor	x9, x1, x3
    8c88:	3f fd 3f f1	cmp	x9, #0xfff
    8c8c:	69 00 00 54	b.ls	0x8c98
    8c90:	69 18 40 f9	ldr	x9, [x3, #0x30]
    8c94:	02 00 00 14	b	0x8c9c
    8c98:	69 e0 00 91	add	x9, x3, #0x38
    8c9c:	08 3d 40 93	sxth	x8, w8
    8ca0:	2a 15 80 d2	mov	x10, #0xa9
    8ca4:	48 1d 40 b3	bfxil	x8, x10, #0, #8
    8ca8:	28 00 00 f9	str	x8, [x1]
    8cac:	60 14 42 a9	ldp	x0, x5, [x3, #0x20]
    8cb0:	24 b1 7d 92	and	x4, x9, #0xfffffffffff8
    8cb4:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    8cb8:	a0 00 1f d6	br	x5
    8cbc:	00 00 00 90	adrp	x0, ___anon_8291@PAGE
    8cc0:	00 00 00 91	add	x0, x0, ___anon_8291@PAGEOFF
    8cc4:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8cc8:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8ccc:	41 01 80 52	mov	w1, #0xa
    8cd0:	00 00 00 94	bl	_debug.defaultPanic
    8cd4:	00 00 00 90	adrp	x0, ___anon_8284@PAGE
    8cd8:	00 00 00 91	add	x0, x0, ___anon_8284@PAGEOFF
    8cdc:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8ce0:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8ce4:	c1 00 80 52	mov	w1, #0x6
    8ce8:	00 00 00 94	bl	_debug.defaultPanic
