; symbol: _primitives.threadedFunctions.primitiveModule.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.primitiveModule.threadedFn:
    6d28:	ff 43 03 d1	sub	sp, sp, #0xd0
    6d2c:	fa 67 08 a9	stp	x26, x25, [sp, #0x80]
    6d30:	f8 5f 09 a9	stp	x24, x23, [sp, #0x90]
    6d34:	f6 57 0a a9	stp	x22, x21, [sp, #0xa0]
    6d38:	f4 4f 0b a9	stp	x20, x19, [sp, #0xb0]
    6d3c:	fd 7b 0c a9	stp	x29, x30, [sp, #0xc0]
    6d40:	fd 03 03 91	add	x29, sp, #0xc0
    6d44:	f3 03 04 aa	mov	x19, x4
    6d48:	f5 03 00 aa	mov	x21, x0
    6d4c:	44 03 f8 b7	tbnz	x4, #0x3f, 0x6db4
    6d50:	f7 03 01 aa	mov	x23, x1
    6d54:	f6 03 02 aa	mov	x22, x2
    6d58:	f4 03 03 aa	mov	x20, x3
    6d5c:	a1 02 40 f9	ldr	x1, [x21]
    6d60:	e0 23 00 91	add	x0, sp, #0x8
    6d64:	00 00 00 94	bl	_object.ObjectFunctions.arrayAsSlice__anon_8605
    6d68:	e8 33 40 79	ldrh	w8, [sp, #0x18]
    6d6c:	f8 07 40 f9	ldr	x24, [sp, #0x8]
    6d70:	1f 01 00 71	cmp	w8, #0x0
    6d74:	04 0b 40 fa	ccmp	x24, #0x0, #0x4, eq
    6d78:	21 03 00 54	b.ne	0x6ddc
    6d7c:	68 be 40 92	and	x8, x19, #0xffffffffffff
    6d80:	69 fe 70 d3	lsr	x9, x19, #48
    6d84:	3f 01 00 f1	cmp	x9, #0x0
    6d88:	08 11 9f 9a	csel	x8, x8, xzr, ne
    6d8c:	09 00 00 90	adrp	x9, "_primitives.Object.basicAt:.primitive"@PAGE
    6d90:	29 01 00 91	add	x9, x9, "_primitives.Object.basicAt:.primitive"@PAGEOFF
    6d94:	09 a5 01 a9	stp	x9, x9, [x8, #0x18]
    6d98:	a5 02 5f f8	ldur	x5, [x21, #-0x10]
    6d9c:	64 02 41 b2	orr	x4, x19, #0x8000000000000000
    6da0:	e0 03 15 aa	mov	x0, x21
    6da4:	e1 03 17 aa	mov	x1, x23
    6da8:	e2 03 16 aa	mov	x2, x22
    6dac:	e3 03 14 aa	mov	x3, x20
    6db0:	04 00 00 14	b	0x6dc0
    6db4:	a5 12 40 f9	ldr	x5, [x21, #0x20]
    6db8:	a0 c2 00 91	add	x0, x21, #0x30
    6dbc:	64 fa 40 92	and	x4, x19, #0x7fffffffffffffff
    6dc0:	fd 7b 4c a9	ldp	x29, x30, [sp, #0xc0]
    6dc4:	f4 4f 4b a9	ldp	x20, x19, [sp, #0xb0]
    6dc8:	f6 57 4a a9	ldp	x22, x21, [sp, #0xa0]
    6dcc:	f8 5f 49 a9	ldp	x24, x23, [sp, #0x90]
    6dd0:	fa 67 48 a9	ldp	x26, x25, [sp, #0x80]
    6dd4:	ff 43 03 91	add	sp, sp, #0xd0
    6dd8:	a0 00 1f d6	br	x5
    6ddc:	f9 0b 40 f9	ldr	x25, [sp, #0x10]
    6de0:	a1 0a 40 f9	ldr	x1, [x21, #0x10]
    6de4:	e0 83 00 91	add	x0, sp, #0x20
    6de8:	00 00 00 94	bl	_object.ObjectFunctions.arrayAsSlice__anon_8605
    6dec:	e8 63 40 79	ldrh	w8, [sp, #0x30]
    6df0:	68 fc ff 35	cbnz	w8, 0x6d7c
    6df4:	e1 13 40 f9	ldr	x1, [sp, #0x20]
    6df8:	21 fc ff b4	cbz	x1, 0x6d7c
    6dfc:	e2 17 40 f9	ldr	x2, [sp, #0x28]
    6e00:	e0 e3 00 91	add	x0, sp, #0x38
    6e04:	e3 03 18 aa	mov	x3, x24
    6e08:	e4 03 19 aa	mov	x4, x25
    6e0c:	00 00 00 94	bl	_primitives.Module.findNamedPrimitive
    6e10:	e8 e3 41 39	ldrb	w8, [sp, #0x78]
    6e14:	48 fb ff 34	cbz	w8, 0x6d7c
    6e18:	e5 27 40 f9	ldr	x5, [sp, #0x48]
    6e1c:	05 fb ff b4	cbz	x5, 0x6d7c
    6e20:	68 be 40 92	and	x8, x19, #0xffffffffffff
    6e24:	69 fe 70 d3	lsr	x9, x19, #48
    6e28:	3f 01 00 f1	cmp	x9, #0x0
    6e2c:	08 11 9f 9a	csel	x8, x8, xzr, ne
    6e30:	05 95 01 a9	stp	x5, x5, [x8, #0x18]
    6e34:	e0 03 15 aa	mov	x0, x21
    6e38:	e1 03 17 aa	mov	x1, x23
    6e3c:	e2 03 16 aa	mov	x2, x22
    6e40:	e3 03 14 aa	mov	x3, x20
    6e44:	e4 03 13 aa	mov	x4, x19
    6e48:	de ff ff 17	b	0x6dc0
