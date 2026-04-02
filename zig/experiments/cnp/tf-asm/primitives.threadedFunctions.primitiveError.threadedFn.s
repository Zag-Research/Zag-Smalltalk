; symbol: _primitives.threadedFunctions.primitiveError.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.primitiveError.threadedFn:
    6abc:	ff 43 02 d1	sub	sp, sp, #0x90
    6ac0:	f8 5f 05 a9	stp	x24, x23, [sp, #0x50]
    6ac4:	f6 57 06 a9	stp	x22, x21, [sp, #0x60]
    6ac8:	f4 4f 07 a9	stp	x20, x19, [sp, #0x70]
    6acc:	fd 7b 08 a9	stp	x29, x30, [sp, #0x80]
    6ad0:	fd 03 02 91	add	x29, sp, #0x80
    6ad4:	f3 03 04 aa	mov	x19, x4
    6ad8:	f5 03 01 aa	mov	x21, x1
    6adc:	f4 03 00 aa	mov	x20, x0
    6ae0:	24 04 f8 b7	tbnz	x4, #0x3f, 0x6b64
    6ae4:	f7 03 02 aa	mov	x23, x2
    6ae8:	f6 03 03 aa	mov	x22, x3
    6aec:	81 1e 40 39	ldrb	w1, [x20, #0x7]
    6af0:	e0 23 00 91	add	x0, sp, #0x8
    6af4:	00 00 00 94	bl	_primitives.Module.findNumberedPrimitive
    6af8:	e8 23 41 39	ldrb	w8, [sp, #0x48]
    6afc:	c8 01 00 34	cbz	w8, 0x6b34
    6b00:	e5 13 40 f9	ldr	x5, [sp, #0x20]
    6b04:	85 01 00 b4	cbz	x5, 0x6b34
    6b08:	68 be 40 92	and	x8, x19, #0xffffffffffff
    6b0c:	69 fe 70 d3	lsr	x9, x19, #48
    6b10:	3f 01 00 f1	cmp	x9, #0x0
    6b14:	08 11 9f 9a	csel	x8, x8, xzr, ne
    6b18:	05 95 01 a9	stp	x5, x5, [x8, #0x18]
    6b1c:	e0 03 14 aa	mov	x0, x20
    6b20:	e1 03 15 aa	mov	x1, x21
    6b24:	e2 03 17 aa	mov	x2, x23
    6b28:	e3 03 16 aa	mov	x3, x22
    6b2c:	e4 03 13 aa	mov	x4, x19
    6b30:	18 00 00 14	b	0x6b90
    6b34:	68 be 40 92	and	x8, x19, #0xffffffffffff
    6b38:	69 fe 70 d3	lsr	x9, x19, #48
    6b3c:	3f 01 00 f1	cmp	x9, #0x0
    6b40:	08 11 9f 9a	csel	x8, x8, xzr, ne
    6b44:	09 00 00 90	adrp	x9, _primitives.noPrimWithError@PAGE
    6b48:	29 01 00 91	add	x9, x9, _primitives.noPrimWithError@PAGEOFF
    6b4c:	09 a5 01 a9	stp	x9, x9, [x8, #0x18]
    6b50:	a1 22 00 d1	sub	x1, x21, #0x8
    6b54:	3f 20 7d f2	tst	x1, #0xff8
    6b58:	00 01 00 54	b.eq	0x6b78
    6b5c:	3f 00 00 f9	str	xzr, [x1]
    6b60:	07 00 00 14	b	0x6b7c
    6b64:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    6b68:	80 82 00 91	add	x0, x20, #0x20
    6b6c:	64 fa 40 92	and	x4, x19, #0x7fffffffffffffff
    6b70:	e1 03 15 aa	mov	x1, x21
    6b74:	07 00 00 14	b	0x6b90
    6b78:	01 00 80 d2	mov	x1, #0x0
    6b7c:	85 02 5f f8	ldur	x5, [x20, #-0x10]
    6b80:	64 02 41 b2	orr	x4, x19, #0x8000000000000000
    6b84:	e0 03 14 aa	mov	x0, x20
    6b88:	e2 03 17 aa	mov	x2, x23
    6b8c:	e3 03 16 aa	mov	x3, x22
    6b90:	fd 7b 48 a9	ldp	x29, x30, [sp, #0x80]
    6b94:	f4 4f 47 a9	ldp	x20, x19, [sp, #0x70]
    6b98:	f6 57 46 a9	ldp	x22, x21, [sp, #0x60]
    6b9c:	f8 5f 45 a9	ldp	x24, x23, [sp, #0x50]
    6ba0:	ff 43 02 91	add	sp, sp, #0x90
    6ba4:	a0 00 1f d6	br	x5
