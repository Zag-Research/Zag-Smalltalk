; symbol: _primitives.BlockClosure.threadedFns.createClosure.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.createClosure.threadedFn:
    8ab4:	ff c3 01 d1	sub	sp, sp, #0x70
    8ab8:	f6 57 04 a9	stp	x22, x21, [sp, #0x40]
    8abc:	f4 4f 05 a9	stp	x20, x19, [sp, #0x50]
    8ac0:	fd 7b 06 a9	stp	x29, x30, [sp, #0x60]
    8ac4:	fd 83 01 91	add	x29, sp, #0x60
    8ac8:	e8 03 03 aa	mov	x8, x3
    8acc:	f3 03 02 aa	mov	x19, x2
    8ad0:	e2 03 01 aa	mov	x2, x1
    8ad4:	f4 03 00 aa	mov	x20, x0
    8ad8:	35 00 80 52	mov	w21, #0x1
    8adc:	d6 03 80 52	mov	w22, #0x1e
    8ae0:	76 00 a0 72	movk	w22, #0x3, lsl #16
    8ae4:	07 00 00 14	b	0x8b00
    8ae8:	e0 c3 00 91	add	x0, sp, #0x30
    8aec:	e1 03 08 aa	mov	x1, x8
    8af0:	00 00 00 94	bl	_context.push
    8af4:	e2 23 43 a9	ldp	x2, x8, [sp, #0x30]
    8af8:	09 e1 00 91	add	x9, x8, #0x38
    8afc:	24 b1 7d 92	and	x4, x9, #0xfffffffffff8
    8b00:	89 fc 70 d3	lsr	x9, x4, #48
    8b04:	69 00 00 b4	cbz	x9, 0x8b10
    8b08:	83 bc 40 92	and	x3, x4, #0xffffffffffff
    8b0c:	e3 fe ff b5	cbnz	x3, 0x8ae8
    8b10:	89 02 40 f9	ldr	x9, [x20]
    8b14:	2a dd 68 d3	ubfx	x10, x9, #40, #16
    8b18:	5f 45 00 71	cmp	w10, #0x11
    8b1c:	aa 22 ca 1a	lsl	w10, w21, w10
    8b20:	4a 01 16 0a	and	w10, w10, w22
    8b24:	44 99 40 7a	ccmp	w10, #0x0, #0x4, ls
    8b28:	80 01 00 54	b.eq	0x8b58
    8b2c:	41 20 00 d1	sub	x1, x2, #0x8
    8b30:	3f 20 7d f2	tst	x1, #0xff8
    8b34:	e1 02 00 54	b.ne	0x8b90
    8b38:	e0 03 00 91	mov	x0, sp
    8b3c:	e1 03 02 aa	mov	x1, x2
    8b40:	e2 03 08 aa	mov	x2, x8
    8b44:	e3 03 04 aa	mov	x3, x4
    8b48:	00 00 00 94	bl	_process.Stack.spillStack
    8b4c:	e2 23 40 a9	ldp	x2, x8, [sp]
    8b50:	e4 0b 40 f9	ldr	x4, [sp, #0x10]
    8b54:	eb ff ff 17	b	0x8b00
    8b58:	09 01 02 ca	eor	x9, x8, x2
    8b5c:	3f fd 3f f1	cmp	x9, #0xfff
    8b60:	69 03 00 54	b.ls	0x8bcc
    8b64:	49 e0 3f 11	add	w9, w2, #0xff8
    8b68:	3f 21 7d f2	tst	x9, #0xff8
    8b6c:	c1 03 00 54	b.ne	0x8be4
    8b70:	e0 63 00 91	add	x0, sp, #0x18
    8b74:	e1 03 02 aa	mov	x1, x2
    8b78:	e2 03 08 aa	mov	x2, x8
    8b7c:	e3 03 04 aa	mov	x3, x4
    8b80:	00 00 00 94	bl	_process.Stack.spillStack
    8b84:	e2 a3 41 a9	ldp	x2, x8, [sp, #0x18]
    8b88:	e4 17 40 f9	ldr	x4, [sp, #0x28]
    8b8c:	dd ff ff 17	b	0x8b00
    8b90:	2a fd 68 d3	lsr	x10, x9, #40
    8b94:	09 e1 c9 93	extr	x9, x8, x9, #0x38
    8b98:	29 dd 78 d3	lsl	x9, x9, #8
    8b9c:	49 11 7d b3	bfi	x9, x10, #3, #5
    8ba0:	29 01 40 b2	orr	x9, x9, #0x1
    8ba4:	29 00 00 f9	str	x9, [x1]
    8ba8:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    8bac:	80 82 00 91	add	x0, x20, #0x20
    8bb0:	e2 03 13 aa	mov	x2, x19
    8bb4:	e3 03 08 aa	mov	x3, x8
    8bb8:	fd 7b 46 a9	ldp	x29, x30, [sp, #0x60]
    8bbc:	f4 4f 45 a9	ldp	x20, x19, [sp, #0x50]
    8bc0:	f6 57 44 a9	ldp	x22, x21, [sp, #0x40]
    8bc4:	ff c3 01 91	add	sp, sp, #0x70
    8bc8:	a0 00 1f d6	br	x5
    8bcc:	00 00 00 90	adrp	x0, ___anon_14394@PAGE
    8bd0:	00 00 00 91	add	x0, x0, ___anon_14394@PAGEOFF
    8bd4:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8bd8:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8bdc:	c1 04 80 52	mov	w1, #0x26
    8be0:	00 00 00 94	bl	_debug.defaultPanic
    8be4:	00 00 00 90	adrp	x0, ___anon_14418@PAGE
    8be8:	00 00 00 91	add	x0, x0, ___anon_14418@PAGEOFF
    8bec:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    8bf0:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    8bf4:	a1 04 80 52	mov	w1, #0x25
    8bf8:	00 00 00 94	bl	_debug.defaultPanic
