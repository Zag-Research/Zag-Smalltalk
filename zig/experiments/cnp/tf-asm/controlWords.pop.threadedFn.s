; symbol: _controlWords.pop.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.pop.threadedFn:
    5a6c:	ff 83 01 d1	sub	sp, sp, #0x60
    5a70:	fa 67 01 a9	stp	x26, x25, [sp, #0x10]
    5a74:	f8 5f 02 a9	stp	x24, x23, [sp, #0x20]
    5a78:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    5a7c:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    5a80:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    5a84:	fd 43 01 91	add	x29, sp, #0x50
    5a88:	f3 03 02 aa	mov	x19, x2
    5a8c:	f4 03 00 aa	mov	x20, x0
    5a90:	19 00 40 f9	ldr	x25, [x0]
    5a94:	f5 03 01 aa	mov	x21, x1
    5a98:	b8 86 40 f8	ldr	x24, [x21], #0x8
    5a9c:	88 bc 40 92	and	x8, x4, #0xffffffffffff
    5aa0:	89 fc 70 d3	lsr	x9, x4, #48
    5aa4:	a9 03 00 b4	cbz	x9, 0x5b18
    5aa8:	88 03 00 b4	cbz	x8, 0x5b18
    5aac:	e0 03 00 91	mov	x0, sp
    5ab0:	e1 03 03 aa	mov	x1, x3
    5ab4:	e2 03 15 aa	mov	x2, x21
    5ab8:	e3 03 08 aa	mov	x3, x8
    5abc:	00 00 00 94	bl	_context.push
    5ac0:	f5 5b 40 a9	ldp	x21, x22, [sp]
    5ac4:	c8 e2 00 91	add	x8, x22, #0x38
    5ac8:	17 b1 7d 92	and	x23, x8, #0xfffffffffff8
    5acc:	28 3b 48 d3	ubfx	x8, x25, #8, #7
    5ad0:	e0 0e 08 8b	add	x0, x23, x8, lsl #3
    5ad4:	39 ff 58 d3	lsr	x25, x25, #24
    5ad8:	19 01 00 b4	cbz	x25, 0x5af8
    5adc:	28 27 40 92	and	x8, x25, #0x3ff
    5ae0:	00 78 68 f8	ldr	x0, [x0, x8, lsl #3]
    5ae4:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12019
    5ae8:	28 ff 4a d3	lsr	x8, x25, #10
    5aec:	3f ff 0f f1	cmp	x25, #0x3ff
    5af0:	f9 03 08 aa	mov	x25, x8
    5af4:	48 ff ff 54	b.hi	0x5adc
    5af8:	18 00 00 f9	str	x24, [x0]
    5afc:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5b00:	80 82 00 91	add	x0, x20, #0x20
    5b04:	e1 03 15 aa	mov	x1, x21
    5b08:	e2 03 13 aa	mov	x2, x19
    5b0c:	e3 03 16 aa	mov	x3, x22
    5b10:	e4 03 17 aa	mov	x4, x23
    5b14:	20 00 00 14	b	0x5b94
    5b18:	69 01 00 b4	cbz	x9, 0x5b44
    5b1c:	2a cc 74 92	and	x10, x1, #0xfffffffffffff000
    5b20:	29 01 0a aa	orr	x9, x9, x10
    5b24:	09 01 00 b4	cbz	x9, 0x5b44
    5b28:	f7 03 03 aa	mov	x23, x3
    5b2c:	f6 03 04 aa	mov	x22, x4
    5b30:	28 ff 50 d3	lsr	x8, x25, #16
    5b34:	20 0d 28 cb	sub	x0, x9, w8, uxtb #3
    5b38:	39 ff 58 d3	lsr	x25, x25, #24
    5b3c:	19 01 00 b5	cbnz	x25, 0x5b5c
    5b40:	0e 00 00 14	b	0x5b78
    5b44:	f7 03 03 aa	mov	x23, x3
    5b48:	f6 03 04 aa	mov	x22, x4
    5b4c:	29 3b 48 d3	ubfx	x9, x25, #8, #7
    5b50:	00 0d 09 8b	add	x0, x8, x9, lsl #3
    5b54:	39 ff 58 d3	lsr	x25, x25, #24
    5b58:	19 01 00 b4	cbz	x25, 0x5b78
    5b5c:	28 27 40 92	and	x8, x25, #0x3ff
    5b60:	00 78 68 f8	ldr	x0, [x0, x8, lsl #3]
    5b64:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12019
    5b68:	28 ff 4a d3	lsr	x8, x25, #10
    5b6c:	3f ff 0f f1	cmp	x25, #0x3ff
    5b70:	f9 03 08 aa	mov	x25, x8
    5b74:	48 ff ff 54	b.hi	0x5b5c
    5b78:	18 00 00 f9	str	x24, [x0]
    5b7c:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5b80:	80 82 00 91	add	x0, x20, #0x20
    5b84:	e1 03 15 aa	mov	x1, x21
    5b88:	e2 03 13 aa	mov	x2, x19
    5b8c:	e3 03 17 aa	mov	x3, x23
    5b90:	e4 03 16 aa	mov	x4, x22
    5b94:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    5b98:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    5b9c:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    5ba0:	f8 5f 42 a9	ldp	x24, x23, [sp, #0x20]
    5ba4:	fa 67 41 a9	ldp	x26, x25, [sp, #0x10]
    5ba8:	ff 83 01 91	add	sp, sp, #0x60
    5bac:	a0 00 1f d6	br	x5
