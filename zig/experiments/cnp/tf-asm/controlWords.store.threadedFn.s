; symbol: _controlWords.store.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.store.threadedFn:
    5e40:	ff 83 01 d1	sub	sp, sp, #0x60
    5e44:	fa 67 01 a9	stp	x26, x25, [sp, #0x10]
    5e48:	f8 5f 02 a9	stp	x24, x23, [sp, #0x20]
    5e4c:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    5e50:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    5e54:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    5e58:	fd 43 01 91	add	x29, sp, #0x50
    5e5c:	f3 03 02 aa	mov	x19, x2
    5e60:	f5 03 01 aa	mov	x21, x1
    5e64:	f4 03 00 aa	mov	x20, x0
    5e68:	19 00 40 f9	ldr	x25, [x0]
    5e6c:	38 00 40 f9	ldr	x24, [x1]
    5e70:	88 bc 40 92	and	x8, x4, #0xffffffffffff
    5e74:	89 fc 70 d3	lsr	x9, x4, #48
    5e78:	a9 03 00 b4	cbz	x9, 0x5eec
    5e7c:	88 03 00 b4	cbz	x8, 0x5eec
    5e80:	e0 03 00 91	mov	x0, sp
    5e84:	e1 03 03 aa	mov	x1, x3
    5e88:	e2 03 15 aa	mov	x2, x21
    5e8c:	e3 03 08 aa	mov	x3, x8
    5e90:	00 00 00 94	bl	_context.push
    5e94:	f5 5b 40 a9	ldp	x21, x22, [sp]
    5e98:	c8 e2 00 91	add	x8, x22, #0x38
    5e9c:	17 b1 7d 92	and	x23, x8, #0xfffffffffff8
    5ea0:	28 3b 48 d3	ubfx	x8, x25, #8, #7
    5ea4:	e0 0e 08 8b	add	x0, x23, x8, lsl #3
    5ea8:	39 ff 58 d3	lsr	x25, x25, #24
    5eac:	19 01 00 b4	cbz	x25, 0x5ecc
    5eb0:	28 27 40 92	and	x8, x25, #0x3ff
    5eb4:	00 78 68 f8	ldr	x0, [x0, x8, lsl #3]
    5eb8:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12019
    5ebc:	28 ff 4a d3	lsr	x8, x25, #10
    5ec0:	3f ff 0f f1	cmp	x25, #0x3ff
    5ec4:	f9 03 08 aa	mov	x25, x8
    5ec8:	48 ff ff 54	b.hi	0x5eb0
    5ecc:	18 00 00 f9	str	x24, [x0]
    5ed0:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5ed4:	80 82 00 91	add	x0, x20, #0x20
    5ed8:	e1 03 15 aa	mov	x1, x21
    5edc:	e2 03 13 aa	mov	x2, x19
    5ee0:	e3 03 16 aa	mov	x3, x22
    5ee4:	e4 03 17 aa	mov	x4, x23
    5ee8:	20 00 00 14	b	0x5f68
    5eec:	69 01 00 b4	cbz	x9, 0x5f18
    5ef0:	aa ce 74 92	and	x10, x21, #0xfffffffffffff000
    5ef4:	29 01 0a aa	orr	x9, x9, x10
    5ef8:	09 01 00 b4	cbz	x9, 0x5f18
    5efc:	f7 03 03 aa	mov	x23, x3
    5f00:	f6 03 04 aa	mov	x22, x4
    5f04:	28 ff 50 d3	lsr	x8, x25, #16
    5f08:	20 0d 28 cb	sub	x0, x9, w8, uxtb #3
    5f0c:	39 ff 58 d3	lsr	x25, x25, #24
    5f10:	19 01 00 b5	cbnz	x25, 0x5f30
    5f14:	0e 00 00 14	b	0x5f4c
    5f18:	f7 03 03 aa	mov	x23, x3
    5f1c:	f6 03 04 aa	mov	x22, x4
    5f20:	29 3b 48 d3	ubfx	x9, x25, #8, #7
    5f24:	00 0d 09 8b	add	x0, x8, x9, lsl #3
    5f28:	39 ff 58 d3	lsr	x25, x25, #24
    5f2c:	19 01 00 b4	cbz	x25, 0x5f4c
    5f30:	28 27 40 92	and	x8, x25, #0x3ff
    5f34:	00 78 68 f8	ldr	x0, [x0, x8, lsl #3]
    5f38:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12019
    5f3c:	28 ff 4a d3	lsr	x8, x25, #10
    5f40:	3f ff 0f f1	cmp	x25, #0x3ff
    5f44:	f9 03 08 aa	mov	x25, x8
    5f48:	48 ff ff 54	b.hi	0x5f30
    5f4c:	18 00 00 f9	str	x24, [x0]
    5f50:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    5f54:	80 82 00 91	add	x0, x20, #0x20
    5f58:	e1 03 15 aa	mov	x1, x21
    5f5c:	e2 03 13 aa	mov	x2, x19
    5f60:	e3 03 17 aa	mov	x3, x23
    5f64:	e4 03 16 aa	mov	x4, x22
    5f68:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    5f6c:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    5f70:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    5f74:	f8 5f 42 a9	ldp	x24, x23, [sp, #0x20]
    5f78:	fa 67 41 a9	ldp	x26, x25, [sp, #0x10]
    5f7c:	ff 83 01 91	add	sp, sp, #0x60
    5f80:	a0 00 1f d6	br	x5
