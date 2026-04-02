; symbol: _primitives.BlockClosure.threadedFns.pushClosure.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.pushClosure.threadedFn:
    888c:	ff c3 03 d1	sub	sp, sp, #0xf0
    8890:	fc 6f 09 a9	stp	x28, x27, [sp, #0x90]
    8894:	fa 67 0a a9	stp	x26, x25, [sp, #0xa0]
    8898:	f8 5f 0b a9	stp	x24, x23, [sp, #0xb0]
    889c:	f6 57 0c a9	stp	x22, x21, [sp, #0xc0]
    88a0:	f4 4f 0d a9	stp	x20, x19, [sp, #0xd0]
    88a4:	fd 7b 0e a9	stp	x29, x30, [sp, #0xe0]
    88a8:	fd 83 03 91	add	x29, sp, #0xe0
    88ac:	f9 03 04 aa	mov	x25, x4
    88b0:	f6 03 03 aa	mov	x22, x3
    88b4:	e2 0b 00 f9	str	x2, [sp, #0x10]
    88b8:	f7 03 01 aa	mov	x23, x1
    88bc:	f5 03 00 aa	mov	x21, x0
    88c0:	14 00 40 f9	ldr	x20, [x0]
    88c4:	9c fe 48 d3	lsr	x28, x20, #8
    88c8:	9a fe 72 d3	lsr	x26, x20, #50
    88cc:	93 56 48 d3	ubfx	x19, x20, #8, #14
    88d0:	b3 00 00 b4	cbz	x19, 0x88e4
    88d4:	62 f2 7d d3	lsl	x2, x19, #3
    88d8:	e0 63 00 91	add	x0, sp, #0x18
    88dc:	e1 03 17 aa	mov	x1, x23
    88e0:	00 00 00 94	bl	_memcpy
    88e4:	9b 03 1a 0b	add	w27, w28, w26
    88e8:	48 0f 00 11	add	w8, w26, #0x3
    88ec:	09 29 7d d3	ubfiz	x9, x8, #3, #11
    88f0:	f8 02 09 eb	subs	x24, x23, x9
    88f4:	e9 ca 73 92	and	x9, x23, #0xffffffffffffe000
    88f8:	00 13 49 fa	ccmp	x24, x9, #0x0, ne
    88fc:	a3 0b 00 54	b.lo	0x8a70
    8900:	f9 07 00 f9	str	x25, [sp, #0x8]
    8904:	f9 03 16 aa	mov	x25, x22
    8908:	7b 07 00 11	add	w27, w27, #0x1
    890c:	88 fe 56 d3	lsr	x8, x20, #22
    8910:	89 8e 56 d3	ubfx	x9, x20, #22, #14
    8914:	08 01 1c 4b	sub	w8, w8, w28
    8918:	3f 01 13 6b	cmp	w9, w19
    891c:	00 04 00 54	b.eq	0x899c
    8920:	0a 00 80 d2	mov	x10, #0x0
    8924:	09 23 00 91	add	x9, x24, #0x8
    8928:	8b 2b 7d d3	ubfiz	x11, x28, #3, #11
    892c:	6b 01 17 8b	add	x11, x11, x23
    8930:	0c 35 40 92	and	x12, x8, #0x3fff
    8934:	0d 35 00 12	and	w13, w8, #0x3fff
    8938:	bf 21 00 71	cmp	w13, #0x8
    893c:	03 02 00 54	b.lo	0x897c
    8940:	2d 01 0b cb	sub	x13, x9, x11
    8944:	bf 01 01 f1	cmp	x13, #0x40
    8948:	a3 01 00 54	b.lo	0x897c
    894c:	8a 29 7d 92	and	x10, x12, #0x3ff8
    8950:	6d 81 00 91	add	x13, x11, #0x20
    8954:	2e 81 00 91	add	x14, x9, #0x20
    8958:	ef 03 0a aa	mov	x15, x10
    895c:	a0 05 7f ad	ldp	q0, q1, [x13, #-0x20]
    8960:	a2 0d c2 ac	ldp	q2, q3, [x13], #0x40
    8964:	c0 05 3f ad	stp	q0, q1, [x14, #-0x20]
    8968:	c2 0d 82 ac	stp	q2, q3, [x14], #0x40
    896c:	ef 21 00 f1	subs	x15, x15, #0x8
    8970:	61 ff ff 54	b.ne	0x895c
    8974:	5f 01 0c eb	cmp	x10, x12
    8978:	20 01 00 54	b.eq	0x899c
    897c:	4c 01 0c cb	sub	x12, x10, x12
    8980:	4d f1 7d d3	lsl	x13, x10, #3
    8984:	6a 01 0d 8b	add	x10, x11, x13
    8988:	29 01 0d 8b	add	x9, x9, x13
    898c:	4b 85 40 f8	ldr	x11, [x10], #0x8
    8990:	2b 85 00 f8	str	x11, [x9], #0x8
    8994:	8c 05 00 b1	adds	x12, x12, #0x1
    8998:	a3 ff ff 54	b.lo	0x898c
    899c:	08 71 1d 53	lsl	w8, w8, #3
    89a0:	08 21 00 11	add	w8, w8, #0x8
    89a4:	08 29 7d 92	and	x8, x8, #0x3ff8
    89a8:	1c 03 08 8b	add	x28, x24, x8
    89ac:	68 07 00 11	add	w8, w27, #0x1
    89b0:	09 35 40 92	and	x9, x8, #0x3fff
    89b4:	48 0b 00 11	add	w8, w26, #0x2
    89b8:	08 35 40 92	and	x8, x8, #0x3fff
    89bc:	29 01 08 eb	subs	x9, x9, x8
    89c0:	a0 00 00 54	b.eq	0x89d4
    89c4:	22 f1 7d d3	lsl	x2, x9, #3
    89c8:	80 0f 08 8b	add	x0, x28, x8, lsl #3
    89cc:	e1 63 00 91	add	x1, sp, #0x18
    89d0:	00 00 00 94	bl	_memcpy
    89d4:	08 0e 80 52	mov	w8, #0x70
    89d8:	e8 b3 01 39	strb	w8, [sp, #0x6c]
    89dc:	68 2b 00 12	and	w8, w27, #0x7ff
    89e0:	e8 e3 00 79	strh	w8, [sp, #0x70]
    89e4:	ff df 00 79	strh	wzr, [sp, #0x6e]
    89e8:	9f 02 00 34	cbz	wzr, 0x8a38
    89ec:	88 03 00 f9	str	x8, [x28]
    89f0:	a8 0a 40 f9	ldr	x8, [x21, #0x10]
    89f4:	88 07 00 f9	str	x8, [x28, #0x8]
    89f8:	5a 00 00 b4	cbz	x26, 0x8a00
    89fc:	96 0b 00 f9	str	x22, [x28, #0x10]
    8a00:	1c 03 00 f9	str	x28, [x24]
    8a04:	a5 12 40 f9	ldr	x5, [x21, #0x20]
    8a08:	a0 c2 00 91	add	x0, x21, #0x30
    8a0c:	e1 03 18 aa	mov	x1, x24
    8a10:	e4 8b 40 a9	ldp	x4, x2, [sp, #0x8]
    8a14:	e3 03 19 aa	mov	x3, x25
    8a18:	fd 7b 4e a9	ldp	x29, x30, [sp, #0xe0]
    8a1c:	f4 4f 4d a9	ldp	x20, x19, [sp, #0xd0]
    8a20:	f6 57 4c a9	ldp	x22, x21, [sp, #0xc0]
    8a24:	f8 5f 4b a9	ldp	x24, x23, [sp, #0xb0]
    8a28:	fa 67 4a a9	ldp	x26, x25, [sp, #0xa0]
    8a2c:	fc 6f 49 a9	ldp	x28, x27, [sp, #0x90]
    8a30:	ff c3 03 91	add	sp, sp, #0xf0
    8a34:	a0 00 1f d6	br	x5
    8a38:	e8 be 70 d3	lsl	x8, x23, #16
    8a3c:	08 51 6d 92	and	x8, x8, #0xfffff80000
    8a40:	e9 b3 41 39	ldrb	w9, [sp, #0x6c]
    8a44:	28 19 58 b3	bfi	x8, x9, #40, #7
    8a48:	e9 e3 40 79	ldrh	w9, [sp, #0x70]
    8a4c:	29 29 00 12	and	w9, w9, #0x7ff
    8a50:	08 d1 09 aa	orr	x8, x8, x9, lsl #52
    8a54:	09 05 80 52	mov	w9, #0x28
    8a58:	08 01 09 aa	orr	x8, x8, x9
    8a5c:	88 03 00 f9	str	x8, [x28]
    8a60:	a8 0a 40 f9	ldr	x8, [x21, #0x10]
    8a64:	88 07 00 f9	str	x8, [x28, #0x8]
    8a68:	ba fc ff b5	cbnz	x26, 0x89fc
    8a6c:	e5 ff ff 17	b	0x8a00
    8a70:	18 35 40 92	and	x24, x8, #0x3fff
    8a74:	a0 a3 01 d1	sub	x0, x29, #0x68
    8a78:	e1 03 17 aa	mov	x1, x23
    8a7c:	e2 03 16 aa	mov	x2, x22
    8a80:	e3 03 19 aa	mov	x3, x25
    8a84:	00 00 00 94	bl	_process.Stack.spillStack
    8a88:	a8 e7 79 a9	ldp	x8, x25, [x29, #-0x68]
    8a8c:	a9 83 5a f8	ldur	x9, [x29, #-0x58]
    8a90:	e9 07 00 f9	str	x9, [sp, #0x8]
    8a94:	18 0d 18 cb	sub	x24, x8, x24, lsl #3
    8a98:	7b 07 00 11	add	w27, w27, #0x1
    8a9c:	88 fe 56 d3	lsr	x8, x20, #22
    8aa0:	89 8e 56 d3	ubfx	x9, x20, #22, #14
    8aa4:	08 01 1c 4b	sub	w8, w8, w28
    8aa8:	3f 01 13 6b	cmp	w9, w19
    8aac:	a1 f3 ff 54	b.ne	0x8920
    8ab0:	bb ff ff 17	b	0x899c
