; symbol: _primitives.BlockClosure.threadedFns.asThunk.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.asThunk.threadedFn:
    7f20:	f8 5f bc a9	stp	x24, x23, [sp, #-0x40]!
    7f24:	f6 57 01 a9	stp	x22, x21, [sp, #0x10]
    7f28:	f4 4f 02 a9	stp	x20, x19, [sp, #0x20]
    7f2c:	fd 7b 03 a9	stp	x29, x30, [sp, #0x30]
    7f30:	fd c3 00 91	add	x29, sp, #0x30
    7f34:	28 00 40 f9	ldr	x8, [x1]
    7f38:	09 09 00 12	and	w9, w8, #0x7
    7f3c:	3f 05 00 71	cmp	w9, #0x1
    7f40:	20 01 00 54	b.eq	0x7f64
    7f44:	69 06 00 34	cbz	w9, 0x8010
    7f48:	1f 1d 7c f2	tst	x8, #0xff0
    7f4c:	a1 01 00 54	b.ne	0x7f80
    7f50:	09 cd 74 92	and	x9, x8, #0xfffffffffffff000
    7f54:	09 0d 78 b3	bfi	x9, x8, #8, #4
    7f58:	28 12 80 52	mov	w8, #0x91
    7f5c:	28 01 08 aa	orr	x8, x9, x8
    7f60:	25 00 00 14	b	0x7ff4
    7f64:	09 10 e0 d2	mov	x9, #0x80000000000000
    7f68:	09 01 09 8b	add	x9, x8, x9
    7f6c:	29 fd 78 d3	lsr	x9, x9, #56
    7f70:	89 00 00 b5	cbnz	x9, 0x7f80
    7f74:	29 0a 80 52	mov	w9, #0x51
    7f78:	28 21 08 aa	orr	x8, x9, x8, lsl #8
    7f7c:	1e 00 00 14	b	0x7ff4
    7f80:	49 c8 73 92	and	x9, x2, #0xffffffffffffe000
    7f84:	2a 19 48 f9	ldr	x10, [x9, #0x1030]
    7f88:	4b 41 00 91	add	x11, x10, #0x10
    7f8c:	2b 19 08 f9	str	x11, [x9, #0x1030]
    7f90:	49 fd 43 d3	lsr	x9, x10, #3
    7f94:	2b 29 13 12	and	w11, w9, #0xffe000
    7f98:	29 35 4b 4a	eor	w9, w9, w11, lsr #13
    7f9c:	6b 97 8a 52	mov	w11, #0x54bb
    7fa0:	ab fd bf 72	movk	w11, #0xffed, lsl #16
    7fa4:	29 7d 0b 1b	mul	w9, w9, w11
    7fa8:	2b 3d 18 12	and	w11, w9, #0xffff00
    7fac:	29 21 4b 4a	eor	w9, w9, w11, lsr #8
    7fb0:	2b 6a 89 52	mov	w11, #0x4b51
    7fb4:	8b f5 bf 72	movk	w11, #0xffac, lsl #16
    7fb8:	29 7d 0b 1b	mul	w9, w9, w11
    7fbc:	2b 31 15 12	and	w11, w9, #0xfff800
    7fc0:	29 2d 4b 4a	eor	w9, w9, w11, lsr #11
    7fc4:	6b 75 91 52	mov	w11, #0x8bab
    7fc8:	2b 06 a0 72	movk	w11, #0x31, lsl #16
    7fcc:	29 7d 0b 1b	mul	w9, w9, w11
    7fd0:	2b 25 12 12	and	w11, w9, #0xffc000
    7fd4:	29 39 4b 4a	eor	w9, w9, w11, lsr #14
    7fd8:	8b 04 80 d2	mov	x11, #0x24
    7fdc:	0b 60 ce f2	movk	x11, #0x7300, lsl #32
    7fe0:	2b 02 e0 f2	movk	x11, #0x11, lsl #48
    7fe4:	2b 5d 70 b3	bfi	x11, x9, #16, #24
    7fe8:	4b 21 00 a9	stp	x11, x8, [x10]
    7fec:	28 27 80 52	mov	w8, #0x139
    7ff0:	08 41 0a aa	orr	x8, x8, x10, lsl #16
    7ff4:	28 00 00 f9	str	x8, [x1]
    7ff8:	05 04 41 f8	ldr	x5, [x0], #0x10
    7ffc:	fd 7b 43 a9	ldp	x29, x30, [sp, #0x30]
    8000:	f4 4f 42 a9	ldp	x20, x19, [sp, #0x20]
    8004:	f6 57 41 a9	ldp	x22, x21, [sp, #0x10]
    8008:	f8 5f c4 a8	ldp	x24, x23, [sp], #0x40
    800c:	a0 00 1f d6	br	x5
    8010:	f3 03 00 aa	mov	x19, x0
    8014:	e0 03 08 aa	mov	x0, x8
    8018:	f4 03 04 aa	mov	x20, x4
    801c:	f6 03 03 aa	mov	x22, x3
    8020:	f5 03 02 aa	mov	x21, x2
    8024:	f7 03 01 aa	mov	x23, x1
    8028:	00 00 00 94	bl	_object.zag.Object.toWithCheck__anon_12925
    802c:	e1 03 17 aa	mov	x1, x23
    8030:	e2 03 15 aa	mov	x2, x21
    8034:	e3 03 16 aa	mov	x3, x22
    8038:	e4 03 14 aa	mov	x4, x20
    803c:	e8 03 00 aa	mov	x8, x0
    8040:	e0 03 13 aa	mov	x0, x19
    8044:	29 09 80 52	mov	w9, #0x49
    8048:	28 41 08 aa	orr	x8, x9, x8, lsl #16
    804c:	ea ff ff 17	b	0x7ff4
