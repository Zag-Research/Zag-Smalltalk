; symbol: _dispatch.threadedFunctions.send.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_dispatch.threadedFunctions.send.threadedFn:
    6144:	ff 83 01 d1	sub	sp, sp, #0x60
    6148:	f8 5f 02 a9	stp	x24, x23, [sp, #0x20]
    614c:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    6150:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    6154:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    6158:	fd 43 01 91	add	x29, sp, #0x50
    615c:	f3 03 00 aa	mov	x19, x0
    6160:	0a 00 40 f9	ldr	x10, [x0]
    6164:	48 fd 5d d3	lsr	x8, x10, #29
    6168:	15 11 7d 92	and	x21, x8, #0xf8
    616c:	a9 02 01 8b	add	x9, x21, x1
    6170:	28 01 40 f9	ldr	x8, [x9]
    6174:	0b 09 00 12	and	w11, w8, #0x7
    6178:	7f 05 00 71	cmp	w11, #0x1
    617c:	00 01 00 54	b.eq	0x619c
    6180:	ab 01 00 34	cbz	w11, 0x61b4
    6184:	2b 04 80 52	mov	w11, #0x21
    6188:	48 9d 40 92	and	x8, x10, #0xffffffffff
    618c:	68 3d 58 b3	bfi	x8, x11, #40, #16
    6190:	5f 01 08 eb	cmp	x10, x8
    6194:	c0 01 00 54	b.eq	0x61cc
    6198:	17 00 00 14	b	0x61f4
    619c:	0b 1d 03 53	ubfx	w11, w8, #3, #5
    61a0:	48 9d 40 92	and	x8, x10, #0xffffffffff
    61a4:	68 3d 58 b3	bfi	x8, x11, #40, #16
    61a8:	5f 01 08 eb	cmp	x10, x8
    61ac:	00 01 00 54	b.eq	0x61cc
    61b0:	11 00 00 14	b	0x61f4
    61b4:	68 01 00 b4	cbz	x8, 0x61e0
    61b8:	0b 01 40 79	ldrh	w11, [x8]
    61bc:	48 9d 40 92	and	x8, x10, #0xffffffffff
    61c0:	68 3d 58 b3	bfi	x8, x11, #40, #16
    61c4:	5f 01 08 eb	cmp	x10, x8
    61c8:	61 01 00 54	b.ne	0x61f4
    61cc:	76 0a 40 f9	ldr	x22, [x19, #0x10]
    61d0:	d7 a2 00 91	add	x23, x22, #0x28
    61d4:	88 fc 70 d3	lsr	x8, x4, #48
    61d8:	48 04 00 b5	cbnz	x8, 0x6260
    61dc:	40 00 00 14	b	0x62dc
    61e0:	8b 02 80 52	mov	w11, #0x14
    61e4:	48 9d 40 92	and	x8, x10, #0xffffffffff
    61e8:	68 3d 58 b3	bfi	x8, x11, #40, #16
    61ec:	5f 01 08 eb	cmp	x10, x8
    61f0:	e0 fe ff 54	b.eq	0x61cc
    61f4:	0c 00 00 90	adrp	x12, __MergedGlobals@PAGE
    61f8:	8c 01 00 91	add	x12, x12, __MergedGlobals@PAGEOFF
    61fc:	60 3d 40 92	and	x0, x11, #0xffff
    6200:	8b 0d 00 8b	add	x11, x12, x0, lsl #3
    6204:	6b 55 40 f9	ldr	x11, [x11, #0xa8]
    6208:	6c 05 40 f9	ldr	x12, [x11, #0x8]
    620c:	ed 03 0a 2a	mov	w13, w10
    6210:	8c 7d 0d 9b	mul	x12, x12, x13
    6214:	8c fd 60 d3	lsr	x12, x12, #32
    6218:	6b 0d 0c 8b	add	x11, x11, x12, lsl #3
    621c:	76 8d 41 f8	ldr	x22, [x11, #0x18]!
    6220:	cc 06 40 f9	ldr	x12, [x22, #0x8]
    6224:	9f 01 08 eb	cmp	x12, x8
    6228:	20 01 00 54	b.eq	0x624c
    622c:	76 05 40 f9	ldr	x22, [x11, #0x8]
    6230:	cc 06 40 f9	ldr	x12, [x22, #0x8]
    6234:	9f 01 08 eb	cmp	x12, x8
    6238:	a0 00 00 54	b.eq	0x624c
    623c:	76 09 40 f9	ldr	x22, [x11, #0x10]
    6240:	cb 06 40 f9	ldr	x11, [x22, #0x8]
    6244:	7f 01 08 eb	cmp	x11, x8
    6248:	41 06 00 54	b.ne	0x6310
    624c:	5f 3d 58 f2	tst	x10, #0xffff0000000000
    6250:	00 03 00 54	b.eq	0x62b0
    6254:	d7 a2 00 91	add	x23, x22, #0x28
    6258:	88 fc 70 d3	lsr	x8, x4, #48
    625c:	08 04 00 b4	cbz	x8, 0x62dc
    6260:	88 bc 40 92	and	x8, x4, #0xffffffffffff
    6264:	c8 03 00 b4	cbz	x8, 0x62dc
    6268:	e0 43 00 91	add	x0, sp, #0x10
    626c:	e9 03 01 aa	mov	x9, x1
    6270:	e1 03 03 aa	mov	x1, x3
    6274:	f4 03 02 aa	mov	x20, x2
    6278:	e2 03 09 aa	mov	x2, x9
    627c:	e3 03 08 aa	mov	x3, x8
    6280:	00 00 00 94	bl	_context.push
    6284:	e1 0f 41 a9	ldp	x1, x3, [sp, #0x10]
    6288:	68 12 40 f9	ldr	x8, [x19, #0x20]
    628c:	69 c2 00 91	add	x9, x19, #0x30
    6290:	69 20 02 a9	stp	x9, x8, [x3, #0x20]
    6294:	a8 02 01 0b	add	w8, w21, w1
    6298:	c9 b2 7d 92	and	x9, x22, #0xfffffffffff8
    629c:	24 c1 08 aa	orr	x4, x9, x8, lsl #48
    62a0:	c5 0e 40 f9	ldr	x5, [x22, #0x18]
    62a4:	e0 42 00 91	add	x0, x23, #0x10
    62a8:	e2 03 14 aa	mov	x2, x20
    62ac:	13 00 00 14	b	0x62f8
    62b0:	2a 01 80 52	mov	w10, #0x9
    62b4:	ea 03 00 39	strb	w10, [sp]
    62b8:	68 02 00 f9	str	x8, [x19]
    62bc:	6a 22 00 39	strb	w10, [x19, #0x8]
    62c0:	88 00 80 52	mov	w8, #0x4
    62c4:	e8 23 00 39	strb	w8, [sp, #0x8]
    62c8:	76 0a 00 f9	str	x22, [x19, #0x10]
    62cc:	68 62 00 39	strb	w8, [x19, #0x18]
    62d0:	d7 a2 00 91	add	x23, x22, #0x28
    62d4:	88 fc 70 d3	lsr	x8, x4, #48
    62d8:	48 fc ff b5	cbnz	x8, 0x6260
    62dc:	68 12 40 f9	ldr	x8, [x19, #0x20]
    62e0:	6a c2 00 91	add	x10, x19, #0x30
    62e4:	6a 20 02 a9	stp	x10, x8, [x3, #0x20]
    62e8:	c5 0e 40 f9	ldr	x5, [x22, #0x18]
    62ec:	c8 b2 7d 92	and	x8, x22, #0xfffffffffff8
    62f0:	04 c1 09 aa	orr	x4, x8, x9, lsl #48
    62f4:	e0 42 00 91	add	x0, x23, #0x10
    62f8:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    62fc:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    6300:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    6304:	f8 5f 42 a9	ldp	x24, x23, [sp, #0x20]
    6308:	ff 83 01 91	add	sp, sp, #0x60
    630c:	a0 00 1f d6	br	x5
    6310:	e1 03 08 aa	mov	x1, x8
    6314:	00 00 00 94	bl	_dispatch.DispatchHandler.loadMethodForClass
