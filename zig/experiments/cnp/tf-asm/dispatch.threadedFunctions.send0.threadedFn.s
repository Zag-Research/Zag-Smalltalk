; symbol: _dispatch.threadedFunctions.send0.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_dispatch.threadedFunctions.send0.threadedFn:
    6350:	ff 43 01 d1	sub	sp, sp, #0x50
    6354:	f6 57 02 a9	stp	x22, x21, [sp, #0x20]
    6358:	f4 4f 03 a9	stp	x20, x19, [sp, #0x30]
    635c:	fd 7b 04 a9	stp	x29, x30, [sp, #0x40]
    6360:	fd 03 01 91	add	x29, sp, #0x40
    6364:	f3 03 00 aa	mov	x19, x0
    6368:	09 00 40 f9	ldr	x9, [x0]
    636c:	28 00 40 f9	ldr	x8, [x1]
    6370:	0a 09 00 12	and	w10, w8, #0x7
    6374:	5f 05 00 71	cmp	w10, #0x1
    6378:	00 01 00 54	b.eq	0x6398
    637c:	aa 01 00 34	cbz	w10, 0x63b0
    6380:	2a 04 80 52	mov	w10, #0x21
    6384:	28 9d 40 92	and	x8, x9, #0xffffffffff
    6388:	48 3d 58 b3	bfi	x8, x10, #40, #16
    638c:	3f 01 08 eb	cmp	x9, x8
    6390:	c0 01 00 54	b.eq	0x63c8
    6394:	17 00 00 14	b	0x63f0
    6398:	0a 1d 03 53	ubfx	w10, w8, #3, #5
    639c:	28 9d 40 92	and	x8, x9, #0xffffffffff
    63a0:	48 3d 58 b3	bfi	x8, x10, #40, #16
    63a4:	3f 01 08 eb	cmp	x9, x8
    63a8:	00 01 00 54	b.eq	0x63c8
    63ac:	11 00 00 14	b	0x63f0
    63b0:	68 01 00 b4	cbz	x8, 0x63dc
    63b4:	0a 01 40 79	ldrh	w10, [x8]
    63b8:	28 9d 40 92	and	x8, x9, #0xffffffffff
    63bc:	48 3d 58 b3	bfi	x8, x10, #40, #16
    63c0:	3f 01 08 eb	cmp	x9, x8
    63c4:	61 01 00 54	b.ne	0x63f0
    63c8:	75 0a 40 f9	ldr	x21, [x19, #0x10]
    63cc:	b6 a2 00 91	add	x22, x21, #0x28
    63d0:	88 fc 70 d3	lsr	x8, x4, #48
    63d4:	48 04 00 b5	cbnz	x8, 0x645c
    63d8:	3f 00 00 14	b	0x64d4
    63dc:	8a 02 80 52	mov	w10, #0x14
    63e0:	28 9d 40 92	and	x8, x9, #0xffffffffff
    63e4:	48 3d 58 b3	bfi	x8, x10, #40, #16
    63e8:	3f 01 08 eb	cmp	x9, x8
    63ec:	e0 fe ff 54	b.eq	0x63c8
    63f0:	0b 00 00 90	adrp	x11, __MergedGlobals@PAGE
    63f4:	6b 01 00 91	add	x11, x11, __MergedGlobals@PAGEOFF
    63f8:	40 3d 40 92	and	x0, x10, #0xffff
    63fc:	6a 0d 00 8b	add	x10, x11, x0, lsl #3
    6400:	4a 55 40 f9	ldr	x10, [x10, #0xa8]
    6404:	4b 05 40 f9	ldr	x11, [x10, #0x8]
    6408:	ec 03 09 2a	mov	w12, w9
    640c:	6b 7d 0c 9b	mul	x11, x11, x12
    6410:	6b fd 60 d3	lsr	x11, x11, #32
    6414:	4a 0d 0b 8b	add	x10, x10, x11, lsl #3
    6418:	55 8d 41 f8	ldr	x21, [x10, #0x18]!
    641c:	ab 06 40 f9	ldr	x11, [x21, #0x8]
    6420:	7f 01 08 eb	cmp	x11, x8
    6424:	20 01 00 54	b.eq	0x6448
    6428:	55 05 40 f9	ldr	x21, [x10, #0x8]
    642c:	ab 06 40 f9	ldr	x11, [x21, #0x8]
    6430:	7f 01 08 eb	cmp	x11, x8
    6434:	a0 00 00 54	b.eq	0x6448
    6438:	55 09 40 f9	ldr	x21, [x10, #0x10]
    643c:	aa 06 40 f9	ldr	x10, [x21, #0x8]
    6440:	5f 01 08 eb	cmp	x10, x8
    6444:	01 06 00 54	b.ne	0x6504
    6448:	3f 3d 58 f2	tst	x9, #0xffff0000000000
    644c:	e0 02 00 54	b.eq	0x64a8
    6450:	b6 a2 00 91	add	x22, x21, #0x28
    6454:	88 fc 70 d3	lsr	x8, x4, #48
    6458:	e8 03 00 b4	cbz	x8, 0x64d4
    645c:	88 bc 40 92	and	x8, x4, #0xffffffffffff
    6460:	a8 03 00 b4	cbz	x8, 0x64d4
    6464:	e0 43 00 91	add	x0, sp, #0x10
    6468:	e9 03 01 aa	mov	x9, x1
    646c:	e1 03 03 aa	mov	x1, x3
    6470:	f4 03 02 aa	mov	x20, x2
    6474:	e2 03 09 aa	mov	x2, x9
    6478:	e3 03 08 aa	mov	x3, x8
    647c:	00 00 00 94	bl	_context.push
    6480:	e1 0f 41 a9	ldp	x1, x3, [sp, #0x10]
    6484:	68 12 40 f9	ldr	x8, [x19, #0x20]
    6488:	69 c2 00 91	add	x9, x19, #0x30
    648c:	69 20 02 a9	stp	x9, x8, [x3, #0x20]
    6490:	a5 0e 40 f9	ldr	x5, [x21, #0x18]
    6494:	a8 b2 7d 92	and	x8, x21, #0xfffffffffff8
    6498:	04 c1 01 aa	orr	x4, x8, x1, lsl #48
    649c:	c0 42 00 91	add	x0, x22, #0x10
    64a0:	e2 03 14 aa	mov	x2, x20
    64a4:	13 00 00 14	b	0x64f0
    64a8:	29 01 80 52	mov	w9, #0x9
    64ac:	e9 03 00 39	strb	w9, [sp]
    64b0:	68 02 00 f9	str	x8, [x19]
    64b4:	69 22 00 39	strb	w9, [x19, #0x8]
    64b8:	88 00 80 52	mov	w8, #0x4
    64bc:	e8 23 00 39	strb	w8, [sp, #0x8]
    64c0:	75 0a 00 f9	str	x21, [x19, #0x10]
    64c4:	68 62 00 39	strb	w8, [x19, #0x18]
    64c8:	b6 a2 00 91	add	x22, x21, #0x28
    64cc:	88 fc 70 d3	lsr	x8, x4, #48
    64d0:	68 fc ff b5	cbnz	x8, 0x645c
    64d4:	68 12 40 f9	ldr	x8, [x19, #0x20]
    64d8:	69 c2 00 91	add	x9, x19, #0x30
    64dc:	69 20 02 a9	stp	x9, x8, [x3, #0x20]
    64e0:	a5 0e 40 f9	ldr	x5, [x21, #0x18]
    64e4:	a8 b2 7d 92	and	x8, x21, #0xfffffffffff8
    64e8:	04 c1 01 aa	orr	x4, x8, x1, lsl #48
    64ec:	c0 42 00 91	add	x0, x22, #0x10
    64f0:	fd 7b 44 a9	ldp	x29, x30, [sp, #0x40]
    64f4:	f4 4f 43 a9	ldp	x20, x19, [sp, #0x30]
    64f8:	f6 57 42 a9	ldp	x22, x21, [sp, #0x20]
    64fc:	ff 43 01 91	add	sp, sp, #0x50
    6500:	a0 00 1f d6	br	x5
    6504:	e1 03 08 aa	mov	x1, x8
    6508:	00 00 00 94	bl	_dispatch.DispatchHandler.loadMethodForClass
