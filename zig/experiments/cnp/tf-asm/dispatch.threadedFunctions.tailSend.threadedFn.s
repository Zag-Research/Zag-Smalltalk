; symbol: _dispatch.threadedFunctions.tailSend.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_dispatch.threadedFunctions.tailSend.threadedFn:
    650c:	ff 83 00 d1	sub	sp, sp, #0x20
    6510:	fd 7b 01 a9	stp	x29, x30, [sp, #0x10]
    6514:	fd 43 00 91	add	x29, sp, #0x10
    6518:	09 00 40 f9	ldr	x9, [x0]
    651c:	28 9d 60 d3	ubfx	x8, x9, #32, #8
    6520:	28 78 68 f8	ldr	x8, [x1, x8, lsl #3]
    6524:	0a 09 00 12	and	w10, w8, #0x7
    6528:	5f 05 00 71	cmp	w10, #0x1
    652c:	00 01 00 54	b.eq	0x654c
    6530:	aa 01 00 34	cbz	w10, 0x6564
    6534:	28 04 80 52	mov	w8, #0x21
    6538:	21 9d 40 92	and	x1, x9, #0xffffffffff
    653c:	01 3d 58 b3	bfi	x1, x8, #40, #16
    6540:	3f 01 01 eb	cmp	x9, x1
    6544:	c1 01 00 54	b.ne	0x657c
    6548:	2d 00 00 14	b	0x65fc
    654c:	08 1d 03 53	ubfx	w8, w8, #3, #5
    6550:	21 9d 40 92	and	x1, x9, #0xffffffffff
    6554:	01 3d 58 b3	bfi	x1, x8, #40, #16
    6558:	3f 01 01 eb	cmp	x9, x1
    655c:	01 01 00 54	b.ne	0x657c
    6560:	27 00 00 14	b	0x65fc
    6564:	c8 05 00 b4	cbz	x8, 0x661c
    6568:	08 01 40 79	ldrh	w8, [x8]
    656c:	21 9d 40 92	and	x1, x9, #0xffffffffff
    6570:	01 3d 58 b3	bfi	x1, x8, #40, #16
    6574:	3f 01 01 eb	cmp	x9, x1
    6578:	20 04 00 54	b.eq	0x65fc
    657c:	0a 00 00 90	adrp	x10, __MergedGlobals@PAGE
    6580:	4a 01 00 91	add	x10, x10, __MergedGlobals@PAGEOFF
    6584:	08 3d 40 92	and	x8, x8, #0xffff
    6588:	4a 0d 08 8b	add	x10, x10, x8, lsl #3
    658c:	4a 55 40 f9	ldr	x10, [x10, #0xa8]
    6590:	4b 05 40 f9	ldr	x11, [x10, #0x8]
    6594:	ec 03 09 2a	mov	w12, w9
    6598:	6b 7d 0c 9b	mul	x11, x11, x12
    659c:	6b fd 60 d3	lsr	x11, x11, #32
    65a0:	4b 0d 0b 8b	add	x11, x10, x11, lsl #3
    65a4:	6a 8d 41 f8	ldr	x10, [x11, #0x18]!
    65a8:	4c 05 40 f9	ldr	x12, [x10, #0x8]
    65ac:	9f 01 01 eb	cmp	x12, x1
    65b0:	20 01 00 54	b.eq	0x65d4
    65b4:	6a 05 40 f9	ldr	x10, [x11, #0x8]
    65b8:	4c 05 40 f9	ldr	x12, [x10, #0x8]
    65bc:	9f 01 01 eb	cmp	x12, x1
    65c0:	a0 00 00 54	b.eq	0x65d4
    65c4:	6a 09 40 f9	ldr	x10, [x11, #0x10]
    65c8:	4b 05 40 f9	ldr	x11, [x10, #0x8]
    65cc:	7f 01 01 eb	cmp	x11, x1
    65d0:	21 02 00 54	b.ne	0x6614
    65d4:	3f 3d 58 f2	tst	x9, #0xffff0000000000
    65d8:	21 01 00 54	b.ne	0x65fc
    65dc:	28 01 80 52	mov	w8, #0x9
    65e0:	e8 03 00 39	strb	w8, [sp]
    65e4:	01 00 00 f9	str	x1, [x0]
    65e8:	08 20 00 39	strb	w8, [x0, #0x8]
    65ec:	88 00 80 52	mov	w8, #0x4
    65f0:	e8 23 00 39	strb	w8, [sp, #0x8]
    65f4:	0a 08 00 f9	str	x10, [x0, #0x10]
    65f8:	08 60 00 39	strb	w8, [x0, #0x18]
    65fc:	00 00 00 90	adrp	x0, ___anon_8320@PAGE
    6600:	00 00 00 91	add	x0, x0, ___anon_8320@PAGEOFF
    6604:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    6608:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    660c:	61 01 80 52	mov	w1, #0xb
    6610:	00 00 00 94	bl	_debug.defaultPanic
    6614:	e0 03 08 aa	mov	x0, x8
    6618:	00 00 00 94	bl	_dispatch.DispatchHandler.loadMethodForClass
    661c:	88 02 80 52	mov	w8, #0x14
    6620:	21 9d 40 92	and	x1, x9, #0xffffffffff
    6624:	01 3d 58 b3	bfi	x1, x8, #40, #16
    6628:	3f 01 01 eb	cmp	x9, x1
    662c:	81 fa ff 54	b.ne	0x657c
    6630:	f3 ff ff 17	b	0x65fc
