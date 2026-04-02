; symbol: _controlWords.classCase.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.classCase.threadedFn:
    5808:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    580c:	fd 03 00 91	mov	x29, sp
    5810:	28 00 40 f9	ldr	x8, [x1]
    5814:	09 09 00 12	and	w9, w8, #0x7
    5818:	3f 05 00 71	cmp	w9, #0x1
    581c:	80 00 00 54	b.eq	0x582c
    5820:	a9 00 00 34	cbz	w9, 0x5834
    5824:	29 04 80 52	mov	w9, #0x21
    5828:	05 00 00 14	b	0x583c
    582c:	09 1d 03 53	ubfx	w9, w8, #3, #5
    5830:	03 00 00 14	b	0x583c
    5834:	48 09 00 b4	cbz	x8, 0x595c
    5838:	09 01 40 79	ldrh	w9, [x8]
    583c:	0a 02 80 52	mov	w10, #0x10
    5840:	eb ff 87 52	mov	w11, #0x3fff
    5844:	e8 03 00 aa	mov	x8, x0
    5848:	0c 04 45 f8	ldr	x12, [x0], #0x50
    584c:	8d 55 08 53	ubfx	w13, w12, #8, #14
    5850:	bf 01 09 6b	cmp	w13, w9
    5854:	80 06 00 54	b.eq	0x5924
    5858:	8d 55 08 53	ubfx	w13, w12, #8, #14
    585c:	6d 03 00 34	cbz	w13, 0x58c8
    5860:	bf 01 0b 6b	cmp	w13, w11
    5864:	c0 02 00 54	b.eq	0x58bc
    5868:	8d 8d 56 d3	ubfx	x13, x12, #22, #14
    586c:	bf 01 09 6b	cmp	w13, w9
    5870:	20 03 00 54	b.eq	0x58d4
    5874:	ad 03 00 34	cbz	w13, 0x58e8
    5878:	bf 01 0b 6b	cmp	w13, w11
    587c:	00 03 00 54	b.eq	0x58dc
    5880:	8d c5 64 d3	ubfx	x13, x12, #36, #14
    5884:	bf 01 09 6b	cmp	w13, w9
    5888:	60 03 00 54	b.eq	0x58f4
    588c:	4d 04 00 34	cbz	w13, 0x5914
    5890:	bf 01 0b 6b	cmp	w13, w11
    5894:	40 03 00 54	b.eq	0x58fc
    5898:	8c fd 72 d3	lsr	x12, x12, #50
    589c:	3f 01 0c 6b	cmp	w9, w12
    58a0:	00 04 00 54	b.eq	0x5920
    58a4:	ec 04 00 34	cbz	w12, 0x5940
    58a8:	9f 01 0b 6b	cmp	w12, w11
    58ac:	c1 fc ff 54	b.ne	0x5844
    58b0:	09 80 00 d1	sub	x9, x0, #0x20
    58b4:	0a 08 80 52	mov	w10, #0x40
    58b8:	13 00 00 14	b	0x5904
    58bc:	0a 02 80 52	mov	w10, #0x10
    58c0:	e9 03 08 aa	mov	x9, x8
    58c4:	10 00 00 14	b	0x5904
    58c8:	0a 02 80 52	mov	w10, #0x10
    58cc:	e9 03 08 aa	mov	x9, x8
    58d0:	1e 00 00 14	b	0x5948
    58d4:	0a 04 80 52	mov	w10, #0x20
    58d8:	13 00 00 14	b	0x5924
    58dc:	09 41 00 91	add	x9, x8, #0x10
    58e0:	0a 04 80 52	mov	w10, #0x20
    58e4:	08 00 00 14	b	0x5904
    58e8:	09 41 00 91	add	x9, x8, #0x10
    58ec:	0a 04 80 52	mov	w10, #0x20
    58f0:	16 00 00 14	b	0x5948
    58f4:	0a 06 80 52	mov	w10, #0x30
    58f8:	0b 00 00 14	b	0x5924
    58fc:	09 81 00 91	add	x9, x8, #0x20
    5900:	0a 06 80 52	mov	w10, #0x30
    5904:	05 69 6a f8	ldr	x5, [x8, x10]
    5908:	20 81 00 91	add	x0, x9, #0x20
    590c:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    5910:	a0 00 1f d6	br	x5
    5914:	09 81 00 91	add	x9, x8, #0x20
    5918:	0a 06 80 52	mov	w10, #0x30
    591c:	0b 00 00 14	b	0x5948
    5920:	0a 08 80 52	mov	w10, #0x40
    5924:	00 69 6a f8	ldr	x0, [x8, x10]
    5928:	08 04 41 f8	ldr	x8, [x0], #0x10
    592c:	09 00 00 90	adrp	x9, _process.fullCheck@PAGE
    5930:	29 01 00 91	add	x9, x9, _process.fullCheck@PAGEOFF
    5934:	5f 04 7b f2	tst	x2, #0x60
    5938:	05 01 89 9a	csel	x5, x8, x9, eq
    593c:	05 00 00 14	b	0x5950
    5940:	09 80 00 d1	sub	x9, x0, #0x20
    5944:	0a 08 80 52	mov	w10, #0x40
    5948:	05 69 6a f8	ldr	x5, [x8, x10]
    594c:	20 81 00 91	add	x0, x9, #0x20
    5950:	21 20 00 91	add	x1, x1, #0x8
    5954:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    5958:	a0 00 1f d6	br	x5
    595c:	89 02 80 52	mov	w9, #0x14
    5960:	b7 ff ff 17	b	0x583c
