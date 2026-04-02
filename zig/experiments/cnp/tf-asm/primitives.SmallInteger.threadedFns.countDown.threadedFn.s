; symbol: _primitives.SmallInteger.threadedFns.countDown.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.SmallInteger.threadedFns.countDown.threadedFn:
    76f4:	ff 83 01 d1	sub	sp, sp, #0x60
    76f8:	f6 57 03 a9	stp	x22, x21, [sp, #0x30]
    76fc:	f4 4f 04 a9	stp	x20, x19, [sp, #0x40]
    7700:	fd 7b 05 a9	stp	x29, x30, [sp, #0x50]
    7704:	fd 43 01 91	add	x29, sp, #0x50
    7708:	28 00 40 f9	ldr	x8, [x1]
    770c:	09 1d 40 92	and	x9, x8, #0xff
    7710:	3f a5 02 f1	cmp	x9, #0xa9
    7714:	e1 00 00 54	b.ne	0x7730
    7718:	08 dd 78 92	and	x8, x8, #0xffffffffffffff00
    771c:	08 01 04 f1	subs	x8, x8, #0x100
    7720:	e9 77 9f 1a	cset	w9, vs
    7724:	e9 43 00 39	strb	w9, [sp, #0x10]
    7728:	e9 3f 00 39	strb	w9, [sp, #0xf]
    772c:	a7 01 00 54	b.vc	0x7760
    7730:	35 0d 80 52	mov	w21, #0x69
    7734:	28 20 00 d1	sub	x8, x1, #0x8
    7738:	1f 21 7d f2	tst	x8, #0xff8
    773c:	60 02 00 54	b.eq	0x7788
    7740:	15 01 00 f9	str	x21, [x8]
    7744:	05 04 41 f8	ldr	x5, [x0], #0x10
    7748:	e1 03 08 aa	mov	x1, x8
    774c:	fd 7b 45 a9	ldp	x29, x30, [sp, #0x50]
    7750:	f4 4f 44 a9	ldp	x20, x19, [sp, #0x40]
    7754:	f6 57 43 a9	ldp	x22, x21, [sp, #0x30]
    7758:	ff 83 01 91	add	sp, sp, #0x60
    775c:	a0 00 1f d6	br	x5
    7760:	29 15 80 52	mov	w9, #0xa9
    7764:	09 01 09 aa	orr	x9, x8, x9
    7768:	29 00 00 f9	str	x9, [x1]
    776c:	29 0d 80 52	mov	w9, #0x69
    7770:	2a 0c 80 52	mov	w10, #0x61
    7774:	1f 01 00 f1	cmp	x8, #0x0
    7778:	55 c1 89 9a	csel	x21, x10, x9, gt
    777c:	28 20 00 d1	sub	x8, x1, #0x8
    7780:	1f 21 7d f2	tst	x8, #0xff8
    7784:	e1 fd ff 54	b.ne	0x7740
    7788:	f3 03 00 aa	mov	x19, x0
    778c:	e0 63 00 91	add	x0, sp, #0x18
    7790:	f4 03 02 aa	mov	x20, x2
    7794:	e2 03 03 aa	mov	x2, x3
    7798:	e3 03 04 aa	mov	x3, x4
    779c:	00 00 00 94	bl	_process.Stack.spillStack
    77a0:	e1 8f 41 a9	ldp	x1, x3, [sp, #0x18]
    77a4:	35 8c 1f f8	str	x21, [x1, #-0x8]!
    77a8:	65 06 41 f8	ldr	x5, [x19], #0x10
    77ac:	e4 17 40 f9	ldr	x4, [sp, #0x28]
    77b0:	e0 03 13 aa	mov	x0, x19
    77b4:	e2 03 14 aa	mov	x2, x20
    77b8:	e5 ff ff 17	b	0x774c
