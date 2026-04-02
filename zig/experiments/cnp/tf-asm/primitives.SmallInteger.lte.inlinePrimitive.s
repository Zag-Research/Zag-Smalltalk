; symbol: _primitives.SmallInteger.<=.inlinePrimitive
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.SmallInteger.<=.inlinePrimitive:
    769c:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    76a0:	fd 03 00 91	mov	x29, sp
    76a4:	28 04 40 f9	ldr	x8, [x1, #0x8]
    76a8:	09 1d 40 92	and	x9, x8, #0xff
    76ac:	3f a5 02 f1	cmp	x9, #0xa9
    76b0:	c1 01 00 54	b.ne	0x76e8
    76b4:	29 00 40 f9	ldr	x9, [x1]
    76b8:	2a 1d 40 92	and	x10, x9, #0xff
    76bc:	5f a5 02 f1	cmp	x10, #0xa9
    76c0:	41 01 00 54	b.ne	0x76e8
    76c4:	2a 0d 80 52	mov	w10, #0x69
    76c8:	2b 0c 80 52	mov	w11, #0x61
    76cc:	1f 01 09 eb	cmp	x8, x9
    76d0:	68 c1 8a 9a	csel	x8, x11, x10, gt
    76d4:	28 8c 00 f8	str	x8, [x1, #0x8]!
    76d8:	05 10 40 f9	ldr	x5, [x0, #0x20]
    76dc:	00 c0 00 91	add	x0, x0, #0x30
    76e0:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    76e4:	a0 00 1f d6	br	x5
    76e8:	05 04 41 f8	ldr	x5, [x0], #0x10
    76ec:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    76f0:	a0 00 1f d6	br	x5
