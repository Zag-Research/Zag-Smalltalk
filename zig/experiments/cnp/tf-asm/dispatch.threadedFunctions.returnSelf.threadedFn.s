; symbol: _dispatch.threadedFunctions.returnSelf.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_dispatch.threadedFunctions.returnSelf.threadedFn:
    5f84:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    5f88:	fd 03 00 91	mov	x29, sp
    5f8c:	89 fc 70 d3	lsr	x9, x4, #48
    5f90:	68 00 01 ca	eor	x8, x3, x1
    5f94:	09 01 00 b4	cbz	x9, 0x5fb4
    5f98:	2a cc 74 92	and	x10, x1, #0xfffffffffffff000
    5f9c:	21 01 0a aa	orr	x1, x9, x10
    5fa0:	a1 00 00 b4	cbz	x1, 0x5fb4
    5fa4:	1f fd 3f f1	cmp	x8, #0xfff
    5fa8:	a9 02 00 54	b.ls	0x5ffc
    5fac:	68 18 40 f9	ldr	x8, [x3, #0x30]
    5fb0:	14 00 00 14	b	0x6000
    5fb4:	1f fd 3f f1	cmp	x8, #0xfff
    5fb8:	c8 02 00 54	b.hi	0x6010
    5fbc:	68 04 40 f9	ldr	x8, [x3, #0x8]
    5fc0:	48 03 00 b4	cbz	x8, 0x6028
    5fc4:	69 00 40 f9	ldr	x9, [x3]
    5fc8:	29 21 7d 92	and	x9, x9, #0xff8
    5fcc:	69 0c 09 8b	add	x9, x3, x9, lsl #3
    5fd0:	0a 00 00 90	adrp	x10, _process.fullCheck@PAGE
    5fd4:	4a 01 00 91	add	x10, x10, _process.fullCheck@PAGEOFF
    5fd8:	5f 04 7b f2	tst	x2, #0x60
    5fdc:	00 2d 42 a9	ldp	x0, x11, [x8, #0x20]
    5fe0:	65 01 8a 9a	csel	x5, x11, x10, eq
    5fe4:	0a 19 40 f9	ldr	x10, [x8, #0x30]
    5fe8:	21 01 01 91	add	x1, x9, #0x40
    5fec:	44 b1 7d 92	and	x4, x10, #0xfffffffffff8
    5ff0:	e3 03 08 aa	mov	x3, x8
    5ff4:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    5ff8:	a0 00 1f d6	br	x5
    5ffc:	68 e0 00 91	add	x8, x3, #0x38
    6000:	60 14 42 a9	ldp	x0, x5, [x3, #0x20]
    6004:	04 b1 7d 92	and	x4, x8, #0xfffffffffff8
    6008:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    600c:	a0 00 1f d6	br	x5
    6010:	00 00 00 90	adrp	x0, ___anon_8291@PAGE
    6014:	00 00 00 91	add	x0, x0, ___anon_8291@PAGEOFF
    6018:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    601c:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    6020:	41 01 80 52	mov	w1, #0xa
    6024:	00 00 00 94	bl	_debug.defaultPanic
    6028:	00 00 00 90	adrp	x0, ___anon_8284@PAGE
    602c:	00 00 00 91	add	x0, x0, ___anon_8284@PAGEOFF
    6030:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    6034:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    6038:	c1 00 80 52	mov	w1, #0x6
    603c:	00 00 00 94	bl	_debug.defaultPanic
