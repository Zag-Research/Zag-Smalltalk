; symbol: _dispatch.threadedFunctions.returnTop.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_dispatch.threadedFunctions.returnTop.threadedFn:
    6040:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    6044:	fd 03 00 91	mov	x29, sp
    6048:	2a 00 40 f9	ldr	x10, [x1]
    604c:	88 fc 70 d3	lsr	x8, x4, #48
    6050:	68 01 00 b4	cbz	x8, 0x607c
    6054:	29 cc 74 92	and	x9, x1, #0xfffffffffffff000
    6058:	08 01 09 aa	orr	x8, x8, x9
    605c:	08 01 00 b4	cbz	x8, 0x607c
    6060:	0a 01 00 f9	str	x10, [x8]
    6064:	60 14 42 a9	ldp	x0, x5, [x3, #0x20]
    6068:	69 00 01 ca	eor	x9, x3, x1
    606c:	3f fd 3f f1	cmp	x9, #0xfff
    6070:	49 02 00 54	b.ls	0x60b8
    6074:	69 18 40 f9	ldr	x9, [x3, #0x30]
    6078:	11 00 00 14	b	0x60bc
    607c:	68 00 01 ca	eor	x8, x3, x1
    6080:	1f fd 3f f1	cmp	x8, #0xfff
    6084:	88 03 00 54	b.hi	0x60f4
    6088:	68 04 40 f9	ldr	x8, [x3, #0x8]
    608c:	08 04 00 b4	cbz	x8, 0x610c
    6090:	69 00 40 f9	ldr	x9, [x3]
    6094:	29 21 7d 92	and	x9, x9, #0xff8
    6098:	69 0c 09 8b	add	x9, x3, x9, lsl #3
    609c:	2a 01 04 f8	stur	x10, [x9, #0x40]
    60a0:	00 29 42 a9	ldp	x0, x10, [x8, #0x20]
    60a4:	0b 01 01 ca	eor	x11, x8, x1
    60a8:	7f fd 3f f1	cmp	x11, #0xfff
    60ac:	09 01 00 54	b.ls	0x60cc
    60b0:	0b 19 40 f9	ldr	x11, [x8, #0x30]
    60b4:	07 00 00 14	b	0x60d0
    60b8:	69 e0 00 91	add	x9, x3, #0x38
    60bc:	24 b1 7d 92	and	x4, x9, #0xfffffffffff8
    60c0:	e1 03 08 aa	mov	x1, x8
    60c4:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    60c8:	a0 00 1f d6	br	x5
    60cc:	0b e1 00 91	add	x11, x8, #0x38
    60d0:	0c 00 00 90	adrp	x12, _process.fullCheck@PAGE
    60d4:	8c 01 00 91	add	x12, x12, _process.fullCheck@PAGEOFF
    60d8:	5f 04 7b f2	tst	x2, #0x60
    60dc:	45 01 8c 9a	csel	x5, x10, x12, eq
    60e0:	21 01 01 91	add	x1, x9, #0x40
    60e4:	64 b1 7d 92	and	x4, x11, #0xfffffffffff8
    60e8:	e3 03 08 aa	mov	x3, x8
    60ec:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    60f0:	a0 00 1f d6	br	x5
    60f4:	00 00 00 90	adrp	x0, ___anon_8291@PAGE
    60f8:	00 00 00 91	add	x0, x0, ___anon_8291@PAGEOFF
    60fc:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    6100:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    6104:	41 01 80 52	mov	w1, #0xa
    6108:	00 00 00 94	bl	_debug.defaultPanic
    610c:	00 00 00 90	adrp	x0, ___anon_8284@PAGE
    6110:	00 00 00 91	add	x0, x0, ___anon_8284@PAGEOFF
    6114:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    6118:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    611c:	c1 00 80 52	mov	w1, #0x6
    6120:	00 00 00 94	bl	_debug.defaultPanic
