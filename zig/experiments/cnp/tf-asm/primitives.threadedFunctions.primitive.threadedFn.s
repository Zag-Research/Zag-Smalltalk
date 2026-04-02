; symbol: _primitives.threadedFunctions.primitive.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.primitive.threadedFn:
    6634:	ff 43 02 d1	sub	sp, sp, #0x90
    6638:	f8 5f 05 a9	stp	x24, x23, [sp, #0x50]
    663c:	f6 57 06 a9	stp	x22, x21, [sp, #0x60]
    6640:	f4 4f 07 a9	stp	x20, x19, [sp, #0x70]
    6644:	fd 7b 08 a9	stp	x29, x30, [sp, #0x80]
    6648:	fd 03 02 91	add	x29, sp, #0x80
    664c:	f3 03 04 aa	mov	x19, x4
    6650:	f4 03 00 aa	mov	x20, x0
    6654:	24 02 f8 b7	tbnz	x4, #0x3f, 0x6698
    6658:	f5 03 01 aa	mov	x21, x1
    665c:	f6 03 02 aa	mov	x22, x2
    6660:	f7 03 03 aa	mov	x23, x3
    6664:	81 1e 40 39	ldrb	w1, [x20, #0x7]
    6668:	68 be 40 92	and	x8, x19, #0xffffffffffff
    666c:	69 fe 70 d3	lsr	x9, x19, #48
    6670:	3f 01 00 f1	cmp	x9, #0x0
    6674:	18 11 9f 9a	csel	x24, x8, xzr, ne
    6678:	e0 23 00 91	add	x0, sp, #0x8
    667c:	00 00 00 94	bl	_primitives.Module.findNumberedPrimitive
    6680:	e8 23 41 39	ldrb	w8, [sp, #0x48]
    6684:	28 01 00 34	cbz	w8, 0x66a8
    6688:	e5 0f 40 f9	ldr	x5, [sp, #0x18]
    668c:	25 01 00 b5	cbnz	x5, 0x66b0
    6690:	05 0f 40 f9	ldr	x5, [x24, #0x18]
    6694:	08 00 00 14	b	0x66b4
    6698:	85 0a 40 f9	ldr	x5, [x20, #0x10]
    669c:	80 82 00 91	add	x0, x20, #0x20
    66a0:	64 fa 40 92	and	x4, x19, #0x7fffffffffffffff
    66a4:	09 00 00 14	b	0x66c8
    66a8:	05 00 00 90	adrp	x5, "_primitives.Object.basicAt:.primitive"@PAGE
    66ac:	a5 00 00 91	add	x5, x5, "_primitives.Object.basicAt:.primitive"@PAGEOFF
    66b0:	05 97 01 a9	stp	x5, x5, [x24, #0x18]
    66b4:	e3 03 17 aa	mov	x3, x23
    66b8:	e2 03 16 aa	mov	x2, x22
    66bc:	e1 03 15 aa	mov	x1, x21
    66c0:	e0 03 14 aa	mov	x0, x20
    66c4:	e4 03 13 aa	mov	x4, x19
    66c8:	fd 7b 48 a9	ldp	x29, x30, [sp, #0x80]
    66cc:	f4 4f 47 a9	ldp	x20, x19, [sp, #0x70]
    66d0:	f6 57 46 a9	ldp	x22, x21, [sp, #0x60]
    66d4:	f8 5f 45 a9	ldp	x24, x23, [sp, #0x50]
    66d8:	ff 43 02 91	add	sp, sp, #0x90
    66dc:	a0 00 1f d6	br	x5
