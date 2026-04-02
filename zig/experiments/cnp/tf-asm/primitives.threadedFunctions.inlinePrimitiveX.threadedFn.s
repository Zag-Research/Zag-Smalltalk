; symbol: _primitives.threadedFunctions.inlinePrimitiveX.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.inlinePrimitiveX.threadedFn:
    728c:	ff 43 02 d1	sub	sp, sp, #0x90
    7290:	f8 5f 05 a9	stp	x24, x23, [sp, #0x50]
    7294:	f6 57 06 a9	stp	x22, x21, [sp, #0x60]
    7298:	f4 4f 07 a9	stp	x20, x19, [sp, #0x70]
    729c:	fd 7b 08 a9	stp	x29, x30, [sp, #0x80]
    72a0:	fd 03 02 91	add	x29, sp, #0x80
    72a4:	f3 03 04 aa	mov	x19, x4
    72a8:	f4 03 03 aa	mov	x20, x3
    72ac:	f5 03 02 aa	mov	x21, x2
    72b0:	f6 03 01 aa	mov	x22, x1
    72b4:	f7 03 00 aa	mov	x23, x0
    72b8:	01 1c 40 39	ldrb	w1, [x0, #0x7]
    72bc:	e0 03 00 91	mov	x0, sp
    72c0:	00 00 00 94	bl	_primitives.Module.findNumberedPrimitive
    72c4:	e8 03 41 39	ldrb	w8, [sp, #0x40]
    72c8:	48 02 00 34	cbz	w8, 0x7310
    72cc:	e5 13 40 f9	ldr	x5, [sp, #0x20]
    72d0:	05 02 00 b4	cbz	x5, 0x7310
    72d4:	68 00 80 52	mov	w8, #0x3
    72d8:	a8 83 1c 38	sturb	w8, [x29, #-0x38]
    72dc:	e5 02 1f f8	stur	x5, [x23, #-0x10]
    72e0:	e8 82 1f 38	sturb	w8, [x23, #-0x8]
    72e4:	e0 03 17 aa	mov	x0, x23
    72e8:	e1 03 16 aa	mov	x1, x22
    72ec:	e2 03 15 aa	mov	x2, x21
    72f0:	e3 03 14 aa	mov	x3, x20
    72f4:	e4 03 13 aa	mov	x4, x19
    72f8:	fd 7b 48 a9	ldp	x29, x30, [sp, #0x80]
    72fc:	f4 4f 47 a9	ldp	x20, x19, [sp, #0x70]
    7300:	f6 57 46 a9	ldp	x22, x21, [sp, #0x60]
    7304:	f8 5f 45 a9	ldp	x24, x23, [sp, #0x50]
    7308:	ff 43 02 91	add	sp, sp, #0x90
    730c:	a0 00 1f d6	br	x5
    7310:	00 00 00 90	adrp	x0, ___anon_8733@PAGE
    7314:	00 00 00 91	add	x0, x0, ___anon_8733@PAGEOFF
    7318:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    731c:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    7320:	c1 03 80 52	mov	w1, #0x1e
    7324:	00 00 00 94	bl	_debug.defaultPanic
