; symbol: _primitives.threadedFunctions.inlinePrimitiveModule.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.inlinePrimitiveModule.threadedFn:
    7328:	ff 43 02 d1	sub	sp, sp, #0x90
    732c:	f8 5f 05 a9	stp	x24, x23, [sp, #0x50]
    7330:	f6 57 06 a9	stp	x22, x21, [sp, #0x60]
    7334:	f4 4f 07 a9	stp	x20, x19, [sp, #0x70]
    7338:	fd 7b 08 a9	stp	x29, x30, [sp, #0x80]
    733c:	fd 03 02 91	add	x29, sp, #0x80
    7340:	f3 03 04 aa	mov	x19, x4
    7344:	f4 03 03 aa	mov	x20, x3
    7348:	f5 03 02 aa	mov	x21, x2
    734c:	f6 03 01 aa	mov	x22, x1
    7350:	f7 03 00 aa	mov	x23, x0
    7354:	01 1c 40 39	ldrb	w1, [x0, #0x7]
    7358:	e0 03 00 91	mov	x0, sp
    735c:	00 00 00 94	bl	_primitives.Module.findNumberedPrimitive
    7360:	e8 03 41 39	ldrb	w8, [sp, #0x40]
    7364:	48 02 00 34	cbz	w8, 0x73ac
    7368:	e5 13 40 f9	ldr	x5, [sp, #0x20]
    736c:	05 02 00 b4	cbz	x5, 0x73ac
    7370:	68 00 80 52	mov	w8, #0x3
    7374:	a8 83 1c 38	sturb	w8, [x29, #-0x38]
    7378:	e5 02 1f f8	stur	x5, [x23, #-0x10]
    737c:	e8 82 1f 38	sturb	w8, [x23, #-0x8]
    7380:	e0 03 17 aa	mov	x0, x23
    7384:	e1 03 16 aa	mov	x1, x22
    7388:	e2 03 15 aa	mov	x2, x21
    738c:	e3 03 14 aa	mov	x3, x20
    7390:	e4 03 13 aa	mov	x4, x19
    7394:	fd 7b 48 a9	ldp	x29, x30, [sp, #0x80]
    7398:	f4 4f 47 a9	ldp	x20, x19, [sp, #0x70]
    739c:	f6 57 46 a9	ldp	x22, x21, [sp, #0x60]
    73a0:	f8 5f 45 a9	ldp	x24, x23, [sp, #0x50]
    73a4:	ff 43 02 91	add	sp, sp, #0x90
    73a8:	a0 00 1f d6	br	x5
    73ac:	00 00 00 90	adrp	x0, ___anon_8740@PAGE
    73b0:	00 00 00 91	add	x0, x0, ___anon_8740@PAGEOFF
    73b4:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    73b8:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    73bc:	c1 04 80 52	mov	w1, #0x26
    73c0:	00 00 00 94	bl	_debug.defaultPanic
