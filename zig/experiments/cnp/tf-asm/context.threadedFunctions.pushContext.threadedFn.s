; symbol: _context.threadedFunctions.pushContext.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_context.threadedFunctions.pushContext.threadedFn:
    73c4:	88 fc 70 d3	lsr	x8, x4, #48
    73c8:	28 03 00 b4	cbz	x8, 0x742c
    73cc:	88 bc 40 92	and	x8, x4, #0xffffffffffff
    73d0:	e8 02 00 b4	cbz	x8, 0x742c
    73d4:	ff c3 00 d1	sub	sp, sp, #0x30
    73d8:	f4 4f 01 a9	stp	x20, x19, [sp, #0x10]
    73dc:	fd 7b 02 a9	stp	x29, x30, [sp, #0x20]
    73e0:	fd 83 00 91	add	x29, sp, #0x20
    73e4:	f3 03 00 aa	mov	x19, x0
    73e8:	e0 03 00 91	mov	x0, sp
    73ec:	e9 03 01 aa	mov	x9, x1
    73f0:	e1 03 03 aa	mov	x1, x3
    73f4:	f4 03 02 aa	mov	x20, x2
    73f8:	e2 03 09 aa	mov	x2, x9
    73fc:	e3 03 08 aa	mov	x3, x8
    7400:	00 00 00 94	bl	_context.push
    7404:	e1 0f 40 a9	ldp	x1, x3, [sp]
    7408:	65 06 41 f8	ldr	x5, [x19], #0x10
    740c:	68 e0 00 91	add	x8, x3, #0x38
    7410:	04 b1 7d 92	and	x4, x8, #0xfffffffffff8
    7414:	e0 03 13 aa	mov	x0, x19
    7418:	e2 03 14 aa	mov	x2, x20
    741c:	fd 7b 42 a9	ldp	x29, x30, [sp, #0x20]
    7420:	f4 4f 41 a9	ldp	x20, x19, [sp, #0x10]
    7424:	ff c3 00 91	add	sp, sp, #0x30
    7428:	a0 00 1f d6	br	x5
    742c:	05 04 41 f8	ldr	x5, [x0], #0x10
    7430:	a0 00 1f d6	br	x5
