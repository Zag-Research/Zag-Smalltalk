; symbol: _process.threadedFunctions.pushThisProcess.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_process.threadedFunctions.pushThisProcess.threadedFn:
    74c0:	21 20 00 d1	sub	x1, x1, #0x8
    74c4:	3f 20 7d f2	tst	x1, #0xff8
    74c8:	a0 00 00 54	b.eq	0x74dc
    74cc:	48 c8 73 92	and	x8, x2, #0xffffffffffffe000
    74d0:	28 00 00 f9	str	x8, [x1]
    74d4:	05 04 41 f8	ldr	x5, [x0], #0x10
    74d8:	a0 00 1f d6	br	x5
    74dc:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    74e0:	fd 03 00 91	mov	x29, sp
    74e4:	00 00 00 90	adrp	x0, ___anon_8775@PAGE
    74e8:	00 00 00 91	add	x0, x0, ___anon_8775@PAGEOFF
    74ec:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    74f0:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    74f4:	a1 01 80 52	mov	w1, #0xd
    74f8:	00 00 00 94	bl	_debug.defaultPanic
