; symbol: _process.threadedFunctions.debug.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_process.threadedFunctions.debug.threadedFn:
    74fc:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    7500:	fd 03 00 91	mov	x29, sp
    7504:	e8 03 02 aa	mov	x8, x2
    7508:	09 09 82 d2	mov	x9, #0x1048
    750c:	42 00 7a b2	orr	x2, x2, #0x40
    7510:	28 31 40 b3	bfxil	x8, x9, #0, #13
    7514:	29 00 80 52	mov	w9, #0x1
    7518:	09 01 00 39	strb	w9, [x8]
    751c:	05 04 41 f8	ldr	x5, [x0], #0x10
    7520:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    7524:	a0 00 1f d6	br	x5
