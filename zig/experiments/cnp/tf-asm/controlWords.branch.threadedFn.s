; symbol: _controlWords.branch.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.branch.threadedFn:
    57c8:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    57cc:	fd 03 00 91	mov	x29, sp
    57d0:	00 00 40 f9	ldr	x0, [x0]
    57d4:	08 04 41 f8	ldr	x8, [x0], #0x10
    57d8:	09 00 00 90	adrp	x9, _process.fullCheck@PAGE
    57dc:	29 01 00 91	add	x9, x9, _process.fullCheck@PAGEOFF
    57e0:	5f 04 7b f2	tst	x2, #0x60
    57e4:	05 01 89 9a	csel	x5, x8, x9, eq
    57e8:	42 14 82 9a	cinc	x2, x2, eq
    57ec:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    57f0:	a0 00 1f d6	br	x5
