; symbol: _controlWords.popAssociationValue.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.popAssociationValue.threadedFn:
    5bb0:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    5bb4:	fd 03 00 91	mov	x29, sp
    5bb8:	08 00 40 f9	ldr	x8, [x0]
    5bbc:	09 09 40 92	and	x9, x8, #0x7
    5bc0:	1f 01 00 f1	cmp	x8, #0x0
    5bc4:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    5bc8:	61 00 00 54	b.ne	0x5bd4
    5bcc:	29 00 40 f9	ldr	x9, [x1]
    5bd0:	09 09 00 f9	str	x9, [x8, #0x10]
    5bd4:	05 08 40 f9	ldr	x5, [x0, #0x10]
    5bd8:	00 80 00 91	add	x0, x0, #0x20
    5bdc:	21 20 00 91	add	x1, x1, #0x8
    5be0:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    5be4:	a0 00 1f d6	br	x5
