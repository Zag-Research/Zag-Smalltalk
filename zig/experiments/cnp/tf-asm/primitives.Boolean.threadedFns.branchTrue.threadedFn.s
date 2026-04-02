; symbol: _primitives.Boolean.threadedFns.branchTrue.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.Boolean.threadedFns.branchTrue.threadedFn:
    8d0c:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
    8d10:	fd 03 00 91	mov	x29, sp
    8d14:	28 00 40 f9	ldr	x8, [x1]
    8d18:	1f a5 01 f1	cmp	x8, #0x69
    8d1c:	01 01 00 54	b.ne	0x8d3c
    8d20:	00 00 40 f9	ldr	x0, [x0]
    8d24:	08 04 41 f8	ldr	x8, [x0], #0x10
    8d28:	09 00 00 90	adrp	x9, _process.fullCheck@PAGE
    8d2c:	29 01 00 91	add	x9, x9, _process.fullCheck@PAGEOFF
    8d30:	5f 04 7b f2	tst	x2, #0x60
    8d34:	05 01 89 9a	csel	x5, x8, x9, eq
    8d38:	03 00 00 14	b	0x8d44
    8d3c:	05 08 40 f9	ldr	x5, [x0, #0x10]
    8d40:	00 80 00 91	add	x0, x0, #0x20
    8d44:	21 20 00 91	add	x1, x1, #0x8
    8d48:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
    8d4c:	a0 00 1f d6	br	x5
