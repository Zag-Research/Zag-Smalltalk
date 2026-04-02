; symbol: _controlWords.pushLiteral.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.pushLiteral.threadedFn:
    5dc4:	ff 43 01 d1	sub	sp, sp, #0x50
    5dc8:	f6 57 02 a9	stp	x22, x21, [sp, #0x20]
    5dcc:	f4 4f 03 a9	stp	x20, x19, [sp, #0x30]
    5dd0:	fd 7b 04 a9	stp	x29, x30, [sp, #0x40]
    5dd4:	fd 03 01 91	add	x29, sp, #0x40
    5dd8:	14 00 40 f9	ldr	x20, [x0]
    5ddc:	28 20 00 d1	sub	x8, x1, #0x8
    5de0:	1f 21 7d f2	tst	x8, #0xff8
    5de4:	40 01 00 54	b.eq	0x5e0c
    5de8:	14 01 00 f9	str	x20, [x8]
    5dec:	05 08 40 f9	ldr	x5, [x0, #0x10]
    5df0:	00 80 00 91	add	x0, x0, #0x20
    5df4:	e1 03 08 aa	mov	x1, x8
    5df8:	fd 7b 44 a9	ldp	x29, x30, [sp, #0x40]
    5dfc:	f4 4f 43 a9	ldp	x20, x19, [sp, #0x30]
    5e00:	f6 57 42 a9	ldp	x22, x21, [sp, #0x20]
    5e04:	ff 43 01 91	add	sp, sp, #0x50
    5e08:	a0 00 1f d6	br	x5
    5e0c:	f5 03 00 aa	mov	x21, x0
    5e10:	e0 23 00 91	add	x0, sp, #0x8
    5e14:	f3 03 02 aa	mov	x19, x2
    5e18:	e2 03 03 aa	mov	x2, x3
    5e1c:	e3 03 04 aa	mov	x3, x4
    5e20:	00 00 00 94	bl	_process.Stack.spillStack
    5e24:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    5e28:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    5e2c:	34 8c 1f f8	str	x20, [x1, #-0x8]!
    5e30:	a5 0a 40 f9	ldr	x5, [x21, #0x10]
    5e34:	a0 82 00 91	add	x0, x21, #0x20
    5e38:	e2 03 13 aa	mov	x2, x19
    5e3c:	ef ff ff 17	b	0x5df8
