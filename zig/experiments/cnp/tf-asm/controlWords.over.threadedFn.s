; symbol: _controlWords.over.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.over.threadedFn:
    59f4:	ff 43 01 d1	sub	sp, sp, #0x50
    59f8:	f6 57 02 a9	stp	x22, x21, [sp, #0x20]
    59fc:	f4 4f 03 a9	stp	x20, x19, [sp, #0x30]
    5a00:	fd 7b 04 a9	stp	x29, x30, [sp, #0x40]
    5a04:	fd 03 01 91	add	x29, sp, #0x40
    5a08:	35 04 40 f9	ldr	x21, [x1, #0x8]
    5a0c:	28 20 00 d1	sub	x8, x1, #0x8
    5a10:	1f 21 7d f2	tst	x8, #0xff8
    5a14:	20 01 00 54	b.eq	0x5a38
    5a18:	15 01 00 f9	str	x21, [x8]
    5a1c:	05 04 41 f8	ldr	x5, [x0], #0x10
    5a20:	e1 03 08 aa	mov	x1, x8
    5a24:	fd 7b 44 a9	ldp	x29, x30, [sp, #0x40]
    5a28:	f4 4f 43 a9	ldp	x20, x19, [sp, #0x30]
    5a2c:	f6 57 42 a9	ldp	x22, x21, [sp, #0x20]
    5a30:	ff 43 01 91	add	sp, sp, #0x50
    5a34:	a0 00 1f d6	br	x5
    5a38:	f3 03 00 aa	mov	x19, x0
    5a3c:	e0 23 00 91	add	x0, sp, #0x8
    5a40:	f4 03 02 aa	mov	x20, x2
    5a44:	e2 03 03 aa	mov	x2, x3
    5a48:	e3 03 04 aa	mov	x3, x4
    5a4c:	00 00 00 94	bl	_process.Stack.spillStack
    5a50:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    5a54:	35 8c 1f f8	str	x21, [x1, #-0x8]!
    5a58:	65 06 41 f8	ldr	x5, [x19], #0x10
    5a5c:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    5a60:	e0 03 13 aa	mov	x0, x19
    5a64:	e2 03 14 aa	mov	x2, x20
    5a68:	ef ff ff 17	b	0x5a24
