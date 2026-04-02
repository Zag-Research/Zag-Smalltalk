; symbol: _controlWords.pushAssociationValue.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_controlWords.pushAssociationValue.threadedFn:
    5d24:	ff 43 01 d1	sub	sp, sp, #0x50
    5d28:	f6 57 02 a9	stp	x22, x21, [sp, #0x20]
    5d2c:	f4 4f 03 a9	stp	x20, x19, [sp, #0x30]
    5d30:	fd 7b 04 a9	stp	x29, x30, [sp, #0x40]
    5d34:	fd 03 01 91	add	x29, sp, #0x40
    5d38:	08 00 40 f9	ldr	x8, [x0]
    5d3c:	09 09 40 92	and	x9, x8, #0x7
    5d40:	1f 01 00 f1	cmp	x8, #0x0
    5d44:	20 19 40 fa	ccmp	x9, #0x0, #0x0, ne
    5d48:	c0 01 00 54	b.eq	0x5d80
    5d4c:	14 00 80 d2	mov	x20, #0x0
    5d50:	28 20 00 d1	sub	x8, x1, #0x8
    5d54:	1f 21 7d f2	tst	x8, #0xff8
    5d58:	c0 01 00 54	b.eq	0x5d90
    5d5c:	14 01 00 f9	str	x20, [x8]
    5d60:	05 08 40 f9	ldr	x5, [x0, #0x10]
    5d64:	00 80 00 91	add	x0, x0, #0x20
    5d68:	e1 03 08 aa	mov	x1, x8
    5d6c:	fd 7b 44 a9	ldp	x29, x30, [sp, #0x40]
    5d70:	f4 4f 43 a9	ldp	x20, x19, [sp, #0x30]
    5d74:	f6 57 42 a9	ldp	x22, x21, [sp, #0x20]
    5d78:	ff 43 01 91	add	sp, sp, #0x50
    5d7c:	a0 00 1f d6	br	x5
    5d80:	14 09 40 f9	ldr	x20, [x8, #0x10]
    5d84:	28 20 00 d1	sub	x8, x1, #0x8
    5d88:	1f 21 7d f2	tst	x8, #0xff8
    5d8c:	81 fe ff 54	b.ne	0x5d5c
    5d90:	f5 03 00 aa	mov	x21, x0
    5d94:	e0 23 00 91	add	x0, sp, #0x8
    5d98:	f3 03 02 aa	mov	x19, x2
    5d9c:	e2 03 03 aa	mov	x2, x3
    5da0:	e3 03 04 aa	mov	x3, x4
    5da4:	00 00 00 94	bl	_process.Stack.spillStack
    5da8:	e1 8f 40 a9	ldp	x1, x3, [sp, #0x8]
    5dac:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    5db0:	34 8c 1f f8	str	x20, [x1, #-0x8]!
    5db4:	a5 0a 40 f9	ldr	x5, [x21, #0x10]
    5db8:	a0 82 00 91	add	x0, x21, #0x20
    5dbc:	e2 03 13 aa	mov	x2, x19
    5dc0:	eb ff ff 17	b	0x5d6c
