; symbol: _primitives.threadedFunctions.primitiveModuleError.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.threadedFunctions.primitiveModuleError.threadedFn:
    7150:	ff 43 03 d1	sub	sp, sp, #0xd0
    7154:	fa 67 08 a9	stp	x26, x25, [sp, #0x80]
    7158:	f8 5f 09 a9	stp	x24, x23, [sp, #0x90]
    715c:	f6 57 0a a9	stp	x22, x21, [sp, #0xa0]
    7160:	f4 4f 0b a9	stp	x20, x19, [sp, #0xb0]
    7164:	fd 7b 0c a9	stp	x29, x30, [sp, #0xc0]
    7168:	fd 03 03 91	add	x29, sp, #0xc0
    716c:	f3 03 04 aa	mov	x19, x4
    7170:	f7 03 01 aa	mov	x23, x1
    7174:	f4 03 00 aa	mov	x20, x0
    7178:	e4 02 f8 b7	tbnz	x4, #0x3f, 0x71d4
    717c:	f6 03 02 aa	mov	x22, x2
    7180:	f5 03 03 aa	mov	x21, x3
    7184:	81 02 40 f9	ldr	x1, [x20]
    7188:	e0 23 00 91	add	x0, sp, #0x8
    718c:	00 00 00 94	bl	_object.ObjectFunctions.arrayAsSlice__anon_8605
    7190:	e8 33 40 79	ldrh	w8, [sp, #0x18]
    7194:	f8 07 40 f9	ldr	x24, [sp, #0x8]
    7198:	1f 01 00 71	cmp	w8, #0x0
    719c:	04 0b 40 fa	ccmp	x24, #0x0, #0x4, eq
    71a0:	41 02 00 54	b.ne	0x71e8
    71a4:	68 be 40 92	and	x8, x19, #0xffffffffffff
    71a8:	69 fe 70 d3	lsr	x9, x19, #48
    71ac:	3f 01 00 f1	cmp	x9, #0x0
    71b0:	08 11 9f 9a	csel	x8, x8, xzr, ne
    71b4:	09 00 00 90	adrp	x9, _primitives.noPrimWithError@PAGE
    71b8:	29 01 00 91	add	x9, x9, _primitives.noPrimWithError@PAGEOFF
    71bc:	09 a5 01 a9	stp	x9, x9, [x8, #0x18]
    71c0:	e1 22 00 d1	sub	x1, x23, #0x8
    71c4:	3f 20 7d f2	tst	x1, #0xff8
    71c8:	80 04 00 54	b.eq	0x7258
    71cc:	3f 00 00 f9	str	xzr, [x1]
    71d0:	23 00 00 14	b	0x725c
    71d4:	85 12 40 f9	ldr	x5, [x20, #0x20]
    71d8:	80 c2 00 91	add	x0, x20, #0x30
    71dc:	64 fa 40 92	and	x4, x19, #0x7fffffffffffffff
    71e0:	e1 03 17 aa	mov	x1, x23
    71e4:	23 00 00 14	b	0x7270
    71e8:	f9 0b 40 f9	ldr	x25, [sp, #0x10]
    71ec:	81 0a 40 f9	ldr	x1, [x20, #0x10]
    71f0:	e0 83 00 91	add	x0, sp, #0x20
    71f4:	00 00 00 94	bl	_object.ObjectFunctions.arrayAsSlice__anon_8605
    71f8:	e8 63 40 79	ldrh	w8, [sp, #0x30]
    71fc:	48 fd ff 35	cbnz	w8, 0x71a4
    7200:	e1 13 40 f9	ldr	x1, [sp, #0x20]
    7204:	01 fd ff b4	cbz	x1, 0x71a4
    7208:	e2 17 40 f9	ldr	x2, [sp, #0x28]
    720c:	e0 e3 00 91	add	x0, sp, #0x38
    7210:	e3 03 18 aa	mov	x3, x24
    7214:	e4 03 19 aa	mov	x4, x25
    7218:	00 00 00 94	bl	_primitives.Module.findNamedPrimitive
    721c:	e8 e3 41 39	ldrb	w8, [sp, #0x78]
    7220:	28 fc ff 34	cbz	w8, 0x71a4
    7224:	e5 2b 40 f9	ldr	x5, [sp, #0x50]
    7228:	e5 fb ff b4	cbz	x5, 0x71a4
    722c:	68 be 40 92	and	x8, x19, #0xffffffffffff
    7230:	69 fe 70 d3	lsr	x9, x19, #48
    7234:	3f 01 00 f1	cmp	x9, #0x0
    7238:	08 11 9f 9a	csel	x8, x8, xzr, ne
    723c:	05 95 01 a9	stp	x5, x5, [x8, #0x18]
    7240:	e0 03 14 aa	mov	x0, x20
    7244:	e1 03 17 aa	mov	x1, x23
    7248:	e2 03 16 aa	mov	x2, x22
    724c:	e3 03 15 aa	mov	x3, x21
    7250:	e4 03 13 aa	mov	x4, x19
    7254:	07 00 00 14	b	0x7270
    7258:	01 00 80 d2	mov	x1, #0x0
    725c:	85 02 5f f8	ldur	x5, [x20, #-0x10]
    7260:	64 02 41 b2	orr	x4, x19, #0x8000000000000000
    7264:	e0 03 14 aa	mov	x0, x20
    7268:	e2 03 16 aa	mov	x2, x22
    726c:	e3 03 15 aa	mov	x3, x21
    7270:	fd 7b 4c a9	ldp	x29, x30, [sp, #0xc0]
    7274:	f4 4f 4b a9	ldp	x20, x19, [sp, #0xb0]
    7278:	f6 57 4a a9	ldp	x22, x21, [sp, #0xa0]
    727c:	f8 5f 49 a9	ldp	x24, x23, [sp, #0x90]
    7280:	fa 67 48 a9	ldp	x26, x25, [sp, #0x80]
    7284:	ff 43 03 91	add	sp, sp, #0xd0
    7288:	a0 00 1f d6	br	x5
