; symbol: _primitives.BlockClosure.threadedFns.returnLiteralClosure.threadedFn
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_primitives.BlockClosure.threadedFns.returnLiteralClosure.threadedFn:
    8050:	ff c3 01 d1	sub	sp, sp, #0x70
    8054:	f8 5f 03 a9	stp	x24, x23, [sp, #0x30]
    8058:	f6 57 04 a9	stp	x22, x21, [sp, #0x40]
    805c:	f4 4f 05 a9	stp	x20, x19, [sp, #0x50]
    8060:	fd 7b 06 a9	stp	x29, x30, [sp, #0x60]
    8064:	fd 83 01 91	add	x29, sp, #0x60
    8068:	e8 03 03 aa	mov	x8, x3
    806c:	f3 03 02 aa	mov	x19, x2
    8070:	e2 03 01 aa	mov	x2, x1
    8074:	f5 03 00 aa	mov	x21, x0
    8078:	14 80 00 91	add	x20, x0, #0x20
    807c:	36 03 80 52	mov	w22, #0x19
    8080:	37 00 80 52	mov	w23, #0x1
    8084:	38 04 80 52	mov	w24, #0x21
    8088:	89 fc 70 d3	lsr	x9, x4, #48
    808c:	89 01 00 b5	cbnz	x9, 0x80bc
    8090:	0d 00 00 14	b	0x80c4
    8094:	e0 83 00 91	add	x0, sp, #0x20
    8098:	e1 03 08 aa	mov	x1, x8
    809c:	00 00 00 94	bl	_context.push
    80a0:	e2 23 42 a9	ldp	x2, x8, [sp, #0x20]
    80a4:	09 e1 00 91	add	x9, x8, #0x38
    80a8:	24 b1 7d 92	and	x4, x9, #0xfffffffffff8
    80ac:	a9 0a 40 f9	ldr	x9, [x21, #0x10]
    80b0:	14 25 02 a9	stp	x20, x9, [x8, #0x20]
    80b4:	89 fc 70 d3	lsr	x9, x4, #48
    80b8:	69 00 00 b4	cbz	x9, 0x80c4
    80bc:	83 bc 40 92	and	x3, x4, #0xffffffffffff
    80c0:	a3 fe ff b5	cbnz	x3, 0x8094
    80c4:	a9 02 40 f9	ldr	x9, [x21]
    80c8:	2a 1d 40 92	and	x10, x9, #0xff
    80cc:	5f a5 02 f1	cmp	x10, #0xa9
    80d0:	21 01 00 54	b.ne	0x80f4
    80d4:	2a fd 48 93	asr	x10, x9, #8
    80d8:	5f 81 2a eb	cmp	x10, w10, sxtb
    80dc:	e1 06 00 54	b.ne	0x81b8
    80e0:	0a dd 78 d3	lsl	x10, x8, #8
    80e4:	4a b1 75 92	and	x10, x10, #0xfffffffffff800
    80e8:	2a 3d 48 b3	bfxil	x10, x9, #8, #8
    80ec:	c9 22 0a aa	orr	x9, x22, x10, lsl #8
    80f0:	1a 00 00 14	b	0x8158
    80f4:	2a 09 00 12	and	w10, w9, #0x7
    80f8:	5f 05 00 71	cmp	w10, #0x1
    80fc:	80 01 00 54	b.eq	0x812c
    8100:	ca 05 00 35	cbnz	w10, 0x81b8
    8104:	09 01 00 b4	cbz	x9, 0x8124
    8108:	29 01 40 79	ldrh	w9, [x9]
    810c:	2a 3d 00 12	and	w10, w9, #0xffff
    8110:	4b 31 00 51	sub	w11, w10, #0xc
    8114:	7f 09 00 71	cmp	w11, #0x2
    8118:	43 01 00 54	b.lo	0x8140
    811c:	5f 51 00 71	cmp	w10, #0x14
    8120:	c1 04 00 54	b.ne	0x81b8
    8124:	09 43 08 aa	orr	x9, x24, x8, lsl #16
    8128:	0c 00 00 14	b	0x8158
    812c:	29 1d 03 53	ubfx	w9, w9, #3, #5
    8130:	2a 3d 00 12	and	w10, w9, #0xffff
    8134:	4b 31 00 51	sub	w11, w10, #0xc
    8138:	7f 09 00 71	cmp	w11, #0x2
    813c:	02 ff ff 54	b.hs	0x811c
    8140:	e9 0e 09 2a	orr	w9, w23, w9, lsl #3
    8144:	29 3d 40 92	and	x9, x9, #0xffff
    8148:	0a dd 78 d3	lsl	x10, x8, #8
    814c:	4a b1 75 92	and	x10, x10, #0xfffffffffff800
    8150:	49 01 09 8b	add	x9, x10, x9
    8154:	09 23 09 aa	orr	x9, x24, x9, lsl #8
    8158:	41 20 00 d1	sub	x1, x2, #0x8
    815c:	3f 20 7d f2	tst	x1, #0xff8
    8160:	61 01 00 54	b.ne	0x818c
    8164:	e0 23 00 91	add	x0, sp, #0x8
    8168:	e1 03 02 aa	mov	x1, x2
    816c:	e2 03 08 aa	mov	x2, x8
    8170:	e3 03 04 aa	mov	x3, x4
    8174:	00 00 00 94	bl	_process.Stack.spillStack
    8178:	e2 a3 40 a9	ldp	x2, x8, [sp, #0x8]
    817c:	e4 0f 40 f9	ldr	x4, [sp, #0x18]
    8180:	89 fc 70 d3	lsr	x9, x4, #48
    8184:	c9 f9 ff b5	cbnz	x9, 0x80bc
    8188:	cf ff ff 17	b	0x80c4
    818c:	29 00 00 f9	str	x9, [x1]
    8190:	a5 0a 40 f9	ldr	x5, [x21, #0x10]
    8194:	e0 03 14 aa	mov	x0, x20
    8198:	e2 03 13 aa	mov	x2, x19
    819c:	e3 03 08 aa	mov	x3, x8
    81a0:	fd 7b 46 a9	ldp	x29, x30, [sp, #0x60]
    81a4:	f4 4f 45 a9	ldp	x20, x19, [sp, #0x50]
    81a8:	f6 57 44 a9	ldp	x22, x21, [sp, #0x40]
    81ac:	f8 5f 43 a9	ldp	x24, x23, [sp, #0x30]
    81b0:	ff c3 01 91	add	sp, sp, #0x70
    81b4:	a0 00 1f d6	br	x5
    81b8:	00 00 00 90	adrp	x0, ___anon_9243@PAGE
    81bc:	00 00 00 91	add	x0, x0, ___anon_9243@PAGEOFF
    81c0:	02 00 00 90	adrp	x2, l___unnamed_18@PAGE
    81c4:	42 00 00 91	add	x2, x2, l___unnamed_18@PAGEOFF
    81c8:	41 03 80 52	mov	w1, #0x1a
    81cc:	00 00 00 94	bl	_debug.defaultPanic
