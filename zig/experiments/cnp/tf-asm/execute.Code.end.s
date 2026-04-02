; symbol: _execute.Code.end
; object: /Users/nish7/Code/not-mine/Zag-Smalltalk/zig/zig-out/lib/cnp-fib-bench.o

_execute.Code.end:
       0:	fd 7b bf a9	stp	x29, x30, [sp, #-0x10]!
       4:	fd 03 00 91	mov	x29, sp
       8:	e0 03 01 aa	mov	x0, x1
       c:	48 c8 73 92	and	x8, x2, #0xffffffffffffe000
      10:	03 05 08 f9	str	x3, [x8, #0x1008]
      14:	01 11 08 f9	str	x1, [x8, #0x1020]
      18:	fd 7b c1 a8	ldp	x29, x30, [sp], #0x10
      1c:	c0 03 5f d6	ret
