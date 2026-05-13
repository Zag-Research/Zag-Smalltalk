Spur is the encoding used by [OpenSmalltalkVM](https://github.com/OpenSmalltalk) and was designed by Elliot Miranda and Clement Bera. They tagged only a few kinds of immediate values because their philosophy is that most objects should be in memory (i.e. heap).

Spur uses the [following format](https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/):
![[Pasted image 20240115082827.png]]
This provides 61-bit SmallInteger and immediate floats that have an exponent range equivalent to 32-bit floats (approximately 1e-77 to 1e77) with the full 64-bit float mantissa. Determining which of the 4 types is a simple bit-test. Decoding the float is a test for the zero case and then a shift, add a 64-bit constant, and a rotate.
