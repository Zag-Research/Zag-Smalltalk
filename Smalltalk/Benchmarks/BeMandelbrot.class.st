"
I implement a Mandelbrot benchmark

I test floating point operations (mostly addition and multiplications, with a few divisions) with a triply-nested loop.

Derived from https://github.com/smarr/are-we-fast-yet/blob/master/benchmarks/Smalltalk/Mandelbrot.som
"
Class {
	#name : 'BeMandelbrot',
	#superclass : 'Benchmark',
	#category : 'Benchmarks-Simple',
	#package : 'Benchmarks',
	#tag : 'Simple'
}

{ #category : 'benchmarking' }
BeMandelbrot >> benchmark [

	^ self mandelbrot: self iterations
]

{ #category : 'controlling' }
BeMandelbrot >> innerBenchmarkLoop: innerIterations [
 
   ^ self verify: (self mandelbrot: innerIterations) inner: innerIterations

]

{ #category : 'accessing' }
BeMandelbrot >> iterations [

	^ 750
]

{ #category : 'benchmarking' }
BeMandelbrot >> mandelbrot: size [
      | sum byteAcc bitNum y |
      sum     := 0.
      byteAcc := 0.
      bitNum  := 0.

      y := 0.

      [y < size] whileTrue: [
          | ci x |
          ci := (2.0 * y / size) - 1.0.
          x  := 0.

          [x < size] whileTrue: [
              | zrzr zi zizi cr escape z notDone |
              zrzr := 0.0.
              zizi := zi := 0.0.
              cr   := (2.0 * x / size) - 1.5.

              z := 0.
              notDone := true.
              escape := 0.
              [notDone and: [z < 50]] whileTrue: [
                  | zr |
                  zr := zrzr - zizi + cr.
                  zi := 2.0 * zr * zi + ci.

                  "preserve recalculation"
                  zrzr := zr * zr.
                  zizi := zi * zi.

                  (zrzr + zizi > 4.0) ifTrue: [
                      notDone := false.
                      escape  := 1.
                  ].
                  z := z + 1.
              ].

              byteAcc := (byteAcc << 1) + escape.
              bitNum  := bitNum + 1.

              " Code is very similar for these cases, but using separate blocks
                ensures we skip the shifting when it's unnecessary,
                which is most cases. "
              bitNum = 8
                  ifTrue: [
                    sum := sum bitXor: byteAcc.
                    byteAcc := 0.
                    bitNum  := 0. ]
                  ifFalse: [
                    (x = (size - 1)) ifTrue: [
                        byteAcc := byteAcc << (8 - bitNum).
                        sum := sum bitXor: byteAcc.
                        byteAcc := 0.
                        bitNum  := 0. ]].
              x := x + 1.
          ].
          y := y + 1.
      ].

      ^ sum
]

{ #category : 'verification' }
BeMandelbrot >> verify: result inner: innerIterations [

	innerIterations = 500 ifTrue: [ ^ result = 191 ].
	innerIterations = 750 ifTrue: [ ^ result = 50 ].
	innerIterations = 1000 ifTrue: [ ^ result = 101 ].
	innerIterations = 2500 ifTrue: [ ^ result = 231 ].
	innerIterations = 1 ifTrue: [ ^ result = 128 ].

	Transcript
		crShow: 'No verification result for ';
		crShow: innerIterations asString;
		crShow: ' found'.
	Transcript
		crShow: 'Result is: ';
		crShow: result asString.
		self halt.
	^ false
]

{ #category : 'benchmarking' }
BeMandelbrot >> verifyResult: result [

	^ self verify: result inner: self iterations
]
