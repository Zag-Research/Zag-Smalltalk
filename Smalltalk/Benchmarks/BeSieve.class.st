"
Classic Sieve of Eratosthenes banchmark
"
Class {
	#name : 'BeSieve',
	#superclass : 'Benchmark',
	#category : 'Benchmarks-Simple',
	#package : 'Benchmarks',
	#tag : 'Simple'
}

{ #category : 'benchmarking' }
BeSieve >> benchmark [
    | flags |
    flags  := Array new: 5000 withAll: true.
    ^ self sieve: flags size: 5000.

]

{ #category : 'accessing' }
BeSieve >> iterations [

	^ 10000
]

{ #category : 'benchmarking' }
BeSieve >> sieve: flags size: size [
    | primeCount |
    primeCount := 0.

    2 to: size do: [ :i |
      (flags at: i - 1)
        ifTrue: [
          | k |
          primeCount := primeCount + 1.
          k := i + i.
          [ k <= size ]
            whileTrue: [
              flags at: k - 1 put: false.
              k := k + i ]. ] ].
    ^ primeCount

]

{ #category : 'benchmarking' }
BeSieve >> verifyResult: result [
    ^ 669 = result
]
