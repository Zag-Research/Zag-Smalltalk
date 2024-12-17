"
I am the master benchmark class

Asking me to run `Benchmark run` will run all of my subclasses in a standard execution, outputing statistics on Transcript.
"
Class {
	#name : 'Benchmark',
	#superclass : 'Object',
	#classInstVars : [
		'timings'
	],
	#category : 'Benchmarks-Kernel',
	#package : 'Benchmarks',
	#tag : 'Kernel'
}

{ #category : 'running' }
Benchmark class >> results [

	^ timings
]

{ #category : 'running' }
Benchmark class >> run [

	timings := (1 to: 10) collect: [: unused |
		self new run
		]
]

{ #category : 'benchmarking' }
Benchmark >> benchmark [  

	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
Benchmark >> innerBenchmarkLoop: innerIterations [ 
      1 to: innerIterations do: [:i |
        (self verifyResult: self benchmark) ifFalse: [ ^ false ].
      ].
      ^ true
]

{ #category : 'running' }
Benchmark >> run [

	^ [self assert: [ self innerBenchmarkLoop: self iterations ]] timeToRunWithoutGC
]

{ #category : 'benchmarking' }
Benchmark >> verifyResult: result [

	^ self subclassResponsibility
]
