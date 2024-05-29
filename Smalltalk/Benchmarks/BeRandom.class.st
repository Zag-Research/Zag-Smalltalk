"
I generate a random integer sequence based on .....

This is rather than using the system random generator in order to have a predictable sequence for benchmarking.
"
Class {
	#name : 'BeRandom',
	#superclass : 'Object',
	#instVars : [
		'seed'
	],
	#category : 'Benchmarks-Kernel',
	#package : 'Benchmarks',
	#tag : 'Kernel'
}

{ #category : 'initialization' }
BeRandom >> initialize [

	super initialize.
	seed := 74755
]

{ #category : 'initialization' }
BeRandom >> next [

	seed := ((seed * 1309) + 13849) & 65535.
   ^ seed
]
