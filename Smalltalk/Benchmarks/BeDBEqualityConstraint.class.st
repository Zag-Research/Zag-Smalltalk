"
I constrain two variables to have the same value: ""v1 = v2"".
"
Class {
	#name : 'BeDBEqualityConstraint',
	#superclass : 'BeDBBinaryConstraint',
	#category : 'Benchmarks-DeltaBlue',
	#package : 'Benchmarks',
	#tag : 'DeltaBlue'
}

{ #category : 'instance creation' }
BeDBEqualityConstraint class >> var: variable1 var: variable2 strength: strengthSymbol [
	"Install a constraint with the given strength equating the given
	 variables."

	^(self new) var: variable1 var: variable2 strength: strengthSymbol
]

{ #category : 'execution' }
BeDBEqualityConstraint >> execute [
	"Enforce this constraint. Assume that it is satisfied."

	(direction == #forward)
		ifTrue: [v2 value: v1 value]
		ifFalse: [v1 value: v2 value].
]
