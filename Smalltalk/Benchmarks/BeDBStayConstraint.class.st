"
I mark variables that should, with some level of preference, stay the same.
I have one method with zero inputs and one output, which does nothing. Planners
may exploit the fact that, if I am satisfied, my output will not change during
plan execution. This is called ""stay optimization.""
"
Class {
	#name : 'BeDBStayConstraint',
	#superclass : 'BeDBUnaryConstraint',
	#category : 'Benchmarks-DeltaBlue',
	#package : 'Benchmarks',
	#tag : 'DeltaBlue'
}

{ #category : 'instance creation' }
BeDBStayConstraint class >> var: aVariable strength: strengthSymbol [
	"Install a stay constraint with the given strength on the given variable."

	^(self new) var: aVariable strength: strengthSymbol
]

{ #category : 'execution' }
BeDBStayConstraint >> execute [
	"Stay constraints do nothing."
]
