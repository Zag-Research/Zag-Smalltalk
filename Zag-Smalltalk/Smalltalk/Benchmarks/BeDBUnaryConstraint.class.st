"
I am an abstract superclass for constraints having a single possible output
variable.

Instance variables:
	output		possible output variable <BeDBVariable>
	satisfied		true if I am currently satisfied <Boolean>
"
Class {
	#name : 'BeDBUnaryConstraint',
	#superclass : 'BeDBAbstractConstraint',
	#instVars : [
		'output',
		'satisfied'
	],
	#category : 'Benchmarks-DeltaBlue',
	#package : 'Benchmarks',
	#tag : 'DeltaBlue'
}

{ #category : 'add/remove' }
BeDBUnaryConstraint >> addToGraph [
	"Add myself to the constraint graph."

	output addConstraint: self.
	satisfied := false.
]

{ #category : 'planning' }
BeDBUnaryConstraint >> chooseMethod: mark [
	"Decide if I can be satisfied and record that decision."

	satisfied :=
		(output mark ~= mark) and:
		[strength stronger: output walkStrength].
]

{ #category : 'planning' }
BeDBUnaryConstraint >> execute [
	"Enforce this constraint. Assume that it is satisfied."

	self subclassResponsibility
]

{ #category : 'planning' }
BeDBUnaryConstraint >> inputsDo: aBlock [
	"I have no input variables."
]

{ #category : 'queries' }
BeDBUnaryConstraint >> isSatisfied [
	"Answer true if this constraint is satisfied in the current solution."

	^satisfied
]

{ #category : 'planning' }
BeDBUnaryConstraint >> markUnsatisfied [
	"Record the fact that I am unsatisfied."

	satisfied := false.
]

{ #category : 'planning' }
BeDBUnaryConstraint >> output [
	"Answer my current output variable."

	^output
]

{ #category : 'planning' }
BeDBUnaryConstraint >> recalculate [
	"Calculate the walkabout strength, the stay flag, and, if it is 'stay',
	 the value for the current output of this constraint. Assume this
	 constraint is satisfied."

	output walkStrength: strength.
	output stay: (self isInput not).
	(output stay) ifTrue: [self execute].	"stay optimization"
]

{ #category : 'add/remove' }
BeDBUnaryConstraint >> removeFromGraph [
	"Remove myself from the constraint graph."

	(output == nil) ifFalse: [output removeConstraint: self].
	satisfied := false.
]

{ #category : 'initialize-release' }
BeDBUnaryConstraint >> var: aVariable strength: strengthSymbol [
	"Initialize myself with the given variable and strength."

	strength := BeDBStrength of: strengthSymbol.
	output := aVariable.
	satisfied := false.
	self addConstraint.
]
