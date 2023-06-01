"
I am an abstract superclass for constraints having a single possible output
variable.

Instance variables:
	output		possible output variable <Variable>
	satisfied		true if I am currently satisfied <Boolean>
"
Class {
	#name : #PjDbUnaryConstraint,
	#superclass : #PjDbAbstractConstraint,
	#instVars : [
		'output',
		'satisfied'
	],
	#category : #'Zag-Benchmarking-DeltaBlue-Constraints'
}

{ #category : #testing }
PjDbUnaryConstraint class >> isAbstract [

	^ self = PjDbUnaryConstraint
]

{ #category : #'add/remove' }
PjDbUnaryConstraint >> addToGraph [
	"Add myself to the constraint graph."

	output addConstraint: self.
	satisfied := false.
]

{ #category : #planning }
PjDbUnaryConstraint >> chooseMethod: mark [
	"Decide if I can be satisfied and record that decision."

	satisfied :=
		(output mark ~= mark) and:
		[strength stronger: output walkStrength].
]

{ #category : #planning }
PjDbUnaryConstraint >> execute [
	"Enforce this constraint. Assume that it is satisfied."

	self subclassResponsibility
]

{ #category : #planning }
PjDbUnaryConstraint >> inputsDo: aBlock [
	"I have no input variables."
]

{ #category : #queries }
PjDbUnaryConstraint >> isSatisfied [
	"Answer true if this constraint is satisfied in the current solution."

	^satisfied
]

{ #category : #planning }
PjDbUnaryConstraint >> markUnsatisfied [
	"Record the fact that I am unsatisfied."

	satisfied := false.
]

{ #category : #planning }
PjDbUnaryConstraint >> output [
	"Answer my current output variable."

	^output
]

{ #category : #planning }
PjDbUnaryConstraint >> recalculate [
	"Calculate the walkabout strength, the stay flag, and, if it is 'stay',
	 the value for the current output of this constraint. Assume this
	 constraint is satisfied."

	output walkStrength: strength.
	output stay: (self isInput not).
	(output stay) ifTrue: [self execute].	"stay optimization"
]

{ #category : #'add/remove' }
PjDbUnaryConstraint >> removeFromGraph [
	"Remove myself from the constraint graph."

	(output == nil) ifFalse: [output removeConstraint: self].
	satisfied := false.
]

{ #category : #'initialize-release' }
PjDbUnaryConstraint >> var: aVariable strength: strengthSymbol [
	"Initialize myself with the given variable and strength."

	strength := PjDbStrength of: strengthSymbol.
	output := aVariable.
	satisfied := false.
	self addConstraint.
]
