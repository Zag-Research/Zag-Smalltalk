"
I am a unary input constraint used to mark a variable that the client
wishes to change.
"
Class {
	#name : #PjDbEditConstraint,
	#superclass : #PjDbUnaryConstraint,
	#category : #'Zag-Benchmarking-DeltaBlue-Constraints'
}

{ #category : #'instance creation' }
PjDbEditConstraint class >> var: aVariable strength: strengthSymbol [
	"Install an edit constraint with the given strength on the given
	 variable."

	^(self new) var: aVariable strength: strengthSymbol
]

{ #category : #execution }
PjDbEditConstraint >> execute [
	"Edit constraints do nothing."
]

{ #category : #queries }
PjDbEditConstraint >> isInput [
	"I indicate that a variable is to be changed by imperative code."

	^true
]
