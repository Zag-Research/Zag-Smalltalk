"
I am a unary input constraint used to mark a variable that the client
wishes to change.
"
Class {
	#name : 'BeDBEditConstraint',
	#superclass : 'BeDBUnaryConstraint',
	#category : 'Benchmarks-DeltaBlue',
	#package : 'Benchmarks',
	#tag : 'DeltaBlue'
}

{ #category : 'instance creation' }
BeDBEditConstraint class >> var: aVariable strength: strengthSymbol [
	"Install an edit constraint with the given strength on the given
	 variable."

	^(self new) var: aVariable strength: strengthSymbol
]

{ #category : 'execution' }
BeDBEditConstraint >> execute [
	"Edit constraints do nothing."
]

{ #category : 'queries' }
BeDBEditConstraint >> isInput [
	"I indicate that a variable is to be changed by imperative code."

	^true
]
