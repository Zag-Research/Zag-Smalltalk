"
I represent any literal value.
"
Class {
	#name : #ASLiteral,
	#superclass : #ASExpression,
	#instVars : [
		'literal'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASLiteral >> = other [

	^ self class = other class and: [ self literal = other literal ]
]

{ #category : #accessing }
ASLiteral >> literal [

	^ literal
]

{ #category : #accessing }
ASLiteral >> literal: anInteger [

	literal := anInteger
]

{ #category : #printing }
ASLiteral >> printOn: s [

	s << self className << ' new literal: '.
	literal printOn: s
]
