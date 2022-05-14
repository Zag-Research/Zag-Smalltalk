"
I am the only non-expression statement type
"
Class {
	#name : #ASReturn,
	#superclass : #ASStatement,
	#instVars : [
		'expression'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASReturn >> = other [

	^ self class = other class and: [ self expression = other expression ]
]

{ #category : #accessing }
ASReturn >> expression [

	^ expression
]

{ #category : #accessing }
ASReturn >> expression: anASNode [
	expression := anASNode
]

{ #category : #testing }
ASReturn >> isReturn [
	^ true
]

{ #category : #printing }
ASReturn >> printOn: s [

	s << self className << ' new expression: ('.
	expression printOn: s.
	s << ')'
]
