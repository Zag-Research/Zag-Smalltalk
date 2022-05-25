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

{ #category : #'instance creation' }
ASReturn class >> expression: anASNode [
^self new expression: anASNode
]

{ #category : #comparing }
ASReturn >> = other [

	^ self class = other class and: [ self expression = other expression ]
]

{ #category : #visiting }
ASReturn >> acceptVisitor: aVisitor [

	^ aVisitor visitReturn: self
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

	s << self className << ' expression: ('.
	expression printOn: s.
	s << ')'
]
