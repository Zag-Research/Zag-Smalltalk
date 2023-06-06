"
I create an array from a sequence of expressions { axp1 . exp2 }
"
Class {
	#name : #ASArray,
	#superclass : #ASExpression,
	#instVars : [
		'statements'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASArray class >> statements: aCollection [ 
	^ self new statements: aCollection
]

{ #category : #comparing }
ASArray >> = other [

	^ self class = other class and: [ 
			  self statements = other statements ]
]

{ #category : #printing }
ASArray >> baseValue [
   | bv |
	bv := OrderedCollection new.
	statements 
		do: [ :statement | bv add: statement baseValue].
	^ bv asArray
]

{ #category : #printing }
ASArray >> printOn: s [
	s << self class name << ' statements: {'.
	statements do: [: statement| statement printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #accessing }
ASArray >> statements [

	^ statements
]

{ #category : #accessing }
ASArray >> statements: aCollection [ 
	statements := aCollection
]
