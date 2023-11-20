"
I create an array from a sequence of expressions { axp1 . exp2 }
"
Class {
	#name : #ASArray,
	#superclass : #ASExpression,
	#type : #variable,
	#category : 'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASArray class >> statements: aCollection [

	^ aCollection collect: #yourself as: self
]

{ #category : #comparing }
ASArray >> = other [

	^ self class = other class and: [ 
			  self statements = other statements ]
]

{ #category : #visiting }
ASArray >> acceptVisitor: aVisitor [

	^aVisitor visitArray: self
]

{ #category : #printing }
ASArray >> baseValue [

	| bv |
	bv := OrderedCollection new.
	self do: [ :statement | bv add: statement baseValue ].
	^ bv asArray
]

{ #category : #printing }
ASArray >> do: aBlock [
	1 to: self size do: [ : idx| aBlock value: (self at: idx) ]
]

{ #category : #printing }
ASArray >> printOn: s [

	s << self class name << ' {'.
	self
		do: [ :statement | statement printOn: s ]
		separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #compiling }
ASArray >> zigWalk: aGenerator [

	self do: [ : each| each zigWalk: aGenerator ].
	^ aGenerator pushArray: self size
]
