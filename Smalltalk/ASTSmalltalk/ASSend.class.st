"
I represent an expression that is a message send.
"
Class {
	#name : #ASSend,
	#superclass : #ASExpression,
	#instVars : [
		'target',
		'selector',
		'args'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASSend >> = other [

	^ self class = other class and: [ 
		  self selector = other selector and: [ 
			  self target = other target and: [ self args = other args ] ] ]
]

{ #category : #parsing }
ASSend >> args [
	^ args
]

{ #category : #printing }
ASSend >> printOn: s [

	s << self className << ' new '.
	target ifNotNil: [
		target printOn: s.
		s << '; '].
	s << 'selector: '.
	selector printOn: s.
	s << ' args: {'.
	args do: [: arg | arg printOn: s ] separatedBy: [ s << '.'].
	s << '}'
]

{ #category : #parsing }
ASSend >> selector [
	^ selector
]

{ #category : #parsing }
ASSend >> selector: token2 args: aCollection [

	selector := token2.
	args := aCollection
]

{ #category : #parsing }
ASSend >> target [

	^ target
]

{ #category : #parsing }
ASSend >> target: expression [

	target := expression 
]
