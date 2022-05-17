"
I am a cascade send. I take a target expression and a list of ASSends without targets.
"
Class {
	#name : #ASCascade,
	#superclass : #ASExpression,
	#instVars : [
		'target',
		'sends'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASCascade class >> target: t sends: s [
	^ self new target: t; sends: s
]

{ #category : #comparing }
ASCascade >> = other [

	^ self class = other class and: [ 
		  self target = other target and: [ 
			  self sends = other sends ] ]
]

{ #category : #printing }
ASCascade >> printOn: s [

	s << self className << ' target: ('.
	target printOn: s.
	s << ') sends: {'.
	sends do: [ :send | send printOn: s ] separatedBy: [ s << '.' ].
	s << '}'
]

{ #category : #parsing }
ASCascade >> sends [

	^ sends
]

{ #category : #parsing }
ASCascade >> sends: aCollection [

	sends := aCollection
]

{ #category : #parsing }
ASCascade >> target [

	^ target
]

{ #category : #parsing }
ASCascade >> target: expression [

	target := expression 
]
