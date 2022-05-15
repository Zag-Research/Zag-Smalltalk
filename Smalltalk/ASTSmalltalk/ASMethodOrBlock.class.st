"
I contain all the common respersentation for Methods and Blocks
"
Class {
	#name : #ASMethodOrBlock,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'nestedScopeOf',
		'body',
		'locals'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASMethodOrBlock >> = other [

	^ self arguments = other arguments and: [ 
		  self locals = other locals and: [ 
			  self body = other body ] ]
]

{ #category : #accessing }
ASMethodOrBlock >> addStatement: anASStatement [

	body add: anASStatement
]

{ #category : #accessing }
ASMethodOrBlock >> arguments [
	^ arguments
]

{ #category : #accessing }
ASMethodOrBlock >> arguments: aCollection [ 
	arguments := aCollection asArray
]

{ #category : #accessing }
ASMethodOrBlock >> body [
	^ body
]

{ #category : #accessing }
ASMethodOrBlock >> body: anASNode [ 
	body := anASNode
]

{ #category : #initialization }
ASMethodOrBlock >> initialize [

	super initialize.
	arguments := #().
	locals := #().
	body := OrderedCollection new
]

{ #category : #accessing }
ASMethodOrBlock >> locals [

	^ locals
]

{ #category : #accessing }
ASMethodOrBlock >> locals: aCollection [

	locals := aCollection asArray
]

{ #category : #accessing }
ASMethodOrBlock >> nestedScopeOf: anASClassNode [ 
	nestedScopeOf := anASClassNode
]

{ #category : #printing }
ASMethodOrBlock >> printOn: s [

	arguments isEmpty ifFalse: [ 
		s << ';arguments: '.
		arguments printOn: s ].
	locals isEmpty ifFalse: [ 
		s << ';locals: '.
		locals printOn: s ].
	s << ';body: {'.
	body do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #accessing }
ASMethodOrBlock >> scope [
	^ locals
]
