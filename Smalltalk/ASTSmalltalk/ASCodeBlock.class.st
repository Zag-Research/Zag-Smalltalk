"
I represent a block of code.

"
Class {
	#name : #ASCodeBlock,
	#superclass : #Object,
	#instVars : [
		'nestedScopeOf',
		'body',
		'locals'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASCodeBlock >> = other [

	self class = other class and: [ 
		locals = other locals and: [ body asArray = other body asArray ] ]
]

{ #category : #accessing }
ASCodeBlock >> addStatement: anASStatement [

	body add: anASStatement
]

{ #category : #accessing }
ASCodeBlock >> body [
	^ body
]

{ #category : #accessing }
ASCodeBlock >> body: anASNode [ 
	body := anASNode
]

{ #category : #initialization }
ASCodeBlock >> initialize [
	super initialize.
		locals := #().
	body := OrderedCollection new
]

{ #category : #accessing }
ASCodeBlock >> locals [

	^ locals
]

{ #category : #accessing }
ASCodeBlock >> locals: aCollection [

	locals := aCollection collect: [: symbol | ASLocal name: symbol] as: Array
]

{ #category : #scopes }
ASCodeBlock >> nestedScopeOf: anASClassNode [ 
	nestedScopeOf := anASClassNode
]

{ #category : #printing }
ASCodeBlock >> printOn: s [

	locals isEmpty ifFalse: [ 
		s << ' locals: '.
		locals printOn: s ].
	s << ' body: {'.
	body do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #accessing }
ASCodeBlock >> scope [
	^ locals
]
