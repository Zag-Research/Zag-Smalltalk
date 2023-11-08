"
I represent a block of code.

"
Class {
	#name : #ASCodeBlock,
	#superclass : #Object,
	#instVars : [
		'body',
		'locals',
		'scope',
		'nestedScopeOf'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASCodeBlock >> = other [

	^ self class = other class and: [ 
		locals = other locals and: [ body = other body ] ]
]

{ #category : #visiting }
ASCodeBlock >> acceptVisitor: aVisitor [
	self subclassResponsibility 
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
ASCodeBlock >> body: anASSequence [
	body := anASSequence
]

{ #category : #accessing }
ASCodeBlock >> children [
	| c |
	c := OrderedCollection new.
	c addAll: locals.
	c add: body.
	^ c
]

{ #category : #initialization }
ASCodeBlock >> initialize [

	super initialize.
	locals := #(  ).
	scope := Dictionary new.
	body := ASSequence new
]

{ #category : #testing }
ASCodeBlock >> isASRef [
	^false
]

{ #category : #testing }
ASCodeBlock >> isASVariable [
	^ false
]

{ #category : #accessing }
ASCodeBlock >> locals [

	^ locals
]

{ #category : #accessing }
ASCodeBlock >> locals: aCollection [

	locals := aCollection
		          collect: [ :symbol | 
			          | local |
			          local := ASLocal name: symbol.
			scope at: symbol put: local.
			local ]
		          as: Array
]

{ #category : #initialization }
ASCodeBlock >> lookup: symbol [
	^ scope at: symbol ifAbsent: [ nestedScopeOf lookup: symbol ]
]

{ #category : #initialization }
ASCodeBlock >> nestedScopeOf: anASClassNode [
	nestedScopeOf := anASClassNode
	
]

{ #category : #printing }
ASCodeBlock >> printOn: s [

	locals isEmpty ifFalse: [ 
		s << ' locals: #('.
		locals do: [ :local | s << local name ] separatedBy: [ s << ' ' ].
		s<<')' ].
	s << ' body: {'.
	body printOn: s.
	s << '}'
]

{ #category : #accessing }
ASCodeBlock >> roots [
	| roots |
	roots := OrderedCollection new.
	roots add: self.
	^ roots
]

{ #category : #initialization }
ASCodeBlock >> scope [
 ^ scope
]
