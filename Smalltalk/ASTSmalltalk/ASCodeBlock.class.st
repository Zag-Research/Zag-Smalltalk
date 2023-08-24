"
I represent a block of code.

"
Class {
	#name : #ASCodeBlock,
	#superclass : #Object,
	#instVars : [
		'nestedScopeOf',
		'body',
		'locals',
		'scope'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASCodeBlock >> = other [

	^ self class = other class and: [ 
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
	locals := #(  ).
	body := OrderedCollection new.
	scope := Dictionary new
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

{ #category : #scopes }
ASCodeBlock >> lookup: symbol [
	^ scope at: symbol ifAbsent: [ nestedScopeOf lookup: symbol ]
]

{ #category : #scopes }
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
	body do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #accessing }
ASCodeBlock >> scope [
	^ scope
]
