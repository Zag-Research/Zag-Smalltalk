"
I represent a block of code.

"
Class {
	#name : 'ASCodeBlock',
	#superclass : 'AST',
	#instVars : [
		'body',
		'locals',
		'arguments',
		'scope',
		'enclosingScope'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASCodeBlock class >> locals: locals body: body [

	^ self new
		  locals: locals;
		  body: body
]

{ #category : 'comparing' }
ASCodeBlock >> = other [

	^ self class = other class and: [
		  locals = other locals and: [ body = other body and: [ arguments = other arguments ] ] ]
]

{ #category : 'accessing' }
ASCodeBlock >> addStatement: anASStatement [

	body add: anASStatement
]

{ #category : 'adding' }
ASCodeBlock >> addToScope: aName [

	scope add: aName
]

{ #category : 'accessing' }
ASCodeBlock >> arguments [
	^ arguments
]

{ #category : 'accessing' }
ASCodeBlock >> arguments: aCollection [

	arguments := aCollection asArray
]

{ #category : 'accessing' }
ASCodeBlock >> body [
	^ body
]

{ #category : 'accessing' }
ASCodeBlock >> body: aCollection [

	body := aCollection asASSequence
]

{ #category : 'accessing' }
ASCodeBlock >> children [

	^ OrderedCollection newFrom: body
]

{ #category : 'initialization' }
ASCodeBlock >> initialize [

	super initialize.
	locals := #(  ).
	body := ASSequence new.
	arguments := #(  ).
	scope := OrderedCollection new
]

{ #category : 'printing' }
ASCodeBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	
	aStream nextPut: $(.
	self selectorWithArgs ifNotEmpty: [ :s | aStream print: s ].
	locals ifNotEmpty: [
		aStream space.
		aStream << #|.
		aStream space.
		locals do: [ :local |
			aStream << local.
			aStream space ].
		aStream << #| ].

	aStream nextPut: $)
]

{ #category : 'testing' }
ASCodeBlock >> isASRef [
	^false
]

{ #category : 'testing' }
ASCodeBlock >> isASVariable [
	^ false
]

{ #category : 'accessing' }
ASCodeBlock >> locals [

	^ locals
]

{ #category : 'accessing' }
ASCodeBlock >> locals: aCollection [

	locals := aCollection asArray
]

{ #category : 'as yet unclassified' }
ASCodeBlock >> nestedScopeOf: anASClassNode [ 

	enclosingScope := anASClassNode
]

{ #category : 'accessing' }
ASCodeBlock >> roots [
	| roots |
	roots := OrderedCollection new.
	roots add: self.
	^ roots
]

{ #category : 'accessing' }
ASCodeBlock >> selectorWithArgs [
self subclassResponsibility 
]
