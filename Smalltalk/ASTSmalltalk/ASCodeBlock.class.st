"
I represent a block of code.

"
Class {
	#name : 'ASCodeBlock',
	#superclass : 'Object',
	#instVars : [
		'body',
		'locals',
		'arguments'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'tests - instance creation' }
ASCodeBlock class >> locals: locals body: body [

	^ self new
		  locals: locals;
		  body: body
]

{ #category : 'comparing' }
ASCodeBlock >> = other [

	^ self class = other class and: [
		  locals = other locals and: [
			  body = other body and: [ self arguments = other arguments ] ] ]
]

{ #category : 'accessing' }
ASCodeBlock >> addStatement: anASStatement [

	body add: anASStatement
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
	arguments := #(  ).
	body := ASSequence new
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

{ #category : 'accessing' }
ASCodeBlock >> locals [

	^ locals
]

{ #category : 'accessing' }
ASCodeBlock >> locals: aCollection [

	locals := aCollection asArray
]

{ #category : 'printing' }
ASCodeBlock >> printOn: s [

	arguments isEmpty ifFalse: [
		s << ' arguments: #('.
		arguments do: [ :arg | s << arg ] separatedBy: [ s << ' ' ].
		s << ')' ].
	locals isEmpty ifFalse: [
		s << ' locals: #('.
		locals do: [ :local | s << local ] separatedBy: [ s << ' ' ].
		s << ')' ].
	s << ' body: {'.
	body printOn: s.
	s << '}'
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

{ #category : 'codegen' }
ASCodeBlock >> zigWalkMethod: aGenerator [

	self halt.
	aGenerator initCompilableUnitBlockFrom: self.
	^ aGenerator walkBody: [
		  body
			  zigWalkEmpty: [ aGenerator returnEmpty ]
			  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
			  last: [ :exp | exp zigWalkFinal: aGenerator ] ]
]
