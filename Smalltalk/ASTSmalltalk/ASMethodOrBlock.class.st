"
I contain all the common respersentation for Methods and Blocks
"
Class {
	#name : 'ASMethodOrBlock',
	#superclass : 'ASCodeBlock',
	#instVars : [
		'arguments'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'tests - instance creation' }
ASMethodOrBlock class >> arguments: args locals: locals body: body [

	^ (self locals: locals
		  body: body)
		  arguments: args
		  
]

{ #category : 'comparing' }
ASMethodOrBlock >> = other [

	^ super = other and: [ 
		  self arguments = other arguments  ]
]

{ #category : 'accessing' }
ASMethodOrBlock >> arguments [
	^ arguments
]

{ #category : 'accessing' }
ASMethodOrBlock >> arguments: aCollection [

	arguments := aCollection asArray
]

{ #category : 'printing' }
ASMethodOrBlock >> compileMethod: aCompiledMethod [ 
| method |
	method := (ASPharoVisitor compiler: self) visit: aCompiledMethod ast.
	method instructions:  (aCompiledMethod ir accept: (ASPharoVisitor compiler: self)).
	^ method
]

{ #category : 'initialization' }
ASMethodOrBlock >> initialize [

	super initialize.
	arguments := #(  )
]

{ #category : 'printing' }
ASMethodOrBlock >> inspectZagASTOn: aStream [

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

{ #category : 'printing' }
ASMethodOrBlock >> printOn: s [

	arguments isEmpty ifFalse: [
		s << ' arguments: #('.
		arguments do: [ :arg | s << arg  ] separatedBy: [ s << ' ' ].
		s << ')' ].
	super printOn: s
]

{ #category : 'accessing' }
ASMethodOrBlock >> selectorWithArgs [
self subclassResponsibility 
]
