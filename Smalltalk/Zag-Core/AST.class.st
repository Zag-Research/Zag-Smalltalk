"
I am the superclass of all Zag AST Nodes
"
Class {
	#name : 'AST',
	#superclass : 'Object',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
AST class >> with: anObject [

	^ self new with: anObject
]

{ #category : 'visiting' }
AST >> accept: aVisitor [

	^ self subclassResponsibility
]

{ #category : 'converting' }
AST >> asAST [
]

{ #category : 'converting' }
AST >> asPharoAST [
	"Converts me to the associated Pharo AST."
	self subclassResponsibility.
]

{ #category : 'printing' }
AST >> printOn: aStream [

	ZagSmalltalk new stream: aStream; visit: self
	
]

{ #category : 'executing' }
AST >> runAsPharo [
	"Convert me to a Pharo AST, then run me using the Opal Compiler."
	| pharoAST compiler |
	pharoAST := self asPharoAST.
	compiler := OpalCompiler new.
	compiler ast: pharoAST.
	compiler evaluate.
]
