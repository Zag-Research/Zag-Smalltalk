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

{ #category : 'printing' }
AST >> printOn: aStream [

	ZagSmalltalk new stream: aStream; visit: self
	
]
