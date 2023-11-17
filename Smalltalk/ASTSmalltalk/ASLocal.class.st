"
I represent a local (temporary) variable
"
Class {
	#name : 'ASLocal',
	#superclass : 'ASVariable',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'visiting' }
ASLocal >> acceptVisitor: aVisitor [
	^ aVisitor visitNamedAS: self
]

{ #category : 'compiling' }
ASLocal >> lookup: aScope [
	^ aScope lookupLocal: name
]

{ #category : 'affecting' }
ASLocal >> pop: output [

	^ output visitPopLocal: self
]

{ #category : 'affecting' }
ASLocal >> push: output [

	^ output visitPushLocal: self
]

{ #category : 'affecting' }
ASLocal >> store: output [

	^ output visitStoreLocal: self
]
