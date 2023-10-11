"
I represent a local (temporary) variable
"
Class {
	#name : #ASLocal,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASLocal >> acceptVisitor: aVisitor [
	^ aVisitor visitNamedAS: self
]

{ #category : #compiling }
ASLocal >> lookup: aScope [
	^ aScope lookupLocal: name
]
