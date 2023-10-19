"
I represent a global (most likely a class)
"
Class {
	#name : #ASGlobalVariable,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #compiling }
ASGlobalVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitGlobal: self
]

{ #category : #compiling }
ASGlobalVariable >> tell: aCompiler youAreSent: aSymbol [

	aCompiler sent: aSymbol toGlobal: name
]
