"
I remember literal values to output.
"
Class {
	#name : 'ASCLiteral',
	#superclass : 'ASCOperations',
	#instVars : [
		'literal'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCLiteral >> outputTo: output [ 
	^ output visitPushLiteral: literal
]

{ #category : 'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]

{ #category : #visitor }
ASCLiteral >> zigAccept: aVisitor [

	^ aVisitor visitLiteral: literal
]
