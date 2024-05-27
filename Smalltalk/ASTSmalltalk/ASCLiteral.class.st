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

{ #category : 'printing' }
ASCLiteral >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: literal.
	aStream << $)
]

{ #category : 'code generation' }
ASCLiteral >> outputTo: output [ 
	^ output visitPushLiteral: literal
]

{ #category : 'stack analysis' }
ASCLiteral >> stackOp: output [

	^ output stackPushLiteral: self
]

{ #category : 'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
