"
I remember literal values to output.
"
Class {
	#name : 'ASCLiteral',
	#superclass : 'ASCNonStackReferencingOperations',
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

{ #category : 'stack' }
ASCLiteral >> resultStack: anASStack basicBlock: aBasicBlock [

	^ anASStack pushDataAsTemporary:
		  (AStackValue fromValue: literal)
]

{ #category : 'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
