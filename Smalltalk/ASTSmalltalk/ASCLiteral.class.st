"
I remember literal values to output.
"
Class {
	#name : #ASCLiteral,
	#superclass : #ASCOperations,
	#instVars : [
		'literal'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCLiteral >> outputTo: output [ 
	output visitPushLiteral: literal
]

{ #category : #printing }
ASCLiteral >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: literal asString;
		nextPut: $)
]

{ #category : #'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
