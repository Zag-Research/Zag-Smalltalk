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

{ #category : #printing }
ASCLiteral >> inspectString: aStream [

	super inspectString: aStream.
	aStream
		nextPut: $(;
		nextPutAll: literal asString;
		nextPut: $)
]

{ #category : #'code generation' }
ASCLiteral >> outputTo: output [ 
	output visitPushLiteral: literal
]

{ #category : #'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
