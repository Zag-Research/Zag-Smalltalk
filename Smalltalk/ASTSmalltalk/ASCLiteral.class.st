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

{ #category : #'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
