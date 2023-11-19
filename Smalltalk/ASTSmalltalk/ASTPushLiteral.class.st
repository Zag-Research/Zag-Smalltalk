Class {
	#name : 'ASTPushLiteral',
	#superclass : 'Object',
	#instVars : [
		'literal'
	],
	#category : 'ASTSmalltalk-IR',
	#package : 'ASTSmalltalk',
	#tag : 'IR'
}

{ #category : 'initializing' }
ASTPushLiteral >> from: anIRPushLiteral [ 
	literal := anIRPushLiteral literal
]
