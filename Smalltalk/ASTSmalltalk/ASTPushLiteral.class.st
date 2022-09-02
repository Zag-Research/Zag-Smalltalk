Class {
	#name : #ASTPushLiteral,
	#superclass : #Object,
	#instVars : [
		'literal'
	],
	#category : #'ASTSmalltalk-IR'
}

{ #category : #initializing }
ASTPushLiteral >> from: anIRPushLiteral [ 
	literal := anIRPushLiteral literal
]
