"
I remember the selector for sends
"
Class {
	#name : #ASCSend,
	#superclass : #ASCOperations,
	#instVars : [
		'selector'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCSend >> outputTo: anOutput [ 
	anOutput send: selector 
]

{ #category : #'instance creation' }
ASCSend >> with: aSymbol [ 
	selector := aSymbol
]
