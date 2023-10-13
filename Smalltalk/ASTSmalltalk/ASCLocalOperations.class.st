"
I am an abstract class for local operations
"
Class {
	#name : #ASCLocalOperations,
	#superclass : #ASCOperations,
	#instVars : [
		'local'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCLocalOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : #'instance creation' }
ASCLocalOperations >> with: anInteger [ 
	local := anInteger
]
