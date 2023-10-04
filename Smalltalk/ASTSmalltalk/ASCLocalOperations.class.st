"
I am an abstract class for local operations
"
Class {
	#name : #ASCLocalOperations,
	#superclass : #ASCOperations,
	#instVars : [
		'localNumber'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCLocalOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : #'instance creation' }
ASCLocalOperations >> with: anInteger [ 
	localNumber := anInteger
]
