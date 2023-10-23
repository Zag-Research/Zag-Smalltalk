"
I am an abstract class for local operations
"
Class {
	#name : #ASCVariableOperations,
	#superclass : #ASCOperations,
	#instVars : [
		'variable'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCVariableOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : #printing }
ASCVariableOperations >> printOn: aStream [

	super printOn: aStream.
	aStream nextPut: $(.
	variable printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'instance creation' }
ASCVariableOperations >> with: anInteger [ 
	variable := anInteger
]
