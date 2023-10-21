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

{ #category : #printing }
ASCVariableOperations >> inspectString: aStream [

	super inspectString: aStream.
	aStream nextPut: $(.
	variable printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'code generation' }
ASCVariableOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : #'instance creation' }
ASCVariableOperations >> with: anInteger [ 
	variable := anInteger
]
