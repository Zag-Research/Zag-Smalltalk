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
ASCVariableOperations >> inspectStringOn: aStream [

	super inspectStringOn: aStream.
	aStream nextPut: $(.
	aStream nextPutAll: variable name.
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
