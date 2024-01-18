"
I am an abstract class for local operations
"
Class {
	#name : 'ASCVariableOperations',
	#superclass : 'ASCOperations',
	#instVars : [
		'variable'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCVariableOperations >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream tab.
	aStream << variable name
	
]

{ #category : 'code generation' }
ASCVariableOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : 'instance creation' }
ASCVariableOperations >> with: ascVariable [
	variable := ascVariable
]
