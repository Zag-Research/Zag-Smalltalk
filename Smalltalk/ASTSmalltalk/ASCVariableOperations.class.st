"
I am an abstract class for local operations
"
Class {
	#name : 'ASCVariableOperations',
	#superclass : 'ASCStackReferencingOperations',
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

{ #category : 'data flow instructions' }
ASCVariableOperations >> markReferences [

	stack markReferenced
]

{ #category : 'code generation' }
ASCVariableOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : 'stack' }
ASCVariableOperations >> resultStack: anASStack [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCVariableOperations >> with: ascVariable [
	variable := ascVariable
]
