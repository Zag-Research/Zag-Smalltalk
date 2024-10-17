"
I am an abstract class for local operations
"
Class {
	#name : 'ASCVariableOperations',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'variableLocation'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCVariableOperations >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream tab.
	aStream << variableLocation name
	
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
ASCVariableOperations >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCVariableOperations >> with: ascVariable [
	variableLocation := ascVariable
]
