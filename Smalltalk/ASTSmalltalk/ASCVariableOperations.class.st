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

{ #category : 'printing' }
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
ASCVariableOperations >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCVariableOperations >> variableReferences: referenced modifies: modified block: aDispatchableBlock [
	modified
		at: variable location
		ifPresent: [ :dic |
			dic at: aDispatchableBlock ifPresent: [ :count |
			dic at: aDispatchableBlock put: count + 1 ] ifAbsentPut: [ 1 ]]
	 	ifAbsentPut: [ Dictionary new at: aDispatchableBlock put: 1; yourself ]
]

{ #category : 'instance creation' }
ASCVariableOperations >> variableUsage: aDispatchableBlock [

	aDispatchableBlock modified: variable
]

{ #category : 'instance creation' }
ASCVariableOperations >> with: ascVariable [
	variable := ascVariable
]
