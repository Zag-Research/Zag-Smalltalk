"
I am an abstract class for local operations
"
Class {
	#name : 'ASCVariableOperations',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'variableLocation'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'printing' }
ASCVariableOperations >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream tab.
	aStream << 'into: '.
	aStream << variableLocation name
]

{ #category : 'testing' }
ASCVariableOperations >> needsContext: aStackContext [
	" this should be removed when reifyLocals is to work "

	^ self needsLocals
]

{ #category : 'testing' }
ASCVariableOperations >> needsLocals [

	^ true
]

{ #category : 'code generation' }
ASCVariableOperations >> outputTo: unknown [
self subclassResponsibility 
]

{ #category : 'finalAnalysis' }
ASCVariableOperations >> variableReferences: ignore modifies: modified block: aDispatchableBlock [

	modified
		at: variableLocation
		ifPresent: [ :bag | bag add: aDispatchableBlock ]
		ifAbsentPut: [ IdentityBag with: aDispatchableBlock ]
]

{ #category : 'instance creation' }
ASCVariableOperations >> with: ascVariable [
	variableLocation := ascVariable
]
