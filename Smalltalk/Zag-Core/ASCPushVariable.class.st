"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCNonStackReferencingOperations',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#instVars : [
		'stackOffset',
		'variableLocation'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'callgraph' }
ASCPushVariable >> dotStringOn: aStream [

	aStream
		<< 'push offset ';
		print: variableLocation offset - stackOffset
]

{ #category : 'printing' }
ASCPushVariable >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream tab.
	aStream << 'value: '.
	aStream << variableLocation name
]

{ #category : 'testing' }
ASCPushVariable >> needsContext: aStackContext [
	" this should be removed when reifyLocals is to work "

	^ self needsLocals
]

{ #category : 'testing' }
ASCPushVariable >> needsLocals [

	^ true
]

{ #category : 'code generation' }
ASCPushVariable >> outputTo: anOutput [

	^ variableLocation pushTheVariable: anOutput stackOffset: stackOffset
]

{ #category : 'printing' }
ASCPushVariable >> printOn: aStream [
	variableLocation printOn: aStream
	"aStream nextPutAll: variableLocation name."
]

{ #category : 'stack' }
ASCPushVariable >> resultStack: anASStack [

	stack := anASStack.
	stackOffset := anASStack offset.
	^ self createsTemporary: anASStack data: variableLocation stackValue
]

{ #category : 'finalAnalysis' }
ASCPushVariable >> variableReferences: referenced modifies: ignore block: aDispatchableBlock [

	referenced
		at: variableLocation
		ifPresent: [ :bag | bag add: aDispatchableBlock ]
		ifAbsentPut: [ IdentityBag with: aDispatchableBlock ]
]

{ #category : 'instance creation' }
ASCPushVariable >> with: target [


	variableLocation := target
]
