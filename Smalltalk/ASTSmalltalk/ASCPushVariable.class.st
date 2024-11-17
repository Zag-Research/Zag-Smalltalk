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
		'variableLocation',
		'needsContext'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
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
	aStream << $(;
		<< '???'; "variableLocaton name;"
		<< $)
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

	aStream nextPutAll: ' - ';nextPutAll: variableLocation name
]

{ #category : 'stack' }
ASCPushVariable >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	stackOffset := anASStack offset.
	^ self createsTemporary: anASStack data: variableLocation stackValue
]

{ #category : 'as yet unclassified' }
ASCPushVariable >> variableReferences: referenced modifies: modified block: aDispatchableBlock [
	referenced
		at: variableLocation 
		ifPresent: [ :dic |
			dic
				at: aDispatchableBlock
				ifPresent: [ :count | dic at: aDispatchableBlock put: count + 1 ]
				ifAbsentPut: [ 1 ] ]
		ifAbsentPut: [
			Dictionary new
				at: aDispatchableBlock put: 1;
				yourself ]
]

{ #category : 'as yet unclassified' }
ASCPushVariable >> variableUsage: aDispatchableBlock [

	aDispatchableBlock referenced: variable
]

{ #category : 'instance creation' }
ASCPushVariable >> with: target [


	needsContext := target isLocal.
	variableLocation := target
]
