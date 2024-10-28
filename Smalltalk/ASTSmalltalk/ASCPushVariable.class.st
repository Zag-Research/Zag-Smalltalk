"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'variable',
		'stackOffset'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCPushVariable >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(;
		<< variable name;
		<< $)
]

{ #category : 'code generation' }
ASCPushVariable >> outputTo: anOutput [

	^ variable pushTheVariable: anOutput stackOffset: stackOffset
]

{ #category : 'printing' }
ASCPushVariable >> printOn: aStream [

	aStream nextPutAll: ' - ';nextPutAll: variable name
]

{ #category : 'stack' }
ASCPushVariable >> resultStack: anASStack basicBlock: aBasicBlock [

	stackOffset := anASStack offset.
	^ self created: (anASStack pushDataAsTemporary: variable stackValue)
]

{ #category : 'as yet unclassified' }
ASCPushVariable >> variableReferences: referenced modifies: modified block: aDispatchableBlock [
	referenced
		at: variable location
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

	variable := target
]
