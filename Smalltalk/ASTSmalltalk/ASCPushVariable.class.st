"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'stackOffset',
		'variableLocation'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCPushVariable >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(;
		<< variableLocation name;
		<< $)
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

	stackOffset := anASStack offset.
	^ self created: (anASStack pushDataAsTemporary: variableLocation stackValue)
]

{ #category : 'instance creation' }
ASCPushVariable >> with: target [

	variableLocation := target
]
