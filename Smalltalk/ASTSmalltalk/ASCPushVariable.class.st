"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'variable'
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
	^ variable pushTheVariable: anOutput
]

{ #category : 'printing' }
ASCPushVariable >> printOn: aStream [

	aStream nextPutAll: ' - ';nextPutAll: variable name
]

{ #category : 'stack' }
ASCPushVariable >> resultStack: anASStack basicBlock: aBasicBlock [

	^ anASStack pushDataAsTemporary: variable stackValue
]

{ #category : 'instance creation' }
ASCPushVariable >> with: target [

	variable := target
]
