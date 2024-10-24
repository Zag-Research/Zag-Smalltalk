"
I represent a return
"
Class {
	#name : 'ASCBranch',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'bb'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCBranch >> dotEdgesOn: aStream from: aBasicBlock [

	aBasicBlock dotNameOn: aStream.
	aStream << ':end:e ->'.
	bb dotNameOn: aStream.
	^ aStream << ';
'
]

{ #category : 'API' }
ASCBranch >> dotStringOn: aStream [

	aStream
		<< '<end>branch '
]

{ #category : 'printing' }
ASCBranch >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		<< ' -> ';
		<< bb label
]

{ #category : 'as yet unclassified' }
ASCBranch >> nextBasicBlocks [

	^ bb
]

{ #category : 'code generation' }
ASCBranch >> outputTo: output [

	^ output visitBranch: bb
]

{ #category : 'stack' }
ASCBranch >> resultStack: anASStack basicBlock: anObject [

	^ anASStack
]

{ #category : 'as yet unclassified' }
ASCBranch >> with: target [

	bb := target

	
]
