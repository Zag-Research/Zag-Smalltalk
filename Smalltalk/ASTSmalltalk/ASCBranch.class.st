"
I represent a return
"
Class {
	#name : 'ASCBranch',
	#superclass : 'ASCOperations',
	#instVars : [
		'bb'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCBranch >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		<< ' -> ';
		<< bb label
]

{ #category : 'accessing' }
ASCBranch >> next [

	^ bb nextInline
]

{ #category : 'code generation' }
ASCBranch >> outputTo: output [

	^ output visitBranch: bb
]

{ #category : 'instance creation' }
ASCBranch >> with: target [

	bb := target

	
]
