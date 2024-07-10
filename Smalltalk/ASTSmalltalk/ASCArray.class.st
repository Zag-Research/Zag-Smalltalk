"
I create an array and push the address on the stack
"
Class {
	#name : 'ASCArray',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'size'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCArray >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: size.
	aStream << $)
]

{ #category : 'code generation' }
ASCArray >> outputTo: output [

	^ output visitPushArray: size
]

{ #category : 'stack' }
ASCArray >> resultStack: anASStack basicBlock: anObject [

	^ anASStack pushClass: Array
]

{ #category : 'instance creation' }
ASCArray >> with: anInteger [

	size := anInteger
]
