"
I am a BasicBlock that is the return target for a send or call
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCReturnBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: label.
	aStream << $)
]

{ #category : 'writing' }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: label.
	output outputMethodLabel: label.
	^ self outputRestTo: output
]
