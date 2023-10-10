"
I am a BasicBlock that is the return target for a send or call
"
Class {
	#name : #ASCReturnBlock,
	#superclass : #ASCBasicBlock,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #wriiting }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: label.
	output outputMethodLabel: label.
	self outputRestTo: output
]
