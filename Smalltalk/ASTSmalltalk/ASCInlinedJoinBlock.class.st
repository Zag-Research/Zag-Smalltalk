"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCInlinedJoinBlock',
	#superclass : 'ASCInlineBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlinedJoinBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	^ stack
]

{ #category : 'as yet unclassified' }
ASCInlinedJoinBlock >> flowForLocals [

	^ ASFlowPhi
]

{ #category : 'as yet unclassified' }
ASCInlinedJoinBlock >> flowForTemporaries [

	^ ASFlowPhi
]
