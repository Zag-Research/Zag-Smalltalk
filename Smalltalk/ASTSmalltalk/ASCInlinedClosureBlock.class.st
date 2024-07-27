"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCInlinedClosureBlock',
	#superclass : 'ASCInlineBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCInlinedClosureBlock >> selfElementClass [

	^ ASStackNamedIndirect
]
