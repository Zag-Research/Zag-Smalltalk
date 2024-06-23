"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCBlockClosureBlock',
	#superclass : 'ASCInlineBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'data flow instructions' }
ASCBlockClosureBlock >> selfElementClass [

	^ ASStackNamedIndirect
]
