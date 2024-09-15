"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCClosureBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCClosureBlock >> selfElementClass [

	^ ASStackNamedIndirect
]
