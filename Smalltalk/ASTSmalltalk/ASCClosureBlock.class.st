"
I am an inline block, but I'm used to inline a BlockClosure

Returns from me are stack adjustment and branch to the return point
My final result will be a stack adjustment and branch to the return point
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
