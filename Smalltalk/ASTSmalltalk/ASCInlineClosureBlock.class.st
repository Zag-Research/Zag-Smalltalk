"
I am an inline block,  used to inline a BlockClosure

Returns from me are stack adjustment and branch to the return point for the defining method
My final expression will be a stack adjustment and branch to the return point, replacing `self` with `nil`
"
Class {
	#name : 'ASCInlineClosureBlock',
	#superclass : 'ASCInlinedBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCInlineClosureBlock >> selfElementClass [

	^ ASStackNamedIndirect
]
