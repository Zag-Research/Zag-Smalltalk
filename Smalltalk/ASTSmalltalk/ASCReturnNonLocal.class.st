"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturnNonLocal',
	#superclass : 'ASCStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'block handling' }
ASCReturnNonLocal >> next [

	^ nil
]

{ #category : 'code generation' }
ASCReturnNonLocal >> outputTo: output [

	^ output visitReturnNonLocalTop
]

{ #category : 'stack' }
ASCReturnNonLocal >> resultStack: aStack [

	self flag: 'needs to know where self is and cleanup stack to that point'.
	^ aStack drop
]

{ #category : 'instance creation' }
ASCReturnNonLocal >> with: ignored [

]
