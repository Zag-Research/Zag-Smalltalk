"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturnNonLocal',
	#superclass : 'ASCOperations',
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

{ #category : 'instance creation' }
ASCReturnNonLocal >> with: ignored [

]
