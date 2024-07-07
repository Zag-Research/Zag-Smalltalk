"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturn',
	#superclass : 'ASCStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCReturn >> next [

	^ nil
]

{ #category : 'instance creation' }
ASCReturn >> with: ignored [

]
